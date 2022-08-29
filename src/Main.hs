{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Applicative (Alternative (..))
import Control.Category ((.))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.Maybe (isJust)
import Data.Time.Calendar (Day)
import GHC.Generics (Generic)
import qualified Network.HTTP.Types as HTTP
import Network.Wai (Application)
import qualified Network.Wai.Handler.Warp as Warp
import Web.HttpApiData (FromHttpApiData)
import WebGear.Server
import Prelude hiding ((.))

--------------------------------------------------------------------------------
-- An example program that uses WebGear to build a simple HTTP API to
-- perform CRUD operations on user records.
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Model for users
--------------------------------------------------------------------------------
data User = User
  { userId :: UserId
  , userName :: Text
  , dateOfBirth :: Day
  , gender :: Gender
  , emailAddress :: Text
  }
  deriving (Generic, FromJSON, ToJSON)

newtype UserId = UserId Int
  deriving (Eq, FromJSON, ToJSON, Hashable, FromHttpApiData) via Int

data Gender = Male | Female | OtherGender
  deriving (Generic, FromJSON, ToJSON)

--------------------------------------------------------------------------------
-- An in-memory store and associated operations for users
--------------------------------------------------------------------------------
newtype UserStore = UserStore (IORef (HM.HashMap UserId User))

addUser :: MonadIO m => UserStore -> User -> m ()
addUser (UserStore ref) user = liftIO $ modifyIORef ref (HM.insert (userId user) user)

lookupUser :: MonadIO m => UserStore -> UserId -> m (Maybe User)
lookupUser (UserStore ref) uid = liftIO (HM.lookup uid <$> readIORef ref)

removeUser :: MonadIO m => UserStore -> UserId -> m Bool
removeUser store@(UserStore ref) uid = liftIO $ do
  u <- lookupUser store uid
  modifyIORef ref (HM.delete uid)
  pure $ isJust u

--------------------------------------------------------------------------------
-- Routes of the API
--------------------------------------------------------------------------------
type IntUserId = PathVar "userId" UserId
type Auth = BasicAuth App () Credentials

authConfig :: BasicAuth App () Credentials
authConfig =
  BasicAuth'
    { toBasicAttribute = \creds ->
        pure $
          if creds == Credentials "panther" "forever"
            then Right creds
            else Left ()
    }

-- The route handlers run in the App monad
newtype App a = App {unApp :: ReaderT UserStore IO a}
  deriving newtype
    ( Functor
    , Applicative
    , Alternative
    , Monad
    , MonadIO
    , MonadReader UserStore
    )

userRoutes :: RequestHandler (ServerHandler App) req
userRoutes =
  [match| /v1/users/userId:UserId |] -- non-TH version: path @"/v1/users" . pathVar @"userId" @Int
    (publicRoutes <+> protectedRoutes)

-- | Routes accessible without any authentication
publicRoutes ::
  ( HasTrait IntUserId req
  , StdHandler
      h
      App
      '[]
      '[RequiredHeader "Content-Type" Text, JSONBody User]
  ) =>
  RequestHandler h req
publicRoutes = getUser

-- | Routes that require HTTP basic authentication
protectedRoutes ::
  forall h req.
  ( HasTrait IntUserId req
  , StdHandler
      h
      App
      '[BasicAuth App () Credentials, JSONBody User]
      '[RequiredHeader "Content-Type" Text, JSONBody User, Body Text]
  ) =>
  RequestHandler h req
protectedRoutes = basicAuth authConfig forbidden $ putUser <+> deleteUser
  where
    forbidden :: h a Response
    forbidden = proc _ -> unlinkA . respondA @Text HTTP.forbidden403 "text/plain" -< "Unauthorized"

getUser ::
  forall h req.
  ( HasTrait IntUserId req
  , StdHandler
      h
      App
      '[]
      '[RequiredHeader "Content-Type" Text, JSONBody User]
  ) =>
  RequestHandler h req
getUser = method HTTP.GET $
  proc request -> do
    let uid = pick @IntUserId $ from request
    maybeUser <- fetchUser -< uid
    case maybeUser of
      Nothing -> unlinkA . notFound404 -< ()
      Just user -> unlinkA . respondJsonA @User HTTP.ok200 -< user
  where
    fetchUser :: h UserId (Maybe User)
    fetchUser = arrM $ \uid -> do
      store <- ask
      lookupUser store uid

putUser ::
  forall h req.
  ( HaveTraits [Auth, IntUserId] req
  , StdHandler
      h
      App
      '[JSONBody User]
      '[RequiredHeader "Content-Type" Text, JSONBody User, Body Text]
  ) =>
  RequestHandler h req
putUser = method HTTP.PUT $
  jsonRequestBody @User badRequestBody $
    proc request -> do
      let uid = pick @IntUserId $ from request
          user = pick @(JSONBody User) $ from request
          user' = user{userId = uid}
      doAdd -< (request, user')
      unlinkA . respondJsonA @User HTTP.ok200 -< user'
  where
    badRequestBody :: h a Response
    badRequestBody = proc _ ->
      unlinkA . respondA @Text HTTP.badRequest400 "text/plain" -< "Could not parse body"

    doAdd = arrM $ \(request, user) -> do
      store <- ask
      addUser store user
      logActivity request "updated"

deleteUser ::
  forall h req.
  ( HaveTraits [Auth, IntUserId] req
  , StdHandler
      h
      App
      '[]
      '[]
  ) =>
  RequestHandler h req
deleteUser = method HTTP.DELETE $
  proc request -> do
    let uid = pick @IntUserId $ from request
    removed <- doRemove -< (request, uid)
    if removed
      then unlinkA . noContent204 -< ()
      else unlinkA . notFound404 -< ()
  where
    doRemove = arrM $ \(request, uid) -> do
      store <- ask
      found <- removeUser store uid
      when found $
        logActivity request "deleted"
      pure found

logActivity :: (MonadIO m, HasTrait Auth req) => Linked req Request -> String -> m ()
logActivity request msg = do
  let name = credentialsUsername $ pick @Auth $ from request
  liftIO $ putStrLn $ msg <> ": by " <> show name

--------------------------------------------------------------------------------
-- The application server
-------------------------------------------------------------------------------
application :: UserStore -> Application
application store = toApplication $ transform appToRouter userRoutes
  where
    appToRouter :: App a -> IO a
    appToRouter = flip runReaderT store . unApp

main :: IO ()
main = do
  store <- newIORef HM.empty
  Warp.run 3000 (application $ UserStore store)

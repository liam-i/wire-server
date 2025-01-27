-- | This module is meant to show how Testlib can be used
module Test.Demo where

import qualified API.Brig as Public
import qualified API.GalleyInternal as Internal
import GHC.Stack
import SetupHelpers
import Testlib.Prelude

testCantDeleteLHClient :: HasCallStack => App ()
testCantDeleteLHClient = do
  user <- randomUser ownDomain def
  client <-
    Public.addClient user def {Public.ctype = "legalhold", Public.internal = True}
      >>= getJSON 201

  bindResponse (Public.deleteClient user client) $ \resp -> do
    resp.status `shouldMatchInt` 400

testDeleteUnknownClient :: HasCallStack => App ()
testDeleteUnknownClient = do
  user <- randomUser ownDomain def
  let fakeClientId = "deadbeefdeadbeef"
  bindResponse (Public.deleteClient user fakeClientId) $ \resp -> do
    resp.status `shouldMatchInt` 404
    resp.json %. "label" `shouldMatch` "client-not-found"

testModifiedBrig :: HasCallStack => App ()
testModifiedBrig = do
  withModifiedService
    Brig
    (setField "optSettings.setFederationDomain" "overridden.example.com")
    $ bindResponse (Public.getAPIVersion ownDomain)
    $ \resp -> do
      resp.status `shouldMatchInt` 200
      (resp.json %. "domain") `shouldMatch` "overridden.example.com"

testModifiedGalley :: HasCallStack => App ()
testModifiedGalley = do
  (_user, tid) <- createTeam ownDomain

  let getFeatureStatus = do
        bindResponse (Internal.getTeamFeature "searchVisibility" tid) $ \res -> do
          res.status `shouldMatchInt` 200
          res.json %. "status"

  do
    getFeatureStatus `shouldMatch` "disabled"

  withModifiedService
    Galley
    (setField "settings.featureFlags.teamSearchVisibility" "enabled-by-default")
    $ do
      getFeatureStatus `shouldMatch` "enabled"

testWebSockets :: HasCallStack => App ()
testWebSockets = do
  user <- randomUser ownDomain def
  withWebSocket user $ \ws -> do
    client <- Public.addClient user def >>= getJSON 201
    n <- awaitMatch 3 (\n -> nPayload n %. "type" `isEqual` "user.client-add") ws
    nPayload n %. "client.id" `shouldMatch` (client %. "id")

testMultipleBackends :: App ()
testMultipleBackends = do
  ownDomainRes <- (Public.getAPIVersion ownDomain >>= getJSON 200) %. "domain"
  otherDomainRes <- (Public.getAPIVersion otherDomain >>= getJSON 200) %. "domain"
  ownDomainRes `shouldMatch` ownDomain
  otherDomainRes `shouldMatch` otherDomain
  ownDomain `shouldNotMatch` otherDomain

testUnrace :: App ()
testUnrace = do
  {-
  -- the following would retry for ~30s and only then fail
  unrace $ do
    True `shouldMatch` True
    True `shouldMatch` False
  -}
  unrace $ True `shouldMatch` True

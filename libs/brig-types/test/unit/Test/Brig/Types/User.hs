{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Test.Brig.Types.User where

import Brig.Types.Intra (NewUserScimInvitation (..), ReAuthUser (..))
import Brig.Types.SparAuthId
import Brig.Types.User (ManagedByUpdate (..), RichInfoUpdate (..))
import qualified Data.Aeson as Aeson
import Data.Id
import Data.String.Conversions (cs)
import Imports
import SAML2.WebSSO as SAML
import Test.Brig.Roundtrip (testRoundTrip)
import Test.QuickCheck (Arbitrary (arbitrary))
import Test.Tasty
import Test.Tasty.HUnit
import URI.ByteString.QQ (uri)
import qualified Wire.API.User.Identity as Identity

tests :: TestTree
tests = testGroup "User (types vs. aeson)" $ roundtripTests <> specialCases

roundtripTests :: [TestTree]
roundtripTests =
  [ testRoundTrip @AuthId,
    testRoundTrip @ManagedByUpdate,
    testRoundTrip @NewUserScimInvitation,
    testRoundTrip @ReAuthUser,
    testRoundTrip @RichInfoUpdate
  ]

instance Arbitrary AuthId where
  arbitrary = undefined

instance Arbitrary ManagedByUpdate where
  arbitrary = ManagedByUpdate <$> arbitrary

instance Arbitrary NewUserScimInvitation where
  arbitrary = NewUserScimInvitation <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ReAuthUser where
  arbitrary = ReAuthUser <$> arbitrary

instance Arbitrary RichInfoUpdate where
  arbitrary = RichInfoUpdate <$> arbitrary

specialCases :: [TestTree]
specialCases =
  [ testCase "parse legacy UserSSOId values as LegacyAuthId" $ do
      let tid :: TeamId
          tid = undefined ("b6e09060-7b73-11eb-af7f-9b48ef85f610" :: Text)

          samples :: [(LByteString, AuthId)]
          samples =
            [ ( -- Wire.API.User.Identity.UserSSOId "https://example.com/a6ce4950-7b4c-11eb-b572-bfe2f05ec462" "user@example.com"
                "{\"subject\":\"user@example.com\",\"tenant\":\"https://example.com/a6ce4950-7b4c-11eb-b572-bfe2f05ec462\"}",
                AuthSAML
                  ( SAML.UserRef
                      (SAML.Issuer [uri|https://example.com/a6ce4950-7b4c-11eb-b572-bfe2f05ec462|])
                      (fromRight (error "impossible") (emailNameID "user@example.com"))
                  )
              ),
              ( -- Wire.API.User.Identity.UserScimExternalId "user@example.com"
                "{\"scim_external_id\":\"user@example.com\"}",
                AuthSCIM
                  ( ScimDetails
                      (ExternalId tid "user@example.com")
                      (EmailWithSource (fromJust $ Identity.parseEmail "user@example.com") EmailFromExternalIdField)
                  )
              )
            ]

      forM_ samples $ \(input, want) -> do
        let have = (fromLegacyAuthId . fromJust . Aeson.decode $ input) tid
        assertEqual (cs input) want have
  ]

-- TODO: what else to test from SparAuthId?  other types?  functions?
-- TODO: are there any undefineds left in the SparAuthId module?  have they not been caught by the tests?

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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
import Brig.Types.SparAuthId (AuthId (..))
import Brig.Types.User (ManagedByUpdate (..), RichInfoUpdate (..))
import Imports
import Test.Brig.Roundtrip (testRoundTrip)
import Test.QuickCheck (Arbitrary (arbitrary))
import Test.Tasty

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

instance Arbitrary ManagedByUpdate where
  arbitrary = ManagedByUpdate <$> arbitrary

instance Arbitrary RichInfoUpdate where
  arbitrary = RichInfoUpdate <$> arbitrary

instance Arbitrary ReAuthUser where
  arbitrary = ReAuthUser <$> arbitrary

instance Arbitrary NewUserScimInvitation where
  arbitrary = NewUserScimInvitation <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

specialCases :: [TestTree]
specialCases =
  [ testCase "parse (removed UserSSOId) as LegacyAuthId" _
  ]

-- UserSSOId

vals =
  [ UserSSOId "https://example.com/a6ce4950-7b4c-11eb-b572-bfe2f05ec462" "user@example.com",
    UserScimExternalId "user@example.com"
  ]

-- TODO: what else to test from SparAuthId?  other types?  functions?
-- TODO: are there any undefineds left in the SparAuthId module?  have they not been caught by the tests?

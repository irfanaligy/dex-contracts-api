module GeniusYield.Api.Vesting.Types
  ( GYTimeBeforeSystemStartException (..)
  , VestingConfig (..)
  , Recipient (..)
  , VestingInfo (..)
  )
where

import Data.Csv qualified as Csv
import GeniusYield.HTTP.Errors
import GeniusYield.Imports
import GeniusYield.Types

{- $setup

>>> :set -XOverloadedStrings -XTypeApplications
>>> import qualified Data.ByteString.Lazy.Char8 as LBS8
>>> import qualified Data.Csv                   as Csv
>>> import           GeniusYield.Types
-}

newtype GYTimeBeforeSystemStartException = GYTimeBeforeSystemStartException GYTime
  deriving (Eq, Ord, Read, Show)

instance Exception GYTimeBeforeSystemStartException

instance IsGYApiError GYTimeBeforeSystemStartException

data VestingConfig = VestingConfig
  { vcSKeyFile :: !FilePath
  -- ^ Payment Signing Key File Path
  }
  deriving (FromJSON, Generic, Show)

{- |

>>> :{
LBS8.putStr $ Csv.encode
    [ Recipient
        { recAddress    = unsafeAddressFromText "addr_test1vr5e43pjacjpmrtnzxchhe58gwsem3axcvlc4q8yfdnycnctcev6t"
        , recCancelTime = "1970-01-01T00:00:00Z"
        , recUnlockTime = "1970-05-31T00:00:00Z"
        , recAmount     = 123456
        , recToken      = "lovelace"
        }
    , Recipient
        { recAddress    = unsafeAddressFromText "addr1w89tm5vmtr2zn877qk6nctqt47tch7dduu6tfy8upnyt05q6kgucg"
        , recCancelTime = "2022-10-31T19:22:10Z"
        , recUnlockTime = "2022-12-01T20:00:50Z"
        , recAmount     = 999999999
        , recToken      = "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef.474f4c44"
        }
    ]
:}
addr_test1vr5e43pjacjpmrtnzxchhe58gwsem3axcvlc4q8yfdnycnctcev6t,1970-01-01T00:00:00Z,1970-05-31T00:00:00Z,123456,lovelace
addr1w89tm5vmtr2zn877qk6nctqt47tch7dduu6tfy8upnyt05q6kgucg,2022-10-31T19:22:10Z,2022-12-01T20:00:50Z,999999999,ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef.474f4c44

>>> :{
Csv.decode @Recipient Csv.NoHeader $
    "addr_test1vr5e43pjacjpmrtnzxchhe58gwsem3axcvlc4q8yfdnycnctcev6t,1970-01-01T00:00:00Z,1970-05-31T00:00:00Z,123456,lovelace\n" <>
    "addr1w89tm5vmtr2zn877qk6nctqt47tch7dduu6tfy8upnyt05q6kgucg,2022-10-31T19:22:10Z,2022-12-01T20:00:50Z,999999999,ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef.474f4c44"
:}
Right [Recipient {recAddress = unsafeAddressFromText "addr_test1vr5e43pjacjpmrtnzxchhe58gwsem3axcvlc4q8yfdnycnctcev6t", recCancelTime = GYTime 0s, recUnlockTime = GYTime 12960000s, recAmount = 123456, recToken = GYLovelace},Recipient {recAddress = unsafeAddressFromText "addr1w89tm5vmtr2zn877qk6nctqt47tch7dduu6tfy8upnyt05q6kgucg", recCancelTime = GYTime 1667244130s, recUnlockTime = GYTime 1669924850s, recAmount = 999999999, recToken = GYToken "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef" "GOLD"}]
-}
data Recipient = Recipient
  { recAddress :: !GYAddress
  , recCancelTime :: !GYTime
  , recUnlockTime :: !GYTime
  , recAmount :: !Natural
  , recToken :: !GYAssetClass
  }
  deriving (Csv.FromRecord, Csv.ToRecord, Generic, Show)

data VestingInfo = VestingInfo
  { viRef :: !GYTxOutRef
  , viRecipient :: !GYAddress
  , viDeposit :: !GYAddress
  , viCancelKey :: !GYPubKeyHash
  , viCancelTime :: !GYTime
  , viUnlockTime :: !GYTime
  , viValue :: !GYValue
  }
  deriving (Csv.FromRecord, Csv.ToRecord, Generic, Show)

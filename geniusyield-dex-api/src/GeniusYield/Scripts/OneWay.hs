module GeniusYield.Scripts.OneWay
  ( oneWayValidator
  , oneWayPlutusAddress
  , oneWayAddress
  )
where

import GeniusYield.Types
import PlutusLedgerApi.V1 qualified as Plutus (Address)

import GeniusYield.OnChain.OneWay.Compiled qualified as OnChain

oneWayValidator :: GYScript PlutusV2
oneWayValidator = validatorFromPlutus OnChain.oneWayValidator

-- Note that Plutus addresses are independant of the network.
oneWayAddress :: GYAddress
oneWayAddress =
  unsafeAddressFromText
    "addr_test1wpgexmeunzsykesf42d4eqet5yvzeap6trjnflxqtkcf66g0kpnxt"

oneWayPlutusAddress :: Plutus.Address
oneWayPlutusAddress = addressToPlutus oneWayAddress

module GeniusYield.OnChain.Staking.Stake.Compiled
  ( originalStakeValidator
  , optimizedStakeValidator
  )
where

import Data.Default (def)
import Data.Text (Text)
import Plutarch.Api.V2
import Plutarch.Prelude
import Plutarch.Unsafe qualified as PUNSAFE
import Plutonomy qualified
import Ply hiding ((#))
import Ply.Plutarch

import GeniusYield.OnChain.Staking.Stake
import GeniusYield.Plutonomy ()

originalStakeValidator :: Either Text (TypedScript 'ValidatorRole '[])
originalStakeValidator = toTypedScript def stakeValidator'

optimizedStakeValidator :: Either Text (TypedScript 'ValidatorRole '[])
optimizedStakeValidator = Plutonomy.optimizeUPLC <$> originalStakeValidator

stakeValidator' :: ClosedTerm PValidator
stakeValidator' = plam $ \datm redm ctx ->
  popaque
    $ stakeValidator
      # PUNSAFE.punsafeCoerce datm
      # PUNSAFE.punsafeCoerce redm
      # pdata ctx

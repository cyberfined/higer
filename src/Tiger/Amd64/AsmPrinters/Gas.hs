module Tiger.Amd64.AsmPrinters.Gas (Gas(..)) where

import           Tiger.TextUtils (TextBuildable (..))

newtype Gas r = Gas r deriving newtype (Enum, TextBuildable)

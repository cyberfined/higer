module Tiger.Amd64.Frame (Frame(..)) where

import           Control.Monad  (foldM)
import           Data.Proxy     (Proxy (..))

import           Tiger.Expr     (Escaping (..))
import           Tiger.Frame    (Access (..), wordSize)
import           Tiger.IR.Types (Binop (..), Expr (..))
import           Tiger.Temp     hiding (Temp)

import qualified Tiger.Frame    as FrameClass

data Frame = Frame
    { frLabel      :: !Label
    , frCurOffset  :: !Int
    , frArgsOffset :: !Int
    , frArgs       :: ![Access]
    }

maxRegisterArgs :: Int
maxRegisterArgs = 6

instance FrameClass.Frame Frame where
    newFrame label args = do
        (argsOffset, _, accessArgs) <- foldM escToAccess acc args
        pure $ Frame { frLabel      = label
                     , frCurOffset  = 0
                     , frArgsOffset = argsOffset
                     , frArgs       = accessArgs
                     }
      where acc = (wordSize $ Proxy @Frame, 0, [])
            escToAccess :: MonadTemp m
                        => (Int, Int, [Access])
                        -> Escaping
                        -> m (Int, Int, [Access])
            escToAccess (curOffset, numRegArgs, as) = \case
                Escaping -> escResult
                Remaining
                  | numRegArgs < maxRegisterArgs
                  -> (\tmp -> (curOffset, numRegArgs + 1, InReg tmp : as)) <$> newTemp
                  | otherwise
                  -> escResult
              where nextOffset = curOffset + wordSize (Proxy @Frame)
                    escResult = pure (nextOffset, numRegArgs, InFrame nextOffset : as)
    frameName Frame{..} = frLabel
    frameArgs Frame{..} = frArgs
    allocLocal f@Frame{..} = \case
        Escaping  -> pure (f { frCurOffset = nextOffset }, InFrame (-nextOffset))
        Remaining -> newTemp >>= \tmp -> pure (f, InReg tmp)
      where nextOffset = frCurOffset + wordSize (Proxy @Frame)
    wordSize = const 8
    accessToIR _ access frameAddress = case access of
        InReg t -> pure $ Temp t
        InFrame off
          | off == 0  -> pure frameAddress
          | off < 0   -> pure $ Binop Sub frameAddress (Const $ -off)
          | otherwise -> pure $ Binop Add frameAddress (Const off)
    externalCall _ name args = Call (LabelText name) args
    procEntryExit1 _ body = body

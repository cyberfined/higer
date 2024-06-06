module Tiger.Amd64.Frame (Frame(..)) where

import           Data.Foldable              (foldrM)
import           Data.Proxy                 (Proxy (..))
import           Data.Text.Lazy.Builder.Int (decimal)

import           Tiger.Expr                 (Escaping (..))
import           Tiger.Frame                (Access (..), accessBuilder, wordSize)
import           Tiger.IR.Types             (Binop (..), Expr (..))
import           Tiger.Temp                 hiding (Temp)
import           Tiger.TextUtils            (intercalate)

import qualified Tiger.Frame                as FrameClass

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
        (argsOffset, _, accessArgs) <- foldrM escToAccess acc args
        pure $ Frame { frLabel      = label
                     , frCurOffset  = 0
                     , frArgsOffset = argsOffset
                     , frArgs       = accessArgs
                     }
      where acc = (wordSize $ Proxy @Frame, 0, [])
            escToAccess :: MonadTemp m
                        => Escaping
                        -> (Int, Int, [Access])
                        -> m (Int, Int, [Access])
            escToAccess esc (curOffset, numRegArgs, as) = case esc of
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
    frameBuilder Frame{..} =  "Amd64.Frame\n"
                           <> "    { frLabel = " <> labelBuilder frLabel <> "\n"
                           <> "    , frCurOffset = " <> decimal frCurOffset <> "\n"
                           <> "    , frArgsOffset = " <> decimal frArgsOffset <> "\n"
                           <> "    , frArgs = [" <>  argsBuilder <> "]\n"
                           <> "    }"
      where argsBuilder = intercalate "," (map accessBuilder frArgs)

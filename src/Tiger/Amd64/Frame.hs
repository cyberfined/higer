module Tiger.Amd64.Frame
    ( Frame(..)
    , wordSize
    , newFrame
    , frameName
    , frameArgs
    , allocLocal
    , accessToIR
    , procEntryExit1
    , frameBuilder
    ) where

import           Data.Foldable              (foldrM)
import           Data.Text.Lazy.Builder     (Builder)
import           Data.Text.Lazy.Builder.Int (decimal)

import           Tiger.Expr                 (Escaping (..))
import           Tiger.Frame                (Access (..))
import           Tiger.IR.Types             (Binop (..), Expr (..), Stmt)
import           Tiger.Temp                 hiding (Temp)
import           Tiger.TextUtils            (TextBuildable (..), intercalate)

data Frame = Frame
    { frLabel      :: !Label
    , frCurOffset  :: !Int
    , frArgsOffset :: !Int
    , frArgs       :: ![Access]
    }

wordSize :: Int
wordSize = 8

newFrame :: MonadTemp m => Int -> Label -> [Escaping] -> m Frame
newFrame maxRegisterArgs label args = do
    (argsOffset, _, accessArgs) <- foldrM escToAccess acc args
    pure $ Frame { frLabel      = label
                 , frCurOffset  = 0
                 , frArgsOffset = argsOffset
                 , frArgs       = accessArgs
                 }
  where acc = (wordSize, 0, [])
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
          where nextOffset = curOffset + wordSize
                escResult = pure (nextOffset, numRegArgs, InFrame nextOffset : as)

frameName :: Frame -> Label
frameName Frame{..} = frLabel

frameArgs :: Frame -> [Access]
frameArgs Frame{..} = frArgs

allocLocal :: MonadTemp m => Frame -> Escaping -> m (Frame, Access)
allocLocal f@Frame{..} = \case
    Escaping  -> pure (f { frCurOffset = nextOffset }, InFrame (-nextOffset))
    Remaining -> newTemp >>= \tmp -> pure (f, InReg tmp)
  where nextOffset = frCurOffset + wordSize

accessToIR :: MonadTemp m => Frame -> Access -> Expr -> m Expr
accessToIR _ access frameAddress = case access of
    InReg t -> pure $ Temp t
    InFrame off
      | off == 0  -> pure frameAddress
      | off < 0   -> pure $ Binop Sub frameAddress (Const $ -off)
      | otherwise -> pure $ Binop Add frameAddress (Const off)

procEntryExit1 :: Frame -> Stmt -> Stmt
procEntryExit1 _ body = body

frameBuilder :: Builder -> Frame -> Builder
frameBuilder header Frame{..} =  header <> "\n"
                              <> "    { frLabel = " <> toTextBuilder frLabel <> "\n"
                              <> "    , frCurOffset = " <> decimal frCurOffset <> "\n"
                              <> "    , frArgsOffset = " <> decimal frArgsOffset <> "\n"
                              <> "    , frArgs = [" <>  argsBuilder <> "]\n"
                              <> "    }"
  where argsBuilder = intercalate "," (map toTextBuilder frArgs)

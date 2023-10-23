{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}

module Tiger.Amd64.Frame (Frame) where

import           Control.Monad          (forM)
import           Control.Monad.IO.Class
import           Data.IORef
import           Data.Proxy             (Proxy (..))

import           Tiger.Expr             (Escaping (..))
import           Tiger.Frame            (Access (..), wordSize)
import           Tiger.IR
import           Tiger.Temp             hiding (Temp)

import qualified Tiger.Frame            as FrameClass

data Frame = Frame
    { frLabel      :: !Label
    , frCurOffset  :: !(IORef Int)
    , frArgsOffset :: !(IORef Int)
    , frArgs       :: ![Access]
    }

maxRegisterArgs :: Int
maxRegisterArgs = 6

instance FrameClass.Frame Frame where
    newFrame label args = do
        curOffsetRef <- liftIO $ newIORef 0
        numRegArgsRef <- liftIO (newIORef 0 :: IO (IORef Int))
        argsOffsetRef <- liftIO $ newIORef 8
        accessArgs <- forM args $ \case
            Escaping -> placeInFrame argsOffsetRef 1
            Remaining -> do
                numRegArgs <- liftIO $ readIORef numRegArgsRef
                if numRegArgs < maxRegisterArgs
                   then do
                       liftIO $ modifyIORef' numRegArgsRef (+1)
                       InReg <$> newTemp
                   else placeInFrame argsOffsetRef 1

        pure Frame { frLabel      = label
                   , frCurOffset  = curOffsetRef
                   , frArgsOffset = argsOffsetRef
                   , frArgs       = accessArgs
                   }
    frameName = frLabel
    frameArgs = frArgs
    allocLocal Frame{..} = \case
        Escaping  -> placeInFrame frCurOffset (-1)
        Remaining -> InReg <$> newTemp
    wordSize = const 8
    accessToIR _ access frameAddress = case access of
        InReg t -> pure ([], t)
        InFrame off
          | off == 0 -> case frameAddress of
              Temp t -> pure ([], t)
              _      -> (\t -> ([Assign t frameAddress], t)) <$> newTemp
          | otherwise -> do
            t <- newTemp
            let (posOff, op) = if off < 0 then (-off, Sub) else (off, Add)
            pure ([Binop t op frameAddress (Const posOff)], t)
    procEntryExit = const ([], [])
    externalCall _ dst name = Call dst (externalFunLabel name)

placeInFrame :: MonadIO m => IORef Int -> Int -> m Access
placeInFrame curOffsetRef pos = do
    offset <- liftIO $ do
        res <- readIORef curOffsetRef
        modifyIORef' curOffsetRef (+(wordSize $ Proxy @Frame))
        pure (res +(wordSize $ Proxy @Frame))
    pure $ InFrame (pos * offset)

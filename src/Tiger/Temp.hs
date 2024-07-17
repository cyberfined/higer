module Tiger.Temp
    ( Temp(..)
    , tempBuilder
    , Label(..)
    , labelBuilder
    , MonadTemp(..)
    , TempM
    , InitTemp(..)
    , InitLabel(..)
    , runTempM
    ) where

import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Reader       (MonadReader (..), ReaderT, asks, runReaderT)
import           Data.Hashable              (Hashable (..))
import           Data.IORef                 (IORef, newIORef, readIORef, writeIORef)
import           Data.Text                  (Text)
import           Data.Text.Lazy.Builder     (Builder, fromText)
import           Data.Text.Lazy.Builder.Int (decimal)

data Temp
    = Temp !Int
    | FP
    | RV
    deriving Eq

tempBuilder :: Temp -> Builder
tempBuilder (Temp t) = "t" <> decimal t
tempBuilder FP       = "FP"
tempBuilder RV       = "RV"

data Label
    = LabelInt !Int
    | LabelText !Text
    deriving Eq


instance Hashable Label where
    hashWithSalt salt = \case
        LabelInt i  -> hashWithSalt salt i
        LabelText s -> hashWithSalt salt s

labelBuilder :: Label -> Builder
labelBuilder (LabelInt l)  = "l" <> decimal l
labelBuilder (LabelText l) = fromText l

class Monad m => MonadTemp m where
    newTemp :: m Temp
    newLabel :: m Label

newtype TempM a = TempM (ReaderT Context IO a)
    deriving newtype (Functor, Applicative, Monad, MonadReader Context, MonadIO)

data Context = Context
    { ctxNextTemp  :: !(IORef Int)
    , ctxNextLabel :: !(IORef Int)
    }

instance MonadTemp TempM where
    newTemp = do
        nextTempRef <- asks ctxNextTemp
        t <- liftIO $ readIORef nextTempRef
        liftIO $ writeIORef nextTempRef (t + 1)
        pure $ Temp t
    newLabel = do
        nextLabelRef <- asks ctxNextLabel
        l <- liftIO $ readIORef nextLabelRef
        liftIO $ writeIORef nextLabelRef (l + 1)
        pure $ LabelInt l

newtype InitTemp = InitTemp Int

newtype InitLabel = InitLabel Int

runTempM :: InitTemp -> InitLabel -> TempM a -> IO a
runTempM (InitTemp initTemp) (InitLabel initLabel) (TempM ma) = do
    ctxNextTemp <- liftIO $ newIORef initTemp
    ctxNextLabel <- liftIO $ newIORef initLabel
    let ctx = Context {..}
    runReaderT ma ctx

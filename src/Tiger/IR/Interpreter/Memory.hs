{-# LANGUAGE ScopedTypeVariables #-}

module Tiger.IR.Interpreter.Memory
    ( Memory
    , memoryBaseAddr
    , newMemory
    , allocMemory
    , readMemory
    , writeMemory

    , Stack
    , stackBaseAddr
    , newStack
    , allocStack
    , reduceStack
    , readStack
    , writeStack

    , GrowVector
    , newGrowVector
    , readGrowVector
    , writeGrowVector

    , Size(..)
    , Address(..)
    ) where

import           Control.Monad               (void, when)
import           Control.Monad.IO.Class      (MonadIO (liftIO))
import           Data.Bits                   (Bits (shiftL), FiniteBits (..))
import           Data.IORef                  (IORef, newIORef, readIORef, writeIORef)
import           Data.Vector.Unboxed         (MVector, Unbox)

import qualified Data.Vector.Unboxed.Mutable as Vec

newtype Memory a = Memory { getMemory :: GenericMemory a }

newtype Stack a = Stack  { getStack :: GenericMemory a }

data GenericMemory a = GenericMemory
    { memVector   :: !(IORef (MVector Vec.RealWorld a))
    , memBaseAddr :: !Address
    , memSize     :: !(IORef Size)
    , memWordSize :: !Int
    }

memoryBaseAddr :: Memory a -> Address
memoryBaseAddr = memBaseAddr . getMemory

stackBaseAddr :: Stack a -> Address
stackBaseAddr = memBaseAddr . getStack

newtype Size = Size Int deriving newtype (Eq, Ord, Show)

newtype Address = Address { getAddress :: Int } deriving newtype (Eq, Ord, Show)

newMemory :: forall a m. (Unbox a, FiniteBits a, Num a, MonadIO m)
          => Size
          -> Address
          -> m (Memory a)
newMemory size baseAddr = Memory <$> newGenericMemory size baseAddr

newStack :: forall a m. (Unbox a, FiniteBits a, Num a, MonadIO m)
         => Size
         -> Address
         -> m (Stack a)
newStack size baseAddr = Stack <$> newGenericMemory size baseAddr

newGenericMemory :: forall a m. (Unbox a, FiniteBits a, Num a, MonadIO m)
                 => Size
                 -> Address
                 -> m (GenericMemory a)
newGenericMemory (Size size) baseAddr = liftIO $ do
    vec <- Vec.new actualSize
    vecRef <- newIORef vec
    sizeRef <- newIORef (Size 0)
    pure $ GenericMemory { memVector   = vecRef
                         , memBaseAddr = baseAddr
                         , memSize     = sizeRef
                         , memWordSize = wordSize
                         }
  where Size actualSize = initSize (Size $ size `div` wordSize)
        wordSize = finiteBitSize (fromIntegral @Int @a 0) `div` 8

allocMemory :: forall a m. (Unbox a, MonadIO m)
            => Memory a
            -> Size
            -> m Address
allocMemory (Memory mem@GenericMemory{..}) size = do
    Size oldSize <- liftIO $ readIORef memSize
    void $ allocGenericMemory mem size
    pure $ Address $ getAddress memBaseAddr + oldSize * memWordSize

allocStack :: forall a m. (Unbox a, MonadIO m)
           => Stack a
           -> Size
           -> m Address
allocStack (Stack mem@GenericMemory{..}) size = do
    Size newSize <- allocGenericMemory mem size
    pure $ Address $ getAddress memBaseAddr - newSize * memWordSize

allocGenericMemory :: forall a m. (Unbox a, MonadIO m)
                   => GenericMemory a
                   -> Size
                   -> m Size
allocGenericMemory (GenericMemory{..}) (Size numBytes) = liftIO $ do
    vec <- readIORef memVector
    let vecSize = Vec.length vec
    Size size <- readIORef memSize
    let newSize = size + numWords
    writeIORef memSize (Size newSize)
    when (newSize > vecSize) $ do
        newVec <- Vec.grow vec (growSize vecSize newSize)
        writeIORef memVector newVec
    pure $ Size newSize
  where numWords = getNumWords numBytes memWordSize

reduceStack :: forall a m. (Unbox a, MonadIO m)
            => Stack a
            -> Size
            -> m ()
reduceStack (Stack GenericMemory{..}) (Size numBytes) = liftIO $ do
    Size size <- readIORef memSize
    let newSize = size - numWords
    when (newSize < 0) $ error "Reduce unallocated stack"
    writeIORef memSize (Size newSize)
  where numWords = getNumWords numBytes memWordSize

getNumWords :: Int -> Int -> Int
getNumWords numBytes wordSize
    | remainder == 0 = res
    | otherwise      = error "size is not a multiple of a word size"
  where (res, remainder) = numBytes `divMod` wordSize

readMemory :: (Unbox a, MonadIO m) => Memory a -> Address -> m a
readMemory (Memory mem) addr = readGenericMemory mem (memIndex mem addr)

readStack :: (Unbox a, MonadIO m) => Stack a -> Address -> m a
readStack (Stack stack) addr = readGenericMemory stack (stackIndex stack addr)

readGenericMemory :: (Unbox a, MonadIO m) => GenericMemory a -> Int -> m a
readGenericMemory mem@GenericMemory{..} idx = liftIO $ do
    checkMemoryBounds mem idx
    vec <- readIORef memVector
    Vec.unsafeRead vec idx

writeMemory :: (Unbox a, MonadIO m) => Memory a -> Address -> a -> m ()
writeMemory (Memory mem) addr = writeGenericMemory mem (memIndex mem addr)

writeStack :: (Unbox a, MonadIO m) => Stack a -> Address -> a -> m ()
writeStack (Stack stack) addr = writeGenericMemory stack (stackIndex stack addr)

writeGenericMemory :: (Unbox a, MonadIO m) => GenericMemory a -> Int -> a -> m ()
writeGenericMemory mem@GenericMemory{..} idx x = liftIO $ do
    checkMemoryBounds mem idx
    vec <- readIORef memVector
    Vec.unsafeWrite vec idx x

checkMemoryBounds :: (Unbox a, MonadIO m) => GenericMemory a -> Int -> m ()
checkMemoryBounds GenericMemory{..} idx
    | idx < 0   = unallocatedMemoryAccess
    | otherwise = do
          Size size <- liftIO $ readIORef memSize
          when (idx >= size) $ unallocatedMemoryAccess
  where unallocatedMemoryAccess = error "unallocated memory access"

memIndex :: GenericMemory a -> Address -> Int
memIndex GenericMemory{..} (Address addr) = wordIndex wordOffset memWordSize
  where wordOffset = addr - getAddress memBaseAddr

stackIndex :: GenericMemory a -> Address -> Int
stackIndex GenericMemory{..} (Address addr) = wordIndex wordOffset memWordSize
  where wordOffset = getAddress memBaseAddr - addr - memWordSize

wordIndex :: Int -> Int -> Int
wordIndex numWords wordSize
    | remainder == 0  = res
    | otherwise       = error "Trying to access not word size multiple address"
  where (res, remainder) = numWords `divMod` wordSize

newtype GrowVector a = GrowVector (IORef (MVector Vec.RealWorld a))

newGrowVector :: (Unbox a, MonadIO m) => Size -> m (GrowVector a)
newGrowVector size = liftIO $ do
    vec <- Vec.new actualSize
    vecRef <- newIORef vec
    pure $ GrowVector vecRef
  where Size actualSize = initSize size

readGrowVector :: (Unbox a, MonadIO m) => GrowVector a -> Int -> m a
readGrowVector (GrowVector vecRef) idx = liftIO $ do
    vec <- readIORef vecRef
    Vec.read vec idx

writeGrowVector :: (Unbox a, MonadIO m) => GrowVector a -> Int -> a -> m ()
writeGrowVector (GrowVector vecRef) idx x = liftIO $ do
    vec <- readIORef vecRef
    let vecSize = Vec.length vec
    if vecSize > idx
       then Vec.write vec idx x
       else do
           newVec <- Vec.grow vec (growSize vecSize $ idx + 1)
           writeIORef vecRef newVec
           Vec.write newVec idx x

growSize :: Int -> Int -> Int
growSize len newLen = growSize' (len * 2) - len
  where growSize' curLen
          | curLen >= newLen = curLen
          | otherwise        = growSize' (curLen * 2)

initSize :: Size -> Size
initSize (Size size) = Size $ max (1 `shiftL` (logBase2 size + 1)) minSize
  where minSize = 16

logBase2 :: Int -> Int
logBase2 x = finiteBitSize x - 1 - countLeadingZeros x

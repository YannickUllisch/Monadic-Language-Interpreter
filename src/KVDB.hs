-- | Key-value database.
module KVDB
  ( KVDB,
    startKVDB,
    kvGet,
    kvPut,
  )
where

import Control.Monad (forM_, forever, liftM, ap)
import GenServer

-- | Server State
data KVDBState k v = KVDBState
  { kvPairs :: [(k, v)],
    pendingGets :: [(k, ReplyChan v)]
  }

-- | KVDBM monad for more optimal KVDB state manipulation..
-- I decided to create a monadic state to make code more readable and more easily maintainable
-- technically it isn't needed for a very simple key value storage server, but nice to have. 
newtype KVDBM k v a = KVDBM (KVDBState k v -> IO (a, KVDBState k v))

instance Functor (KVDBM k v) where
  fmap = liftM

instance Applicative (KVDBM k v) where
  pure a = KVDBM $ \s -> pure (a, s)
  (<*>) = ap

instance Monad (KVDBM k v) where
  KVDBM m >>= f = KVDBM $ \s -> do
    (a, s') <- m s
    let KVDBM f' = f a
    f' s'

-- | Running the KVDB Monad 
runKVDBM :: KVDBState k v -> KVDBM k v a -> IO a
runKVDBM s (KVDBM f) = do
  (res, _) <- f s
  pure res

-- | Lift an IO action into the KVDBM monad.
io :: IO a -> KVDBM k v a
io x = KVDBM $ \s -> do
  res <- x
  pure (res, s)

-- | Get the current state.
get :: KVDBM k v (KVDBState k v)
get = KVDBM $ \s -> pure (s, s)

-- | Replace the current state with a new state.
put :: KVDBState k v -> KVDBM k v ()
put s = KVDBM $ \_ -> pure ((), s)

-- | A reference to a KVDB instance that stores keys of type 'k' and
-- corresponding values of type 'v'.
data KVDB k v = KVDB (Server (KVDBMessage k v))

-- | Messages for KVDB
data KVDBMessage k v
  = MsgGet k (ReplyChan v)
  | MsgPut k v

checkWaiting :: (Eq k) => k -> KVDBM k v ()
checkWaiting key = do
  state <- get
  let waiters = filter ((== key) . fst) $ pendingGets state

  case lookup key $ kvPairs state of
    Just val -> do
      forM_ waiters $ \(_, rpc) -> io $ reply rpc val
      let updatedPendingGets = filter ((/= key) . fst) $ pendingGets state
      put $ state { pendingGets = updatedPendingGets }
    Nothing -> pure ()

-- | Server loop for handling KVDB requests.
serverLoop :: (Eq k) => Chan (KVDBMessage k v) -> KVDBM k v ()
serverLoop c = do
  msg <- io $ receive c
  case msg of
    MsgGet key rpc -> do
      state <- get
      case lookup key $ kvPairs state of
        Just v -> io $ reply rpc v
        Nothing -> do
          put $ state {
            pendingGets = (key, rpc) : pendingGets state
          }
    MsgPut key val -> do
      state <- get
      put $ state { 
        kvPairs = (key, val) : kvPairs state
      }
      checkWaiting key

-- REQUIRED API FUNCTIONS BELOW

-- | Start a new KVDB instance.
startKVDB :: (Ord k) => IO (KVDB k v)
startKVDB = do
  let init_state = KVDBState { kvPairs = [], pendingGets = [] }
  server <- spawn $ \c -> runKVDBM init_state $ forever $ serverLoop c
  pure $ KVDB server

-- | Retrieve the value corresponding to a given key. If that key does
-- not exist in the store, then this function blocks until another
-- thread writes the desired key with 'kvPut', after which this
-- function returns the now available value.
kvGet :: (Eq k) => KVDB k v -> k -> IO v
kvGet (KVDB s) key = requestReply s $ MsgGet key

-- | Write a key-value mapping to the database. Replaces any prior
-- mapping of the key.
kvPut :: (Eq k) => KVDB k v -> k -> v -> IO ()
kvPut (KVDB s) key val = sendTo s $ MsgPut key val 

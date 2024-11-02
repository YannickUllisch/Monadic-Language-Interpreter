{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
-- | Key-value database.
module KVDB
  ( KVDB,
    startKVDB,
    kvGet,
    kvPut,
  )
where

import Control.Monad (forM_, forever, liftM, ap)
import Data.List (partition)
import GenServer

-- KVDB initial server setup

-- | Messages for KVDB
data KVDBMsg k v
  = MsgGet k (ReplyChan v)
  | MsgPut k v

-- | A reference to a KVDB instance that stores keys of type 'k' and
-- corresponding values of type 'v'.
data KVDB k v = KVDB (Server (KVDBMsg k v))

-- | Our KVDB Server State
-- Consists of key value pairs the server keeps track of
-- and a list of waiters, that wait on a specific key becoming available
data KVDBState k v = KVDBState
  { kvdbKVPairs :: [(k, v)],
    kvdbWaiting :: [(k, ReplyChan v)]
  } 

-- KVDBM Monad setup for state handling

-- | KVDBM monad for more optimal KVDB state manipulation..
-- I decided to create a monadic state to make code more readable and more easily maintainable
-- technically it isn't needed for a very simple key value storage server, but nice to have. 
newtype KVDBM k v a = KVDBM (KVDBState k v -> IO (a, KVDBState k v))

instance Functor (KVDBM k v) where
  fmap = liftM

instance Applicative (KVDBM k v) where
  pure x = KVDBM $ \s -> pure (x, s)
  (<*>) = ap

instance Monad (KVDBM k v) where
  KVDBM m >>= f = KVDBM $ \s -> do
    (x, s') <- m s
    let KVDBM f' = f x
    f' s'

-- | Run the KVDB monad
runKVDBM :: KVDBState k v -> KVDBM k v a -> IO a
runKVDBM s (KVDBM f) = do
  (x, _) <- f s
  pure x

-- | Lift IO action into KVDBM monad.
io :: IO a -> KVDBM k v a
io x = KVDBM $ \s -> do
  r <- x
  pure (r, s)

-- | Access current state
get :: KVDBM k v (KVDBState k v)
get = KVDBM $ \s -> pure (s, s)

-- | Replaces current state with new given state
put :: KVDBState k v -> KVDBM k v ()
put s = KVDBM $ \_ -> pure ((), s)

-- | Server loop for handling KVDB requests.
serverLoop :: (Eq k) => Chan (KVDBMsg k v) -> KVDBM k v ()
serverLoop c = do
  msg <- io $ receive c
  case msg of
    MsgGet key rpc -> do
      state <- get
      case lookup key $ kvdbKVPairs state of
        Just v -> io $ reply rpc v
        Nothing -> do
          put $ state {
            kvdbWaiting = (key, rpc) : kvdbWaiting state
          }
    MsgPut key val -> do
      state <- get
      let (waiters, rest) = partition ((== key) . fst) $ kvdbWaiting state
      -- We update state before replying, since we could otherwise get in trouble when
      -- a new thread tries to retrieve the given key while we respond to all waiters.
      -- In that case it would be added to waiters, not be considered here and not get a reply until the same 
      -- key is updated again. (Would probably just be an issue for larger systems with many waiters)
      put $ state { kvdbWaiting = rest, kvdbKVPairs = (key, val) : kvdbKVPairs state }
      forM_ waiters $ \(_, rpc) -> io $ reply rpc val


-- REQUIRED API FUNCTIONS BELOW

-- | Start a new KVDB instance.
startKVDB :: (Ord k) => IO (KVDB k v)
startKVDB = do
  let init_state = KVDBState { kvdbKVPairs = [], kvdbWaiting = [] }
  server <- spawn $ \c -> runKVDBM init_state $ forever $ serverLoop c
  pure $ KVDB server

-- | Retrieve the value corresponding to a given key. If that key does
-- not exist in the store, then this function blocks until another
-- thread writes the desired key with 'kvPut', after which this
-- function returns the now available value.
-- | We add constraint to the key type, to be
kvGet :: (Eq k) =>  KVDB k v -> k -> IO v
kvGet (KVDB s) key = requestReply s $ MsgGet key

-- | Write a key-value mapping to the database. Replaces any prior
-- mapping of the key.
kvPut :: (Eq k) => KVDB k v -> k -> v -> IO ()
kvPut (KVDB s) key val = sendTo s $ MsgPut key val 

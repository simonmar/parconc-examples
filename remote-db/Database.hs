module Database (
       Database,
       Key, Value,
       createDB,
       get, set,
       rcdata,
  ) where

import qualified Data.Map as Map
import Data.Map (Map)
import Remote

type Key   = String
type Value = String

type Database = ProcessId

createDB :: ProcessM Database
createDB = error "not implemented!" -- exercise 5.1

set :: Database -> Key -> Value -> ProcessM ()
set db k v = error "not implemented!" -- exercise 5.1

get :: Database -> Key -> ProcessM (Maybe Value)
get db k = error "not implemented!" -- exercise 5.1

rcdata = [] -- exercise 5.1: you will need to add any necessary
            -- __remoteCallMetadata to this list, e.g.
            -- [Database.__remoteCallMetadata]

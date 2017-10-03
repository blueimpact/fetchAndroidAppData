module DB where

import Message
import Protolude hiding (toList)
import Data.Time

import Control.Applicative (Applicative, (<$>))
import Control.Lens (Lens', lens, (^.), (%%~))
import Control.Monad.Haskey
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.BTree.Alloc (AllocM, AllocReaderM)
import Data.BTree.Impure (Tree, insertTree, lookupTree, toList)
import Data.BTree.Primitives (Value, Key)
import Data.Binary (Binary)
import Data.Foldable (foldlM)
import Data.Int (Int64)
import Data.Text (Text, unpack)
import Data.Typeable (Typeable)
import qualified Data.BTree.Impure as Tree

import Database.Haskey.Alloc.Concurrent (Root)

import GHC.Generics (Generic)
import Data.Binary.Orphans

type RankingList = [(Rank,AppDetails)]
type RankingTree = Tree RankingListId RankingList

data SchemaTable = JP | EN

data Schema = Schema {
    _schemaJpRankings :: RankingTree
  , _schemaEnRankings :: RankingTree
  } deriving (Generic, Show, Typeable)

instance Binary Schema
instance Value Schema
instance Root Schema

instance Key RankingListId
instance Value RankingListId
instance Binary RankingListId

instance Value Rank
instance Binary Rank
instance Value AppDetails
instance Binary AppDetails

emptySchema :: Schema
emptySchema = Schema Tree.empty Tree.empty

schemaJpRankings :: Lens' Schema (RankingTree)
schemaJpRankings = lens _schemaJpRankings $ \s x -> s { _schemaJpRankings = x }

schemaEnRankings :: Lens' Schema (RankingTree)
schemaEnRankings = lens _schemaEnRankings $ \s x -> s { _schemaEnRankings = x }

insertRankingData :: AllocM n
  => SchemaTable
  -> UTCTime
  -> RankingList
  -> Schema -> n Schema
insertRankingData tbl k v =
  f %%~ insertTree (RankingListId k) v
  where f = case tbl of
              JP -> schemaJpRankings
              EN -> schemaEnRankings

-- | Query all
queryAllRankingsLists :: AllocReaderM n
  => SchemaTable
  -> Schema
  -> n [(RankingListId, RankingList)]
queryAllRankingsLists tbl root =
  toList (root ^. f)
  where f = case tbl of
              JP -> schemaJpRankings
              EN -> schemaEnRankings

-- | Query a list
queryList :: AllocReaderM n
  =>SchemaTable
  -> RankingListId
  -> Schema
  -> n (Maybe RankingList)
queryList tbl k root =
  lookupTree k (root ^. f)
  where f = case tbl of
              JP -> schemaJpRankings
              EN -> schemaEnRankings

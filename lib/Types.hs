{-# LANGUAGE GADTs #-}
module Types
  (
  PathMap,
  FIFOSet,
  fifoQueue,
  fifoSet,
  enqueue,
  dequeue,
  newFIFOSet,
  Action(..),
  Sort(..)
  )
where
import           Data.Sequence (Seq ((:|>)), ViewL (EmptyL, (:<)), viewl)
import           Data.Set      (Set, delete, insert, member)
import           Prelude       hiding (log)

data Action = Global | Matched deriving (Eq, Ord)

type PathMap = FilePath -> FilePath

data FIFOSet a where
  FIFOSet :: {
                 fifoQueue :: Seq a,
                 fifoSet :: Set a
             } -> FIFOSet a

data Sort = Name | PercentComplete | Downloaded | DownloadSpeed | Uploaded | UploadSpeed | ETA | Ratio | TotalSize
  | Peers | Seeds | DateAdded | Labels deriving (Enum, Eq)

enqueue :: Ord a => a -> FIFOSet a -> FIFOSet a
enqueue x f@(FIFOSet q s)
  | x `member` s = f
  | otherwise = FIFOSet (q :|> x) (insert x s)

dequeue :: Ord a => FIFOSet a -> Maybe (a, FIFOSet a)
dequeue (FIFOSet q s) = case viewl q of
                                EmptyL -> Nothing
                                (x :< rest) -> Just (x, FIFOSet rest $ delete x s)

newFIFOSet :: Ord a => FIFOSet a
newFIFOSet = FIFOSet mempty mempty

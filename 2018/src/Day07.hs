module Day07 where

import Text.ParserCombinators.Parsec
import Data.List (find)
import qualified Data.Map as M

data SleighTask = SleighTask {
  taskId :: TaskId,
  deps :: [Char]
} deriving (Eq, Show)

data SleighInst = SleighInst {
  taskIdHrgh :: Char,
  dep :: Char
} deriving (Eq, Show)

taskFile :: GenParser Char st [SleighInst]
taskFile = do
  result <- many task
  eof
  return result

task :: GenParser Char st SleighInst
task = do
  string "Step "
  taskId <- upper
  string " must be finished before step "
  dep <- upper
  string " can begin."
  optional $ char '\n' --eol
  return SleighInst { taskIdHrgh=taskId, dep=dep }

-- parseInput :: String -> Either ParseError [SleighInst]
parseInput = parse taskFile "goobers"

type SleighTasks = M.Map Char SleighTask
type Dependency = Char
type TaskId = Char

-- For some reason, B and C aren't showing up
instsToTasks :: [SleighInst] -> M.Map TaskId [Dependency]
-- instsToTasks' is = fmap addDep is
instsToTasks = foldr (
    \inst taskMap -> 
      let 
          dependency = dep inst
          addDep1 = addDepAsEntry dependency
          addDep0 = addDepToEntry dependency (taskIdHrgh inst)
      in (addDep1 . addDep0) taskMap 
  ) M.empty

addDepToEntry ::  TaskId -> Dependency -> M.Map TaskId [Dependency] -> M.Map TaskId [Dependency]
addDepToEntry key dep m = if M.member key m then M.insertWith (++) key [dep] m else M.insertWith (++) key [] m

-- TODO Do we need insertWith here? Maybe just an insert is enough
addDepAsEntry :: Dependency -> M.Map TaskId [Dependency] -> M.Map TaskId [Dependency]
addDepAsEntry dep m = if M.member dep m then m else M.insertWith (++) dep [] m

taskByChar :: Char -> SleighTask -> Bool
taskByChar c t = taskId t == c

instToTask :: SleighInst -> SleighTask 
instToTask inst = SleighTask { taskId=taskIdHrgh inst, deps=[dep inst] }

-- processTasks :: SleighTasks -> [Char]
-- processTasks ts = M.fold (\elem str -> ) "" ts

getWithNoDeps :: SleighTasks -> SleighTasks
getWithNoDeps ts = M.foldr (\elem m ->
  case length (deps elem) of
     0 -> M.insert (taskId elem) elem m
     _ -> m
  ) M.empty ts
-- insert :: Ord k => k -> a -> Map k a -> Map k a

testInput = "Step C must be finished before step A can begin.\
\Step C must be finished before step F can begin.\
\Step A must be finished before step B can begin.\
\Step A must be finished before step D can begin.\
\Step B must be finished before step E can begin.\
\Step D must be finished before step E can begin.\
\Step F must be finished before step E can begin."

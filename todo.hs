import System.IO
import System.Directory
import System.Environment
import Data.List

main = do
  (command:args) <- getArgs
  let (Just action) = lookup command dispatch
  action args

dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("add", add)
           , ("view", view)
           , ("remove", remove)
           , ("bump", bump)
           ]

add :: [String] -> IO ()
add [fileName, todoItem] = do
  appendFile fileName (todoItem ++ "\n")
  view [fileName]

view :: [String] -> IO ()
view [fileName] = do
  contents <- readFile fileName
  let todoTasks = lines contents
      numberedTasks = zipWith (\n t -> show n ++ " - " ++ t) [0..] todoTasks
  putStrLn "Here are your todo items:"
  mapM_ putStrLn numberedTasks

remove :: [String] -> IO ()
remove args = do
  modify args removeTask
  where removeTask [] _ = []
        removeTask tasks n = delete (tasks !! n) tasks

bump :: [String] -> IO ()
bump args = do
  modify args bumpTask
  where bumpTask [] _ = []
        bumpTask tasks n = selected:(delete selected tasks)
            where selected = tasks !! n

modify :: [String] -> ([String] -> Int -> [String]) -> IO ()
modify [fileName, numberString] proc = do
  handle <- openFile fileName ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- hGetContents handle
  let number = read numberString :: Int
      todoTasks = lines contents
      newTodoItems = proc todoTasks number
  hPutStr tempHandle $ unlines newTodoItems
  hClose handle
  hClose tempHandle
  removeFile fileName
  renameFile tempName fileName
  view [fileName]
-- based on the Learn You a Haskell program with an additional "bump" function added.

import System.Environment
import System.Directory
import System.IO
import Data.List
import Control.Exception

dispatch :: String -> [String] -> IO ()
dispatch "add"    = add
dispatch "view"   = view
dispatch "remove" = remove
dispatch "bump"   = bump

main = do
    (command:argList) <- getArgs
    dispatch command argList

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

view :: [String] -> IO ()
view [fileName] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line)
                        [0..] todoTasks
    putStr $ unlines numberedTasks

remove :: [String] -> IO ()
remove [fileName, numberString] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStrLn "These are your TO-DO items:"
    mapM_ putStrLn numberedTasks
    let number = read numberString
        newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
    bracketOnError (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            hPutStr tempHandle newTodoItems
            hClose tempHandle
            removeFile "dolist.txt"
            renameFile tempName "dolist.txt")

bump :: [String] -> IO ()
bump [fileName, numberString] = do
     contents <- readFile fileName
     let todoTasks = lines contents
     let number = read numberString
     let bumpedTask = lines $ todoTasks !! number
     let oldTasksWithoutBump = delete (todoTasks !! number) todoTasks
     let newTaskswithBump = bumpedTask ++ oldTasksWithoutBump
     let sTnewTaskswithBump = unlines newTaskswithBump
     let newnumberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] newTaskswithBump
     putStrLn "Go do these things:"
     mapM_ putStrLn newnumberedTasks
     bracketOnError (openTempFile "." "temp")
         (\(tempName, tempHandle) -> do
             hClose tempHandle
             removeFile tempName)
         (\(tempName, tempHandle) -> do
             hPutStr tempHandle sTnewTaskswithBump
             hClose tempHandle
             removeFile "dolist.txt"
             renameFile tempName "dolist.txt")

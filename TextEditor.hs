import Safe
import Data.List.Index
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Data.Maybe
import System.IO

type Line = String
type Buffer = [Line] -- TODO array
type EditorMonad a = StateT Buffer IO a -- TODO IO??
type LineNum = Int
type CharNum = Int
type Coord = (LineNum, CharNum)
data LongCmd = LongCmd { runLongCmd :: Char -> Either LongCmd (EditorMonad ()) }
data CmdState = CmdFail | CmdShort (EditorMonad ()) | CmdLong LongCmd
type CmdsList = [Char -> CmdState]

getBuffer :: EditorMonad Buffer
getBuffer = get

getBufferF :: (Buffer -> a) -> EditorMonad a
getBufferF = (<$> getBuffer)

modifyBuffer :: (Buffer -> Buffer) -> EditorMonad ()
modifyBuffer = modify 

setBuffer :: Buffer -> EditorMonad ()
setBuffer = modifyBuffer . const

bufferGetLine :: Int -> Buffer -> Maybe Line
bufferGetLine = flip atMay

bufferModifyLine :: Int -> (Line -> Line) -> Buffer -> Buffer
bufferModifyLine = modifyAt

bufferSetLine :: Int -> Line -> Buffer -> Buffer
bufferSetLine = setAt

getLine :: Int -> EditorMonad (Maybe Line)
getLine = getBufferF . bufferGetLine

modifyLine :: Int -> (Line -> Line) -> EditorMonad ()
modifyLine n f = modifyBuffer $ bufferModifyLine n f

setLine :: Int -> Line -> EditorMonad ()
setLine n l = modifyBuffer $ bufferSetLine n l

cmdsList :: CmdsList
cmdsList = []

editorLongCmdInProgress :: LongCmd -> EditorMonad ()
editorLongCmdInProgress f = lift getChar >>= (either editorLongCmdInProgress id . runLongCmd f)

editorHelper :: Char -> CmdsList -> EditorMonad ()
editorHelper c [] = lift $ putStrLn $ " No command found for " <> show c
editorHelper c (a:b) = case (a c) of
  CmdFail -> editorHelper c b
  (CmdShort m) -> m
  (CmdLong f) -> editorLongCmdInProgress f

editor :: CmdsList -> EditorMonad ()
editor allCmds = do
  chr <- lift getChar
  editorHelper chr allCmds
  editor allCmds

main = do
  hSetBuffering stdin NoBuffering
  runStateT (editor cmdsList) []

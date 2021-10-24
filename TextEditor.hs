import Safe
import Data.List.Index
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Data.Maybe
import Data.List
import System.IO
import qualified Data.Map as M

type Line = String -- TODO array
type Buffer = [Line] -- TODO array
type EditorMonad a = StateT Buffer IO a -- TODO IO??
type LineNum = Int
type CharNum = Int
type Coord = (LineNum, CharNum)
data LongCmd = LongCmd { runLongCmd :: Char -> Either LongCmd (EditorMonad ()) }
data Cmd = CmdShort (EditorMonad ()) | CmdLong LongCmd
type CmdsList = M.Map Char Cmd -- TODO IntMap

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

getLine :: Int -> EditorMonad (Maybe Line)
getLine = getBufferF . bufferGetLine

modifyLine :: Int -> (Line -> Line) -> EditorMonad ()
modifyLine n f = modifyBuffer $ bufferModifyLine n f

setLine :: Int -> Line -> EditorMonad ()
setLine n l = modifyBuffer $ bufferSetLine n l

bufferModifyLine :: Int -> (Line -> Line) -> Buffer -> Buffer
bufferModifyLine = modifyAt

bufferSetLine :: Int -> Line -> Buffer -> Buffer
bufferSetLine = setAt

-- not gonna do instance Show Buffer because read . show should be id
showBuffer :: Buffer -> String
showBuffer = intercalate "\n"

zCmd :: Cmd
zCmd = CmdShort $ getBufferF (('\n' : ) . showBuffer) >>= lift . putStrLn

cmdsList :: CmdsList
cmdsList = M.fromList [('z',zCmd)]

editorLongCmdInProgress :: LongCmd -> EditorMonad ()
editorLongCmdInProgress f = lift getChar >>= (either editorLongCmdInProgress id . runLongCmd f)

editorHelper :: Char -> CmdsList -> EditorMonad ()
editorHelper c s = maybe failed success $ M.lookup c s where
  failed = lift $ putStrLn $ " No command found for " <> show c
  success (CmdShort m) = m
  success (CmdLong  f) = editorLongCmdInProgress f

editor :: CmdsList -> EditorMonad ()
editor allCmds = do
  chr <- lift getChar
  editorHelper chr allCmds
  editor allCmds

main = do
  hSetBuffering stdin NoBuffering
  runStateT (editor cmdsList) ["test line 1","test line 2"]

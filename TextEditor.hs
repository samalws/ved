import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Data.Maybe
import Data.List
import Data.List.Index
import Safe
import System.IO
import qualified Data.Map as M

type Line = String -- TODO array
type Buffer = [Line] -- TODO array
type LineNum = Int
type CharNum = Int
type Coord = (LineNum, CharNum)
data StateInfo = StateInfo { stateBuffer :: Buffer, stateCoord :: Coord }
type EditorMonad a = StateT StateInfo IO a -- TODO IO??
data Cmd = CmdShort (EditorMonad ()) | CmdLong LongCmd
data LongCmd = LongCmd { runLongCmd :: Char -> Either LongCmd (EditorMonad ()) }
type CmdsList = M.Map Char Cmd -- TODO IntMap

getBuffer :: EditorMonad Buffer
getBuffer = stateBuffer <$> get

getCoord :: EditorMonad Coord
getCoord = stateCoord <$> get

modifyBuffer :: (Buffer -> Buffer) -> EditorMonad ()
modifyBuffer f = modify (\s -> s { stateBuffer = f $ stateBuffer s })

modifyCoord :: (Coord -> Coord) -> EditorMonad ()
modifyCoord f = modify (\s -> s { stateCoord = f $ stateCoord s })

setBuffer :: Buffer -> EditorMonad ()
setBuffer = modifyBuffer . const

setCoord :: Coord -> EditorMonad ()
setCoord = modifyCoord . const

bufferGetLine :: Int -> Buffer -> Maybe Line
bufferGetLine = flip atMay

getLine :: Int -> EditorMonad (Maybe Line)
getLine = (<$> getBuffer) . bufferGetLine

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
zCmd = CmdShort $ ((('\n' : ) . showBuffer) <$> getBuffer) >>= lift . putStrLn

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
  runStateT (editor cmdsList) (StateInfo { stateBuffer = ["test line 1","test line 2"], stateCoord = (0,0) })

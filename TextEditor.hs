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
-- TODO maybe carriage return to write over the same stuff

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

getBufLine :: Int -> EditorMonad (Maybe Line)
getBufLine = (<$> getBuffer) . bufferGetLine

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

cmd_z :: EditorMonad ()
cmd_z = ((('\n' : ) . showBuffer) <$> getBuffer) >>= lift . putStrLn

cmd_p :: EditorMonad ()
cmd_p = do
  coord <- getCoord
  line  <- getBufLine $ fst coord
  lift $ putStrLn $ '\n':(maybe "" id line)
  lift $ putStrLn $ replicate (snd coord) ' ' <> "^"

cmd_n :: EditorMonad ()
cmd_n = do
  coord <- getCoord
  line  <- getBufLine $ fst coord
  lift $ putStrLn $ "\n" <> (show $ fst coord) <> " " <> maybe "" id line
  lift $ putStrLn $ replicate (snd coord + length (show $ fst coord) + 1) ' ' <> "^"

cmd_posthl = cmd_p -- todo
cmd_postjk = cmd_n -- todo

cmd_hjkl :: Coord -> EditorMonad ()
cmd_hjkl (a,b) = do
  modifyCoord (\(x,y) -> (x+a,y+b))
  if a==0 then cmd_posthl else cmd_postjk

cmdsList :: CmdsList
cmdsList = M.fromList [
  ('z',CmdShort cmd_z),
  ('p',CmdShort cmd_p),
  ('n',CmdShort cmd_n),
  ('h',CmdShort $ cmd_hjkl ( 0,-1)),
  ('j',CmdShort $ cmd_hjkl ( 1, 0)),
  ('k',CmdShort $ cmd_hjkl (-1, 0)),
  ('l',CmdShort $ cmd_hjkl ( 0, 1))
  ]

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

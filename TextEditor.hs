import Safe
import Data.List.Index
import Control.Monad.State

type Line = String
type Buffer = [Line] -- TODO array
type EditState a = State Buffer a
type LineNum = Int
type CharNum = Int
type Coord = (LineNum, CharNum)

getBuffer :: EditState Buffer
getBuffer = get

getBufferF :: (Buffer -> a) -> EditState a
getBufferF = (<$> getBuffer)

modifyBuffer :: (Buffer -> Buffer) -> EditState ()
modifyBuffer = modify 

setBuffer :: Buffer -> EditState ()
setBuffer = modifyBuffer . const

bufferGetLine :: Int -> Buffer -> Maybe Line
bufferGetLine = flip atMay

bufferModifyLine :: Int -> (Line -> Line) -> Buffer -> Buffer
bufferModifyLine = modifyAt

bufferSetLine :: Int -> Line -> Buffer -> Buffer
bufferSetLine = setAt

getLine :: Int -> EditState (Maybe Line)
getLine = getBufferF . bufferGetLine

modifyLine :: Int -> (Line -> Line) -> EditState ()
modifyLine n f = modifyBuffer $ bufferModifyLine n f

setLine :: Int -> Line -> EditState ()
setLine n l = modifyBuffer $ bufferSetLine n l

main = pure ()

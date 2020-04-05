module SideProc where

import           Control.Concurrent           (ThreadId, forkIO, killThread,
                                               threadDelay)
import           Control.Concurrent.MVar      (newEmptyMVar, putMVar,
                                               tryTakeMVar)
import           Control.Concurrent.STM       (STM, atomically)
import           Control.Concurrent.STM.TChan (TChan, newTChanIO, tryReadTChan,
                                               writeTChan)
import           System.IO                    (Handle, hGetChar, hGetLine,
                                               hIsEOF, hReady, hWaitForInput)
import           System.Process               (ProcessHandle,
                                               runInteractiveCommand)

data SideProcess = SideProcess {
        _stdout      :: Handle,
        _stdout_chan :: TChan String,
        _stdout_tid  :: ThreadId,
        _stderr      :: Handle,
        _stderr_chan :: TChan String,
        _stderr_tid  :: ThreadId,
        _proc        :: ProcessHandle
        }

readLines :: TChan String -> IO [String]
readLines chan = do
    mb_val <- atomically $ tryReadTChan chan
    case mb_val of
        Just line -> do
            sub_lines <- readLines chan
            return (line:sub_lines)
        Nothing -> return []


runSideProcess :: String -> IO SideProcess
runSideProcess cmd = do
    (_, stdout, stderr, handle ) <- runInteractiveCommand cmd
    stdout_chan <- newTChanIO
    stdout_tid <- forkIO $ read_line stdout stdout_chan
    stderr_chan <- newTChanIO
    stderr_tid <- forkIO $ read_line stderr stderr_chan
    return SideProcess {_stdout = stdout, _stdout_chan = stdout_chan, _stdout_tid = stdout_tid, _stderr = stderr, _stderr_chan = stderr_chan, _stderr_tid = stderr_tid, _proc = handle}
    where
        read_line :: Handle -> TChan String -> IO()
        read_line hndl chan = do
            is_eof <- hIsEOF hndl
            if is_eof
            then return ()
            else do
                line <- hGetLine hndl
                atomically $ writeTChan chan line
                read_line hndl chan

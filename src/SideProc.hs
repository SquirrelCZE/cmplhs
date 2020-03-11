module SideProc where

import Control.Concurrent (forkIO, threadDelay, killThread)
import          Control.Concurrent.MVar (newEmptyMVar, tryTakeMVar, putMVar)
import           System.IO      (Handle, hGetLine, hIsEOF, hWaitForInput)
import           System.Process (ProcessHandle, runInteractiveCommand)

data SideProcess = SideProcess {
        _stdout :: Handle,
        _stderr :: Handle,
        _proc   :: ProcessHandle
        }

readLines :: Handle -> IO [String]
readLines hndl = do
    m_var <- newEmptyMVar
    tid <- forkIO $ do
        is_eof <- hIsEOF hndl
        if is_eof
        then return ()
        else do
            line <- hGetLine hndl
            putMVar m_var line

    threadDelay 20
    killThread tid

    mb_line <- tryTakeMVar m_var
    case mb_line of
        Just line -> do
            sub_lines <- readLines hndl
            return (line:sub_lines)
        Nothing -> return []


readLines2 :: Handle -> IO [String]
readLines2 hndl = do
    is_eof <- hIsEOF hndl
    if is_eof
    then return []
    else do
        has_input <- hWaitForInput hndl 20
        if not has_input
        then return []
        else do
            line <- hGetLine hndl
            sub <- readLines2 hndl
            return ( line : sub )

runSideProcess :: String -> IO SideProcess
runSideProcess cmd = do
    (_, stdout, stderr, handle ) <- runInteractiveCommand cmd
    return SideProcess {_stdout = stdout, _stderr = stderr, _proc = handle}

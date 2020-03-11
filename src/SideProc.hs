module SideProc where

import           System.IO      (Handle, hGetLine, hIsEOF, hWaitForInput)
import           System.Process (ProcessHandle, runInteractiveCommand)

data SideProcess = SideProcess {
        _stdout :: Handle,
        _stderr :: Handle,
        _proc   :: ProcessHandle
        }

readLines :: Handle -> IO [String]
readLines hndl = do
    is_eof <- hIsEOF hndl
    if is_eof
    then return []
    else do
        has_input <- hWaitForInput hndl 20
        if not has_input
        then return []
        else do
            line <- hGetLine hndl
            sub <- readLines hndl
            return ( line : sub )

runSideProcess :: String -> IO SideProcess
runSideProcess cmd = do
    (_, stdout, stderr, handle ) <- runInteractiveCommand cmd
    return SideProcess {_stdout = stdout, _stderr = stderr, _proc = handle}

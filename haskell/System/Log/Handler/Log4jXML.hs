{- arch-tag: log4j XMLLayout log handler
Copyright (C) 2004-2006 John Goerzen <jgoerzen@complete.org>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation; either version 2.1 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
-}

{- |
   Module     : System.Log.Handler.Log4jXML
   Copyright  : Copyright (C) 2004-2007 John Goerzen
   License    : GNU LGPL, version 2.1 or above

   Maintainer : bjorn.buckwalter@gmail.com
   Stability  : experimental
   Portability: GHC only?

log4j XMLLayout log handler

Written by Bjorn Buckwalter, bjorn.buckwalter\@gmail.com
-}

module System.Log.Handler.Log4jXML
    where

-- Base.
import Control.Concurrent (ThreadId, myThreadId)  -- myThreadId is GHC only!
import Control.Concurrent.MVar
import Data.List (isPrefixOf)
import System.IO
import System.Locale (defaultTimeLocale)
-- Time.
import Data.Time
-- hslogger.
import System.Log
import System.Log.Handler


-- | Log to a stream.
log4jXMLStreamHandler :: Handle -> Priority -> IO (GenericHandler Handle)
log4jXMLStreamHandler h pri = do
    lock <- newMVar ()
    let mywritefunc hdl (prio, msg) loggername =
         withMVar lock (\_ -> do 
            time   <- getCurrentTime
            thread <- myThreadId
            hPutStrLn h $ show 
                $ Log4jEvent loggername time prio thread (Log4jMessage msg)
            hFlush hdl
            )
    return (GenericHandler {priority = pri,
                            privData = h,
                            writeFunc = mywritefunc,
                            closeFunc = \x -> return ()})

-- | Log to a file.
log4jXMLFileHandler :: FilePath -> Priority -> IO (GenericHandler Handle)
log4jXMLFileHandler fp pri = do
                     h <- openFile fp AppendMode
                     lh <- log4jXMLStreamHandler h pri
                     return (lh{closeFunc = hClose})

{- The following is an example log message with log4j's XMLLayout:

<log4j:event logger="csa.Main" timestamp="1147868447298" level="DEBUG" thread="Timer-0">
<log4j:message><![CDATA[Doing Message Tasks : 0 tasks in queue.]]></log4j:message>
</log4j:event>

We define two data types to represent this log message. Perhaps we should
really use some fance XML library, especially to get the escaping right.
-}

-- | Data type representing a <log4j:message> element.
data Log4jMessage = Log4jMessage String
instance Show Log4jMessage where
    show (Log4jMessage msg) = 
        "<log4j:message>"
        ++ "<![CDATA[" ++ escapeCDATA msg ++ "]]>" -- TODO msg should be escaped!
        ++ "</log4j:message>"
        where
            -- | Escapes CDATA strings.
            escapeCDATA = replace "]]>" "]]&lt;"  -- The best we can do, I guess.

-- | Data type representing a <log4j:event> element.
data Log4jEvent = Log4jEvent String UTCTime Priority ThreadId Log4jMessage
instance Show Log4jEvent where
    show (Log4jEvent logger timestamp level thread message) =
        "<log4j:event"
        ++ " logger=\""    ++ escapeAttr logger             ++ "\""
        ++ " timestamp=\"" ++ escapeAttr (millis timestamp) ++ "\""
        ++ " level=\""     ++ escapeAttr (show level)       ++ "\""
        ++ " thread=\""    ++ escapeAttr (show thread)      ++ "\""
        ++ ">"
        ++ show message
        ++ "</log4j:event>"
        where
            -- | This is an ugly hack to get a unix epoch with milliseconds.
            -- The use of "take 3" causes the milliseconds to always be 
            -- rounded downwards, which I suppose may be the expected
            -- behaviour for time.
            millis t = formatTime defaultTimeLocale "%s" t
                ++ (take 3 $ formatTime defaultTimeLocale "%q" t)
            -- | Escapes attribute strings.
            escapeAttr = replace "\"" "&quot;"
                       . replace "<" "&lt;"
                       . replace "&" "&amp;"


-- Copied straight from 'System.Log.Handler.Simple'.
data GenericHandler a = GenericHandler {priority :: Priority,
                                        privData :: a,
                                        writeFunc :: a -> LogRecord -> String -> IO (),
                                        closeFunc :: a -> IO () }

instance LogHandler (GenericHandler a) where
    setLevel sh p = sh{priority = p}
    getLevel sh = priority sh
    emit sh lr loggername = (writeFunc sh) (privData sh) lr loggername
    close sh = (closeFunc sh) (privData sh)

-- Replaces instances of first list by second list in third list.
-- Definition blatantly stoled from jethr0's comment at
-- http://bluebones.net/2007/01/replace-in-haskell/. Can be swapped
-- with definition (or import) from MissingH.
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace _    _  [       ] = []
replace from to xs@(a:as) =
    if isPrefixOf from xs
	then to ++ drop (length from) xs else a : replace from to as

-- Shoehorns the hslogger priorities to the log4j basic priorities.
-- This ensures compatible levels without having to hack Chainsaw/log4j.
show' :: Priority -> String
show' NOTICE    = "INFO"
show' WARNING   = "WARN"
show' CRITICAL  = "ERROR"
show' ALERT     = "ERROR"
show' EMERGENCY = "FATAL"
show' l         = show l  -- Identical for DEBUG, INFO, ERROR.


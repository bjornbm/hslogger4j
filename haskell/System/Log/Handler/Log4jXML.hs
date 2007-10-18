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

{- The following is an example log message with log4j's XMLLayout:

<log4j:event logger="csa.Main" timestamp="1147868447298" level="DEBUG" thread="Timer-0">
<log4j:message><![CDATA[Doing Message Tasks : 0 tasks in queue.]]></log4j:message>
</log4j:event>

-}

module System.Log.Handler.Log4jXML 
    ( log4jStreamHandler , log4jFileHandler
    , log4jStreamHandler', log4jFileHandler'
    ) where

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


-- Copied straight from 'System.Log.Handler.Simple'.
data GenericHandler a = GenericHandler {priority :: Priority,
                                        privData :: a,
                                        writeFunc :: a -> LogRecord -> String -> IO (),
                                        closeFunc :: a -> IO () }

-- Copied straight from 'System.Log.Handler.Simple'.
instance LogHandler (GenericHandler a) where
    setLevel sh p = sh{priority = p}
    getLevel sh = priority sh
    emit sh lr loggername = (writeFunc sh) (privData sh) lr loggername
    close sh = (closeFunc sh) (privData sh)


-- Handler that logs to a handle rendering message priorities according
-- to the supplied function.
log4jHandler :: (Priority -> String) -> Handle -> Priority -> IO (GenericHandler Handle)
log4jHandler showPrio h pri = do
    lock <- newMVar ()
    let mywritefunc hdl (prio, msg) loggername = withMVar lock (\_ -> do 
        time   <- getCurrentTime
        thread <- myThreadId
        hPutStrLn hdl (show $ createMessage loggername time prio thread msg)
        hFlush hdl
        )
    return (GenericHandler { priority  = pri,
                             privData  = h,
                             writeFunc = mywritefunc,
                             closeFunc = \x -> return () })
    where
        -- Creates an XML element representing a log4j event/message.
        createMessage :: String -> UTCTime -> Priority -> ThreadId -> String -> XML
        createMessage logger time prio thread msg = Elem "log4j:event" 
            [ ("logger"   , logger       )
            , ("timestamp", millis time  )
            , ("level"    , showPrio prio)
            , ("thread"   , show thread  )
            ]
            (Just $ Elem "log4j:message" [] (Just $ CDATA msg))
            where
                -- This is an ugly hack to get a unix epoch with milliseconds.
                -- The use of "take 3" causes the milliseconds to always be 
                -- rounded downwards, which I suppose may be the expected
                -- behaviour for time.
                millis t = formatTime defaultTimeLocale "%s" t
                    ++ (take 3 $ formatTime defaultTimeLocale "%q" t)


-- | Logs to a handle using hslogger priorities.
log4jStreamHandler :: Handle -> Priority -> IO (GenericHandler Handle)
log4jStreamHandler = log4jHandler show

-- | Logs to a handle using log4j levels (priorities). The priorities of
-- messages are shoehorned into log4j levels as follows:
--      DEBUG -> DEBUG
--      INFO, NOTICE -> INFO
--      WARNING -> WARN
--      ERROR, CRITICAL, ALERT -> ERROR
--      EMERGENCY -> FATAL
-- This is useful when the log will only be consumed by log4j tools and
-- you don't want to go out of your way transforming the log or configuring
-- the tools.
log4jStreamHandler' :: Handle -> Priority -> IO (GenericHandler Handle)
log4jStreamHandler' = log4jHandler show' where
    show' :: Priority -> String
    show' NOTICE    = "INFO"
    show' WARNING   = "WARN"
    show' CRITICAL  = "ERROR"
    show' ALERT     = "ERROR"
    show' EMERGENCY = "FATAL"
    show' p         = show p  -- Identical for DEBUG, INFO, ERROR.

-- | Log to a file with hslogger priorities.
log4jFileHandler :: FilePath -> Priority -> IO (GenericHandler Handle)
log4jFileHandler fp pri = openFile fp AppendMode 
    >>= (flip log4jStreamHandler) pri
    >>= \lh -> return $ lh { closeFunc = hClose }

-- | Log to a file with log4j priorities.
log4jFileHandler' :: FilePath -> Priority -> IO (GenericHandler Handle)
log4jFileHandler' fp pri = openFile fp AppendMode 
    >>= (flip log4jStreamHandler') pri
    >>= \lh -> return $ lh { closeFunc = hClose }


-- A type for building and showing XML elements. Could use a fancy XML
-- library but am reluctant to introduce dependencies.
data XML = Elem  String [(String, String)] (Maybe XML)
         | CDATA String
      -- | Text  String

instance Show XML where
    show (CDATA s) = "<![CDATA[" ++ escapeCDATA s ++ "]]>" where
        escapeCDATA = replace "]]>" "]]&lt;"  -- The best we can do, I guess.
    show (Elem name attrs child) = "<" ++ name ++ showAttrs attrs ++ showChild child where
        showAttrs []         = ""
        showAttrs ((k,v):as) = " " ++ k ++ "=\"" ++ escapeAttr v ++ "\"" 
                             ++ showAttrs as
            where escapeAttr = replace "\"" "&quot;" 
                             . replace "<" "&lt;" 
                             . replace "&" "&amp;"
        showChild Nothing  = "/>"
        showChild (Just c) = ">" ++ show c ++ "</" ++ name ++ ">"


-- Replaces instances of first list by second list in third list.
-- Definition blatantly stoled from jethr0's comment at
-- http://bluebones.net/2007/01/replace-in-haskell/. Can be swapped
-- with definition (or import) from MissingH.
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace _    _  [       ] = []
replace from to xs@(a:as) = if isPrefixOf from xs
    then to ++ drop (length from) xs else a : replace from to as

        
test1 = Elem "log4j:event" [("time","1234"), ("level", "WARN")] Nothing
test2 = Elem "log4j:event" [("time","1234"), ("level", "WARN")] $ Just $ Elem "log4j:message" [] $ Just $ CDATA "My log message"


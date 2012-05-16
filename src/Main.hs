-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  : ponchik
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import System
import System (getArgs)
import System.Console.GetOpt
import RssReader (retrieveSubscriptionTitles)

data Options = Options{
    version :: IO (),
    optOutput :: IO ()
}

defaultOptions :: Options
defaultOptions = Options {
    version = return(),
    optOutput = return ()
}

options :: [OptDescr (Options -> IO Options)]
options = [
            Option ['V'] ["version"]    (NoArg (\opt -> return opt { version = showVersion })) "show version number",
            Option ['L'] ["list"]       (NoArg (\opt -> return opt { optOutput = showSubscriptions}))  "show list of subscriptions"
            ]

header = "Usage: main [Option...]"

showSubscriptions = do
    subscriptionTitles <- retrieveSubscriptionTitles "http://javablogs.com/ViewDaysBlogs.action?view=rss"
    putStrLn $ show subscriptionTitles
    exitWith ExitSuccess

showVersion = do
    putStrLn "hrss version 0.0.1"
    exitWith ExitSuccess

main = do
    args <- getArgs
    let ( actions, nonOpts, msgs ) = getOpt RequireOrder options args
    opts <- foldl (>>=) (return defaultOptions) actions
    let Options {optOutput = outputFun, version = versionFun} = opts
    outputFun >> versionFun

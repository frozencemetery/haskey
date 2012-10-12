module Args where

import System.Console.GetOpt
import Data.Maybe

data Action = Create | Lookup | Delete | List

data Options = Options { optShowVersion :: Bool
                       , optShowLicense :: Bool
                       , optService :: Maybe String
                       , optUser :: Maybe String
                       , optPassword :: Maybe String
                       , optGenPw :: Maybe Int
                       , optGenUser :: Maybe Int
                       , optAction :: Maybe Action
                       , optDBlocat :: FilePath
                       , optXOut :: Bool
                       }

defaultOptions :: FilePath -> Options
defaultOptions home = Options { optShowVersion = False
                              , optShowLicense = False
                              , optService = Nothing
                              , optUser = Nothing
                              , optPassword = Nothing
                              , optGenPw = Nothing
                              , optGenUser = Nothing
                              , optAction = Nothing
                              , optDBlocat = home ++ "/.pw.db"
                              , optXOut = False
                              }

options :: FilePath -> [OptDescr (Options -> Options)]
options home = [ Option ['v'] ["version"]
                     (NoArg (\opts -> opts { optShowVersion = True }))
                     "Display version information"
          , Option [] ["license"]
                     (NoArg (\opts -> opts { optShowLicense = True }))
                     "Print license (GPLv3) information"
          , Option ['l'] ["lookup"]
                     (NoArg (\opts -> opts { optAction = Just Lookup }))
                     "Perform a username/password lookup"
          , Option ['x'] ["xout"]
                     (NoArg (\opts -> opts { optXOut = True }))
                     "Outputs the password as if it were typed"
          , Option ['c'] ["create"]
                     (NoArg (\opts -> opts { optAction = Just Create }))
                     "Create a new entry (overwriting any duplicate)"
          , Option ['L'] ["list"]
                     (NoArg (\opts -> opts { optAction = Just List }))
                     "List the service names of all entries"
          , Option ['s'] ["service"]
                     (OptArg ((\f opts -> opts { optService = Just f })
                              . fromMaybe "debian") "SERVICENAME")
                     "name of service"
          , Option ['u'] ["username"]
                     (OptArg ((\f opts -> opts { optUser = Just f })
                              . fromMaybe "frozencemetery") "USERNAME")
                     "name of user"
          , Option ['p'] ["password"]
                     (OptArg ((\f opts -> opts { optPassword = Just f })
                              . fromMaybe "hunter2") "PASSWORD")
                     "password to use"
          , Option ['P'] ["genpw"]
                     (OptArg ((\f opts -> opts { optGenPw = Just $ read f })
                              . fromMaybe "128") "PASSLENGTH")
                     "desired length of password (defaults to 128)"
          , Option ['U'] ["genuser"]
                     (OptArg ((\f opts -> opts { optGenUser = Just $ read f })
                              . fromMaybe "128") "NAMELENGTH")
                     "desired length of username (defaults to 128)"
          , Option [] ["delete"] -- do not bind a shortarg to this command
                     (NoArg (\opts -> opts { optAction = Just Delete }))
                     "delete an entry"
          , Option ['d'] ["dblocat"]
                     (OptArg ((\f opts -> opts { optDBlocat = f})
                              . fromMaybe (home ++ "/.pw.db")) "FILE")
                     "location of database (defaults to ~/.pw.db)"          
          ]

compilerOpts :: [String] -> FilePath -> IO (Options, [String])
compilerOpts argv home =
   let header = "Usage: pwman [Option...] files..."
   in case getOpt Permute (options home) argv of
        (o, n, []) ->
          return (foldl (flip id) (defaultOptions home) o, n)
        (_,_,errs) ->
          ioError (userError (concat errs ++ usageInfo header (options home)))

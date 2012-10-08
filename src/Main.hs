import Pwgen
import Storage
import System.Console.GetOpt
import System.Environment
import Data.Maybe
import Prelude
import System.IO
import Control.Monad

version = "0.5hg"

data Action = Create | Lookup | Delete | List

data Options = Options { optShowVersion :: Bool
                       , optShowLicense :: Bool
                       , optServicename :: Maybe String
                       , optUsername :: Maybe String
                       , optPassword :: Maybe String
                       , optGenPw :: Maybe Int
                       , optGenUser :: Maybe Int
                       , optAction :: Maybe Action
                       -- , optDBlocat :: FilePath
                       }

defaultOptions :: Options
defaultOptions = Options { optShowVersion = False
                         , optShowLicense = False
                         , optServicename = Nothing
                         , optUsername = Nothing
                         , optPassword = Nothing
                         , optGenPw = Nothing
                         , optGenUser = Nothing
                         , optAction = Nothing
                         -- , optDBlocat = "/home/frozencemetery/.pw.db"
                         }

options :: [OptDescr (Options -> Options)]
options = [ Option ['v'] ["version"] 
                     (NoArg (\opts -> opts { optShowVersion = True }))
                     "Display version information"
          , Option ['l'] ["license"]
                     (NoArg (\opts -> opts { optShowLicense = True }))
                     "Print license (GPLv3) information"
          , Option ['L'] ["lookup"]
                     (NoArg (\opts -> opts { optAction = Just Lookup }))
                     "Perform a username/password lookup"
          , Option ['c'] ["create"]
                     (NoArg (\opts -> opts { optAction = Just Create }))
                     "Create a new entry (overwriting any duplicate)"
          , Option [] ["list"]
                     (NoArg (\opts -> opts { optAction = Just List }))
                     "List the service names of all entries"
          , Option ['s'] ["service"]
                     (OptArg ((\f opts -> opts { optServicename = Just f }) . fromMaybe "debian") "SERVICENAME")
                     "name of service"
          , Option ['u'] ["username"]
                     (OptArg ((\f opts -> opts { optUsername = Just f }) . fromMaybe "frozencemetery") "USERNAME")
                     "name of user"
          , Option ['p'] ["password"]
                     (OptArg ((\f opts -> opts { optPassword = Just f }) . fromMaybe "hunter2") "PASSWORD")
                     "password to use"
          , Option ['P'] ["genpw"]
                     (OptArg ((\f opts -> opts { optGenPw = Just $ read f }) . fromMaybe "128") "PASSLENGTH")
                     "desired length of password (defaults to 128)"
          , Option ['U'] ["genuser"]
                     (OptArg ((\f opts -> opts { optGenUser = Just $ read f }) . fromMaybe "128") "NAMELENGTH")
                     "desired length of username (defaults to 128)"
          , Option [] ["delete"] -- do not bind a shortarg to this command
                     (NoArg (\opts -> opts { optAction = Just Delete }))
                     "delete an entry"
          -- , Option ['d'] ["dblocat"]
          --            (OptArg ((\f opts -> opts { optDBlocat = f}) . fromMaybe "/home/frozencemetery/.pw.db") "FILE")
          --            "location of database (defaults to ~/.pw.db)"
          ]

compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv = 
  let header = "Usage: pwstore [Option...] files..."
  in case getOpt Permute options argv of
       (o, n, []) -> return (foldl (flip id) defaultOptions o, n)
       (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))

main :: IO ()
main = do
  args <- getArgs
  (opts, trash) <- compilerOpts args
  when (optShowVersion opts) $ print version
  when (optShowLicense opts) $ print "GPLv3 motherfuckers!"
  case optAction opts of
    Nothing -> return ()
    Just List -> 
      do entries <- listEntries 
         putStrLn entries
         return ()
    Just Lookup -> 
      do entry <- get (optServicename opts) (optUsername opts) (optPassword opts)
         let entry' = maybe "no entry found" showdbent entry
         putStrLn entry'
         return ()
    Just Create -> return ()
    Just Delete -> 
      do killp <- del (optServicename opts) (optUsername opts) (optPassword opts)
         case killp of True -> putStrLn "Deleted."
                       False -> putStrLn "No entries matched to delete."
         return ()
  return ()

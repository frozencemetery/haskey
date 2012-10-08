import Pwgen
import Storage
import System.Console.GetOpt
import System.Environment
import Data.Maybe
import Prelude
import System.IO

version = "0.5hg"

data Options = Options { optShowVersion :: Bool
                       , optShowLicense :: Bool
                       , optLookup :: Bool
                       , optCreate :: Bool
                       , optList :: Bool
                       , optServicename :: Maybe String
                       , optUsername :: Maybe String
                       , optPassword :: Maybe String
                       , optGenPw :: Maybe Int
                       , optGenUser :: Maybe Int
                       , optDelete :: Bool
                       -- , optDBlocat :: FilePath
                       }

defaultOptions :: Options
defaultOptions = Options { optShowVersion = False
                         , optShowLicense = False
                         , optLookup = False
                         , optCreate = False
                         , optList = False
                         , optServicename = Nothing
                         , optUsername = Nothing
                         , optPassword = Nothing
                         , optGenPw = Nothing
                         , optGenUser = Nothing
                         , optDelete = False
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
                     (NoArg (\opts -> opts { optLookup = True }))
                     "Perform a username/password lookup"
          , Option ['c'] ["create"]
                     (NoArg (\opts -> opts { optCreate = True }))
                     "Create a new entry (overwriting any duplicate)"
          , Option [] ["list"]
                     (NoArg (\opts -> opts { optList = True }))
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
                     (NoArg (\opts -> opts { optDelete = True }))
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
  if optShowVersion opts then print version else return ()
  if optShowLicense opts then print "GPLv3 motherfuckers!" else return ()
  if optList opts then do entries <- listEntries; hPutStrLn stdout entries else return ()
  return ()

module Main where
import MOD_Dump.Module
import MOD_Dump.STC
import MOD_Dump.ASC
import MOD_Dump.PT2
import MOD_Dump.PT3
import MOD_Dump.Utils
import qualified Data.ByteString.Lazy as B
import System.Console.GetOpt
import System.Environment
import Control.Monad.State
import System.FilePath (takeExtension)

data Options = Options { optHelp :: Bool
                       , optAll  :: Bool
                       , optPartial :: Bool
                       , optSamples :: [(Int,Int)]
                       , optOrnament :: [(Int,Int)]
                       , optPatterns :: [(Int,Int)]
                       , optWidth :: Int
                       } deriving (Eq, Show)

defaultOptions = Options False False False [] [] [] 128

optDefs :: [ OptDescr (State Options () ) ]
optDefs = [ Option "h?" ["help","info"] (NoArg (modify $ \x -> x{optHelp = True})) "Print this help message"
          , Option "a"  ["all"]         (NoArg (modify $ \x -> x{optAll = True}))  "Print all sections of module"
          , Option "b"  ["brief"]       (NoArg (modify $ \x -> x{optPartial = True})) "Print only header and some parts specified explicitly."
          , Option "s"  ["samples"]     (OptArg readSampleRange "FROM-TO") "Print samples in range FROM-TO"
          , Option "o"  ["ornaments"]   (OptArg readOrnamentRange "FROM-TO") "Print ornamets in range FROM-TO"
          , Option "p"  ["patterns"]    (OptArg readPatternRange "FROM-TO") "Print patterns in range FROM-TO"
          , Option "w"  ["width"]       (ReqArg readWidth "WIDTH") "Set columns count for patterns print"
          ]
    where
        readSampleRange :: Maybe String -> State Options ()
        readSampleRange ms   = modify $ \x -> x { optSamples = optSamples x ++ readRange ms, optPartial = True}

        readOrnamentRange :: Maybe String -> State Options ()
        readOrnamentRange ms = modify $ \x -> x { optOrnament = optOrnament x ++ readRange ms, optPartial = True}

        readPatternRange :: Maybe String -> State Options ()
        readPatternRange ms  = modify $ \x -> x { optPatterns = optPatterns x ++ readRange ms, optPartial = True}

        readWidth :: String -> State Options ()
        readWidth s          = modify $ \x -> x { optWidth = readValue s}

moduleReaders :: [String -> B.ByteString -> Maybe ShowModule]
moduleReaders = [readSTCModule, readASCModule, readPT2Module, readPT3Module]

main :: IO ()
main = do
    args <- getArgs
    let (o, p, e) = getOpt RequireOrder optDefs args
    let opts = execState (sequence_ o) defaultOptions
    let needHelp = optHelp opts || (not.null) e || null p
    if needHelp then putStrLn $ usageInfo "Program help" optDefs
        else forM_ p $ \i -> do
            bs <- B.readFile i
            let m = msum [ f (takeExtension i) bs | f <- moduleReaders ]
            doPrint opts m

doPrint :: Options -> Maybe ShowModule -> IO ()
doPrint _ Nothing = do
    putStrLn "Module cannot be printed."
    putStrLn $ usageInfo "Program help" optDefs
doPrint opts (Just m) = do
    printInfo m
    let a = optAll opts || (not (optPartial opts) && null (optSamples opts)  && null (optOrnament opts) && null (optPatterns opts) )
    let w = optWidth opts
    when (optAll opts || (not.null) (optSamples opts)) $
        printSamples m w (optSamples opts)
    when (optAll opts || (not.null) (optOrnament opts)) $
        printOrnaments m w (optOrnament opts)
    when (not $ optPartial opts && null (optPatterns opts) ) $
        printPatterns m w (optPatterns opts)

select a [] _ xs = if a then xs else []
select _ nums f xs = [ x | x <- xs, let i = f x, any (\(f,t) -> i>=f && i<=t) nums]


readRange :: Maybe String -> [(Int, Int)]
readRange Nothing = [(0,maxBound)]
readRange (Just s) = map readRange' $ breakBy ',' s
    where
        breakBy _ "" = []
        breakBy c (',':s) = breakBy c s
        breakBy c s = let (h, t) = break (==c) s in h : breakBy c t
        readRange' s = let  (f,t) = break (=='-') s
                            ff = if f == "" then 0 else readValueDef 0 f
                            tt | t == ""  = ff 
                               | t == "-" = maxBound 
                               | otherwise = readValueDef ff $ tail t
                        in (ff, tt)
                            

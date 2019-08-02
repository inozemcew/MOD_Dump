module Main where
import MOD_Dump.Module
import MOD_Dump.STC
import MOD_Dump.ASC
import qualified Data.ByteString.Lazy as B
import System.Console.GetOpt
import System.Environment
import Control.Monad.State
import System.FilePath (takeExtension)

--data Options = OptHelp | OptAll | OptSamples (Int, Int) | OptOrnament (Int, Int) | OptPatterns (Int, Int) | OptWidth Int deriving (Eq,Show)
data Options = Options { optHelp :: Bool
                        ,optAll  :: Bool
                        ,optPartial :: Bool
                        ,optSamples :: [(Int,Int)]
                        ,optOrnament :: [(Int,Int)]
                        ,optPatterns :: [(Int,Int)]
                        ,optWidth :: Int
                    }  deriving (Eq, Show)

defaultOptions = Options False False False [] [] [] 4

optDefs :: [ OptDescr (State Options () ) ]
optDefs = [ Option "h?" ["help","info"] (NoArg (modify $ \x -> x{optHelp = True})) "Print this help message"
          , Option "a"  ["all"]         (NoArg (modify $ \x -> x{optAll = True}))  "Print all sections of module"
          , Option "b"  ["brief"]       (NoArg (modify $ \x -> x{optPartial = True})) "Print only header and some parts specified explicitly."
          , Option "s"  ["samples"]     (OptArg getSampleRange "FROM-TO") "Print samples in range FROM-TO" 
          , Option "o"  ["ornaments"]   (OptArg getOrnamentRange "FROM-TO") "Print ornamets in range FROM-TO"
          , Option "p"  ["patterns"]    (OptArg getPatternRange "FROM-TO") "Print patterns in range FROM-TO"
          , Option "w"  ["width"]       (ReqArg getWidth "WIDTH") "Set columns count for patterns print"]
    where
        getSampleRange :: Maybe String -> State Options ()
        getSampleRange ms   = modify $ \x -> x { optSamples = optSamples x ++ getRange ms, optPartial = True}

        getOrnamentRange :: Maybe String -> State Options ()
        getOrnamentRange ms = modify $ \x -> x { optOrnament = optOrnament x ++ getRange ms, optPartial = True}

        getPatternRange :: Maybe String -> State Options ()
        getPatternRange ms  = modify $ \x -> x { optPatterns = optPatterns x ++ getRange ms, optPartial = True}

        getWidth :: String -> State Options ()
        getWidth s          = modify $ \x -> x { optWidth = getValue s}

moduleReaders :: [String -> B.ByteString -> Maybe Module]
moduleReaders = [readSTCModule, readASCModule]

main :: IO ()
main = do
    args <- getArgs
    let (o, p, e) = getOpt RequireOrder optDefs args
    let opts = execState (sequence_ o) defaultOptions
    let needHelp = optHelp opts || (not.null) e || null p
    if needHelp then putStrLn $ usageInfo "Program help" optDefs
        else forM_ p $ \i -> do
            bs <- B.readFile i
            let m = foldr1 mplus [ f (takeExtension i) bs | f <- moduleReaders ]
            doPrint opts m

doPrint :: Options -> Maybe Module -> IO ()
doPrint _ Nothing = do
    putStrLn "No module can be printed."
    putStrLn $ usageInfo "Program help" optDefs
doPrint opts (Just m) = do
    printInfo m
    let a = optAll opts || (not (optPartial opts) && null (optSamples opts)  && null (optOrnament opts) && null (optPatterns opts) )
    when (optAll opts || (not.null) (optSamples opts)) $
        printSamples m (optSamples opts)
    when (optAll opts || (not.null) (optOrnament opts)) $
        printOrnaments m (optOrnament opts)
    when (not $ optPartial opts && null (optPatterns opts) ) $
        printPatterns m (optWidth opts) (optPatterns opts)

select a [] _ xs = if a then xs else []
select _ nums f xs = [ x | x <- xs, let i = f x, any (\(f,t) -> i>=f && i<=t) nums]


getRange :: Maybe String -> [(Int, Int)]
getRange Nothing = [(0,maxBound)]
getRange (Just s) = map getRange' $ breakBy ',' s
    where
        breakBy _ "" = []
        breakBy c (',':s) = breakBy c s
        breakBy c s = let (h, t) = break (==c) s in h : breakBy c t
        getRange' s = let   (f,t) = break (=='-') s
                            ff = if f == "" then 0 else getValueDef 0 f
                            tt | t == ""  = ff 
                               | t == "-" = maxBound 
                               | otherwise = getValueDef ff $ tail t
                        in (ff, tt)
                            
getValue :: String -> Int                           
getValue = getValueDef (-1) 

getValueDef :: Int -> String -> Int
getValueDef d s = let v = reads s in if null v then d else fst $ head v                     

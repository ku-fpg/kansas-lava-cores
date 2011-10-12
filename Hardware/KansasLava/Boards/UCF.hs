module Hardware.KansasLava.Boards.UCF (copyUCF) where
                
import Language.KansasLava as KL
import System.IO
import Data.Char

import Paths_kansas_lava_cores

copyUCF :: FilePath -> FilePath -> KLEG -> IO ()
copyUCF src dest kleg = do
        let inputs = theSrcs kleg
        let findMe = concat
                     [ case toStdLogicType ty of
                         SL -> [ nm ]
                         SLV n -> [ nm ++ "<" ++ show i ++ ">" 
                                  | i <- [0..(n-1)]
                                  ]
                     | (nm,ty) <- (theSrcs kleg) ++ map (\ (a,b,c) -> (a,b)) (theSinks kleg)
                     ]

        let isComment ('#':_) = True
            isComment xs             | all isSpace xs = True
            isComment _       = False

        let getName xs | take 5 xs == "NET \""
                       = Just (takeWhile (/= '"') (drop 5 xs))
            getName _ = Nothing

        let hdr = unlines 
                [ "# Generated automatically by kansas-lava-cores"
                , "#" ++ show findMe
                ]

        filename <- getDataFileName ("UCF/" ++ src)
        big_ucf <- readFile filename
        let lns = unlines
                  [ let allow = case getName ln of
                          Nothing -> True
                          Just nm -> nm `elem` findMe
                    in (if allow then ""  else "# -- ") ++ ln
                  | ln <- lines big_ucf
                  ]

        writeFile dest (hdr ++ lns)

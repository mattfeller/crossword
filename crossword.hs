import System.IO (isEOF)

data Dir = Hor | Ver

haddword :: Int -> String -> String -> String
haddword i piece ""   = piece
haddword 0 piece word = ((head word) : (haddword 0 (tail piece) (tail word)))
haddword i piece word = ((head piece) : (haddword (i-1) (tail piece) word))

addword :: Dir -> Int -> Int -> [String] -> String -> [String]
addword d i j grid ""       = grid
addword Hor i 0 grid word   = ((haddword i (head grid) word) : (tail grid))
addword Ver i 0 grid word   = ((haddword i (head grid) (take 1 word)) : (addword Ver i 0 (tail grid) (tail word)))
addword d i j grid word     = (take j grid) ++ (addword d i 0 (drop j grid) word)

hfits :: String -> String -> Bool
hfits piece ""    = True
hfits "" word     = False
hfits piece word  = if (head piece) == '-'
                        then hfits (tail piece) (tail word)
                    else if (head piece) == (head word)
                        then hfits (tail piece) (tail word)
                    else False

emptyspot :: String -> String -> Bool
emptyspot "" word     = False
emptyspot piece ""    = False
emptyspot piece word  = if (head piece) == '-'
                            then True
                        else emptyspot (tail piece) (tail word)
                    
extractvert :: Int -> [String] -> String
extractvert i [str] = (drop i (take (i+1) str))
extractvert i grid  = ((head grid)!!i : (extractvert i (tail grid)))

fits :: Dir -> Int -> Int -> [String] -> String -> Bool
fits d i j grid "" = True
fits Hor i 0 grid word = (hfits (drop i (head grid)) word && emptyspot (drop i (head grid)) word)
fits Ver i 0 grid word = (hfits (extractvert i grid) word && emptyspot (extractvert i grid) word)
fits d i j grid word = fits d i 0 (drop j grid) word

fillin :: Int -> Int -> [String] -> [String] -> [String]
fillin i j grid [] = grid
fillin 9 9 grid words = []
fillin 9 j grid words = if fits Ver 9 j grid (head words)
                            then take 10 ((fillin 0 0 (addword Ver 9 j grid (head words)) (tail words)) ++ (fillin 0 0 grid ((tail words) ++ (take 1 words))))
                            else fillin 0 (j+1) grid words
fillin i j grid words = if fits Ver i j grid (head words)
                            then take 10 ((fillin 0 0 (addword Ver i j grid (head words)) (tail words)) ++ (fillin 0 0 grid ((tail words) ++ (take 1 words))))
                        else if fits Hor i j grid (head words)
                            then take 10 ((fillin 0 0 (addword Hor i j grid (head words)) (tail words)) ++ (fillin 0 0 grid ((tail words) ++ (take 1 words))))
                            else fillin (i+1) j grid words
    
getword :: IO String  
getword = do
            done <- isEOF
            if done
                then return ""
                else do
                    c <- getChar  
                    if c == ';' then return ""  
                        else do
                            s <- getword 
                            return (c:s)
                                  
ntimes :: Int -> IO String -> IO [String]
ntimes 0 foo = do
    return []
ntimes n foo = do
    this <- foo
    if this == [] then return []
        else do
            rest <- ntimes (n - 1) foo
            return (this : rest)
            
showme :: [String] -> IO ()
showme [] = return ()
showme [str] = putStrLn str
showme grid = do
    putStrLn (head grid)
    showme (tail grid)

main = do
    grid <- ntimes 10 getLine
    words <- ntimes 10 getword
    showme (fillin 0 0 grid words)

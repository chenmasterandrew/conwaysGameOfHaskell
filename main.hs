{-
Ahoy!

This is Andrew Chen's text-based Conway's Game of Life Game programmed in Haskell!

-}
cellGenerator 0 = []
cellGenerator n = [0] ++ cellGenerator (n-1)

printLine [] = putStrLn ""
printLine l = do
    putStr ((head l) ++ "  ")
    printLine (tail l)

printer w 0 l = putStrLn ""
printer w h l = do
    printLine (take w (drop 2 l))
    printer w (h-1) (drop (w+2) l)

neighbors w h c = [if (elem x [(w + 2)..(length c - w - 4)] && not (elem x [0,(w + 2)..(length c)]) && not (elem x [(w+1),(2*w+3)..(length c)])) then ((c !! (x - 1)) + (c !! (x + 1)) + (c !! (x - w - 1)) + (c !! (x - w - 2)) + (c !! (x - w - 3)) + (c !! (x + w + 1)) + (c !! (x + w + 2)) + (c !! (x + w + 3))) else 0| x <- [0..(length c - 1)]]

judger c l = [if l !! x == 3 || (l !! x == 2 && c !! x == 1) then 1 else 0| x <- [0..(length c - 1)]]

texter l = [if x == 0 then "." else "x"| x <- l]

replace n newVal (x:xs)
   | n == 0 = newVal:xs
   | otherwise = x:replace (n-1) newVal xs

ticker w h c = do
    putStrLn ""
    printer w h (texter (drop (w + 1) c))
    putStrLn "Name the cell row then column you wish to switch e.g. \"1\" then \"2\" or type \"0\" to go to the next tick or \"-1\" to exit."
    s <- readLn
    if s == 0
        then
            ticker w h (judger c (neighbors w h c))
        else do
            f <- readLn
            if f == 0
            then
                ticker w h (judger c (neighbors w h c))
            else
                if f > w || s > h
                    then
                        ticker w h c
                    else
                        if f < 1 || s < 1
                        then
                            return()
                        else do
                            let place = ((s) * (w + 2) + f)
                            let cell = c !! place
                            if cell == 2 || cell == 1
                                then
                                    ticker w h (replace place 0 c)
                                else
                                    ticker w h (replace place 1 c)
    
    
main = do
    putStrLn "How wide should the grid be? (0<w<51)"
    width <- readLn
    putStrLn "How high should the grid be? (0<w<51)"
    height <- readLn
    if width < 1 || width > 50 || height < 1 || height > 50
        then main
        else do
            let cells = cellGenerator ((width + 2) * (height + 2))
            ticker width height cells
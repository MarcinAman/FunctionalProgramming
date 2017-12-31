getLine' :: IO String
getLine' = do
        x <- getChar
        if x == '\n'
        then return []
        else do
            xs <- getLine'
            return (x:xs)


main = do
    line <- getLine'
    putStrLn line

actSeq = putChar 'A' >> putChar 'G' >> putChar 'H' >>  putChar '\n'

doActSeq = do
    putChar 'A'
    putChar 'G'
    putChar 'H'
    putChar '\n'

echo1 = getLine >>= putStrLn

doEcho1 = do
  variable <- getLine
  putStrLn (reverse variable)

echo2 = getLine >>= \line -> putStrLn $ line ++ "!"

doEcho2 = do
  line <- getLine
  putStrLn $ line ++ "!"

echo3 :: IO ()
echo3 =  getLine >>= \l1 -> getLine >>= \l2 -> putStrLn $ l1 ++ l2

dialog :: IO ()
dialog = putStr "What is your happy number? "
         >> getLine
         >>= \n -> let num = read n :: Int in
                   if num == 7
                   then putStrLn "Ah, lucky 7!"
                   else if odd num
                        then putStrLn "Odd number! That's most people's choice..."
                        else putStrLn "Hm, even number? Unusual!"

echo3' :: IO ()
echo3' = do
    firstPart <- getLine
    secondPart <- getLine
    putStrLn $firstPart ++ " " ++ secondPart

dialog' :: IO ()
dialog' = do
    putStrLn "What is your happy number? "
    line <- getLine
    let num = read line :: Int in
        if num == 7
        then putStrLn "Ah, lucky 7!"
        else if odd num
             then putStrLn "Odd number! That's most people's choice..."
             else putStrLn "Hm, even number? Unusual!"


twoQuestions :: IO ()
twoQuestions = do
  putStr "What is your name? "
  name <- getLine
  putStr "How old are you? "
  age <- getLine
  print (name,age)

twoQuestions' :: IO ()
twoQuestions' = putStrLn "What is your name? " 
                >> getLine 
                >>= \l1 -> putStrLn "How old are you? " 
                >> getLine 
                >>= \l2 -> print (l1,l2)


nTimes :: Int -> IO () -> IO ()
nTimes 0 action = return ()
nTimes n action = do
  action
  nTimes (n-1) action

ioActionFactory :: Int -> String -> IO ()
ioActionFactory n = case n of
  1 -> \name -> putStrLn ("Good morning, " ++ name)
  2 -> \name -> putStrLn ("Good afternoon, " ++ name)
  3 -> \name -> putStrLn ("Good night, " ++ name)
  _ -> \name -> putStrLn ("Hello, " ++ name)

actionList :: [IO ()]
actionList = [ioActionFactory 1 "Ben",
              ioActionFactory 2 "Joe",
              ioActionFactory 3 "Ally"]

sequence'        :: [IO ()] -> IO ()
sequence' []     =  return ()
sequence' (a:as) =  do a
                       sequence' as
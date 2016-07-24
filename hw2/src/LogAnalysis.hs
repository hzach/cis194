{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where
  import Data.List.Split
  import Data.Char
  import Log

  isInt :: String -> Bool
  isInt s =
    case dropWhile isDigit s of
      "" -> True
      _  -> False

  parseMessage :: String -> LogMessage
  parseMessage msg =
    case words msg of
      ("E":n:t:xs) 
        | isInt n && isInt t -> LogMessage (Error $ read n) (read t) (unwords xs)
      ("W":t:xs)
        | isInt t            -> LogMessage Warning (read t) (unwords xs)
      ("I":t:xs)
        | isInt t            -> LogMessage Info (read t) (unwords xs)
      _                      -> Unknown ("Format error: " ++ msg)

  parse :: String -> [LogMessage]
  parse s = map parseMessage (splitOn "\n" s)

  insert :: LogMessage -> MessageTree -> MessageTree
  insert (Unknown _) tree = tree
  insert msg Leaf = Node Leaf msg Leaf
  insert msg (Node left value right) = 
    case msg `compare` value of
      LT -> Node (insert msg left) value right
      _  -> Node left value (insert msg right)
  
  inOrder :: MessageTree -> [LogMessage]
  inOrder Leaf = []
  inOrder (Node left msg right) = (inOrder left) ++ [msg] ++ (inOrder right)

  build :: [LogMessage] -> MessageTree
  build [] = Leaf
  build (x:xs) = insert x $ build xs

  whatWentWrong :: [LogMessage] -> [String]
  whatWentWrong x = map text $ filter isError . inOrder $ build x
      where isError (LogMessage (Error _) _ _) = True
            isError _ = False
            text = \(LogMessage _ _ c) -> c
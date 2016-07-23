{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where
  import Log
  import Data.Char

  -- Utils ----
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
{-# LANGUAGE TemplateHaskell #-}

module Days.Day4(run) where

import Control.Lens
import Data.Function
import Data.List
import GHC.Exts(groupWith)
import Text.Parsec

data Timestamp = Timestamp {
    _year :: Int,
    _month :: Int,
    _day :: Int,
    _hour :: Int,
    _minute :: Int
} deriving (Eq, Ord, Show)
makeLenses ''Timestamp

data Action = Start Int | FallAsleep | WakeUp
    deriving (Eq, Ord, Show)

data Event = Event {
    _timestamp :: Timestamp,
    _action :: Action
} deriving (Eq, Ord, Show)
makeLenses ''Event

data Schedule = Schedule {
    _guard :: Int,
    _asleep :: [Int]
} deriving (Show)
makeLenses ''Schedule

run :: String -> String
run s = 
    let events = parseEvents s 
        schedule = daySchedule (sort events)
          in show (strategy1 schedule) ++ ", " ++ show (strategy2 schedule)
   
-- parse input
parseEvents :: String -> [Event]
parseEvents s = case parse parser "" s of
    Left err -> error (show err)
    Right events -> events

parser :: Parsec String st [Event]
parser = do 
    events <- many parseLine
    eof
    pure events

parseLine :: Parsec String st Event
parseLine = do 
    ts <- parseTimestamp
    char ' '
    a <- parseAction
    endOfLine
    pure Event {_timestamp=ts, _action=a}

parseTimestamp :: Parsec String st Timestamp
parseTimestamp = do 
    char '['
    y <- read <$> count 4 digit
    char '-'
    m <- read <$> count 2 digit 
    char '-'
    d <- read <$> count 2 digit
    char ' '
    h <- read <$> count 2 digit 
    char ':'
    mi <- read <$> count 2 digit
    char ']'
    pure Timestamp {_year=y, _month=m, _day=d, _hour=h, _minute=mi}

parseAction :: Parsec String st Action
parseAction = parseStart <|> (string "falls asleep" >> pure FallAsleep) <|> (string "wakes up" >> pure WakeUp)
    where parseStart = do 
            string "Guard #"
            i <- read <$> many1 digit 
            string " begins shift"
            pure (Start i)

-- create schedule
daySchedule :: [Event] -> [Schedule]
daySchedule [] = []
daySchedule (e:es) = let (front, rest) = span (\ev -> (ev ^. action) `elem` [FallAsleep, WakeUp]) es in
    (makeSchedule (e:front)):(daySchedule rest)

makeSchedule :: [Event] -> Schedule
makeSchedule (e:es) = 
    let (Start guardId) = e ^. action
      in Schedule {_guard=guardId, _asleep=sleepTime es}

sleepTime :: [Event] -> [Int]
sleepTime [] = []
sleepTime (s:a:es) = [(min s)..(pred $ min a)] ++ sleepTime es
    where min e = e ^. timestamp . minute

-- first strategy
strategy1 :: [Schedule] -> Int
strategy1 schedule = (bestMinute (bestGuard schedule) schedule) * (bestGuard schedule)
    where bestGuard = snd . maximum . guardsAndCumulatedTimes
          guardsAndCumulatedTimes = map (\l -> (sum $ map snd l, fst $ head l)) . groupWith fst . guardsAndTimes
          guardsAndTimes = map (\s -> (s ^. guard, length $ s ^. asleep))
          bestMinute guardId schedule = snd $ maximum $ map (\i -> (length [s | s <- schedule, s ^. guard == guardId, i `elem` s ^. asleep], i)) [0..59]

-- second strategy
strategy2 :: [Schedule] -> Int
strategy2 schedule = snd $ maximum $
    [(length [s | s <- schedule, (s ^. guard) == g, m `elem` (s ^. asleep)], g * m) | 
     g <- map (view guard) schedule, m <- [0..59]]
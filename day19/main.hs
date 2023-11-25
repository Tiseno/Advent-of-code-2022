import qualified Data.Bifunctor as Bifunctor
import qualified Data.Char      as Char
import qualified Debug.Trace    as Debug

data BluePrint =
  BluePrint
    { bluePrintId      :: Int
    , oreRobotOreCost  :: Int
    , clayRobotOreCost :: Int
    , obsRobotOreCost  :: Int
    , obsRobotClayCost :: Int
    , geoRobotOreCost  :: Int
    , geoRobotObsCost  :: Int
    , oreRobotMax      :: Int
    , clayRobotMax     :: Int
    , obsRobotMax      :: Int
    }
  deriving (Show)

parseBluePrint :: String -> (Int, BluePrint)
parseBluePrint input =
  let [bluePrintId, oreRobotOreCost, clayRobotOreCost, obsRobotOreCost, obsRobotClayCost, geoRobotOreCost, geoRobotObsCost] =
        read . filter (/= ':') <$>
        filter (all (\c -> Char.isDigit c || c == ':')) (words input)
   in ( bluePrintId
      , BluePrint
          { bluePrintId = bluePrintId
          , oreRobotOreCost = oreRobotOreCost
          , clayRobotOreCost = clayRobotOreCost
          , obsRobotOreCost = obsRobotOreCost
          , obsRobotClayCost = obsRobotClayCost
          , geoRobotOreCost = geoRobotOreCost
          , geoRobotObsCost = geoRobotObsCost
          , oreRobotMax =
              maximum
                [ oreRobotOreCost
                , clayRobotOreCost
                , obsRobotOreCost
                , geoRobotOreCost
                ]
          , clayRobotMax = obsRobotClayCost
          , obsRobotMax = geoRobotObsCost
          })

data State =
  State
    { oreRobots   :: Int
    , currentOre  :: Int
    , clayRobots  :: Int
    , currentClay :: Int
    , obsRobots   :: Int
    , currentObs  :: Int
    , geoRobots   :: Int
    , currentGeo  :: Int
    }
  deriving (Show)

parseBluePrints :: String -> [(Int, BluePrint)]
parseBluePrints input = parseBluePrint <$> lines input

produce :: State -> State
produce s =
  s
    { currentOre = currentOre s + oreRobots s
    , currentClay = currentClay s + clayRobots s
    , currentObs = currentObs s + obsRobots s
    , currentGeo = currentGeo s + geoRobots s
    }

data RobotConstraints =
  RobotConstraints
    { oreForbidden  :: Bool
    , clayForbidden :: Bool
    , obsForbidden  :: Bool
    }

newConstraints :: RobotConstraints -> BluePrint -> State -> RobotConstraints
newConstraints rc bp s =
  RobotConstraints
    { oreForbidden = oreForbidden rc || oreRobotOreCost bp <= currentOre s
    , clayForbidden = clayForbidden rc || clayRobotOreCost bp <= currentOre s -- NOTE this one did not work for example input
    , obsForbidden =
        obsForbidden rc ||
        (obsRobotOreCost bp <= currentOre s &&
         obsRobotClayCost bp <= currentClay s)
    }

buildGeo :: BluePrint -> State -> State
buildGeo bp s =
  s
    { geoRobots = geoRobots s + 1
    , currentOre = currentOre s - geoRobotOreCost bp
    , currentObs = currentObs s - geoRobotObsCost bp
    }

buildObs :: BluePrint -> State -> State
buildObs bp s =
  s
    { obsRobots = obsRobots s + 1
    , currentOre = currentOre s - obsRobotOreCost bp
    , currentClay = currentClay s - obsRobotClayCost bp
    }

buildClay :: BluePrint -> State -> State
buildClay bp s =
  s
    { clayRobots = clayRobots s + 1
    , currentOre = currentOre s - clayRobotOreCost bp
    }

buildOre :: BluePrint -> State -> State
buildOre bp s =
  s
    { oreRobots = oreRobots s + 1
    , currentOre = currentOre s - oreRobotOreCost bp
    }

theoreticalMax :: State -> Int -> Int
theoreticalMax s step =
  currentGeo s + geoRobots s * (step - 1) + (step * (step - 1)) `div` 2

-- Heuristics
-- 1. Only allow building a number of robots corresponding to the biggest cost for a robot of that resource, when we have the max of all robots we can build any robot we want every step
-- 2. If we decide to not build any robots at a step, but we have enough resources to build some type of robot, dissallow building any more of that robot type in the following steps, as building it at a later time is just a worse decision
-- 3. DFS instead of BFS, passing the max forward to all branches to cut it short whenever it is not able to meet the already found maximum
findMaxGeode :: Int -> State -> BluePrint -> Int
findMaxGeode step s bp =
  findMaxGeodeR (RobotConstraints False False False) s step 0
  where
    findMaxGeodeR :: RobotConstraints -> State -> Int -> Int -> Int
    findMaxGeodeR _ s 0 _ = currentGeo s
    findMaxGeodeR rc s0 step0 maxFound =
      if theoreticalMax s0 step0 < maxFound
        then maxFound
        else let step = step0 - 1
                 s = s0
                 s' = produce s
                 chooseGeoResult =
                   if geoRobotOreCost bp <= currentOre s &&
                      geoRobotObsCost bp <= currentObs s
                     then findMaxGeodeR rc (buildGeo bp s') step 0
                     else 0
                 chooseObsResult =
                   if obsRobotOreCost bp <= currentOre s &&
                      obsRobotClayCost bp <= currentClay s &&
                      obsRobots s < obsRobotMax bp && not (obsForbidden rc)
                     then findMaxGeodeR rc (buildObs bp s') step chooseGeoResult
                     else 0
                 chooseClayResult =
                   if clayRobotOreCost bp <= currentOre s &&
                      clayRobots s < clayRobotMax bp && not (clayForbidden rc)
                     then findMaxGeodeR
                            rc
                            (buildClay bp s')
                            step
                            chooseObsResult
                     else 0
                 chooseOreResult =
                   if oreRobotOreCost bp <= currentOre s &&
                      oreRobots s < oreRobotMax bp && not (oreForbidden rc)
                     then findMaxGeodeR
                            rc
                            (buildOre bp s')
                            step
                            chooseClayResult
                     else 0
                 waitResult =
                   findMaxGeodeR
                     (newConstraints rc bp s)
                     s'
                     step
                     chooseOreResult
              in maximum
                   [ chooseGeoResult
                   , chooseObsResult
                   , chooseClayResult
                   , chooseOreResult
                   , waitResult
                   ]

initialState =
  State
    { oreRobots = 1
    , currentOre = 0
    , clayRobots = 0
    , currentClay = 0
    , obsRobots = 0
    , currentObs = 0
    , geoRobots = 0
    , currentGeo = 0
    }

part1 input = do
  let bluePrints = parseBluePrints input
  let results =
        fmap (Bifunctor.second (findMaxGeode 24 initialState)) bluePrints
  sum $ fmap (uncurry (*)) results

part2 input = do
  let bluePrints = parseBluePrints input
  let results = fmap (findMaxGeode 32 initialState . snd) (take 3 bluePrints)
  product results

main :: IO ()
main = do
  exampleInput <- readFile "input.txt"
  -- exampleInput <- readFile "input.example.txt"
  print $ part1 exampleInput
  print $ part2 exampleInput

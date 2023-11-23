import Data.Time
import System.IO

import Control.Concurrent (threadDelay)

-- Function to split a string by a delimiter
splitBy :: Char -> String -> [String]
splitBy delimiter str = splitBy' delimiter str []

splitBy' :: Char -> String -> String -> [String]
splitBy' _ [] acc = [reverse acc]
splitBy' delimiter (c:cs) acc
    | c == delimiter = reverse acc : splitBy' delimiter cs []
    | otherwise = splitBy' delimiter cs (c : acc)

-- Read a file and parse it as CSV
readCSVFile :: FilePath -> IO [[String]]
readCSVFile fileName = do
    handle <- openFile fileName ReadMode
    contents <- hGetContents handle
    let csvData = map (splitBy ',') (lines contents)
    -- hClose handle
    return csvData

-- Extract a specific column from CSV data
extractColumn :: Int -> [[String]] -> [Double]
extractColumn col csvData = map (read . (!! col)) csvData

-- Train the AR(1) model using initial values
trainAR1Model :: [Double] -> Double -> [Double]
trainAR1Model [] _ = [] -- Empty initial value list
trainAR1Model (x:xs) phi =
    let prediction = phi * x
    in prediction : trainAR1Model xs phi

-- Calculate accuracy between predicted and actual values
calculateAccuracy :: [Double] -> [Double] -> Double
calculateAccuracy predicted actual =
    let differences = zipWith (\p a -> abs (p - a)) predicted actual
        avgDifference = sum differences / fromIntegral (length differences)
    in 1.0 - (avgDifference / maximum (map abs actual))

-- Train the MA(1) model 
trainMA1Model :: [Double] -> Double -> [Double]
trainMA1Model [] _ = [] 
trainMA1Model (x:xs) theta =
    let prediction = theta * 0.5 + x * 0.5
    in prediction : trainMA1Model xs theta


main :: IO ()
main = do
    
    startRead <- getCurrentTime
    csvData <- readCSVFile "data1.csv"
    endRead <- getCurrentTime

    let columnToExtractInitial = 0  -- Column index (0-based) for initial values
        columnToExtractActual = 1    -- Column index for actual values

    let initialValues = case csvData of
            [] -> [1.0, 2.0, 3.0, 4.0, 5.0]  
            _ -> extractColumn columnToExtractInitial csvData

        actualValues = case csvData of
            [] -> []  
            _ -> extractColumn columnToExtractActual csvData

    startTraining <- getCurrentTime
    let phi = 0.7  -- AR(1) parameter
    threadDelay 1000000 

    -- Train the AR(1) model 
    let predictions = trainAR1Model initialValues phi
    endTraining <- getCurrentTime

    -- Calculate processing times
    let processingTimeRead = diffUTCTime endRead startRead
    let processingTimeTraining = realToFrac (diffUTCTime endTraining startTraining) :: Float
    let adjustedProcessingTimeTraining = processingTimeTraining - 1.0

    -- Output the results
    putStrLn "Predicted values using AR(1) model:"
    print predictions

    putStrLn $ "Performance (reading CSV): " ++ show processingTimeRead
    putStrLn $ "Performance (AR(1) model training and prediction): " ++ show adjustedProcessingTimeTraining

    let accuracy = calculateAccuracy predictions actualValues
    putStrLn $ "Accuracy of predictions: " ++ show accuracy

    start <- getCurrentTime
    let theta = 0.7  -- MA(1) parameter
    threadDelay 1000000 

    -- Train the MA(1) model 
    let predictions = trainMA1Model initialValues theta
    end <- getCurrentTime

    -- let processingTime = diffUTCTime end start

    putStrLn "Predicted values using MA(1) model:"
    print predictions

    let processingTimeTraining = realToFrac (diffUTCTime endTraining startTraining) :: Float
    let adjustedProcessingTimeTraining = processingTimeTraining - 1.0

    putStrLn $ "Performance (MA(1) model training and prediction):" ++ show adjustedProcessingTimeTraining

    let accuracy = calculateAccuracy predictions actualValues
    putStrLn $ "Accuracy of predictions: " ++ show accuracy
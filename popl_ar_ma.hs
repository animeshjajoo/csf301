import Data.Time
import System.IO

import Control.Concurrent (threadDelay)

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

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

-- Function to handle file reading errors
handleFileReadError :: SomeException -> IO [[String]]
handleFileReadError ex = do
    putStrLn $ "Error reading file: " ++ show ex
    return []

-- Function to handle CSV parsing errors
handleCSVError :: SomeException -> IO [[String]]
handleCSVError ex = do
    putStrLn $ "Error parsing CSV: " ++ show ex
    return []

-- Function to handle training errors
handleTrainingError :: SomeException -> IO [Double]
handleTrainingError ex = do
    putStrLn $ "Error training model: " ++ show ex
    return []


main :: IO ()
main = do
    
    startRead <- getCurrentTime
    csvData <- handle handleFileReadError $ do
        contents <- readFile "data1.csv"
        handle handleCSVError $ return $ map (splitBy ',') (lines contents)
    endRead <- getCurrentTime

    let columnToExtractInitial = 0  -- Column index (0-based) for initial values
        columnToExtractActual = 1    -- Column index for actual values

    let initialValues = case csvData of
            [] -> [1.0, 2.0, 3.0, 4.0, 5.0]  
            _ -> extractColumn columnToExtractInitial csvData

        actualValues = case csvData of
            [] -> []  
            _ -> extractColumn columnToExtractActual csvData

    let phi = 0.7  -- AR(1) parameter
    predictionsAR1 <- handle handleTrainingError $ do
        startTrainingAR1 <- getCurrentTime
        threadDelay 1000000
        let predictions = trainAR1Model initialValues phi
        endTrainingAR1 <- getCurrentTime
        let processingTimeTrainingAR1 = realToFrac (diffUTCTime endTrainingAR1 startTrainingAR1) :: Float
        let adjustedProcessingTimeTrainingAR1 = processingTimeTrainingAR1 - 1.0
        putStrLn $ "Performance (AR(1) model training and prediction): " ++ show adjustedProcessingTimeTrainingAR1
        return predictions

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
    predictionsMA1 <- handle handleTrainingError $ do
        startTrainingMA1 <- getCurrentTime
        threadDelay 1000000
        let predictions = trainMA1Model initialValues theta
        endTrainingMA1 <- getCurrentTime
        let processingTimeTrainingMA1 = realToFrac (diffUTCTime endTrainingMA1 startTrainingMA1) :: Float
        let adjustedProcessingTimeTrainingMA1 = processingTimeTrainingMA1 - 1.0
        putStrLn $ "Performance (MA(1) model training and prediction): " ++ show adjustedProcessingTimeTrainingMA1

    print predictions

    let processingTimeTraining = realToFrac (diffUTCTime endTraining startTraining) :: Float
    let adjustedProcessingTimeTraining = processingTimeTraining - 1.0

    putStrLn $ "Performance (MA(1) model training and prediction):" ++ show adjustedProcessingTimeTraining

    let accuracy = calculateAccuracy predictions actualValues
    putStrLn $ "Accuracy of predictions: " ++ show accuracy

    -- Time Series Plot
    let timePoints = [1..length actualValues]
    toFile def "time_series_plot.png" $ do
        layout_title .= "Actual vs Predicted Values"
        plot (line "Actual" [zip timePoints actualValues])
        plot (line "AR(1) Predicted" [zip timePoints predictionsAR1])
        plot (line "MA(1) Predicted" [zip timePoints predictionsMA1])
 
    -- Performance Metrics Plot
    toFile def "performance_metrics_plot.png" $ do
        layout_title .= "Performance Metrics"
        plot (bars "Processing Time" [ (1, processingTimeRead), (2, adjustedProcessingTimeTraining) ])
 
    -- Accuracy Comparison Plot
    toFile def "accuracy_comparison_plot.png" $ do
        layout_title .= "Accuracy of Predictions"
        plot (bars "Accuracy" [ (1, accuracyAR1), (2, accuracyMA1) ])

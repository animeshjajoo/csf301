import Data.Time
import System.IO

import Control.Concurrent (threadDelay)
import Control.Exception (catch, IOException)

-- import Data.Colour
-- import Graphics.Rendering.Chart.Easy
-- import Graphics.Rendering.Chart.Backend.Cairo


-- Function to split a string by a delimiter
splitBy :: Char -> String -> [String]
splitBy delimiter str = splitBy' delimiter str []

splitBy' :: Char -> String -> String -> [String]
splitBy' _ [] acc = [reverse acc]
splitBy' delimiter (c:cs) acc
    | c == delimiter = reverse acc : splitBy' delimiter cs []
    | otherwise = splitBy' delimiter cs (c : acc)
    

-- Read a file and parse it as CSV
-- readCSVFile :: FilePath -> IO (Either String [[String]])
-- readCSVFile fileName = do
--     handle <- openFile fileName ReadMode
--     contents <- hGetContents handle
--     let csvData = map (splitBy ',') (lines contents)
--     -- hClose handle
--     return (Right csvData) `catch` handleFileError


readCSVFile :: FilePath -> IO (Either String [[String]])
readCSVFile fileName = do
    result <- tryReadFile fileName
    case result of
        Left e -> return (Left (show (e :: IOException)))
        Right contents -> do
            let csvData = map (splitBy ',') (lines contents)
            return (Right csvData)

            

tryReadFile :: FilePath -> IO (Either IOException String)
tryReadFile fileName = catch (Right <$> readFile fileName) (\e -> return (Left e))

handleFileError :: IOError -> IO (Either String a)
handleFileError e = return $ Left ("Error reading file: " ++ show e)

-- Extract a specific column from CSV data
extractColumn :: Int -> [[String]] -> Either String [Double]
extractColumn col csvData
    | null csvData = Left "CSV data is empty"
    | col < 0 || col >= length (head csvData) = Left "Invalid column index"
    | otherwise = Right $ map (read . (!! col)) csvData

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
    csvDataResult <- readCSVFile "data1.csv"
    endRead <- getCurrentTime

    case csvDataResult of
        Left errMsg -> putStrLn errMsg
        Right csvData -> do
            let columnToExtractInitial = 0 -- Column index (0-based) for initial values
                columnToExtractActual = 1 -- Column index for actual values

            let initialValues = case extractColumn columnToExtractInitial csvData of
                    Left errMsg -> [1.0, 2.0, 3.0, 4.0, 5.0]
                    Right values -> values

                actualValues = case extractColumn columnToExtractActual csvData of
                    Left errMsg -> []
                    Right values -> values



                    

            startTraining <- getCurrentTime
            let phi = 0.7 -- AR(1) parameter
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
            let theta = 0.7 -- MA(1) parameter
            threadDelay 1000000



            

            -- Train the MA(1) model
            let predictionsMA = trainMA1Model initialValues theta
            end <- getCurrentTime

            -- let processingTime = diffUTCTime end start

            putStrLn "Predicted values using MA(1) model:"
            print predictionsMA

            let processingTimeTraining = realToFrac (diffUTCTime endTraining startTraining) :: Float
            let adjustedProcessingTimeTraining = processingTimeTraining - 1.0

            putStrLn $ "Performance (MA(1) model training and prediction):" ++ show adjustedProcessingTimeTraining

            let accuracyMA = calculateAccuracy predictionsMA actualValues
            putStrLn $ "Accuracy of predictions: " ++ show accuracyMA

            

            -- Time Series Plot
            -- let timePoints = [1..length actualValues]
            -- toFile def "time_series_plot.png" $ do
            --     layout_title .= "Actual vs Predicted Values"
            --     plot (line "Actual" [zip timePoints actualValues])
            --     plot (line "AR(1) Predicted" [zip timePoints predictions])
            --     plot (line "MA(1) Predicted" [zip timePoints predictionsMA])
        
            -- -- Performance Metrics Plot
            -- toFile def "performance_metrics_plot.png" $ do
            --     layout_title .= "Performance Metrics"
            --     plot (bars "Processing Time" [ (1, processingTimeRead), (2, adjustedProcessingTimeTraining) ])
        
            -- -- Accuracy Comparison Plot
            -- toFile def "accuracy_comparison_plot.png" $ do
            --     layout_title .= "Accuracy of Predictions"
            --     plot (bars "Accuracy" [ (1, accuracy), (2, accuracyMA) ])
        
            -- where

            --     predictionsAR1 = trainAR1Model initialValues phi
            --     accuracyAR1 = calculateAccuracy predictionsAR1 actualValues
        
            --     predictionsMA1 = trainMA1Model initialValues theta
            --     accuracyMA1 = calculateAccuracy predictionsMA1 actualValues

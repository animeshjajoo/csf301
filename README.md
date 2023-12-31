# ARIMA Model Training and Prediction
## Group 56

| Name             | Campus ID     |
|------------------|---------------|
| Animesh Jajoo    | 2020B3A71260G |
| Pratik Lahiri    | 2020B3A71165G |
| Soumil Ghosh     | 2020B1A72102G |
| Gaurang Aswal    | 2020B1A71960G |

This repository contains Haskell code for training AR(1) and MA(1) models and making predictions on time series data in CSV format.

## Overview

The code provides functionalities to:
- Read a CSV file and parse its contents.
- Extract specific columns from the CSV data.
- Train an AR(1) model using initial values and a given parameter (phi).
- Train an MA(1) model using initial values and a given parameter (theta).
- Calculate accuracy between predicted and actual values.

## Software Architecture
- We have used Haskell to write our code from scratch without the use of libraries.
- The python file is reused using common known libraries.
- The CSV file used was downloaded locally though we can use online csv files from Yahoo or some other source.

## How Haskell is Ideal for this Use Case

### Functional Purity
Haskell's functional purity helps ensure side-effect-free functions, enhancing predictability and reliability in computations, crucial for numerical algorithms like these models.

### Type Safety
Strong static typing in Haskell minimizes runtime errors, aiding in catching potential bugs during development, crucial for numerical algorithms' accuracy.

### Immutability and Referential Transparency
Immutability in Haskell ensures that once data is created, it cannot be changed. This property is beneficial for maintaining the integrity of time series data and preventing unintended side effects during model training. Referential transparency aids in reasoning about the behavior of functions, enhancing predictability in computations.

### Concurrency Control
Haskell's concurrency control mechanisms enable efficient handling of computational tasks, beneficial for parallelizing and optimizing numerical computations.

### Lazy Evaluation
Haskell's lazy evaluation strategy allows for efficient handling of potentially infinite data structures. In time series analysis, where data streams can be continuous or very large, lazy evaluation can lead to more efficient resource utilization.

## What Haskell aspects we used?

### Monads: 
The IO monad is used extensively for handling file I/O operations and introducing delays.

### List Manipulation: 
Various list manipulation functions such as map, zipWith, sum, length, maximum, and list comprehensions.

### Concurrency: 
'threadDelay': Function for introducing a delay in the program, used here for simulating a time-consuming process.

### File I/O: 
'openFile', 'hGetContents', and 'hClose': Functions for opening a file, reading its contents, and closing the file handle, respectively.

### Lazy Evaluation: 
Haskell uses lazy evaluation, meaning that computations are only performed when the result is actually needed. The 'accuracy' value is computed when it is printed.

### Pattern Matching: 
Pattern matching is used in the definition of 'splitBy' to handle different cases based on the input.

### Exception Handling with Control.Exception:
Exception handling is implemented using the Control.Exception module to catch and handle specific exceptions, such as IOException during file I/O operations.

### Either Monad Usage:
The code utilizes the Either monad to handle errors, allowing explicit representation of success or failure in the result types.

## Usage

### Prerequisites
- Haskell GHC Compiler

### Running the Code
1. Ensure the required CSV file (e.g., data1.csv) is available.
2. Compile the Haskell code using GHC: `ghc -o main Main.hs`.
3. Execute the compiled code: `./main`.

## File Structure

- `Main.hs`: Main code file containing functions for model training and prediction.
- `data1.csv`: Sample CSV file for testing.

## Performance

The code reports performance metrics for reading CSV files and training/predicting using AR(1) and MA(1) models. Adjustments may be needed based on the dataset size and computational resources.

## Future Scopes

The focused future scopes for ARIMA models in Haskell and their potential real-world applications:

Incorporating Exogenous Variables:
Enhancing ARIMA models in Haskell to incorporate external factors such as economic indicators, weather patterns, or social trends as exogenous variables (ARIMAX models). This advancement could find applications in:

- Finance: Improved stock market prediction by considering economic indicators, news sentiment analysis, or geopolitical events alongside historical stock data.
- Epidemiology: Better forecasting of disease outbreaks by integrating health-related data like vaccination rates, climate conditions, or population movements into the model.
- Enhanced Seasonal Modeling: Advancing ARIMA models in Haskell to better capture and forecast seasonal variations in time series data. This improvement could benefit industries such as:

## Real World Applications

- Haskell's adeptness in mathematical computations and its capacity to handle quantitative algorithms suggest that financial institutions like Jane Street or Goldman Sachs have in-house tools developed in Haskell for time series analysis, encompassing models like ARIMA. Haskell's functional design and strong mathematical underpinnings are well-aligned with the demands of quantitative finance, positioning it as a viable choice for implementing sophisticated forecasting models such as ARIMA within these organizations. Nevertheless, detailed insights into their internal methodologies and tools remain closely guarded proprietary information.
- Haskell's adoption in blockchain technology, notably seen in projects like Cardano, showcases its prowess in building robust, secure, and mathematically precise blockchain solutions. Its strong type system and reliability make it apt for smart contract development, formal verification, and research within the blockchain sphere. Haskell's role in enhancing security, reducing vulnerabilities, and fostering innovation underscores its significance in shaping the future of blockchain technology.

## Acknowledgments

The code snippets and functionalities are adapted and extended from various numerical analysis libraries and research in time series analysis.

---

Feel free to contribute, report issues, or suggest enhancements!

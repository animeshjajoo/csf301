# ARIMA Model Training and Prediction

This repository contains Haskell code for training AR(1) and MA(1) models and making predictions on time series data in CSV format.

Haskell's strength in implementing ARIMA models stems from its functional nature, which aligns closely with the mathematical concepts behind time series analysis. With immutability, purity, and a strong type system, Haskell ensures precise and reliable data manipulation essential for ARIMA modeling. Its support for function composition and libraries for numerical computations further enhances the implementation process, allowing for efficient handling of time series data and calculations within the model.

In a recent comparison between Python and Haskell implementations of an ARIMA model for time series forecasting, the Haskell version outperformed Python, yielding notably superior results. Despite Python's popularity in data analysis and its rich ecosystem of libraries, Haskell's functional paradigm and strong type system seemed to provide an edge in this specific scenario. The precise handling of data transformations and computations in Haskell contributed to more accurate predictions, showcasing the language's prowess in modeling time series data. The outcome underscores how Haskell's mathematical foundations and inherent reliability can sometimes offer a performance advantage over widely used data analysis tools like Python in specific modeling contexts.

## Overview

The code provides functionalities to:
- Read a CSV file and parse its contents.
- Extract specific columns from the CSV data.
- Train an AR(1) model using initial values and a given parameter (phi).
- Train an MA(1) model using initial values and a given parameter (theta).
- Calculate accuracy between predicted and actual values.

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

## Acknowledgments

The code snippets and functionalities are adapted and extended from various numerical analysis libraries and research in time series analysis.

---

Feel free to contribute, report issues, or suggest enhancements!

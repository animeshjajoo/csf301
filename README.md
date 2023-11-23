# ARIMA Model Training and Prediction

This repository contains Haskell code for training AR(1) and MA(1) models and making predictions on time series data in CSV format.

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

### Immutability
Immutable data structures in Haskell prevent accidental changes, ensuring stability in complex calculations.

### Concurrency Control
Haskell's concurrency control mechanisms enable efficient handling of computational tasks, beneficial for parallelizing and optimizing numerical computations.

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

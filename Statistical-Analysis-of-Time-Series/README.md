# Seminar 4 — Time Series Analysis

**Course:** Statistical Analysis of Time Series  
**University:** University of Geneva (UNIGE) — MSc in Statistics  
**Language:** R

## Overview

This script explores time series decomposition, detrending, seasonality, autocorrelation, and data transformations using US electricity consumption and gas flow datasets.

## Contents

1. **Classical Decomposition:** Additive decomposition into trend, seasonal, and irregular components using centered moving averages.
2. **OLS Detrending:** Linear time trend regression with monthly seasonal dummies and residual diagnostics.
3. **Seasonal Differencing:** 12-month differencing to remove seasonality and comparison with OLS residuals.
4. **ACF and PACF:** Analysis of serial dependence in detrended and seasonally differenced series.
5. **Gas Flow Analysis:** Time series visualization, descriptive statistics, and empirical distributions.
6. **Box-Cox Transformations:** Comparison of alternative transformation parameters to study distributional shape.

## Requirements

R packages:
- `psych`
- `forecast`

## Data

- `electricity.txt` — Monthly US electricity consumption.
- `GasFlowData.csv` — Gas flow measurements.

**Note:** Update the local file paths before running the script.

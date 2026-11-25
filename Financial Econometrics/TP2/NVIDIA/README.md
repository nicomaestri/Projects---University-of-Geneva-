# NVIDIA Stock Price Dynamics: Trend and Volatility Analysis

## Overview

This project provides an empirical time series analysis of NVIDIA (NASDAQ: NVDA) stock prices, focusing on deterministic trends, stationarity and conditional volatility.

The objective is to investigate the evolution of stock prices relative to an estimated linear trend and to characterize the time-varying volatility of daily stock returns through a GARCH framework.

## Data

- **Asset:** NVIDIA Corporation (NVDA)
- **Source:** Yahoo Finance
- **Frequency:** Daily
- **Sample period:** January 2015 – latest available observation
- **Variable:** Adjusted closing price (USD)

Data are automatically retrieved using the `quantmod` R package.

## Methodology

### 1. Deterministic Trend Estimation

A linear deterministic trend is estimated by Ordinary Least Squares (OLS), using observations from the minimum stock price recorded between 2023 and 2024 to June 30, 2025.

The estimated trend is subsequently extrapolated out of sample to investigate deviations of observed stock prices from their historical trajectory.

### 2. Stationarity Analysis

The Kwiatkowski–Phillips–Schmidt–Shin (KPSS) test is employed to assess the null hypothesis of trend stationarity over the estimation sample.

OLS residuals are also examined to investigate the properties of the detrended price series.

### 3. Rolling Volatility Analysis

Daily logarithmic returns are computed from adjusted closing prices.

A seven-trading-day rolling standard deviation is employed to examine short-term volatility dynamics. Standardized rolling volatility measures of log returns and OLS residuals are compared.

### 4. Conditional Volatility Modelling

A GARCH(1,1) model with Student-t innovations is estimated by maximum likelihood to capture volatility clustering, persistence and heavy-tailed return distributions.

The estimated conditional variance and standard deviation are used to investigate the evolution of financial risk over time.

Daily returns are also plotted alongside conditional volatility bands defined by plus and minus two conditional standard deviations.

## Outputs

- Stock prices and estimated deterministic trend
- Out-of-sample trend extrapolation
- OLS residuals and regression diagnostics
- KPSS stationarity test
- Rolling standard deviations
- Daily logarithmic and squared returns
- Deviations from the estimated trend
- GARCH(1,1) conditional variance
- Daily returns and conditional volatility bands

## Software and Dependencies

The analysis is entirely implemented in R using the following packages:

- `quantmod` — financial data retrieval
- `urca` — stationarity testing
- `zoo` — rolling statistics
- `rugarch` — conditional volatility modelling

## Reproducibility

The R script automatically retrieves the available data and performs the entire empirical analysis. Results may change over time as new observations become available.

## Limitations

The deterministic trend is estimated over a selected historical period and should be interpreted as an exploratory statistical benchmark rather than a measure of fundamental value.

Deviations from the estimated trend do not necessarily imply stock price overvaluation or undervaluation, particularly in the absence of trend stationarity.

The conditional volatility bands are descriptive and do not represent exact 95% prediction intervals under Student-t innovations.

This project is intended for academic and educational purposes and does not constitute investment advice.

## Author

Niccolò Maestri

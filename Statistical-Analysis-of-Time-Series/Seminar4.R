
library(psych)
library(forecast)
# Seminar 2 R - Time series analysis #

elecUS <- read.table("C:/Users/Lenovo/Downloads/electricity.txt"
                     )

gas <- read.csv("C:/Users/Lenovo/Downloads/GasFlowData.csv")

gas.ts <- ts(
  gas,
  start = 1,
  frequency = 1
)

elecUS.ts <- ts(
  elecUS,
  start = c(1985,1),
  frequency = 12
)

ts.plot(
  elecUS.ts,
  xlab = "Date",
  ylab = "Electricity Consumption",
  lwd = 2
)

# Trend - seasonality - white noise #
# Use a centered moving average #

# Trend #
weights <- c(1 / 24, rep(1 / 12, 11), 1 / 24)

trend <- stats::filter(
  elecUS.ts,
  filter = weights,
  sides = 2
)

# Seasonality #

detrended <- elecUS.ts - trend

seasonal_factors <- tapply(
  as.numeric(detrended),
  cycle(elecUS.ts),
  mean,
  na.rm = TRUE
)

seasonal_factors <- seasonal_factors - mean(seasonal_factors)

seasonal <- ts(
  seasonal_factors[cycle(elecUS.ts)],
  start = start(elecUS.ts),
  frequency = frequency(elecUS.ts)
)
residual <- elecUS.ts - trend - seasonal

par(mfrow = c(4, 1), mar = c(2, 4, 2, 1))

plot(elecUS.ts, main = "Observed series", ylab = "Consumption")
plot(trend, main = "Trend-cycle", ylab = "Trend")
plot(seasonal, main = "Seasonal component", ylab = "Seasonal")
plot(residual, main = "Irregular component", ylab = "Irregular")

par(mfrow = c(1, 1))

acf(na.omit(residual), lag.max = 24)

# 2d #

# Time trend
electime <- 1:142

# Seasonal dummy matrix
elecseason <- model.matrix(~ factor(cycle(elecUS.ts)))

colnames(elecseason) <- c(
  "(Intercept)", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

# Complete design matrix
X <- cbind(
  Intercept = elecseason[, 1],
  Trend = electime,
  elecseason[, -1]
)

# Check design matrix
head(X)

# model #
#cancel the first colunm of elecseason that already contains a constant
elecUS.lm <- lm(
  elecUS.ts ~ electime + elecseason[, -1]
)

# Model diagnostics
plot(elecUS.lm)

# Residuals as a time series
residelecUS.ts <- ts(
  resid(elecUS.lm),
  start = start(elecUS.ts),
  frequency = frequency(elecUS.ts)
)

# Plot residuals
ts.plot(
  residelecUS.ts,
  xlab = "Date",
  ylab = "Residuals",
  lwd = 2
)

abline(h = 0, col = "gray", lty = 2)

# 2.2.2 - Differencing approach

# 2j - Seasonal differencing
diff12.elecUS.ts <- diff(elecUS.ts, lag = 12)

length(diff12.elecUS.ts)  # 130 observations


# 2k - Plot differenced series
ts.plot(
  diff12.elecUS.ts,
  xlab = "Date",
  ylab = "YoY Delta - Electricity Consumption",
  lwd = 2
)

abline(h = 0, col = "gray", lty = 2)


# 2l - Compare with OLS residuals
ts.plot(
  diff12.elecUS.ts,
  residelecUS.ts,
  col = c("blue", "black"),
  xlab = "Date",
  ylab = "Electricity Consumption (changes/deviations)",
  lwd = 2
)

legend(
  "topleft",
  legend = c("Seasonal Difference", "OLS Residuals"),
  col = c("blue", "black"),
  lty = 1,
  lwd = 2,
  bty = "n"
)
# 2.3.1 - ACF and PACF
# 2.3.1 - ACF and PACF

par(mfrow = c(2, 2),
    mar = c(3, 3, 3, 1))

acf(na.omit(residelecUS.ts), lag.max = 24,
    main = "ACF - OLS Residuals",
    cex.main = 0.85)

pacf(na.omit(residelecUS.ts), lag.max = 24,
     main = "PACF - OLS Residuals",
     cex.main = 0.85)

acf(na.omit(diff12.elecUS.ts), lag.max = 24,
    main = "ACF - Seasonal Differences",
    cex.main = 0.85)

pacf(na.omit(diff12.elecUS.ts), lag.max = 24,
     main = "PACF - Seasonal Differences",
     cex.main = 0.85)

# Restore default layout
par(mfrow = c(1, 1))

# plot gas


gas.rescaled <- gas.ts / 10^4
ts.plot(
  gas.rescaled,
  col = "black",
  lty = c(1, 2, 3),
  lwd = 2,
  xlab = "Time",
  ylab = "Gas Flow"
)

legend(
  "bottomright",
  legend = colnames(gas.ts),
  col = "black",
  lty = c(1, 2, 3),
  lwd = 2,
  bty = "n"
)

# distributions

par(mfrow = c(1, 3))

hist(gas.rescaled[, "Flow1"], main = "Flow1")
hist(gas.rescaled[, "Flow2"], main = "Flow2")
hist(gas.rescaled[, "Flow3"], main = "Flow3")

par(mfrow = c(1, 1))
psych::describe(gas.rescaled)


# box-cox #
flow2 <- gas.rescaled[, "Flow2"]

# Box-Cox transformation
flow2_bc <- BoxCox(flow2, lambda = 2)

hist(flow2_bc, main = "Box-Cox - Lambda = 2")

par(mfrow = c(2, 4))

hist(flow2, main = "Original")

hist(BoxCox(flow2, 0), main = "Log")
hist(BoxCox(flow2, 2), main = "Lambda = 2")
hist(BoxCox(flow2, 3), main = "Lambda = 3")
hist(BoxCox(flow2, 4), main = "Lambda = 4")
hist(BoxCox(flow2, 5), main = "Lambda = 5")
hist(BoxCox(flow2, 6), main = "Lambda = 6")
hist(BoxCox(flow2, 7), main = "Lambda = 7")

par(mfrow = c(1, 1))

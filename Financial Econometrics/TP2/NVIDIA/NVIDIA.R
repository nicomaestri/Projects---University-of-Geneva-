
library(quantmod)
library(urca)

# 1. DATA --------------------------------------------------

nvda <- getSymbols("NVDA", src = "yahoo",
                   from = "2015-01-01", auto.assign = FALSE)

df <- data.frame(
  date = as.Date(index(nvda)),
  price = as.numeric(Ad(nvda))
)

# 2. LINEAR TREND (OLS) ------------------------------------

# Starting point: minimum between 2023 and 2024
window <- df$date >= "2023-01-01" & df$date <= "2024-12-31"
start_date <- df$date[which(window)[which.min(df$price[window])]]

fit_end <- as.Date("2025-06-30")
df$t <- as.numeric(df$date - start_date)

train <- df$date >= start_date & df$date <= fit_end
view <- df$date >= start_date

model <- lm(price ~ t, data = df[train, ])
summary(model)

df$trend <- predict(model, newdata = df)

par(mfrow = c(2,2))
plot(model)
par(mfrow = c(1,1))


# 3. TREND EXTRAPOLATION -----------------------------------

future_dates <- seq(max(df$date) + 1, by = "day", length.out = 180)

future <- data.frame(
  date = future_dates,
  t = as.numeric(future_dates - start_date)
)

future$trend <- predict(model, newdata = future)

# 4. PRICE AND TREND PLOT ----------------------------------

plot(df$date[view], df$price[view],
     type = "l", col = "black", lwd = 1,
     xlim = c(start_date, max(future$date)),
     ylim = range(c(df$price[view], df$trend[view],
                    future$trend)),
     xlab = "Date", ylab = "Adjusted Price (USD)",
     main = "NVIDIA - Linear Trend")

# In-sample trend
lines(df$date[train], df$trend[train],
      col = "red", lwd = 2)

# Out-of-sample extrapolation
out <- df$date >= max(df$date[train])

lines(c(df$date[out], future$date),
      c(df$trend[out], future$trend),
      col = "red", lwd = 2, lty = 2)

# Current deviation
last <- nrow(df)

segments(df$date[last], df$price[last],
         df$date[last], df$trend[last],
         col = "blue", lwd = 2, lty = 3)

abline(v = df$date[last], col = "gray60", lty = 3)

legend("topleft",
       legend = c("Actual Price", "Linear Trend",
                  "Extrapolation", "Current Gap"),
       col = c("black", "red", "red", "blue"),
       lty = c(1, 1, 2, 3),
       lwd = c(1, 2, 2, 2), bty = "n")

# 5. OLS RESIDUALS -----------------------------------------

res <- residuals(model)

plot(df$date[train], res,
     type = "l", col = "black",
     xlab = "Date", ylab = "OLS Residuals (USD)",
     main = "NVIDIA - Detrended Prices")

abline(h = 0, col = "red", lty = 2)

# 6. KPSS TEST ---------------------------------------------

kpss <- ur.kpss(df$price[train],
                type = "tau", lags = "short")

summary(kpss)


library(zoo)

# 7. ROLLING VARIANCE --------------------------------------

df$return <- c(NA, diff(log(df$price)))

df$rolling_var <- rollapply(
  df$return,
  width = 7,
  FUN = sd,
  fill = NA,
  align = "right"
)

plot(df$date[train], df$rolling_var[train],
     type = "l", col = "red",
     xlab = "Date",
     ylab = "Rolling Standard Deviation",
     main = "NVIDIA - 30-Day Rolling Standard Deviation")

stdevres <- rollapply(
  res,
  width = 7,
  FUN = sd,
  fill = NA,
  align = "right"
)

# 7. ROLLING STANDARD DEVIATION ----------------------------

library(zoo)

df$return <- c(NA, diff(log(df$price)))

df$rolling_sd <- rollapply(
  df$return,
  width = 7,
  FUN = sd,
  fill = NA,
  align = "right"
)

stdevres <- rollapply(
  res,
  width = 7,
  FUN = sd,
  fill = NA,
  align = "right"
)

# Standardize both series for comparison
sd_returns <- as.numeric(scale(df$rolling_sd[train]))
sd_residuals <- as.numeric(scale(stdevres))

# Plot
plot(df$date[train], sd_returns,
     type = "l", col = "red",
     ylim = range(c(sd_returns, sd_residuals), na.rm = TRUE),
     xlab = "Date",
     ylab = "Standardized Rolling SD",
     main = "NVIDIA - 7-Day Rolling Standard Deviation")

lines(df$date[train], sd_residuals,
      type = "l",
      col = "black"
      )

legend("topleft",
       legend = c("Log Returns", "OLS Residuals"),
       col = c("red", "black"),
       lty = 1,
       bty = "n")
gap <- df$price[last] - df$trend[last]
print(gap)


plot(df$date, df$return,
     type = "l", 
     col = "black",
     xlab = "Date",
     ylab = "log returns",
     main = "NVIDIA log returns")

df$returnsquared <- df$return^2

plot(df$date, df$returnsquared,
     type = "l", 
     col = "black",
     xlab = "Date",
     ylab = "Squared Returns",
     main = "NVIDIA Squared Returns")


df$gap <- df$price - df$trend

plot(df$date[view], df$gap[view],
     type = "l",
     lwd = 1,
     col = "red",
     xlab = "Date",
     ylab = "Deviation from Linear Trend (USD)",
     main = "NVIDIA - Actual Price Deviation from Estimated Trend"
)

abline(h = 0, col = "black", lty = 2)




# 8. GARCH(1,1) --------------------------------------------

library(rugarch)

# Use existing log returns, expressed in percentage points
valid <- which(is.finite(df$return))
r <- 100 * df$return[valid]

# Model specification
spec <- ugarchspec(
  variance.model = list(
    model = "sGARCH",
    garchOrder = c(1, 1)
  ),
  mean.model = list(
    armaOrder = c(0, 0),
    include.mean = TRUE
  ),
  distribution.model = "std"
)

# Maximum likelihood estimation
fit <- ugarchfit(
  spec = spec,
  data = r,
  solver = "hybrid"
)

show(fit)

# Conditional variance and standard deviation
# Convert back to original log-return units
df$garch_var <- NA_real_
df$garch_sd <- NA_real_

df$garch_sd[valid] <- as.numeric(sigma(fit)) / 100
df$garch_var[valid] <- df$garch_sd[valid]^2

# Plot conditional variance
plot(df$date[valid], df$garch_var[valid],
     type = "l", col = "red", lwd = 1.5,
     xlab = "Date",
     ylab = "Conditional Variance",
     main = "NVIDIA - GARCH(1,1) Conditional Variance")

# Print latest estimates
cat("Current conditional variance:",
    tail(df$garch_var[valid], 1), "\n")

cat("Current daily conditional volatility:",
    100 * tail(df$garch_sd[valid], 1), "%\n")


# 9. LOG RETURNS AND GARCH VOLATILITY ----------------------

# Convert returns and conditional SD to percentage points
returns <- 100 * df$return[valid]
vol <- 100 * df$garch_sd[valid]

# Plot
plot(df$date[valid], returns,
     type = "p",
     col = "black",
     lwd = 0.7,
     ylim = range(c(returns, -2*vol, 2*vol), na.rm = TRUE),
     xlab = "Date",
     ylab = "Daily Log Returns (%)",
     main = "NVIDIA - Daily Returns and GARCH(1,1) Volatility")

# Conditional volatility bands
lines(df$date[valid],  2*vol,
      col = "red", lwd = 1.5)

lines(df$date[valid], -2*vol,
      col = "red", lwd = 1.5)

abline(h = 0, col = "gray60", lty = 2)

legend("topright",
       legend = c("Log Returns", "GARCH +/- 2 SD"),
       col = c("black", "red"),
       lty = 1,
       lwd = c(0.7, 1.5),
       bty = "n")


# 10. EXPORT ALL FIGURES -----------------------------------

dir.create("figures", showWarnings = FALSE)

# Function for exporting high-resolution figures
save_plot <- function(filename, expr) {
  
  png(file.path("figures", filename),
      width = 2400,
      height = 1400,
      res = 250,
      bg = "white")
  
  on.exit(dev.off(), add = TRUE)
  
  par(mar = c(4.5, 4.5, 3, 1.5))
  
  force(expr)
}


# 01. Linear trend and extrapolation -----------------------

save_plot("01_linear_trend.png", {
  
  plot(df$date[view], df$price[view],
       type = "l", col = "black", lwd = 1,
       xlim = c(start_date, max(future$date)),
       ylim = range(c(df$price[view], df$trend[view],
                      future$trend)),
       xlab = "Date",
       ylab = "Adjusted Price (USD)",
       main = "NVIDIA - Linear Trend")
  
  lines(df$date[train], df$trend[train],
        col = "red", lwd = 2)
  
  lines(c(df$date[out], future$date),
        c(df$trend[out], future$trend),
        col = "red", lwd = 2, lty = 2)
  
  segments(df$date[last], df$price[last],
           df$date[last], df$trend[last],
           col = "blue", lwd = 2, lty = 3)
  
  abline(v = df$date[last], col = "gray60", lty = 3)
  
  legend("topleft",
         legend = c("Actual Price", "Linear Trend",
                    "Extrapolation", "Current Gap"),
         col = c("black", "red", "red", "blue"),
         lty = c(1, 1, 2, 3),
         lwd = c(1, 2, 2, 2),
         bty = "n")
})


# 02. OLS diagnostics --------------------------------------

save_plot("02_ols_diagnostics.png", {
  
  par(mfrow = c(2, 2))
  plot(model)
})


# 03. OLS residuals ----------------------------------------

save_plot("03_ols_residuals.png", {
  
  plot(df$date[train], res,
       type = "l", col = "black",
       xlab = "Date",
       ylab = "OLS Residuals (USD)",
       main = "NVIDIA - Detrended Prices")
  
  abline(h = 0, col = "red", lty = 2)
})


# 04. Rolling standard deviation ---------------------------

save_plot("04_rolling_sd.png", {
  
  plot(df$date[train], df$rolling_sd[train],
       type = "l", col = "red",
       xlab = "Date",
       ylab = "Rolling Standard Deviation",
       main = "NVIDIA - 7-Day Rolling Standard Deviation")
})


# 05. Standardized rolling volatility comparison -----------

save_plot("05_rolling_sd_comparison.png", {
  
  plot(df$date[train], sd_returns,
       type = "l", col = "red",
       ylim = range(c(sd_returns, sd_residuals),
                    na.rm = TRUE),
       xlab = "Date",
       ylab = "Standardized Rolling SD",
       main = "NVIDIA - Rolling Volatility Comparison")
  
  lines(df$date[train], sd_residuals,
        col = "black")
  
  legend("topleft",
         legend = c("Log Returns", "OLS Residuals"),
         col = c("red", "black"),
         lty = 1, bty = "n")
})


# 06. Daily log returns ------------------------------------

save_plot("06_log_returns.png", {
  
  plot(df$date, df$return,
       type = "l", col = "black",
       xlab = "Date",
       ylab = "Log Returns",
       main = "NVIDIA - Daily Log Returns")
  
  abline(h = 0, col = "gray60", lty = 2)
})


# 07. Squared returns --------------------------------------

save_plot("07_squared_returns.png", {
  
  plot(df$date, df$returnsquared,
       type = "l", col = "black",
       xlab = "Date",
       ylab = "Squared Returns",
       main = "NVIDIA - Squared Returns")
})


# 08. Deviation from estimated trend -----------------------

save_plot("08_trend_deviation.png", {
  
  plot(df$date[view], df$gap[view],
       type = "l", col = "red", lwd = 1,
       xlab = "Date",
       ylab = "Deviation from Linear Trend (USD)",
       main = "NVIDIA - Deviation from Estimated Trend")
  
  abline(h = 0, col = "black", lty = 2)
})


# 09. GARCH conditional variance ---------------------------

save_plot("09_garch_variance.png", {
  
  plot(df$date[valid], df$garch_var[valid],
       type = "l", col = "red", lwd = 1.5,
       xlab = "Date",
       ylab = "Conditional Variance",
       main = "NVIDIA - GARCH(1,1) Conditional Variance")
})


# 10. Returns and GARCH volatility bands --------------------

save_plot("10_garch_volatility.png", {
  
  plot(df$date[valid], returns,
       type = "p", col = "black",
       pch = 20, cex = 0.5,
       ylim = range(c(returns, -2*vol, 2*vol),
                    na.rm = TRUE),
       xlab = "Date",
       ylab = "Daily Log Returns (%)",
       main = "NVIDIA - Daily Returns and GARCH(1,1) Volatility")
  
  lines(df$date[valid], 2*vol,
        col = "red", lwd = 1.5)
  
  lines(df$date[valid], -2*vol,
        col = "red", lwd = 1.5)
  
  abline(h = 0, col = "gray60", lty = 2)
  
  legend("topright",
         legend = c("Log Returns", "GARCH +/- 2 SD"),
         col = c("black", "red"),
         pch = c(20, NA),
         lty = c(NA, 1),
         bty = "n")
})


# Export confirmation --------------------------------------

cat("\nFigures saved in:\n",
    normalizePath("figures"), "\n")

print(list.files("figures"))
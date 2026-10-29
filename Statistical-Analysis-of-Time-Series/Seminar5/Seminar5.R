ir <- read.table("C:/Users/Lenovo/Downloads/UKinterestrates.txt")

ir.ts <- ts(
  ir,
  start = c(1953,3),
  frequency = 12
)

ts.plot(
  ir.ts,
  xlab = "Date",
  ylab = "Nominal Interest Rate",
  lwd = 2
)

# Trend - seasonality - white noise #
# Use a centered moving average #

# Trend #
weights <- c(1 / 24, rep(1 / 12, 11), 1 / 24)

trend <- stats::filter(
  ir.ts,
  filter = weights,
  sides = 2
)

# Seasonality #

detrended <- ir.ts - trend

seasonal_factors <- tapply(
  as.numeric(detrended),
  cycle(ir.ts),
  mean,
  na.rm = TRUE
)

seasonal_factors <- seasonal_factors - mean(seasonal_factors)
seasonal <- ts(
  seasonal_factors[cycle(ir.ts)],
  start = start(ir.ts),
  frequency = frequency(ir.ts)
)
residual <- ir.ts - trend - seasonal
par(mfrow = c(4, 1), mar = c(2, 4, 2, 1))

plot(ir.ts, main = "Observed series", ylab = "Interest Rate")
plot(trend, main = "Trend-cycle", ylab = "Trend")
plot(seasonal, main = "Seasonal component", ylab = "Seasonal")
plot(residual, main = "Irregular component", ylab = "Irregular")

par(mfrow = c(1, 1))


# exercise 3B #

y <- as.numeric(ir.ts)
t <- seq_along(y) 

# Candidate polynomial degrees
degrees <- 0:8

# Estimate models
models <- lapply(degrees, function(k) {
  if (k == 0) {
    lm(y ~ 1)
  } else {
    lm(y ~ poly(t, k))
  }
})

# Calculate AIC for each model
aic_values <- sapply(models, AIC)

# Compare results
data.frame(
  Degree = degrees,
  AIC = aic_values
)

# model identification

best_index <- which.min(aic_values)

best_k <- degrees[best_index]
best_k

# save model
interest.lm <- models[[best_index]]

summary(interest.lm)

plot(
  ir.ts,
  main = "Polynomial Trend",
  ylab = "Nominal Interest Rate",
  lwd = 1
)

lines(
  ts(
    fitted(interest.lm),
    start = start(ir.ts),
    frequency = frequency(ir.ts)
  ),
  col = "black",
  lwd = 2
)

plot(
  ir.ts,
  main = "Polynomial vs Moving Average Trend",
  xlab = "Date",
  ylab = "Nominal Interest Rate",
  col = "grey50",
  lwd = 1
)

lines(
  ts(
    fitted(interest.lm),
    start = start(ir.ts),
    frequency = frequency(ir.ts)
  ),
  col = "black",
  lwd = 2
)

lines(
  trend,
  col = "red",
  lwd = 2
)

legend(
  "topleft",
  legend = c(
    "Original Series",
    "Polynomial Trend (k = 7)",
    "Centered Moving Average"
  ),
  col = c("grey50", "black", "red"),
  lwd = c(1, 2, 2),
  bty = "n"
)

# Exercise 3c
# Restricted model: polynomial trend only
interest.lm <- models[[best_index]]


# Monthly dummy variables
month <- factor(cycle(ir.ts))

# Unrestricted model: trend + seasonality
interest.seasonal <- lm(
  y ~ poly(t, 7) + month
)

# Joint significance test
anova(interest.lm, interest.seasonal)

# Conclusion --> adding temporal dummies is useless 
# 3d

plot(interest.lm)

#3f

acf(
  residuals(interest.lm),
  lag.max = 100,
  main = "ACF - Polynomial Trend Residuals"
)

pacf(
  residuals(interest.lm),
  lag.max = 50,
  main = "PACF - Polynomial Trend Residuals"
)

## The ACF shows strong persistence and a gradual, oscillatory decay.
# The PACF exhibits two significant spikes at lags 1 and 2, followed
# by mostly insignificant values. This suggests an AR(2) process,
# since the PACF of an AR(p) theoretically cuts off after lag p,
# while the ACF gradually decays.

# Exercise 3f #

# Detrended series
e <- residuals(interest.lm)

# Store results
results <- data.frame(
  p = integer(),
  q = integer(),
  AIC = numeric()
)

models <- list()

# Estimate all ARMA(p,q), p <= 3, q <= 3
for (p in 0:3) {
  for (q in 0:3) {
    
    fit <- tryCatch(
      arima(
        e,
        order = c(p, 0, q),
        include.mean = FALSE,
        method = "ML"
      ),
      error = function(err) NULL
    )
    
    if (!is.null(fit)) {
      
      name <- paste0("ARMA(", p, ",", q, ")")
      models[[name]] <- fit
      
      results <- rbind(
        results,
        data.frame(
          p = p,
          q = q,
          AIC = AIC(fit)
        )
      )
    }
  }
}

# Rank models according to AIC
results <- results[order(results$AIC), ]

# Difference from minimum AIC
results$delta_AIC <- results$AIC - min(results$AIC)

print(results)

best_p <- results$p[1]
best_q <- results$q[1]

best_name <- paste0(
  "ARMA(", best_p, ",", best_q, ")"
)

best_model <- models[[best_name]]

summary(best_model)
best_model
arma_residuals <- residuals(best_model)
par(mfrow = c(2, 2))

plot(
  arma_residuals,
  type = "l",
  main = "ARMA Residuals"
)

acf(
  arma_residuals,
  main = "Residual ACF"
)

pacf(
  arma_residuals,
  main = "Residual PACF"
)

qqnorm(arma_residuals)
qqline(arma_residuals)

par(mfrow = c(1, 1))

# Compare the best ARMA candidates
head(results, 5)

# Ljung-Box test for residual autocorrelation
Box.test(
  residuals(best_model),
  lag = 24,
  type = "Ljung-Box",
  fitdf = best_p + best_q
)

# Residual variance
var(residuals(best_model))

# Compare ACF of detrended series and fitted ARMA residuals
par(mfrow = c(1, 2))

acf(e, lag.max = 48,
    main = "ACF - Detrended Series")

acf(residuals(best_model), lag.max = 48,
    main = "ACF - ARMA Residuals")

par(mfrow = c(1, 1))


############################################################
# EXERCISE 5 - Quantile Regression: AR(2) Simulation Study
############################################################

# 5.a Simulate n = 1000 observations from an AR(2)

set.seed(123)

n <- 1000
theta1 <- 0.4
theta2 <- 0.2
sigma <- sqrt(0.25)
tau <- 0.90

y <- arima.sim(
  model = list(ar = c(theta1, theta2)),
  n = n,
  sd = sigma
)

plot(
  y,
  type = "l",
  main = "Simulated AR(2) Process",
  xlab = "Time",
  ylab = "Y"
)


# 5.b Define the quantile regression loss function
# Check loss: rho_tau(u) = u * (tau - I(u < 0))

check_loss <- function(theta, y, X, tau) {
  
  u <- y - as.vector(X %*% theta)
  
  sum(u * (tau - (u < 0)))
  
}


# 5.c Construct the covariate matrix
# X_t = (Y_{t-1}, Y_{t-2}, 1)

X <- cbind(
  y[2:(n - 1)],
  y[1:(n - 2)],
  rep(1, n - 2)
)

colnames(X) <- c("Lag1", "Lag2", "Intercept")

Y <- y[3:n]

# True parameters at tau = 0.90
theta_true <- c(
  theta1,
  theta2,
  qnorm(tau, mean = 0, sd = sigma)
)

# True conditional quantiles
Q_true <- as.vector(X %*% theta_true)


# 5.d Estimate parameters by minimizing the check loss

estimation <- optim(
  par = c(0, 0, 0),
  fn = check_loss,
  y = Y,
  X = X,
  tau = tau,
  method = "Nelder-Mead",
  control = list(maxit = 20000, reltol = 1e-10)
)

theta_hat <- estimation$par

# Compare true and estimated parameters
comparison <- data.frame(
  Parameter = c("AR(1)", "AR(2)", "Intercept"),
  True = theta_true,
  Estimated = theta_hat
)

print(comparison)

# Check convergence (0 = successful convergence)
estimation$convergence

# Estimated conditional quantiles
Q_hat <- as.vector(X %*% theta_hat)


# 5.e Plot the simulated series and conditional quantiles

plot(
  1:n,
  y,
  type = "l",
  col = "grey60",
  lwd = 1,
  main = "AR(2) - True vs Estimated Conditional Quantiles",
  xlab = "Time",
  ylab = "Y"
)

lines(
  3:n,
  Q_true,
  col = "blue",
  lwd = 2
)

lines(
  3:n,
  Q_hat,
  col = "red",
  lwd = 2,
  lty = 2
)

legend(
  "topright",
  legend = c(
    "Simulated AR(2)",
    "True Quantile (90%)",
    "Estimated Quantile (90%)"
  ),
  col = c("grey60", "blue", "red"),
  lty = c(1, 1, 2),
  lwd = c(1, 2, 2),
  bty = "n"
)

# Quantile estimation accuracy
MAE <- mean(abs(Q_true - Q_hat))
RMSE <- sqrt(mean((Q_true - Q_hat)^2))

cat("MAE:", MAE, "\n")
cat("RMSE:", RMSE, "\n")


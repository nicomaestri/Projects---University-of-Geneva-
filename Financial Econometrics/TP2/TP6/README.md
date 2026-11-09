# TP6 — Bootstrap and Asymptotic Confidence Intervals

**Financial Econometrics | University of Geneva (UNIGE)**  
Course: Prof. Olivier Scaillet · Teaching Assistant: Ernest Ivanaj

## Overview

This project studies statistical inference for the **Sharpe ratio**, comparing asymptotic confidence intervals based on the **Delta Method** with **IID** and **non-overlapping block bootstrap** intervals. Through simulated returns and Monte Carlo experiments, it examines how sample size and serial dependence affect interval width and empirical coverage.

The analysis follows the TP6 assignment and includes an additional extension to a stationary **AR(1)** process.

## Notebook contents

1. **Delta Method:** Simulate Gaussian returns, estimate the Sharpe ratio, derive its asymptotic variance using the multivariate Delta Method, and construct a 95% confidence interval.
2. **IID Bootstrap:** Generate bootstrap resamples, construct basic (reverse-percentile) 95% confidence intervals, and compare them with Delta Method intervals for different sample sizes.
3. **Block Bootstrap:** Implement non-overlapping block resampling and compare IID and block bootstrap interval widths and coverage across independent Monte Carlo datasets.
4. **AR(1) extension:** Simulate serially dependent returns, examine theoretical and empirical ACF/PACF, illustrate how resampling affects autocorrelation, and evaluate Delta, IID bootstrap, and block bootstrap inference under dependence.

## Simulation setup

| Parameter | Baseline |
| --- | ---: |
| Mean return, μ | 1 |
| Return standard deviation, σ | 4 |
| True Sharpe ratio (zero risk-free rate) | 0.25 |
| Sample size, T | 100 |
| Bootstrap resamples per dataset, B | 2,000 |
| Non-overlapping block length | 10 |
| Monte Carlo datasets (IID comparison) | 100 |
| AR(1) coefficient, ρ | 0.8 |
| AR(1) sample size | 500 |
| Monte Carlo datasets (AR(1) comparison) | 500 |

The simulated returns are expressed in monthly percentage units; the Sharpe ratios are **not annualized**.

## Selected findings

The Monte Carlo experiments compare **average 95% confidence interval widths** and **empirical coverage** (the proportion of intervals containing the true Sharpe ratio).

| Data-generating process | Method | Average CI width | Empirical coverage |
| --- | --- | ---: | ---: |
| IID Gaussian | IID bootstrap | 0.4054 | 95.0% |
| IID Gaussian | Block bootstrap | 0.3714 | 89.0% |
| AR(1), ρ = 0.8 | Naive IID Delta Method | 0.1790 | 49.0% |
| AR(1), ρ = 0.8 | IID bootstrap | 0.1791 | 48.6% |
| AR(1), ρ = 0.8 | Block bootstrap | 0.4140 | 87.0% |

**Main takeaway:** IID-based inference can substantially underestimate uncertainty when returns are positively autocorrelated. Block resampling preserves dependence *within* blocks and improves coverage in the AR(1) experiment, although a fixed block length of 10 does not fully restore the nominal 95% coverage. These are simulation-specific results, not universal performance guarantees.

## Requirements and usage

- Python 3
- Jupyter Notebook or JupyterLab
- NumPy
- Matplotlib

Install the required packages:

```bash
pip install numpy matplotlib notebook
```

Open the TP6 notebook (`.ipynb`) in Jupyter or VS Code and run the cells sequentially. The notebook uses fixed random seeds for reproducibility and **does not require external datasets**.

## Scope

This is an educational simulation study. It focuses on the statistical properties of confidence intervals for a non-annualized Sharpe ratio, not on empirical investment recommendations.

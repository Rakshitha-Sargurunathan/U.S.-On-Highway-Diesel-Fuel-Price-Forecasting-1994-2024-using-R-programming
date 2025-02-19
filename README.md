# U.S.-On-Highway-Diesel-Fuel-Price-Forecasting-1994-2024-using-R-programming
This project analyzes and forecasts U.S. on-highway diesel fuel prices using advanced time series forecasting methods. By studying diesel price trends over the last three decades, we identified key factors such as seasonal fluctuations, market volatility, and external events like the 2008 financial crisis and the COVID-19 pandemic. I employed a combination of models including Time Series Regression, Exponential Smoothing (ETS), ARIMA, and an Ensemble Approach to generate accurate price forecasts.

The final model, an Ensemble Model, demonstrated robust accuracy, combining the strengths of multiple methodologies to capture both short-term and long-term price trends. The 36-month forecast provides insights into potential price movements, helping businesses, policymakers, and consumers make informed decisions in an unpredictable market.

## Project Overview

Diesel prices have a significant impact on transportation costs, shipping rates, and the price of consumer goods. Understanding the trends and predicting future prices is crucial for better decision-making in the energy and transportation sectors. The project includes:

- **Data Analysis**: Time series decomposition and seasonal analysis to identify key patterns and fluctuations in diesel prices.
- **Modeling**: Various time series forecasting models were applied, including:
  - **Time Series Regression**: To capture long-term trends and seasonal patterns.
  - **Exponential Smoothing (ETS)**: To account for trend and seasonality.
  - **ARIMA**: To model temporal dependencies and fluctuations.
  - **Ensemble Model**: A combination of multiple models using inverse variance weighting for more accurate and robust predictions.

## Key Findings

- The **Ensemble Model** provided the best results, outperforming individual models in terms of forecast accuracy, robustness, and stability.
- A 36-month operational forecast predicts moderate increases in diesel prices, with a reliable forecast within the first 12 months.
- The model highlights significant price volatility, especially in the long term, driven by external economic and geopolitical factors.

## Project Files

- **Data**: Historical diesel price data (1994-2024)
- **Scripts**: R code for data preprocessing, model building, evaluation, and forecasting
- **Results**: Model performance metrics, diagnostics, and forecast plots

## Getting Started

Clone this repository and run the R scripts to replicate the analysis and forecasting process. Make sure you have R installed along with the necessary libraries like `forecast`, `ggplot2`, and `dplyr`.

## Requirements

- R version 4.0 or later
- Libraries: `forecast`, `ggplot2`, `dplyr`, and others (listed in the script)

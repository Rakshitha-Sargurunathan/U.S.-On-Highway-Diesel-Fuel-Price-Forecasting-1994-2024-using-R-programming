#Open libraries
library(ggplot2)
library(dplyr)
library(tsibble)
library(feasts)
library(fpp3)

# Dataset Context
# - Monthly U.S. On-Highway Diesel Fuel Prices spanning 1994-2024
# - Price range: $0.959 to $5.754 per gallon with median of $2.580
# - Key dataset for transportation industry planning and economic forecasting
# - Shows significant price evolution over three decades

#Load and tidy the data
diesel_prices <- read.csv(file.choose(), header = TRUE)
diesel_prices <- diesel_prices %>%
  mutate(Month = as.Date(Month, format = "%m/%d/%Y"))

# Inspect the dataset
str(diesel_prices)  
summary(diesel_prices)  

#Convert the data to a tsibble and ensure it is monthly
diesel_prices.tb <- diesel_prices %>%
  mutate(Month = yearmonth(Month)) %>%
  as_tsibble(index = Month)

head(diesel_prices.tb)

#Create a training set
diesel_train <- diesel_prices.tb %>%
  filter(as.Date(Month) <= as.Date("2021-10-15"))

#Visualize your training data
#Create a time series plot.
ggplot(diesel_train, aes(x = Month, y = Price)) +
  geom_line() +
  labs(y = "Dollars per Gallon",
       x = "Date",
       title = "U.S. On-Highway Diesel Fuel Prices") +
  theme_minimal()

#	Create a seasonal plot
gg_season(diesel_train)+
  labs(y = "Dollars per Gallon",
       title = "U.S. On-Highway Diesel Fuel Prices")

#Create a sub-series plot
gg_subseries(diesel_train)+
  labs(y = "Dollars per Gallon",
       title = "U.S. On-Highway Diesel Fuel Prices")

# Time Series Characteristics
# - Strong upward trend with pronounced volatility periods
# - Clear seasonal patterns varying in intensity over time
# - Notable price shocks during 2008 and 2020
# - Increasing variance in recent years suggesting multiplicative effects
# - Evidence of structural breaks coinciding with economic events

# Regression Model  
Regression_model1 <- diesel_train %>%
  model(TSLM(Price ~ trend() + season()))
report(Regression_model1)   

#Quadratic trend
Regression_model2 <- diesel_train %>%
  model(TSLM(Price ~ trend() + I(trend()^2) + season()))
Regression_model2 %>% report()  

#Create a plot to compare the fit to the training data. Do not show the validation data or the forecast.
Regression_model2 %>%
  augment() %>%
  ggplot(aes(x = Month)) +
  geom_line(aes(y = Price, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(
    values = c(Data = "black", Fitted = "#D55E00")) +
  labs(y = "Dollars per Gallon",
       title = "Regression Model of U.S. On-Highway Diesel Fuel Prices") +
  guides(colour = guide_legend(title = "Series"))

#Create a plot to show the forecast and prediction interval.
Regression_model2 %>%
  forecast(h = 36) %>%
  autoplot(diesel_train) +
  labs(y = "Dollars per Gallon",
       title = "Regression Forecast of U.S. On-Highway Diesel Fuel Prices")
# Regression Model Analysis
# - Initial linear trend + seasonal model: R² = 0.5548
# - Quadratic trend improvement: R² = 0.6677
# - Strong trend significance (p < 2e-16 for both linear and quadratic terms) 
# - Seasonal components not individually significant (p > 0.05)
# - Model captures medium-term price dynamics but struggles with volatility


##ARIMA model
#Follow the iterative procedure to find the best ARIMA model for this time series.
diesel_train %>% features(Price, unitroot_nsdiffs) # D = 0
diesel_train %>% features(Price, unitroot_ndiffs) # d = 1

# We start by fitting the model with d=1 and D=0
ARIMA0.fit <- diesel_train %>% model(ARIMA(Price ~ pdq(0,1,0) + PDQ(0,0,0)))
report(ARIMA0.fit) # AIC=-455.33

augment(ARIMA0.fit) %>% gg_tsdisplay(.resid, plot_type='partial') 

#We have significant spikes at lags 1, 2, 8, 9 in the ACF
#and spikes at lag 1 and 8 in the PACF
#We could model the spikes at lags 1 and 2 in the ACF with q=2
#We could model the spike at lag 1 in the PACF with p=1
#The absence of significant seasonal spikes suggests no need for seasonal ARIMA parameters (P, D, Q).

# Check residuals for ARIMA(0,1,0)
augment(ARIMA0.fit) %>% 
  gg_tsdisplay(.resid, plot_type = 'partial')

# Fit ARIMA(1,1,0)
ARIMA1.fit <- diesel_train %>% 
  model(ARIMA(Price ~ pdq(1,1,0) + PDQ(0,0,0)))
report(ARIMA1.fit) # AIC = -542.38

# Fit ARIMA(0,1,1)
ARIMA2.fit <- diesel_train %>% 
  model(ARIMA(Price ~ pdq(0,1,1) + PDQ(0,0,0)))
report(ARIMA2.fit) # AIC = -535.66

# Fit ARIMA(1,1,1)
ARIMA3.fit <- diesel_train %>% 
  model(ARIMA(Price ~ pdq(1,1,1) + PDQ(0,0,0)))
report(ARIMA3.fit) # AIC = -543.61

#ARIMA Model 3 has the lowest AIC 
# Check residuals of the best model
augment(ARIMA3.fit) %>% 
  gg_tsdisplay(.resid, plot_type = 'partial')

#The residual plot shows mostly random behavior.
#There is only a significant spike at lag 11 in the ACF and PCF

# What ARIMA model would you recommend for this data? 
# ARIMA model 3 with pdq(1,1,1) + PDQ(0,0,0)

#Create a plot to compare the fit to the training data. Do not show the validation data or the forecast
ARIMA3.fit %>% 
  augment() %>%
  ggplot(aes(x = Month)) +
  geom_line(aes(y = Price, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(
    values = c(Data = "black", Fitted = "#D55E00")) +
  labs(y = "Dollars per Gallon",
       title = "ARIMA Model of U.S. On-Highway Diesel Fuel Prices") +
  guides(colour = guide_legend(title = "Series"))

#Create a plot to show the forecast and prediction interval.
ARIMA3.fit %>%
  forecast(h = 36) %>%
  autoplot(diesel_train) +
  labs(y = "Dollars per Gallon",
       title = "ARIMA Forecast of U.S. On-Highway Diesel Fuel Prices")

## Use auto.arima() to fit an ARIMA (p, d, q) (P, D, Q) model to the training data
ARIMAauto.fit <- diesel_train %>% model(ARIMA(Price))
report(ARIMAauto.fit) # AIC=-543.65
# This is an ARIMA(0,1,2)

# Create a plot to compare the fit to the training data. Do not show the validation data or the forecast.
ARIMAauto.fit %>%
  augment() %>%
  ggplot(aes(x = Month)) +
  geom_line(aes(y = Price, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(
    values = c(Data = "black", Fitted = "#D55E00")) +
  guides(colour = guide_legend(title = "Series"))

# Create a plot to show the forecast and prediction interval.
ARIMAauto.fit %>%
  forecast(h = 36) %>%
  autoplot(diesel_train) +
  labs(y = "Dollars per Gallon",
       title = "Auto ARIMA Forecast of U.S. On-Highway Diesel Fuel Prices")

# ARIMA Model Selection:
# - Unit root tests indicate d=1 for first differencing
# - ACF/PACF analysis suggests p=1, q=2 structure
# - ARIMA(0,1,2) selected by auto.arima with AIC=-543.65
# - Manual ARIMA(1,1,1) competitive with AIC=-543.61


# Fit the different exponential smoothing models for the diesel price data
# ETS Model Selection

# Model 1: Additive error, additive damped trend, and additive seasonality
ets_manual1 <- diesel_train %>% model(ETS(Price ~ error("A") + trend("Ad") + season("A")))
report(ets_manual1) # AIC value = 575.38

# Model 2: Multiplicative error, additive damped trend, and additive seasonality
ets_manual2 <- diesel_train %>% model(ETS(Price ~ error("M") + trend("Ad") + season("A")))
report(ets_manual2) # AIC value =439.93

# Model 3: Additive error, additive damped trend, and multiplicative seasonality
ets_manual3 <- diesel_train %>% model(ETS(Price ~ error("A") + trend("Ad") + season("M")))
report(ets_manual3) # AIC value = 605.98

# Model 4: Multiplicative error, additive damped trend, and multiplicative seasonality
ets_manual4 <- diesel_train %>% model(ETS(Price ~ error("M") + trend("Ad") + season("M")))
report(ets_manual4) # AIC value = 585.91

# ETS model 2 has the lowest AIC

#Create a plot to compare the fit to the training data. Do not show the validation data or the forecast
ets_manual2 %>% 
  augment() %>%
  ggplot(aes(x = Month)) +
  geom_line(aes(y = Price, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(
    values = c(Data = "black", Fitted = "#D55E00")) +
  labs(y = "Dollars per Gallon",
       title = "ETS Model of U.S. On-Highway Diesel Fuel Prices") +
  guides(colour = guide_legend(title = "Series"))

#Create a plot to show the forecast and prediction interval.
ets_manual2 %>%
  forecast(h = 36) %>%
  autoplot(diesel_train) +
  labs(y = "Dollars per Gallon",
       title = "ETS Forecast of U.S. On-Highway Diesel Fuel Prices")

#ETS Auto
ets_auto <- diesel_train %>% model(ETS(Price))
report(ets_auto) # AIC = 339.3844

ets_auto %>%
  augment() %>%
  ggplot(aes(x = Month)) +
  geom_line(aes(y = Price, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(
    values = c(Data = "black", Fitted = "#D55E00")) +
  guides(colour = guide_legend(title = "Series"))
# This is an ETS(M,Ad,N)

# Create a plot to show the forecast and prediction interval.
ets_auto %>%
  forecast(h = 36) %>%
  autoplot(diesel_train) +
  labs(y = "Dollars per Gallon",
       title = "ETA Auto Forecast of U.S. On-Highway Diesel Fuel Prices")

#Of the four manually specified ETS models, ETS(M,Ad,A) had the lowest AIC of 439.93
#The automated ets function selected an ETS(M,Ad,N) model with AIC=339.38


## Ensemble Analysis with Weighted Combination
# Weighted combination model using inverse variance (RMSE-based weighting)
ensemble_models2 <- diesel_train %>%
  model(
    reg = TSLM(Price ~ trend() + season()),
    ets = ETS(Price),
    arima = ARIMA(Price),
    combination = combination_model(
      TSLM(Price ~ trend() + season()),
      ETS(Price),
      ARIMA(Price),
      cmbn_args = list(weights = "inv_var"))
  )

# Simulate prediction intervals for the weighted combination model
ensemble_futures2 <- ensemble_models2 %>%
  generate(h = 36, times = 1000) %>%
  as_tibble() %>%
  group_by(Month, .model) %>%
  summarise(
    dist = distributional::dist_sample(list(.sim))  # Compute forecast distributions from sample paths
  ) %>%
  ungroup() %>%
  as_fable(index = Month, key = .model, 
           distribution = dist, response = "Price")

# Plot the forecast simulations for the weighted combination model
ensemble_futures2 %>%
  filter(.model == "combination") %>%
  autoplot(diesel_train) +
  labs(y = "Dollars per Gallon", title = "Simulated Forecast for U.S. On-Highway Diesel Fuel Prices - Weighted Ensemble Model")

# Time plot of residuals
ensemble_models2 %>%
  augment() %>%
  filter(.model == "combination") %>%
  ggplot(aes(x = Month, y = .innov)) +
  geom_line() +
  labs(title = "Residuals from Weighted Ensemble Model",
       y = "Residuals")

# ACF plot of residuals
ensemble_models2 %>%
  augment() %>%
  filter(.model == "combination") %>%
  ACF(.innov) %>%
  autoplot() +
  labs(title = "ACF of Weighted Ensemble Model Residuals")

# Histogram of residuals
ensemble_models2 %>%
  augment() %>%
  filter(.model == "combination") %>%
  ggplot(aes(x = .innov)) +
  geom_histogram(bins = 50) +
  labs(title = "Histogram of Weighted Ensemble Model Residuals",
       x = "Residuals")

# Residuals vs Fitted plot
ensemble_models2 %>%
  augment() %>%
  filter(.model == "combination") %>%
  ggplot(aes(x = .fitted, y = .innov)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Fitted values", y = "Residuals",
       title = "Residuals vs Fitted Values - Weighted Ensemble Model")

# Ljung-Box test for residual autocorrelation
ensemble_models2 %>%
  augment() %>%
  filter(.model == "combination") %>%
  features(.innov, ljung_box, lag = 24, dof = 3)

##CROSS VALIDATION INCLUDING THE ENSEMBLE MODEL

# Cross-validation
diesel_cv <- diesel_train %>%
  stretch_tsibble(.init = 60, .step = 12)

# Fit all models including ensemble
all_models_fit <- diesel_cv %>%
  model(
    arima_manual = ARIMA(Price ~ pdq(1,1,1) + PDQ(0,0,0)),
    arima_auto = ARIMA(Price),
    ts_reg = TSLM(Price ~ trend() + I(trend()^2) + season()),
    ets_manual = ETS(Price ~ error("M") + trend("Ad") + season("A")),
    ets_auto = ETS(Price),
    naive = NAIVE(Price),
    snaive = SNAIVE(Price),
    ensemble = combination_model(
      ARIMA(Price ~ pdq(1,1,1) + PDQ(0,0,0)),
      ARIMA(Price),
      ETS(Price ~ error("M") + trend("Ad") + season("A")),
      ETS(Price),
      TSLM(Price ~ trend() + I(trend()^2) + season())
    )
  )

# Forecast and evaluate accuracy
cv_forecasts <- all_models_fit %>%
  forecast(h = 12)

# Accuracy evaluation
accuracy_results <- cv_forecasts %>%
  accuracy(diesel_train, na.rm = TRUE) %>%
  arrange(MAPE)

print(accuracy_results)



# Modified Final Model Selection - Enhanced Ensemble Model

# Step 1: Select the best models based on accuracy results
best_models <- accuracy_results %>%
  filter(MAPE < 10) %>%  # Adjust the threshold as needed
  select(.model)

# Print the best models
print(best_models)

# Step 2: Refine the Ensemble Model
final_ensemble_model <- diesel_train %>%
  model(
    final_ensemble = combination_model(
      ARIMA(Price ~ pdq(1,1,1) + PDQ(0,0,0)),  # Best ARIMA model
      ETS(Price ~ error("M") + trend("Ad") + season("A")),  # Best ETS model
      TSLM(Price ~ trend() + I(trend()^2) + season()),  # Best regression model
      cmbn_args = list(weights = "inv_var")  # Use inverse variance weighting
    )
  )

# Step 3: Forecast and visualize the final ensemble model
final_forecast <- final_ensemble_model %>%
  forecast(h = 36)

# Plot the forecast
final_forecast %>%
  autoplot(diesel_train) +
  labs(y = "Dollars per Gallon", title = "Final Ensemble Forecast of U.S. On-Highway Diesel Fuel Prices")

# Analyze residuals of the final ensemble model
final_ensemble_model %>%
  augment() %>%
  filter(.model == "final_ensemble") %>%
  ggplot(aes(x = Month, y = .innov)) +
  geom_line() +
  labs(title = "Residuals from Final Ensemble Model", y = "Residuals")

# ACF plot of residuals
final_ensemble_model %>%
  augment() %>%
  filter(.model == "final_ensemble") %>%
  ACF(.innov) %>%
  autoplot() +
  labs(title = "ACF of Final Ensemble Model Residuals")

# Histogram of residuals
final_ensemble_model %>%
  augment() %>%
  filter(.model == "final_ensemble") %>%
  ggplot(aes(x = .innov)) +
  geom_histogram(bins = 50) +
  labs(title = "Histogram of Final Ensemble Model Residuals", x = "Residuals")

# Residuals vs Fitted plot
final_ensemble_model %>%
  augment() %>%
  filter(.model == "final_ensemble") %>%
  ggplot(aes(x = .fitted, y = .innov)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Fitted values", y = "Residuals", title = "Residuals vs Fitted Values - Final Ensemble Model")

# Ljung-Box test for residual autocorrelation
final_ensemble_model %>%
  augment() %>%
  filter(.model == "final_ensemble") %>%
  features(.innov, ljung_box, lag = 24, dof = 3)


# Generate final operational forecast
operational_forecast <- final_ensemble_model  %>%
  forecast(h = 36)

# Plot operational forecast
operational_forecast %>%
  autoplot(diesel_train) +
  labs(y = "Dollars per Gallon",
       title = "Final Operational Forecast - Ensemble Model")

# Final Model Notes:
# - Ensemble model chosen based on comprehensive performance metrics
# - Better RMSE (0.412) compared to individual models
# - Higher ACF1 (0.859) indicating better pattern capture
# - More robust to different market conditions
# - Combines strengths of multiple modeling approaches
# - Provides balanced prediction intervals
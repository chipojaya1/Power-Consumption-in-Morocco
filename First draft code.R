data <- read.csv("C:/Users/xianghan/Downloads/PowerConsumption.csv", header = TRUE)
summary(data)

install.packages('corrplot')
library(corrplot)
cor_data <- data[, c("Temperature", "Humidity", "Wind.Speed", "general.diffuse.flows", "diffuse.flows", "Total")]
cor_matrix <- cor(cor_data, use = "complete.obs")
corrplot(cor_matrix, method = "color")

# Create hourly time series
ts_temp  <- ts(data$Temperature, start = c(1, 0), frequency = 24)
ts.plot(ts_temp, col = "steelblue", ylab = "Temperature (°C)", main = "Hourly Temperature")

ts_total <- ts(data$Total, start = c(1, 0), frequency = 24)  # frequency = 24 hours/day
ts.plot(ts_total, col = "steelblue", ylab = "Total Consumption", main = "Hourly Power Consumption")


# Overlap
ts_total_scaled <- ts(scale(ts_total), start = start(ts_total), frequency = frequency(ts_total))
ts_temp_scaled  <- ts(scale(ts_temp),  start = start(ts_temp),  frequency = frequency(ts_temp))
ts.plot(ts_total_scaled, ts_temp_scaled, col = c("steelblue", "tomato"),
        ylab = "Standardized Values", main = "Standardized Total Consumption and Temperature")
legend("topright", legend = c("Temerature", "Total Comsumption"), col = c("tomato", "steelblue"), lty = 1)


boxplot(ts_total ~ cycle(ts_total),
        xlab = "Hour of Day", ylab = "Total Consumption",
        col = "lightblue", main = "Hourly Seasonal Boxplot")


par(mfrow = c(1, 1))  # two plots side by side
acf(ts_total, lag.max = 200, col = "blue", lwd = 2,
    main = "ACF of Power Consumption")


install.packages("TSA")
library(TSA)
ts_total <- ts(data$Total, frequency = 24)  # hourly data
time_index <- 1:length(ts_total)
detrend <- lm(ts_total ~ time_index)
periodogram(detrend$residuals, col = "blue", main = "Periodogram of Detrended Power Consumption")


time_index <- 1:length(ts_total)
detrend <- lm(ts_total ~ time_index)
prdgrm <- periodogram(detrend$residuals, plot = FALSE)
frequency <- prdgrm$freq
amplitude <- prdgrm$spec
period <- 1 / frequency
all <- cbind(period, frequency, amplitude)
all_sorted <- all[order(-amplitude), ]
round(all_sorted[1:20, ], 3)















#2
# Seasonal dummy
TOTAL = data$Total
HOUR = cycle(ts(TOTAL, frequency = 24))  # extract hour-of-day (1–24)
n_TOTAL = TOTAL[1:7500]
n_HOUR = HOUR[1:7500]
fit = lm(n_TOTAL ~ as.factor(n_HOUR))
summary(fit)
plot.ts(n_TOTAL / 1000, 
        main = "Actual vs Fitted (Seasonal Dummies Only)", 
        ylab = "Consumption (000s)", 
        col = "blue", 
        lwd = 2)

lines(predict(fit) / 1000, col = "red", lwd = 2)
legend("topright", 
       legend = c("Actual", "Fitted"), 
       col = c("blue", "red"), 
       lty = 1, lwd = 2)

# Convert to factor and set level 16 as reference
n_HOUR_factor = relevel(as.factor(n_HOUR), ref = "16")

# Re-fit the model using the re-leveled factor
fit_ref16 = lm(n_TOTAL ~ n_HOUR_factor)
summary(fit_ref16)

# Hold-out sample: observations 7501 to 8736
hold_TOTAL = data$Total[7501:8736]
hold_HOUR = cycle(ts(data$Total[7501:8736], frequency = 24))

# Predict using seasonal dummy-only model
pred = predict(fit, data.frame(n_HOUR = hold_HOUR), interval = "prediction")

plot.ts(hold_TOTAL / 1000, 
        main = "Hold-out: Actual(blue) vs Predicted Power Consumption(red) (Seasonal Only)", 
        ylab = "Consumption (000s)", 
        col = "blue", 
        lwd = 2)

lines(pred[,1] / 1000, col = "red", lwd = 2)

legend("rightdown", 
       legend = c("Actual", "Predicted"), 
       col = c("blue", "red"), 
       lty = 1, lwd = 2)

# MAPE
mape = mean(abs(hold_TOTAL - pred[,1]) / hold_TOTAL)

# RMSE
rmse = sqrt(mean((hold_TOTAL - pred[,1])^2))

# MAE
mae = mean(abs(hold_TOTAL - pred[,1]))

# Print results
print(paste("MAPE:", round(mape, 4)))
print(paste("RMSE:", round(rmse, 2)))
print(paste("MAE:", round(mae, 2)))

fit = lm(n_TOTAL ~ as.factor(n_HOUR))
acf(residuals(fit), lag.max = 8736, col = "red")




















# Seasonal dummy + Time
n_TOTAL = data$Total[1:7500]
n_HOUR = cycle(ts(n_TOTAL, frequency = 24))
TIME = 1:7500  # time index
fit_combined = lm(n_TOTAL ~ TIME + as.factor(n_HOUR))
summary(fit_combined)

# Hold-out indices
test_index = 7501:8736

# Actual hold-out data
hold_TOTAL = data$Total[test_index]
hold_TIME = test_index
hold_HOUR = cycle(ts(data$Total[test_index], frequency = 24))

# Predict from combined model
pred_test = predict(fit_combined, 
                    newdata = data.frame(
                      TIME = hold_TIME,
                      n_HOUR = hold_HOUR
                    ))

# Plot actual vs predicted (hold-out only)
plot.ts(hold_TOTAL / 1000, 
        main = "Hold-Out: Actual(blue) vs Predicted(red) (Trend + Seasonal)", 
        ylab = "Consumption (000s)", 
        col = "blue", 
        lwd = 2)

lines(pred_test / 1000, col = "red", lwd = 2)

legend(legend = c("Actual", "Predicted"), 
       col = c("blue", "red"), 
       lty = 1, lwd = 2)

# MAPE
mape = mean(abs(hold_TOTAL - pred_test) / hold_TOTAL)

# RMSE
rmse = sqrt(mean((hold_TOTAL - pred_test)^2))

# MAE
mae = mean(abs(hold_TOTAL - pred_test))

# Print results
print(paste("MAPE:", round(mape, 4)))
print(paste("RMSE:", round(rmse, 2)))
print(paste("MAE:", round(mae, 2)))

fit_combined = lm(n_TOTAL ~ TIME + as.factor(n_HOUR))
acf(residuals(fit_combined), lag.max = 8736, col = "red", main = "ACF of Residuals (Trend + Seasonal)")





#Cyclical
# Base time and frequency
time = 1:7500
P = 24  # period for daily cycle
freq = 1 / P

# Generate 6 harmonics
sin1 = sin(2 * pi * 1 * freq * time)
cos1 = cos(2 * pi * 1 * freq * time)
sin2 = sin(2 * pi * 2 * freq * time)
cos2 = cos(2 * pi * 2 * freq * time)
sin3 = sin(2 * pi * 3 * freq * time)
cos3 = cos(2 * pi * 3 * freq * time)
sin4 = sin(2 * pi * 4 * freq * time)
cos4 = cos(2 * pi * 4 * freq * time)
sin5 = sin(2 * pi * 5 * freq * time)
cos5 = cos(2 * pi * 5 * freq * time)
sin6 = sin(2 * pi * 6 * freq * time)
cos6 = cos(2 * pi * 6 * freq * time)

n_TOTAL = data$Total[1:7500]

fit_cyc6 = lm(n_TOTAL ~ time +
                sin1 + cos1 +
                sin2 + cos2 +
                sin3 + cos3 +
                sin4 + cos4 +
                sin5 + cos5 +
                sin6 + cos6)

summary(fit_cyc6)

plot.ts(n_TOTAL / 1000, 
        type = "l", 
        col = "blue", 
        ylab = "Consumption (000s)", 
        main = "Actual vs Fitted (Cyclical Trend with 6 Harmonics)",
        lwd = 2)

lines(predict(fit_cyc6) / 1000, col = "red", lwd = 2)

legend("topright", legend = c("Actual", "Fitted"), 
       col = c("blue", "red"), lty = 1, lwd = 2)

acf(residuals(fit_cyc6), lag.max = 8736, col = "red", 
    main = "ACF of Residuals (Cyclical Trend with 6 Harmonics)")

# Step 1: Hold-out time and frequency
time_hold = 7501:8736
freq = 1 / 24  # hourly data with 24-hour cycle

# Step 2: Generate the 6 harmonic terms for hold-out
sin1_hold = sin(2 * pi * 1 * freq * time_hold)
cos1_hold = cos(2 * pi * 1 * freq * time_hold)
sin2_hold = sin(2 * pi * 2 * freq * time_hold)
cos2_hold = cos(2 * pi * 2 * freq * time_hold)
sin3_hold = sin(2 * pi * 3 * freq * time_hold)
cos3_hold = cos(2 * pi * 3 * freq * time_hold)
sin4_hold = sin(2 * pi * 4 * freq * time_hold)
cos4_hold = cos(2 * pi * 4 * freq * time_hold)
sin5_hold = sin(2 * pi * 5 * freq * time_hold)
cos5_hold = cos(2 * pi * 5 * freq * time_hold)
sin6_hold = sin(2 * pi * 6 * freq * time_hold)
cos6_hold = cos(2 * pi * 6 * freq * time_hold)

# Step 3: Predict using the fit_cyc6 model
pred_cyc6 = predict(fit_cyc6, newdata = data.frame(
  time = time_hold,
  sin1 = sin1_hold, cos1 = cos1_hold,
  sin2 = sin2_hold, cos2 = cos2_hold,
  sin3 = sin3_hold, cos3 = cos3_hold,
  sin4 = sin4_hold, cos4 = cos4_hold,
  sin5 = sin5_hold, cos5 = cos5_hold,
  sin6 = sin6_hold, cos6 = cos6_hold
))

plot.ts(hold_TOTAL / 1000,
        main = "Hold-Out: Actual (Blue) vs Predicted (Red) \nCyclical Trend with 6 Harmonics",
        ylab = "Consumption (000s)",
        col = "blue",
        lwd = 2)

lines(pred_cyc6 / 1000, col = "red", lwd = 2)

legend("topright",
       legend = c("Actual", "Predicted"),
       col = c("blue", "red"),
       lty = 1, lwd = 2)


# Accuracy metrics
mape = mean(abs(hold_TOTAL - pred_cyc6) / hold_TOTAL)
rmse = sqrt(mean((hold_TOTAL - pred_cyc6)^2))
mae = mean(abs(hold_TOTAL - pred_cyc6))

# Print results
print(paste("MAPE:", round(mape, 4)))
print(paste("RMSE:", round(rmse, 2)))
print(paste("MAE:", round(mae, 2)))












#exponential smoothing
# Training data
n_TOTAL = data$Total[1:7500]
# Convert to ts object with hourly frequency (daily seasonality)
ts_total = ts(n_TOTAL, frequency = 24)
# Seasonal additive model
fit_hw = HoltWinters(ts_total, seasonal = "additive")
summary(fit_hw)

# Forecast next 1236 hours
install.packages("forecast")
library(forecast)
fc_hw = forecast(fit_hw, h = 1236)

# Plot forecast
plot(fc_hw, main = "Holt-Winters Forecast: Power Consumption")
lines(data$Total[7501:8736], col = "blue")  # overlay actual


# Actual hold-out
hold_TOTAL = data$Total[7501:8736]

# Predicted from exponential smoothing
pred_hw = fc_hw$mean

# Metrics
mape = mean(abs(hold_TOTAL - pred_hw) / hold_TOTAL)
rmse = sqrt(mean((hold_TOTAL - pred_hw)^2))
mae = mean(abs(hold_TOTAL - pred_hw))

print(paste("MAPE:", round(mape, 4)))
print(paste("RMSE:", round(rmse, 2)))
print(paste("MAE:", round(mae, 2)))





library(forecast)

TS_TOTAL = ts(data$Total[1:7500], frequency = 24)  # hourly with daily seasonality
simple_es = ses(TS_TOTAL, h = 1236)
summary(simple_es)

pred_es = fitted(simple_es)

ts.plot(TS_TOTAL, col = "blue", lwd = 2, ylab = "Power Consumption", 
        main = "SES: Actual vs Fitted (Training)")
lines(pred_es, col = "red", lwd = 2)
legend("topright", legend = c("Actual", "Fitted"), col = c("blue", "red"), lty = 1, lwd = 2)

hold_TOTAL = data$Total[7501:8736]
pred_hold = simple_es$mean  # predicted 1236 values

mape = mean(abs(hold_TOTAL - pred_hold) / hold_TOTAL)
rmse = sqrt(mean((hold_TOTAL - pred_hold)^2))
mae = mean(abs(hold_TOTAL - pred_hold))

print(paste("MAPE:", round(mape, 4)))
print(paste("RMSE:", round(rmse, 2)))
print(paste("MAE:", round(mae, 2)))










#3
# Select relevant variables
vars = data[, c("Total", "Temperature", "Humidity", "Wind.Speed", 
                "general.diffuse.flows", "diffuse.flows")]

# Compute correlation matrix
corr_matrix = cor(vars)

# Round to 3 decimal places
round(corr_matrix, 3)

# 1. Total vs Temperature
plot(data$Temperature, data$Total,
     main = "Total vs Temperature",
     xlab = "Temperature (°C)",
     ylab = "Total Consumption",
     col = "blue", pch = 1)

# 2. Total vs Humidity
plot(data$Humidity, data$Total,
     main = "Total vs Humidity",
     xlab = "Humidity (%)",
     ylab = "Total Consumption",
     col = "blue", pch = 1)

# 3. Total vs Wind Speed
plot(data$Wind.Speed, data$Total,
     main = "Total vs Wind Speed",
     xlab = "Wind Speed",
     ylab = "Total Consumption",
     col = "blue", pch = 1)

# Log-transform Wind Speed (add 1 to avoid log(0))
log_wind = log(data$Wind.Speed + 1)

# Plot: Total vs log(Wind Speed)
plot(log_wind, data$Total,
     main = "Total vs log(Wind Speed)",
     xlab = "log(Wind Speed + 1)",
     ylab = "Total Consumption",
     col = "blue", pch = 1)

# 4. Total vs General Diffuse Flows
plot(data$general.diffuse.flows, data$Total,
     main = "Total vs General Diffuse Flows",
     xlab = "General Diffuse Flows",
     ylab = "Total Consumption",
     col = "blue", pch = 1)


# 5. Total vs Diffuse Flows
plot(data$diffuse.flows, data$Total,
     main = "Total vs Diffuse Flows",
     xlab = "Diffuse Flows",
     ylab = "Total Consumption",
     col = "blue", pch = 1)

# Log-transform the variables (add 1 to avoid log(0))
log_general_diffuse = log(data$general.diffuse.flows + 1)
log_diffuse = log(data$diffuse.flows + 1)

par(mfrow = c(1, 1))  # Plot side-by-side

# Plot 1: Total vs log(general.diffuse.flows)
plot(log_general_diffuse, data$Total,
     main = "Total vs log(General Diffuse Flows)",
     xlab = "log(General Diffuse Flows + 1)",
     ylab = "Total Consumption",
     col = "blue", pch = 1)

# Plot 2: Total vs log(diffuse.flows)
plot(log_diffuse, data$Total,
     main = "Total vs log(Diffuse Flows)",
     xlab = "log(Diffuse Flows + 1)",
     ylab = "Total Consumption",
     col = "blue", pch = 1)








# Create training and testing sets
train_total = data$Total[1:7500]
test_total = data$Total[7501:8736]

train_temp = data$Temperature[1:7500]
test_temp = data$Temperature[7501:8736]

train_humidity = data$Humidity[1:7500]
test_humidity = data$Humidity[7501:8736]

# Add more vars if needed (e.g., Wind.Speed, General Diffuse, etc.)
# Fit linear model
fit_model = lm(train_total ~ train_temp + train_humidity)
summary(fit_model)

# In-sample fitted values
fitted_vals = fitted(fit_model)
resid_vals = residuals(fit_model)

# R-squared
summary(fit_model)$r.squared

# MAPE, RMSE, MAE
mape = mean(abs(resid_vals) / train_total)
rmse = sqrt(mean(resid_vals^2))
mae = mean(abs(resid_vals))

cat("In-sample MAPE:", round(mape, 4), "\n")
cat("RMSE:", round(rmse, 2), "\n")
cat("MAE:", round(mae, 2), "\n")


# Predict on hold-out
pred_test = predict(fit_model, newdata = data.frame(
  train_temp = test_temp,
  train_humidity = test_humidity
))

# Compute error metrics
mape_test = mean(abs(test_total - pred_test) / test_total)
rmse_test = sqrt(mean((test_total - pred_test)^2))
mae_test = mean(abs(test_total - pred_test))

cat("Hold-out MAPE:", round(mape_test, 4), "\n")
cat("RMSE:", round(rmse_test, 2), "\n")
cat("MAE:", round(mae_test, 2), "\n")





# Define all 4 independent variables
vars = c("Temperature", "Humidity", "general.diffuse.flows", "diffuse.flows")

# Initialize list to store results
results = data.frame(Model = character(), R2 = numeric(), stringsAsFactors = FALSE)

# Loop through all non-empty combinations of variables
for (k in 1:length(vars)) {
  combos = combn(vars, k, simplify = FALSE)
  
  for (combo in combos) {
    # Build formula string
    formula = as.formula(paste("Total ~", paste(combo, collapse = " + ")))
    
    # Fit model on training data
    fit = lm(formula, data = data[1:7500, ])
    
    # Extract R-squared
    r2 = summary(fit)$r.squared
    
    # Store result
    results = rbind(results, data.frame(Model = paste(combo, collapse = " + "), R2 = round(r2, 4)))
  }
}

# Display all models and R-squared values
results[order(-results$R2), ]











install.packages("Metrics")
# Load required package
library(Metrics)

# Define your dataset
train_data = data[1:7500, ]
test_data = data[7501:8736, ]

# Define independent variables
vars = c("Temperature", "Humidity", "general.diffuse.flows", "diffuse.flows")

# Store results
results = data.frame()

# Loop through all non-empty combinations
for (k in 1:length(vars)) {
  combos = combn(vars, k, simplify = FALSE)
  
  for (combo in combos) {
    predictors = paste(combo, collapse = " + ")
    formula = as.formula(paste("Total ~", predictors))
    
    # Fit model on training data
    fit = lm(formula, data = train_data)
    
    # Training predictions & residuals
    pred_train = predict(fit, train_data)
    resid_train = train_data$Total - pred_train
    
    # R-squared
    r2 = summary(fit)$r.squared
    
    # Training metrics
    mape_train = mean(abs(resid_train) / train_data$Total)
    rmse_train = sqrt(mean(resid_train^2))
    mae_train = mean(abs(resid_train))
    
    # Hold-out predictions
    pred_test = predict(fit, newdata = test_data)
    resid_test = test_data$Total - pred_test
    
    # Hold-out metrics
    mape_test = mean(abs(resid_test) / test_data$Total)
    rmse_test = sqrt(mean(resid_test^2))
    mae_test = mean(abs(resid_test))
    
    # Store results
    results = rbind(results, data.frame(
      Model = predictors,
      R2 = round(r2, 4),
      Train_MAPE = round(mape_train, 4),
      Train_RMSE = round(rmse_train, 2),
      Train_MAE = round(mae_train, 2),
      Test_MAPE = round(mape_test, 4),
      Test_RMSE = round(rmse_test, 2),
      Test_MAE = round(mae_test, 2)
    ))
  }
}

# View sorted by hold-out MAPE
results = results[order(results$Test_MAPE), ]
print(results)

fit_model = lm(Total ~ Temperature + Humidity, data = data)
resid_model = residuals(fit_model)
acf(resid_model, lag.max = 8736, col = "blue", main = "ACF of Residuals (Temperature + Humidity)")


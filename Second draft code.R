data <- read.csv("C:/Users/xianghan/Downloads/PowerConsumption.csv", header = TRUE)
summary(data)

install.packages('corrplot')
library(corrplot)
cor_data <- data[, c("Temperature", "Humidity", "Wind.Speed", "general.diffuse.flows", "diffuse.flows", "Total")]
cor_matrix <- cor(cor_data, use = "complete.obs")
corrplot(cor_matrix, method = "color")

# Create hourly time series
ts_temp  <- ts(data$Temperature, start = c(1, 0), frequency = 24)
ts.plot(ts_temp, col = "red", ylab = "Temperature (°C)", main = "Hourly Temperature")

ts_total <- ts(data$Total, start = c(1, 0), frequency = 24)  # frequency = 24 hours/day
ts.plot(ts_total, col = "blue", ylab = "Total Consumption", main = "Hourly Power Consumption")


# Overlap
ts_total_scaled <- ts(scale(ts_total), start = start(ts_total), frequency = frequency(ts_total))
ts_temp_scaled  <- ts(scale(ts_temp),  start = start(ts_temp),  frequency = frequency(ts_temp))
ts.plot(ts_total_scaled, ts_temp_scaled, col = c("blue", "red"),
        ylab = "Standardized Values", main = "Standardized Total Energy Consumption (blue) and Temperature (red)")


boxplot(ts_total ~ cycle(ts_total),
        xlab = "Hour of Day", ylab = "Total Consumption",
        col = "lightblue", main = "Hourly Seasonal Boxplot")



#Daily box plot
# Convert total consumption to time series with daily frequency
ts_daily = ts(data$Total, frequency = 24)  # still hourly data

# Create a day index (1 to 364)
day_index = rep(1:364, each = 24)

# Create day index: 1 to 364
day_index = rep(1:364, each = 24)

# Daily boxplot without x-axis
boxplot(data$Total ~ day_index,
        xlab = "Day of Year", ylab = "Total Consumption",
        col = "lightblue", main = "Daily Boxplot of Power Consumption",
        xaxt = "n")

# Add biweekly ticks to x-axis (1, 15, 29, ..., up to 363)
biweekly_ticks = seq(1, 364, by = 14)
axis(1, at = biweekly_ticks, labels = biweekly_ticks)



#Week box plot
# Create week index: 1 to 52 (7 days × 24 hours = 168 hours/week)
week_index = rep(1:52, each = 168)

# Make sure it matches
length(week_index)  # Should be 8736

# Weekly boxplot
boxplot(data$Total ~ week_index,
        xlab = "Week of Year", ylab = "Total Consumption",
        col = "lightblue", main = "Weekly Boxplot of Power Consumption")


#Month box plot
# Month days for 2017 (non-leap year)
month_days = c(31,28,31,30,31,30,31,31,30,31,30,30)  # Dec only has 30 days here
month_hours = rep(1:12, times = month_days * 24)

# Monthly boxplot
boxplot(data$Total ~ month_hours,
        xlab = "Month", ylab = "Total Consumption",
        col = "lightblue", main = "Monthly Boxplot of Power Consumption")


#Quarter box plot
# Define number of days in each quarter based on 2017 data ending Dec 30
quarter_days = c(31 + 28 + 31,   # Q1: Jan–Mar
                 30 + 31 + 30,   # Q2: Apr–Jun
                 31 + 31 + 30,   # Q3: Jul–Sep
                 31 + 30 + 30)   # Q4: Oct–Dec (Dec ends at 30)

# Convert days to hours
quarter_hours = rep(1:4, times = quarter_days * 24)

length(quarter_hours)  # Should be 8736
length(data$Total)     # Should also be 8736

# Now plot
boxplot(data$Total ~ quarter_hours,
        xlab = "Quarter", ylab = "Total Consumption",
        col = "lightblue", main = "Quarterly Boxplot of Power Consumption")








par(mfrow = c(1, 1))  # two plots side by side
acf(ts_total, lag.max = 168, col = "blue", lwd = 1,
    main = "ACF of Power Consumption (Daily for a week)")





# Use only training portion: first 7392 hours = 44 weeks
train_hours = data$Total[1:7392]

# Aggregate to daily totals (7392 / 24 = 308 full days)
daily_total_train = rowSums(matrix(train_hours, ncol = 24, byrow = TRUE))
acf(daily_total_train, lag.max = 308, col = "blue", lwd = 1,
    main = "ACF of Daily Power Consumption (Training Sample Only)",
    xlab = "Lag (Days)")


# Aggregate to weekly total (7392 / 168 = 44 weeks)
weekly_total_train = rowSums(matrix(train_hours, ncol = 168, byrow = TRUE))
acf(weekly_total_train, lag.max = 44, col = "blue", lwd = 1,
    main = "ACF of Weekly Power Consumption (Training Sample Only)",
    xlab = "Lag (Weeks)")







par(mfrow = c(1, 2))  # two plots side by side
install.packages("TSA")
library(TSA)
# Define training sample
ts_total_train = ts(data$Total[1:7500], frequency = 24)  # hourly data

# Detrend using training data only
time_index_train = 1:length(ts_total_train)
detrend_train = lm(ts_total_train ~ time_index_train)

# Periodogram of residuals
periodogram(detrend_train$residuals, col = "blue", main = "Periodogram of Detrended Power Consumption (Training Only)")



# Periodogram without plotting
prdgrm = periodogram(detrend_train$residuals, plot = FALSE)

# Extract frequency and compute period
frequency = prdgrm$freq
period = 1 / frequency
amplitude = prdgrm$spec

# Plot periodogram using period as x-axis
plot(period, amplitude, type = "h", col = "blue", lwd = 2,
     xlab = "Period (Total hours)", ylab = "Spectral Density",
     main = "Periodogram of Detrended Power Consumption")



# Plot with custom x-axis ticks
plot(period, amplitude, type = "h", col = "blue", lwd = 2,
     xlab = "Period (24 hours)", ylab = "Spectral Density",
     main = "Zoomed Periodogram of Detrended Power Consumption",
     xlim = c(0, 24), xaxt = "n")  # suppress default x-axis

# Manually add x-axis ticks, including 24
axis(1, at = seq(0, 24, by = 4))  # or use c(0, 6, 12, 18, 24)




# Use only training data
ts_total_train = ts(data$Total[1:7500], frequency = 24)
time_index_train = 1:length(ts_total_train)
detrend_train = lm(ts_total_train ~ time_index_train)

# Compute periodogram
prdgrm = periodogram(detrend_train$residuals, plot = FALSE)
frequency = prdgrm$freq
amplitude = prdgrm$spec
period = 1 / frequency

# Combine and filter
all = cbind(period, frequency, amplitude)
all_filtered = all[period <= 7500, ]

# Sort by amplitude descending
all_sorted = all_filtered[order(-all_filtered[, 3]), ]

# Round period and frequency to 3 decimals
all_sorted[, 1] = round(all_sorted[, 1], 3)  # period
all_sorted[, 2] = round(all_sorted[, 2], 3)  # frequency

# Format amplitude in scientific notation, 3 decimals
amplitude_formatted = formatC(all_sorted[, 3], format = "e", digits = 3)

# Bind final table
final_table = data.frame(
  Period = all_sorted[, 1],
  Frequency = all_sorted[, 2],
  Amplitude = amplitude_formatted
)

# Display top 20
head(final_table, 20)

















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











#Revised model
# Full data
TOTAL = data$Total

# Extract hour-of-day (1–24)
HOUR = cycle(ts(TOTAL, frequency = 24))

# Create time index for trend
TIME = 1:8736

# Extract weekday (1=Monday to 7=Sunday)
WEEKDAY = rep(1:7, length.out = 8736 / 24)
WEEKDAY = rep(WEEKDAY, each = 24)

# TRAINING DATA: First 7500 obs
n_TOTAL = TOTAL[1:7500]
n_HOUR = HOUR[1:7500]
n_TIME = TIME[1:7500]
n_WEEKDAY = WEEKDAY[1:7500]

# HOLD-OUT: obs 7501–8736
hold_TOTAL = TOTAL[7501:8736]
hold_HOUR = HOUR[7501:8736]
hold_TIME = TIME[7501:8736]
hold_WEEKDAY = WEEKDAY[7501:8736]

# Fit the model: hour + trend + weekday
fit_full = lm(n_TOTAL ~ as.factor(n_HOUR) + n_TIME + as.factor(n_WEEKDAY))
summary(fit_full)

# Predict on hold-out set
pred_full = predict(fit_full, newdata = data.frame(
  n_HOUR = hold_HOUR,
  n_TIME = hold_TIME,
  n_WEEKDAY = hold_WEEKDAY
), interval = "prediction")

# Compute max value across both actual and predicted to set y-axis limit
max_y = max(c(hold_TOTAL, pred_full[,1])) / 1000

# Plot: actual vs predicted
plot.ts(hold_TOTAL / 1000, 
        main = "Hold-out: Actual (blue) vs Predicted (red) Power Consumption [Seasonal dummy (hourly and daily) with trend]", 
        ylab = "Consumption (000s)", 
        col = "blue", lwd = 1,
        ylim = c(min(hold_TOTAL, pred_full[,1]) / 1000, max_y))

lines(pred_full[,1] / 1000, col = "red", lwd = 1)



# Performance metrics
mape = mean(abs(hold_TOTAL - pred_full[,1]) / hold_TOTAL)
rmse = sqrt(mean((hold_TOTAL - pred_full[,1])^2))
mae = mean(abs(hold_TOTAL - pred_full[,1]))

cat("MAPE:", round(mape, 4), "\n")
cat("RMSE:", round(rmse, 2), "\n")
cat("MAE:", round(mae, 2), "\n")

# ==== FULL SAMPLE FITTING ====

# Fit the model to all 8736 observations
full_fit = lm(TOTAL ~ as.factor(HOUR) + TIME + as.factor(WEEKDAY))
summary(full_fit)

# Predicted values
full_fitted = fitted(full_fit)

# Plot actual vs fitted on full dataset
max_y_full = max(c(TOTAL, full_fitted)) / 1000

plot.ts(TOTAL / 1000,
        main = "Full Data: Actual (blue) vs Fitted (red) Power Consumption",
        ylab = "Consumption (000s)", 
        col = "blue", lwd = 1,
        ylim = c(min(TOTAL, full_fitted) / 1000, max_y_full))

lines(full_fitted / 1000, col = "red", lwd = 1)
legend("topright", legend = c("Actual", "Fitted"), col = c("blue", "red"), lty = 1, lwd = 1)

# ==== RESIDUAL DIAGNOSTICS ====

# Residuals of full model
resid_full = residuals(full_fit)

# Plot ACF of residuals
acf(resid_full, lag.max = 168, main = "ACF of Residuals (Full Model)", col = "blue", lwd = 1)








#model with 8 hour, 12 hour, 24 hour, 3 months, and trend
# ==== Create full features ====
TOTAL = data$Total
HOUR = cycle(ts(TOTAL, frequency = 24))
TIME = 1:8736
WEEKDAY = rep(1:7, length.out = 8736 / 24)
WEEKDAY = rep(WEEKDAY, each = 24)

# New seasonal dummies
HOUR_GROUP_8 = ceiling(HOUR / 8)       # Values: 1–3
HOUR_GROUP_12 = ceiling(HOUR / 12)     # Values: 1–2
QUARTER = ceiling((1:8736) / 2190)     # Approx 3-month periods (4 total)

# ==== Training: 1–7500 ====
n_TOTAL = TOTAL[1:7500]
n_HOUR = HOUR[1:7500]
n_TIME = TIME[1:7500]
n_WEEKDAY = WEEKDAY[1:7500]
n_GROUP8 = HOUR_GROUP_8[1:7500]
n_GROUP12 = HOUR_GROUP_12[1:7500]
n_QUARTER = QUARTER[1:7500]

# ==== Hold-out: 7501–8736 ====
hold_TOTAL = TOTAL[7501:8736]
hold_HOUR = HOUR[7501:8736]
hold_TIME = TIME[7501:8736]
hold_WEEKDAY = WEEKDAY[7501:8736]
hold_GROUP8 = HOUR_GROUP_8[7501:8736]
hold_GROUP12 = HOUR_GROUP_12[7501:8736]
hold_QUARTER = QUARTER[7501:8736]

# ==== Fit model with full seasonality ====
fit_rich_full = lm(n_TOTAL ~ as.factor(n_HOUR) +
                     as.factor(n_GROUP8) +
                     as.factor(n_GROUP12) +
                     as.factor(n_WEEKDAY) +
                     as.factor(n_QUARTER) +
                     n_TIME)
summary(fit_rich_full)

# ==== Predict on hold-out ====
pred_rich_full = predict(fit_rich_full, newdata = data.frame(
  n_HOUR = hold_HOUR,
  n_GROUP8 = hold_GROUP8,
  n_GROUP12 = hold_GROUP12,
  n_WEEKDAY = hold_WEEKDAY,
  n_QUARTER = hold_QUARTER,
  n_TIME = hold_TIME
))

# ==== Plot hold-out actual vs predicted ====
max_y = max(c(hold_TOTAL, pred_rich_full)) / 1000
plot.ts(hold_TOTAL / 1000, 
        main = "Hold-out: Actual(blue) vs Predicted(red)[hourly, 8 hours, 12 hours, daily, weekly, quarterly] with trend", 
        ylab = "Consumption (000s)", 
        col = "blue", lwd = 1,
        ylim = c(min(hold_TOTAL, pred_rich_full) / 1000, max_y))
lines(pred_rich_full / 1000, col = "red", lwd = 1)


# ==== Performance ====
mape = mean(abs(hold_TOTAL - pred_rich_full) / hold_TOTAL)
rmse = sqrt(mean((hold_TOTAL - pred_rich_full)^2))
mae = mean(abs(hold_TOTAL - pred_rich_full))
cat("MAPE:", round(mape, 4), "\n")
cat("RMSE:", round(rmse, 2), "\n")
cat("MAE:", round(mae, 2), "\n")

# ==== Residual ACF ====
resid_rich_full = residuals(fit_rich_full)
acf(resid_rich_full, lag.max = 168, main = "ACF of Residuals (Full Seasonal Model)", col = "blue", lwd = 1)



















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









# ==== Feature construction ====
TOTAL = data$Total
HOUR = cycle(ts(TOTAL, frequency = 24))
TIME = 1:8736

# Weekday (1 = Monday, ..., 7 = Sunday)
WEEKDAY = rep(1:7, length.out = 8736 / 24)
WEEKDAY = rep(WEEKDAY, each = 24)

# Day index and month (assuming 365 days in order)
DAY_INDEX = rep(1:365, each = 24)[1:8736]
MONTH = ceiling(DAY_INDEX / 30.44)  # Approximate months

# Quarterly index (each ~91.25 days = 2190 hours)
QUARTER = ceiling((1:8736) / 2190)

# ==== Training data (1–7500) ====
n_TOTAL = TOTAL[1:7500]
n_HOUR = HOUR[1:7500]
n_WEEKDAY = WEEKDAY[1:7500]
n_MONTH = MONTH[1:7500]
n_QUARTER = QUARTER[1:7500]
n_TIME = TIME[1:7500]

# ==== Hold-out data (7501–8736) ====
hold_TOTAL = TOTAL[7501:8736]
hold_HOUR = HOUR[7501:8736]
hold_WEEKDAY = WEEKDAY[7501:8736]
hold_MONTH = MONTH[7501:8736]
hold_QUARTER = QUARTER[7501:8736]
hold_TIME = TIME[7501:8736]

# ==== Fit full seasonal model ====
fit_full_seasonal = lm(n_TOTAL ~ as.factor(n_HOUR) + 
                         as.factor(n_WEEKDAY) +
                         as.factor(n_MONTH) + 
                         as.factor(n_QUARTER) +
                         n_TIME)
summary(fit_full_seasonal)

# ==== Predict on hold-out ====
# Match factor levels to training set
hold_HOUR = factor(hold_HOUR, levels = levels(factor(n_HOUR)))
hold_WEEKDAY = factor(hold_WEEKDAY, levels = levels(factor(n_WEEKDAY)))
hold_MONTH = factor(hold_MONTH, levels = levels(factor(n_MONTH)))
hold_QUARTER = factor(hold_QUARTER, levels = levels(factor(n_QUARTER)))

pred_full_seasonal = predict(fit_full_seasonal, newdata = data.frame(
  n_HOUR = hold_HOUR,
  n_WEEKDAY = hold_WEEKDAY,
  n_MONTH = hold_MONTH,
  n_QUARTER = hold_QUARTER,
  n_TIME = hold_TIME
))

# Remove NA predictions
valid_index = !is.na(pred_full_seasonal)

# Then plot only where prediction is available
max_y = max(c(hold_TOTAL[valid_index], pred_full_seasonal[valid_index])) / 1000

plot.ts(hold_TOTAL[valid_index] / 1000, 
        main = "Hold-out: Actual(blue) vs Predicted(red)[hourly+daily+weekly+monthly+quarterly+trend]", 
        ylab = "Consumption (000s)", 
        col = "blue", lwd = 1,
        ylim = c(min(hold_TOTAL[valid_index], pred_full_seasonal[valid_index]) / 1000, max_y))

lines(pred_full_seasonal[valid_index] / 1000, col = "red", lwd = 1)



plot(1:length(pred_full_seasonal[valid_index]), hold_TOTAL[valid_index] / 1000,
     type = "l", col = "blue", lwd = 1,
     ylab = "Consumption (000s)",
     xlab = "Time (Hours)",
     main = "Hold-out: Actual(blue) vs Predicted(red) [hourly+daily+weekly+monthly+quarterly+trend]")

lines(1:length(pred_full_seasonal[valid_index]), pred_full_seasonal[valid_index] / 1000,
      col = "red", lwd = 1)


length(pred_full_seasonal)
sum(!is.na(pred_full_seasonal))


valid_index = !is.na(pred_full_seasonal)

mape = mean(abs(hold_TOTAL[valid_index] - pred_full_seasonal[valid_index]) / hold_TOTAL[valid_index])
rmse = sqrt(mean((hold_TOTAL[valid_index] - pred_full_seasonal[valid_index])^2))
mae = mean(abs(hold_TOTAL[valid_index] - pred_full_seasonal[valid_index]))

cat("MAPE:", round(mape, 4), "\n")
cat("RMSE:", round(rmse, 2), "\n")
cat("MAE:", round(mae, 2), "\n")


# ==== Residual ACF ====
resid_full_seasonal = residuals(fit_full_seasonal)
acf(resid_full_seasonal, lag.max = 168, main = "ACF of Residuals (Full Seasonal Model)", col = "blue", lwd = 1)



















#New Cyclical
ts_train = ts(data$Total[1:7500], frequency = 24)
detrend_train = lm(ts_train ~ time)  # remove linear trend first
resid_train = residuals(detrend_train)

library(TSA)
prdgrm = periodogram(resid_train, plot = FALSE)

# Convert to period and sort by amplitude
period = 1 / prdgrm$freq
amplitude = prdgrm$spec
pgram_df = data.frame(period = period, amplitude = amplitude)
pgram_sorted = pgram_df[order(-pgram_df$amplitude), ]

head(pgram_sorted, 10)

# ==== Step 1: Training features ====
time = 1:7500
P = 24  # daily periodicity
freq = 1 / P

# Generate 3 harmonics
sin1 = sin(2 * pi * 1 * freq * time)
cos1 = cos(2 * pi * 1 * freq * time)
sin2 = sin(2 * pi * 2 * freq * time)
cos2 = cos(2 * pi * 2 * freq * time)
sin3 = sin(2 * pi * 3 * freq * time)
cos3 = cos(2 * pi * 3 * freq * time)

# Training target
n_TOTAL = data$Total[1:7500]

# ==== Step 2: Fit cyclical model with 3 harmonics ====
fit_cyc3 = lm(n_TOTAL ~ time + sin1 + cos1 + sin2 + cos2 + sin3 + cos3)
summary(fit_cyc3)

# ==== Step 3: ACF of residuals ====
acf(residuals(fit_cyc3), lag.max = 168, 
    main = "ACF of Residuals (Cyclical Model, 3 Harmonics)", 
    col = "blue", lwd = 1)

# ==== Step 4: Predict on hold-out set ====
# Hold-out features
time_hold = 7501:8736
sin1_hold = sin(2 * pi * 1 * freq * time_hold)
cos1_hold = cos(2 * pi * 1 * freq * time_hold)
sin2_hold = sin(2 * pi * 2 * freq * time_hold)
cos2_hold = cos(2 * pi * 2 * freq * time_hold)
sin3_hold = sin(2 * pi * 3 * freq * time_hold)
cos3_hold = cos(2 * pi * 3 * freq * time_hold)

# Predict
pred_cyc3 = predict(fit_cyc3, newdata = data.frame(
  time = time_hold,
  sin1 = sin1_hold, cos1 = cos1_hold,
  sin2 = sin2_hold, cos2 = cos2_hold,
  sin3 = sin3_hold, cos3 = cos3_hold
))

# ==== Step 5: Hold-out target ====
hold_TOTAL = data$Total[7501:8736]

# ==== Step 6: Accuracy metrics (valid predictions only) ====
valid_index = !is.na(pred_cyc3)

mape = mean(abs(hold_TOTAL[valid_index] - pred_cyc3[valid_index]) / hold_TOTAL[valid_index])
rmse = sqrt(mean((hold_TOTAL[valid_index] - pred_cyc3[valid_index])^2))
mae = mean(abs(hold_TOTAL[valid_index] - pred_cyc3[valid_index]))

cat("MAPE:", round(mape, 4), "\n")
cat("RMSE:", round(rmse, 2), "\n")
cat("MAE:", round(mae, 2), "\n")

# ==== Step 7: Plot actual vs predicted ====
# Compute a shared y-axis limit based on both actual and predicted
ylim_range = range(c(hold_TOTAL[valid_index], pred_cyc3[valid_index])) / 1000

# ==== Updated Step 7: Plot with full ylim ====
plot(1:length(hold_TOTAL[valid_index]), hold_TOTAL[valid_index]/1000, type = "l",
     col = "blue", lwd = 1, xlab = "Hour (Hold-Out Period)", ylab = "Consumption (000s)",
     main = "Hold-out: Actual (Blue) vs Predicted (Red) with 3 Harmonics",
     ylim = ylim_range)

lines(1:length(pred_cyc3[valid_index]), pred_cyc3[valid_index]/1000, col = "red", lwd = 1)











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

acf(residuals(fit_cyc6), lag.max = 128, col = "red", 
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




install.packages("forecast", repos = "https://cloud.r-project.org", dependencies = TRUE)
library(forecast)

TS_TOTAL = ts(data$Total[1:7500], frequency = 24)  # hourly with daily seasonality
simple_es = ses(TS_TOTAL, h = 1236)
summary(simple_es)

pred_es = fitted(simple_es)

ts.plot(TS_TOTAL, col = "blue", lwd = 1, ylab = "Power Consumption", 
        main = "SES: Actual vs Fitted (Training)")
lines(pred_es, col = "red", lwd = 1)
legend("topright", legend = c("Actual", "Fitted"), col = c("blue", "red"), lty = 1, lwd = 2)

hold_TOTAL = data$Total[7501:8736]
pred_hold = simple_es$mean  # predicted 1236 values

mape = mean(abs(hold_TOTAL - pred_hold) / hold_TOTAL)
rmse = sqrt(mean((hold_TOTAL - pred_hold)^2))
mae = mean(abs(hold_TOTAL - pred_hold))

print(paste("MAPE:", round(mape, 4)))
print(paste("RMSE:", round(rmse, 2)))
print(paste("MAE:", round(mae, 2)))


plot(fc_hw, main = "Holt-Winters Forecast: Power Consumption",
     ylab = "Consumption", xlab = "Hour")
lines(hold_TOTAL, col = "blue", lwd = 1)
legend("topright", legend = c("Forecast", "Actual"), col = c("black", "blue"), lty = 1)





library(forecast)

# Convert training to ts
ts_total = ts(data$Total[1:7500], frequency = 24)

# Fit ETS model (auto-detects trend and seasonality)
fit_ets = ets(ts_total)

# Forecast next 1236 hours
fc_ets = forecast(fit_ets, h = 1236)

# Plot with actual hold-out
plot(fc_ets, main = "ETS Forecast with Trend + Seasonality", ylab = "Consumption", xlab = "Hour")
lines(data$Total[7501:8736], col = "blue", lwd = 1)
legend("topright", legend = c("Forecast", "Actual"), col = c("black", "blue"), lty = 1)

hold_TOTAL = data$Total[7501:8736]
pred_ets = fc_ets$mean

mape = mean(abs(hold_TOTAL - pred_ets) / hold_TOTAL)
rmse = sqrt(mean((hold_TOTAL - pred_ets)^2))
mae = mean(abs(hold_TOTAL - pred_ets))

cat("MAPE:", round(mape, 4), "\n")
cat("RMSE:", round(rmse, 2), "\n")
cat("MAE:", round(mae, 2), "\n")








#3
# Select relevant variables
vars = data[, c("Total", "Temperature", "Humidity", "Wind.Speed", 
                "general.diffuse.flows", "diffuse.flows")]

# Compute correlation matrix
corr_matrix = cor(vars)

# Round to 3 decimal places
round(corr_matrix, 3)

par(mfrow = c(3, 2))  # Plot side-by-side

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
acf(resid_model, lag.max = 168, col = "blue", main = "ACF of Residuals (Temperature + Humidity)")


# ============================================================
# DATA ACQUISITION
# 
# ============================================================

install.packages(c("tidyverse", "lubridate", "forecast", 
                   "tseries", "ggplot2", "flextable", "scales"))

library(tidyverse)
library(lubridate)
library(forecast)
library(tseries)
library(ggplot2)
library(flextable)
library(scales)

# ============================================================
# Load Dataset and Inspect Structure
# ============================================================

# Load the dataset — update the path to where your file is saved
# Import dataset
setwd("C:/Users/user/OneDrive - The University of South Dakota/Documents/Data Mining for Competitive Advantage/Project/Siki")
##this checks the working directory
getwd()

meta_data <- read.csv("marketing_campaign.csv")


# Check dimensions
cat("Rows:", nrow(meta_data), "\n")
cat("Columns:", ncol(meta_data), "\n")

# Check structure
str(meta_data)

# View first 6 rows
head(meta_data)



# ============================================================
#  Clean Data — Parse Dates and Convert String Columns
# ============================================================

# Parse Date column properly
meta_data$Date <- mdy(meta_data$Date)

# Strip $ and spaces from cost columns, convert to numeric
meta_data$Cost.per.Facebook.Ad <- as.numeric(
  gsub("[$, ]", "", meta_data$Cost.per.Facebook.Ad))

meta_data$Facebook.Cost.per.Click..Ad.Cost...Clicks. <- as.numeric(
  gsub("[$, ]", "", meta_data$Facebook.Cost.per.Click..Ad.Cost...Clicks.))

meta_data$Cost.per.AdWords.Ad <- as.numeric(
  gsub("[$, ]", "", meta_data$Cost.per.AdWords.Ad))

meta_data$AdWords.Cost.per.Click..Ad.Cost...Clicks. <- as.numeric(
  gsub("[$, ]", "", meta_data$AdWords.Cost.per.Click..Ad.Cost...Clicks.))

# Strip % and spaces from rate columns, convert to numeric
meta_data$Facebook.Click.Through.Rate..Clicks...View. <- as.numeric(
  gsub("[%, ]", "", meta_data$Facebook.Click.Through.Rate..Clicks...View.))

meta_data$Facebook.Conversion.Rate..Conversions...Clicks. <- as.numeric(
  gsub("[%, ]", "", meta_data$Facebook.Conversion.Rate..Conversions...Clicks.))

meta_data$AdWords.Click.Through.Rate..Clicks...View. <- as.numeric(
  gsub("[%, ]", "", meta_data$AdWords.Click.Through.Rate..Clicks...View.))

meta_data$AdWords.Conversion.Rate..Conversions...Click. <- as.numeric(
  gsub("[%, ]", "", meta_data$AdWords.Conversion.Rate..Conversions...Click.))

# Verify the cleaning worked
str(meta_data)



# ============================================================
#  Missing Values Check + Structure Summary Table
# ============================================================

# Check for missing values across all columns
missing_check <- data.frame(
  Variable = names(meta_data),
  Data_Type = sapply(meta_data, class),
  Missing_Values = colSums(is.na(meta_data))
)

# Reset row names for clean display
rownames(missing_check) <- NULL

# Display as flextable
flextable(missing_check) %>%
  set_caption("Table: Dataset Structure and Missing Values Summary") %>%
  theme_booktabs() %>%
  autofit()




# ============================================================
#  Descriptive Statistics for Facebook Variables Only
# ============================================================


# Step 1: Select only Facebook-related numeric columns
fb_vars <- meta_data %>%
  select(
    Facebook.Ad.Views,
    Facebook.Ad.Clicks,
    Facebook.Ad.Conversions,
    Cost.per.Facebook.Ad,
    Facebook.Click.Through.Rate..Clicks...View.,
    Facebook.Conversion.Rate..Conversions...Clicks.,
    Facebook.Cost.per.Click..Ad.Cost...Clicks.
  )

# Step 2: Build descriptive statistics table
desc_stats <- data.frame(
  Variable = c("Ad Views", "Ad Clicks", "Ad Conversions",
               "Cost per Ad ($)", "Click-Through Rate (%)",
               "Conversion Rate (%)", "Cost per Click ($)"),
  Mean   = round(sapply(fb_vars, mean,   na.rm = TRUE), 2),
  Median = round(sapply(fb_vars, median, na.rm = TRUE), 2),
  SD     = round(sapply(fb_vars, sd,     na.rm = TRUE), 2),
  Min    = round(sapply(fb_vars, min,    na.rm = TRUE), 2),
  Max    = round(sapply(fb_vars, max,    na.rm = TRUE), 2)
)

rownames(desc_stats) <- NULL

# Step 3: Display as flextable
flextable(desc_stats) %>%
  set_caption("Table: Descriptive Statistics — Facebook Ad Campaign Variables") %>%
  theme_booktabs() %>%
  autofit()


############################### DATA EXPLORATION #############################

# ============================================================
#  Convert to Time Series Object
# ============================================================


# Convert Facebook Ad Conversions to a daily time series
# frequency = 7 means weekly seasonality (7 days per week)
conversions_ts <- ts(
  meta_data$Facebook.Ad.Conversions,
  start     = c(2019, 1),
  frequency = 7
)

# Display time series properties as flextable
ts_summary <- data.frame(
  Property            = c("Start Date", "End Date", "Frequency", 
                          "Total Observations", "Min Value", 
                          "Max Value", "Mean Value"),
  Value               = c("2019 - Week 1", "2019 - Week 52", 
                          "7 (Daily with Weekly Seasonality)",
                          as.character(length(conversions_ts)),
                          as.character(min(conversions_ts)),
                          as.character(max(conversions_ts)),
                          as.character(round(mean(conversions_ts), 2)))
)

flextable(ts_summary) %>%
  set_caption("Table: Time Series Object Properties — Facebook Ad Conversions") %>%
  theme_booktabs() %>%
  autofit()


# ============================================================
#  Time Series Plot of Daily Facebook Ad Conversions
# ============================================================


png("Daily_Facebook_Ad_Conversions.png", width = 1400, height = 600, res = 120)

ggplot(meta_data, aes(x = Date, y = Facebook.Ad.Conversions)) +
  geom_line(color = "steelblue", linewidth = 0.7) +
  geom_smooth(method = "loess", color = "red", 
              se = FALSE, linewidth = 1) +
  labs(
    title = "Daily Facebook Ad Conversions — January to December 2019",
    x     = "Date",
    y     = "Daily Conversions"
  ) +
  theme_minimal() +
  theme(
    plot.title  = element_text(face = "bold", size = 13),
    axis.title  = element_text(size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

dev.off()


# ============================================================
#  Seasonal Plot by Day of Week
# ============================================================

# Add day of week to the dataset
meta_data$DayOfWeek <- wday(meta_data$Date, 
                            label = TRUE, 
                            abbr  = FALSE)

# Calculate average conversions by day of week
dow_summary <- meta_data %>%
  group_by(DayOfWeek) %>%
  summarise(
    Mean_Conversions   = round(mean(Facebook.Ad.Conversions), 2),
    Median_Conversions = round(median(Facebook.Ad.Conversions), 2),
    SD_Conversions     = round(sd(Facebook.Ad.Conversions), 2)
  )

# Plot
png("Seasonal_Plot_DayOfWeek.png", width = 1400, height = 600, res = 120)

ggplot(meta_data, aes(x = DayOfWeek, 
                      y = Facebook.Ad.Conversions,
                      group = 1)) +
  stat_summary(fun = mean, geom = "line", 
               color = "steelblue", linewidth = 1) +
  stat_summary(fun = mean, geom = "point", 
               color = "steelblue", size = 3) +
  labs(
    title = "Average Daily Facebook Ad Conversions by Day of Week — 2019",
    x     = "Day of Week",
    y     = "Average Conversions"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    axis.title = element_text(size = 11)
  )

dev.off()

# Print summary table
flextable(dow_summary) %>%
  set_caption("Table: Average Conversions by Day of Week") %>%
  theme_booktabs() %>%
  autofit()   


# ============================================================
# Classical Decomposition
# ============================================================

# Decompose the time series into trend, seasonal, and remainder
decomp <- decompose(conversions_ts, type = "additive")

# Save the decomposition plot
png("Decomposition_Facebook_Conversions.png", width = 1400, height = 900, res = 120)

plot(decomp,
     col  = "steelblue",
     xlab = "Time (Weeks)")

title(main = "Classical Decomposition — Daily Facebook Ad Conversions 2019",
      line = -1,
      cex.main = 1.2)

dev.off()

# Extract and display decomposition summary as flextable
decomp_summary <- data.frame(
  Component = c("Trend", "Seasonal", "Remainder"),
  Min       = c(round(min(decomp$trend,    na.rm = TRUE), 2),
                round(min(decomp$seasonal, na.rm = TRUE), 2),
                round(min(decomp$random,   na.rm = TRUE), 2)),
  Max       = c(round(max(decomp$trend,    na.rm = TRUE), 2),
                round(max(decomp$seasonal, na.rm = TRUE), 2),
                round(max(decomp$random,   na.rm = TRUE), 2)),
  Range     = c(round(max(decomp$trend,    na.rm = TRUE) - 
                        min(decomp$trend,    na.rm = TRUE), 2),
                round(max(decomp$seasonal, na.rm = TRUE) - 
                        min(decomp$seasonal, na.rm = TRUE), 2),
                round(max(decomp$random,   na.rm = TRUE) - 
                        min(decomp$random,   na.rm = TRUE), 2))
)

flextable(decomp_summary) %>%
  set_caption("Table: Decomposition Component Summary — Facebook Ad Conversions") %>%
  theme_booktabs() %>%
  autofit()


# ============================================================
#  Autocorrelation Function (ACF) Plot
# ============================================================

png("ACF_Facebook_Conversions.png", width = 1400, height = 600, res = 120)

acf(conversions_ts,
    lag.max = 30,
    main    = "ACF — Daily Facebook Ad Conversions 2019",
    col     = "steelblue",
    lwd     = 2)

dev.off()

# Extract ACF values into a flextable
acf_values <- acf(conversions_ts, lag.max = 14, plot = FALSE)

acf_table <- data.frame(
  Lag           = as.numeric(acf_values$lag),
  ACF_Value     = round(as.numeric(acf_values$acf), 4)
)

flextable(acf_table) %>%
  set_caption("Table: ACF Values at Key Lags — Facebook Ad Conversions") %>%
  theme_booktabs() %>%
  autofit()


# ============================================================
# ACF Table — Fixed with Integer Lags
# ============================================================

acf_values <- acf(conversions_ts, lag.max = 14, plot = FALSE)

acf_table <- data.frame(
  Lag       = 0:14,
  ACF_Value = round(as.numeric(acf_values$acf), 4)
)

flextable(acf_table) %>%
  set_caption("Table: ACF Values at Key Lags — Daily Facebook Ad Conversions") %>%
  theme_booktabs() %>%
  autofit()




# ============================================================
#  Partial Autocorrelation Function (PACF) Plot
# ============================================================

png("PACF_Facebook_Conversions.png", width = 1400, height = 600, res = 120)

pacf(conversions_ts,
     lag.max = 30,
     main    = "PACF — Daily Facebook Ad Conversions 2019",
     col     = "steelblue",
     lwd     = 2)

dev.off()

# Extract PACF values into a flextable
pacf_values <- pacf(conversions_ts, lag.max = 14, plot = FALSE)

pacf_table <- data.frame(
  Lag        = 1:14,
  PACF_Value = round(as.numeric(pacf_values$acf), 4)
)

flextable(pacf_table) %>%
  set_caption("Table: PACF Values at Key Lags — Daily Facebook Ad Conversions") %>%
  theme_booktabs() %>%
  autofit()




 ###################################### DATA TRANSFORMATION ##########################
# ============================================================
#  Train / Validation Split (80/20)
# ============================================================

# Calculate split point
train_size <- floor(0.8 * length(conversions_ts))
test_size  <- length(conversions_ts) - train_size

# Split the time series
train_ts <- head(conversions_ts, train_size)
test_ts  <- tail(conversions_ts, test_size)

# Split the dataframe as well for TSLM later
train_df <- head(meta_data, train_size)
test_df  <- tail(meta_data, test_size)

# Display split summary as flextable
split_table <- data.frame(
  Dataset        = c("Training Set", "Validation Set", "Total"),
  Start_Date     = c(as.character(min(train_df$Date)),
                     as.character(min(test_df$Date)),
                     as.character(min(meta_data$Date))),
  End_Date       = c(as.character(max(train_df$Date)),
                     as.character(max(test_df$Date)),
                     as.character(max(meta_data$Date))),
  Observations   = c(train_size, test_size, 
                     length(conversions_ts))
)

flextable(split_table) %>%
  set_caption("Table: Training and Validation Split Summary") %>%
  theme_booktabs() %>%
  autofit()


# ============================================================
# Augmented Dickey-Fuller (ADF) Test
# ============================================================

library(tseries)

# Run ADF test on training data
adf_result <- adf.test(train_ts)

# Display results as flextable
adf_table <- data.frame(
  Test              = "Augmented Dickey-Fuller",
  Test_Statistic    = round(adf_result$statistic, 4),
  P_Value           = round(adf_result$p.value, 4),
  Lags_Used         = adf_result$parameter,
  Decision          = ifelse(adf_result$p.value < 0.05,
                             "Reject H0 — Stationary",
                             "Fail to Reject H0 — Non-Stationary")
)

flextable(adf_table) %>%
  set_caption("Table: Augmented Dickey-Fuller Test Results — Facebook Ad Conversions") %>%
  theme_booktabs() %>%
  autofit()





# ============================================================
#  Box-Cox Transformation Assessment
# ============================================================

library(forecast)

# Estimate optimal lambda
lambda <- BoxCox.lambda(train_ts)

cat("Optimal Box-Cox Lambda:", round(lambda, 4), "\n")

# Display result as flextable
boxcox_table <- data.frame(
  Assessment        = "Box-Cox Transformation",
  Optimal_Lambda    = round(lambda, 4),
  Interpretation    = ifelse(abs(lambda - 1) < 0.25,
                             "Lambda close to 1 — No transformation needed",
                             ifelse(abs(lambda) < 0.25,
                                    "Lambda close to 0 — Log transformation suggested",
                                    "Lambda suggests power transformation")),
  Decision          = ifelse(abs(lambda - 1) < 0.25,
                             "Proceed without transformation",
                             ifelse(abs(lambda) < 0.25,
                                    "Consider log transformation",
                                    "Consider power transformation"))
)

flextable(boxcox_table) %>%
  set_caption("Table: Box-Cox Transformation Assessment — Facebook Ad Conversions") %>%
  theme_booktabs() %>%
  autofit()



# ============================================================
# Create Time Index and Lag Variables
# ============================================================

library(dplyr)

# Add time index and lag variable to full dataset
meta_data <- meta_data %>%
  mutate(
    Time_Index = row_number(),
    Month      = month(Date),
    DayOfWeek  = wday(Date, label = TRUE, abbr = FALSE),
    Lag1       = lag(Facebook.Ad.Conversions, 1),
    Lag7       = lag(Facebook.Ad.Conversions, 7)
  )

# Recreate train and test splits with new variables
train_df <- head(meta_data, train_size)
test_df  <- tail(meta_data, test_size)

# Show first 10 rows of key new variables
preview_table <- meta_data %>%
  select(Date, Time_Index, Month, 
         DayOfWeek, Facebook.Ad.Conversions, 
         Lag1, Lag7) %>%
  head(10)

flextable(preview_table) %>%
  set_caption("Table: Sample of Created Predictor Variables — Facebook Ad Conversions") %>%
  theme_booktabs() %>%
  autofit()



# ============================================================
#  Naïve and Seasonal Naïve Models
# ============================================================

library(forecast)

# --- Fit Models ---
naive_model  <- naive(train_ts,  h = test_size)
snaive_model <- snaive(train_ts, h = test_size)

# --- Accuracy ---
accuracy_naive  <- accuracy(naive_model,  test_ts)
accuracy_snaive <- accuracy(snaive_model, test_ts)

# --- Naïve Accuracy Table ---
naive_table <- data.frame(
  Set   = c("Training", "Validation"),
  ME    = round(accuracy_naive[, "ME"],   4),
  RMSE  = round(accuracy_naive[, "RMSE"], 4),
  MAE   = round(accuracy_naive[, "MAE"],  4),
  MAPE  = round(accuracy_naive[, "MAPE"], 4),
  MASE  = round(accuracy_naive[, "MASE"], 4),
  ACF1  = round(accuracy_naive[, "ACF1"], 4)
)

flextable(naive_table) %>%
  set_caption("Table: Naïve Model Accuracy — Training and Validation Sets") %>%
  theme_booktabs() %>%
  autofit()


#########################################################################################################
# --- Seasonal Naïve Accuracy Table ---
snaive_table <- data.frame(
  Set   = c("Training", "Validation"),
  ME    = round(accuracy_snaive[, "ME"],   4),
  RMSE  = round(accuracy_snaive[, "RMSE"], 4),
  MAE   = round(accuracy_snaive[, "MAE"],  4),
  MAPE  = round(accuracy_snaive[, "MAPE"], 4),
  MASE  = round(accuracy_snaive[, "MASE"], 4),
  ACF1  = round(accuracy_snaive[, "ACF1"], 4)
)

flextable(snaive_table) %>%
  set_caption("Table: Seasonal Naïve Model Accuracy — Training and Validation Sets") %>%
  theme_booktabs() %>%
  autofit()


# ============================================================
#  Save Naïve and Seasonal Naïve Plots
# ============================================================

# Naïve plot
png("Naive_Forecast.png", width = 1400, height = 600, res = 120)
autoplot(naive_model) +
  autolayer(test_ts, series = "Actual", color = "red") +
  labs(
    title = "Naïve Model Forecast — Facebook Ad Conversions",
    x     = "Time (Weeks)",
    y     = "Daily Conversions"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 13))
dev.off()


################# saving both the plots ######################
# Seasonal Naïve plot
# ============================================================
# STEP 4A: Fixed Plots with Proper Date Labels
# ============================================================

# Naïve plot
png("Naive_Forecast.png", width = 1400, height = 600, res = 120)
autoplot(naive_model) +
  autolayer(test_ts, series = "Actual", color = "red") +
  scale_x_continuous(
    breaks = seq(2019, 2020, by = 0.25),
    labels = c("Jan 2019", "Apr 2019", 
               "Jul 2019", "Oct 2019", "Jan 2020")
  ) +
  labs(
    title = "Naïve Model Forecast — Facebook Ad Conversions",
    x     = "Date",
    y     = "Daily Conversions"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 13))
dev.off()

####################### Seasonal Naïve plot ####################################
# ============================================================
#  Save Naïve and Seasonal Naïve Plots
# ============================================================

# Naïve plot
png("Naive_Forecast.png", width = 1400, height = 600, res = 120)
autoplot(naive_model) +
  autolayer(test_ts, series = "Actual", color = "red") +
  labs(
    title = "Naïve Model Forecast — Facebook Ad Conversions",
    x     = "Time (Weeks)",
    y     = "Daily Conversions"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 13))
dev.off()

# Seasonal Naïve plot
png("SNaive_Forecast.png", width = 1400, height = 600, res = 120)
autoplot(snaive_model) +
  autolayer(test_ts, series = "Actual", color = "red") +
  labs(
    title = "Seasonal Naïve Model Forecast — Facebook Ad Conversions",
    x     = "Time (Weeks)",
    y     = "Daily Conversions"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 13))
dev.off()






################### ETS MODEL #########################################
# ============================================================
# ETS Model
# ============================================================

library(forecast)

# Fit ETS model on training data
ets_model <- ets(train_ts)

# Check which model was selected
cat("ETS Model Selected:", ets_model$method, "\n")

# Display ETS model summary as flextable
ets_selected <- data.frame(
  Property    = c("Model Selected", "AIC", "BIC", "AICc"),
  Value       = c(ets_model$method,
                  round(ets_model$aic,  3),
                  round(ets_model$bic,  3),
                  round(ets_model$aicc, 3))
)

flextable(ets_selected) %>%
  set_caption("Table: ETS Model Selection Summary") %>%
  theme_booktabs() %>%
  autofit()


# ============================================================
# ETS Forecast and Accuracy
# ============================================================

# Generate forecast on validation period
ets_forecast <- forecast(ets_model, h = test_size)

# Accuracy on training and validation
accuracy_ets <- accuracy(ets_forecast, test_ts)

# Accuracy table
ets_table <- data.frame(
  Set  = c("Training", "Validation"),
  ME   = round(accuracy_ets[, "ME"],   4),
  RMSE = round(accuracy_ets[, "RMSE"], 4),
  MAE  = round(accuracy_ets[, "MAE"],  4),
  MAPE = round(accuracy_ets[, "MAPE"], 4),
  MASE = round(accuracy_ets[, "MASE"], 4),
  ACF1 = round(accuracy_ets[, "ACF1"], 4)
)

flextable(ets_table) %>%
  set_caption("Table: ETS Model Accuracy — Training and Validation Sets") %>%
  theme_booktabs() %>%
  autofit()


# ============================================================
#  Save ETS Plot
# ============================================================

png("ETS_Forecast.png", width = 1400, height = 600, res = 120)
autoplot(ets_forecast) +
  autolayer(test_ts, series = "Actual", color = "red") +
  labs(
    title = "ETS(M,N,N) Model Forecast — Facebook Ad Conversions",
    x     = "Time (Weeks)",
    y     = "Daily Conversions"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 13))
dev.off()





# ============================================================
#  ARIMA Model
# ============================================================

library(forecast)

# Fit ARIMA model on training data
arima_model <- auto.arima(train_ts)

# Display ARIMA model summary as flextable
arima_selected <- data.frame(
  Property = c("Model Selected", "AR Order (p)", 
               "Differencing (d)", "MA Order (q)",
               "AIC", "BIC", "AICc"),
  Value    = c(as.character(arima_model),
               as.character(arimaorder(arima_model)[1]),
               as.character(arimaorder(arima_model)[2]),
               as.character(arimaorder(arima_model)[3]),
               round(arima_model$aic,  3),
               round(arima_model$bic,  3),
               round(arima_model$aicc, 3))
)

flextable(arima_selected) %>%
  set_caption("Table: ARIMA Model Selection Summary") %>%
  theme_booktabs() %>%
  autofit()


# ============================================================
#  ARIMA Forecast and Accuracy
# ============================================================

# Generate forecast on validation period
arima_forecast <- forecast(arima_model, h = test_size)

# Accuracy on training and validation
accuracy_arima <- accuracy(arima_forecast, test_ts)

# Accuracy table
arima_table <- data.frame(
  Set  = c("Training", "Validation"),
  ME   = round(accuracy_arima[, "ME"],   4),
  RMSE = round(accuracy_arima[, "RMSE"], 4),
  MAE  = round(accuracy_arima[, "MAE"],  4),
  MAPE = round(accuracy_arima[, "MAPE"], 4),
  MASE = round(accuracy_arima[, "MASE"], 4),
  ACF1 = round(accuracy_arima[, "ACF1"], 4)
)

flextable(arima_table) %>%
  set_caption("Table: ARIMA Model Accuracy — Training and Validation Sets") %>%
  theme_booktabs() %>%
  autofit()


# ============================================================
#  Save ARIMA Plot
# ============================================================

png("ARIMA_Forecast.png", width = 1400, height = 600, res = 120)
autoplot(arima_forecast) +
  autolayer(test_ts, series = "Actual", color = "red") +
  labs(
    title = "ARIMA(0,1,1)(1,0,1)[7] Model Forecast — Facebook Ad Conversions",
    x     = "Time (Weeks)",
    y     = "Daily Conversions"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 13))
dev.off()



# ============================================================
# TSLM Model
# ============================================================

library(forecast)

# Fit TSLM model with trend and season on training data
tslm_model <- tslm(train_ts ~ trend + season)

# Display TSLM model summary as flextable
tslm_summary <- summary(tslm_model)

tslm_selected <- data.frame(
  Property = c("Model Type", "Predictors", 
               "R-Squared", "Adjusted R-Squared",
               "Residual Std Error", "F-Statistic"),
  Value    = c("Time Series Linear Model",
               "Trend + Weekly Seasonality",
               round(tslm_summary$r.squared,      4),
               round(tslm_summary$adj.r.squared,  4),
               round(tslm_summary$sigma,           4),
               round(tslm_summary$fstatistic[1],  4))
)

flextable(tslm_selected) %>%
  set_caption("Table: TSLM Model Summary") %>%
  theme_booktabs() %>%
  autofit()



# ============================================================
#  TSLM Forecast and Accuracy
# ============================================================

# Generate forecast on validation period
tslm_forecast <- forecast(tslm_model, h = test_size)

# Accuracy on training and validation
accuracy_tslm <- accuracy(tslm_forecast, test_ts)

# Accuracy table
tslm_table <- data.frame(
  Set  = c("Training", "Validation"),
  ME   = round(accuracy_tslm[, "ME"],   4),
  RMSE = round(accuracy_tslm[, "RMSE"], 4),
  MAE  = round(accuracy_tslm[, "MAE"],  4),
  MAPE = round(accuracy_tslm[, "MAPE"], 4),
  MASE = round(accuracy_tslm[, "MASE"], 4),
  ACF1 = round(accuracy_tslm[, "ACF1"], 4)
)

flextable(tslm_table) %>%
  set_caption("Table: TSLM Model Accuracy — Training and Validation Sets") %>%
  theme_booktabs() %>%
  autofit()



# ============================================================
#  Save TSLM Plot
# ============================================================

png("TSLM_Forecast.png", width = 1400, height = 600, res = 120)
autoplot(tslm_forecast) +
  autolayer(test_ts, series = "Actual", color = "red") +
  labs(
    title = "TSLM Model Forecast — Facebook Ad Conversions",
    x     = "Time (Weeks)",
    y     = "Daily Conversions"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 13))
dev.off()





# ============================================================
#  Residual Diagnostics — ARIMA Model
# ============================================================

# Save checkresiduals plot for ARIMA
png("Residuals_ARIMA.png", width = 1400, height = 800, res = 120)
checkresiduals(arima_model)
dev.off()


# ============================================================
#  Ljung-Box White Noise Test
# ============================================================

lb_test <- Box.test(residuals(arima_model), 
                    lag  = 20, 
                    type = "Ljung-Box")

lb_table <- data.frame(
  Test           = "Ljung-Box",
  Lag            = 20,
  Test_Statistic = round(lb_test$statistic, 4),
  P_Value        = round(lb_test$p.value,   4),
  Decision       = ifelse(lb_test$p.value > 0.05,
                          "Fail to Reject H0 — Residuals are White Noise",
                          "Reject H0 — Residuals are NOT White Noise")
)

flextable(lb_table) %>%
  set_caption("Table: Ljung-Box White Noise Test — ARIMA Residuals") %>%
  theme_booktabs() %>%
  autofit()



# ============================================================
#  Master Model Comparison Table
# ============================================================

comparison_table <- data.frame(
  Model = c("Naïve", "Seasonal Naïve", 
            "ETS(M,N,N)", "ARIMA(0,1,1)(1,0,1)[7]", 
            "TSLM"),
  
  Training_RMSE = c(round(accuracy_naive[1,  "RMSE"], 4),
                    round(accuracy_snaive[1, "RMSE"], 4),
                    round(accuracy_ets[1,    "RMSE"], 4),
                    round(accuracy_arima[1,  "RMSE"], 4),
                    round(accuracy_tslm[1,   "RMSE"], 4)),
  
  Validation_RMSE = c(round(accuracy_naive[2,  "RMSE"], 4),
                      round(accuracy_snaive[2, "RMSE"], 4),
                      round(accuracy_ets[2,    "RMSE"], 4),
                      round(accuracy_arima[2,  "RMSE"], 4),
                      round(accuracy_tslm[2,   "RMSE"], 4)),
  
  Training_MASE = c(round(accuracy_naive[1,  "MASE"], 4),
                    round(accuracy_snaive[1, "MASE"], 4),
                    round(accuracy_ets[1,    "MASE"], 4),
                    round(accuracy_arima[1,  "MASE"], 4),
                    round(accuracy_tslm[1,   "MASE"], 4)),
  
  Validation_MASE = c(round(accuracy_naive[2,  "MASE"], 4),
                      round(accuracy_snaive[2, "MASE"], 4),
                      round(accuracy_ets[2,    "MASE"], 4),
                      round(accuracy_arima[2,  "MASE"], 4),
                      round(accuracy_tslm[2,   "MASE"], 4)),
  
  Validation_MAE = c(round(accuracy_naive[2,  "MAE"], 4),
                     round(accuracy_snaive[2, "MAE"], 4),
                     round(accuracy_ets[2,    "MAE"], 4),
                     round(accuracy_arima[2,  "MAE"], 4),
                     round(accuracy_tslm[2,   "MAE"], 4)),
  
  Best_Model = c("✓", "", "", "", "")
)

flextable(comparison_table) %>%
  set_caption("Table: Model Comparison — All Models, Training and Validation Sets") %>%
  set_header_labels(
    Model           = "Model",
    Training_RMSE   = "Training RMSE",
    Validation_RMSE = "Validation RMSE",
    Training_MASE   = "Training MASE",
    Validation_MASE = "Validation MASE",
    Validation_MAE  = "Validation MAE",
    Best_Model      = "Best Model"
  ) %>%
  theme_booktabs() %>%
  autofit()


# ============================================================
# Multi-Color Overlay Chart
# ============================================================

library(ggplot2)

# Extract fitted values from each model
fitted_naive  <- as.numeric(fitted(naive_model))
fitted_ets    <- as.numeric(fitted(ets_model))
fitted_arima  <- as.numeric(fitted(arima_model))
fitted_tslm   <- as.numeric(fitted(tslm_model))

# Create date sequence for training period
train_dates <- meta_data$Date[1:train_size]

# Build combined dataframe
overlay_df <- data.frame(
  Date         = train_dates,
  Actual       = as.numeric(train_ts),
  Naive        = fitted_naive,
  ETS          = fitted_ets,
  ARIMA        = fitted_arima,
  TSLM         = fitted_tslm
)

# Add validation actual values
valid_dates  <- meta_data$Date[(train_size+1):nrow(meta_data)]
valid_actual <- as.numeric(test_ts)

valid_df <- data.frame(
  Date   = valid_dates,
  Actual = valid_actual
)

# Save plot
png("Multicolor_Overlay.png", width = 1600, height = 700, res = 120)

ggplot() +
  # Actual training values
  geom_line(data = overlay_df, 
            aes(x = Date, y = Actual, color = "Actual"),
            linewidth = 0.6) +
  # Fitted values — each model different color
  geom_line(data = overlay_df,
            aes(x = Date, y = Naive, color = "Naïve Fitted"),
            linewidth = 0.5, linetype = "dashed") +
  geom_line(data = overlay_df,
            aes(x = Date, y = ETS, color = "ETS Fitted"),
            linewidth = 0.5, linetype = "dashed") +
  geom_line(data = overlay_df,
            aes(x = Date, y = ARIMA, color = "ARIMA Fitted"),
            linewidth = 0.5, linetype = "dashed") +
  geom_line(data = overlay_df,
            aes(x = Date, y = TSLM, color = "TSLM Fitted"),
            linewidth = 0.5, linetype = "dashed") +
  # Actual validation values
  geom_line(data = valid_df,
            aes(x = Date, y = Actual, color = "Validation Actual"),
            linewidth = 0.8) +
  # Colors for each line
  scale_color_manual(values = c(
    "Actual"           = "black",
    "Naïve Fitted"     = "blue",
    "ETS Fitted"       = "green4",
    "ARIMA Fitted"     = "purple",
    "TSLM Fitted"      = "orange",
    "Validation Actual"= "red"
  )) +
  labs(
    title  = "Multi-Model Fitted Values vs Actual — Facebook Ad Conversions 2019",
    x      = "Date",
    y      = "Daily Conversions",
    color  = "Legend"
  ) +
  theme_minimal() +
  theme(
    plot.title   = element_text(face = "bold", size = 13),
    axis.title   = element_text(size = 11),
    legend.position = "bottom"
  )

dev.off()


# ============================================================
#  365-Day Future Forecast
# ============================================================

library(forecast)
library(flextable)

# Refit naïve model on FULL dataset
full_ts <- ts(meta_data$Facebook.Ad.Conversions,
              start     = c(2019, 1),
              frequency = 7)

# Forecast 365 days ahead
future_forecast <- naive(full_ts, h = 365)

# Create future dates
future_dates <- seq(as.Date("2020-01-01"),
                    by     = "day",
                    length.out = 365)

# Build forecast table
forecast_df <- data.frame(
  Date             = format(future_dates, "%b %d %Y"),
  Predicted_Value  = round(as.numeric(future_forecast$mean), 2),
  Lower_80         = round(as.numeric(future_forecast$lower[,1]), 2),
  Upper_80         = round(as.numeric(future_forecast$upper[,1]), 2),
  Lower_95         = round(as.numeric(future_forecast$lower[,2]), 2),
  Upper_95         = round(as.numeric(future_forecast$upper[,2]), 2)
)

# Show first 30 rows as preview
flextable(head(forecast_df, 30)) %>%
  set_caption("Table: 365-Day Forecast — Daily Facebook Ad Conversions (Jan–Dec 2020)") %>%
  set_header_labels(
    Date            = "Date",
    Predicted_Value = "Predicted Conversions",
    Lower_80        = "Lower 80%",
    Upper_80        = "Upper 80%",
    Lower_95        = "Lower 95%",
    Upper_95        = "Upper 95%"
  ) %>%
  theme_booktabs() %>%
  autofit()



# ============================================================
# Save full 365-day forecast table to Word
# ============================================================

library(officer)

ft_full_forecast <- flextable(forecast_df) %>%
  set_caption("Table: 365-Day Forecast — Daily Facebook Ad Conversions (Jan–Dec 2020)") %>%
  set_header_labels(
    Date            = "Date",
    Predicted_Value = "Predicted Conversions",
    Lower_80        = "Lower 80%",
    Upper_80        = "Upper 80%",
    Lower_95        = "Lower 95%",
    Upper_95        = "Upper 95%"
  ) %>%
  theme_booktabs() %>%
  autofit()

doc <- read_docx()
doc <- body_add_flextable(doc, ft_full_forecast)
print(doc, target = "Forecast_365_Days.docx")



# ============================================================
# Save 365-Day Forecast Plot
# ============================================================

png("Forecast_365_Days.png", width = 1600, height = 600, res = 120)

autoplot(future_forecast) +
  labs(
    title = "365-Day Naïve Forecast — Daily Facebook Ad Conversions (2020)",
    x     = "Date",
    y     = "Daily Conversions"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    axis.title = element_text(size = 11)
  )

dev.off()



# ============================================================
#  Fixed 365-Day Forecast Plot with Proper Dates
# ============================================================

# Create a proper dataframe for plotting with real dates
all_dates    <- seq(as.Date("2019-01-01"), 
                    by = "day", length.out = 365)
future_dates <- seq(as.Date("2020-01-01"), 
                    by = "day", length.out = 365)

# Historical data
hist_df <- data.frame(
  Date  = all_dates,
  Value = as.numeric(full_ts),
  Type  = "Historical"
)

# Forecast data
fore_df <- data.frame(
  Date     = future_dates,
  Value    = as.numeric(future_forecast$mean),
  Lower_80 = as.numeric(future_forecast$lower[,1]),
  Upper_80 = as.numeric(future_forecast$upper[,1]),
  Lower_95 = as.numeric(future_forecast$lower[,2]),
  Upper_95 = as.numeric(future_forecast$upper[,2])
)

png("Forecast_365_Days.png", width = 1600, height = 600, res = 120)

ggplot() +
  # Historical actual values
  geom_line(data = hist_df,
            aes(x = Date, y = Value),
            color = "black", linewidth = 0.6) +
  # 95% prediction interval
  geom_ribbon(data = fore_df,
              aes(x = Date, ymin = Lower_95, ymax = Upper_95),
              fill = "steelblue", alpha = 0.2) +
  # 80% prediction interval
  geom_ribbon(data = fore_df,
              aes(x = Date, ymin = Lower_80, ymax = Upper_80),
              fill = "steelblue", alpha = 0.3) +
  # Forecast line
  geom_line(data = fore_df,
            aes(x = Date, y = Value),
            color = "steelblue", linewidth = 0.8) +
  # Vertical line separating history from forecast
  geom_vline(xintercept = as.Date("2020-01-01"),
             linetype = "dashed", color = "red", linewidth = 0.7) +
  labs(
    title = "365-Day Naïve Forecast — Daily Facebook Ad Conversions (2020)",
    x     = "Date",
    y     = "Daily Conversions"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    axis.title = element_text(size = 11)
  )

dev.off()


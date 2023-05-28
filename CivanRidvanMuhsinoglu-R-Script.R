# Load required libraries
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(forecast)

# Step 1: Data cleaning and manipulation
# Read the Excel file
data<-read_excel("TS-covid-data_Full.xlsx",
                 col_types = c(rep("text", 4),rep("numeric",63)))


# date as date
data$date<-as.Date(data$date)

# Check for missing values
sum(is.na(data))

# coutry choose
Turkey<-subset(data,location=="Turkey")
summary(Turkey$new_tests)


# Impute missing values using mean imputation
imputed_data <- Turkey %>%
  mutate(new_tests = ifelse(is.na(new_tests), mean(new_tests, na.rm = TRUE), new_tests))

summary(imputed_data$new_tests)


# Combine the imputed data sets
combined_data <- bind_rows(imputed_data)

# Create a daily time series
daily_data <- combined_data %>%
  group_by(date) %>%
  summarize(total_tests = sum(new_tests))

# Create weekly time series
weekly_data <- combined_data %>%
  group_by(week = ceiling_date(date, "week")) %>%
  summarize(total_tests = sum(new_tests))

# Create monthly time series
monthly_data <- combined_data %>%
  group_by(month = ceiling_date(date, "month")) %>%
  summarize(total_tests = sum(new_tests))


# Step 2: Preliminary analysis
# Descriptive analysis
summary(data$new_tests)
summary(weekly_data$total_tests)
summary(monthly_data$total_tests)

# Check structure and contents of daily_data$total_tests
str(daily_data$total_tests)
head(daily_data$total_tests)

# Convert daily_data to a time series object
ts_data<-ts(daily_data$total_tests,start=c(2020,1),end = c(2023,6),frequency = 365)


# Decompose the time series
decomposed_data <- decompose(ts_data, type = "multiplicative")

# Plot decomposed components
plot(decomposed_data)


# Step 3: Time series modeling


# SES
ses_model <- ses(ts_data)
summary(ses_model)
plot(ses_model)

# Holt's
holt_model <- holt(ts_data)
summary(holt_model)
plot(holt_model)

# Holt-Winters

library(forecast)
holt_winters_model <-HoltWinters(ts_data, seasonal= "additive")
summary(holt_winters_model)
plot(holt_winters_model)

# Step 4: Candidate models analysis
# SES
ses_residuals <- residuals(ses_model)
plot(ses_residuals)
shapiro.test(ses_residuals)

# Holt's
holt_residuals <- residuals(holt_model)
plot(holt_residuals)
shapiro.test(holt_residuals)

# Holt-Winters
holt_winters_residuals <- residuals(holt_winters_model)
plot(holt_winters_residuals)
shapiro.test(holt_winters_residuals)

# Step 5: Auto.arima model and comparison
# auto.arima
arima_model <- auto.arima(ts_data)
summary(arima_model)
plot(arima_model)

# Step 6: Forecasting
# Classical methods
ses_forecast <- forecast(ses_model, h = 4)
holt_forecast <- forecast(holt_model, h = 4)
holt_winters_forecast <- forecast(holt_winters_model, h = 4)

# Print the forecasts
print(ses_forecast)
print(holt_forecast)
print(holt_winters_forecast)


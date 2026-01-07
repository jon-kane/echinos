source(here::here("_common.R"))
library(tidyquant)

data_series = c(
  "CPIAUCSL",     # Consumer Price Index for All Urban Consumers: All Items in U.S. City Average
  "MSPUS",        # Median Sales Price of Houses Sold for the United States
  "MORTGAGE30US", # 30-Year Fixed Rate Mortgage Average in the United States
  "MEHOINUSA646N" # Median Household Income in the United States
)

tb_n1 = tq_get(data_series[1:3], get = "economic.data", from = as.Date("1900-01-01"), to = Sys.Date())
tb_n2 = tq_get(data_series[4], get = "economic.data", from = as.Date("1900-01-01"), to = Sys.Date())

# the FRED series for MEHOINUSA646N goes back to 1984
# I'll supplement with historical data from the census found below
# https://www2.census.gov/programs-surveys/cps/tables/time-series/historical-income-households/h06ar.xlsx
# data pulled on 2026/01/07

tb_n2.1 = tibble(
  symbol = rep("MEHOINUSA646N", times = 9),
  date = as_date("1975-01-01") + years(0:8),
  price = c(11800, 12690, 13570, 15060, 16460, 17710, 19070, 20170, 20890)
)

tb_n = bind_rows(
  tb_n1,
  tb_n2,
  tb_n2.1
)

# get the max-min date to consider
min_date = tb_n %>% 
  group_by(symbol) %>% 
  summarize(min_date = min(date)) %>% 
  pull(min_date) %>% 
  max()

max_date = tb_n %>% 
  pull(date) %>% 
  max()
# ------------------------------------------------------------------------------
# Model the data

library(timetk)     # For easy time series padding/imputation
library(fable)      # For tidy forecasting
library(tsibble)    # The time-series tibble structure
library(distributional) # To extract SD from forecast distributions

# 2. Define a function to harmonize frequencies
# This function detects the data type and standardizes it to Monthly
harmonizeFrequency = function(data) {
  
  # Group by symbol to handle each series individually
  data_grouped = data %>%
    group_by(symbol) 
  
  # Step A: Summarize to Monthly (Handles Weekly -> Monthly)
  # This downsamples high freq and sets the grid for low freq
  data_monthly = data_grouped %>%
    summarise_by_time(
      .date_var = date,
      .by       = "month",
      price     = mean(price, na.rm = TRUE)
    )
  
  # Step B: Pad and Impute (Handles Quarterly/Annual -> Monthly)
  # This fills in the missing months for Q and A series
  data_harmonized = data_monthly %>%
    pad_by_time(date, .by = "month") %>%
    mutate(price = ts_impute_vec(price, period = 1)) %>%
    ungroup()
  
  return(data_harmonized)
}

# 3. Process the data
# Convert to tsibble (time-series tibble) required for fable
harmonized_data = harmonizeFrequency(tb_n) %>%
  mutate(date = yearmonth(date)) %>%
  as_tsibble(index = date, key = symbol)

# 4. Build Models
# We use an automatic ARIMA model for each series
models = harmonized_data %>%
  model(
    arima = ARIMA(price)
  )

# 5. Forecast
# Forecast 2 years (24 months) ahead
forecasts = models %>%
  forecast(h = "2 years")

# 6. Extract Mean and Prediction SD
# fable returns a distribution; we extract the SD from it
final_predictions = forecasts %>%
  hilo(level = 95) %>% # Optional: Calculate intervals if needed
  mutate(
    pred_mean = .mean,
    pred_sd   = sqrt(variance(price)) # Extract sigma from the distribution
  ) %>%
  select(symbol, date, pred_mean, pred_sd)

# join the final predictions with the harmonized data

modeled_data = harmonized_data %>% 
  as_tibble() %>% 
  mutate(pred_sd = 0) %>% 
  bind_rows(
    final_predictions %>% 
      rename(price = pred_mean)
  ) %>% 
  filter(between(date, min_date, max_date)) %>% 
  mutate(date = as_date(date))

data_list = list(
  tb_n  = tb_n, # national data, raw
  tb_nm = modeled_data # national data, modeled
)

saveRDS(data_list, here::here("posts/COST_OF_HOUSING/national-data.rds"))

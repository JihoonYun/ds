# Required libraries
library(dplyr)

# Load CSV 
df <- read.csv("used_cars_data_cleaned_final_ver.csv")

# Set seed for reproducibility
set.seed(2010)

# Number of rows to sample 
num_rows_to_sample <- 10000

# Randomly sample 100 rows from the scaled_data dataframe
df_sampled <- df[sample(nrow(df), num_rows_to_sample, replace = FALSE), ]

# store CSV 
write.csv(df_sampled, "used_cars_data_cleaned_final_ver_arm_sampled.csv")
df_sampled <- read.csv("used_cars_data_cleaned_final_ver_arm_sampled.csv")
table(df_sampled$year)
# Select the required columns from the dataframe
# df_tx <- df_sampled %>%
#   select(make_name, horsepower, torque_lbft, highway_fuel_economy, engine_cylinders, body_type, fuel_type, mileage, year, has_accidents, wheelbase, price)
df_tx <- df_sampled %>%
  select(make_name, horsepower, torque_lbft, highway_fuel_economy, engine_displacement, engine_cylinders, body_type, mileage, year, wheel_system, wheelbase, price)
write.csv(df_tx, file = "used_cars_data_cleaned_final_ver_arm_sampled2.csv", row.names = FALSE)


# Select columns to discretize
columns_to_discretize <- c( "highway_fuel_economy", "engine_displacement", "engine_cylinders", "horsepower", "torque_lbft", "mileage", "year", "wheelbase", "price")


# Define the interval to be discretized
breaks <- list(
  highway_fuel_economy = quantile(df_tx$highway_fuel_economy, probs = c(0, 0.23, 0.66, 1)),
  engine_displacement = quantile(df_tx$engine_displacement, probs = c(0, 0.25, 0.75, 0.95, 1)),
  engine_cylinders = quantile(df_tx$engine_cylinders, probs = c(0, 0.25, 0.75, 0.95, 1)),
  horsepower = quantile(df_tx$horsepower, probs = c(0, 0.25, 0.5, 0.75, 1)),
  torque_lbft = quantile(df_tx$torque_lbft, probs = c(0, 0.33, 0.66, 1)),
  mileage = quantile(df_tx$mileage, probs = c(0, 0.25, 0.5, 0.75, 1)),
  year = c(1989, 2014, 2018, 2021),  #quantile(df_tx$year, probs = c(0, 0.1, 0.6, 1)),
  wheelbase = quantile(df_tx$wheelbase, probs = c(0, 0.25, 0.5, 0.75, 1)),
  # price = quantile(df_tx$price, probs = c(0, 0.25, 0.5, 0.7, 1))
  price = quantile(df_tx$price, probs = c(0, 0.3, 0.7, 1))
)

# Column discretization and conversion to string labels
label_mappings <- list(
  highway_fuel_economy = c("HW Fuel Economy Poor", "HW Fuel Economy Average", "HW Fuel Economy Good"),
  engine_displacement = c("Low Engine Displacement", "Medium Engine Displacement", "High Engine Displacement", "Very High Engine Displacement"),
  engine_cylinders = c("Low Cylinder", "Medium Cylinder", "High Cylinder", "Very High Cylinder"),
  horsepower = c("Low Horsepower", "Medium Horsepower", "High Horsepower", "Very High Horsepower"),
  torque_lbft = c("Low Torque", "Medium Torque", "High Torque"),
  mileage = c("Low Mileage", "Medium Mileage", "High Mileage", "Very High Mileage"),
  year = c("1995 - 2014", "2015-2017", "2018-2021"),
  wheelbase = c("Short Wheelbase", "Medium Wheelbase", "Long Wheelbase", "Very Long Wheelbase"),
  price = c("Low Price", "Medium Price", "High Price")
  # price = c("Low Price", "Medium Price", "High Price", "Very High Price")
)  

for (col in columns_to_discretize) {
  print(col)
  df_tx[[col]] <- cut(df_tx[[col]], breaks = breaks[[col]], labels = label_mappings[[col]])
}

# Change the has_accidents column to factor
# df_tx$has_accidents <- factor(df_tx$has_accidents, levels = c(FALSE, TRUE), labels = c("No Accident", "Accident"))

# Transform the data into transaction format
df_transactions <- df_tx %>%
  select(make_name, horsepower, torque_lbft, highway_fuel_economy, engine_displacement, engine_cylinders, body_type, mileage, year, wheel_system, wheelbase, price)

# Save the data
write.csv(df_transactions, file = "used_cars_data_cleaned_final_ver_arm_sampled_transactions.csv", row.names = FALSE)

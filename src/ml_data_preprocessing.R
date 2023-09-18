# Set the CSV file path
csv_file_path <- "used_cars_data.csv"

# Read the CSV file
df <- read.csv(csv_file_path)

# Print the data frame
str(df)


################################################################################
# Cleaning
################################################################################
# 1. Duplicated values
# Checking for duplicate rows
duplicate_rows <- sum(duplicated(df))

cat("The number of duplicate rows are", duplicate_rows, ".\n")

# Making a copy with the duplicated rows dropped
df_cleaned <- df[!duplicated(df), ]

dim(df_cleaned)
dim(df)


# 2. Missing Values
# Load the necessary library
library(dplyr)

# Identify character-type columns
char_columns <- df_cleaned %>%
  select_if(is.character)

# Create a new dataframe with empty strings replaced by missing values
df_cleaned01 <- df_cleaned

# Replace empty strings ("") with NA in the entire dataframe
df_cleaned01[df_cleaned01 == ""] <- NA


# Load the necessary libraries
library(ggplot2)

# Calculate the missing percentage for each column in the dataframe
missing_percent <- colMeans(is.na(df_cleaned01)) * 100

# Create a dataframe
missing_data <- data.frame(Column = names(df_cleaned01), MissingPercent = missing_percent)

# Highlight columns with 50% or more missing data in red
missing_data$Color <- ifelse(missing_data$MissingPercent > 55, "High Rate", "Low Rate")

# Visualization: Horizontal bar chart
ggplot(data = missing_data, aes(x = reorder(Column, -MissingPercent), y = MissingPercent, fill = Color)) +
  geom_bar(stat = "identity") +
  labs(title = "Missing Data Percentage", x = "Columns", y = "Missing Data Percentage (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = c("Low Rate" = "gray", "High Rate" = "darkred"))




# Calculate the number of missing values for each column in the dataframe
missing_count <- df_cleaned01 %>%
  summarise_all(~ sum(is.na(.)))

# Calculate the percentage of missing values for each column
missing_percent <- missing_count / nrow(df_cleaned01) * 100

# Find columns with more than 50% missing values
columns_with_high_missing <- names(missing_percent)[missing_percent > 55]

# Create a new dataframe with columns removed
df_cleaned02 <- df_cleaned01 %>%
  select(-one_of(columns_with_high_missing))

# Print the columns removed
cat("Columns removed due to more than 50% missing values:\n")
cat(columns_with_high_missing, sep = ", ")

# Print the structure of the new dataframe
str(df_cleaned02)



# Remove all rows containing NA values from the dataframe
df_cleaned03 <- na.omit(df_cleaned02)

# Delete "main_picture_url" column
df_cleaned03 <- df_cleaned03 %>%
  select(-main_picture_url)

# Delete "description" column
df_cleaned03 <- df_cleaned03 %>%
  select(-description)

# Print the cleaned dataframe
print(df_cleaned03)


# Calculate the number of missing values for each column
missing_count <- df_cleaned03 %>%
  summarise_all(~ sum(is.na(.)))

# Print the result
print(missing_count)


head(df_cleaned03)
write.csv(df_cleaned03, file = "used_cars_data_cleaned01.csv", row.names = TRUE)

df_cleaned03 <- read.csv('used_cars_data_cleaned01.csv')


unique_values <- df_cleaned03 %>%
  summarise(
    fleet = paste(unique(fleet), collapse = ", "),
    frame_damaged = paste(unique(frame_damaged), collapse = ", "),
    franchise_dealer = paste(unique(franchise_dealer), collapse = ", "),
    has_accidents = paste(unique(has_accidents), collapse = ", "),
    isCab = paste(unique(isCab), collapse = ", "),
    is_new = paste(unique(is_new), collapse = ", "),
    salvage = paste(unique(salvage), collapse = ", "),
    theft_title = paste(unique(theft_title), collapse = ", ")
  )

print(unique_values)


# Convert a column consisting of “True” and “False” values to character type to Boolean type.


df_cleaned04 <- df_cleaned03 
str(df_cleaned04)

# back_legroom
# Remove "in" and convert to numeric
df_cleaned04$back_legroom <- sub(" in", "", df_cleaned04$back_legroom)

# If there is "--" among the column values, replace it with null and fill it with the average value.
df_cleaned04$back_legroom[df_cleaned04$back_legroom == "--"] <- NA
df_cleaned04$back_legroom <- as.numeric(df_cleaned04$back_legroom)
df_cleaned04$back_legroom[is.na(df_cleaned04$back_legroom)] <- mean(df_cleaned04$back_legroom, na.rm = TRUE)

# engine_cylinders
# Convert to the number of vehicle cylinders.
unique(df_cleaned04$engine_cylinders )
# Split the engine_cylinders column by space and extract the first element
df_cleaned04$engine_cylinders <- sapply(strsplit(df_cleaned04$engine_cylinders, " "), function(x) as.numeric(gsub("[^0-9]", "", x[1])))
df_cleaned04$engine_cylinders <- as.numeric(df_cleaned04$engine_cylinders)

# front_legroom
# Remove "in" and convert to numeric
df_cleaned04$front_legroom <- sub(" in", "", df_cleaned04$front_legroom)

# If there is "--" among the column values, replace it with null and fill it with the average value.
df_cleaned04$front_legroom[df_cleaned04$front_legroom == "--"] <- NA
df_cleaned04$front_legroom <- as.numeric(df_cleaned04$front_legroom)
df_cleaned04$front_legroom[is.na(df_cleaned04$front_legroom)] <- mean(df_cleaned04$front_legroom, na.rm = TRUE)

# fuel_tank_volume
# Remove "gal" and convert to numeric
df_cleaned04$fuel_tank_volume <- sub(" gal", "", df_cleaned04$fuel_tank_volume)
df_cleaned04$fuel_tank_volume[df_cleaned04$fuel_tank_volume == "--"] <- NA
df_cleaned04$fuel_tank_volume <- as.numeric(df_cleaned04$fuel_tank_volume)
df_cleaned04$fuel_tank_volume[is.na(df_cleaned04$fuel_tank_volume)] <- mean(df_cleaned04$fuel_tank_volume, na.rm = TRUE)

# engine_type
# engine_type is replaced by engine_cylinders and fuel_type. The engine_type column is deleted.
df_cleaned04 <- df_cleaned04 %>% select(-engine_type)

# fuel_type
# Convert fuel_type to a factor (categorical variable)
df_cleaned04$fuel_type <- as.factor(df_cleaned04$fuel_type)

# Convert character columns to boolean type
df_cleaned04$fleet <- as.logical(tolower(df_cleaned04$fleet))
df_cleaned04$frame_damaged <- as.logical(tolower(df_cleaned04$frame_damaged))
df_cleaned04$franchise_dealer <- as.logical(tolower(df_cleaned04$franchise_dealer))
df_cleaned04$has_accidents <- as.logical(tolower(df_cleaned04$has_accidents))
df_cleaned04$isCab <- as.logical(tolower(df_cleaned04$isCab))
df_cleaned04$is_new <- as.logical(tolower(df_cleaned04$is_new))
df_cleaned04$salvage <- as.logical(tolower(df_cleaned04$salvage)) 
df_cleaned04$theft_title <- as.logical(tolower(df_cleaned04$theft_title))

str(df_cleaned04)

# height
# Remove "in" and convert to numeric
df_cleaned04$height <- sub(" in", "", df_cleaned04$height)
df_cleaned04$height[df_cleaned04$height == "--"] <- NA
df_cleaned04$height <- as.numeric(df_cleaned04$height)
df_cleaned04$height[is.na(df_cleaned04$height)] <- mean(df_cleaned04$height, na.rm = TRUE)


# listed_date
# Convert listed_date to Date data type
df_cleaned04$listed_date <- as.Date(df_cleaned04$listed_date)


# major_options
# Delete major_options
# df_cleaned04 <- df_cleaned04 %>% select(-major_options)


# maximum_seating
# Remove "in" and convert to numeric
df_cleaned04$maximum_seating <- sub(" seats", "", df_cleaned04$maximum_seating)
df_cleaned04$maximum_seating[df_cleaned04$maximum_seating == "--"] <- NA
df_cleaned04$maximum_seating <- as.numeric(df_cleaned04$maximum_seating)
df_cleaned04$maximum_seating[is.na(df_cleaned04$maximum_seating)] <- mean(df_cleaned04$maximum_seating, na.rm = TRUE)


# power
# In power, horsepower already has a column, so only the rpm value is used here. 
# Create a new rpm column and delete the power field.
df_cleaned04$rpm <- sub(".* @ ([0-9,]+) RPM", "\\1", df_cleaned04$power)
df_cleaned04$rpm <- as.numeric(gsub(",", "", df_cleaned04$rpm))
df_cleaned04 <- df_cleaned04 %>% select(-power)



# length
# Remove "in" and convert to numeric
df_cleaned04$length <- sub(" in", "", df_cleaned04$length)
df_cleaned04$length[df_cleaned04$length == "--"] <- NA
df_cleaned04$length <- as.numeric(df_cleaned04$length)
df_cleaned04$length[is.na(df_cleaned04$length)] <- mean(df_cleaned04$length, na.rm = TRUE)

# torque_lbft
df_cleaned04$torque_lbft <- as.numeric(sub(" lb-ft .*", "", df_cleaned04$torque))
df_cleaned04 <- df_cleaned04 %>% select(-torque)

unique(df_cleaned04$wheel_system)
# transmission
# Convert transmission to a factor (categorical variable)
df_cleaned04$transmission <- as.factor(df_cleaned04$transmission)


# transmission_display
# Transmission_display is replaced by transmission, so delete transmission_display
df_cleaned04 <- df_cleaned04 %>% select(-transmission_display)


# trimId 
# Deleted
df_cleaned04 <- df_cleaned04 %>% select(-trimId)


# wheel_system
# Convert wheel_system to a factor (categorical variable)
df_cleaned04$wheel_system <- as.factor(df_cleaned04$wheel_system)


# wheel_system_display
# wheel_system_display is replaced by wheel_system, so delete wheel_system_display
df_cleaned04 <- df_cleaned04 %>% select(-wheel_system_display)




# wheelbase
# Remove "in" and convert to numeric
df_cleaned04$wheelbase <- sub(" in", "", df_cleaned04$wheelbase)
df_cleaned04$wheelbase[df_cleaned04$wheelbase == "--"] <- NA
df_cleaned04$wheelbase <- as.numeric(df_cleaned04$wheelbase)
df_cleaned04$wheelbase[is.na(df_cleaned04$wheelbase)] <- mean(df_cleaned04$wheelbase, na.rm = TRUE)



# width
# Remove "in" and convert to numeric
df_cleaned04$width <- sub(" in", "", df_cleaned04$width)
df_cleaned04$width[df_cleaned04$width == "--"] <- NA
df_cleaned04$width <- as.numeric(df_cleaned04$width)
df_cleaned04$width[is.na(df_cleaned04$width)] <- mean(df_cleaned04$width, na.rm = TRUE)

# body_type
# Convert body_type to a factor (categorical variable)
df_cleaned04$body_type <- as.factor(df_cleaned04$body_type)


# franchise_make
# Convert franchise_make to a factor (categorical variable)
df_cleaned04$franchise_make <- as.factor(df_cleaned04$franchise_make)

# make_name
# Convert franchise_make to a factor (categorical variable)
df_cleaned04$make_name <- as.factor(df_cleaned04$make_name)

# Extract the first 5 characters of 'dealer_zip' and overwrite the 'dealer_zip' column
df_cleaned04$dealer_zip <- substring(df_cleaned04$dealer_zip, 1, 5)


# is_luxury
# Define a vector of luxury maker names
luxury_maker <- c("Ferrari", "Rolls-Royce", "Lamborghini", "McLaren", "Bentley", "Aston Martin", "SRT", "Porsche")

# Create a new column 'is_luxury_maker' based on the condition
df_cleaned04$is_luxury <- df_cleaned04$make_name %in% luxury_maker

# Convert the 'is_luxury_maker' column to boolean (logical) type
df_cleaned04$is_luxury <- as.logical(df_cleaned04$is_luxury)


# Remove Outlier

# Create a box plot of 'price' by 'make_name' with log-transformed y-axis
ggplot(df_cleaned04, aes(x = make_name, y = price)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(x = "Make Name", y = "Log(Price)", title = "Price Distribution by Make Name (Log Scale)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_log10()


# Create a new data frame to store the filtered data
df_cleaned_without_outliers <- data.frame()

# Define a function to filter outliers by IQR for each 'make_name'
filter_outliers_by_make_name <- function(df) {
  df %>%
    group_by(make_name) %>%
    filter(between(price, quantile(price, 0.25) - 1.5 * IQR(price), quantile(price, 0.75) + 1.5 * IQR(price))) %>%
    ungroup()
}

# Apply the function to filter outliers by 'make_name' and append to the new data frame
l_cleaned_without_outliers <- df_cleaned04 %>%
  group_split(make_name) %>%
  lapply(filter_outliers_by_make_name) %>%
  bind_rows()

df_cleaned_without_outliers <- data.frame(l_cleaned_without_outliers)
class(df_cleaned_without_outliers)

# Create a box plot of 'price' by 'make_name' with log-transformed y-axis
ggplot(df_cleaned_without_outliers, aes(x = make_name, y = price)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(x = "Make Name", y = "Log(Price)", title = "Price Distribution by Make Name") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 


# Zipcode and States
# https://postalpro.usps.com/ZIP_Locale_Detail

# Read the CSV file
zip_csv <- 'ZIP_Locale_Detail.csv'
df_zipcode<- read.csv(zip_csv)


# Select and extract unique values from specific 2 columns in the data frame
df_zipcode <- df_zipcode %>%
  distinct(DELIVERY.ZIPCODE, PHYSICAL.STATE)

# Check for duplicate rows in df_zipcode
duplicate_rows <- df_zipcode[duplicated(df_zipcode), ]

# Display the duplicate rows
if (nrow(duplicate_rows) > 0) {
  cat("Duplicate rows found:\n")
  print(duplicate_rows)
} else {
  cat("No duplicate rows found.\n")
}


# Convert the 'PHYSICAL.ZIP' column to character type
df_zipcode$DELIVERY.ZIPCODE <- as.character(df_zipcode$DELIVERY.ZIPCODE)


# Pad the 'PHYSICAL.ZIP' column with leading zeros to make it 5 characters long
df_zipcode$DELIVERY.ZIPCODE <- sprintf("%05s", df_zipcode$DELIVERY.ZIPCODE)

# Remove rows where 'DELIVERY.ZIPCODE' is "02914" and 'PHYSICAL.STATE' is "MA"
df_zipcode <- df_zipcode %>%
  filter(!(DELIVERY.ZIPCODE == "02914" & PHYSICAL.STATE == "MA"))



# https://wisevoter.com/state-rankings/snowiest-states/

# Add state info
df_cleaned05 <- df_cleaned_without_outliers %>%
  left_join(df_zipcode, by = c("dealer_zip" = "DELIVERY.ZIPCODE"))

colnames(df_cleaned05)[colnames(df_cleaned05) == "PHYSICAL.STATE"] <- "state"


# Check the number of missing values in state
sum(is.na(df_cleaned05$state))


write.csv(df_cleaned05, file = "used_cars_data_cleaned_final_ver.csv", row.names = TRUE)
str(df_cleaned05)



################################################################################
# <EDA>
################################################################################
df_eda <- df_cleaned05
str(df_eda)


# Price
# Load the ggplot2 library
library(ggplot2)


# Calculate median and mean of 'price' column
median_price <- median(df_eda$price)
mean_price <- mean(df_eda$price)
# Create a histogram of 'price' with median and mean annotations
ggplot(df_eda, aes(x = price)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 300) +
  geom_vline(xintercept = median_price, color = "red", linetype = "solid", size = 1) +  # Vertical line for median
  geom_vline(xintercept = mean_price, color = "blue", linetype = "solid", size = 1) +  # Vertical line for mean
  annotate("text", x = median_price, y = 25000, label = paste("Median:", round(median_price, 2)), color = "red", size = 4, hjust = 0) +
  annotate("text", x = mean_price, y = 35000, label = paste("Mean:", round(mean_price, 2)), color = "blue", size = 4, hjust = 0) +
  labs(x = "Price", y = "Frequency", title = "Histogram of Price") +
  theme_minimal()

# Filter the data where 'is_luxury' is FALSE
df_filtered <- df_eda[df_eda$is_luxury == FALSE,]
df_filtered2 <- df_eda[df_eda$is_luxury == TRUE,]

# Calculate median and mean of 'price' column
median_price <- median(df_filtered$price)
mean_price <- mean(df_filtered$price)

# Create a histogram of 'price' with median and mean annotations
ggplot(df_filtered, aes(x = price)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 100) +
  geom_vline(xintercept = median_price, color = "red", linetype = "solid", size = 1) +  # Vertical line for median
  geom_vline(xintercept = mean_price, color = "blue", linetype = "solid", size = 1) +  # Vertical line for mean
  annotate("text", x = median_price, y = 25000, label = paste("Median:", round(median_price, 2)), color = "red", size = 4, hjust = 0) +
  annotate("text", x = mean_price, y = 35000, label = paste("Mean:", round(mean_price, 2)), color = "blue", size = 4, hjust = 0) +
  labs(x = "Price", y = "Frequency", title = "Histogram of Price (is_luxury = FALSE)") +
  theme_minimal()



# Calculate median and mean of 'price' column
median_price <- median(df_filtered2$price)
mean_price <- mean(df_filtered2$price)

# Create a histogram of 'price' with median and mean annotations
ggplot(df_filtered2, aes(x = price)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 100) +
  geom_vline(xintercept = median_price, color = "red", linetype = "solid", size = 1) +  # Vertical line for median
  geom_vline(xintercept = mean_price, color = "blue", linetype = "solid", size = 1) +  # Vertical line for mean
  annotate("text", x = median_price, y = 150, label = paste("Median:", round(median_price, 2)), color = "red", size = 4, hjust = 0) +
  annotate("text", x = mean_price, y = 100, label = paste("Mean:", round(mean_price, 2)), color = "blue", size = 4, hjust = 0) +
  labs(x = "Price", y = "Frequency", title = "Histogram of Price (is_luxury = TRUE)") +
  theme_minimal()



# make name

# Group the data by 'make_name' and calculate the number of rows in each group
make_count <- df_eda %>%
  group_by(make_name) %>%
  summarise(row_count = n())

# Create a bar plot to visualize the number of rows for each 'make_name'
ggplot(make_count, aes(x = reorder(make_name, -row_count), y = row_count)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(x = "Make Name", y = "Count", title = "Count by Make Name") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for better readability


# make name
# Group the data by 'make_name' and calculate the number of rows in each group
make_count <- df_eda %>%
  group_by(make_name) %>%
  summarise(row_count = n())

# Create a bar plot to visualize the number of rows for each 'make_name'
ggplot(make_count, aes(x = reorder(make_name, -row_count), y = row_count)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(x = "Make Name", y = "Count", title = "Count by Make Name") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for better readability


#mileage

# Calculate median and mean of 'mileage' column
median_mileage <- median(df_eda$mileage)
mean_mileage <- mean(df_eda$mileage)

# Create a histogram of 'mileage' with median and mean annotations
ggplot(df_eda, aes(x = mileage)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 100) +
  geom_vline(xintercept = median_mileage, color = "red", linetype = "solid", size = 1) +  # Vertical line for median
  geom_vline(xintercept = mean_mileage, color = "blue", linetype = "solid", size = 1) +  # Vertical line for mean
  annotate("text", x = median_mileage, y = 25000, label = paste("Median:", round(median_mileage, 2)), color = "red", size = 4, hjust = 0) +
  annotate("text", x = mean_mileage, y = 35000, label = paste("Mean:", round(mean_mileage, 2)), color = "blue", size = 4, hjust = 0) +
  labs(x = "Price", y = "Frequency", title = "Histogram of Mileage") +
  theme_minimal()



# Create a bar plot of vehicle counts by 'transmission'
ggplot(df_eda, aes(x = transmission)) +
  geom_bar(fill = "lightblue") +
  labs(
    x = "Transmission",
    y = "Vehicle Count",
    title = "Vehicle Count by Transmission"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Create a bar plot of vehicle counts by 'fuel_type'
ggplot(df_eda, aes(x = fuel_type)) +
  geom_bar(fill = "lightblue") +
  labs(
    x = "Fuel Type",
    y = "Vehicle Count",
    title = "Vehicle Count by Fuel Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Load the ggplot2 library
library(ggplot2)

# Create a bar plot of vehicle counts by 'wheel_system'
ggplot(df_eda, aes(x = wheel_system)) +
  geom_bar(fill = "lightblue") +
  labs(
    x = "Wheel System",
    y = "Vehicle Count",
    title = "Vehicle Count by Wheel System"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Load the ggplot2 library
library(ggplot2)

# Create a bar plot of vehicle counts by 'body_type'
ggplot(df_eda, aes(x = body_type)) +
  geom_bar(fill = "lightblue") +
  labs(
    x = "Body Type",
    y = "Vehicle Count",
    title = "Vehicle Count by Body Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Load the required libraries
library(ggplot2)
library(gridExtra)

# Create individual bar plots
plot1 <- ggplot(df_eda, aes(x = transmission)) +
  geom_bar(fill = "lightblue", color='black') +
  labs(x = "Transmission", y = "Vehicle Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
  coord_flip()

plot2 <- ggplot(df_eda, aes(x = fuel_type)) +
  geom_bar(fill = "lightblue", color='black') +
  labs(x = "Fuel Type", y = "Vehicle Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
  coord_flip()

plot3 <- ggplot(df_eda, aes(x = wheel_system)) +
  geom_bar(fill = "lightblue", color='black') +
  labs(x = "Wheel System", y = "Vehicle Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
  coord_flip()

plot4 <- ggplot(df_eda, aes(x = body_type)) +
  geom_bar(fill = "lightblue", color='black') +
  labs(x = "Body Type", y = "Vehicle Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
  coord_flip()

# Combine the plots into a grid
grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)




library(maps)

# Calculate the frequencies of each state in the dataframe
state_counts <- table(df_eda$state)

# Load the U.S. map data
us_map <- map_data("state")

# Merge the U.S. map data with the frequency information
state_abbreviations <- c(
  "alabama" = "AL",
  "alaska" = "AK",
  "arizona" = "AZ",
  "arkansas" = "AR",
  "california" = "CA",
  "colorado" = "CO",
  "connecticut" = "CT",
  "delaware" = "DE",
  "florida" = "FL",
  "georgia" = "GA",
  "hawaii" = "HI",
  "idaho" = "ID",
  "illinois" = "IL",
  "indiana" = "IN",
  "iowa" = "IA",
  "kansas" = "KS",
  "kentucky" = "KY",
  "louisiana" = "LA",
  "maine" = "ME",
  "maryland" = "MD",
  "massachusetts" = "MA",
  "michigan" = "MI",
  "minnesota" = "MN",
  "mississippi" = "MS",
  "missouri" = "MO",
  "montana" = "MT",
  "nebraska" = "NE",
  "nevada" = "NV",
  "new hampshire" = "NH",
  "new jersey" = "NJ",
  "new mexico" = "NM",
  "new york" = "NY",
  "north carolina" = "NC",
  "north dakota" = "ND",
  "ohio" = "OH",
  "oklahoma" = "OK",
  "oregon" = "OR",
  "pennsylvania" = "PA",
  "rhode island" = "RI",
  "south carolina" = "SC",
  "south dakota" = "SD",
  "tennessee" = "TN",
  "texas" = "TX",
  "utah" = "UT",
  "vermont" = "VT",
  "virginia" = "VA",
  "washington" = "WA",
  "west virginia" = "WV",
  "wisconsin" = "WI",
  "wyoming" = "WY"
)
# Update the 'region' column with uppercase abbreviations
us_map$region <- state_abbreviations[us_map$region]

us_map <- merge(us_map, state_counts, by.x = "region", by.y = "Var1", all.x = TRUE)

us_map <- us_map %>%
  select(-Freq.x) %>%   # Remove the Freq.x column
  rename(Freq = Freq.y) # Rename Freq.y to Freq

# Create the visualization
ggplot(us_map, aes(x = long, y = lat, group = group, fill = Freq)) +
  geom_polygon() +
  scale_fill_gradient(low = "white", high = "darkblue") +
  labs(title = "Number of used cars for sale by U.S. state") +
  theme_void() +
  coord_fixed(ratio = 1.25)  # Fix the aspect ratio of the plot


# Calculate the counts of each state
state_counts <- table(df_eda$state)

# Create a dataframe from the state_counts table
state_counts_df <- as.data.frame(state_counts)
names(state_counts_df) <- c("State", "Frequency")

# Create a bar plot with reversed order
ggplot(state_counts_df, aes(x = reorder(State, -Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(x = "State", y = "Frequency", title = "Number of Used Car by State") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Calculate the counts of each state
city_counts <- table(df_eda$city)

# Create a dataframe from the city_counts table
city_counts_df <- as.data.frame(city_counts)
names(city_counts_df) <- c("City", "Frequency")

# Get the top 50 cities by frequency
top_50_cities <- head(city_counts_df[order(-city_counts_df$Frequency), ], 50)

# Create a bar plot with reversed order for the top 50 cities
ggplot(top_50_cities, aes(x = reorder(City, -Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(x = "City", y = "Frequency", title = "Number of Used Cars by City (Top 50)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# Apply logarithm to 'price'
df_eda$log_price <- log(df_eda$price)

# Create a scatterplot with a regression line
scatterplot <- ggplot(df_eda, aes(x = mileage, y = log_price)) +
  geom_point(color = "lightblue", shape = 21) + 
  geom_smooth(method = "lm", formula = y ~ x, color = "darkblue") +  
  labs(x = "Mileage", y = "Log(Price)", title = "Scatterplot with Regression Line (Log-Transformed Price)") +
  theme_minimal()

# Display the scatterplot with regression line
scatterplot



library(reshape2)

# Convert boolean columns in df_eda to numeric in df_eda2
df_eda2 <- df_eda

# Remove 'X' and 'X.1' columns from df_eda2
df_eda2 <- df_eda2[, !(names(df_eda2) %in% c('X', 'X.1'))]

df_eda2$frame_damaged <- as.numeric(df_eda2$frame_damaged)
df_eda2$salvage <- as.numeric(df_eda2$salvage)
df_eda2$is_new <- as.numeric(df_eda2$is_new)
df_eda2$is_cab <- as.numeric(df_eda2$is_cab)
df_eda2$fleet <- as.numeric(df_eda2$fleet)

# Calculate the correlation matrix between numeric variables
correlation_matrix <- cor(df_eda2[, sapply(df_eda2, is.numeric)])

# Create a correlation matrix in a long format
correlation_df <- melt(correlation_matrix)

# Create a heatmap plot for the correlation matrix
ggplot(correlation_df, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkred") +
  labs(title = "Correlation Heatmap",
       x = "Variables",
       y = "Variables") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






# Log transform the price variable
df_eda$log_price <- log(df_eda$price)

# Create a box plot comparing log-transformed vehicle prices by frame_damaged status
ggplot(df_eda, aes(x = frame_damaged, y = log_price, fill = frame_damaged)) +
  geom_boxplot() +
  labs(x = "Frame Damaged", y = "Log(Price)", title = "Comparison of Log-Transformed Vehicle Prices by Frame Damaged Status") +
  theme_minimal()



# Create a box plot comparing log-transformed vehicle prices by frame_damaged status
ggplot(df_eda, aes(x = salvage, y = log_price, fill = salvage)) +
  geom_boxplot() +
  labs(x = "Salvage", y = "Log(Price)", title = "Comparison of Log-Transformed Vehicle Prices by Salvage Status") +
  theme_minimal()




# Create a bar plot comparing the mean prices for frame_damaged groups (without log transformation)
df_eda %>%
  mutate(frame_damaged = ifelse(frame_damaged, "Damaged", "Not Damaged")) %>%
  ggplot(aes(x = frame_damaged, y = price, fill = frame_damaged)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(x = "Frame Damaged Status", y = "Mean Price",
       title = "Comparison of Mean Vehicle Prices by Frame Damaged Status") +
  theme_minimal()

# Create a bar plot comparing the mean prices for frame_damaged groups (without log transformation)
df_eda %>%
  mutate(salvage = ifelse(salvage, "Salvaged", "Not Salvaged")) %>%
  ggplot(aes(x = salvage, y = price, fill = salvage)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(x = "Salvage Status", y = "Mean Price",
       title = "Comparison of Mean Vehicle Prices by Salvage Status") +
  theme_minimal()



library(patchwork)
# Create a bar plot comparing the mean prices for frame_damaged groups (without log transformation)
plot1 <- df_eda %>%
  mutate(frame_damaged = ifelse(frame_damaged, "Damaged", "Not Damaged")) %>%
  ggplot(aes(x = frame_damaged, y = price, fill = frame_damaged)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(x = "Frame Damaged Status", y = "Mean Price",
       title = "Comparison of Mean Vehicle Prices by Frame Damaged Status") +
  theme_minimal()

# Create a bar plot comparing the mean prices for salvage groups (without log transformation)
plot2 <- df_eda %>%
  mutate(salvage = ifelse(salvage, "Salvaged", "Not Salvaged")) %>%
  ggplot(aes(x = salvage, y = price, fill = salvage)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(x = "Salvage Status", y = "Mean Price",
       title = "Comparison of Mean Vehicle Prices by Salvage Status") +
  theme_minimal()

# Combine the plots using patchwork
combined_plots <- plot1 + plot2
combined_plots




library(patchwork)

# Create a bar plot comparing the mean prices for frame_damaged groups (without log transformation)
plot1 <- df_eda %>%
  mutate(fleet = ifelse(fleet, "Fleet", "Not Fleet")) %>%
  ggplot(aes(x = fleet, y = price, fill = fleet)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(x = "Fleet Status", y = "Mean Price",
       title = "Mean Vehicle Prices by Fleet Status") +
  theme_minimal()

# Create a bar plot comparing the mean prices for salvage groups (without log transformation)
plot2 <- df_eda %>%
  mutate(is_new = ifelse(is_new, "New", "Not New")) %>%
  ggplot(aes(x = is_new, y = price, fill = is_new)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(x = "New Status", y = "Mean Price",
       title = "Mean Vehicle Prices by New Status") +
  theme_minimal()

# Create a bar plot comparing the mean prices for isCab, is_new, fleet groups (without log transformation)
plot3 <- df_eda %>%
  mutate(isCab = ifelse(isCab, "Cab", "Not Cab"),
         is_new = ifelse(is_new, "New", "Not New"),
         fleet = ifelse(fleet, "Fleet", "Not Fleet")) %>%
  ggplot(aes(x = isCab, y = price, fill = isCab)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(x = "Cab Status", y = "Mean Price",
       title = "Mean Vehicle Prices by Cab Status") +
  theme_minimal() +
  plot_layout(guides = "collect")

# Combine the plots using patchwork
combined_plots <- (plot1 + plot2) + plot3

# Display the combined plots
combined_plots



ggplot(df_eda, aes(x = make_name, y = price)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Price Distribution by Make Name", x = "Make Name", y = "Price")



####
ggplot(df_eda, aes(x = log(price), y = horsepower)) +
  geom_point(color = "lightblue", shape = 21) + 
  geom_smooth(method = "lm", se = FALSE) +  
  labs(title = "Scatter Plot of Log(Price) vs. Horsepower",
       x = "Log(Price)",
       y = "Horsepower") +
  theme_minimal()


#### Scatter plot
df_eda1 <- df_eda
df_eda1$is_old <- df_eda1$year < 2000

# Scatter plot
ggplot(df_eda1, aes(y = log(price), x= year, color = is_old)) +
  geom_point(shape = 21) + 
  geom_smooth(method = "lm", se = FALSE) +  
  labs(title = "Scatter Plot of Log(Price) vs. Year (Color by 'is_old': Whether the vehicle was produced before 2000)",
       y = "Log(Price)",
       x = "Year") +
  theme_minimal()

str(df_eda)


p1<-ggplot(df_eda, aes(x = log(price), y = body_type, fill = body_type)) +
  geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 3, color = "red") + 
  labs(title = "Box Plot of Log(Price) by Body Type",
       x = "Log(Price)",
       y = "Body Type") +
  theme_minimal()


# Create a histogram with grid facet for is_luxury and body_type
p2<-ggplot(df_eda, aes(x = body_type)) +
  geom_histogram(aes(y = log(..count..)), stat = "count", fill = "lightblue", color = "black") +
  facet_grid(is_luxury ~ .) +  # Grid facet by is_luxury
  labs(title = "Histogram of Body Type by Luxury Status (Log-scaled Frequency)",
       x = "Body Type",
       y = "Log(Frequency)") +
  theme_minimal()


# Arrange and display the plots as one
grid.arrange(p1, p2, ncol = 2)



# Create histograms with facets by wheel_system
ggplot(df_eda, aes(x = latitude, fill = wheel_system)) +
  geom_histogram(binwidth = 0.1, position = "identity", alpha = 0.5) +
  labs(title = "Histogram of Latitude by Wheel System",
       x = "Latitude",
       y = "Count") +
  facet_grid(. ~ wheel_system) +  # Facet by wheel_system
  theme_minimal()



# Create a scatter plot of log(price) vs. owner_count with a regression line
p1 <- ggplot(df_eda, aes(x = owner_count, y = log(price))) +
  geom_point(color = "lightblue", shape = 21, alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add a linear regression line
  labs(title = "Scatter Plot of Log(Price) vs. Owner Count ",
       x = "Owner Count",
       y = "Log(Price)") +
  theme_minimal()

# Scatter plot of mileage vs. owner_count
p2 <- ggplot(df_eda, aes(x = mileage, y = owner_count)) +
  geom_point(color = "lightblue", shape = 21, alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add a linear regression line
  labs(title = "Scatter Plot of Mileage vs. Owner Count",
       x = "Mileage",
       y = "Owner Count") +
  theme_minimal() +
  coord_flip()


# Arrange and display the plots as one
grid.arrange(p1, p2, ncol = 2)


ggplot(df_eda, aes(x = city_fuel_economy, y = log(price))) +
  geom_point(color = "lightblue", shape = 21, alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add a linear regression line
  labs(title = "Scatter Plot of Log(Price) vs. City Fuel Economy with Regression Line",
       x = "City Fuel Economy",
       y = "Log(Price)") +
  theme_minimal()

ggplot(df_eda, aes(x = highway_fuel_economy, y = log(price))) +
  geom_point(color = "lightblue", shape = 21, alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add a linear regression line
  labs(title = "Scatter Plot of Log(Price) vs. Highway Fuel Economy with Regression Line",
       x = "Highway Fuel Economy",
       y = "Log(Price)") +
  theme_minimal()


# Correlation matrix
cor_matrix <- cor(df_eda[c("horsepower", "city_fuel_economy", "highway_fuel_economy")])

# Create a heatmap of the correlation matrix
ggplot(data = melt(cor_matrix), aes(x = Var1, y = Var2, fill = value, label = round(value, 2))) +
  geom_tile() +
  geom_text(aes(label = ifelse(abs(value) > 0.5, as.character(round(value, 2)), "")), color = "lightblue", size = 6) +
  scale_fill_gradient(low = "white", high = "darkblue") +
  labs(title = "Correlation Heatmap with Text",
       x = "Variables",
       y = "Variables") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Create the individual plots
plot1 <- ggplot(df_eda, aes(x = city_fuel_economy, y = log(price))) +
  geom_point(color = "lightblue", shape = 21, alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Log(Price) vs. City Fuel Economy",
       x = "City Fuel Economy",
       y = "Log(Price)") +
  theme_minimal()

plot2 <- ggplot(df_eda, aes(x = highway_fuel_economy, y = log(price))) +
  geom_point(color = "lightblue", shape = 21, alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Log(Price) vs. Highway Fuel Economy",
       x = "Highway Fuel Economy",
       y = "Log(Price)") +
  theme_minimal()

plot3 <- ggplot(df_eda, aes(x = horsepower, y = log(price))) +
  geom_point(color = "lightblue", shape = 21, alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Log(Price) vs. Horsepower",
       x = "Horsepower",
       y = "Log(Price)") +
  theme_minimal()

cor_matrix <- cor(df_eda[c("price", "horsepower", "city_fuel_economy", "highway_fuel_economy")])

plot4 <- ggplot(data = melt(cor_matrix), aes(x = Var1, y = Var2, fill = value, label = round(value, 2))) +
  geom_tile() +
  geom_text(aes(label = ifelse(abs(value) > 0.5, as.character(round(value, 2)), "")), color = "lightblue", size = 3) +
  scale_fill_gradient(low = "white", high = "darkblue") +
  labs(title = "Correlation Heatmap",
       x = "Variables",
       y = "Variables") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Arrange and display the plots as one
grid.arrange(plot1, plot2, plot3, plot4, ncol = 4)


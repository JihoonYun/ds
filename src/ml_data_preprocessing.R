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



# Zipcode and States
# https://postalpro.usps.com/ZIP_Locale_Detail

# Read the CSV file
zip_csv <- 'ZIP_Locale_Detail.csv'
df_zipcode<- read.csv(zip_csv)


# Select and extract unique values from specific 2 columns in the data frame
df_zipcode <- df_zipcode %>%
  distinct(DELIVERY.ZIPCODE, PHYSICAL.STATE)


# Convert the 'PHYSICAL.ZIP' column to character type
df_zipcode$DELIVERY.ZIPCODE <- as.character(df_zipcode$DELIVERY.ZIPCODE)


# Pad the 'PHYSICAL.ZIP' column with leading zeros to make it 5 characters long
df_zipcode$DELIVERY.ZIPCODE <- sprintf("%05s", df_zipcode$DELIVERY.ZIPCODE)


# https://wisevoter.com/state-rankings/snowiest-states/

# Add state info
df_cleaned05 <- df_cleaned04 %>%
  left_join(df_zipcode, by = c("dealer_zip" = "DELIVERY.ZIPCODE"))

colnames(df_cleaned05)[colnames(df_cleaned05) == "PHYSICAL.STATE"] <- "state"

dim(df_cleaned05)
str(df_cleaned05)

df_cleaned05[is.na(df_cleaned05$dealer_zip), ]
df_cleaned05[df_cleaned05$dealer_zip == "", ]
df_cleaned05[is.na(df_cleaned05$state), ]

# Check the number of missing values in state
sum(is.na(df_cleaned05$state))


write.csv(df_cleaned05, file = "used_cars_data_cleaned_final.csv", row.names = TRUE)


################################################################################
# <Preprocessing>
################################################################################
df_cleaned_final <- df_cleaned05

# 1 Visualize data counts by US state 
# Load the packages
# Calculate the number of rows per state in the dataframe
state_counts <- df_cleaned_final %>%
  group_by(state) %>%
  summarise(row_count = n()) %>%
  arrange(desc(row_count)) 


# draw plot
ggplot(state_counts, aes(x = reorder(state, -row_count), y = row_count, fill = state)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Used Cars per State",
       x = "State",
       y = "Used Cars") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),  
        legend.position = "none") +
  scale_fill_manual(values = rep("#77AADD", nrow(state_counts)))






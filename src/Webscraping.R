# Libraries
library(tidyverse)
library(httr)
library(RSelenium)
library(rvest)
library(stringr)

##Selenium
remDr <- remoteDriver(remoteServerAddr="localhost", 
                      port=4445L, 
                      browserName="chrome")
remDr$open()


zipcode_url <- 'https://www.edq.com/resources/glossary/zip-codes-of-major-cities-in-the-united-states/'
remDr$navigate(zipcode_url)
web <- remDr$getPageSource()[[1]] %>% read_html() 

zipcode_strs <- web %>% 
  html_elements(".BulletListStyle1 .NormalTextRun") %>% 
  html_text()

# Initialize an empty vector to store the extracted zipcodes
extracted_zipcodes <- numeric(length(zipcode_strs))

# Loop through each string in the array
for (i in 1:length(zipcode_strs)) {
  # Extract the first 5 digits from the string if they are numeric
  if (grepl("^\\d{5}", zipcode_strs[i])) {
    extracted_zipcode <- substr(zipcode_strs[i], 1, 5)
    print(extracted_zipcode)
    
    extracted_zipcodes[i] <- extracted_zipcode
  }
}

extracted_zipcodes <- extracted_zipcodes[extracted_zipcodes != 0]

#print(extracted_zipcodes)

#############





##Variables
base_url <- 'https://www.cargurus.com/Cars/inventorylisting/viewDetailsFilterViewInventoryListing.action?sourceContext=carGurusHomePageModel&entitySelectingHelper.selectedEntity=&zip=__ZIP__#resultsPage=__PAGE__'



remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4445L,
  browserName = "chrome",
  extraCapabilities = list(
    chromeOptions = list(
      args = list('--headless')
    )
  )
)


#chrome_options <- c("--headless")
#remDr$serverInfo(extraCapabilities = list(chromeOptions = list(args = chrome_options)))

eCaps <- list(
  chromeOptions = list(
    prefs = list(
      "profile.managed_default_content_settings.images" = 2L
    )
  )
)

remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4445L,
  browserName = "chrome",
  extraCapabilities = eCaps
)


remDr <- remoteDriver(remoteServerAddr = "localhost", 
                      port = 4445L, 
                      browserName = "chrome")

remDr$open()


remDr$navigate('https://www.cargurus.com')

df <- data.frame(
  zipcode = numeric(0),
  name = character(0),
  price = numeric(0),
  mileage = numeric(0),
  city = character(0),
  state = character(0),
  year = numeric(0),
  make = character(0),
  model = character(0),
  body = character(0),
  doors = character(0),
  drivetrain = character(0),
  engine = character(0),
  color = character(0),
  fuel = character(0),
  transmission = character(0)
)




# Define the page range
start_page <- 1
end_page <- 1

# Loop to generate and print URLs
for (extracted_zipcode in extracted_zipcodes) {
  # Substitute the ZIP code into the URL
  url_1 <- str_replace(base_url, '__ZIP__', extracted_zipcode)
  print(extracted_zipcode)
  for (page in start_page:end_page) {
    # Substitute the page number into the URL
    url <- str_replace(url_1, '__PAGE__', as.character(page))
    remDr$navigate(url)
    
    #Sys.sleep(1)
    web <- remDr$getPageSource()[[1]] %>% read_html() 
    
    print(url)
    
    zipcodes <- rep(extracted_zipcode, 16)
    
    model_name <- web %>% 
      html_elements(".vO42pn") %>% 
      html_text()
    
    price <- web %>% 
      html_elements(".JzvPHo") %>% 
      html_text()
    
    car_price <- numeric(length(price))
    # Loop through each string in the array
    for (i in 1:length(price)) {
      # Extract the dollar amount from the string
      # Using regular expression to match the dollar sign and digits with commas
      match <- regexpr("\\$[0-9,]+", price[i])
      if (match > 0) {
        amount_str <- substr(price[i], match, attr(match, "match.length"))
        # Remove the dollar sign and commas, then convert to numeric
        amount <- as.numeric(gsub("\\$|,", "", amount_str))
        car_price[i] <- amount
      }
    }
    
    
    
    loc <- web %>% 
      html_elements(".umcYBP span") %>% 
      html_text()
    
    
    # Initialize empty arrays to store extracted information
    mileages <- numeric()
    cities <- character()
    states <- character()
    
    # Loop through the array with a step of 4
    for (i in seq(1, length(loc), by = 4)) {
      # Extract mileage information (numeric with optional comma)
      mileage_match <- regmatches(loc[i + 1], regexpr("\\d+(,\\d+)* mi", loc[i + 1]))
      if (length(mileage_match) > 0) {
        mileage_str <- gsub(",", "", mileage_match[[1]]) # Remove commas
        mileage_str <- gsub(" mi", "", mileage_str)      # Remove " mi"
        mileages <- c(mileages, as.numeric(mileage_str))  
      }
      
      # Extract city and state information (city, state format)
      city_state_match <- regmatches(loc[i + 3], regexpr("(.+), ([A-Z]{2})", loc[i + 3]))
      if (length(city_state_match) > 0) {
        city <- substr(city_state_match[[1]][1], 1, nchar(city_state_match[[1]][1]) - 4)  # Remove the last 3 digits
        state <- substr(city_state_match[[1]][1], nchar(city_state_match[[1]][1]) - 1, nchar(city_state_match[[1]][1]))  # Extract the last 2 characters
        cities <- c(cities, city)
        states <- c(states, state)
      }
    }
    
    
    # Extract values from elements with "dd" tag within "propertyList" class
    property_list <- web %>%
      html_elements("dl") %>%
      html_text()
    
    
    # Extract Year value
    year_value <- web %>%
      html_elements("dt:contains('Year') + dd") %>%
      html_text()
    
    make_value <- web %>%
      html_elements("dt:contains('Make') + dd") %>%
      html_text()
    
    model_value <- web %>%
      html_elements("dt:contains('Model') + dd") %>%
      html_text()
    
    body_type_value <- web %>%
      html_elements("dt:contains('Body') + dd") %>%
      html_text()
    
    doors_value <- web %>%
      html_elements("dt:contains('Doors') + dd") %>%
      html_text()
    
    drivetrain_value <- web %>%
      html_elements("dt:contains('Drivetrain') + dd") %>%
      html_text()
    
    engine_value <- web %>%
      html_elements("dt:contains('Engine') + dd") %>%
      html_text()
    
    exterior_color_value <- web %>%
      html_elements("dt:contains('Exterior') + dd") %>%
      html_text()
    
    fuel_type_value <- web %>%
      html_elements("dt:contains('Fuel') + dd") %>%
      html_text()
    
    transmission_value <- web %>%
      html_elements("dt:contains('Transmission') + dd") %>%
      html_text()
    
    new_df <- cbind(zipcodes,
                    model_name, 
                    car_price, 
                    mileages, 
                    cities, 
                    states, 
                    year_value,
                    make_value, 
                    model_value, 
                    body_type_value, 
                    doors_value, 
                    drivetrain_value,
                    engine_value, 
                    exterior_color_value, 
                    fuel_type_value, 
                    transmission_value )
    colnames(new_df) <- colnames(df)
    
    
    
    
    df <- rbind(df, new_df)
  }
}


write.csv(df, file = "my_data.csv", row.names = TRUE)



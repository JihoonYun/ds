# Required libraries
library(dplyr)
library(viridis)
library(arules)
library(TSP)
library(data.table)
#library(ggplot2)
#library(Matrix)
library(tcltk)
library(dplyr)
library(devtools)
library(purrr)
library(tidyr)
library(htmlwidgets)
library(xtable)

##############
## IF YOUR CODE BREAKS - TRY THIS
##
## Error in length(obj) : Method length not implemented for class rules 
## DO THIS: 
## (1) detach("package:arulesViz", unload=TRUE)
## (2) detach("package:arules", unload=TRUE)
## (3) library(arules)
## (4) library(arulesViz)
###################################################################

## To see if you have tcltk run this on the console...
# capabilities()["tcltk"]
library(arulesViz)

# Load data
df_transactions <- read.csv("used_cars_data_cleaned_final_ver_arm_sampled_transactions.csv")

# Read Transaction
Usedcars <- read.transactions("used_cars_data_cleaned_final_ver_arm_sampled_transactions.csv",
                              rm.duplicates = FALSE, 
                              format = "basket",  ##if you use "single" also use cols=c(1,2)
                              sep=",",  ## csv file
                              header = TRUE,
                              cols=NULL) ## The dataset has no row numbers

## Plot of which items are most frequent
itemFrequencyPlot(Usedcars, topN=20, type="absolute")

# Convert used car data frames into transactions.
transactions <- as(Usedcars, "transactions")

# Use the itemFrequency function to find the absolute frequency of the top 20 items.
item_freq <- itemFrequency(transactions)
print(head(sort(item_freq, decreasing = TRUE), 20))

# Define the user-defined function
# Function to convert command result to an HTML table
convertToHTMLTable <- function(rules, num_top = 15, sort_by = "support", decreasing = TRUE) {
  # Execute the command and store the result as a data frame
  result_df <- as(sort(rules, decreasing = decreasing, by = sort_by)[1:num_top], "data.frame")
  
  # Convert to an HTML table
  print(xtable(result_df), type = "html")
}

################################################################################################
# Apriori > Low Price
################################################################################################
PriceRules_low <- apriori(data=Usedcars, parameter = list(supp=.095, conf=.1, minlen=3),
                      appearance = list(default="lhs", rhs="Low Price"),
                      control=list(verbose=FALSE))

PriceRules_low <- unique(PriceRules_low)
inspect(sort(PriceRules_low, decreasing=TRUE, by="support"))
inspect(sort(PriceRules_low, decreasing=TRUE, by="support")[1:15])
inspect(sort(PriceRules_low, decreasing=TRUE, by="confidence")[1:15])
inspect(sort(PriceRules_low, decreasing=TRUE, by="lift")[1:15]) 

convertToHTMLTable(PriceRules_low, 15, "lift", TRUE)

# Scatter Plot
plot(PriceRules_low, method = "scatterplot")
# Graph Plot
plot(PriceRules_low, method="graph", engine="htmlwidget")
saveWidget(plot(PriceRules_low, method="graph", engine="htmlwidget"), file = "output_low.html")

################################################################################################
# Apriori > Medium Price
################################################################################################
PriceRules_md <- apriori(data=Usedcars, parameter = list(supp=.095, conf=.1, minlen=3),
                         appearance = list(default="lhs", rhs="Medium Price"),
                         control=list(verbose=FALSE))

PriceRules_md <- unique(PriceRules_md)
inspect(sort(PriceRules_md, decreasing=TRUE, by="support"))
inspect(sort(PriceRules_md, decreasing=TRUE, by="support")[1:15])
inspect(sort(PriceRules_md, decreasing=TRUE, by="confidence")[1:15])
inspect(sort(PriceRules_md, decreasing=TRUE, by="lift")[1:15]) 

convertToHTMLTable(PriceRules_md, 15, "lift", TRUE)

# Scatter Plot
plot(PriceRules_md, method = "scatterplot")
# Graph Plot
plot(PriceRules_md, method="graph", engine="htmlwidget")
saveWidget(plot(PriceRules_md, method="graph", engine="htmlwidget"), file = "output_medium.html")

################################################################################################
# Apriori > High Price
################################################################################################
PriceRules_high <- apriori(data=Usedcars, parameter = list(supp=.095, conf=.1, minlen=3),
                         appearance = list(default="lhs", rhs="High Price"),
                         control=list(verbose=FALSE))

PriceRules_high <- unique(PriceRules_high)
inspect(sort(PriceRules_high, decreasing=TRUE, by="support"))
inspect(sort(PriceRules_high, decreasing=TRUE, by="support")[1:15])
inspect(sort(PriceRules_high, decreasing=TRUE, by="confidence")[1:15])
inspect(sort(PriceRules_high, decreasing=TRUE, by="lift")[1:15]) 

convertToHTMLTable(PriceRules_high, 15, "lift", TRUE)

# Scatter Plot
plot(PriceRules_high, method = "scatterplot")
# Graph Plot
plot(PriceRules_high, method="graph", engine="htmlwidget")
saveWidget(plot(PriceRules_high, method="graph", engine="htmlwidget"), file = "output_high.html")

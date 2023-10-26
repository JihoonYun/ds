# Required libraries
library(dplyr)
library(png)
library(grDevices)
library(Metrics)
library(reshape2)
library(ggplot2)
library(tidyr)
library(knitr)
library(kableExtra)
library(e1071)  
library(caTools)
library(caret)


# Load CSV 
train_data <- read.csv("used_cars_data_cleaned_final_ver_nb_sampled_preprocessed_train_data.csv", row.names = 1)
test_data <- read.csv("used_cars_data_cleaned_final_ver_nb_sampled_preprocessed_test_data.csv", row.names = 1)

# Convert all columns to factors
train_data[] <- lapply(train_data, as.factor)
test_data[] <- lapply(test_data, as.factor)

# Split the dataset into training and test sets (70% train, 30% test)
# set.seed(123)  # for reproducibility
# split <- sample.split(df$label, SplitRatio = 0.7)
# train_data <- df[split, ]
# test_data <- df[!split, ]

# Define functions
calculate_metrics <- function(conf_matrix, features) {
  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
  precision <- diag(conf_matrix) / colSums(conf_matrix)
  recall <- diag(conf_matrix) / rowSums(conf_matrix)
  f1_score <- 2 * precision * recall / (precision + recall)  
  
  result <- data.frame(Features = features[3], Class = c("Low", "Moderate", "High"), Accuracy = accuracy, Precision = precision, Recall = recall, F1_Score = f1_score)
  rownames(result) <- NULL
  return(result)
}

analyze_nb <- function(features, result_df) {
  # Build a Naive Bayes model
  model <- naiveBayes(features, data = train_data)
  
  # Make predictions on the test set
  predictions <- predict(model, newdata = test_data, type = "class")
  
  # Calculate the confusion matrix
  conf_matrix <- confusionMatrix(predictions, test_data$label)
  
  # Calculate accuracy, precision, recall, specificity, and F1 score for Gini method
  result_df <- rbind(result_df, calculate_metrics(conf_matrix$table, as.character(features)))
  
  return (list(result_df, conf_matrix, model, predictions))
}


# Define result dataframe
result_df <- data.frame(formula = character(),
                        Class = character(), 
                        Accuracy = numeric(), 
                        Precision = numeric(), 
                        Recall = numeric(),
                        F1_Score = numeric(),
                        stringsAsFactors = FALSE)

result <- analyze_nb(label ~., result_df)

print("Prior Probabilities:")
print(result[[3]]$apriori)
print("\nConditional Probabilities:")
print(result[[3]]$table)

png("apriori_nb.png", width = 3600, height = 2800, units = "px", pointsize = 12, res = 600)
ggplot(as.data.frame(result[[3]]$apriori), aes(x = class_labels, y = prior_prob, fill = class_labels)) +
  geom_bar(stat = "identity") +
  labs(title = "Prior Probabilities of Vehicle Price Classes",
       x = "Price Class",
       y = "Prior Probability") +
  theme_minimal()
# Save the plot
dev.off()

result[[2]]

conf_mat_table <- as.data.frame(result[[2]]$table)

# Visualization
png("confusionMatrix_nb.png", width = 3600, height = 2800, units = "px", pointsize = 12, res = 600)
ggplot(data = conf_mat_table, aes(x = Reference, y = Prediction, fill = Freq, label = Freq)) +
  geom_tile() +
  geom_text(color = "white", size = 8, fontface = "bold") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Confusion Matrix Heatmap based",
       x = "Reference",
       y = "Prediction",
       fill = "Frequency")
# Save the plot
dev.off()

# Generate Html Table code
kable(result[[1]], format = "html", table.attr = ' class="table  table-sm" style="font-size: 12px;"')

# Clear df
result_df <- result_df[0, ]

# Define formulars
fml <- list(
  label ~ horsepower + torque_lbft,
  label ~ horsepower + torque_lbft + fuel_tank_volume,
  label ~ horsepower + torque_lbft + fuel_tank_volume + mileage,
  label ~ horsepower + torque_lbft + fuel_tank_volume + mileage + year,
  label ~ horsepower + torque_lbft + fuel_tank_volume + mileage + year + highway_fuel_economy,
  label ~ horsepower + torque_lbft + fuel_tank_volume + mileage + year + highway_fuel_economy + wheelbase,
  label ~ horsepower + torque_lbft + fuel_tank_volume + mileage + year + highway_fuel_economy + wheelbase + owner_count
)


for (i in 1:length(fml)) {
  print(fml[[i]])
  result_df <- analyze_nb(fml[[i]], result_df)[[1]]
}
result_df
kable(result_df, format = "html", table.attr = ' class="table  table-sm" style="font-size: 12px;"')





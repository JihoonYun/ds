# Required libraries
library(dplyr)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(caret)
library(png)
library(grDevices)
library(Metrics)
library(reshape2)
library(ggplot2)
library(tidyr)
library(knitr)
library(kableExtra)

# Load CSV 
df <- read.csv("used_cars_data_cleaned_final_ver.csv")
str(df)

# Set seed for reproducibility
set.seed(2010)

# Number of rows to sample 
num_rows_to_sample <- 100000

# Randomly sample 100 rows from the scaled_data dataframe
df_sampled <- df[sample(nrow(df), num_rows_to_sample, replace = FALSE), ]

# store CSV 
write.csv(df_sampled, "used_cars_data_cleaned_final_ver_dt_sampled.csv")
df_sampled <- read.csv("used_cars_data_cleaned_final_ver_dt_sampled.csv")

# Extract the desired columns from scaled_org_data
# Exclude fields inappropriate for analysis
# df_dt <- df_sampled[, c('transmission', 'fuel_type', 'wheel_system', 'body_type', "city_fuel_economy", "highway_fuel_economy", "engine_displacement",  "fuel_tank_volume", "engine_cylinders", "horsepower", "torque_lbft", "mileage", "year", "width", "length", "owner_count", "has_accidents", "fleet", "body_type", "price")]
df_dt <- subset(df_sampled, select = -c(sp_name, sp_id, latitude, listing_color, interior_color, longitude, is_luxury, trim_name, model_name, major_options, main_picture_url, listing_id, description, vin, dealer_zip, city, exterior_color,franchise_make, listed_date, make_name,state, X.3, X.2, X.1,X))

# Convert categorical variables to numeric types through One-Hot Encoding
# Perform one-hot encoding for the 'transmission' column
df_dt_encoded <- cbind(df_dt, model.matrix(~ transmission - 1, data = df_dt))
# Remove the original 'transmission' column after encoding
df_dt_encoded <- df_dt_encoded[, !names(df_dt) %in% 'transmission']

# Perform one-hot encoding for the 'fuel_type' column
df_dt_encoded <- cbind(df_dt_encoded, model.matrix(~ fuel_type - 1, data = df_dt_encoded))
# Remove the original 'fuel_type' column after encoding
df_dt_encoded <- df_dt_encoded[, !names(df_dt_encoded) %in% 'fuel_type']

# Perform one-hot encoding for the 'wheel_system' column
df_dt_encoded <- cbind(df_dt_encoded, model.matrix(~ wheel_system - 1, data = df_dt_encoded))
# Remove the original 'wheel_system' column after encoding
df_dt_encoded <- df_dt_encoded[, !names(df_dt_encoded) %in% 'wheel_system']

# Perform one-hot encoding for the 'body_type' column
df_dt_encoded <- cbind(df_dt_encoded, model.matrix(~ body_type - 1, data = df_dt_encoded))
# Remove the original 'body_type' column after encoding
df_dt_encoded <- df_dt_encoded[, !names(df_dt_encoded) %in% 'body_type']

df_dt <- df_dt_encoded

###########################################################################
# For regression
# Dataset for a model that predicts vehicle prices through regression among decision trees
# Label is set to vehicle price
###########################################################################

# Dataframe for regression
df_dt_regression <- df_dt_encoded

# Change the column name 'price' to 'label' in df_dt_regression
colnames(df_dt_regression)[colnames(df_dt_regression) == "price"] <- "label"

# Identify the position of the 'label' column
label_column_index <- which(names(df_dt_regression) == "label")

# Move the 'label' column to the last position
df_dt_regression <- df_dt_regression[, c(1:(label_column_index - 1), (label_column_index + 1):ncol(df_dt_regression), label_column_index)]

# Split the dataset
set.seed(123) #reproducability setting
train_indices_regression <- sample(1:nrow(df_dt_regression), 0.7 * nrow(df_dt_regression))#createDataPartition(y = df_dt_regression$label, p = 0.7, list = FALSE)
df_dt_train_regression <- df_dt_regression[train_indices_regression, ]
df_dt_test_regression <- df_dt_regression[-train_indices_regression, ]

cat("Training set length: ", nrow(df_dt_train_regression), "\n")
cat("Test set length: ", nrow(df_dt_test_regression), "\n")

###########################################################################
# For classification
# Label is set to vehicle price
###########################################################################
# Add a new column (category) to the dataframe to convert price into a categorical variable
df_dt$label <- cut(df_dt$price, breaks = c(0, 13000, 30000, 10000000), labels = c("Low", "Moderate", "High"))
df_dt$label <- as.factor(df_dt$label)

# Delete price column
df_dt <- df_dt[, -which(names(df_dt) == "price")]
df_dt$label
# Split the dataset
set.seed(123) #reproducability setting
train_indices <- sample(1:nrow(df_dt), 0.7 * nrow(df_dt))
df_dt_train <- df_dt[train_indices, ]
df_dt_test <- df_dt[-train_indices, ]

cat("Training set length: ", nrow(df_dt_train), "\n")
cat("Test set length: ", nrow(df_dt_test), "\n")

df_dt_html <- kable(head(df_dt), format = "html", table.attr = "style='width:100%;'") %>%
  kable_styling(full_width = FALSE)

# Define result dataframe
result_df <- data.frame(Method = character(), 
                 Minsplit = numeric(),         
                 Maxdepth = numeric(),         
                 Accuracy = numeric(), 
                 Precision = numeric(), 
                 Recall = numeric(),
                 Specificity = numeric(),
                 F1_Score = numeric(),
                 stringsAsFactors = FALSE)


write.csv(df_dt, "used_cars_data_cleaned_final_ver_dt_sampled_preprocessed.csv") 
write.csv(df_dt_train, "used_cars_data_cleaned_final_ver_dt_sampled_preprocessed_train.csv") 
write.csv(df_dt_test, "used_cars_data_cleaned_final_ver_dt_sampled_preprocessed_test.csv")


###########################################################################################
# Gini
###########################################################################################
# Train
rpart_gini <- rpart(df_dt_train$label ~., data = df_dt_train, method='class') 

# Create a high-quality PNG file
png("fancyRpartPlot_gini.png", width = 3600, height = 2800, units = "px", pointsize = 12, res = 600)
fancyRpartPlot(rpart_gini)
# Save the plot
dev.off()

# Extract variable importance
var_imp <- varImp(rpart_gini) %>% 
           rownames_to_column() %>% 
           arrange(desc(Overall)) %>% 
           slice(1:20)

ggplot(var_imp, aes(x = reorder(rowname, -Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Top 20 Variable Importance", x = "Variable", y = "Importance") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Find the cp value for pruning
# printcp(rpart_gini)
# 
# cp_value <- rpart_gini$cptable[which.min(rpart_gini$cptable[,"xerror"]),"CP"]
# 
# pruned_rpart_entropy <- prune(rpart_entropy, cp = 0.02)  # 
# fancyRpartPlot(pruned_rpart_entropy)

# Prediction
rpart_gini_pd <- predict(rpart_gini, df_dt_test, type = "class")

# Calculate confusion matrix
confusion_matrix_gini <- confusionMatrix(rpart_gini_pd, df_dt_test$label)

# Calculate accuracy, precision, recall, specificity, and F1 score for Gini method

accuracy_gini <- confusion_matrix_gini$overall[1]
precision_gini <- confusion_matrix_gini$byClass[1]
recall_gini <- confusion_matrix_gini$byClass[2]
specificity_gini <- confusion_matrix_gini$byClass[3]
f1_score_gini <- confusion_matrix_gini$byClass[4]

new_row <- data.frame(Method = "Gini",
                      Minsplit = -1,         
                      Maxdepth = -1,    
                      Accuracy = accuracy_gini,
                      Precision = precision_gini,
                      Recall = recall_gini,
                      Specificity = specificity_gini,
                      F1_Score = f1_score_gini,
                      stringsAsFactors = FALSE)

result_df <- rbind(result_df, new_row)

###########################################################################################
# Entropy
###########################################################################################
# Train
rpart_entropy <- rpart(df_dt_train$label ~., data = df_dt_train, method='class', parms = list(split="information"))
rpart.plot(rpart_entropy)
fancyRpartPlot(rpart_entropy)
prp(rpart_entropy)

# Prediction
rpart_entropy_pd <- predict(rpart_entropy, df_dt_test, type = "class", parms = list(split="information"))

# Calculate confusion matrix
confusion_matrix_entropy <- confusionMatrix(rpart_entropy_pd, df_dt_test$label)

# Calculate accuracy, precision, recall, specificity, and F1 score for Entropy method
accuracy_entropy <- confusion_matrix_entropy$overall[1]
precision_entropy <- confusion_matrix_entropy$byClass[1]
recall_entropy <- confusion_matrix_entropy$byClass[2]
specificity_entropy <- confusion_matrix_entropy$byClass[3]
f1_score_entropy <- confusion_matrix_entropy$byClass[4]

new_row <- data.frame(Method = "Entropy",
                      Minsplit = -1,         
                      Maxdepth = -1,    
                      Accuracy = accuracy_entropy,
                      Precision = precision_entropy,
                      Recall = recall_entropy,
                      Specificity = specificity_entropy,
                      F1_Score = f1_score_entropy,
                      stringsAsFactors = FALSE)

result_df <- rbind(result_df, new_row)


###########################################################################################
# minsplit = 10, 20, 30Permalink
###########################################################################################
for (s in c('gini', 'information')){
  for (i in c(1, 5, 10, 20, 40)){
    rpart_minsplit <- rpart(df_dt_train$label ~., data = df_dt_train, method='class',control = rpart.control(minsplit = i), parms = list(split=s) )
    #fancyRpartPlot(rpart_minsplit)
    
    rpart_minsplit_pd <- predict(rpart_minsplit, df_dt_test, type = "class", parms = list(split=s))
    
    # Calculate confusion matrix
    confusion_matrix_minsplit <- confusionMatrix(rpart_minsplit_pd, df_dt_test$label)
    
    # Calculate accuracy, precision, recall, specificity, and F1 score for Gini method
    
    accuracy_minsplit <- confusion_matrix_minsplit$overall[1]
    precision_minsplit <- confusion_matrix_minsplit$byClass[1]
    recall_minsplit <- confusion_matrix_minsplit$byClass[2]
    specificity_minsplit <- confusion_matrix_minsplit$byClass[3]
    f1_score_minsplit <- confusion_matrix_minsplit$byClass[4]
    
    new_row <- data.frame(Method = ifelse(s == 'gini', 'Gini', ifelse(s == 'information', 'Entropy', NA)),
                          Minsplit = i,         
                          Maxdepth = -1,    
                          Accuracy = accuracy_minsplit,
                          Precision = precision_minsplit,
                          Recall = recall_minsplit,
                          Specificity = specificity_minsplit,
                          F1_Score = f1_score_minsplit,
                          stringsAsFactors = FALSE)
    
    result_df <- rbind(result_df, new_row)
  }
}

###########################################################################################
# minsplit = 4, 5, 6 maxdepth
###########################################################################################
for (s in c('gini', 'information')){
  for (i in c(2, 3, 4, 5, 6, 7)){
    rpart_maxdepth <- rpart(df_dt_train$label ~., data = df_dt_train, method='class',control = rpart.control(maxdepth = i), parms = list(split=s))
      # fancyRpartPlot(rpart_maxdepth)
    png(paste0("fancyRpartPlot_",s,"_maxdepth_",i,".png"), width = 3600, height = 2800, units = "px", pointsize = 12, res = 600)
    fancyRpartPlot(rpart_maxdepth)
    # Save the plot
    dev.off()
    
    rpart_maxdepth_pd <- predict(rpart_maxdepth, df_dt_test, type = "class", parms = list(split=s))
    
    confusion_matrix_maxdepth <- confusionMatrix(rpart_maxdepth_pd, df_dt_test$label)
    
    # Calculate accuracy, precision, recall, specificity, and F1 score for Gini method
    
    accuracy_maxdepth <- confusion_matrix_maxdepth$overall[1]
    precision_maxdepth <- confusion_matrix_maxdepth$byClass[1]
    recall_maxdepth <- confusion_matrix_maxdepth$byClass[2]
    specificity_maxdepth <- confusion_matrix_maxdepth$byClass[3]
    f1_score_maxdepth <- confusion_matrix_maxdepth$byClass[4]
    
    new_row <- data.frame(Method = ifelse(s == 'gini', 'Gini', ifelse(s == 'information', 'Entropy', NA)),
                          Minsplit = -1,         
                          Maxdepth = i,    
                          Accuracy = accuracy_maxdepth,
                          Precision = precision_maxdepth,
                          Recall = recall_maxdepth,
                          Specificity = specificity_maxdepth,
                          F1_Score = f1_score_maxdepth,
                          stringsAsFactors = FALSE)
    
    result_df <- rbind(result_df, new_row)
  }
}

result_df



# Train the model
rpart_regression <- rpart(label ~ ., data = df_dt_train_regression)
# Create a high-quality PNG file
png("fancyRpartPlot_regression.png", width = 3600, height = 2800, units = "px", pointsize = 12, res = 600)
fancyRpartPlot(rpart_regression)
# Save the plot
dev.off()


# Visualize the decision tree
rpart.plot(rpart_regression, main = "Decision Tree for Regression")

# Predict using the test set
predictions <- predict(rpart_regression, df_dt_test_regression)

mse <- mse(df_dt_test_regression$label, predictions)
mae <- mae(df_dt_test_regression$label, predictions)
rsquared <- R2(predictions, df_dt_test_regression$label)

# Create a table to display the evaluation metrics
evaluation_table <- data.frame(
  Metric = c("Mean Squared Error", "Mean Absolute Error", "R-Squared"),
  Value = c(mse, mae, rsquared)
)


evaluation_table <- data.frame("MSE" = mse, 
                               "MAE" = mae,         
                               "R-Squared" = rsquared)

# Print the evaluation table
print(evaluation_table)







# Train a decision tree model on training and test sets
rpart_model_before <- rpart(label ~ ., data = df_dt_train, method = 'class')
rpart_model_after <- rpart(label ~ ., data = df_dt_train, method = 'class', cp = 0.001) 


# Tree visualization before pruning for training set
rpart.plot(rpart_model_before, main = "Decision Tree Before Pruning")
# Tree visualization after pruning on the training set
rpart.plot(rpart_model_after, main = "Decision Tree After Pruning")


train_predictions_before <- predict(rpart_model_before, newdata = df_dt_train, type = "class")
train_predictions_after <- predict(rpart_model_after, newdata = df_dt_train, type = "class")


# Accuracy output of training set and test set
train_accuracy_before <- mean(train_predictions_before == df_dt_train$label)
train_accuracy_after <- mean(train_predictions_after == df_dt_train$label)


# Predictions on the test set
predictions_before <- predict(rpart_model_before, newdata = df_dt_test, type = "class")
predictions_after <- predict(rpart_model_after, newdata = df_dt_test, type = "class")

# Accuracy comparison before and after pruning
accuracy_before <- mean(predictions_before == df_dt_test$label)
accuracy_after <- mean(predictions_after == df_dt_test$label)

# Result output
print(paste("Accuracy before pruning:", accuracy_before))
print(paste("Accuracy after pruning:", accuracy_after))

# Check for overfitting
if (train_accuracy_before - accuracy_before > 0.05) {
  print("Overfitting detected before pruning.")
} else {
  print("No overfitting detected before pruning.")
}

if (train_accuracy_after - accuracy_after > 0.05) {
  print("Overfitting detected after pruning.")
} else {
  print("No overfitting detected after pruning.")
}




# Predictions on the training set
train_predictions_before <- predict(rpart_model_before, newdata = df_dt_train, type = "class")
train_predictions_after <- predict(rpart_model_after, newdata = df_dt_train, type = "class")

# Accuracy output of training set and test set
train_accuracy_before <- mean(train_predictions_before == df_dt_train$label)
train_accuracy_after <- mean(train_predictions_after == df_dt_train$label)

print(paste("Train Accuracy before pruning:", train_accuracy_before))
print(paste("Train Accuracy after pruning:", train_accuracy_after))

# Check for overfitting
if (train_accuracy_before - accuracy_before > 0.05) {
  print("Overfitting detected before pruning.")
} else {
  print("No overfitting detected before pruning.")
}

if (train_accuracy_after - accuracy_after > 0.05) {
  print("Overfitting detected after pruning.")
} else {
  print("No overfitting detected after pruning.")
}




# Train model
rpart_model <- rpart(label ~ ., data = df_dt_train, method = 'class')

# Error visualization based on cp value
plotcp(rpart_model)

# Find the optimal cp value
opt_cp <- rpart_model$cptable[which.min(rpart_model$cptable[,"xerror"]),"CP"]
print(paste("Optimal CP value:", opt_cp))



# Define a sequence of cp values
cp_seq <- seq(0.01, 0.5, by = 0.01)

# Create empty vectors to store accuracy values
train_accuracy <- vector("numeric", length = length(cp_seq))
test_accuracy <- vector("numeric", length = length(cp_seq))

# Train models with different cp values and compute accuracies
for (i in 1:length(cp_seq)) {
  rpart_model <- rpart(label ~ ., data = df_dt_train, method = "class", cp = cp_seq[i])
  train_predictions <- predict(rpart_model, newdata = df, type = "class")
  train_accuracy[i] <- mean(train_predictions == df_dt_train$label)
}

# Plot the learning curve
learning_curve <- data.frame(cp = cp_seq, train_accuracy = train_accuracy)
ggplot(learning_curve, aes(x = cp, y = train_accuracy)) +
  geom_line() +
  labs(title = "Learning Curve", x = "CP Value", y = "Accuracy") +
  theme_minimal()

# Find the cp value where overfitting occurs
overfitting_cp <- cp_seq[which.max(train_accuracy)]
print(paste("CP value where overfitting occurs:", overfitting_cp))








# Define a sequence of cp values
cp_seq <- seq(0.0001, 0.5, by = 0.0005)

# Create empty vectors to store accuracy values
train_accuracy <- vector("numeric", length = length(cp_seq))
test_accuracy <- vector("numeric", length = length(cp_seq))

# Train models with different cp values and compute accuracies
for (i in 1:length(cp_seq)) {
  rpart_model <- rpart(label ~ ., data = df_dt_train, method = "class", cp = cp_seq[i])
  train_predictions <- predict(rpart_model, newdata = df_dt_train, type = "class")
  train_accuracy[i] <- mean(train_predictions == df_dt_train$label)
  test_predictions <- predict(rpart_model, newdata = df_dt_test, type = "class")
  test_accuracy[i] <- mean(test_predictions == df_dt_test$label)
}

# Create empty vectors to store accuracy values
train_accuracy <- vector("numeric", length = length(cp_seq))
test_accuracy <- vector("numeric", length = length(cp_seq))
accuracy_diff <- vector("numeric", length = length(cp_seq))

# Train models with different cp values and compute accuracies
for (i in 1:length(cp_seq)) {
  rpart_model <- rpart(label ~ ., data = df_dt_train, method = "class", cp = cp_seq[i])
  train_predictions <- predict(rpart_model, newdata = df_dt_train, type = "class")
  train_accuracy[i] <- mean(train_predictions == df_dt_train$label)
  
  test_predictions <- predict(rpart_model, newdata = df_dt_test, type = "class")
  test_accuracy[i] <- mean(test_predictions == df_dt_test$label)
  
  accuracy_diff[i] <- abs(train_accuracy[i] - test_accuracy[i])
}
# Filter values with accuracy difference <= 0.05
filtered_indices <- which(accuracy_diff <= 0.05)
optimal_cp <- cp_seq[filtered_indices[1]]

# Plot the learning curve
learning_curve <- data.frame(cp = cp_seq, train_accuracy = train_accuracy, test_accuracy = test_accuracy, accuracy_diff = accuracy_diff)
ggplot(learning_curve, aes(x = cp)) +
  geom_line(aes(y = train_accuracy, color = "Train Accuracy")) +
  geom_line(aes(y = test_accuracy, color = "Test Accuracy")) +
  labs(title = paste("Learning Curve (Optimal CP =", round(optimal_cp, 3), ")"), x = "CP Value", y = "Accuracy") +
  scale_color_manual(name = "Accuracy", values = c("Train Accuracy" = "blue", "Test Accuracy" = "red")) +
  theme_minimal() +
  geom_vline(xintercept = optimal_cp, linetype = "dashed", color = "green") +
  annotate("text", x = optimal_cp, y = max(learning_curve$test_accuracy), 
           label = paste("Optimal CP =", round(optimal_cp, 3)), vjust = -1, hjust = -0.5, color = "green")

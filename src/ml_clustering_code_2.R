# Required Libraries
library(cluster)
library(proxy)
library(NbClust)

# Load CSV 
scaled_org_data <- read.csv("used_cars_data_cleaned_final_ver_sampled.csv")
#scaled_data <- read.csv("used_cars_data_cleaned_final_ver_sampled_preprocessed.csv")

# Set seed for reproducibility
set.seed(123)

# Number of rows to sample 
num_rows_to_sample <- 100

# Randomly sample 100 rows from the scaled_data dataframe
sampled_org_data <- scaled_org_data[sample(nrow(scaled_org_data), num_rows_to_sample, replace = FALSE), ]

# Extract the desired columns from scaled_org_data
sampled_data <- sampled_org_data[, c('horsepower', 'engine_displacement', 'mileage', 'year')]

# Convert 'has_accidents' column from strings to 1 for True and 0 for False, and then to numeric
#sampled_data$has_accidents <- as.numeric(sampled_data$has_accidents == "True")

# Scaling Data
sampled_data <- scale(sampled_data)

# Calculate average value for each cluster
cosine_distance <- proxy::dist(as.matrix(sampled_data), method = "cosine")

# Perform hierarchical clustering
hierarchical_clustering <- hclust(cosine_distance, method = "ward.D2") 

########
# Create a cluster with K set to 3
k <-4
clusters <- cutree(hierarchical_clustering, k)

# Label settings
#labels <- round(sampled_org_data$price / 1000, digits = 2)#rownames(sampled_data)
labels <- paste(sampled_org_data$model_name, '/', sampled_org_data$year)
# unique_breaks <- unique(c(min(sampled_org_data$price), quantile(sampled_org_data$price, c(0, 0.33, 0.66, 1)), max(sampled_org_data$price)))
# labels <- cut(
#   sampled_org_data$price,
#   breaks = unique_breaks,
#   labels = c("HIGH PRICE", "MEDIUM PRICE", "LOW PRICE"),
#   include.lowest = TRUE
# )

# Draw a dendrogram (show clusters)
plot(hierarchical_clustering, labels = labels, cex = 0.6)

# Show K clusters (colored)
rect.hclust(hierarchical_clustering, k = k, border = 'blue')

# Calculate average value for each cluster
cluster_means <- aggregate(sampled_data, by = list(Cluster = clusters), FUN = mean)
cluster_means

cluster_counts <- table(clusters)
cluster_counts
########



# Call NbClust
nc <- NbClust(data = sampled_data, min.nc = 2, max.nc = 10, method = "ward.D2")
nc$Best.nc[1]
table(nc$Best.nc[1,])
########



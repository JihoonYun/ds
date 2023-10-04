# Required Libraries
library(cluster)
library(proxy)
library(NbClust)

# Load CSV 
scaled_org_data <- read.csv("used_cars_data_cleaned_final_ver_sampled.csv")
#scaled_data <- read.csv("used_cars_data_cleaned_final_ver_sampled_preprocessed.csv")

# Extract the desired columns from scaled_org_data
scaled_data <- scaled_org_data[, c('horsepower', 'highway_fuel_economy', 'mileage', 'wheelbase', 'year', 'price', 'has_accidents')]

# Convert 'has_accidents' column from strings to 1 for True and 0 for False, and then to numeric
scaled_data$has_accidents <- as.numeric(scaled_data$has_accidents == "True")

# Scaling Data
scaled_data <- scale(scaled_data)


# Set seed for reproducibility
set.seed(123)

# Number of rows to sample (100 in this case)
num_rows_to_sample <- 80

# Randomly sample 100 rows from the scaled_data dataframe
sampled_data <- scaled_data[sample(nrow(scaled_data), num_rows_to_sample, replace = FALSE), ]
sampled_data <- sampled_data[, -1]

# Calculate average value for each cluster
cosine_distance <- proxy::dist(as.matrix(sampled_data), method = "cosine")

# Perform hierarchical clustering
hierarchical_clustering <- hclust(cosine_distance, method = "ward") 

########
# Create a cluster with K set to 3
k <- 3
clusters <- cutree(hierarchical_clustering, k)

# Label settings
#labels <- round(sampled_org_data$price / 1000, digits = 2)#rownames(sampled_data)
labels <- paste(sampled_org_data$model_name, '/', sampled_org_data$year)


# Draw a dendrogram (show clusters)
plot(hierarchical_clustering, labels = labels)

# Show K clusters (colored)
rect.hclust(hierarchical_clustering, k = k, border = 'blue')

# Calculate average value for each cluster
cluster_means <- aggregate(sampled_data, by = list(Cluster = clusters), FUN = mean)
cluster_means

cluster_counts <- table(clusters)
cluster_counts
########

########
# Create a cluster with K set to 3
k <- 5
clusters <- cutree(hierarchical_clustering, k)

# Label settings
#labels <- round(sampled_org_data$price / 1000, digits = 2)#rownames(sampled_data)
labels <- paste(sampled_org_data$model_name, '/', sampled_org_data$year)


# Draw a dendrogram (show clusters)
plot(hierarchical_clustering, labels = labels)

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



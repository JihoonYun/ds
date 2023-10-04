# Required Libraries
library(cluster)
library(proxy)
library(NbClust)

# Load CSV 
scaled_org_data <- read.csv("used_cars_data_cleaned_final_ver_sampled.csv")
#scaled_data <- read.csv("used_cars_data_cleaned_final_ver_sampled_preprocessed.csv")

# Set seed for reproducibility
set.seed(2010)

# Number of rows to sample 
num_rows_to_sample <- 100

# Randomly sample 100 rows from the scaled_data dataframe
sampled_org_data <- scaled_org_data[sample(nrow(scaled_org_data), num_rows_to_sample, replace = FALSE), ]

# Extract the desired columns from scaled_org_data
sampled_data <- sampled_org_data[, c('horsepower', 'engine_cylinders', 'mileage', 'year', 'price')]
sampled_data2 <- sampled_org_data
# Convert 'has_accidents' column from strings to 1 for True and 0 for False, and then to numeric
# sampled_data$has_accidents <- as.numeric(sampled_data$has_accidents == "True")

# Scaling Data
sampled_data <- scale(sampled_data)
sampled_data

# Calculate average value for each cluster
cosine_distance <- proxy::dist(as.matrix(sampled_data), method = "cosine")

# Perform hierarchical clustering
hierarchical_clustering <- hclust(cosine_distance, method = "ward.D") 

########
# Create a cluster with K set to 3
k <-3
clusters <- cutree(hierarchical_clustering, k)

unique_breaks <- unique(c(min(sampled_data2$price), quantile(sampled_data2$price, c(0, 0.3, 0.7, 1)), max(sampled_data2$price)))
labels <- cut(
  sampled_data2$price,
  breaks = unique_breaks,
  labels = c("LOW PRICE", "MEDIUM PRICE", "HIGH PRICE"),
  include.lowest = TRUE
)

# Draw a dendrogram (show clusters)
plot(hierarchical_clustering, labels = labels, cex = 0.7)

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



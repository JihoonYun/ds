import pandas as pd
import numpy as np
from sklearn.decomposition import PCA
from sklearn.preprocessing import StandardScaler
from sklearn.cluster import KMeans
import seaborn as sns
import matplotlib.pyplot as plt
from sklearn.metrics import silhouette_score, silhouette_samples
import lux
from yellowbrick.cluster import KElbowVisualizer

# Set the path to the CSV file
# file_path = '/Users/jihoonyun/CUBoulder/R/workspace/used_cars_data_cleaned_final_ver_sampled_preprocessed.csv'
file_path = '/Users/jihoonyun/CUBoulder/R/workspace/used_cars_data_cleaned_final_ver_sampled.csv'

# Load the CSV file into a DataFrame
df = pd.read_csv(file_path)

# define a random state variable
R_STATE = 7

numeric_columns = ['horsepower', 'highway_fuel_economy', 'mileage', 'wheelbase', 'year', 'price']
boolean_columns = ['has_accidents']

# # Extract numeric columns
numeric_df = df[numeric_columns]
# # Extract boolean columns
boolean_df = df[boolean_columns]

# Create a new DataFrame by concatenating numeric and boolean DataFrames
cluster_df = pd.concat([numeric_df, boolean_df], axis=1)
# cluster_df = numeric_df

# # Convert boolean columns to numeric (True -> 1, False -> 0)
for column in boolean_columns:
    cluster_df[column] = cluster_df[column].astype(int)

# Define Max Cluster
max_clusters = 9

# # Create a scaler
scaler = StandardScaler()

# Apply scaling to all columns
df_scaled = pd.DataFrame(scaler.fit_transform(cluster_df), columns=cluster_df.columns)

# Print the DataFrame
print(df_scaled.head())  # Print the first 5 rows of the DataFrame
print(df_scaled.info())


for k in range(2, max_clusters):
    df_scaled_copy = df_scaled.copy()

    # Create a K-Means clustering model
    kmeans = KMeans(n_clusters=k, random_state=R_STATE)

    # Fit the model to the scaled data
    kmeans.fit(df_scaled_copy)

    # Get cluster labels for each data point
    cluster_labels = kmeans.labels_

    # Perform PCA for dimensionality reduction to 2D
    pca = PCA(n_components=2)
    reduced_data = pca.fit_transform(df_scaled_copy)

    # Add cluster labels to the reduced data DataFrame
    reduced_df = pd.DataFrame(reduced_data, columns=['PC1', 'PC2'])
    reduced_df['Cluster'] = cluster_labels
    df_scaled_copy['Cluster'] = cluster_labels

    # Calculate the mean of each column for each cluster
    cluster_means = df_scaled_copy.groupby('Cluster').mean()

    print(f'K = {k}')
    pd.set_option('display.max_columns', None)  # No limit to show the maximum columns
    print(cluster_means)
    print(cluster_means.to_html())

    # Calculate cluster centers in the reduced 2D space
    cluster_centers_2d = pca.transform(kmeans.cluster_centers_)

    plt.figure(dpi=300)
    plt.figure(figsize=(10, 6))


    # Plot the clustered data in 2D using seaborn
    sns.scatterplot(data=reduced_df, x='PC1', y='PC2', hue='Cluster', palette='Set1')
    plt.scatter(cluster_centers_2d[:, 0], cluster_centers_2d[:, 1], c='black', marker='X', s=100,
                label='Cluster Centers')


    plt.title('K-Means Clustering Results (2D) with Cluster Centers')
    plt.xlabel('Principal Component 1')
    plt.ylabel('Principal Component 2')
    plt.legend()
    plt.show()


################################################################################
# Elbow Point
################################################################################

# Use the Elbow Method to find the optimal number of clusters
visualizer = KElbowVisualizer(KMeans(n_clusters=2, random_state=R_STATE), k=(1, max_clusters), timings=False)
visualizer.fit(df_scaled)
visualizer.show()


################################################################################
# Silhouette Score
################################################################################

# max_clusters_ss = 7
# Calculate silhouette scores to find the optimal number of clusters
silhouette_scores = []
for k in range(2, max_clusters):
       kmeans = KMeans(n_clusters=k, random_state=R_STATE).fit(df_scaled)
       silhouette_avg = silhouette_score(df_scaled, kmeans.labels_)
       silhouette_scores.append(silhouette_avg)
       print(f'cluster: {k} // silhouette index: {silhouette_avg}')

# Display the Silhouette Score plot
plt.figure(dpi=300)
plt.figure(figsize=(8, 6))
plt.plot(range(2, max_clusters), silhouette_scores, marker='o', linestyle='-')
plt.xlabel('Number of Clusters (K)')
plt.ylabel('Silhouette Score')
plt.title('Silhouette Score for Optimal K')
plt.grid(True)
plt.show()


for n_clusters in range(2, max_clusters):
       # Create K-means model
       kmeans = KMeans(n_clusters=n_clusters, random_state=R_STATE)
       cluster_labels = kmeans.fit_predict(df_scaled)

       # Silhouette Score Calculation
       silhouette_avg = silhouette_score(df_scaled, cluster_labels)
       print(f"For n_clusters = {n_clusters}, the average silhouette score is {silhouette_avg:.2f}")

       # Calculate the silhouette score for each sample
       sample_silhouette_values = silhouette_samples(df_scaled, cluster_labels)

       plt.figure(figsize=(6, 4))
       y_lower = 10

       for i in range(n_clusters):
              ith_cluster_silhouette_values = sample_silhouette_values[cluster_labels == i]
              ith_cluster_silhouette_values.sort()

              size_cluster_i = ith_cluster_silhouette_values.shape[0]
              y_upper = y_lower + size_cluster_i

              color = plt.cm.get_cmap("Spectral")(float(i) / n_clusters)
              plt.fill_betweenx(np.arange(y_lower, y_upper), 0, ith_cluster_silhouette_values, facecolor=color,
                                edgecolor=color, alpha=0.7)

              plt.text(-0.05, y_lower + 0.5 * size_cluster_i, str(i))
              y_lower = y_upper + 10

       plt.title("Silhouette plot for various clusters")
       plt.xlabel("Silhouette coefficient values")
       plt.ylabel("Cluster label")

       # The vertical line for average silhouette score of all the values
       plt.axvline(x=silhouette_avg, color="red", linestyle="--")

       plt.show()

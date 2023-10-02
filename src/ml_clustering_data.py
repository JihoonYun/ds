import pandas as pd
from sklearn.preprocessing import StandardScaler

# Set the path to the CSV file
file_path = '/Users/jihoonyun/CUBoulder/R/workspace/used_cars_data_cleaned_final_ver.csv'
file_path2 = '/Users/jihoonyun/CUBoulder/R/workspace/used_cars_data_cleaned_final_ver_sampled.csv'

# Load the CSV file into a DataFrame
df = pd.read_csv(file_path)

# Print the DataFrame
print(df.head())  # Print the first 5 rows of the DataFrame

# define a random state variable
R_STATE = 99

print(f'Total Rows of df: {df.shape[0]}')
################################################################################
# Sampling
################################################################################
sampled_df = df.sample(frac=0.03, random_state=R_STATE)

print(f'Total Rows of sampled_df: {sampled_df.shape[0]}')

# Extract numeric columns
numeric_columns = ['horsepower', 'mileage', 'price', 'year', 'highway_fuel_economy', 'length', 'latitude', 'longitude']

# The new DataFrame
cluster_df = sampled_df[numeric_columns]
print("New DataFrame:")
print(cluster_df.info())

# Create a scaler
scaler = StandardScaler()

# Apply Min-Max scaling to all columns
df_scaled = pd.DataFrame(scaler.fit_transform(cluster_df), columns=cluster_df.columns)

# Display the scaled DataFrame
print("Scaled DataFrame:")
print(df_scaled.info())

df_scaled.to_csv(file_path2)


# Dataframe to HTML
df_10 = df.head(5)
df_10 = df_10.transpose()

df_10_html = df_10.to_html()
with open('df_10.html', 'w') as f:
    f.write(df_10_html)

df_scaled_10 = df_scaled.head(5)
df_scaled_10 = df_scaled_10.transpose()

df_scaled_10_html = df_scaled_10.to_html()
with open('df_scaled_10.html', 'w') as f:
    f.write(df_scaled_10_html)


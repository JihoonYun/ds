import pandas as pd
from sklearn.preprocessing import StandardScaler
from sklearn.preprocessing import MinMaxScaler

# Set the path to the CSV file
file_path = '/Users/jihoonyun/CUBoulder/R/workspace/used_cars_data_cleaned_final_ver.csv'
file_path2 = '/Users/jihoonyun/CUBoulder/R/workspace/used_cars_data_cleaned_final_ver_sampled.csv'
file_path3 = '/Users/jihoonyun/CUBoulder/R/workspace/used_cars_data_cleaned_final_ver_sampled_preprocessed.csv'

# Load the CSV file into a DataFrame
df = pd.read_csv(file_path)

# Print the DataFrame
print(df.head())  # Print the first 5 rows of the DataFrame

# define a random state variable
R_STATE = 7

print(f'Total Rows of df: {df.shape[0]}')
################################################################################
# Sampling
################################################################################
sampled_df = df.sample(n=500, random_state=R_STATE)
#sampled_df = df.sample(frac=0.01, random_state=R_STATE)

print(f'Total Rows of sampled_df: {sampled_df.shape[0]}')

sampled_df.to_csv(file_path2)

# Extract numeric columns
#       ['back_legroom', 'city_fuel_economy',
#        'daysonmarket', 'engine_cylinders', 'engine_displacement',
#        'front_legroom', 'fuel_tank_volume', 'height', 'highway_fuel_economy',
#        'horsepower', 'latitude', 'length', 'longitude',
#        'maximum_seating', 'mileage', 'owner_count', 'price', 'savings_amount',
#        'seller_rating', 'wheelbase', 'width', 'year', 'rpm',
#        'torque_lbft']

numeric_columns = ['horsepower', 'highway_fuel_economy', 'mileage', 'wheelbase', 'year', 'price']
numeric_df = sampled_df[numeric_columns]



# boolean_columns
# ['fleet', 'frame_damaged', 'franchise_dealer', 'has_accidents', 'isCab', 'is_new', 'salvage', 'theft_title']
boolean_columns = ['has_accidents']

# Extract boolean columns
boolean_df = sampled_df[boolean_columns]
boolean_df = boolean_df.astype(int)

# The new DataFrame
# cluster_df = pd.concat([numeric_df], axis=1)
cluster_df = pd.concat([numeric_df, boolean_df], axis=1)

print("New DataFrame:")
print(cluster_df.info())

# Create a scaler
scaler = StandardScaler()

# Apply Min-Max scaling to all columns
df_scaled = pd.DataFrame(scaler.fit_transform(cluster_df), columns=cluster_df.columns)

# Display the scaled DataFrame
print("Scaled DataFrame:")
print(df_scaled.info())

df_scaled.to_csv(file_path3)



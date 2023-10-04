import pandas as pd

# Set the path to the CSV file
file_path = '/Users/jihoonyun/CUBoulder/R/workspace/used_cars_data_cleaned_final_ver.csv'
file_path2 = '/Users/jihoonyun/CUBoulder/R/workspace/used_cars_data_cleaned_final_ver_sampled.csv'

# Load the CSV file into a DataFrame
df = pd.read_csv(file_path)

# Print the DataFrame
print(df.head())  # Print the first 5 rows of the DataFrame

# define a random state variable
R_STATE = 2

print(f'Total Rows of df: {df.shape[0]}')
################################################################################
# Sampling
################################################################################
sampled_df = df.sample(n=1000, random_state=R_STATE)
#sampled_df = df.sample(frac=0.01, random_state=R_STATE)

print(f'Total Rows of sampled_df: {sampled_df.shape[0]}')

sampled_df.to_csv(file_path2)

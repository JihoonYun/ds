##########################################################################################
# ml_rg_data.py
##########################################################################################

import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
from matplotlib.font_manager import FontProperties
from sklearn.preprocessing import StandardScaler
from statsmodels.graphics.gofplots import ProbPlot
import statsmodels.api as sm
from scipy.stats import boxcox
from statsmodels.stats.outliers_influence import variance_inflation_factor
from scipy.stats import norm

# Load CSV
df = pd.read_csv("used_cars_data_cleaned_final_ver.csv")

# Display structure of the dataframe
print(df.info())

# Set seed for reproducibility
np.random.seed(2010)

# Number of rows to sample
num_rows_to_sample = 30000

# Randomly sample 100 rows from the dataframe
df_sampled = df.sample(n=num_rows_to_sample, replace=False)
# Select only numeric columns
numeric_columns = df.select_dtypes(include=np.number).columns

# Calculate correlations
correlations = df[numeric_columns].corr()['price'].sort_values(ascending=False)

# Exclude 'price' from selected features
selected_features = correlations.abs().sort_values(ascending=False).drop('price')

# Select only the top 10 features
selected_features = selected_features.head(10)

# Plot bar chart with seaborn
plt.figure(figsize=(12, 7))
palette = ['grey' for _ in selected_features]  # Set all bars to grey
barplot = sns.barplot(x=selected_features.index, y=selected_features.values, palette=palette)
plt.title('Top 10 Features with Correlation to Price (|correlation| >= 0.3)')
plt.xlabel('Features')
plt.ylabel('Correlation')

# Adjust x-axis label rotation and alignment
plt.xticks(rotation=45, ha='right')

# Remove spines (borders) on the right and top
sns.despine()

# Add light grey horizontal grid lines
plt.grid(axis='y', linestyle='--', alpha=0.7, color='lightgrey')

# Add a black dashed line at y=0
plt.axhline(y=0, color='black', linestyle='-', linewidth=1)

# Make x-axis labels with correlation >= 0.4 bold
fontprops = FontProperties()
for label in barplot.get_xticklabels():
    if abs(selected_features[label.get_text()]) >= 0.3:
        label.set_fontweight('bold')

plt.tight_layout()  # Adjust layout to prevent cropping
plt.show()

# Select top 10 features
selected_features = correlations.abs().sort_values(ascending=False).drop('price').head(10)

# Extract top 10 features and 'price'
selected_columns = selected_features.index.tolist() + ['price']

# Create a new DataFrame with selected columns
df_sampled = df_sampled[selected_columns].copy()
# Copy the dataframe to avoid modifying the original
df_normalized = df_sampled.copy()

# Exclude 'price' column
features_to_normalize = df_normalized.drop('price', axis=1)

# Initialize the scaler
scaler = StandardScaler()

# Fit and transform the features
features_normalized = scaler.fit_transform(features_to_normalize)

# Update the dataframe with the normalized features
df_normalized[features_to_normalize.columns] = features_normalized


def remove_outliers_iqr(df, features):
    for feature in features:
        Q1 = df[feature].quantile(0.25)
        Q3 = df[feature].quantile(0.75)
        IQR = Q3 - Q1
        lower_bound = Q1 - 1.5 * IQR
        upper_bound = Q3 + 1.5 * IQR

        # Remove outliers
        df = df[(df[feature] >= lower_bound) & (df[feature] <= upper_bound)]

    return df

# Specify the features for which you want to remove outliers
outlier_features = selected_features.index.tolist()

# Apply the function to remove outliers from df_normalized
df_normalized = remove_outliers_iqr(df_normalized, outlier_features)

print('>>>')
print(df_normalized)

# Assuming df_normalized is your DataFrame
df_normalized['price'] = np.log(df_normalized['price'] + 1)  # Log transformation, adding 1 to handle zero values


# 1. Check Linearity
# Create a pairplot
pairplot = sns.pairplot(df_normalized, x_vars=selected_features.index, y_vars='price', height=4, aspect=1, kind='scatter')

# Add linear regression lines to each scatter plot
for i, feature in enumerate(selected_features.index):
    sns.regplot(x=df_normalized[feature], y=df_normalized['price'], ax=pairplot.axes[0, i], scatter=False, color='red')

plt.show()

# 2. Check Residuals Normality (Q-Q Plot)
X = df_normalized[selected_features.index]  # Replace with your selected features
X = sm.add_constant(X)
y = df_normalized['price']

model = sm.OLS(y, X).fit()
residuals = model.resid

qqplot = ProbPlot(residuals, fit=True)
qqplot.qqplot(line='45', alpha=0.7, color='blue', lw=1)
plt.title('Q-Q Plot of Residuals')
plt.show()


# 3. Check for Multicollinearity
def calculate_vif(data):
    vif_data = pd.DataFrame()
    vif_data["Variable"] = data.columns
    vif_data["VIF"] = [variance_inflation_factor(data.values, i) for i in range(data.shape[1])]
    return vif_data

vif_results = calculate_vif(df_normalized[selected_features.index])  # Use the calculate_vif function from the previous code
print("VIF Results:")
print(vif_results)


# Create a new DataFrame with selected columns
new_df = df_normalized[['horsepower', 'mileage', 'highway_fuel_economy', 'year', 'wheelbase', 'price']].copy()

# Display the new DataFrame
print(new_df)

# Save new_df to a CSV file
new_df.to_csv('used_cars_data_cleaned_final_ver_rg_sampled_preprocessed.csv', index=False)


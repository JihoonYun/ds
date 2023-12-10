import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import accuracy_score, classification_report, confusion_matrix
from sklearn.cluster import KMeans
from sklearn.preprocessing import LabelEncoder
from sklearn.decomposition import PCA
import warnings
import statsmodels.api as sm
from sklearn.preprocessing import StandardScaler

# Ignore warning
warnings.filterwarnings('ignore')

# Set options to display all rows and columns
pd.set_option('display.max_rows', None)
pd.set_option('display.max_columns', None)

# Set the path for the CSV file (assuming the file is in the current working directory)
file_path = 'customer_data.csv'

# Read the CSV file into a DataFrame
df = pd.read_csv(file_path)

# Display the DataFrame
print(df)


##################################################################
# EDA
##################################################################

# Display the first few rows of the DataFrame
print("Sample of the dataset:")
print(df.head())

# Summary statistics of numerical features
print("\nSummary statistics:")
print(df.describe())

# Count of unique values in each column
print("\nCount of unique values:")
print(df.nunique())

missing_values = df.isnull().sum()
print("Check Missing Value:\n", missing_values)

# Distribution of Age
plt.figure(figsize=(10, 6))
sns.histplot(df['Age'], bins=10, kde=True, color='skyblue')
plt.title('Distribution of Age')
plt.xlabel('Age')
plt.ylabel('Frequency')
plt.show()

# Count of Preferred Product Category
plt.figure(figsize=(10, 6))
sns.countplot(x='Preferred_Product_Category', data=df, palette='viridis')
plt.title('Count of Preferred Product Category')
plt.xlabel('Preferred Product Category')
plt.ylabel('Count')
plt.show()

# Distribution of Income
plt.figure(figsize=(10, 6))
sns.histplot(df['Income'], bins=10, kde=True, color='salmon')
plt.title('Distribution of Income')
plt.xlabel('Income')
plt.ylabel('Frequency')
plt.show()

# Count of Card Approval
plt.figure(figsize=(8, 5))
sns.countplot(x='Card_Approval', data=df, palette='pastel')
plt.title('Count of Card Approval')
plt.xlabel('Card Approval')
plt.ylabel('Count')
plt.show()




# Discretize Age and create a new variable 'Age_Group'
bins = [20, 30, 40, 50, 60, 70]
labels = ['20s', '30s', '40s', '50s', '60s']
df['Age_Group'] = pd.cut(df['Age'], bins=bins, labels=labels, right=False)

# Display the results
print(df[['Age', 'Age_Group']])

# Discretize Income into three categories: Low, Medium, High
income_percentiles = df['Income'].quantile([0.25, 0.75])
df['Income_Category'] = pd.cut(df['Income'], bins=[-float('inf'), income_percentiles.iloc[0], income_percentiles.iloc[1], float('inf')],
                                labels=['Low Income', 'Medium Income', 'High Income'], right=False)

# Display the results
print(df[['Income', 'Income_Category']])

# Card_Approval: True > 1, False > 0
df['Card_Approval'] = df['Card_Approval'].astype(int)

print(df)

##################################################################
# Card
##################################################################
# Create a new DataFrame with selected columns
df_card = df[['Age_Group', 'Income_Category', 'Card_Approval']]

# One-Hot Encode 'Age_Group' and 'Income_Category'
df_card_encoded = pd.get_dummies(df_card, columns=['Age_Group', 'Income_Category'], drop_first=True).astype(int)

# Display the result
print(df_card_encoded)

## Logistic Regression
# Extract features (X) and target variable (y)
X = df_card_encoded.drop('Card_Approval', axis=1)
y = df_card_encoded['Card_Approval']

# Split the data into training and testing sets (80% train, 20% test)
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=42)

# Create a logistic regression model
model = LogisticRegression(random_state=42)

# Train the model on the training set
model.fit(X_train, y_train)

# Make predictions on the test set
y_pred = model.predict(X_test)

# Evaluate the model
accuracy = accuracy_score(y_test, y_pred)
conf_matrix = confusion_matrix(y_test, y_pred)
classification_rep = classification_report(y_test, y_pred)

# Display the results
print(f'Accuracy: {accuracy:.2f}')
print('Confusion Matrix:')
print(conf_matrix)
print('Classification Report:')
print(classification_rep)

# Get the feature names
feature_names = X.columns

# Get the coefficients of the model
coefficients = model.coef_[0]

# Create a DataFrame to display feature names and their coefficients
coefficients_df = pd.DataFrame({'Feature': feature_names, 'Coefficient': coefficients})

# Display the DataFrame
print(coefficients_df)

##################################################################
# Product
##################################################################
# Create a new DataFrame with selected columns
df_product = df[['Age_Group', 'Preferred_Product_Category', 'Income_Category']]

# Label encoding is a method of mapping each category to a number.
# Label encoding is applied to the features of df_product
# because the relative sizes between categories are not important.

# <Age_Group>
# 20s 0
# 30s 1
# 40s 2
# 50s 3
# 60s 4

# <Preferred_Product_Category>
# Sports 4
# Books 0
# Electronics 2
# Home Decor 3
# Clothing 1
#
# <Income_Category>
# High Incom 0
# Medium Income 1
# Low Income 2

label_encoder = LabelEncoder()
df_product.loc[:, 'Age_Group']  = label_encoder.fit_transform(df_product['Age_Group'])
df_product.loc[:, 'Preferred_Product_Category'] = label_encoder.fit_transform(df_product['Preferred_Product_Category'])
df_product.loc[:, 'Income_Category'] = label_encoder.fit_transform(df_product['Income_Category'])

# Use the Elbow Method to find the optimal number of clusters (k)
inertia = []

for k in range(1, 11):
    kmeans = KMeans(n_clusters=k, random_state=42)
    kmeans.fit(df_product)
    inertia.append(kmeans.inertia_)

# Visualize the Elbow Method to find the optimal k
plt.plot(range(1, 11), inertia, marker='o', markersize=7)
plt.title('Elbow Method for Optimal k')
plt.xlabel('Number of Clusters (k)')
plt.ylabel('Inertia')
plt.show()


# Perform K-Means Clustering with k=3
k_value = 3
kmeans = KMeans(n_clusters=k_value, random_state=42)
df_product['Cluster'] = kmeans.fit_predict(df_product)

# Analyze and print cluster statistics
cluster_statistics = df_product.groupby('Cluster').mean()
print("Cluster Statistics:")
print(cluster_statistics)

# Count occurrences of each Age_Group category within each cluster
age_group_counts = df_product.groupby(['Cluster', 'Age_Group']).size().unstack(fill_value=0)
age_group_counts.to_csv('counts1.csv')
age_group_counts = df_product.groupby(['Cluster', 'Preferred_Product_Category']).size().unstack(fill_value=0)
age_group_counts.to_csv('counts2.csv')
age_group_counts = df_product.groupby(['Cluster', 'Income_Category']).size().unstack(fill_value=0)
age_group_counts.to_csv('counts3.csv')

# Visualize the clusters in the original feature space
fig, axes = plt.subplots(1, 3, figsize=(12, 5))

# Define markers for each cluster
markers = ['o', 's', '^', 'D']

handles = []

for cluster, marker in zip(range(k_value), markers):
    # Age Group vs. Preferred Product Category
    axes[0].scatter(df_product[df_product['Cluster'] == cluster]['Age_Group'], df_product[df_product['Cluster'] == cluster]['Preferred_Product_Category'], label=f'Cluster {cluster}', cmap='viridis', s=150, marker=marker)
    axes[0].set_title('Age Group vs. Preferred Product Category')

    # Preferred Product Category vs. Income Category
    axes[1].scatter(df_product[df_product['Cluster'] == cluster]['Preferred_Product_Category'], df_product[df_product['Cluster'] == cluster]['Income_Category'], label=f'Cluster {cluster}', cmap='viridis', s=150, marker=marker)
    axes[1].set_title('Preferred Product Category vs. Income Category')

    # Age Group vs. Preferred Product Category vs. Income Category
    scatter = axes[2].scatter(df_product[df_product['Cluster'] == cluster]['Age_Group'], df_product[df_product['Cluster'] == cluster]['Preferred_Product_Category'], label=f'Cluster {cluster}', cmap='viridis', s=150, marker=marker)
    axes[2].set_title('Age Group vs. Preferred Product Category vs. Income Category')

    # Append the scatter plot to handles
    handles.append(scatter)

# Add a legend
fig.legend(handles, labels=[f'Cluster {i}' for i in range(k_value)], loc='upper right')

plt.tight_layout()
plt.show()

print(df_product)


#
# # Apply PCA for dimensionality reduction
# pca = PCA(n_components=2, random_state=42)
# pca_result = pca.fit_transform(df_product.drop('Cluster', axis=1))
#
# # Add PCA components to the DataFrame
# df_product['PCA_Component1'] = pca_result[:, 0]
# df_product['PCA_Component2'] = pca_result[:, 1]
#
# # Visualize PCA components colored by clusters
# plt.figure(figsize=(8, 6))
# scatter = plt.scatter(df_product['PCA_Component1'], df_product['PCA_Component2'], c=df_product['Cluster'], cmap='viridis')
# plt.title('PCA Visualization of Clusters')
# plt.xlabel('PCA Component 1')
# plt.ylabel('PCA Component 2')
#
# # Add a legend
# legend_labels = [f'Cluster {i}' for i in range(k_value)]
# plt.legend(handles=scatter.legend_elements()[0], labels=legend_labels, loc='upper right')
#
# plt.show()

print(df_product)
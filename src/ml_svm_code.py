##########################################################################################
# ml_svm_code.py
##########################################################################################

import pandas as pd
import numpy as np
from sklearn import svm
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.svm import SVC
from sklearn.metrics import confusion_matrix, accuracy_score
from sklearn.decomposition import PCA
import matplotlib.pyplot as plt

# Load the CSV file into a DataFrame
df = pd.read_csv('used_cars_data_cleaned_final_ver_svm_sampled_preprocessed.csv')

##########################################################################################
# Find the optimal kernel type and cost
##########################################################################################
# Defining 'label' is the column to predict
y = df['label']
X = df.drop('label', axis=1)

# Split the data into training and testing sets
# The test_size parameter determines the proportion of the dataset to include in the test split
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=42)

# Standardize the data
scaler = StandardScaler()
X_train_std = scaler.fit_transform(X_train)
X_test_std = scaler.transform(X_test)

# Define different kernels and costs to try
kernels = ['rbf', 'linear', 'poly']
costs = list(range(1, 50, 1))  # [1, 10, 100]

# Create an empty DataFrame to store the results
results_df = pd.DataFrame(columns=['Kernel Type', 'Cost', 'Accuracy', 'Confusion Matrix'])

# Initialize variables to store the best accuracy and corresponding row
best_accuracies = {}
best_rows = {}

# Iterate over kernels and costs
for kernel in kernels:
    for cost in costs:
        # Create and train the SVM model
        model = SVC(kernel=kernel, C=cost)
        model.fit(X_train_std, y_train)

        # Make predictions on the test set
        y_pred = model.predict(X_test_std)

        # Evaluate the model
        accuracy = accuracy_score(y_test, y_pred)
        confusion_mat = confusion_matrix(y_test, y_pred)

        # Add results to the DataFrame
        results_df.loc[len(results_df)] = [kernel, cost, accuracy, confusion_mat]

        # Update best accuracy and corresponding row
        if kernel not in best_accuracies or accuracy > best_accuracies[kernel]:
            best_accuracies[kernel] = accuracy
            best_rows[kernel] = results_df.iloc[-1]

# Print the best rows for each Kernel Type
for kernel, row in best_rows.items():
    print("")
    print(f"Best Accuracy for {kernel}:")
    print(row)
    print("------------------------------")



##########################################################################################
# Find the optimal kernel type and cost
##########################################################################################
# Defining 'label' is the column to predict
y = df['label']
X = df.drop('label', axis=1)

# Apply PCA to reduce dimensionality to 2
pca = PCA(n_components=2)
X_pca = pca.fit_transform(X)

# Split the data into training and testing sets
# The test_size parameter determines the proportion of the dataset to include in the test split
X_train, X_test, y_train, y_test = train_test_split(X_pca, y, test_size=0.3, random_state=42)

# Standardize the data
scaler = StandardScaler()
X_train_std = scaler.fit_transform(X_train)
X_test_std = scaler.transform(X_test)

# Define different kernels and costs to try
kernels = ['rbf', 'linear', 'poly']
costs = list(range(1, 50, 1))  # [1, 10, 100]

# Create an empty DataFrame to store the results
results_pca_df = pd.DataFrame(columns=['Kernel Type', 'Cost', 'Accuracy', 'Confusion Matrix'])

# Initialize variables to store the best accuracy and corresponding row
best_pca_accuracies = {}
best_pca_rows = {}

# Iterate over kernels and costs
for kernel in kernels:
    for cost in costs:
        # Create and train the SVM model
        model = SVC(kernel=kernel, C=cost)
        model.fit(X_train_std, y_train)

        # Make predictions on the test set
        y_pred = model.predict(X_test_std)

        # Evaluate the model
        accuracy = accuracy_score(y_test, y_pred)
        confusion_mat = confusion_matrix(y_test, y_pred)

        # Add results to the DataFrame
        results_pca_df.loc[len(results_pca_df)] = [kernel, cost, accuracy, confusion_mat]

        # Update best accuracy and corresponding row
        if kernel not in best_pca_accuracies or accuracy > best_pca_accuracies[kernel]:
            best_pca_accuracies[kernel] = accuracy
            best_pca_rows[kernel] = results_pca_df.iloc[-1]

# Print the best rows for each Kernel Type
for kernel, row in best_pca_rows.items():
    print("")
    print("<PCA>-------------------------")
    print(f"Best Accuracy for {kernel}:")
    print(row)
    print("------------------------------")

##########################################################################################
# Visualization through dimensionality reduction at each optimal kernel and cost
##########################################################################################
# Apply PCA to reduce dimensionality to 2
pca = PCA(n_components=2)
X_pca = pca.fit_transform(X)

# Split the data into training and testing sets
# The test_size parameter determines the proportion of the dataset to include in the test split
X_pca_train, X_pca_test, y_pca_train, y_pca_test = train_test_split(X_pca, y, test_size=0.3, random_state=42)

# Standardize the data
scaler = StandardScaler()
X_pca_train_std = scaler.fit_transform(X_pca_train)
X_pca_test_std = scaler.transform(X_pca_test)

def svm_model_and_visualize(X_train_std, y_train, X_test_std, y_test, kernel='linear', C=1.0):
    # SVM Modeling
    clf = svm.SVC(kernel=kernel, C=C)
    clf.fit(X_train_std, y_train)

    # Make predictions on the test set
    y_pred = clf.predict(X_test_std)

    # Evaluate the model
    accuracy = accuracy_score(y_test, y_pred)
    confusion_mat = confusion_matrix(y_test, y_pred)

    # Print results
    print("Confusion Matrix:")
    print(confusion_mat)
    print(f"Accuracy: {accuracy:.4f}")
    print("------------------------------")

    # Visualize the decision boundary
    h = .02  # Step size in the mesh
    x_min, x_max = X_train_std[:, 0].min() - 1, X_train_std[:, 0].max() + 1
    y_min, y_max = X_train_std[:, 1].min() - 1, X_train_std[:, 1].max() + 1
    xx, yy = np.meshgrid(np.arange(x_min, x_max, h), np.arange(y_min, y_max, h))

    Z = clf.predict(np.c_[xx.ravel(), yy.ravel()])
    Z = Z.reshape(xx.shape)

    plt.contourf(xx, yy, Z, cmap=plt.cm.coolwarm, alpha=0.8)
    plt.scatter(X_train_std[:, 0], X_train_std[:, 1], c=y_train, cmap=plt.cm.coolwarm, edgecolors='k', marker='o', s=80, label='Training Points')

    plt.suptitle('SVM Decision Boundary with PCA', fontsize=14, fontweight='bold')
    plt.title(f'Kernel Type: {kernel}, Cost: {C}, Accuracy: {accuracy})', fontsize=12)
    plt.xlabel('Principal Component 1')
    plt.ylabel('Principal Component 2')
    plt.legend()
    plt.show()

svm_model_and_visualize(X_pca_train_std, y_pca_train, X_pca_test_std, y_pca_test, kernel='linear', C=1.0)
svm_model_and_visualize(X_pca_train_std, y_pca_train, X_pca_test_std, y_pca_test, kernel='poly', C=12)
svm_model_and_visualize(X_pca_train_std, y_pca_train, X_pca_test_std, y_pca_test, kernel='rbf', C=13.0)

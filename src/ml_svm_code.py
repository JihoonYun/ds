##########################################################################################
# ml_svm_code.py
##########################################################################################

import pandas as pd
import numpy as np
from sklearn import svm
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.svm import SVC
from sklearn.metrics import confusion_matrix, accuracy_score, precision_score, recall_score, f1_score
from sklearn.decomposition import PCA
import matplotlib.pyplot as plt
import seaborn as sns

# Load the CSV file into a DataFrame
df = pd.read_csv('used_cars_data_cleaned_final_ver_svm_sampled_preprocessed.csv')

pd.set_option('display.max_rows', None)
pd.set_option('display.max_columns', None)
np.set_printoptions(threshold=np.inf, precision=4, suppress=True)

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


# Result plots
# Split the data for each kernel type
rbf_data = results_df[results_df['Kernel Type'] == 'rbf']
linear_data = results_df[results_df['Kernel Type'] == 'linear']
poly_data = results_df[results_df['Kernel Type'] == 'poly']

# Find the data point with the highest accuracy for each kernel type
best_rbf = rbf_data.loc[rbf_data['Accuracy'].idxmax()]
best_linear = linear_data.loc[linear_data['Accuracy'].idxmax()]
best_poly = poly_data.loc[poly_data['Accuracy'].idxmax()]

# Plot accuracy based on cost for each kernel type
plt.figure(figsize=(12, 8))

# Linear kernel
plt.subplot(3, 1, 1)
sns.lineplot(x='Cost', y='Accuracy', data=linear_data, label='Linear')
plt.scatter(best_linear['Cost'], best_linear['Accuracy'], color='red')
plt.axvline(x=best_linear['Cost'], linestyle='--', color='gray')
plt.text(best_linear['Cost'] - 0.1, best_linear['Accuracy'] - 0.01, f"({best_linear['Cost']}, {best_linear['Accuracy']:.4f})", ha='left', va='top')
plt.title('Linear Kernel - Accuracy vs Cost')
plt.xlabel('Cost')
plt.ylabel('Accuracy')
plt.legend()

# Poly kernel
plt.subplot(3, 1, 2)
sns.lineplot(x='Cost', y='Accuracy', data=poly_data, label='Poly')
plt.scatter(best_poly['Cost'], best_poly['Accuracy'], color='red')
plt.axvline(x=best_poly['Cost'], linestyle='--', color='gray')
plt.text(best_poly['Cost'] - 0.1, best_poly['Accuracy'] - 0.01, f"({best_poly['Cost']}, {best_poly['Accuracy']:.4f})", ha='left', va='top')
plt.title('Poly Kernel - Accuracy vs Cost')
plt.xlabel('Cost')
plt.ylabel('Accuracy')
plt.legend()

# RBF kernel
plt.subplot(3, 1, 3)
sns.lineplot(x='Cost', y='Accuracy', data=rbf_data, label='RBF')
plt.scatter(best_rbf['Cost'], best_rbf['Accuracy'], color='red')
plt.axvline(x=best_rbf['Cost'], linestyle='--', color='gray')
plt.text(best_rbf['Cost'] - 0.1, best_rbf['Accuracy'] - 0.01, f"({best_rbf['Cost']}, {best_rbf['Accuracy']:.4f})", ha='left', va='top')
plt.title('RBF Kernel - Accuracy vs Cost')
plt.xlabel('Cost')
plt.ylabel('Accuracy')
plt.legend()

plt.tight_layout()
plt.show()

##########################################################################################
# Find the support vector, Margin, Feature Weights
##########################################################################################

def train_svm(X, y, kernel='linear', C=1.0):
    """
    Train SVM model and return the trained model.

    Parameters:
    - X: Feature matrix
    - y: Target vector
    - kernel: Kernel type ('linear', 'poly', 'rbf', etc.)
    - C: Regularization parameter

    Returns:
    - trained_svm_model: Trained SVM model
    """
    clf = svm.SVC(kernel=kernel, C=C)
    clf.fit(X, y)
    return clf

def get_svm_info(trained_svm_model):
    """
    Get information about the SVM model.

    Parameters:
    - trained_svm_model: Trained SVM model

    Returns:
    - support_vectors: Support vectors in feature space
    - margin: Margin of the SVM model
    - weights: Weights assigned to each feature
    """
    support_vectors = trained_svm_model.support_vectors_
    margin = 2 / np.sqrt(np.sum(trained_svm_model.coef_ ** 2))
    weights = trained_svm_model.coef_.flatten()

    feature_names = X.columns
    feature_weights = list(zip(feature_names, weights))
    feature_weights.sort(key=lambda x: abs(x[1]), reverse=True)  # Sort in descending order

    # Visualize feature weights using seaborn barplot
    plot_feature_weights(feature_names, weights)

    return support_vectors, margin, weights, feature_names

def plot_feature_weights(feature_names, feature_weights):
    """
    Visualize the relationship between feature names and their weights using a barplot.

    Parameters:
    - feature_names: List of feature names
    - feature_weights: List of feature weights

    Returns:
    - None (displays the plot)
    """
    # Sort feature_weights in descending order
    sorted_feature_weights = sorted(zip(feature_names, feature_weights), key=lambda x: abs(x[1]), reverse=True)

    # Extract feature names and weights
    sorted_feature_names, sorted_weights = zip(*sorted_feature_weights)

    # Convert boolean values to strings for visualization
    sorted_feature_names = [str(feature) for feature in sorted_feature_names]

    # Convert weights to numeric type
    sorted_weights = [float(weight) for weight in sorted_weights]

    # Seaborn barplot configuration
    sns.set(style="whitegrid")
    plt.figure(figsize=(12, 8))

    # Plot all features in gray
    plot = sns.barplot(x=sorted_feature_names, y=sorted_weights, color="darkgray")
    print(sorted_feature_names)
    print(sorted_weights)
    # Highlight features with different colors
    for feature, weight in sorted_feature_weights:
        color = "darkred" if weight > 0 else "darkblue"
        index = sorted_feature_names.index(str(feature))
        plot.patches[index].set_facecolor(color)

    # Rotate x-axis labels for better readability
    plot.set_xticklabels(plot.get_xticklabels(), rotation=45, ha="right")

    plt.subplots_adjust(bottom=0.2)
    # Display the plot
    plt.title("Feature Weights")
    plt.show()




trained_svm = train_svm(X_train_std, y_train, kernel='linear', C=1.0)
support_vectors, margin, weights, feature_names = get_svm_info(trained_svm)

print("Support Vectors:", support_vectors)
print("Margin:", margin)
print("Feature Weights:", weights)
print("Feature Names:", feature_names)




def train_and_get_accuracy(kernel, cost, degree):
    """
    Train SVM with given parameters and return accuracy.

    Parameters:
    - kernel: Type of SVM kernel ('poly' for polynomial)
    - cost: Cost parameter for SVM
    - degree: Degree of the polynomial kernel

    Returns:
    - accuracy: Accuracy of the trained SVM model
    """

    # Create SVM model
    if kernel == 'poly':
        svm_model = SVC(kernel='poly', C=cost, degree=degree, gamma='auto')
    else:
        # Add conditions for other kernel types if needed
        raise ValueError(f"Unsupported kernel type: {kernel}")

    # Train the SVM model
    svm_model.fit(X_train_std, y_train)

    # Make predictions on the test set
    y_pred = svm_model.predict(X_test_std)

    # Calculate accuracy
    accuracy = accuracy_score(y_test, y_pred)

    return accuracy

def plot_poly_kernel_accuracy(cost=25, degree_range=(2, 10)):
    """
    Plot accuracy for polynomial kernel with varying degrees.

    Parameters:
    - cost: Cost parameter for SVM
    - degree_range: Range of polynomial degrees to explore (e.g., (2, 10))
    """
    degrees = list(range(degree_range[0], degree_range[1] + 1))
    accuracies = []

    for degree in degrees:
        # Assuming you have a function to train SVM with a specific degree
        # and return the accuracy, replace the following line with your code
        accuracy = train_and_get_accuracy(kernel='poly', cost=cost, degree=degree)

        accuracies.append(accuracy)

    # Create a DataFrame for seaborn plotting
    data = {'Degree': degrees, 'Accuracy': accuracies}
    df = pd.DataFrame(data)

    # Plotting with seaborn
    plt.figure(figsize=(10, 6))
    sns.lineplot(x='Degree', y='Accuracy', data=df, marker='o', color='b')

    # Set plot labels and title
    plt.title(f'Accuracy for Polynomial Kernel (Cost={cost})')
    plt.xlabel('Polynomial Degree')
    plt.ylabel('Accuracy')

    plt.show()

plot_poly_kernel_accuracy(cost=25, degree_range=(2, 10))

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


print(results_pca_df)

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

    # Plot decision boundary with different marker shapes for each class
    plt.contourf(xx, yy, Z, cmap=plt.cm.coolwarm, alpha=0.8)
    for i, (marker, color) in zip(np.unique(y_train), [('o', 'blue'), ('s', 'white'), ('^', 'red')]):
        indices = np.where(y_train == i)
        plt.scatter(X_train_std[indices, 0], X_train_std[indices, 1], marker=marker, s=25, edgecolors='k', label=f'Class {i}', c=color)

    plt.suptitle('SVM Decision Boundary with PCA', fontsize=14, fontweight='bold')
    plt.title(f'Kernel Type: {kernel}, Cost: {C}, Accuracy: {accuracy:.4f}', fontsize=12)
    plt.xlabel('Principal Component 1')
    plt.ylabel('Principal Component 2')
    plt.legend()
    plt.show()

svm_model_and_visualize(X_pca_train_std, y_pca_train, X_pca_test_std, y_pca_test, kernel='linear', C=1.0)
svm_model_and_visualize(X_pca_train_std, y_pca_train, X_pca_test_std, y_pca_test, kernel='poly', C=12)
svm_model_and_visualize(X_pca_train_std, y_pca_train, X_pca_test_std, y_pca_test, kernel='rbf', C=13.0)

import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error, mean_absolute_error, r2_score
from scipy.stats import shapiro, probplot

# Load CSV
df = pd.read_csv("used_cars_data_cleaned_final_ver_rg_sampled_preprocessed.csv")

# Split the dataset into features (X) and target variable (y)
X = df.drop('price', axis=1)  # Features (independent variables)
y = df['price']  # Target variable (dependent variable)

# Split the data into training and testing sets (80% training, 20% testing)
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

# Fit a multiple linear regression model
model = LinearRegression()
model.fit(X_train, y_train)

# Make predictions on the test set
y_pred = model.predict(X_test)

# Evaluate the model
mse = mean_squared_error(y_test, y_pred)
print(f"Mean Squared Error: {mse}")

# Evaluate the model with Mean Absolute Error (MAE)
mae = mean_absolute_error(y_test, y_pred)
print(f"Mean Absolute Error: {mae}")

# Evaluate the model with R-squared
r_squared = r2_score(y_test, y_pred)
print(f"R-squared: {r_squared}")

# Check the assumptions of linear regression
# Residuals normality check using Shapiro-Wilk test
residuals = y_test - y_pred
shapiro_test_stat, shapiro_p_value = shapiro(residuals)
print(f"Shapiro-Wilk Test Statistic: {shapiro_test_stat}")
print(f"p-value: {shapiro_p_value}")

# Residuals vs Fitted Values Plot
sns.set(style="whitegrid")
sns.residplot(x=y_pred, y=residuals, lowess=True, line_kws={'color': 'red'}, scatter_kws={'alpha': 0.5})
plt.xlabel('Fitted Values')
plt.ylabel('Residuals')
plt.title('Residuals vs Fitted Values Plot')
plt.show()


# Numerical Analysis
# Mean of Residuals
mean_residual = residuals.mean()
print(f"Mean of Residuals: {mean_residual}")

# Standard Deviation of Residuals
std_residual = residuals.std()
print(f"Standard Deviation of Residuals: {std_residual}")


# Q-Q Plot (Quantile-Quantile Plot)
probplot(residuals, dist="norm", plot=plt)
plt.title("Q-Q Plot")
plt.show()

# Visualize the linear regression model
plt.scatter(y_test, y_pred, alpha=0.5, label='Actual vs Predicted')
plt.plot([min(y_test), max(y_test)], [min(y_test), max(y_test)], color='red', linestyle='--', linewidth=2, label='Regression Line')
plt.xlabel('Actual Prices')
plt.ylabel('Predicted Prices')
plt.title('Actual Prices vs Predicted Prices with Regression Line')
plt.legend()
plt.show()

# Print the coefficients of the linear regression model
coefficients = model.coef_
intercept = model.intercept_
print("Intercept:", intercept)
print("Coefficients for each independent variable:")
for feature, coef in zip(X.columns, coefficients):
    print(f"{feature}: {coef}")

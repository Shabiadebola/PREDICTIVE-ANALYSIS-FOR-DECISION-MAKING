
# Install and load required packages
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
if (!requireNamespace("lmtest", quietly = TRUE)) install.packages("lmtest")
if (!requireNamespace("car", quietly = TRUE)) install.packages("car")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("sandwich", quietly = TRUE)) install.packages("sandwich")
if (!requireNamespace("tsDyn", quietly = TRUE)) install.packages("tsDyn")
if (!requireNamespace("stats", quietly = TRUE)) install.packages("stats")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("GGally", quietly = TRUE)) install.packages("GGally")
if (!requireNamespace("gridExtra", quietly = TRUE)) install.packages("gridExtra")
if (!requireNamespace("margins", quietly = TRUE)) install.packages("margins")
if (!requireNamespace("MASS", quietly = TRUE)) install.packages("MASS")
if (!requireNamespace("AER", quietly = TRUE)) install.packages("AER")
if (!requireNamespace("stargazer", quietly = TRUE)) install.packages("stargazer")



library(readxl)
library(lmtest)
library(car)
library(tsDyn)
library(sandwich)
library(stats)
library(ggplot2)
library(dplyr)
library(GGally)  
library(gridExtra) 
library(margins)
library(MASS)
library(AER)
library(stargazer)

# Load the data
data <- read_excel("nls80.xlsx")

summary(data)

# QUESTION 1
# QUESTION 1A

# Display summary statistics of continuous variables
continuous_vars <- c("wage", "hours", "iq", "kww", "educ", "exper", 
                     "tenure", "age", "sibs", "brthord", "meduc", "feduc", "lwage")
summary(data[, continuous_vars])

# Visualizing distributions
p1 <- ggplot(data, aes(x = lwage)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Log Wages", x = "lwage", y = "Count") +
  theme_minimal()

p2 <- ggplot(data, aes(x = educ)) +
  geom_histogram(binwidth = 0.5, fill = "pink", color = "black") +
  labs(title = "Distribution of Education", x = "EDUCATION", y = "Count") +
  theme_minimal()

p3 <- ggplot(data, aes(x = exper)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
  labs(title = "Years of Experience", x = "Experience", y = "Count") +
  theme_minimal()

grid.arrange(p1, p2, p3, nrow = 1)  # Arrange plots in one row

ggplot(data, aes(y = lwage)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Boxplot of lwages", y = "lWage") +
  theme_minimal()

# Correlation matrix of continuous variables
cor(data[, c("lwage", "educ", "exper", "tenure", "age", "iq")])


# Question 1B

# Fit the multiple linear regression model
model <- lm(lwage ~ educ + exper + tenure + iq + hours, data = data)
# Display model summary
summary(model)

# Question 1D
#Test for heteroskedasticity
bptest(model)

# Using Robust standard errors to account for heteroskedasticity
coeftest(model, vcov = vcovHC(model, type = "HC1"))

#Test for Multicollinearity
# Compute VIF
vif(lm(lwage ~ educ + exper + tenure + iq + hours, data = data))


# QUESTION 2


numericvars <- sapply(data, is.numeric)
numericvars

data$meduc <- as.numeric(levels(factor(data$meduc)))[as.integer(factor(data$meduc))]
data$brthord <- as.numeric(levels(factor(data$brthord)))[as.integer(factor(data$brthord))]
data$feduc <- as.numeric(levels(factor(data$feduc)))[as.integer(factor(data$feduc))]


data <- na.omit(data)   # remove missing values

str(data)
# Question 2a

# Create a binary variable for university education (1 = University Degree, 0 = Otherwise)
data <- data %>%
  mutate(university_edu = ifelse(educ >= 16, 1, 0)) 

# Check distribution
table(data$university_edu)

#Question 2b
# Logit model using GLM
logit_model <- glm(university_edu ~ age + iq + meduc + feduc + sibs + urban, 
                   family = binomial(link = "logit"), data = data)
summary(logit_model)

# Fit Probit Model
probit_model <- glm(university_edu ~ age + iq + meduc + feduc + sibs + urban,
                    data = data, family = binomial(link = "probit"))

summary(probit_model)

#Question 2d

# Marginal effects
margins_logit <- margins(logit_model)
summary(margins_logit)

margins_probit <- margins(probit_model)
summary(margins_probit)

# Plot estimated probabilities
data$pred_logit <- predict(logit_model, type = "response")
data$pred_probit <- predict(probit_model, type = "response")


p1 <- ggplot(data, aes(x = iq, y = pred_logit)) +
  geom_point(aes(color = iq), alpha = 0.5, size = 2) +  # Points with color based on IQ and adjusted size
  geom_smooth(method = "loess", se = FALSE, color = "blue", size = 1) +  # Smoothed line in blue with no confidence interval
  scale_color_gradient(low = "lightblue", high = "darkblue") +  # Gradient color for IQ values
  labs(title = "Estimated Probability of University Educ by IQ",
       subtitle = "Using Logit Model Predictions",
       x = "IQ", y = "Probability of University Education") +
  theme_minimal()


p2 <- ggplot(data, aes(x = iq, y = pred_probit)) +
  geom_point(aes(color = iq), alpha = 0.5, size = 2) +  # Points with color based on IQ and adjusted size
  geom_smooth(method = "loess", se = FALSE, color = "blue", size = 1) +  # Smoothed line in blue with no confidence interval
  scale_color_gradient(low = "lightblue", high = "darkblue") +  # Gradient color for IQ values
  labs(title = " Estimated Probability of University Educ by IQ",
       subtitle = "Using probit Model Predictions",
       x = "IQ", y = "Probability of University Education") +
  theme_minimal()

grid.arrange(p1, p2, ncol = 2)

#Question 2e
# Compare AIC values between Logit and Probit Models
AIC(logit_model, probit_model)

# Plot Observed vs. Predicted Probabilities (Logit vs. Probit)
ggplot(data, aes(x = university_edu)) +
  geom_density(aes(color = "Observed"), alpha = 0.5, size = 1.2) +
  geom_density(aes(x = pred_logit, color = "Logit"), alpha = 0.5, size = 1.2) +
  geom_density(aes(x = pred_probit, color = "Probit"), alpha = 0.5, size = 1.2) +
  labs(title = "Observed vs. Predicted Probability of University Education",
       x = "University Education (Binary Outcome)", y = "Density") +
  scale_color_manual(name = "Model",
                     values = c("Observed" = "black", "Logit" = "red", "Probit" = "blue")) +
  xlim(0, 1) +  # Adjust to ensure visibility of all probabilities
  theme_minimal()

#QUESTION 3
#QUESTION 3A
# Set seed for reproducibility
set.seed(123)

# True parameter values (adjusted for more realistic scale)
beta0 <- 500    # Intercept
beta1 <- 50  # Education (endogenous)
beta2 <- 20 # Experience (exogenous)

# Number of observations and simulations
n <- 1000
n_sims <- 1000

# Function to generate data
generate_data <- function(n) {
  # Generate exogenous variables
  exper <- runif(n, 0, 20)  # Experience
  
  # Generate endogenous education correlated with error term
  v <- rnorm(n, 0, 5)  
 educ <- rnorm(n, 12, 2) + v  
 
  # Generate error term correlated with education
  u <- rnorm(n, 0, 1) + 5*v
  
  # Generate lwage (log-wage) dependent variabl
  lwage <- beta0 + beta1 * educ + beta2 * exper + u 
  
  # Return the data as a data frame
  data.frame(lwage, educ, exper)
  
}

# Generate the dataset
data <- generate_data(n)

# Display the first few rows
head(data)

# Check the correlation structure
cor(data)

#QUESTION 3B
# Number of observations and simulations
n <- 1000
n_sims <- 1000

# Initialize storage for coefficients
ols_results <- matrix(NA, n_sims, 2)

# Run simulations
for (i in 1:n_sims) {
  exper <- runif(n, 0, 20)  
  v <- rnorm(n, 0, 5)  
  educ <- rnorm(n, 12, 2) + v  
  u <- rnorm(n, 0, 1) + 5 * v  
  lwage <- beta0 + beta1 * educ + beta2 * exper + u  
  
  ols_results[i, ] <- coef(lm(lwage ~ educ + exper))[2:3]  
}

# Display results
head(ols_results)
summary(ols_results)


# Plot histograms for coefficient estimates
par(mfrow = c(1, 2))  # Arrange plots side by side

hist(ols_results[, 1], main = "Distribution of Education Coefficients",
     xlab = "Coefficient Value", col = "lightblue", breaks = 30)

hist(ols_results[, 2], main = "Distribution of Experience Coefficients",
     xlab = "Coefficient Value", col = "lightgreen", breaks = 30)

#QUESTION 3C
# True parameter values
beta1_true <- 50  # True value of beta for "educ"
beta2_true <- 20  # True value of beta for "exper"

# Calculate the bias
bias_educ <- mean(ols_results[, 1]) - beta1_true # Bias for "educ" coefficient
bias_exper <- mean(ols_results[, 2]) - beta2_true  # Bias for "exper" coefficient 

# Display bias
cat("Bias for 'educ' coefficient:", bias_educ, "\n")
cat("Bias for 'exper' coefficient:", bias_exper, "\n")

# Plot histogram for education coefficient estimates
hist(ols_results[, 1], main = "Distribution of Education Coefficients",
     xlab = "Coefficient Value", col = "lightblue", breaks = 30)
abline(v = beta1_true, col = "red", lwd = 2, lty = 2)  # True value line for educ

# Plot histogram for experience coefficient estimates
hist(ols_results[, 2], main = "Distribution of Experience Coefficients",
     xlab = "Coefficient Value", col = "lightgreen", breaks = 30)
abline(v = beta2_true, col = "red", lwd = 2, lty = 2)  # True value line for exper



#QUESTION 3D
# Keep previous parameters
# Initialize matrices
ols_results <- matrix(NA, n_sims, 2)
tsls_results <- matrix(NA, n_sims, 2)

# Run simulations
for (i in 1:n_sims) {
  # Generate data
  sibs <- rpois(n, 2)          # Instrumental variable: Average of 2 siblings
  exper <- runif(n, 0, 20)               
  v <- rnorm(n, 0, 5)                    
  educ <- 12 - 0.5 * sibs + v            
  u <- rnorm(n, 0, 1) + 5 * v     
  lwage <- beta0 + beta1 * educ + beta2 * exper + u
  data <- data.frame(lwage, educ, exper, sibs)
  # OLS Estimation
  ols_results[i, ] <- coef(lm(lwage ~ educ + exper, data))[2:3]
  
  # 2SLS Estimation
  tsls_results[i, ] <- coef(ivreg(lwage ~ educ + exper | sibs + exper, data = data))[2:3]
}

# Calculate means, bias, and variance
means_ols <- colMeans(ols_results); means_tsls <- colMeans(tsls_results)
bias_ols <- means_ols - c(beta1, beta2); bias_tsls <- means_tsls - c(beta1, beta2)
var_ols <- apply(ols_results, 2, var); var_tsls <- apply(tsls_results, 2, var)

# Create results table
results_table <- data.frame(
  Estimator = rep(c("OLS", "2SLS"), each = 2),
  Coefficient = rep(c("Education", "Experience"), 2),
  True_Value = c(beta1, beta2, beta1, beta2),
  Estimate = c(means_ols, means_tsls),
  Bias = c(bias_ols, bias_tsls),
  Variance = c(var_ols, var_tsls)
)

# Print results
print(results_table, digits = 4)

# Visualize distributions of estimates using boxplots
par(mfrow = c(2, 2))  # Set up the plotting window for 4 plots
for (i in 1:2) {
  plot_data <- data.frame(
    OLS = ols_results[, i],
    TSLS = tsls_results[, i]
  )
  
  plot_title <- c("Education", "Experience")[i]
  boxplot(plot_data, main = plot_title, ylab = "Coefficient Estimate")
  abline(h = c(beta1, beta2)[i], col = "red", lwd = 2)
}

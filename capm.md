
# CAPM Analysis

## Introduction

In this assignment, you will explore the foundational concepts of the Capital Asset Pricing Model (CAPM) using historical data for AMD and the S&P 500 index. This exercise is designed to provide a hands-on approach to understanding how these models are used in financial analysis to assess investment risks and returns.

## Background

The CAPM provides a framework to understand the relationship between systematic risk and expected return, especially for stocks. This model is critical for determining the theoretically appropriate required rate of return of an asset, assisting in decisions about adding assets to a diversified portfolio.

## Objectives

1. **Load and Prepare Data:** Import and prepare historical price data for AMD and the S&P 500 to ensure it is ready for detailed analysis.
2. **CAPM Implementation:** Focus will be placed on applying the CAPM to examine the relationship between AMD's stock performance and the overall market as represented by the S&P 500.
3. **Beta Estimation and Analysis:** Calculate the beta of AMD, which measures its volatility relative to the market, providing insights into its systematic risk.
4. **Results Interpretation:** Analyze the outcomes of the CAPM application, discussing the implications of AMD's beta in terms of investment risk and potential returns.

## Instructions

### Step 1: Data Loading

- We are using the `quantmod` package to directly load financial data from Yahoo Finance without the need to manually download and read from a CSV file.
- `quantmod` stands for "Quantitative Financial Modelling Framework". It was developed to aid the quantitative trader in the development, testing, and deployment of statistically based trading models.
- Make sure to install the `quantmod` package by running `install.packages("quantmod")` in the R console before proceeding.

```{r load-data}
# Set start and end dates
start_date <- as.Date("2019-05-20")
end_date <- as.Date("2024-05-20")

# Load data for AMD, S&P 500, and the 1-month T-Bill (DTB4WK)
amd_data <- getSymbols("AMD", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
gspc_data <- getSymbols("^GSPC", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
rf_data <- getSymbols("DTB4WK", src = "FRED", from = start_date, to = end_date, auto.assign = FALSE)

# Convert Adjusted Closing Prices and DTB4WK to data frames
amd_df <- data.frame(Date = index(amd_data), AMD = as.numeric(Cl(amd_data)))
gspc_df <- data.frame(Date = index(gspc_data), GSPC = as.numeric(Cl(gspc_data)))
rf_df <- data.frame(Date = index(rf_data), RF = as.numeric(rf_data[,1]))  # Accessing the first column of rf_data

# Merge the AMD, GSPC, and RF data frames on the Date column
df <- merge(amd_df, gspc_df, by = "Date")
df <- merge(df, rf_df, by = "Date")
```

#### Data Processing 
```{r data}
colSums(is.na(df))
# Fill N/A RF data
df <- df %>%
  fill(RF, .direction = "down") 
```

### Step 2: CAPM Analysis

The Capital Asset Pricing Model (CAPM) is a financial model that describes the relationship between systematic risk and expected return for assets, particularly stocks. It is widely used to determine a theoretically appropriate required rate of return of an asset, to make decisions about adding assets to a well-diversified portfolio.

#### The CAPM Formula
The formula for CAPM is given by:

\[ E(R_i) = R_f + \beta_i (E(R_m) - R_f) \]

Where:

- \( E(R_i) \) is the expected return on the capital asset,
- \( R_f \) is the risk-free rate,
- \( \beta_i \) is the beta of the security, which represents the systematic risk of the security,
- \( E(R_m) \) is the expected return of the market.



#### CAPM Model Daily Estimation

- **Calculate Returns**: First, we calculate the daily returns for AMD and the S&P 500 from their adjusted closing prices. This should be done by dividing the difference in prices between two consecutive days by the price at the beginning of the period.
$$
\text{Daily Return} = \frac{\text{Today's Price} - \text{Previous Trading Day's Price}}{\text{Previous Trading Day's Price}}
$$

```{r return}
# Calculate daily returns for AMD using given formula
df$AMD_Return <- (df$AMD / lag(df$AMD) - 1)

# Calculate daily returns for S&P 500 using given formula
df$GSPC_Return <- (df$GSPC / lag(df$GSPC) - 1)


```

- **Calculate Risk-Free Rate**: Calculate the daily risk-free rate by conversion of annual risk-free Rate. This conversion accounts for the compounding effect over the days of the year and is calculated using the formula:
$$
\text{Daily Risk-Free Rate} = \left(1 + \frac{\text{Annual Rate}}{100}\right)^{\frac{1}{360}} - 1
$$

```{r riskfree}
#Calculate Daily Risk-Free Rate using given formula
df$Daily_Risk_Free_Rate <- (1+df$RF/100)^(1/360)-1

```


- **Calculate Excess Returns**: Compute the excess returns for AMD and the S&P 500 by subtracting the daily risk-free rate from their respective returns.

```{r excess return}
#Calculate excess returns
df$AMD_Excess_Return <- df$AMD_Return - df$Daily_Risk_Free_Rate
df$GSPC_Excess_Return <- df$GSPC_Return - df$Daily_Risk_Free_Rate

```


- **Perform Regression Analysis**: Using linear regression, we estimate the beta (\(\beta\)) of AMD relative to the S&P 500. Here, the dependent variable is the excess return of AMD, and the independent variable is the excess return of the S&P 500. Beta measures the sensitivity of the stock's returns to fluctuations in the market.

```{r lm}
# Perform linear regression
capm_model <- lm(AMD_Excess_Return ~ GSPC_Excess_Return, data = df)
summary(capm_model)
```


#### Interpretation

What is your \(\beta\)? Is AMD more volatile or less volatile than the market?

**Answer:**
The calculated \(\beta\) value of AMD was 1.5699987 and can be used to analyse AMD's volatility and systematic risk relative to the market. Foremost, this coefficient represents the relationship between AMD's excess returns and the S&P 500's excess returns. Specifically, for each additional unit increase in the S&P 500's excess return, AMD's excess return is predicted to increase by 1.5699987 units, having other factors constant. This means that AMD's returns are expected to change by approximately 1.57% for every 1% change in the market's returns.

Generally, a \(\beta\) greater than 1, which in our case 1.5699987 is, indicates that the asset is more volatile than the market. This heightened sensitivity could suggest that AMD's stock is more likely to experience larger fluctuations in response to market movements, thus reflecting the higher systematic risk. It's implications for the potential returns for investors is that whilst AMD may promise higher returns in favorable market conditions, in market downturns, AMD may return larger losses.

Consequently, the fairly high \(\beta\) of AMD may be appealing for risk-taking investors who are looking for the possibility of generating higher returns over shorter periods of time during economic upturns. However, for risk averse investors, \(\beta\)'s value of 1.5699987 units may be less attractive as it implies that AMD may possibly under perform the market during economic downturns and highly sensitive market conditions.



#### Plotting the CAPM Line
Plot the scatter plot of AMD vs. S&P 500 excess returns and add the CAPM regression line.

```{r plot}
# Scatter plot of AMD vs. S&P 500 excess returns with regression line
plot <- ggplot(df, aes(x = GSPC_Excess_Return, y = AMD_Excess_Return)) +
  geom_point(alpha = 0.5) +  # Semi-transparent points for better overlap visualization
  geom_smooth(method = "lm", col = "blue") +  # Adds a linear regression line in blue
  labs(title = "CAPM: AMD vs. S&P 500 Excess Returns", 
       x = "S&P 500 Excess Return", 
       y = "AMD Excess Return") +
  theme_minimal()  # Applies a minimalistic theme to the plot

# Print the plot
print(plot)

```

### Step 3: Predictions Interval
Suppose the current risk-free rate is 5.0%, and the annual expected return for the S&P 500 is 13.3%. Determine a 90% prediction interval for AMD's annual expected return.

*Hint: Calculate the daily standard error of the forecast ($s_f$), and assume that the annual standard error for prediction is $s_f \times \sqrt{252}$. Use the simple return average method to convert daily stock returns to annual returns if needed.*


**Answer:**

```{r pi}
#fill the code

# Assuming df is already defined and cleaned

# Step 1: Calculate the daily excess return for AMD and the S&P 500
df$AMD_Excess_Return <- df$AMD_Return - df$Daily_Risk_Free_Rate
df$GSPC_Excess_Return <- df$GSPC_Return - df$Daily_Risk_Free_Rate

# Check for any NA values in the excess returns
cat("Number of NA values in AMD_Excess_Return:", sum(is.na(df$AMD_Excess_Return)), "\n")
cat("Number of NA values in GSPC_Excess_Return:", sum(is.na(df$GSPC_Excess_Return)), "\n")

# Remove rows with NA values
df <- na.omit(df)

# Step 2: Fit a linear model to estimate beta (β)
model <- lm(AMD_Excess_Return ~ GSPC_Excess_Return, data = df)

# Extract the residuals and beta coefficient
residuals <- residuals(model)
beta <- coef(model)[2]

# Step 3: Compute the standard error of the forecast for the daily returns
n <- nrow(df)
mean_gspc_excess <- mean(df$GSPC_Excess_Return)
se <- sqrt(sum(residuals^2) / (n - 2))
SSX <- sum((df$GSPC_Excess_Return - mean_gspc_excess)^2)

# Daily standard error of the forecast
sf <- se * sqrt(1 + 1/n + (mean(df$GSPC_Excess_Return) - mean_gspc_excess)^2 / SSX)

# Print intermediate values for debugging
cat("Number of observations (n):", n, "\n")
cat("Mean GSPC excess return:", mean_gspc_excess, "\n")
cat("Standard error (se):", se, "\n")
cat("Sum of squares (SSX):", SSX, "\n")
cat("Daily standard error of the forecast (sf):", sf, "\n")

# Step 4: Convert the daily standard error to an annual basis
annual_sf <- sf * sqrt(252)  # 252 trading days in a year

# Step 5: Calculate the 90% prediction interval for AMD's annual expected return
annual_rf <- 0.05  # Annual risk-free rate
annual_market_return <- 0.133  # Annual expected return for S&P 500

# Expected annual return for AMD using CAPM
expected_annual_return <- annual_rf + beta * (annual_market_return - annual_rf)

# Calculate the t-value for a 90% prediction interval
alpha <- 0.10
t_value <- qt(1 - alpha/2, df = n - 2)

# Print t-value for debugging
cat("t-value for 90% prediction interval:", t_value, "\n")

# Prediction interval
lower_bound <- expected_annual_return - t_value * annual_sf
upper_bound <- expected_annual_return + t_value * annual_sf

# Print the results
cat("The estimated annual return for AMD is", round(expected_annual_return * 100, 2), "%\n")
cat("The 90% prediction interval is [", round(lower_bound * 100, 2), "%,", round(upper_bound * 100, 2), "%]\n")

```


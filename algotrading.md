
## Algorithmic Trading Strategy

## Introduction

In this assignment, you will develop an algorithmic trading strategy by incorporating financial metrics to evaluate its profitability. This exercise simulates a real-world scenario where you, as part of a financial technology team, need to present an improved version of a trading algorithm that not only executes trades but also calculates and reports on the financial performance of those trades.

## Background

Following a successful presentation to the Board of Directors, you have been tasked by the Trading Strategies Team to modify your trading algorithm. This modification should include tracking the costs and proceeds of trades to facilitate a deeper evaluation of the algorithm’s profitability, including calculating the Return on Investment (ROI).

After meeting with the Trading Strategies Team, you were asked to include costs, proceeds, and return on investments metrics to assess the profitability of your trading algorithm.

## Objectives

1. **Load and Prepare Data:** Open and run the starter code to create a DataFrame with stock closing data.

2. **Implement Trading Algorithm:** Create a simple trading algorithm based on daily price changes.

3. **Customize Trading Period:** Choose your entry and exit dates.

4. **Report Financial Performance:** Analyze and report the total profit or loss (P/L) and the ROI of the trading strategy.

5. **Implement a Trading Strategy:** Implement a trading strategy and analyze the total updated P/L and ROI. 

6. **Discussion:** Summarise your finding.


## Instructions

### Step 1: Data Loading

Start by running the provided code cells in the "Data Loading" section to generate a DataFrame containing AMD stock closing data. This will serve as the basis for your trading decisions. First, create a data frame named `amd_df` with the given closing prices and corresponding dates. 

```{r load-AMD.csv}

#set working directory
setwd("/Users/lukerobinson/Desktop/UNSW/ACTL1101/Assignment A")

# Load data from CSV file
amd_df <- read.csv("AMD.csv")

# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)

amd_df <- amd_df[, c("date", "close")]
```


##Plotting the Data
Plot the closing prices over time to visualize the price movement.
```{r plot}
plot(amd_df$date, amd_df$close,'l')
```


## Step 2: Trading Algorithm
Implement the trading algorithm as per the instructions. You should initialize necessary variables, and loop through the dataframe to execute trades based on the set conditions.

- Initialize Columns: Start by ensuring dataframe has columns 'trade_type', 'costs_proceeds' and 'accumulated_shares'.
- Change the algorithm by modifying the loop to include the cost and proceeds metrics for buys of 100 shares. Make sure that the algorithm checks the following conditions and executes the strategy for each one:
  - If the previous price = 0, set 'trade_type' to 'buy', and set the 'costs_proceeds' column to the current share price multiplied by a `share_size` value of 100. Make sure to take the negative value of the expression so that the cost reflects money leaving an account. Finally, make sure to add the bought shares to an `accumulated_shares` variable.
  - Otherwise, if the price of the current day is less than that of the previous day, set the 'trade_type' to 'buy'. Set the 'costs_proceeds' to the current share price multiplied by a `share_size` value of 100.
  - You will not modify the algorithm for instances where the current day’s price is greater than the previous day’s price or when it is equal to the previous day’s price.
  - If this is the last day of trading, set the 'trade_type' to 'sell'. In this case, also set the 'costs_proceeds' column to the total number in the `accumulated_shares` variable multiplied by the price of the last day.



```{r trading}
# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

#start of trading algorithm
for (i in 1:nrow(amd_df)) { #loop through each row of the data frame
  
  current_price <- amd_df$close[i] #get the current day's closing price
  
  if (i == 1) { # First day of trading
    # Buy 100 shares
    amd_df$trade_type[i] <- "buy" #set trade type to buy
    amd_df$costs_proceeds[i] <- -current_price * share_size #record the cost of buying 100 shares
    accumulated_shares <- accumulated_shares + share_size #update number of accumulated shares
    amd_df$accumulated_shares[i] <- accumulated_shares 
    #record this updated number in the accumulated shares column of our table
    
  } else if (i == nrow(amd_df)) { # Last day of trading
    # Sell all accumulated shares
    amd_df$trade_type[i] <- "sell" #set trade type to sell
    amd_df$costs_proceeds[i] <- current_price * accumulated_shares 
    #records the proceeds from selling all the shares
    
    amd_df$accumulated_shares[i] <- 0 #reset accumulated shares to 0
    
  } else { #start of buying algorithm for the rest of the days
    
    if (current_price < previous_price) { # Buy condition
      # Buy 100 shares
      amd_df$trade_type[i] <- "buy" #set trade type to buy
      amd_df$costs_proceeds[i] <- -current_price * share_size #record cost of buying 100 shares
      accumulated_shares <- accumulated_shares + share_size #update number of accumulated shares
      
    } else { # Hold condition
      amd_df$trade_type[i] <- "hold" #set trade type to hold
      amd_df$costs_proceeds[i] <- 0 #no financial transactions occur as no shares are bought
    }
    amd_df$accumulated_shares[i] <- accumulated_shares #record the accumulated shares in the table
  }
  
  previous_price <- current_price #update the previous price to the current day's price
}
```


## Step 3: Customize Trading Period
- Define a trading period you wanted in the past five years 
```{r period}
# Define dates
start_date <- as.Date('2019-12-02')  #set the start date of the trading period
end_date <- as.Date('2020-12-02')  #set the end date of the trade period  

# Filter the data to include only the trading period
amd_df <- amd_df[amd_df$date >= start_date & amd_df$date <= end_date, ] 
#keeps only the rows where the date is inbetween the start and end date

#Now rerun the algorithm but only for the set trading period
# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

#start of trading algorithm
for (i in 1:nrow(amd_df)) { #loop through each row of the data frame
  
  current_price <- amd_df$close[i] #get the current day's closing price
  
  if (i == 1) { # First day of trading
    # Buy 100 shares
    amd_df$trade_type[i] <- "buy" #set trade type to buy
    amd_df$costs_proceeds[i] <- -current_price * share_size #record the cost of buying 100 shares
    accumulated_shares <- accumulated_shares + share_size #update number of accumulated shares
    amd_df$accumulated_shares[i] <- accumulated_shares 
    #record this updated number in the accumulated shares column of our table
    
  } else if (i == nrow(amd_df)) { # Last day of trading
    # Sell all accumulated shares
    amd_df$trade_type[i] <- "sell" #set trade type to sell
    amd_df$costs_proceeds[i] <- current_price * accumulated_shares 
    #records the proceeds from selling all the shares
    
    amd_df$accumulated_shares[i] <- 0 #reset accumulated shares to 0
    
  } else { #start of buying algorithm for the rest of the days
    
    if (current_price < previous_price) { # Buy condition
      # Buy 100 shares
      amd_df$trade_type[i] <- "buy" #set trade type to buy
      amd_df$costs_proceeds[i] <- -current_price * share_size #record cost of buying 100 shares
      accumulated_shares <- accumulated_shares + share_size #update number of accumulated shares
      
    } else { # Hold condition
      amd_df$trade_type[i] <- "hold" #set trade type to hold
      amd_df$costs_proceeds[i] <- 0 #no financial transactions occur as no shares are bought
    }
    amd_df$accumulated_shares[i] <- accumulated_shares #record the accumulated shares in the table
  }
  
  previous_price <- current_price #update the previous price for the next iteration
}
```


## Step 4: Run Your Algorithm and Analyze Results
After running your algorithm, check if the trades were executed as expected. Calculate the total profit or loss and ROI from the trades.

- Total Profit/Loss Calculation: Calculate the total profit or loss from your trades. This should be the sum of all entries in the 'costs_proceeds' column of your dataframe. This column records the financial impact of each trade, reflecting money spent on buys as negative values and money gained from sells as positive values.
- Invested Capital: Calculate the total capital invested. This is equal to the sum of the 'costs_proceeds' values for all 'buy' transactions. Since these entries are negative (representing money spent), you should take the negative sum of these values to reflect the total amount invested.
- ROI Formula: $$\text{ROI} = \left( \frac{\text{Total Profit or Loss}}{\text{Total Capital Invested}} \right) \times 100$$

```{r}
# Total Profit/Loss Calculation
total_profit_or_loss <- sum(amd_df$costs_proceeds, na.rm = TRUE) 
#calculate p/l by summing the values in the costs proceeds column, ignoring NA values

#Invested Capital Calculation
total_invested <- -sum(amd_df$costs_proceeds[amd_df$trade_type == "buy"], na.rm = TRUE) 
#calculate total invested by summing costs proceeds column where the buy condition was satisfied, ignoring NA values. The negative sign is present to convert the total cost to a positive invested capital amount. 

# ROI Calculation
roi <- (total_profit_or_loss / total_invested) * 100 #use above values and formula to calculate

# Print the results
cat("Total Profit/Loss: ", total_profit_or_loss, "\n")
cat("Total Capital Invested: ", total_invested, "\n")
cat("Return on Investment (ROI): ", roi, "%\n")
```

## Step 5: Profit-Taking Strategy or Stop-Loss Mechanisum (Choose 1)
- Option 1: Implement a profit-taking strategy that you sell half of your holdings if the price has increased by a certain percentage (e.g., 20%) from the average purchase price.
- Option 2: Implement a stop-loss mechanism in the trading strategy that you sell half of your holdings if the stock falls by a certain percentage (e.g., 20%) from the average purchase price. You don't need to buy 100 stocks on the days that the stop-loss mechanism is triggered.


```{r option}
# Define the profit-taking percentage
profit_percentage <- 0.20  # This sets the profit percentage threshold at 20%

# Initialize additional variables for the profit-taking strategy
total_cost <- 0  # This variable will track the total cost of shares purchased
total_shares <- 0  # This variable will track the total number of shares held

# Add new columns for running average purchase price and overall accumulated costs
amd_df$running_avg_purchase_price <- NA  # Initialize the running average purchase price column with NA values

# Start of the updated trading algorithm
for (i in 1:nrow(amd_df)) {
  current_price <- amd_df$close[i]  # Get the current day's closing price
  
  if (i == 1) {  # First day of trading
    # Buy 100 shares
    amd_df$trade_type[i] <- "buy"  # Set the trade type for the first day to "buy"
    cost <- current_price * share_size  # Calculate the cost of purchasing 100 shares
    amd_df$costs_proceeds[i] <- -cost  # Record the cost in the costs proceeds column as a negative value
    accumulated_shares <- share_size  # Set the number of accumulated shares for the first day
    total_cost <- cost  # Set the total cost for the first day
    total_shares <- share_size  # Set the total shares for the first day
    amd_df$accumulated_shares[i] <- accumulated_shares  # Record the accumulated shares for the first day in the accumulated shares column
    
  } else if (i == nrow(amd_df)) {  # Last day of trading
    # Sell all accumulated shares
    amd_df$trade_type[i] <- "sell"  # Set the trade type for the last day to "sell"
    proceeds <- current_price * accumulated_shares  # Calculate the proceeds from selling all the accumulated shares 
    amd_df$costs_proceeds[i] <- proceeds  # Record the proceeds
    accumulated_shares <- 0  # Set accumulated shares back to 0
    amd_df$accumulated_shares[i] <- accumulated_shares  # Record the accumulated shares for the last day in the accumulated shares column
    
  } else {
    avg_purchase_price <- total_cost / total_shares  #Calculate the running average purchase price
    
    # Buy condition
    if (current_price < previous_price) { 
      # Buy 100 shares
      amd_df$trade_type[i] <- "buy"  # Set trade type to "buy"
      cost <- current_price * share_size  # Calculate the cost of purchasing the 100 shares
      amd_df$costs_proceeds[i] <- -cost  # Record the cost as a negative in the costs proceeds column
      accumulated_shares <- accumulated_shares + share_size  # Update the number of accumulated shares
      total_cost <- total_cost + cost  # Update the total cost
      total_shares <- total_shares + share_size  # Update the total shares
 
    # Profit-taking condition     
    } else if (current_price >= avg_purchase_price * (1 + profit_percentage)) { 
      # Sell half of the holdings
      shares_to_sell <- floor(accumulated_shares / 2)  # Calculate the number of shares to sell, which is half of the accumulated shares
      proceeds <- current_price * shares_to_sell  # Calculate the proceeds from selling the shares
      amd_df$trade_type[i] <- "sell"  # Set trade type to "sell"
      amd_df$costs_proceeds[i] <- proceeds  # Record the proceeds in the costs proceeds column 
      accumulated_shares <- accumulated_shares - shares_to_sell  # Update accumulated shares
      total_cost <- avg_purchase_price * accumulated_shares  # Update the total cost for the remaining shares
      total_shares <- accumulated_shares  # Update the total shares
     
    # Hold condition 
    } else { 
      amd_df$trade_type[i] <- "hold"  # Set the trade type to "hold"
      amd_df$costs_proceeds[i] <- 0  # No financial transactions occur as no shares are bought          #or sold
    }
    amd_df$accumulated_shares[i] <- accumulated_shares  # Record accumulated shares in the table
  }
  
  #Record running average purchase price and overall accumulated costs for each date
  amd_df$running_avg_purchase_price[i] <- total_cost / total_shares  # Update the running average purchase price

  previous_price <- current_price  # Update the previous price for the next iteration
}

```


## Step 6: Summarize Your Findings
- Did your P/L and ROI improve over your chosen period?
- Relate your results to a relevant market event and explain why these outcomes may have occurred.


```{r}
# P/L and ROI for new profit-taking strategy:
# Total Profit/Loss Calculation
total_profit_or_loss <- sum(amd_df$costs_proceeds, na.rm = TRUE) #calculate p/l by summing the values in the costs proceeds column, ignoring NA values

#Invested Capital Calculation
total_invested <- -sum(amd_df$costs_proceeds[amd_df$trade_type == "buy"], na.rm = TRUE) 
#calculate total invested by summing costs proceeds column where the buy condition was satisfied, ignoring NA values. The negative sign is present to convert the total cost to a positive invested capital amount. 

# ROI Calculation
roi <- (total_profit_or_loss / total_invested) * 100 #use above values and formula to calculate

# Print the results
cat("Total Profit/Loss: ", total_profit_or_loss, "\n")
cat("Total Capital Invested: ", total_invested, "\n")
cat("Return on Investment (ROI): ", roi, "%\n")

```

Plot of  prices over selected time period for visualisation
```{r plot for trading period}
plot(amd_df$date, amd_df$close,'l')
```
Discussion:
Throughout the set trade period from the 2nd of December 2019 to the 2nd of December 2020, various market events throughout the period evidently impacted the performance and effectiveness of both trading strategies. This was apparent through these market events causing sharp fluctuations in share price, thus affecting the overall market trend.


Evidently, with the visual aid of the plotted share prices for the period, it can be seen that there was a sharp increase in share price towards late July of 2020, specifically on Friday, July 24 2020. On this day, AMD debuted their new Ryzen 4000 Series processors which had strong graphics capabilities and promised powerful gains in performance and energy efficiency. This was expected to assist AMD in competing in the high end gaming market against key competitor, Intel. On top of this, Intel had also been struggling, recently losing a major contract with Apple which resulted in Apple beginning to create its own chips for its Mac computers. With the decline of Intel and the rise of AMD, it only improved investor confidence in regards to AMD shares due to their consequent long term stability in the competitive market with their new Ryzen 4000 series processors having limited competition. Consequently from 2020-07-23 to 2020-07-24 their share price increased from 59.57 to 69.40. 


An additional market event following directly on from this explaining an overall long term trend of an increasing share price was on 2020-07-28 (Tuesday, July 28 2020) where AMD released improved revenue forecasts for the upcoming financial year. This was evident through how AMD expected a 32% revenue growth for the year, up from 25% growth the company predicted in April 2020, leading to increased confidence in AMD's growth potential. CEO of AMD, Lisa Su, stated "We expect strong second-half semi-custom growth as we read production to support the holiday launches of the new PlayStation 5, and Xbox Series X consoles" This sparked high investor confidence in the upward potential of AMD chips due to its increased demand to help build these consoles and thus perform well in the competitive market. Overall, combining these factors can explain the increase of shares from 2020-07-28 (67.61) to 2020-07-29 (76.09)


Relating these events to the performance of the initial trading algorithm in step 2 compared to the Profit Taking strategy algorithm in step 5, using the data for the entire period, it could seen that the initial trading algorithm had both a larger Profit and Loss (PnL) as well as Return on Investment (ROI). This was apparent through the initial trading algorithm generating a PnL of 385727 compared to 171577.3, and a ROI of 52.85531% compared to 23.51086%. This means that the first strategy earned 214149.7 dollars more than the second strategy. Consequently, it can be seen that overall, the initial trading algorithm was more successful and that step 5's profit taking strategy algorithm did not improve ROI.


A possible explanation for why this occurred could be that step 5's profit taking strategy allowed for the capture of profits early by selling half of the holdings when the price reached the 20% threshold. However, step 2's strategies focused on holding shares for longer periods, potentially capturing overall upward trends, especially during stable or steadily increasing markets, which was present in our set time frame. This meant that within the profit taking strategy, it possibly resulted in the sale of shares too early. This could be applied to the provided market event through how although step 5's algorithm sold the shares after the large increase to 69.40, if they had waited 4 days, the share price they could have sold their holdings at would have been 78.20 on 2020-07-30, resulting in higher profits.


Extending on this, another reason for these results was the stability of the market during the period, where the share price was on an overall increase throughout the whole period. This favoured step 2's trading algorithm due to how it was able to hold all shares until the end of the period, and not suffer significant financial losses due to minimal declines in overall share price. Instead, due to the overall large increase in share price from the start of the period to the end of it, it led to higher profits due to their ability to sell all their accumulated shares at larger margins. Having said this, Step 5's algorithm, although the least effective for this selected time frame, would have performed better in a less stable market, reducing risk by selling part of the holdings when the price increased significantly, thus locking in gains and reducing exposure that step 2 would not have been able to achieve with a volatile share price.


To conclude, the analysis of the effectiveness of both trading strategies over the set trade period from December 2, 2019, to December 2, 2020, highlights the impact of market events on their performance, specifically their Profit and Loss as well as Return on Investment. In summation, over our set time frame, the initial trading algorithm in step 2 was more successful than the profit taking algorithm in step 5.





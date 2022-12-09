## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Include your insights and observations.

###############################################################################

# 1. Load and explore the data

# Install and import Tidyverse.
library('tidyverse')

# Import the data set.
data <- read.csv('turtle_sales.csv', header = TRUE)

# Print the data frame.
head(data)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
sales <- select(data, -Ranking, -Year, -Genre, -Publisher)

# View the data frame.
head(sales)

# View the descriptive statistics.
summary(sales)

################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.
qplot(Product, NA_Sales, data=sales, geom=c('point', 'smooth'), 
      main='North American Sales per Product', xlab='Product Code', 
      ylab='North American Sales (£m)')
qplot(Product, EU_Sales, data=sales, geom=c('point', 'smooth'), 
      main='European Sales per Product', xlab='Product Code', 
      ylab='European Sales (£m)')
qplot(Product, Global_Sales, data=sales, geom=c('point', 'smooth'),
      main='Global Sales per Product', xlab='Product Code', 
      ylab='Global Sales (£m)')

## 2b) Histograms
# Create histograms.
qplot(Platform, data=sales, main='Number of Products per Platform', 
      xlab='Platform', ylab='Number of Products')
qplot(NA_Sales, data=sales, main='Distribution of North American Sales', 
      xlab='North American Sales (£m)', ylab='Number of Products')
qplot(EU_Sales, data=sales, main='Distribution of European Sales', 
      xlab='European Sales (£m)', ylab='Number of Products')
qplot(Global_Sales, data=sales, main='Distribution of Global Sales', 
      xlab='Global Sales (£m)', ylab='Number of Products')

## 2c) Boxplots
# Create boxplots.
qplot(NA_Sales, Platform, data=sales, geom='boxplot', 
      main='Distribution of North American Sales per Platform', 
      xlab='North American Sales (£m)', ylab='Platform')
qplot(EU_Sales, Platform, data=sales, geom='boxplot', 
      main='Distribution of European Sales per Platform', 
      xlab='European Sales (£m)', ylab='Platform')
qplot(Global_Sales, Platform, data=sales, geom='boxplot', 
      main='Distribution of Global Sales per Platform', 
      xlab='Global Sales (£m)', ylab='Platform')

###############################################################################

# 3. Observations and insights

## Your observations and insights here ......

# I utilised scatterplots to determine relationships between the numerical 
# variables: Product Code and each Sales variable. The smoothing curve for the 
# each scatterplot resembles negative exponential curves, meaning the highest 
# Sales belonged to Products with low Product Codes. Turtle Games should thus 
# prioritise low Products with low Product Codes to improve Sales.

# I utilised a histogram to determine the Platforms with the most Products: the 
# Platform histogram indicates these Platforms to be the Xbox 360, PS3 and PC. 
# Turtle Games should prioritise Products on these Platforms to improve sales, 
# as they are the most popular Platforms. I also generated histograms of each 
# Sales variable to determine their distribution: the Sales histograms indicate 
# that each Sales variable is positively skewed.

# I utilised boxplots to determine the distribution of each Sales variable by 
# Platform. In each of the 3 Sales boxplots, there was an extreme positive 
# outlier for the Wii Platform, so I would like to further investigate Sales 
# amongst Products on the Wii Platform. In North America and Globally, the NES
# had the highest interquartile range of sales, while in Europe the Wii had the
# highest interquartile range of sales.


###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and maniulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 6. Include your insights and observations.

################################################################################

# 1. Load and explore the data

# View data frame created in Week 4.


# Check output: Determine the min, max, and mean values.


# View the descriptive statistics.


###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.


# View the data frame.


# Explore the data frame.



## 2b) Determine which plot is the best to compare game sales.
# Create scatterplots.


# Create histograms.


# Create boxplots.


###############################################################################


# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plots.



## 3b) Perform Shapiro-Wilk test
# Install and import Moments.


# Perform Shapiro-Wilk test.



## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.



## 3d) Determine correlation
# Determine correlation.


###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.


###############################################################################

# 5. Observations and insights
# Your observations and insights here...



###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explor the data
# View data frame created in Week 5.


# Determine a summary of the data frame.


###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.



## 2b) Create a plot (simple linear regression)
# Basic visualisation.


###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.


# Multiple linear regression model.


###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.



###############################################################################

# 5. Observations and insights
# Your observations and insights here...



###############################################################################
###############################################################################





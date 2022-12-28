## LSE Data Analytics Online Career Accelerator 
# DA301:  Advanced Analytics for Organisational Impact

################################################################################

# Part 4: EDA using R

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
# Create scatterplots of Product Code against Sales.
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
# Create a histogram of Products per Platform.
qplot(Platform, data=sales, main='Number of Products per Platform', 
      xlab='Platform', ylab='Number of Products')

# Create histograms of each Sales variable.
qplot(NA_Sales, data=sales, main='Distribution of North American Sales', 
      xlab='North American Sales (£m)', ylab='Number of Products')
qplot(EU_Sales, data=sales, main='Distribution of European Sales', 
      xlab='European Sales (£m)', ylab='Number of Products')
qplot(Global_Sales, data=sales, main='Distribution of Global Sales', 
      xlab='Global Sales (£m)', ylab='Number of Products')

## 2c) Boxplots
# Create boxplots to determine summary statistics of Sales per Platform.
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


# Part 5: Cleaning and manipulating data using R


################################################################################

# 1. Load and explore the data

# View data frame created in Part 4.
View(sales)

# Check output: Determine the min, max, and mean values for each Sales variable.
apply(select(sales, NA_Sales, EU_Sales, Global_Sales), 2, min)
apply(select(sales, NA_Sales, EU_Sales, Global_Sales), 2, max)
apply(select(sales, NA_Sales, EU_Sales, Global_Sales), 2, mean)

# View the descriptive statistics.
summary(sales)

###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and summarise functions.
# Group data based on Product and determine the Sales sum per Product.
sales2 <- sales %>% group_by(Product) %>% 
  summarise(sum_NA_Sales=sum(NA_Sales), 
            sum_EU_Sales=sum(EU_Sales), 
            sum_Global_Sales=sum(Global_Sales), 
            .groups='drop')

# View the data frame.
View(sales2)

# Explore the data frame.
summary(sales2)


## 2b) Determine which plot is the best to compare game sales.
# Create scatterplots of Product Code against summated Sales.
ggplot(data = sales2,
       mapping = aes(x = Product, y = sum_NA_Sales)) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_smooth(se = FALSE, size = 1.5) + 
  labs(title = "North American Sales per Product",
       x = "Product Code",
       y = "North American Sales (£m)") +
  
  # Add a theme layer. 
  theme_bw()

ggplot(data = sales2,
       mapping = aes(x = Product, y = sum_EU_Sales)) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_smooth(se = FALSE, size = 1.5) + 
  labs(title = "European Sales per Product",
       x = "Product Code",
       y = "European Sales (£m)") +
  
  # Add a theme layer. 
  theme_bw()

ggplot(data = sales2,
       mapping = aes(x = Product, y = sum_Global_Sales)) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_smooth(se = FALSE, size = 1.5) + 
  labs(title = "Global Sales per Product",
       x = "Product Code",
       y = "Global Sales (£m)") +
  
  # Add a theme layer. 
  theme_bw()

# Create histograms of each summated Sales variable.
ggplot(sales2, aes(x = sum_NA_Sales)) +
  # Add fill, colour, and bin number.
  geom_histogram(fill = 'red', color = 'black', bins=20) + 
  # Add the labs function for labels.
  labs(x = "North American Sales (£m)",
       y = "Number of Products",
       title = "Distribution of North American Sales per Product") 

ggplot(sales2, aes(x = sum_EU_Sales)) +
  # Add fill, colour, and bin number.
  geom_histogram(fill = 'blue', color = 'black', bins=20) + 
  # Add the labs function for labels.
  labs(x = "European Sales (£m)",
       y = "Number of Products",
       title = "Distribution of European Sales per Product") 

ggplot(sales2, aes(x = sum_Global_Sales)) +
  # Add fill, colour, and bin number.
  geom_histogram(fill = 'green', color = 'black', bins=20) + 
  # Add the labs function for labels.
  labs(x = "Global Sales (£m)",
       y = "Number of Products",
       title = "Distribution of Global Sales per Product") 

# Create a histogram of Products per Platform in terms of percentages.
ggplot(sales,
       # Specify 'y' to create a percentage. 
       aes(x = Platform, y = ..count../sum(..count..))) +  
  # Specify attributes.
  geom_histogram(fill = 'yellow', color = 'black', stat = 'count') +
  # Specify titles.
  labs(x = "Platform",
       y = "Percent",
       title = "Products by Platform") +  
  # Pass labels to the scale.
  scale_y_continuous(label = scales::percent) +
  # Flip the x-axis and y-axis.
  coord_flip()  

# Create boxplots that visualise summary statistics of Sales per Platform.
ggplot(sales, aes(x = NA_Sales, y = Platform)) +
  # Specify the geom_boxplot function.
  geom_boxplot(fill = 'red', outlier.color = 'red') +
  # Specify the titles.
  labs(title = "North American Sales by Platform", 
       x = "North American Sales (£m)", 
       y = "Platform") +  
  # Add a 'minimal' theme.
  theme_minimal()  

ggplot(sales, aes(x = EU_Sales, y = Platform)) +
  # Specify the geom_boxplot function.
  geom_boxplot(fill = 'green', outlier.color = 'green') +
  # Specify the titles.
  labs(title = "European Sales by Platform", 
       x = "European Sales (£m)", 
       y = "Platform") +  
  # Add a 'minimal' theme.
  theme_minimal()  

ggplot(sales, aes(x = Global_Sales, y = Platform)) +
  # Specify the geom_boxplot function.
  geom_boxplot(fill = 'orange', outlier.color = 'orange') +
  # Specify the titles.
  labs(title = "Global Sales by Platform", 
       x = "Global Sales (£m)", 
       y = "Platform") +  
  # Add a 'minimal' theme.
  theme_minimal()  

###############################################################################


# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots of each summated Sales variable
qqnorm(sales2$sum_NA_Sales)
qqline(sales2$sum_NA_Sales)

qqnorm(sales2$sum_EU_Sales)
qqline(sales2$sum_EU_Sales)

qqnorm(sales2$sum_Global_Sales)
qqline(sales2$sum_Global_Sales)

## 3b) Perform Shapiro-Wilk test.
# Install and import Moments.
library(moments)

# Perform Shapiro-Wilk test on each summated Sales variable.
shapiro.test(sales2$sum_NA_Sales)
shapiro.test(sales2$sum_EU_Sales)
shapiro.test(sales2$sum_Global_Sales)

## 3c) Determine Skewness and Kurtosis on each summated Sales variable.
skewness(sales2$sum_NA_Sales)
kurtosis(sales2$sum_NA_Sales)

skewness(sales2$sum_EU_Sales)
kurtosis(sales2$sum_EU_Sales)

skewness(sales2$sum_Global_Sales)
kurtosis(sales2$sum_Global_Sales)


## 3d) Determine correlation

# None of the Sales variables are normally distributed.
# As the Pearson correlation only works on normally distributed variables,
# the correlation of the Sales variables cannot be determined.

###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.

# Create a multivariate scatterplot of Global Sales by Product and Platform.
ggplot(data = sales,
       mapping = aes(x = Product, y = Global_Sales, color = Platform)) +
  geom_point(alpha = 1, size = 3) +
  labs(title = "Global Sales per Product",
       x = "Product Code",
       y = "Global Sales (£m)") +
  
  # Add a theme layer. 
  theme_bw() +
  scale_fill_brewer('set2')

# Create a KDP of summated Global Sales.
ggplot(sales2, aes(x = sum_Global_Sales)) +
  # Add fill colour to the function.
  geom_density(fill = 'red') + 
  # Specify the title.
  labs(title = "Distribution of Global Sales", 
       x = "Global Sales (£m)", 
       y = "Density")

# Create a violin plot showing the summary statistics and distribution of
# Global Sales amongst the 5 most popular Platforms.
ggplot(subset(sales, Platform=='PC' | Platform=='PS3' | Platform=='X360' | 
                Platform=='Wii' | Platform=='DS'), 
       aes(x = Global_Sales, y = Platform)) +
  # Specify the geom_violin function and fill.
  geom_violin(fill = 'blue') +  
  # Specify the geom_boxplot.
  geom_boxplot(fill = 'orange', width = 0.25,
               outlier.color = 'orange', outlier.size = 1,
               outlier.shape = 'square') +
  # Specify the title.
  labs(title = "Distribution of Global Sales for the 5 top Platforms", 
       x = "Global Sales (£m)", 
       y = "Platform")

###############################################################################

# 5. Observations and insights
# Your observations and insights here...

# The Q-Q plots and Shapiro-Wilk tests of the Sales variables confirm that these
# variables aren't normally distributed. The skewness() & kurtosis() results
# reveal that the Sales variables are extremely positively skewed and 
# heavy-tailed. Under section 4 above, I employed a KDP of Global Sales
# to confirm these findings. As these variables are non-normal, 
# correlation between them cannot be determined.

# I utilised ggplot() to generate improved versions of the visualisations
# discussed in Part 4 (see section 2b above). These confirmed my previous
# insights that the highest-selling Products are those with low Product Codes;
# the most popular Platforms are the Xbox 360, PS3 & PC; and that the Wii has
# several extremely positive outliers in terms of Global Sales.

# I utilised a multivariate scatterplot to inform Turtle Games which Platforms
# the highest selling Products globally belong to. The highest-selling
# Product was on the Wii. I also generated a violin plot of Global Sales
# per the 5 most popular Platform to inform Turtle Games of both the 
# summary statistics and distribution of Sales for the 5 top Platforms they
# should target. 


###############################################################################
###############################################################################

# Part 6: Making recommendations to the business using R

###############################################################################

# 1. Load and explore the data
# View aggregated data frame created in Part 5.
View(sales2)

# Determine a summary of the data frame.
summary(sales2)

###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
cor(select(sales2, -Product))
# Create linear regression models of summated Global Sales against the other
# summated Sales variables.
model1 <- lm(sum_Global_Sales~sum_NA_Sales,
             data=sales2)
summary(model1)

model2 <- lm(sum_Global_Sales~sum_EU_Sales,
          data=sales2)
summary(model2)


## 2b) Create a plot (simple linear regression)
# Basic visualisations of both linear regression models.
plot(sales2$sum_NA_Sales, sales2$sum_Global_Sales)
abline(coefficients(model1))

plot(sales2$sum_EU_Sales, sales2$sum_Global_Sales)
abline(coefficients(model2))

###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the aggregated data frame.
# Create a multiple linear regression model.
modela = lm(sum_Global_Sales~sum_NA_Sales+sum_EU_Sales, data=sales2)
summary(modela)

###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.

# Create a vector to store the test North American sales values.
sum_NA_Sales <- c(34.02, 3.93, 2.73, 2.26, 22.08) 

# Create a vector to store the test European sales values.
sum_EU_Sales <- c(23.80, 1.56, 0.65, 0.97, 0.52)

# Create a DataFrame to store both vectors.
testdata <- data.frame(sum_NA_Sales, sum_EU_Sales)

#View the DataFrame.
testdata

# Generate predicted values based on the test values using the multiple linear
# regression model.
predictTest = predict(modela, newdata=testdata, 
                      interval='confidence')

# View the predicted values.
predictTest

# View the aggregated DataFrame to determine if the multiple linear regression
# model is accurate.
View(sales2)

###############################################################################

# 5. Observations and insights
# Your observations and insights here...

# The correlation matrix of the sales per product variables indicates that 
# North American Sales per Product is more strongly correlated with Global
# Sales per Product than European Sales per Product. 

# I created 2 simple linear regression models: one to determine the effect of
# North American Sales per Product on Global Sales per Product, and the other
# to determine the effect of European Sales per Product on Global Sales per
# Product. In both models, the independent variable's coefficient was positive
# and extremely statistically significant. This is confirmed by the
# visualisations of Global Sales per Product against both independent variables.
# Interestingly, the coefficient for European Sales was stronger than the 
# coefficient for North American Sales, in contrast to the results from the 
# correlation matrix.

#I then utilised a multiple linear regression model to determine the effect of 
# sum_NA_Sales & sum_EU_Sales on sum_Global_Sales. As expected, the coefficients 
# on sum_NA_Sales & sum_EU_Sales were both extremely statistically significant 
# and positive. Interestingly, sum_EU_Sales had a larger coefficient (1.20) than 
# sum_NA_Sales (1.13), suggesting Turtle Games should slightly prioritise Europe 
# to improve Sales. While prior analysis has indicated that the Sales variables 
# are non-normal and very positively skewed, the t-tests utilised should be 
# robust against this non-normality. 

# The R-squared and adjusted R-squared values for the multiple linear regression
# model were very strong at around 97%, which were higher than in both simple 
# linear regression models. I also predicted global sales based on provided
# values to test model accuracy. The actual Global Sales for Test Case 1 
# (£67.85m) fell within the Confidence Interval, but actual Global Sales for 
# Test Case 5 (£23.21m) didn’t fall within the Confidence Interval despite 
# being only £3m off the predicted value. Overall, this multiple linear 
# regression model is a decent predictor of Global Sales, but can be improved by
# adding in more numerical variables such as review scores.


###############################################################################
###############################################################################





# LSE_DA_Turtle_Games_Analysis
This repository contains all project files relevant to analysing Turtle Games.

Turtle Games wishes to improve sales performance. To achieve this, I will analyse their datasets to recommend how to utilise customer trends to increase sales.

After importing necessary libraries, I imported ‘turtle_reviews.csv’ into a DataFrame ‘reviews’. I confirmed no missing values were present. I dropped and renamed several columns to improve clarity.

To determine how customers accumulate loyalty_points, I defined the dependent variable y as loyalty_points, and the independent variables x1-x3 as spending_score, remuneration and age. I created 3 OLS models of y against each x, and printed each model’s summary to obtain OLS regression results. Using each model summary’s coefficients, I created 3 linear regression models of predicted loyalty_points for each x. I created 3 scatterplots to visualise loyalty_points against each x, alongside their respective regression lines (see Appendix Fig. 1-3). The coefficient on age had a p-value above 0.05, meaning the null hypothesis that age’s coefficient equals 0 is not rejected at 5% significance level. Fig. 3 confirms no relationship between age and loyalty_points. The p-values for spending_score and remuneration coefficients were statistically significant: ceteris paribus, a 1-unit increase in spending_score and £1k increase in remuneration leads to a 33.0617 and 34.1878 increase in loyalty_points respectively. Fig. 1-2 confirm initially there’s a linear relationship between loyalty_points and both spending_score and remuneration. However, for spending_scores above 60 and remuneration above £50k, there doesn’t appear to be any relationship with loyalty_points.

I created scatterplots of remuneration against spending_score (Appendix Fig. 4-5), suggesting 5 clusters within the dataset. To identify the optimal number of clusters to utilise, I employed Elbow and Silhouette methods. The Elbow chart (Appendix Fig. 6) appeared to become linear at 5 clusters, and the highest value in the Silhouette chart (Appendix Fig. 7) occurred at 5 clusters, suggesting 5 was the optimal number of clusters. To confirm this, I evaluated usefulness of using 5-7 clusters. Of the 3 pairplots created (Appendix Fig. 8-10), the k-means model with 5 clusters featured closely grouped clusters clearly separated from each other, unlike the other models. I utilised 5 clusters in the final model, and visualised them in a scatterplot.

I prepared review and summary columns for NLP, applied tokenisation, created one list of review and summary tokens, removed alphanumeric characters and stopwords, and created a wordcloud for this list to visualise most utilised words across both columns. I identified the 15 most common words across both columns and their polarity scores. Words like ‘great’ have positive polarity scores, and should be utilised in marketing to evidence positive customer experiences. I identified the top 20 positive reviews and summaries, some should be highlighted in marketing to confirm high-quality experiences to customers. I identified the top 20 negative reviews and summaries, highlighting difficult instructions and boring products. This feedback should be employed to improve products and sales.

In RStudio, I imported the tidyverse library and the turtle_sales dataset, and then removed several unnecessary columns: Ranking, Year, Genre and Publisher. I utilised scatterplots to determine relationships between the numerical variables: Product Code and each Sales variable. The smoothing curve for the scatterplots resembles negative exponential curves, which means the highest Sales belonged to Products with low Product Codes. Turtle Games should thus prioritise low Products with low Product Codes to improve Sales. I utilised a histogram to determine the Platforms with the most Products: the histogram indicates these Platforms to be the Xbox 360, PS3 and PC. Turtle Games should prioritise Products on these Platforms to improve sales, as they are the most popular Platforms. I also generated histograms of each Sales variable to determine their distribution: the Sales histograms indicate that each Sales variable is positively skewed. I utilised boxplots to determine the distribution of each Sales variable by Platform. In each of the Sales boxplots, there was an extreme positive outlier for the Wii Platform, so I would like to further investigate Sales amongst Products on the Wii Platform.

I created a new DataFrame (sales2) that grouped turtle_sales data based on Product and summated each Sales variable per Product. For each Sales variable in sales2, I performed Shapiro-Wilk tests and rejected the null hypotheses that each Sales variable was normally distributed due to p-values below 0.05, meaning that each Sales variables is non-normal. This was confirmed by Q-Q plots of these Sales variables, as all the points don’t lie on a straight line. Consequently, correlation isn’t able to be determined between the different Sales variables. I also used skewness() and kurtosis() to determine that each Sales variable is extremely positively skewed and heavy-tailed. This is confirmed by a KDP of Global Sales from sales2. Utilising ggplot() on the turtle_sales and sales2 data, I generated improved versions of the visualisations presented in the previous section which confirmed my previous insights. I generated a multivariate scatterplot to inform Turtle Games of which platforms the highest selling products globally belong to. I generated a violin plot of Global_Sales per Platform to inform Turtle Games of the summary statistics and distribution of Global_Sales on each Platform.

To determine what relationships exist between the Sales per Product variables (sum_NA_Sales, sum_EU_Sales and sum_Global_Sales), I first created a correlation matrix that indicated that sum_Global_Sales was more strongly correlated with sum_NA_Sales versus sum_EU_Sales As there’re 3 Sales variables, I utilised a multiple linear regression model to determine the effect of sum_NA_Sales & sum_EU_Sales on sum_Global_Sales. As expected, the coefficients on sum_NA_Sales & sum_EU_Sales were both extremely statistically significant and positive. Interestingly, sum_EU_Sales had a larger coefficient (1.20) than sum_NA_Sales (1.13), suggesting Turtle Games should slightly prioritise Europe to improve Sales. While prior analysis has indicated that the Sales variables are non-normal and very positively skewed, the t-tests utilised should be robust against this non-normality. The R-squared and adjusted R-squared values were very strong at around 97%. Overall, this model is a decent predictor of Global Sales, but can be improved by adding in more numerical variables such as review scores.

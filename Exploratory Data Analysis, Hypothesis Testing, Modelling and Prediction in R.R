################### Libraries / Packages ################

# Packages used in this script 
packages <- c("tidyverse", "dplyr", "ggplot2", "pastecs", "statsr", "skimr", "GGally", "Hmisc")

# Install any packages that are not currently installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Loading the packages
invisible(lapply(packages, library, character.only = TRUE))

#################### Loading Data #################

# loading the dataset for version 1 of the assignment 
dataset <- read.csv("http://bit.ly/2OMHgFi")


################### Exploring the variables ##################

glimpse(dataset)
str(dataset)
dim(dataset) 


#################### Data cleaning #################

# changing the variable 'seasons' to a factor, made up of 4 levels (Spring, Summer, Autumn, Winter)
levels(dataset$seasons)
dataset$seasons <- as.factor(dataset$seasons)
dataset$seasons %>% levels()

# changing the variable 'country' to a factor, made up of 2 levels (Country A, Country B)
levels(dataset$country)
dataset$country <- as.factor(dataset$country)
dataset$country %>% levels()

# checking for any missing data
dataset %>% is.na() %>% sum

# sub-setting the dataset to include only data from country A
country_a_sales <- dataset %>% filter(country=="A")
str(country_a_sales)

# sub-setting the dataset to include only data from country B
country_b_sales <- dataset %>% filter(country=="B")
str(country_b_sales)

################# Full dataset EDA #####################

# summary statistics of the full dataset
summary(dataset)

# descriptive statistics of the full dataset
stat.desc(dataset)

dataset %>%
  skim()

# bar chart of ice cream sales by season
ggplot(data=dataset, mapping=aes(x=seasons,y=icecream_sales)) +
  stat_summary(fun.data = mean_sdl,geom='bar') +
  xlab("Season") +
  ylab("Ice cream sales (�)") + 
  ggtitle("Mean ice cream sales by season in country A and B by season") +
  facet_wrap( ~ country) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))

# skimming the data by country
dataset %>%
  dplyr::group_by(country) %>%
  skim()

# correlation coefficients
dataset %>%
  select(icecream_sales,income,price,temperature) %>%
  cor() %>%
  knitr::kable(
    caption = "Correlation between sales, income, price, temperature",
    digits = 3,
    booktabs = TRUE)

# correlation matrix for the numerical variables
ggcorr(dataset, palette = "RdYlGn", name = "Correlation",
       label = TRUE, hjust=0.75, size=4) + ggtitle("Correlations matrix")

# bar chart of mean ice cream sales in country A and B
ggplot(data=dataset, mapping=aes(x=seasons,y=icecream_sales, colour=seasons)) +
  stat_summary(fun.data = mean_sdl,geom='bar') +
  xlab("Season") +
  ylab("Ice cream sales (�)") + 
  ggtitle("Average ice cream sales, by season, in country A and B") +
  facet_wrap( ~ country) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))

# skimming the data by seasons 
dataset %>%
  dplyr::group_by(seasons) %>%
  skim()

summarytools::dfSummary(dataset)


################ Country A EDA ####################

# summary statistics for country A
summarytools::dfSummary(country_a_sales)

# descriptive statistics 
stat.desc(country_a_sales)

# correlation coefficient for country A numerical variables
country_a_sales %>%
  select(icecream_sales,income,price,temperature) %>%
  cor() %>%
  knitr::kable(
    digits = 3,
    caption = "Correlation between sales, income, price, temperature",
    booktabs = TRUE)

# correlation matrix for country A numerical variables
ggcorr(country_a_sales, palette = "RdYlGn", name = "Correlation",
       label = TRUE, hjust=0.75, size=4) + ggtitle("Correlations for Country A")

# histogram of ice cream sales in country A 
hist(x = country_a_sales$icecream_sales, breaks = 25, main = "Ice cream sales in country A",
     xlab = "Ice cream sales (£)")

# boxplot of ice cream price in country a 
boxplot(x = country_a_sales$price,
        main = "Boxplot of ice cream price in country A",
        ylab = "Cost (£)",
        xlab = "Price of ice cream",
        col = "red")

################ Summary statistics Country B EDA ####################

# summary statistics for country B
summarytools::dfSummary(country_b_sales)

# descriptive statistics for country B
stat.desc(country_b_sales)

# correlation coefficients for numerical variables in country B
country_b_sales %>%
  select(icecream_sales,income,price,temperature) %>%
  cor() %>%
  knitr::kable(
    digits = 3,
    caption = "Correlation between sales, income, price, temperature",
    booktabs = TRUE
  )

# correlation matrix for numerical variables in country B
ggcorr(country_b_sales, palette = "RdYlGn", name = "Correlation",
       label = TRUE, level_colour = "Black", hjust=0.75, size=4) + ggtitle("Correlations for Country B")

# histogram of ice cream sales in country B 
hist(x = country_b_sales$icecream_sales, breaks = 25, main = "Ice cream sales in country B",
     xlab = "Ice cream sales (�)")

# box plot of the price of ice cream in country B
boxplot(x = country_b_sales$price,
        main = "Boxplot of ice cream price in Country B",
        ylab = "Cost (�)",
        xlab = "Price of ice cream",
        col = "blue")


################## Graphical comparisons EDA ###################

# visualising the association between temperature and sales 
ggplot(dataset, aes(x = temperature, y = icecream_sales)) +
  geom_point() +
  labs(x = "Temperature (C)", y = "Sales (£)", title = "Relationship between temperature and sales") +
  geom_smooth(method = "lm", se = FALSE)

# visualising the association between price and sales 
ggplot(dataset, aes(x = price,y = icecream_sales)) +
  geom_point() +
  labs(x = "Price (£)", y = "Sales (£)", title = "Relationship between price and sales") +
  geom_smooth(method = "lm", se = FALSE)

# visualising the association between income and sales 
ggplot(dataset, aes(x = income,y = icecream_sales)) +
  geom_point() +
  labs(x = "Average income per year (£)", y = "Sales (£)", title = "Relationship between income and sales") +
  geom_smooth(method = "lm", se = FALSE)

# Ice cream sales by temperature in country A and country B
ggplot(data = dataset,
       aes(x = temperature, y = icecream_sales, by=country, colour = country)) +
  geom_point() +
  xlab("Temperature (C)") +
  ylab("Ice cream sales (£)") +
  title("Ice cream sales by temperature in country A and B") +
  facet_wrap( ~ country) +
  stat_smooth(method=lm, se=FALSE)

# scatter plot of temperature and price by country
ggplot(data = dataset, 
       aes(x = temperature, y = price, by=country, colour = country)) + 
  geom_point() + 
  facet_wrap( ~ country) + 
  xlab("Temperature (C)") + 
  ylab("Price (£)") + 
  ggtitle("Ice cream price by temperature") +
  stat_smooth(method=lm, se=FALSE)

# Box plot of ice cream prices in country A vs country B
ggplot(data = dataset,
       aes(y = price, by=country, colour = country)) +
  geom_boxplot() +
  ylab("Price of ice cream (£)") +
  xlab("Country") +
  facet_wrap( ~ country) +
  title("Ice cream prices in country A and country B")

# box plot of average income by country
ggplot(data = dataset, 
       aes(x = country, y = income, by=country, colour = country)) + 
  geom_boxplot() + 
  xlab("Country") + 
  ylab("Average income (£ per year)") + 
  ggtitle("Average income by country")

# temperature by country
ggplot(dataset) +
  geom_bar(aes(x = temperature, colour=country)) +
  ggtitle("Temperature by country") +
  xlab("Temperature (C)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~country)

# boxplot of ice cream sales by season 
ggplot(data = dataset, aes(x = seasons, y = icecream_sales, colour=seasons)) +
  geom_boxplot() +
  xlab("Seasons") +
  ylab("Ice cream sales (£)") +
  stat_summary(fun = mean, colour = "black", geom = "point", shape = 1, size = 3)   


################# B. Hypothesis testing ################

# Explanatory variable = Country | Response variable = Ice cream sales 

by(dataset$country, length)

# H0 (null hypothesis): Average ice cream sales in country A = Average ice cream sales in country B
# H1 (alternative hypothesis): Average ice cream sales in country A ??? Average ice cream sales in country B

# average ice cream sales by country
ggplot(data = na.omit(dataset), 
       aes(x = country, y = icecream_sales, colour = country))  +   
  geom_boxplot() + xlab("Country")      +   
  ylab("Ice cream sales (£)")                       +        
  ggtitle("Average ice cream sales by country")  +  
  stat_summary(fun = mean, colour = "black", geom = "point", shape = 1, size = 3)   

# checking the conditions using inference 
statsr::inference(y=icecream_sales, x=country, data=dataset,
                  statistic = c('mean'),
                  type=c('ci'),
                  null=0,
                  alternative=c('twosided'),
                  method=c('theoretical'),
                  conf_level = 0.95,
                  order = c("Country_A","Country_B")
                  )

## As the confidence level does not include the null value (0), we can say that
## the difference observed is statistically significant and different from 0.

# performing a t-test hypothesis test 
statsr::inference(y=icecream_sales, x=country, data=dataset,
                  statistic = c('mean'),
                  type=c('ht'),
                  null=0,
                  alternative=c('twosided'),
                  method=c('theoretical'),
                  conf_level = 0.95,
                  order = c("Country_A","Country_B"))

## t-value = 8.59
## p-value < alpha = 0.05

## so conclude that the difference between means is statistically significant 
## at the 95% confidence level
## Reject the null hypothesis, accept the alternative hypothesis 


########################## C. Modelling ###########################

## outcome variable: ice cream sales
## explanatory variables: income, price, temperature, season, country

####

# visualising the relationship of the numerical outcome variable with the explanatory variables 

# association between income and sales by country
income_sales_plot <- ggplot(dataset, aes(x = income, y = icecream_sales)) +
  geom_point() +
  labs(x = "Income (£ per year)", y = "Ice cream sales (£)", title = 
         "Relationship between income and ice cream sales") +
  geom_smooth(method = "lm", se = FALSE)
income_sales_plot

# association between price and sales by country
price_sales_plot <- ggplot(dataset, aes(x = price, y = icecream_sales)) +
  geom_point() +
  labs(x = "Price of ice cream (£)", y = "Ice cream sales (£)", title = 
         "Relationship between the price of ice cream and ice cream sales") +
  geom_smooth(method = "lm", se = FALSE)
price_sales_plot

# association between temperature and sales by country
temp_sales_plot <- ggplot(dataset, aes(x = temperature, y = icecream_sales)) +
  geom_point() +
  labs(x = "Temperature (C)", y = "Ice cream sales (£)", title = 
         "Relationship between temperature and ice cream sales") +
  geom_smooth(method = "lm", se = FALSE)
temp_sales_plot

####

# fitting the multiple linear regression model
sales_regression <- lm(icecream_sales ~ income + price + temperature + country + seasons, data = dataset)

# obtaining summary statistics of the model 
summary(sales_regression)

# baselines for the categorical variables: Country A (countryA) & Autumn (seasonsAutumn)

####

# Predicted ice cream sales in a location in Country A with an average income of  £20,000.

# using the the season 'Autumn' as it is the baseline in the regression model. Setting price and temperature to 0.
prediction1 <- data.frame(country = 'A', income = 20000, price = 0, temperature = 0, seasons = 'Autumn')
prediction1
sales_regression <- lm(icecream_sales ~ income + price + temperature + country + seasons, data = dataset)
predict(sales_regression, prediction1)

# £673

####

# Predicted ice cream sales in a location in Country B with an average income of  £30,000.

# using the the season 'Autumn' as it is the baseline in the regression
prediction2 <- data.frame(country = 'B', income = 30000, price = 0, temperature = 0, seasons = 'Autumn')
prediction2
sales_regression <- lm(icecream_sales ~ income + price + temperature + country + seasons, data = dataset)
predict(sales_regression, prediction2)

# £924

# Q. Predicted difference in sales
sum(predict(sales_regression, prediction1) - predict(sales_regression, prediction2))

####

# Q. All else being equal, what is the predicted change in ice cream sales if the price 
# goes up by  £0.50 and temperature goes up by 2 degrees at the same time?

# using country A and Autumn as they are the baselines
prediction3 <- data.frame(country = "A", income = 0, price = 0.50, temperature = 2, seasons = 'Autumn')
prediction3
sales_regression <- lm(icecream_sales ~ income + price + temperature + country + seasons, data = dataset)
predict(sales_regression, prediction3)

# A.  £392

####

# Q. What percentage of the model is explained by the variance?

summary(sales_regression)
anova(sales_regression)

# A. 44% of the model is explained by the variance (Multiple R-squared value of 0.4427)

####

# Q. Is this model statistically significant 

# A. The model is statistically significant as the p-value is < 2.2e-16 which is 
# close to zero and less than 0.05

####

# Q. What are the confidence intervals of coefficients on explanatory variables 
# at a 90% confidence level? Explain what they mean
confint(sales_regression, level = 0.90)
# 5% is the lower value. 95% is the upper value 
# A. 95% level: Positive for temperature, Country B, Spring, Summer 
# A. 5% level: Positive for income, temperature, country B, Spring, Summer 


# Q. Test and explain if data meets the regression conditions

# looking for a random scatter around zero 

# Regression
income_price_temp_model <- lm(icecream_sales ~ income + price + temperature, data = dataset)

## Linearity
# Residual plots of income 
plot(income_price_temp_model$residuals ~ dataset$income)

# Residual plots of price 
plot(income_price_temp_model$residuals ~ dataset$price)

# Residual plots of temperature 
plot(income_price_temp_model$residuals ~ dataset$temperature)


## Investigating near-normal residuals  

# sum of squares obtained using anova analysis 
anova(income_price_temp_model)

# Residual plot: linearity and independence 
plot(income_price_temp_model$residuals)
# no increasing or decreasing pattern, so it appears that the 
# residuals are independent from each other 

# Normality: is is left or right skewed?
hist(income_price_temp_model$residuals) 
#normal distribution, very slighty skewed to the right 

# investigating the normality using a normal probability plot (Q-Q plot)
qqnorm(income_price_temp_model$residuals)
qqline(income_price_temp_model$residuals)
# some data points deviate from the straight line, at the upper and lower tails
# but overall there is no huge deviance so the condition can be considered satisfied

# Checking for constant variability (homoskedasticity)
plot(income_price_temp_model$residuals ~ income_price_temp_model$fitted)
abline(h=0,col = "blue", lty = 1, lwd = 3)
# expect a randomly scattered residuals around zero without any obvious 
# patterns, like a fan-shape. The plot shows no increasing or decreasing patterns 
# so we can determine that the residuals are independent of each other. 

##################### D. Prediction #######################


# Q. What is the predicted value of ice cream sales in a location in Country A where 
# the average income of residents is  £30,000, the temperature is 23 degrees in Spring, 
# and average price per serving of ice cream is  £3?

prediction4 <- data.frame(country = "A", income = 30000, price = 3, temperature = 23, seasons = 'Spring')
prediction4

sales_regression <- lm(icecream_sales ~ income + price + temperature + country + seasons, data = dataset)
predict(sales_regression, prediction4, interval = "prediction")

# A. £766.69


# Q. Quantifying the uncertainty with a 95% confidence level
prediction4 <- data.frame(country = "A", income = 30000, price = 3, temperature = 23, seasons = 'Spring')
prediction4
# ***** In pounds  £ *****
sales_regression <- lm(icecream_sales ~ income + price + temperature + country + seasons, data = dataset)
predict(sales_regression, prediction4, interval = "prediction", level = 0.95)

# A. Lower = £289.59    Upper = £1244.13


####################### End ####################################

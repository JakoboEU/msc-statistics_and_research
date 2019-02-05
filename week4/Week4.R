# Date: 05/02/2019 
# Name: James Richardson
# Data: Various
# Analysis: Testing for similarities: Correlation and linear regression

# Can data be visualised by an xy scatter plot?
#  - Is relationship linear?
#  - Does the value of y depend on the value of x?
#  - How strong is the relationship? (form perfect line)
#  - No relationship? (random collection of points)
#
# Regression -> one continuous variable impacts the other, causation is implied
# Response (Y - dependent variable) and Explanatory (X - independent variable) variables
#
# Correlation coefficient (r)
#  - a measure of the degree of linear association
#  - r can vary between -1 and +1 -> perfect correlation = 1 or -1 -> No correlation = 0
#
# Outlier have huge influence
# Assumptions:  variables are normally distributed
#               non-parameteric - spearman, kendall
#
# Hypothesis testing - p-values
#     Null hyphothesis, however signficance is based on number of observations
#       (i.e. very small correlation can be signifcant for large number of results)
#
# Regression minimises residuals -> distance of points away from line
#
# R-Squared says how well line fits data:
#  - high R-Squared value when data fits line well (or more data)
#  - R-Squared only for linear relationship
#
# Simple linear regression between two variables implies that we think there is a causal relationship between the two 
#                       – i.e. manipulating one variable directly results in the change of the other, 
#                         e.g. light intensity and rate of photosynthesis.
# Correlations are used when we do not have an expectation of a causal relationship. 
#                       However, just because there doesn’t need to be a causal relationship, 
#                       doesn’t mean we should always correlate things. 
#
# -------------------------------------------------------------------------------
# Name                        Data type                               Test type
# -------------------------------------------------------------------------------
# Spearman rank correlation   Two continuous (or ordinal) variables   Non parametric
# Pearson’s correlation       Two continuous variables                Parametric (when normally distributed)
# Linear regression           Two continuous variables                Parametric (when 'y' normally distributed)
#
#

#########
# Set up
#########

# clear r console
rm(list=ls())
# set working directory
setwd("/Users/jamese.richardson/Projects/Masters/statistics_and_research/week4")
# list files in working directory
list.files() 

######################
# Spearman correlation
#######################
# Squirrels
# The data are the number of offspring produced by brother and sister grey squirrels. 
# We want to know if there is a relationship between the number of offspring produced by siblings, 
# because different forces can influence reproductive success (rs) in each respective sex. But does success run in the family? 

#####################
# Data: grey squirrel.csv
# Analysis: Spearman correlation
# Null Hypothesis: There is no correlation between the number of offspring produced by siblings
#####################

squirrels <- read_csv("grey squirrel.csv")
#Parsed with column specification:
#  cols(
#    brother.rs = col_double()
#    sister.rs = col_double(),
#  )

# 2) Carry out some exploratory data analysis – are the variables normally distributed?

names(squirrels) # [1] "sister.rs"  "brother.rs"
str(squirrels)

mean(squirrels$sister.rs) # [1] 11.1
sd(squirrels$sister.rs) # [1] 5.625519
mean(squirrels$brother.rs) # [1] 4.89
sd(squirrels$brother.rs) # [1] 2.219723

shapiro.test(squirrels$sister.rs) # W = 0.9526, p-value = 0.001235 <- Significantly different from normal

shapiro.test(squirrels$brother.rs) # W = 0.94783, p-value = 0.0005986 <- Significantly different from normal

# 3) Make a scatterplot of the data,
ggplot(squirrels, aes(x=brother.rs, y=sister.rs)) + 
  geom_point()

# What test to use? 
# -----------------
# testing for a relationship, data are not normally distributed, no expectation of a causal relationship. 
# So we need to use a Spearman correlation.

## we will use the function cor.test. 
# But the default is to do a Pearsons correlation, so we need to add in the argument method=”spearman”
cor.test(squirrels$brother.rs, squirrels$sister.rs, method="spearman")
# Spearman's rank correlation rho
#
# data:  squirrels$brother.rs and squirrels$sister.rs
# S = 76404, p-value = 5.966e-09
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.5415275 

# rho is the correlation coefficient. It describes the strength and direction of the relationship. rho ranges from -1 to +1.
# • If rho is 0 there is no tendency for y to increase or decrease with x – so there is no correlation.
# • The closer to either -1 or +1 the more of the variation in y is described by the variation in x – so they are strongly correlated.
# • If rho is +ve, y increases when x increases – positive correlation.
# • If rho is –ve y decreases when x increases – negative correlation.

# So from our output we can see that there is a significant positive correlation, so we can
# write:
# RESULTS STATEMENT:
# ------------------ 
# There was a significant positive correlation between the reproductive success of brother 
# and sister grey squirrels (Spearmans; n=100, p<0.001, rho=0.54)

#####################
# Data: "UN_data.csv"
# Analysis: Spearman correlation
# Null Hypothesis: There is no correlation between the GDP and infant mortality
#####################
un_data <- read_csv("UN_data.csv")
# Parsed with column specification:
#   cols(
#     `Country Name` = col_character(),
#     `Country Code` = col_character(),
#     Forest = col_double(),
#     infant.mort = col_double(),
#     Fertility = col_double(),
#     domestic_fw = col_double(),
#     CO2.emissions = col_double(),
#     gdp = col_double(),
#     region = col_character()
#   )

# This dataset includes nine development variables in 208 countries:
# • Forest: Area of forest in sq km
# • infant.mort: Mortality rate of infants (per 1,000 live births)
# • Fertility: Fertility rate, total (births per woman)
# • Domestic_fw: Annual freshwater withdrawals, domestic (% of total freshwater withdrawal)
# • CO2.emission: CO2 emissions (metric tons per capita)
# • gdp: GDP per capita
# • region: world regions

# Start by doing some inital data exploration. This exploration should enable you to answer the following questions: 
# [hint: you might find na.rm=TRUE useful]. See link for more information on missing data https://www.statmethods.net/input/missingdata.html
# • What is the mean infant mortality in Asia and Europe?
# • Which country has the maximum CO2 emission?
  
names(un_data)
str(un_data)

unique(un_data$region)
mean(un_data[region=="asia"]$infant.mort, na.rm=T)
mean_na <- function(x) mean(x, na.rm=T)
tapply(un_data$infant.mort, un_data$region, FUN=mean_na)
# africa      asia      aust      c_us        eu      n_us      s_us 
# 54.282000 22.778000 23.561538 17.133333  5.102326  5.400000 22.253846 
tapply(un_data$infant.mort, un_data$region, FUN=mean, na.rm=T)
tapply(un_data$infant.mort, un_data$region, FUN=function(x) mean(x, na.rm=T))


max_na <- function(x) max(x, na.rm=T)
tapply(un_data$CO2.emissions, un_data$region, FUN=max_na)
# africa      asia      aust      c_us        eu      n_us      s_us 
# 12.183667 44.018926 16.519210 37.140054 20.897812 17.020216  8.907242 

ggplot(un_data, aes(x=gdp, y=infant.mort)) + 
  geom_point()

# The data on both axes range by several orders of magnitude and so it would be most appropriate to plot them on a log scale.
un_data$log_gdp<-log(un_data$gdp)
un_data$log_infant.mort<-log(un_data$infant.mort)

ggplot(un_data, aes(x=log_gdp, y=log_infant.mort)) + 
  geom_point()

# Now, test the new variables for normality. 
# If they are normally distributed, we can use a Pearson correlation and if not we have to use a Spearman.
shapiro.test(un_data$log_gdp) # W = 0.97497, p-value = 0.001807 <- Significantly different from normal
shapiro.test(un_data$log_infant.mort) # W = 0.9615, p-value = 4.209e-05 <- Significantly different from normal

cor.test(un_data$log_gdp, un_data$log_infant.mort, method="spearman")

# Spearman's rank correlation rho

# data:  un_data$log_gdp and un_data$log_infant.mort
# S = 1893800, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# -0.794652 

nrow(un_data) # [1] 208

# RESULTS STATEMENT:
# ------------------ 
# There was a significant negative correlation between GDP and infant mortality rates 
# (Spearmans; n=208, p<0.001, rho=-0.795)

######################
# Pearsons correlation
######################
# Download and read into R the datafile "USArrests.csv". 
# This dataset contains the number of arrests per 10,000 people for Murder, Assault and Armed Robbery in US states,
# and the percentage of the population that lives in urban areas.

USArrests <- read_csv("USArrests.csv")
# Parsed with column specification:
#   cols(
#     X1 = col_character(),
#     Murder = col_double(),
#     Assault = col_double(),
#     UrbanPop = col_double(),
#     Armed_rob = col_double()
#   )
# Warning message:
#   Missing column names filled in: 'X1' [1] 

# We are interested in how the number of arrests for different crimes are related to the proportion of the 
# population that lives in urban areas. Let’s start with the arrests for murder.

# Do some preliminary data exploration and assess each of the variables for normality. 
# If they are not normally distributed, does transforming by taking the square root or the log of the data make it normal?
str(USArrests)
mean(USArrests$Murder)  
sd(USArrests$Murder)  
mean(USArrests$UrbanPop)
sd(USArrests$UrbanPop)

shapiro.test(USArrests$Murder) # W = 0.95703, p-value = 0.06674 <- NOT Significantly different to normal
shapiro.test(USArrests$UrbanPop) # W = 0.97714, p-value = 0.4385 <- NOT Significantly different to normal

shapiro.test(sqrt(USArrests$Murder)) # W = 0.97494, p-value = 0.3629
shapiro.test(log(USArrests$Murder)) # W = 0.94455, p-value = 0.02047

# Plot a scattergraph of the proportion of the urban population and the arrests for murder. 
# Do you think that there is a correlation?
  
ggplot(USArrests, aes(x=UrbanPop, y=Murder)) + 
  geom_point()

ggplot(USArrests, aes(x=UrbanPop, y=sqrt(Murder))) + 
  geom_point()

# Test for correlation and report the results. 
# Since we are testing for a relationship between two variables that are normally distributed, 
# we can use a Pearsons correlation. The default cor.test function is for a Pearsons, 
# so you don’t need to state a method argument.


cor.test(USArrests$UrbanPop, USArrests$Murder)
# Pearson's product-moment correlation
# 
# data:  USArrests$UrbanPop and USArrests$Murder
# t = 0.48318, df = 48, p-value = 0.6312
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# -0.2128979  0.3413107
# sample estimates:
# cor 
# 0.06957262 

nrow(USArrests)

# RESULTS STATEMENT:
# ------------------ 
# There is not a significant correlation between urban population and murders in the USA
# (Pearson's; n=50, p = 0.6312, rho=0.0696)
# 

######################
# Regression
######################
# Download and read into R the dataset "sperm.csv". 
spermnum <- read_csv("sperm.csv")
# Parsed with column specification:
#   cols(
#     spermnum = col_double(),
#     time = col_double()
#   )


# The data are counts of sperm transferred during matings (spermnum) 
# that were disturbed at different times (minutes) in yellow dung flies (time). 
# We want to know what effect duration of mating has on the amount of sperm transferred to females.
# We can assume that there would be a causal relationship between sperm number and the length of mating duration, 
# so we should use a regression.
# Which is the response variable? Make a histogram of the response variable – is it normally distributed? 
# Test statistically if in doubt.
shapiro.test(spermnum$spermnum) # W = 0.98532, p-value = 0.3347 <- NOT Significantly different to normal
ggplot(spermnum, aes(x=spermnum)) + 
  geom_histogram(bins=10)

# Time needn't be normal for this test as it is the explanatory variable.
# Make a scatterplot of the data, either using the plot() function or try it in ggplot2 (hint: use geom_point()).
# Make sure to label it appropriately. 
# Are the variables on the correct axis?
ggplot(spermnum, aes(x=time, y=spermnum)) + 
  xlab("Amount of time until disturbed (minutes)") +
  ylab("Number of sperm transferred") + 
  geom_point()


# Now we are going to carry out a linear regression to see if there is a significant relationship between sperm and time.
# The test statistic is r-squared, the regression coefficient. 
# This is a quantity describing the magnitude that the x variable explains variation in the y variable 
# - it can range from -1 to +1.

# Linear regression of sperm number and time – note the order of the variables 
LR1<-lm(spermnum ~ time, data=spermnum)
## This gives us the intercept and slope, but no statistical test. 
# For that we can display the linear regression results in a table using summary().
summary(LR1)

# Call:
#   lm(formula = spermnum ~ time, data = spermnum)

# Residuals:
#   Min     1Q Median     3Q    Max 
# -16003  -5382   -138   3217  32549 
# 
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     6192.8        1605.3   3.858 0.000205 ***
#   time          3877.0         265.2  14.619  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 7559 on 98 degrees of freedom
# Multiple R-squared:  0.6856,	Adjusted R-squared:  0.6824 
# F-statistic: 213.7 on 1 and 98 DF,  p-value: < 2.2e-16

# A standard result statement would be something like.... 
# RESULTS STATEMENT:
# ------------------ 
# We found that mating duration was a significant predictor of the number of spermium transferred during mating 
# in yellow dung flies (R2=0.682, P<0.001).

# You might also quote the intercept and slope of your model, we will cover these next week.

# Now, add a trendline to your scatterplot using abline() and the model object (LR1)


ggplot(spermnum, aes(x=time, y=spermnum)) + 
  xlab("Amount of time until disturbed (minutes)") +
  ylab("Number of sperm transferred") + 
  geom_point() + 
  stat_smooth(method = "lm", col = "red")

# Or in simple plot
plot(spermnum ~ time, data = spermnum)
abline(LR1)

plot(spermnum$spermnum ~ spermnum$time)
abline(LR1)

# Next week we will look at this in more detail and start using the model equation to predict values 
# and also check the model validation. For now it is enough to understand
# 1) why you would use this over a correlation when examining relationships between variables
# 2) what statistics you should report in a results statement and
# 3) what the R-squared value means
# For further explanation on R-squared check out the following link:
# http://blog.minitab.com/blog/adventures-in-statistics/regression-analysis-how-do-i-interpret-r- squared-and-assess-the-goodness-of-fit


# Correlation matrices and plots
# Next week we will be moving onto forms of multiple regression. 
# Often your datasets will contain multiple variables that may be correlated to one another 
# and strong intercorrelations make it hard to identify which variables are having effects on your response variable. 
# If you only have four or so variables, it is easy to carry out all the pairs of correlations. 
# But it is a big problem when there are lots of variables (like more than 10 or so) – 
# testing all the pairs would take ages!
#   So we use correlation plots and matrices.
# Let’s investigate the correlations between all the continuous variables in the UN dataset.

## First make a data object with just the columns 3-8 inclusive (all the continuous variables) 
# and at the same time remove all the NAs
un2<-na.omit(un_data[,3:8])
### Then make scatterplots of each pair of variables. 
pairs(un2)

# Now use the cor() function on your data object to print the correlation matrix.
# What are the two most highly correlated variables?
cor(un2)

#                   Forest      infant.mort   Fertility domestic_fw CO2.emissions gdp
# Forest             1.00000000 -0.05640972 -0.07147079 -0.04228505    0.09234652  0.01349488
# infant.mort       -0.05640972  1.00000000  0.86629122  0.07550798   -0.45099655 -0.48982500
# Fertility         -0.07147079  0.86629122  1.00000000  0.06028423   -0.40983597 -0.40432721
# domestic_fw       -0.04228505  0.07550798  0.06028423  1.00000000    0.12587174  0.12557108
# CO2.emissions      0.09234652 -0.45099655 -0.40983597  0.12587174    1.00000000  0.43933329
# gdp                0.01349488 -0.48982500 -0.40432721  0.12557108    0.43933329  1.00000000
            
# infant mortality and fertility

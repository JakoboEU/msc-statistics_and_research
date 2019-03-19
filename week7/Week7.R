# Date: 12/03/2019 
# Name: James Richardson
# Student Number: 18057447

# Analysis: Power Analysis

# Script Set Up
# -------------
# clear r console
rm(list=ls())
# set working directory
setwd("/Users/jamese.richardson/Projects/Masters/statistics_and_research/week7")
# list files in working directory
list.files() 


# Aims of this practical
# ----------------------
# 1. Simulate different data types (normal, poisson, binomial) 
# 2. Manipulation of dataset
#     * Renaming and recoding variables 
#     * Converting between data types 
#     * Long and wide formats
# 3. Power analysis


# Simulating a normal distribution
# --------------------------------
# The following basic commands will be useful for most of you in generating data for your Research Poster. 
# Don’t forget you may need a poisson distribution for count data. 
# If you are testing for differences between factors, you can simulate two or more continuous variables for groups separately 
# and set the means and SD to be different.

# Makes a vector of values with a normal distribution where n=number of observations you want # to simulate.
n1<-rnorm(n=100, mean = 5, sd = 1)
n1

# Makes a vector of values with a Poisson distribution where n=number of observations you 
# want to simulate and lambda is the mean.
p1<-rpois(n=100, lambda = 3)
p1

# Makes a vector of values with a binomial distribution where n=number of observations you
# want to simulate, p=the probability of success in each trial (must be value from 0-1) and size is 
# the number of trials (e.g. if you set this to 1 the values will be either 1 or 0; if you set this to 10 # the values will be between 0-10).
b1<-rbinom(n=100, size=2, prob=0.75)
b1


# Simulating sequence data
# Makes a sequence of data from 0-100, every 5 (e.g. 0, 5, 10) 
s1<-seq(0, 100, by=5 )
s1
# A sequence of 10 values (length.out) evenly spaced between 0-10 
s2<-seq(0, 10, length.out= 10)
s2
# Repeats 1 2 3 4 5 two times 
s3<-rep.int(1:5,2)
s3
# Repeats “cat, dog” fifty times 
s4<-rep(c("cat","dog"),50)
s4


# Now create new data called s5 that contains the length of coat each animal has (long, short, medium, curly), you need 25 of each category.
s5<-rep(c("long","short","medium","curly"),25)
s5
length(s5)

# Simulating relationships
# ------------------------
# Remember from the testing relationships practical that Y=intercept+(slope*X). 
# Using the linear equation Y=mx+c where m=slope and c=intercept, we can create some relationship dummy data.

# Simulating relationship data
n2<-rnorm(n=100, mean= 50, sd=5) # simulate your first variable
n2
# Then simulate your y data using a y=mx+c linear equation. Below I have set c to be 10 and m to be 0.5. 
# You can change the slope and the intercept to be appropriate to your study. 
# Changing the + slope to a –slope will make it a negative relationship. You also need to add on some
# variation in the relationship to make it realistic, see below (i.e. your R2 isn’t going to be 1!).
y1 <- 10+0.5*n2 + rnorm(100,0,5) #Noisy relationship, low R2 
y1
y2 <- 10+0.5*n2 + rnorm(100,0,2) #Neater, moderate R2
y2
y3 <- 10+0.5*n2 + rnorm(100,0,1) #Neat, high R2
y3

# Plot your relationships and check the R-squared values for the linear relationships by running a linear model for each relationship (i.e. n2~y1)
plot(y1~n2)
plot(y2~n2)
plot(y3~n2)

# -----------------
# Data manipulation
# -----------------

# Dataframes: renaming variables
# ------------------------------

# Try adding together, in a data frame, some of the variables you have just created (n1, p1, b1, s3, s4, s5, y1, y2, y3) 
# and call the data frame “myData” (hint: use the data.frame function)
myData <- data.frame(n1, p1, b1, s3, s4, s5, y1, y2, y3)
myData
# Using R’s built in functions rename a column name: change "n1" to "normal"
names(myData)[names(myData)=="n1"] <- "normal"
head(myData) # check using “head” to see if this is what you expected names(myData) # can also check using “names”
# Rename by index in the names vector of your dataframe: change the second item, "p1", to "poisson".
names(myData)[2] <- "poisson"
head(myData)
# You could also use the helpful "plyr" package which makes renaming things very easy, search for the "rename" function in the plyr package and try renaming the b1 variable to "binomial".
library(plyr) # load plyr package 
myData<-rename(myData, c("b1"="binomial")) 
head(myData)

# Dataframes: converting between data types
# -----------------------------------------
# Use the “str” function to view the data types in your dataframe, what are they?
# For some analysis we may need to change between data types (e.g. numbers to factors). 
# There are several ways to do this. 
# Using the following code we can easily change our binomial data to a factor.

myData$binomial <- as.factor(myData$binomial) 
str(myData)

# Try changing the “normal” data column to an integer. What has this done to your numeric data?
myData$normal <- as.integer(myData$binomial) 
str(myData)


# Another handy “plyr” function is the “revalue” function for recoding variables. 
# Use the code below, what has this done to your dataframe?
# Recoding using the plyr package
myData$new.s4 <- revalue(myData$s4, c("cat"="1", "dog"="2"))
str(myData)

# Again there are several other ways to do this in R, it’s a matter of finding one you prefer. 
# See the R Cookbook website ‘Recoding data’ page for different methods. 
# Go to the website and find out how you would do this using R’s built in functions?

# Now lets recode a continuous variable to a categorical variable.
# Recode poisson variable above and below 4 to "small" and "large" 
myData$category[myData$poisson< 4] <- "small" 
myData$category[myData$poisson>=4] <- "large"
# Convert the column to a factor
myData$category <- factor(myData$category) 
head(myData)
str(myData)

# Finally, remember R can be used as a very overly complicated calculator, 
# lets create a new variable called “calc” and add y1 and y2 multiplied by y3
myData$calc <- (myData$y1 + myData$y2) * myData$y3 
head(myData)
str(myData)

# Restructuring data
# ------------------
list.files()

long <- read_csv("olddata_long.csv")
str(long)

long$subject <- as.factor(long$subject)

# Long to wide format
# --------------------
# The spread function requires three bits of information, 
# * the data object you want to reformat, 
# * the name of the column whose values will be used as column heading, 
# * and the name of the column whose values will populate the new columns.

## Long to Wide format
library(tidyr) 
data_wide <- spread(data=long, key=condition, value=measurement) 
data_wide

# Wide to long format
# -------------------
wide <- read_csv("olddata_wide.csv")
str(wide)
wide

# The gather function requires you to enter four things; 
# * the data object you want to reformat, 
# * the name of the column that will contain factors, 
# * the name of the new column that will contain values, 
# * and the names of the source columns that contain the values.
data_long <- gather(data=wide, key=condition, value=measurement, control,cond1,cond2, factor_key=TRUE)
data_long
str(data_long)



# --------------
# Power analysis
# --------------
# Its very important when undertaking any statistical analysis to understand 
# the size of the effect you are reporting and the power you have in a dataset to find significant results. 
# An experiment with good quality data will not be able to detect a difference, 
# even if one exists, if the sample size is small relative to the magnitude of difference.

# Power tells you the probability you will find a difference when it is actually real
######


# What information you need to conduct power analysis
# 1. The statistical test you will be conducting
# 2. Your significance level (often we use 0.05)
# 3. Power to detect an effect (the standard recommended power is 0.80) 
# 4. Effect size – how big is the change of interest (small, medium, large?) 
# 5. Sample size – easier to detect an effect with larger sample size

# Sample size is normally the thing we want to find out so we can plan our fieldwork or sampling regime. 
# To find sample size we need to find out what type of effect size is likely to be suitable for our particular research system. 
# This is the tricky bit and may require,
# 1. Data from a pilot study (not possible for your posters)
# 2. Looking at the literature to see how big or small differences are, or how small or weak relationships are
# 3. Making a conservative but educated guess


# Power Analysis in R – pwr package
# ---------------------------------
install.packages("pwr")
library(pwr)


# The goal here for every example below will be to calculate the required sample size to achieve power = 0.80 
# (high statistical power).
# To do this, we'll need to guess or calculate an effect size (ES). 
# Doing this is specific to each individual test. 
# There are some functions to help us in pwr.


# T-test: 1 and 2 sample
# ----------------------
# Here we'll compare one sample variable against a test parameter, or, more typically, 
# we'll compare two means separated by a categorical variable with two levels using the pwr.t.test() function.
# We need to know the effect size for a t-test. The effect size is the parameter d. 
# The definition for a two sample t-test for d is:
#     d = |(mean1 - mean2)| / (standard deviation)

# Again, 0.2 is a small ES, 0.5 is medium ES and 0.8 is a large ES. 

# First, what is the power for a one sample t-test with n=60 subjects, and effect size of d = 0.2 # (small) 
# and alpha (p-value) = 0.05?
pwr.t.test(power = NULL, d=0.2, n=60, sig.level=0.05, type = "one.sample", alternative = "two.sided")

# What is the power of a (type=) "paired" t-test with medium ES and n=40.?
pwr.t.test(power = NULL, d=0.5, n=40, sig.level=0.05, type = "paired", alternative = "two.sided")

# What is the power of an experiment for a two-sample, two-sided t-test with n = 30, alpha = 0.05, 
# absolute mean difference of 2 , and pooled standard deviation of 2.8?
d <- 2 / 2.8
d
pwr.t.test(power = NULL, d=0.7142857, n=30, sig.level=0.05, type = "two.sample", alternative = "two.sided")

# Is this good enough to do the experiment? How might you change the experiment to your liking?
  
pwr.t.test(power = 0.8, d=0.7142857, n=NULL, sig.level=0.05, type = "two.sample", alternative = "two.sided")
  

# Correlation
# -----------
# Here, we'll be using pwr.r.test(). Look at the help for pwr.r.test
# Conveniently, the correlation coefficient, r, is itself the ES.
# If r = 0.3 and n = 50, what is the power for a 2-sided (i.e., positive or negative) correlation?

pwr.r.test(r=0.3, n=50, sig.level=0.05 ,alternative="two.sided")

# What about an experiment where your alternative hypothesis is (for justifiable biological reasons) 
# that the correlation coefficient is “greater” than 0?  
pwr.r.test(r=0.3, n=50, sig.level=0.05 ,alternative="greater")

# How many subjects required to detect a (two.sided) correlation with .80 power if the ES is 0.10? 0.30? 0.50?
pwr.r.test(r=0.1, sig.level=0.05 ,alternative="two.sided", power = 0.8)
pwr.r.test(r=0.3, sig.level=0.05 ,alternative="two.sided", power = 0.8)
pwr.r.test(r=0.5, sig.level=0.05 ,alternative="two.sided", power = 0.8)

# Chi square
# ----------
# Here Cohen recommends the ES parameter, w, to be 0.1, 0.3 and 0.5, when power is small, medium and large respectively.
# Let's say you have an experiment looking at the proportion of offspring expected from 3 phenotypes. 
# You expect a large effect size. If your sample size is 200 offspring, what is your power expected to be?
pwr.chisq.test(w=0.5 ,df=(3-1), N=200, power = NULL)

# What sample size do you need for power = 0.8?
pwr.chisq.test(w=0.5 ,df=(3-1), N=NULL, power = 0.8)

# In an experiment looking at the frequency of animals visiting 5 types of animal enrichment objects 
# you expect a small effect size. 
# What sample size of visitation records do you need for high power in this experiment?
pwr.chisq.test(w=0.1 ,df=(5-1), N=NULL, power = 0.8)

# General linear model
# --------------------
# Calculating power for a “regular GLM model is fairly straightforward, 
# but there are a few extra bits of information required. 
# Check the help file 
?pwr.f2.test
# pwr.f2.test(u =, v = , f2 = , sig.level = , power = )
# where u and v are the numerator and denominator degrees of freedom. 
# We use f2 as the effect size measure. 
# Cohen suggests f2 values of 0.02, 0.15, and 0.35 to represent small, medium, and large effect sizes.

# For a simple, one factor model (analogous to one-way ANOVA): 
# * The numerator degrees of freedom is usually the number of levels of a factor minus one 
#   (may be more complicated in more complicated models). 
# * The denominator is the sample size minus the numerator degrees of freedom.

# E.g., in an experiment with one factor that has 4 levels (e.g., population north, south, east, west) 
# measuring body size in 100 individuals (25 from each population), u = 4 - 1 and v = 100 - u.
# Let's calculate power for the experiment above if the expected ES is medium
pwr.f2.test(u = 3, v = 97, f2 = 0.15, sig.level = 0.5, power = NULL)

# How many subjects do we need for 80% power if the ES is small? 
pwr.f2.test(u = 3, f2 = 0.02, sig.level = 0.5, power = 0.8)
# v = 145.3662 -> N = 148

# What about large effect size?
pwr.f2.test(u = 3, f2 = 0.35, sig.level = 0.5, power = 0.8)
# v = 6.149992 -> N = 9





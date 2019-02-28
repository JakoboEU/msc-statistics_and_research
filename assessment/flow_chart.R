##############################################
#
#  Flow Chart
#  --------------
#
#
##############################################

# Testing for normality:
# ----------------------
shapiro.test(data_list) 
# * p > 0.05    -> NOT significantly different from normal
# * p <= 0.05   -> Significantly different from normal

# Test for homogeneity of variances:
# ----------------------------------
bartlett.test(var1, var2) 
# * p > 0.05    -> Variances of two groups are NOT significantly different
# * p <= 0.05   -> Variances of two groups are significantly different


# ---------------------------------------
# ---------------------------------------
# Testing for differences between groups:
# ---------------------------------------
# ---------------------------------------

# Testing between sample and population mean:
# -------------------------------------------
# * one-sample t-test (data normally distributed)
# * (week2/Week2.R)
t.test(data.list, mu=population_mean)

# Testing between two categorical variables:
# ------------------------------------------
# * chi-squared test
# * (week3/Week3.R, full_practice/james_richardson.R[Section 5])
chisq.test(data_table)

# data_table:
# var1 / var2   | A | B | C |
#             X | 3 | 2 | 1 |
#             Y | 6 | 5 | 4 |
#             Z | 9 | 8 | 7 |

# * Fisher’s exact test
# * (week3/Week3.R)
# Fisher’s exact test is used to investigate the associations between two categorical variables, 
# but where expected cell counts are less than 5.
fisher.test(data_table)

# Testing between one categorical variable and one continuous variable:
# ---------------------------------------------------------------------
# - 2 Groups:
# -----------

# -- Data Normal
# --------------  

# --- Data paired:
# ----------------
# * paired t-test
# * (week2/Week2.R)
# When comparing measurements taken from the same subject at two different times.
t.test(before,after, paired=TRUE)

# --- Data NOT paired:
# --------------------
# * two-sample independent t-test
# * (week2/Week2.R)
# Before we can carry out our two-sample t-test, we must be satisfied that the data meet the assumptions of the t-test;
# 1) The data are continuous
# 2) The data are approximately normally distributed
# 3) The variances of the two sets of data are homogeneous (the same)
t.test(continuous~categorical)

# -- Data NOT Normal
# ------------------
# Mann-Whitney U test (aka Wilcoxon) – non-parametric
# TRY TRANSFORMING DATA TO BE NORMAL FIRST -> log(), sqrt() -> if normal then used t.test
# * (week2/Week2.R, practice1/james_richardson_practice2.R)
wilcox.test(continuous~categorical)

# - 3+ Groups:
# ------------

# -- Data Normal
# --------------  
# * ANOVA
# * (week2/Week2.R, practice2/james_richardson_practice3.R, full_practice/james_richardson.R[Section 1])
# Now we need to examine the test assumptions for the ANOVA, which are the same we looked at earlier for t-tests
# 1. The data are continuous
# 2. At least approximately normally distributed
# 3. The variances of the groups are homogenous
anova1<-aov(continuous~categorical_with_3plus_groups, data=dataframe)
summary(anova1)

# Can then use 
library(multcomp)
cld(glht(anova1, linfct=mcp(categorical_with_3plus_groups.as.factor="Tukey")))
# to assign letters to groups

# -- Data NOT Normal
# ------------------
# * Kruskal-Wallis test (non-parametric)
# * (week2/Week2.R, full_practice/james_richardson.R[Section 1], full_practice/Full_Practice_Quiz_Answer_Script.R)
kruskal.test(continuous~categorical_with_3plus_groups, data = dataframe)
# Can then use 
pairwise.wilcox.test(continuous, categorical_with_3plus_groups)
# will show you which groups different from others

# Testing between 2+ categorical variables and one continuous variable:
# ---------------------------------------------------------------------
# * ANOVA Family
# * (week3/Week3.R)
# Two-way ANOVAs are used when we have a continuous, response variable measured across levels of two different categorical factors. 
# Factors would classically be manipulated factors consisting of different levels (e.g., different quantities of a drug). 
# The number of subjects measured would be the same for each level of the factor by design.

# For a 2-way ANOVA, the data need to be arranged into three columns as in this data set. 
# If your categorical data were numbers these would first need to be converted to factors.

interaction.plot(categorical1, categorical2, continuous)

anova2<-aov(continuous~categorical1+categorical2, data=dataframe) 
summary(anova2)

## Model with the main effects of both factors AND interaction 
anova3<-aov(continuous~categorical1*categorical2, data=dataframe) 
summary(anova3)

## Compare the two models 
anova(anova2, anova3)

# If the models aren't different (p > 0.05). Then by default some people would drop the interaction term and accept the simplest model.

# Testing between 2+ categorical variables and one continuous variable for same subject over time:
# ------------------------------------------------------------------------------------------------
# * Repeated measures ANOVA & interaction plots
# * (week3/Week3.R, full_practice/james_richardson.R[Section 2])
anova4 <- aov(continuous ~ categorical1 * categorical2 + Error(subject_id), data = dataframe)
summary(anova4)


# -----------------------------------------
# -----------------------------------------
# Testing for relationships between groups:
# -----------------------------------------
# -----------------------------------------

# - Testing with no cause and effect:
# -----------------------------------

# -- Data Normal
# --------------  
# * Pearson Correlation
# * (week4/Week4.R)
cor.test(continuous1, continuous2)

# More that 2 variables
pairs(dataframe)
cor(dataframe)


# -- Data NOT Normal
# ------------------
# * Spearman Correlation
# * (week4/Week4.R, practice3/james_richardson_practice_3.R)
cor.test(continuous1, continuous2, method="spearman")

# More that 2 variables
pairs(dataframe, method="spearman")
cor(dataframe, method="spearman")


# - Testing WITH cause and effect:
# --------------------------------
# * Regression - Linear Model
# * (week4/Week4.R, week5/Week5_LinearModel.R)
linear_regression <- lm(effect ~ cause, data= dataframe)
summary(linear_regression)

# Validate (4 plots)
plot(linear_regression)

# * Regression - Multiple Linear Model - more than one response variable
# * (week4/Week4.R, week5/Week5_MultipleLinearRegression.R, practice4/james_richardson_practice_4.R, full_practice/james_richardson.R[Section 4])
# Remove strongly correlated variables!
multi.lm <- lm(effect ~ cause1 + cause2 + cause3, data=dataframe) 
summary(multi.lm)
# Remove variables that do not explain effect - and re-run model

# * Regression - Generalised Linear Model
# * (week4/Week4.R, week5/Week5_GeneralisedLinearModel.R, practice4/james_richardson_practice_4.R, full_practice/james_richardson.R[Section 4])
# Remove strongly correlated variables!
glmodel <- glm(effect ~ cause1 + cause2, family = poisson, data = dataframe) 
summary(glmodel)
# Remove variables that do not explain effect - and re-run model




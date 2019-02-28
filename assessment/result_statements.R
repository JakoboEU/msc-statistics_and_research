####################
# One sample t-test
####################

# RESULTS STATEMENT: 
# ------------------
# The energy intake of wild Impala (mean±sd, 6754±1142) was significant difference 
# to the zoo recommended energy intake (7725 Kj/day) (t=-2.821, df=10, p=0.018).

######################
# Two-sample t-test
######################

# RESULTS STATEMENT: 
# ------------------
# The mpg of automatic cars  (mean±sd, 17.14737±3.833966) vs manual cars (mean±sd, 24.39231±6.166504) was significant difference 
#  (t=-3.7671, df=18.332, p=0.001374).


###############
# Paired t-test
###############

# RESULTS STATEMENT: 
# ------------------
# There is a significant difference in blood pressure before (mean±SD, 14.5±2.1) and after (12.9±1.3) exercise (t=6.0561, df=14, p<0.001).


#######################################################
# Mann-Whitney U test (aka Wilcoxon) – non-parametric
#######################################################

# RESULTS STATEMENT: 
# ------------------
# There is a significant difference in the petal width between
# versicolor (mean±SD, 1.326±0.1977527) and setosa (0.246±0.1053856) (p-value < 0.001).

# RESULTS STATEMENT: 
# ------------------
# There is a significant difference in the horn length between
# male (mean±SD, 59.53±4.48) and female (39.84±5.22) wildebeest (p-value < 0.001).

#
# RESULTS STATEMENT: 
# ------------------
# Horn length was significantly different between male and female 
# wildebeest (mean±sd; male=59.5±4.5, female=39.8±5.2, W=5; p<0.001)

# Create a plot to display your result
ggplot(beest, aes(factor(sex), horn)) + 
  geom_boxplot() + 
  theme(axis.title.x = element_blank()) +   # Remove x-axis label
  ylab("Horn length (cm)")

#########################################
# One-way ANOVA (analysis of variance)
#########################################

# RESULTS STATEMENT: 
# ------------------
# Animals paced significantly less with enrichment treatment one (mean+/-SD, 21.61+/-3.5)
# than the other two treamtent types (F=11.82, df=2,31, p=0.002). There was no difference in pacing 
# behaviour between treatment two (30.4+/-5.6) and three (27.4+/-5.4).

# Create a plot to display your result
ggplot(enrich, aes(factor(type), pacing)) + 
  geom_boxplot() + 
  theme(axis.title.x = element_blank()) +   # Remove x-axis label
  ylab("Pacing behaviour (min)")


##################
# Two-way ANOVA
##################

# RESULTS STATEMENT: 
# ------------------
# There is no significant association between the bait used in the trap and the number of moths caught
# p = 0.386, df = 2, mean = 56.5 
# There is a signifcant association between the level of the trap and the number of moths caught
# p < 0.001, df = 3, mean = 660.5
# There is no significant interaction between the bait and the level of the trap
# p = 0.932, df = 6, mean = 19.2

# RESULTS STATEMENT: 
# ------------------
# We used 2-way ANOVA to test the effect of sex and population location on raccoon weight. There was a significant
# main effect of sex on raccoon weight (F=207.97, df=1,95, p<0.001) with females being heavier (mean+/- SD, females
# 41.2+/-0.62, males 39.6+/-0.48). However, population location was not significant (F=0.11, df=3,95, P=0.95). There was 
# no significant interaction between sex and population (F=0.68, p=0.569).

#######################################
# Kruskal-Wallis test (non-parametric)
#######################################

# RESULTS STATEMENT: 
# ------------------
# Petal width was significantly different between all species (Chi=131.19; df= 2; p<0.001).

# RESULTS STATEMENT: 
# ------------------
# Insect density was significantly different in areas treated with different
# insect sprays (F=44.8, df=5,66, p<0.001), with densities significantly 
# lower in treatments C, D and E.

#####################
# Chi-squared test
#####################

# RESULTS STATEMENT: 
# ------------------
# There is a significant association between the training method and the defecation rate.
# (X-squared = 23.52, df = 1, p-value < 0.001).

# RESULTS STATEMENT: 
# ------------------
# There is no significant association between eye colour and gender
# (X-squared = 2.6808, df = 2, p-value = 0.2617).

# RESULTS STATEMENT: 
# ------------------
# There is no significant association between frequency of smoking and frequency of excercise
# (X-squared = 5.4885, df = 6, p-value = 0.4828)

# RESULTS STATEMENT: 
# ------------------
# There was a significant association between class of male passenger and their survival (chi squared
# 37.99, df=3, p<0.001), with more 1st class passengers surviving thann the expected number
# and fewer 2nd class surviving than expected.

######################
# Spearman correlation
######################

# RESULTS STATEMENT:
# ------------------ 
# There was a significant positive correlation between the reproductive success of brother 
# and sister grey squirrels (Spearmans; n=100, p<0.001, rho=0.54)

# RESULTS STATEMENT:
# ------------------ 
# There was a significant negative correlation between GDP and infant mortality rates 
# (Spearmans; n=208, p<0.001, rho=-0.795)

# ---------------------------------
# RESULTS STATEMENT FOR Question 1:
# ---------------------------------
# The three strongest significant correlations in the lichen data are a positive correlation 
# between NOx and NH3 (Spearmans; n=177, p<0.001, rho=0.86), a positive correlation between 
# polution.index and NH3 (Spearmans; n=177, p<0.001, rho=0.80) and a negative correlation between
# growth.rate and NH3 (Spearmans; n=177, p<0.001, rho=-0.78)

######################
# Pearsons correlation
######################

# RESULTS STATEMENT:
# ------------------ 
# There is not a significant correlation between urban population and murders in the USA
# (Pearson's; n=50, p = 0.6312, rho=0.0696)

######################
# Regression
######################

# RESULTS STATEMENT:
# ------------------ 
# We found that mating duration was a significant predictor of the number of spermium transferred during mating 
# in yellow dung flies (R2=0.682, P<0.001).

# RESULTS STATEMENT FOR Question 2:
# ---------------------------------
# We found that the polution index was a significant predictor of the growth rate of lichen (R2=0.5692, P<0.001).

# RESULTS STATEMENT:
# ------------------ 
# 1A) There was a significant relationship between the difference in species richness between de-embanked
# and reference marshes and time, with the difference decreasing with time and approaching zero 
# after approximately 120 years (Fig. 2, R2=21.9%, p=0.05). The species richness of de-embanked sites
# declined significnatly with the percentage cover of Spartina anglica (R2=30.3, p=0.018).

# RESULTS STATEMENT:
# ------------------ 
# Age, weight, and height explained 58% of the variation in body fat (adj Rsq=0.58, 
# F=115, df= 3,249, p<0.001). There is a significnat positive relationship between body fat and age (t=4.838, p<0.001),
# and weight (t=17.124, p<0.001), and a significant negative relationship between body fat and height (t=-8.067, p<0.001).

# RESULTS STATEMENT:
# ------------------ 
# Pollution index was a significant negative predictor of lichen 
# growth rate (cm2/yr) (n=177, R2=0.57, P<0.001). As pollution index increased lichen growth rate reduced.
ggplot(lichen, aes(x=polution.index, y=growth.rate)) + 
  geom_point() +
  geom_smooth(method='lm') +
  labs(y="Growth rate (cm2/year)", x="Pollution index")

# RESULTS STATEMENT:
# ------------------ 
# Body size was found to be a significant predictor of female sheep fitness as measured by 
# the number of offspring (Pseudo-R2=0.435, P<0.001)
ggplot(sheepfit, aes(x=body.size, y=fitness)) + 
  geom_point() +
  ylab("Sheep fitness") +
  xlab("Female body size") +
  geom_line(data=add.these2plot, aes(body.size, fit), linetype=1) +
  geom_line(data=add.these2plot, aes(body.size, pred.plus), linetype=2) +
  geom_line(data=add.these2plot, aes(body.size, pred.minus), linetype=2) 
# DD/MM/YY Your name
# Practice Quiz Question 5
# Data - Coffee yield


####################
###################
## Part 1:
#### a) For each figure write a sentence, suitable for a results section, that describes the contents of the figure [2 marks]
# Canopy index is a significant predictor of coffee crop yeild (R2=0.58, P<0.001) with a positive increase of 0.76 tons for every one unit increase in canopy index
# Arthopod (diversity) is also a significant predictor of coffee crop yeild (R2=0.07, P<0.001) but with a weaker positive relationship (slope = 0.45) 

#### b) Using Fig 1, calculate the crop yield when the canopy index is 10.57. [1 mark]
# Y=intercept+(slope*X)
# crop yield = 6.96399 + (0.76280*10.57)
# crop yield = 15.03

#### c) Briefly state the confidence you would place in the relationships in the figures below and why 
# you think that this is the case.[2 marks]
# High R2 for Fig1 so model fits data well, strong confidence in relationship 
# Very low R2 in Fig2 so model does not fit well to linear relationaship, low confidence in relationship, also wide spread in data around the fitted line.

####################
###################
## Part 2:
#### a) Write a sentence suitable for a results section that describes the findings of this model. [3 marks]
# Results of a multiple linear model
# Overall, canopy index, arthropod diversity, and aridity explained 73% of the variation in crop yield (adj Rsq=0.73, 
# F=109.2, df= 4,159, p<0.001). There is a significnat positive relationship between crop yield and canopy index (t=4.953, p<0.001),
# and arthropod diversity (t=3.014, p=0.003), and a significant negative relationships between crop yield and soil aridity (t=-8.700, p<0.001).
# Soil microbial diversity was not a significant predictor of crop yield.

#### b) What would you do next if you got this output? Describe the procedures you would carry out next [2 marks]
# I would remove the non-significant predictor (microbes) and re-run the model. If all other variables remained significant I
# would look at the model validation plots. I would be looking for the residuals in the q-q plot to fit the line, check for patterns in the
# residual vs fitted plots and I would check that there were no outliers having undue influence on the model (cooks distance).



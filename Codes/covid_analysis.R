# Clear memory and change the scientific notation
rm(list=ls())
options(scipen=999)

# Packages to use
library(tidyverse)
library(scales)
library(lspline)
library(estimatr)
library(texreg)
library(ggthemes)
library(car)

# Call the data from github
my_url <- "https://raw.githubusercontent.com/semihozankaya/Coding-1-Covid-Assignment/master/Data/Clean/covid_pop_07-10-2020_clean.csv"
df <- read_csv( my_url )

# Quick check of all the variables
df  %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram(bins = 10)+
  theme_tufte() 

# Quick check of the variables I have been assigned
df %>% select(Death, Confirmed) %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram(bins = 10)+
  theme_tufte() 

summary( df )

# It also seems that we have 0 deaths in certain countries
df %>% filter(Death == 0) %>% arrange(Population)
# There are 15 countries with 0 covid deaths. Some of them are relatively small countries in terms of population
# but some of them are actually quite big. Among them, Uganda for instance had recorded 1000 covid cases but 0 deaths.
df %>% transmute(Death/Confirmed) %>% summary()
# The mean death rate for our date seems to be 3%. The median is 2%.
# I am comfortable in dropping the 15 countries mentioned above, either because they are small in size
# or the death counts seems unlikely. 
df2 <- df %>% filter(Death != 0)

######
# Create new variable: per capita deaths and cases in percentages:

df2 <- df2 %>% mutate( death_per_capita = Death/Population )
df2 <- df2 %>% mutate( case_per_capita = Confirmed/Population )

df2 %>% select(death_per_capita, case_per_capita) %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram(bins = 10)+
  theme_tufte() 

summary(select(df2, death_per_capita, case_per_capita))

# Lets show the per capita measures in percentages.
df2$death_per_capita <- df2$death_per_capita*100 
df2$case_per_capita <- df2$case_per_capita*100

summary(select(df2, death_per_capita, case_per_capita))


# The distribution resembles a log-normal distribution with some outlying values in the right tail. 
# We can think about a log transformation to correct for skewness and outliers for both of the variables.

df2 <- df2 %>% mutate( ln_deaths_pc = log(death_per_capita),
                     ln_cases_pc = log(case_per_capita))


df2 %>% select(ln_deaths_pc, ln_cases_pc) %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram(bins = 10)+
  theme_tufte() 


######
# Checking basic scatter-plots
#   Alternatives are linear, semi-log and log-log scales.

# 1) Per Capita Death - Per Capita Confirmed Cases : linear scaling
ggplot( df2 , aes(y = death_per_capita, x = case_per_capita)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Cases Per Capita",y = "Deaths Per Capita", title = "Covid Statistics in Percentages for 07/10/2020") 
###
# Clearly a non linear pattern of association. 

# 2) Changing the scale for Per Capita Confirmed Cases for checking log-transformation
ggplot( df2 , aes(y = death_per_capita, x = case_per_capita)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Cases Per Capita, ln scale",y = "Deaths Per Capita", 
       title = "Covid Statistics in Percentages for 07/10/2020") +
  scale_x_continuous( trans = log_trans(),  breaks = waiver(), labels = number_format(accuracy = 0.000001) )
###
# The data shows very small values of death per capita for a relatively large range of cases per capita.
# Within a log scale for the x axis, we can see that a large subset of death per capita values have clustered 
# around 0.00 to 0.01. So perhaps we can benefit from a change in scale for Y axis as well.

# 3) Changing the scale for both axes
ggplot( df2 , aes(y = death_per_capita, x = case_per_capita))  +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Cases Per Capita, ln scale",y = "Deaths Per Capita, ln scale", 
       title = "Covid Statistics in Percentages for 07/10/2020") +
  scale_x_continuous( trans = log_trans(),  breaks = waiver(), labels = number_format(accuracy = 0.0001))+
  scale_y_continuous( trans = log_trans(), breaks = waiver(), labels = number_format(accuracy = 0.0001)) 
  
###
# As we have suspected, lots of deaths per capita values have been clustered between 0 and 0.001.
# Thus, changing the scale seems to be resulting with a more informative visualization for our case.
# The fit is much better now. It is almost linear. 


######
#   Testing some models:
#   w ln_cases_pc:
#     reg1: ln_deaths_pc = alpha + beta * ln_cases_pc
#     reg2: ln_deaths_pc = alpha + beta_1 * ln_cases_pc + beta_2 * ln_cases_pc^2
#     reg3: ln_deaths_pc = alpha + beta_1 * ln_cases_pc + beta_2 * ln_cases_pc^2 + beta_3 * ln_cases_pc^3
#     reg4: ln_deaths_pc = alpha + beta_1 * ln_cases_pc*(ln_cases_pc < 50) + beta_2 * ln_cases_pc*(ln_cases_pc >= 50)
#     reg5: ln_deaths_pc = alpha + beta * ln_cases_pc, weights: population

###
# Adding powers of the variables to our dataset:
df2 <- df2 %>% mutate(ln_cases_pc = log(case_per_capita),
                     ln_cases_pc_sq = ln_cases_pc^2,
                     ln_cases_pc_cub = ln_cases_pc^3)

# I am using lm_robust from package estimatr to have heteroskedasticity robust standard errors

# First model:
reg1 <- lm_robust( ln_deaths_pc ~ ln_cases_pc , data = df2 , se_type = "HC2" )
summary( reg1 )

# The model shows a very robust Beta estimate of 0.9743 (with a t statistic of 18.62)

# Our findings are intiutive as well. We shouldn't expect death rate of the Covid-19 to change between countries.
# Covid doesn't seem to differentiate between cultures or geography. 

# Visual inspection:
ggplot( data = df2, aes( y = ln_deaths_pc, x = ln_cases_pc ) ) + 
  geom_point( color='blue') +
  geom_smooth( method = lm , color = 'red' )

# Second model
reg2 <- lm_robust( ln_deaths_pc ~ ln_cases_pc + ln_cases_pc_sq , data = df2 )
summary( reg2 )

# In our second model, Beta2 is not statistically significant. Adjusted R square is actually smaller than 
# our first model. 

# Beta1 shows a less stronger pattern of association between x and y than our first model. 


# Visual inspection of the second model:
ggplot( data = df2, aes( y = ln_deaths_pc, x = ln_cases_pc ) ) + 
  geom_point( color='blue') +
  geom_smooth( formula = y ~ poly(x,2) , method = lm , color = 'red' )

reg3 <- lm_robust( ln_deaths_pc ~ ln_cases_pc + ln_cases_pc_sq + ln_cases_pc_cub , data = df2 )
summary(reg3)

# In our third model, we also included the cubic transformation of our independent variable. 

# The third model seems to have better represent the dispersion in both ends of the ln_cases_pc.
# However, its interpretation is a lot harder than our first model. Here, we are increasing complexity for
# a very little extra information.

# Visual inspection of the third model:
ggplot( data = df2, aes( y = ln_deaths_pc, x = ln_cases_pc ) ) + 
  geom_point( color='blue') +
  geom_smooth( formula = y ~ poly(x,3) , method = lm , color = 'red' )


# Regression with piecewise linear spline:
# 1st define the cutoff for cases per capita
# The pattern of association is rather linear and I am not sure about the cutoff points.
# So, I am choosing the cutoff points in order to understand the dispersion on the tails of cases per capita 
# and isolate the values in the middle where most of the data lies on.
cutoff <- c(0.005, 1)
# 2nd we use a log transformation -> cutoff needs to be transformed as well
cutoff_ln<- log( cutoff )
reg4 <- lm_robust(ln_deaths_pc ~ lspline( ln_cases_pc , cutoff_ln ), data = df2 )
summary( reg4 )

# The model has the highest adjusted R square among the alternative models. Its F statistic is one of the lowest
# but the model is still very significant alltogether. 

# However, this approach is again overly complex and in fact it is identical with dropping the outlying 
# values from our analysis. Even though these outlying values might show some faulty record keeping, fraud or 
# differences in methodologies, our first model implies a very similar result with a much simpler approach.

# Visual inspection
ggplot( data = df2, aes( y = ln_deaths_pc, x = ln_cases_pc ) ) + 
  geom_point( color='blue') +
  geom_smooth( formula = y ~ lspline(x,cutoff_ln) , method = lm , color = 'red' )

# I believe that we can comfortably choose the first model for our analysis.

# Weighted visualization of the first model
ggplot(data = df2, aes(y = ln_deaths_pc, x = ln_cases_pc)) +
  geom_point(data = df2, aes(size=Population),  color = 'blue', shape = 16, alpha = 0.6,  show.legend=F) +
  geom_smooth(aes(weight = Population), method = "lm", color='red') +
  labs(x = "ln(Cases Per Capita) ",y = "ln(Deaths Per Capita)", 
       title = "Covid Statistics in Percentages for 07/10/2020")+ 
  annotate("text", x = -5.11, y = -8.02, label = "China", size=4)+
  annotate("text", x = -0.0296, y = -3.2, label = "USA", size=4)+
  annotate("text", x = -2.81,  y = -6.43, label = "India", size=4)+
  annotate("text", x = 1.29,  y = -5.27, label = "Qatar", size=3)+
  annotate("text", x = -7.41,  y = -11.4, label = "Burma", size=3)

# For the weighted regression, since I am assigned with per capita values, the values are already stripped from
# population values and thus the beta coefficients are the same. But the visualization offers more information
# this way.

#####
# Creating model summary with texreg
data_out <- "/home/ozzy/Documents/CEU/ECBS-5208-Coding-1-Business-Analytics/Ozan/Homework/Output/"
htmlreg(list(reg1 , reg2 , reg3 , reg4),
         file = paste0(data_out,"model_comparison.html"),
         type = 'html',
         custom.model.names = c("Ln(Per Capita Cases)",
                                "Ln(Per Capita Cases) - quadratic",
                                "Ln(Per Capita Cases) - cubic",
                                "Ln(Per Capita Cases) - PLS"),
         caption = "Modelling mortality and confirmed covid cases accross countries")

######
# Conclusion:
# Based on model comparison our chosen model is reg1 - ln_deaths_pc ~ ln_cases_pc


######
# Residual analysis.

# Get the predicted y values from the model
df2$reg1_y_pred <- reg1$fitted.values
# Calculate the errors of the model
df2$reg1_res <- df2$ln_deaths_pc - df2$reg1_y_pred 

# Find countries with largest negative errors
df2 %>% top_n( -5 , reg1_res ) %>% 
  select( Country , Confirmed , Death, ln_deaths_pc, reg1_res ) %>%
  arrange(reg1_res)

# Find countries with largest positive errors
df2 %>% top_n( 5 , reg1_res ) %>% 
  select( Country , Confirmed , Death, ln_deaths_pc, reg1_res ) %>%
  arrange(-reg1_res)


#################################
## Testing hypothesis
#
# The default is already tested where H0 is Beta1 = 0, HA: it is not zero
summary( reg1 )

# Beta1 is statistically different than 0 in 99.9 significance level. We can reject the null.

# Lets also test: H0: Beta1 = 1, HA: Beta1 is not 1
linearHypothesis( reg1 , "ln_cases_pc = 1")

# We cannot reject the null here. We can say that Beta1 can indeed be 1.


#################################
## Prediction uncertainty
#

# Confidence intervals of our predicted values is available in our visualizations
ggplot( data = df2, aes( y = ln_deaths_pc, x = ln_cases_pc ) ) + 
  geom_point( color='blue') +
  geom_smooth( method = lm , color = 'red' , se = T )

# We can also see them with the predict function as below
pred_CI <- predict( reg1, newdata = df2 , interval ="confidence" , alpha = 0.05 )


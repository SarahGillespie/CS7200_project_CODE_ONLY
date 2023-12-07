library(tidyverse)
library(lme4)
library(sjPlot)
library(emmeans)
library(AID)

# GOALS ------------------------------------------------------------------------
# Activity = upright and ambulation
# break apart and analyze upright and ambulation.
# potentially ignore the holiday effect right now.

# DATA -------------------------------------------------------------------------
# read in the data from the CSV
data <- read.csv('data_full.csv')

data <- data %>% 
  drop_na(group) %>% # dropped participants who were not in either control or treatment
  mutate(PARTICIPANT = as.factor(PARTICIPANT),
      # holiday = as.factor(holiday_months_dummy_variable), 
        activity = (Ambulation + Upright..Other.) / HOURS_COVERAGE,
      # this creates a fraction that is the proportion of activity RELATIVE to all hours covered in total.
      # activity is a ratio/proportion, NOT a quantity of hours.
        # holiday_treatment = as.factor(ifelse(group == 'Treatment' & holiday == 1, 1, 0)),
        # holiday_group = as.factor(ifelse(group == 'Treatment' & holiday == 1, 1,
        #                                  ifelse(group == 'Treatment' & holiday == 0, 2,
        #                                         ifelse(group == 'Control' & holiday == 1, 3, 4)))),
      month = as.factor(format(as.Date(DATE, format="%Y-%m-%d"),"%m")))

# analysis for ACTIVITY without consideration to holiday -----------------------

hist(data$activity)  # skewed right
shapiro.test(data$activity) # p-value is significant, cannot assume normality.

# SEG: basic t-test to see if there if a difference between the two groups...
# but we really should use that Wilcoxon test.
# find mean and standard deviation of each group before the Wilcox test.
sum_stats_basic_t_test <- data %>%
  group_by(group) %>%
  summarise(mean_activity = mean(activity), sd(activity))

# SEG: Density plots with semi-transparent fill with the distribution of activity each day for each group.
# SEG: note that this graph is NOT the data normalized via boxcoxnc in Dorsa's code.
ggplot(data, aes(x=activity, fill=group)) + geom_density(alpha=.3)

# Wilcoxon is non-parametric equivalent to t-test

# EDA: Overall -----------------------------------------------------------------
agg_part <- data %>% 
  group_by(PARTICIPANT, group) %>% 
  summarize(activity = sum(activity),
            hours = sum(HOURS_COVERAGE))

agg_month_part <- data %>% 
  group_by(PARTICIPANT, group, month) %>% 
  summarize(activity = mean(activity))

ggplot(agg_part, aes(PARTICIPANT, hours, fill = group)) +
  geom_col() # This is similar to Ethan's paper which shows variance in the time
# each participant contributes in our data set

# EDA: control vs treatment activity -------------------------------------------
ggplot(data, aes(group, activity, y)) +
  geom_boxplot() # both skewed right but non-holiday slightly greater amounts of activity

data %>% 
  group_by(group) %>% 
  summarize(
    count = n(),
    mean = mean(activity),
    median = median(activity, na.rm = TRUE),
    IQR = IQR(activity, na.rm = TRUE)
  )

wilcox.test(activity ~ group, data, alternative = 'greater')$p.value 
# there is a difference between treatment groups

kruskal.test(activity ~ group, data) # there is a difference between treatment groups


# Test for Interaction ---------------------------------------------------------
# PERHAPS WE WILL TEST FOR INTERACTION BETWEEN UPRIGHT AND AMBULATION?
## Visualize Treatment Means Plot
means <- data %>% 
  dplyr::select(group, activity) %>% 
  group_by(group) %>% 
  summarize(means = mean(activity))

## Check if residuals are normal to see if we can use F-test
fit1 <- glm(activity ~ group, family = gaussian, data)
plot(fit1) #SEG: plots residuals
fit2 <- lm(activity ~ group, data)
plot(fit2) #SEG: plots residuals
# we can use generalized linear models to handle non-normal data

res <- residuals(fit1)
hist(res) # looks approx normal?
shapiro.test(res) # not normally distributed because low p-value but okay because
# notes from Section 7 is also not normal?

# SEG: for the Shapiro test, large p-value indicates the data set is normally distributed,
# whereas a low p-value indicates that these residuals aren't normally distributed.
# each one of these subsets of the data has a very low p-value.
# the plot(fit1) and plot(fit2) graphs do seem to have standardized residuals
# that are more positive than negative...
 

## F-test
summary(fit1)
# interaction p-value low = there is interaction
# main effect for holiday p-value low = holiday affects activity
# main effect for group p-value low = group affects activity
# this affirms results with visual

# analysis for UPRIGHT without consideration to holiday ########################

hist(data$Upright..Other.)  # very skewed right
shapiro.test(data$Upright..Other.) # p-value is significant, cannot assume normality.

# SEG: Density plots with semi-transparent fill with the distribution of 
# Upright..Other. each day for each group.
# treatment has much less spread and a high peak at a low upright value.
ggplot(data, aes(x=Upright..Other., fill=group)) + geom_density(alpha=.3)

# Wilcoxon is non-parametric equivalent to t-test

# EDA: Overall -----------------------------------------------------------------
agg_month_part <- data %>% 
  group_by(PARTICIPANT, group, month) %>% 
  summarize(Upright..Other. = mean(Upright..Other.))

ggplot(agg_part, aes(PARTICIPANT, hours, fill = group)) +
  geom_col() # This is similar to Ethan's paper which shows variance in the time
# each participant contributes in our data set

# EDA: control vs treatment Upright..Other. ------------------------------------
ggplot(data, aes(group, Upright..Other., y)) +
  geom_boxplot() # both skewed right

data %>% 
  group_by(group) %>% 
  summarize(
    count = n(),
    mean = mean(Upright..Other.),
    median = median(Upright..Other., na.rm = TRUE),
    IQR = IQR(Upright..Other., na.rm = TRUE)
  )

wilcox.test(Upright..Other. ~ group, data, alternative = 'greater')$p.value 
# there is a difference between treatment groups

kruskal.test(Upright..Other. ~ group, data) # there is a difference between treatment groups

## Check if residuals are normal to see if we can use F-test
fit1 <- glm(Upright..Other. ~ group, family = gaussian, data)
plot(fit1) #SEG: plots residuals
fit2 <- lm(Upright..Other. ~ group, data)
plot(fit2) #SEG: plots residuals
# we can use generalized linear models to handle non-normal data

res <- residuals(fit1)
hist(res) # it's almost normally distributed.
shapiro.test(res) # not normally distributed because low p-value

# SEG: for the Shapiro test, large p-value indicates the data set is normally distributed,
# whereas a low p-value indicates that these residuals aren't normally distributed.

## F-test
summary(fit1)


# analysis for AMBULATION without consideration to holiday #####################

hist(data$Ambulation)  # very right
shapiro.test(data$Ambulation) # p-value is significant, cannot assume normality.

# SEG: Density plots with semi-transparent fill with the distribution of 
# Ambulation each day for each group.
# treatment has much less spread and a high peak at a low upright value.
ggplot(data, aes(x=Ambulation, fill=group)) + geom_density(alpha=.3)

# Wilcoxon is non-parametric equivalent to t-test

# EDA: Overall -----------------------------------------------------------------
agg_month_part <- data %>% 
  group_by(PARTICIPANT, group, month) %>% 
  summarize(Ambulation = mean(Ambulation))

ggplot(agg_part, aes(PARTICIPANT, hours, fill = group)) +
  geom_col() # This is similar to Ethan's paper which shows variance in the time
# each participant contributes in our data set

# EDA: control vs treatment Upright..Other. -------------------------------------------
ggplot(data, aes(group, Ambulation, y)) +
  geom_boxplot()
# SEG: OMG the treatment group DOES have more ambulation!

data %>% 
  group_by(group) %>% 
  summarize(
    count = n(),
    mean = mean(Ambulation),
    median = median(Ambulation, na.rm = TRUE),
    IQR = IQR(Ambulation, na.rm = TRUE)
  )

wilcox.test(Ambulation ~ group, data, alternative = 'less')$p.value 
# there is a difference between treatment groups!

kruskal.test(Ambulation ~ group, data) # there is a difference between treatment groups

## Check if residuals are normal to see if we can use F-test
fit1 <- glm(Ambulation ~ group, family = gaussian, data)
plot(fit1) #SEG: plots residuals
fit2 <- lm(Ambulation ~ group, data)
plot(fit2) #SEG: plots residuals
# we can use generalized linear models to handle non-normal data

res <- residuals(fit1)
hist(res) # it's almost normally distributed.
shapiro.test(res) # not normally distributed because low p-value

# SEG: for the Shapiro test, large p-value indicates the data set is normally distributed,
# whereas a low p-value indicates that these residuals aren't normally distributed.

## F-test
summary(fit1)

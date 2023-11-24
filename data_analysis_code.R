
library(tidyverse)
library(lme4)
library(sjPlot)
library(emmeans)

# Goals: 
# 1) Does holiday make a difference on activity Y_activityij = β0 + β1Holiday + γi + ϵij  
# 2) Does holiday and personalized health coaching make a difference on activity
# Y_activityij = β0 + β1Treatment + β2Holiday + β3Treatment*Holiday + γi + ϵij 
# Activity = upright and ambulation (add together?)

# To do:
# 1) EDA on activity during holiday period vs non-holiday (mean and spread)
# 2) EDA on activity during holiday period vs non-holiday and treatment vs control (mean and spread)
# 3) Analysis to see if holiday*treatment is significant  (similar to HW3 with plot?)
# 4) Repeated measures model
# 5) Investigate race and gender to see if additional variables are significant

# Questions/Notes:
# When looking into holiday*group, interaction is significant
# When separating holiday/group into 4 groups, found interaction does not matter

# DATA
# read in the data from the CSV
data <- read.csv('data_full.csv')



data <- data %>% 
  drop_na(group) %>% # dropped participants who were not in either control or treatment
  mutate(PARTICIPANT = as.factor(PARTICIPANT),
      holiday = as.factor(holiday_months_dummy_variable), 
        activity = (Ambulation + Upright..Other.) / HOURS_COVERAGE,
        holiday_treatment = as.factor(ifelse(group == 'Treatment' & holiday == 1, 1, 0)),
        holiday_group = as.factor(ifelse(group == 'Treatment' & holiday == 1, 1,
                                         ifelse(group == 'Treatment' & holiday == 0, 2,
                                                ifelse(group == 'Control' & holiday == 1, 3, 4)))),
      month = as.factor(format(as.Date(DATE, format="%Y-%m-%d"),"%m")))

hist(data$activity)  # skewed right
shapiro.test(data$activity) # p-value is significant, cannot assume normality

# Wilcoxon is non-parametric equivalent to t-test

# EDA: Overall ------------------------------------
agg_part <- data %>% 
  group_by(PARTICIPANT, holiday_group, group) %>% 
  summarize(activity = sum(activity),
            hours = sum(HOURS_COVERAGE))

agg_month_part <- data %>% 
  group_by(PARTICIPANT, group, holiday, month) %>% 
  summarize(activity = mean(activity))

ggplot(agg_part, aes(PARTICIPANT, hours, fill = group)) +
  geom_col() # This is similar to Ethan's paper which shows variance in the time
## each participant contributes in our dataset

# EDA: holiday vs non-holiday activity ------------------------------------
ggplot(data, aes(holiday, activity)) +
  geom_boxplot() # both skewed right but non-holiday slightly greater amounts of activity

data %>% 
  group_by(holiday) %>% 
  summarize(
    count = n(),
    mean = mean(activity),
    median = median(activity, na.rm = TRUE),
    IQR = IQR(activity, na.rm = TRUE)
  )

wilcox.test(activity ~ holiday, data, alternative = 'greater')$p.value 
# non-holiday greater activity

# EDA: holiday_treatment vs non activity ------------------------------------
ggplot(data, aes(holiday, activity)) +
  geom_boxplot() +
  facet_wrap(~group)
# control: non-holiday greater amounts of activity
# treatment: non-holiday greater amounts of activity
# overall: max = non-holiday and control, min = holiday and treatment

data %>% 
  group_by(holiday_treatment) %>% 
  summarize(
    count = n(),
    mean = mean(activity),
    median = median(activity, na.rm = TRUE),
    IQR = IQR(activity, na.rm = TRUE)
  )

wilcox.test(activity ~ holiday_treatment, data, alternative = 'greater')$p.value 
# holiday and treatment greater activity

kruskal.test(activity ~ group, data) # there is a difference between treatment groups
kruskal.test(activity ~ holiday, data) # there is a difference between holiday periods

# Model 1: repeated measures with MLE for holiday ------------------------------------
model1 <- lmer(activity ~ holiday + 1 + (1|PARTICIPANT), data, REML=FALSE)
tab_model(model1)

agg_month <- data %>% 
  group_by(month, group) %>% 
  summarize(mean = mean(activity))

ggplot(agg_month, aes(month, y = mean, color = as.factor(group))) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun= mean, geom = "line", aes(group = as.factor(group))) +
  scale_x_discrete(limits=c("06", "07", "08", "09", "10", "11", "12", "01", "02", 
                            "03", "04", "05")) +
  annotate("rect", xmin = "11", xmax = "12", ymin = 0.04, ymax = 0.151,
           alpha = .1, fill = "black") +
  labs(x = "Month", y = "Mean Activity", color = "Group") +
  ylim(0,1)

# Test for Interaction between holiday and treatment ------------------------------------
## Visualize Treatment Means Plot
means <- data %>% 
  select(group, holiday, activity) %>% 
  group_by(holiday, group) %>% 
  summarize(means = mean(activity))

ggplot(means, aes(as.factor(holiday), y = means, color = as.factor(group))) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun= mean, geom = "line", aes(group = as.factor(group))) +
  labs(x = "Holiday", y = "Mean Activity", color = "Group")

# there are differences in mean responses across levels and lines not parallel
# there is interaction

## Check if residuals are normal to see if we can use F-test
fit1 <- glm(activity ~ group*holiday, family = gaussian, data)
fit2 <- lm(activity ~ group*holiday, data)
# we can use generalized linear models to handle non-normal data

res <- residuals(fit1)
hist(res) # looks approx normal?
shapiro.test(res) # not normal because low p-value but okay because notes from 7
## also not normal?

## F-test
summary(fit1)
# interaction p-value low = there is interaction
# main effect for holiday p-value low = holiday affects activity
# main effect for group p-value low = group affects activity
# this affirms results with visual

# look into slides starting 7-41 for "interaction important"
# marginal means?


# Two-way Random Effects ---------------------------------------------------------------
# Factor A: Participant 1-30
# Factor B: holiday_group: 1-4
# imbalanced design because not all participants per month

# with 4 groups per participant
ggplot(data, aes(holiday_group, activity)) +
  geom_boxplot()

ggplot(data, aes(as.factor(PARTICIPANT), y = activity, color = holiday_group)) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun= mean, geom = "line", aes(group = holiday_group)) +
  scale_color_discrete(labels = c("Treatment+Holiday", "Treatment+nonHoliday",
                                "Control+Holiday", "Control+nonHoliday")) +
  labs(x = "Participant ID", y = "Mean Activity", color = "Group")

# only control vs treatment per participant
ggplot(data, aes(as.factor(PARTICIPANT), y = activity, color = group)) +
  stat_summary(fun = mean, geom = "point") +
  # stat_summary(fun= mean, geom = "line", aes(group = group)) +
  labs(x = "Participant ID", y = "Mean Activity", color = "Group") +
  ylim(0,1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1)) +
  geom_hline(yintercept = mean(data$activity))

 
# Model 2: Two-way random effects ANOVA & ML ------------------------------------

# ANOVA
model2 <- aov(activity ~ PARTICIPANT*holiday_group, data)
summary(model2)

# REML
model3 <- lmer(activity ~ 1 + (1|PARTICIPANT) + (1|holiday_group) + 
                (1|PARTICIPANT:holiday_group),
             data, REML=TRUE)
summary(model3)

# ML
model4 <- lmer(activity ~ 1 + (1|PARTICIPANT) + (1|holiday_group) + 
                (1|PARTICIPANT:holiday_group),
              data, REML=FALSE)
summary(model4)

# Additive REML
model5 <- lmer(activity ~ 1 + (1|PARTICIPANT)+(1|holiday_group), data,
             REML=TRUE)
summary(model5)

# Additive ML
model6 <- lmer(activity ~ 1 + (1|PARTICIPANT)+(1|holiday_group), data,
             REML=FALSE)
summary(model6)


# Mixed Model -------------------------------------------------------------
model7<- lmer(activity ~ month + (1|holiday_group) + (1|PARTICIPANT) + 
       (1|month:PARTICIPANT), data, REML=TRUE)
summary(model7)


library(tidyverse)
library(lme4)

# Goals:
# - investigate making the project a balanced design (same number of replicates per participant)
# - separate ambulation and upright for own analyses
# - repeated measure analysis
# - test for interaction?
# - repeated measures analysis with interaction?

# TODO:
# determine story/message
# determine what to include in report
# finalized models, visualizations, and analysis

data <- read.csv('data_full.csv')

data <- data %>% 
  drop_na(group) %>% # dropped participants who were not in either control or treatment
  mutate(PARTICIPANT = as.factor(PARTICIPANT),
         # holiday = as.factor(holiday_months_dummy_variable), 
         activity_prop = (Ambulation + Upright..Other.) / HOURS_COVERAGE,
         ambulation_prop = Ambulation/HOURS_COVERAGE,
         upright_prop = Upright..Other. / HOURS_COVERAGE,
         # holiday_treatment = as.factor(ifelse(group == 'Treatment' & holiday == 1, 1, 0)),
         # holiday_group = as.factor(ifelse(group == 'Treatment' & holiday == 1, 1,
         #                                   ifelse(group == 'Treatment' & holiday == 0, 2,
         #                                          ifelse(group == 'Control' & holiday == 1, 3, 4)))),
         month = as.factor(format(as.Date(DATE, format="%Y-%m-%d"),"%m")),
         year = as.factor(format(as.Date(DATE, format="%Y-%m-%d"),"%y")))


# Balanced Design Attempt -------------------------------------------------
# check how many months each participant contributed in each month
# aggregate participant ambulation and upright by each month

# # Get count of how often participants contributed data per month
# participant_count <- data %>% 
#   select(PARTICIPANT, month) %>% 
#   group_by(PARTICIPANT, month) %>% 
#   summarize(n = n())

# participant data for May by first vs second half of month
participant_05 <- data %>% 
  filter(month == "05") %>% 
  select(PARTICIPANT, group, month, year, DATE, upright_prop, ambulation_prop) %>% 
  mutate(day = as.numeric(format(as.Date(DATE), "%d")),  # Extract day as numeric
         # week = case_when(
         #   day <= 7 ~ 1,
         #   day > 7 & day <= 14 ~ 2,
         #   day > 14 & day <= 21 ~ 3,
         #   TRUE ~ 4),
         half = case_when(
           day <= 15 ~ 1,
           TRUE ~ 2),
         half = as.factor(half))

# test to see if samples were pulled from the same group -----------------------
# Kolmogorov-Smirnov Test, right??
# Kolmogorov-Smirnov's null hypothesis = the samples are from the same population.

participant_May_2021 <- participant_05 %>% 
  filter(year == "21")

participant_May_2022 <- participant_05 %>% 
  filter(year == "22")

ks.test(participant_May_2021$upright_prop, participant_May_2022$upright_prop)
# p-value of 0.07598... cannot reject the null so this might be the same population/distribution.

ks.test(participant_May_2021$ambulation_prop, participant_May_2022$ambulation_prop)
# P-value of <0.01... can reject the null, so these are different population/distribution.

# try for treatment/control as a comparison ------------------------------------
participant_all_May_treatment <- participant_05 %>% 
  filter(group == "Treatment")

participant_all_May_control <- participant_05 %>% 
  filter(group == "Control")

ks.test(participant_all_May_treatment$upright_prop, participant_all_May_control$upright_prop)
# P-value of <0.01... can reject the null, so these are different population/distribution.

ks.test(participant_all_May_treatment$ambulation_prop, participant_all_May_control$ambulation_prop)
# P-value of <0.01... can reject the null, so these are different population/distribution.

# Research Questions
# 1) Does month/week make a difference on ambulation and being upright per participant?
# 2) Does month/week and group make a difference on ambulation and being upright per participant?


# EDA Overall -------------------------------------------------------------
# Assumptions: errors independent, normal, non-systematic and constant variance
# possible correlation between months/week
# aggregated ambulation and upright may be nonnormal

# ggplot(participant_45, aes(month, mean_upright)) +
#   geom_boxplot()
# 
# 
# ggplot(participant_5_week, aes(PARTICIPANT, mean_upright)) +
#   geom_col()

# Models ------------------------------------------------------------------
# basic: upright/ambulation ~ month/week for each participant (fixed)
# additive: upright/ambulation ~ month/week for each participant + trt group (mixed)
# 4 total models with repeated measures?
# trt/ctrl groups were randomized
# month is fixed
# participants random?
# could we use month * group as interaction? test for interaction

# fit1 <- lmer(mean_ambulation ~ month + (1|PARTICIPANT), participant_45, 
#              REML=FALSE)
# summary(fit1)
# 
# fit2 <- lmer(mean_ambulation ~ week + (1|PARTICIPANT) + (1|group),  
#              participant_5_week, REML=FALSE)
# summary(fit2)

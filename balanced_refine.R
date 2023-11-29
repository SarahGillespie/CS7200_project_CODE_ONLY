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
         holiday = as.factor(holiday_months_dummy_variable), 
         activity_prop = (Ambulation + Upright..Other.) / HOURS_COVERAGE,
         ambulation_prop = Ambulation/HOURS_COVERAGE,
         upright_prop = Upright..Other. / HOURS_COVERAGE,
         holiday_treatment = as.factor(ifelse(group == 'Treatment' & holiday == 1, 1, 0)),
         holiday_group = as.factor(ifelse(group == 'Treatment' & holiday == 1, 1,
                                           ifelse(group == 'Treatment' & holiday == 0, 2,
                                                  ifelse(group == 'Control' & holiday == 1, 3, 4)))),
         month = as.factor(format(as.Date(DATE, format="%Y-%m-%d"),"%m")))


# Balanced Design Attempt -------------------------------------------------
# check how many months each participant contributed in each month
# aggregate participant ambulation and upright by each month

# Get count of how often participants contributed data per month
participant_count <- data %>% 
  select(PARTICIPANT, month) %>% 
  group_by(PARTICIPANT, month) %>% 
  summarize(n = n())

# 17 participants contributed data in April and May
participant_45 <- data %>% 
  filter(month == "04" | month == "05") %>% 
  filter(PARTICIPANT %in% c("CR01", "CR02", "CR03", "CR04", "CR05", "CR07",
                            "CR21B", "CR23B", "CR33B", "CR34", "CR38", "CR39", 
                            "CR41", "CR42", "CR43", "CR44", "CR46")) %>% 
  group_by(PARTICIPANT, month, group) %>% 
  summarize(mean_upright = mean(upright_prop),
            mean_ambulation = mean(ambulation_prop))

# 14 participants contributed data in June and July
participant_67 <- data %>% 
  filter(month == "06" | month == "07") %>% 
  filter(PARTICIPANT %in% c("CR01", "CR02", "CR03", "CR04", "CR05", "CR07", "CR10",
                            "CR14", "CR15", "CR18", "CR21B", "CR43", "CR44", "CR46")) %>% 
  group_by(PARTICIPANT, month, group) %>% 
  summarize(mean_upright = mean(upright_prop),
            mean_ambulation = mean(ambulation_prop))

# participant data for May by week
participant_5 <- data %>% 
  filter(month == "05") %>% 
  select(PARTICIPANT, group, month, DATE, upright_prop, ambulation_prop) %>% 
  mutate(day = as.numeric(format(as.Date(DATE), "%d")),  # Extract day as numeric
         week = case_when(
           day <= 7 ~ 1,
           day > 7 & day <= 14 ~ 2,
           day > 14 & day <= 21 ~ 3,
           TRUE ~ 4),
         week = as.factor(week))

# 15 participants contributed data for the whole month of May
participant_5_week <- participant_5 %>% 
  filter(PARTICIPANT != 'CR10' | PARTICIPANT != 'CR34' | PARTICIPANT != 'CR38') %>% 
  group_by(PARTICIPANT, week, group) %>% 
  summarize(mean_upright = mean(upright_prop),
            mean_ambulation = mean(ambulation_prop))


# Research Questions
# 1) Does month/week make a difference on ambulation and being upright per participant?
# 2) Does month/week and group make a difference on ambulation and being upright per participant?


# EDA Overall -------------------------------------------------------------
# Assumptions: errors independent, normal, non-systematic and constant variance
# possible correlation between months/week
# aggregated ambulation and upright may be nonnormal

ggplot(participant_45, aes(month, mean_upright)) +
  geom_boxplot()


ggplot(participant_5_week, aes(PARTICIPANT, mean_upright)) +
  geom_col()

# Models ------------------------------------------------------------------
# basic: upright/ambulation ~ month/week for each participant (fixed)
# additive: upright/ambulation ~ month/week for each participant + trt group (mixed)
# 4 total models with repeated measures?
# trt/ctrl groups were randomized
# month is fixed
# participants random?
# could we use month * group as interaction? test for interaction

fit1 <- lmer(mean_ambulation ~ month + (1|PARTICIPANT), participant_45, 
             REML=FALSE)
summary(fit1)

fit2 <- lmer(mean_ambulation ~ week + (1|PARTICIPANT) + (1|group),  
             participant_5_week, REML=FALSE)
summary(fit2)

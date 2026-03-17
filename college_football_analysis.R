# ---------------------------------------------------------
# 2023 College Football Win Predictors
# Author: Quinn Brennan
# Description:
#   This script analyzes 2023 NCAA football team data to
#   evaluate whether offensive or defensive performance
#   better predicts win percentage using linear regression
#   and data visualization.
# ---------------------------------------------------------

# Load libraries
library(tidyverse)
library(broom)

# Read and inspect data
# Make sure the cfb23.csv file is in the same folder as this script before running
cfb23 <- read_csv("cfb23.csv")
glimpse(cfb23)


# ---------------------------------------------------------
# Data cleaning and feature engineering
# ---------------------------------------------------------

# Separate win-loss record
cfb23 <- cfb23 %>% 
  separate('Win-Loss', c('win', 'loss'), sep = '-', convert = TRUE, remove = FALSE)

# Convert relevant features to numeric
features <- c("Off Yards per Game", 
              "Off TDs", 
              "Points Per Game", 
              "Yards Per Game Allowed", 
              "Total TDs Allowed", 
              "Penalty Yards Per Game", 
              "Avg Points per Game Allowed",
              "win",
              "Off Rank",
              "Def Rank",
              "Off TDs Allowed",
              "Turnover Margin",
              "Games")

cfb23 <- cfb23 %>%
  mutate(across(all_of(features), as.numeric))

# Create win percentage
cfb23 <- cfb23 %>%
  mutate(win_percentage = (win / Games) * 100)


# ---------------------------------------------------------
# Offensive touchdowns vs win percentage
# ---------------------------------------------------------

cfb23 %>% 
  filter(! is.na(`Off Rank`)) %>% 
  ggplot(aes(`Off TDs`, win_percentage, color = `Off Rank`))+
  geom_jitter()+
  labs(y = "Win Percentage",
       x = "Offensive Touchdowns Scored")

# Linear model
offensive_mod <- lm(win_percentage ~ `Off TDs`, data = cfb23)
tidy(offensive_mod)
glance(offensive_mod)


# ---------------------------------------------------------
# Defensive touchdowns allowed vs win percentage
# ---------------------------------------------------------

cfb23 %>% 
  filter(! is.na(`Def Rank`)) %>% 
  ggplot(aes(`Off TDs Allowed`, win_percentage, color = `Def Rank`))+
  geom_jitter()+
  scale_x_continuous(limits = c(10, 65), breaks = seq(10, 65, by = 10))+
  labs(y = "Win Percentage",
       x = "Offensive Touchdowns Allowed")

# Linear model
defense_mod <- lm(win_percentage ~ `Off TDs Allowed`, data = cfb23)
tidy(defense_mod)
glance(defense_mod)


# ---------------------------------------------------------
# Offensive and defensive yardage model
# ---------------------------------------------------------

yardage_mod <- lm(win_percentage ~ `Off Yards per Game` + `Yards Per Game Allowed`, data = cfb23)
tidy(yardage_mod)
glance(yardage_mod)


# ---------------------------------------------------------
# Turnover margin analysis
# ---------------------------------------------------------

cfb23 %>% 
  filter(! is.na(`Turnover Margin`), `Turnover Margin` %in% c(-9,-6,-3,0,3,6,9)) %>%
  ggplot(aes(as.factor(`Turnover Margin`), win_percentage)) +
  geom_boxplot(fill = "lightblue", color = "black")+
  labs(y = "Win Percentage",
       x = "Turnover Margin")

# Linear model
turnover_mod <- lm(win_percentage ~ `Turnover Margin`, data = cfb23)
tidy(turnover_mod)

glance(turnover_mod)


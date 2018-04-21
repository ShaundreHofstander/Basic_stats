# Day 5 
# Continuing with ANOVA, corelations and regressions
# 20 April 2018
# Shaundre'

# load libraries
library(tidyverse)
library(Rmisc)

# Load data

snakes <- read_csv("C:/Users/BCB User/Desktop/Intro_R_Workshop-master/Basic_stats/snakes.csv") %>% 
  mutate(day = as.factor(day))

# or 
snakes <- read_csv("C:/Users/BCB User/Desktop/Intro_R_Workshop-master/Basic_stats/snakes.csv")
snakes$day = as.factor(snakes$day)

# Summarise the data -----------------------------------------------------

snakes_summary <- snakes %>%
  group_by(day) %>% 
  summarise(snakes_mean = mean(openings),
            snake_sd = sd(openings))

# data violates the assumption that its not independant, because the same snakes are used

# Formulate hypothesis ----------------------------------------------------


# H0: there is no difference in the number of openings (that the snakes respond to) from day to day
# H1: there is a difference in the number of openings from day to day

# unambiguous set of coniditons that there is only one of two outcomes
# 95% probability explains the hypothesis

# Test a hypothesis -------------------------------------------------------

# First calculate SE and CI
snakes.summary2 <- summarySE(data = snakes,
                             measurevar = "openings",
                             groupvars = c("day"))

# Then visualise the data
ggplot(data = snakes, aes(x = day, y = openings)) +
  geom_segment(data = snakes.summary2, aes(x = day, xend = day, y = openings - ci, 
                                           yend = openings + ci, colour = day),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = day), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.05)
# end of x and y shares the same day


# ask question across all of the  days, there is no difference between snam=kes in how they respond to the openings
# H0: there is no difference between snakes with respect to the number of openings at which they habituate
# H1: there is a difference between days in terms of the number of openings in which the snakes habituate

# Test the hypothesis
snakes.day.aov <- aov(openings ~ day, data = snakes) # one factor of analysis of variance
summary(snakes.day.aov) # p is less than 0.05 therfore there is a significant difference

# Test both hypotheses
snakes.all.aov <- aov(openings ~ day + snake, data = snakes)
summary(snakes.all.aov)
# snakes do not have asignificant difference
# residual is the amount of error in the data

# Test assumptions afterwards ---------------------------------------------

# First visualise normality of results
snakes.residuals <- residuals(snakes.all.aov)
hist(snakes.residuals)

# Then visualise homoscedasticity of rsults
plot(fitted(snakes.all.aov), residuals(snakes.all.aov))

# Check Tukey results 
snakes.tukey <- TukeyHSD(snakes.all.aov, which = "day")
plot(snakes.tukey)

# Visulise the factor interaction
ggplot(data = snakes, aes(x = as.numeric(day),
                          y = openings,
                          colour = snake))+
  geom_line(size = 3) +
  geom_point(size = 4)



# Exercise ----------------------------------------------------------------

# Load libraries
library(tidyverse)
library(ggpubr)
library(Rmisc)

# Get the moth data from GitHub
# Run a two-way ANOVA on them

# Load data
moths <- read_csv("C:/Users/BCB User/Desktop/Intro_R_Workshop-master/Basic_stats/moth_traps.csv.csv") %>% 
  gather(key ="trap", value = "count", -Location)

# Summarise the data ------------------------------------------------------

moth_loc_summary <- moths %>%
  group_by(Location) %>% 
  summarise(moth_mean = mean(count),
            moth_sd = sd(count))

moth_trap_summary <- moths %>%
  group_by(trap) %>% 
  summarise(moth_mean = mean(count),
            moth_sd = sd(count))

# Formulate the hypotheses --------------------------------------------------

# HO: There is no difference in the count depending of different locations
# HO: There is no difference in count depending on the different trap types

#  Calculate SE & CI ------------------------------------------------------

moth_loc_summary_2 <- summarySE(data = moths,
                                measurevar = "count",
                                groupvars = c("Location"))

moth_trap_summary_2 <- summarySE(data = moths,
                                 measurevar = "count",
                                 groupvars = c("trap"))

# Visualise the data ------------------------------------------------------

Location <- ggplot(data = moths, aes(x = Location, y = count)) +
  geom_segment(data = moth_loc_summary_2, aes(x = Location, xend = Location, y = count - ci, yend = count + ci, colour = Location),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = Location), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.05)

Trap <- ggplot(data = moths, aes(x = trap, y = count)) +
  geom_segment(data = moth_trap_summary_2, aes(x = trap, xend = trap, y = count - ci, yend = count + ci, colour = trap),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = trap), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.05)

Final <- ggarrange(Location, Trap,
                   ncol = 2, nrow = 1,
                   labels = c("Location", "Trap"),
                   common.legend = TRUE)
Final

# Test the hypothesis -----------------------------------------------------

moth.loc.aov <- aov(count ~ Location, data = moths)
summary(moth.loc.aov)

moth.trap.aov <- aov(count ~ trap, data = moths)
summary(moth.trap.aov)

# Test both hypotheses

moths.all.aov <- aov(count ~ Location + trap, data = moths)
summary(moths.all.aov)

# Testing assumptions 

# First visualise normality of data

moths.residuals <- residuals(moths.all.aov)
hist(moths.residuals)

# Visualise homoscedasticity

plot(fitted(moths.all.aov), residuals(moths.all.aov))

# Apply tukey test 

moths.loc.tukey <- TukeyHSD(moths.all.aov, which = "Location")
plot(moths.loc.tukey)

moths.trap.tukey <- TukeyHSD(moths.all.aov, which = "trap")
plot(moths.trap.tukey)


# Regressions -------------------------------------------------------

# y = mx+c
# For the explanation of this statistical analysis
# We are going to use eruption data from ol' Faithful

head(faithful)

# Plot aquicck scatterplot
ggplot(data = faithful, aes(x = waiting, y = eruptions))+
  geom_point()+
  geom_smooth(method = "lm", se = F, colour = "hotpink")


# Form a hypothesis -------------------------------------------------------

# H0: Waiting time does not influence the duratio of an eruption
# H1: Waiting time does influence the duration of an eruption


# Test a hypothesis -------------------------------------------------------

faithful_lm <- lm(eruptions ~ waiting, data = faithful)
summary(faithful_lm)

# Correlations ------------------------------------------------------------


# Load libraries
library(tidyverse)
library(ggpubr)
library(corrplot)

# Load data
ecklonia <- read_csv("C:/Users/BCB User/Desktop/Intro_R_Workshop-master/data/ecklonia.csv")

# Formulate hypothesis ----------------------------------------------------

# H0: There is no relationship between stipe length and stipe mass for the kelp Ecklonia maxima
# H1: There is a relationship between stipe length and stipe mass for the kelp Ecklonia maxima


# Test the hypothesis -----------------------------------------------------

cor.test(ecklonia$frond_length, ecklonia$frond_mass)

# Visualise the data
ggplot(data = ecklonia, aes(x = frond_length, y = frond_mass))+
  geom_point()

# Run hecka tests at once -------------------------------------------------

ecklonia_sub <- ecklonia %>% select(frond_length:epiphyte_length)

ecklonia_cor <- cor(ecklonia_sub)
ecklonia_cor 


# Spearman rank test ------------------------------------------------------

# Create ordinal data
ecklonia$length <- as.numeric(cut((ecklonia$stipe_length + ecklonia$frond_length), 3))

# Then run a Spearman test
cor.test(ecklonia$length, ecklonia$stipe_diameter, method = "spearman")

# Kendall rank correlation ------------------------------------------------

cor.test(ecklonia$primary_blade_length, ecklonia$primary_blade_width, method = "kendall")


# Visualise all the things ------------------------------------------------

ecklonia_pearson <- cor(ecklonia_sub)
ecklonia_pearson

corrplot(ecklonia_cor, method = "circle")
# the size of do shows the strength of the correlation




# Extra Exercise ----------------------------------------------------------


# Load data
ecklonia <- read_csv("G:/Intro_R_Workshop-master/data/ecklonia.csv")

# Select only the necessary columns
ecklonia_sub <- ecklonia %>% select(frond_length:epiphyte_length)

# pearson
ecklonia_pearson <- cor(ecklonia_sub)

# pallete
ecklonia_colour <- colorRampPalette(c("blue", "green", "orange"))(n = 299) 

# Create heatmap
ecklonia_heat <- heatmap(ecklonia_pearson, Rowv = NA, Colv = NA,
                         col = ecklonia_colour, scale = "column",
                         margins = c(11, 8))


# How to create a data matrix in general
# ecklonia_new <- data.matrix(ecklonia_pearson)
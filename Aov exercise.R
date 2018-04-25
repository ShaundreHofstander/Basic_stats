# ANOVA ANALYSIS 
# 24 April 2018
# Shaundre'
# Extra Exercise


# load library
library(tidyverse)
library(Rmisc)
library(ggplot2)

# Exercise 1 --------------------------------------------------------------


# enter the mass at the end of the experiment

feed_1 <- c(60.8, 57.0, 65.0, 58.6, 61.7)
feed_2 <- c(68.7, 67.7, 74.0, 66.3, 69.8)
feed_3 <- c(102.6, 102.1, 100.2, 96.5)
feed_4 <- c(87.9, 84.2, 83.1, 85.7, 90.3)

# make a dataframe
bacon <- as.tibble(data.frame(
  feed = c(
    rep("Feed 1", length(feed_1)),
    rep("Feed 2", length(feed_2)),
    rep("Feed 3", length(feed_3)),
    rep("Feed 4", length(feed_4))
  ),
  mass = c(feed_1, feed_2, feed_3, feed_4)
))



# Exercise 2 --------------------------------------------------------------

# Construct suitable null and alternative hypotheses for 
# the built-in ToothGrowth data, and test your hypotheses using an ANOVA.

teeth <- datasets::ToothGrowth

# H0: There is no difference in toothgrowth at a dose of 0.5
# H1: There is a difference in toothgrowth at a dose of 0.5

teeth_1 <- teeth %>% 
  filter(dose == 0.5) %>% 
  select(len, dose)

teeth_1_aov <- aov(dose ~ len, data = teeth_1)
summary(teeth_1_aov)

# Conclusion: p value (0.149) is greater than 0.05, therefore the null hypothesis is 
# not rejected. Thus there is no significant difference in toothgrowth at a dose of 0.5.



# Exercise 3 --------------------------------------------------------------



# Find or generate your own data that lend themselves 
# to being analysed by a two-way ANOVA. 
# Generate suitable hypotheses about your data, and analyse it. 
# Supplement your analysis by providing a suitable descriptive 
# statistical summary and graph(s) of your data.

# create data
feed_1 <- c(60.8, 57.0, 65.0, 58.6, 61.7)
feed_2 <- c(68.7, 67.7, 74.0, 66.3, 69.8)
feed_3 <- c(102.6, 102.1, 100.2, 96.5, 94.2)
feed_4 <- c(87.9, 84.2, 83.1, 85.7, 90.3)

# make a dataframe
bacon <- as.tibble(data.frame(
  pig = c("Pig 1","Pig 2","Pig 3","Pig 4","Pig 5"),
  feed = c(
    rep("Feed 1", length(feed_1)),
    rep("Feed 2", length(feed_2)),
    rep("Feed 3", length(feed_3)),
    rep("Feed 4", length(feed_4))),
  mass = c(feed_1, feed_2, feed_3, feed_4)))


# H0: There is no difference in the mass of the pigs for the different feeds
# H1: There is a difference in the mass of the pigs for the different feeds

# First calculate SE and CI
bacon.summary <- summarySE(data = bacon,
                             measurevar = "mass",
                             groupvars = c("feed"))

# Visualise the data
ggplot(data = bacon, aes(x = feed, y = mass)) +
  geom_segment(data = bacon.summary, aes(x = feed, xend = feed, y = mass - ci, 
                                           yend = mass + ci, colour = feed),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = feed), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.05)

# Test the hypothesis
bacon.aov <- aov(mass ~ feed, data = bacon)
summary(bacon.aov)

# Test both hypotheses
bacon.all.aov <- aov(mass ~ feed + pig, data = bacon)
summary(bacon.all.aov)

# Conclusion: p value is below 0.05, therefore the null hypothesis is not accepted as
# there is a significant difference in the mass of the pigs for the different feeds

# Testing assumptions 

# First visualise normality of data

bacon.residuals <- residuals(bacon.all.aov)
hist(bacon.residuals)

# Visualise homoscedasticity

plot(fitted(bacon.all.aov), residuals(bacon.all.aov))

# Apply tukey test 

bacon.feed.tukey <- TukeyHSD(bacon.all.aov, which = "feed")
plot(bacon.feed.tukey)

# Visulise the factor interaction
ggplot(data = bacon, aes(x = as.numeric(feed),
                          y = mass,
                          colour = pig))+
  geom_line(size = 1.5) +
  geom_point(size = 2)


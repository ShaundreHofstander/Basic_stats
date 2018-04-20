# Day 4 of R
# 18 April 2018
# ANOVA
# Shaundre'

# Load libararies
library(tidyverse)


# t test ------------------------------------------------------------------


# First grab the data
chicks <- as_tibble(ChickWeight)

# Then subset out only the sample sets to be compared
chicks_sub <- chicks %>% 
  filter(Diet %in% c(1, 2), Time == 21)

# t-test
t.test(weight ~ Diet, data = chicks_sub)


# 1-way ANOVA -------------------------------------------------------------

# Research question: is there a difference in chicken mass attained after 21 days
# after the chickens having been fed for different diets?

# Null hypothesis: there is no difference in chicken mass at 21 days 
# after having been fed one of the four diets

chicks_21 <- chicks %>% 
  filter(Time == 21)

chicks.aov1 <- aov(weight ~ Diet, data = chicks_21)

summary(chicks.aov1)

# or a quicker way to do so
chicks.aov1 <- aov(weight ~ Diet, data = filter(chicks, Time == 21))

# Conclusion: the p value (0.00686) is smaller than 0.05, therefore we do not accept the null hypothesis
# So there is a significant difference in chicken mass at 21 days 
# after having been fed one of the four diets


# Task: What does the outcome say about the chicken masses? Which ones are different from each other?
# Task: Devise a graphical display of this outcome
#  use notch boxplot

ggplot(data = chicks.aov1, aes(x = Diet, y = weight)) +
  geom_boxplot(aes(fill = Diet), notch = TRUE)
# notches are the indentations in the boxplot
# 1 & 3 and 1 & 4 do not overlap, therefore there is a significant difference
# 2, 3 & 4 there is no significant difference because they overlap
# the ears show that the two notches are similar in size, shhowing that it is symmetrical


# Tukey HSD test ----------------------------------------------------------

TukeyHSD(chicks.aov1)

# it's comparing each of the diets to one another
# diet 2 and 1 does not result in diffferent chicken messes
# there's a difference, except for the last diet group


# ANOVA all the things ----------------------------------------------------

# load libraries


# Visuals -----------------------------------------------------------------

chicks_21 <- ChickWeight %>% 
  filter(Time == 21)

# Anova
summary(aov(weight ~ Diet, data = chicks_21))

# Tukey
TukeyHSD(aov(weight ~ Diet, data = chicks_21))

# Boxplot
ggplot(data = chicks_21, aes(x = Diet, y = weight, fill = Diet)) +
  geom_boxplot(notch = TRUE, colour = "grey50") +
  geom_segment(aes(x = Diet, xend = Diet, y = weight, yend = weight+2))

# Segments showing confidence intervals
# Dataframe of segments
chicks_Tukey <- as.data.frame(TukeyHSD(aov(weight ~ Diet, data = chicks_21))$Diet)
chicks_Tukey$pairs <- as.factor(row.names(chicks_Tukey))                              

ggplot(data = chicks_Tukey, aes(x = pairs, y = diff))+
  geom_segment(aes(x = lwr, xend = upr, y = pairs, yend = pairs))+
  geom_abline(mapping = NULL, data = NULL, slope = 90, intercept = 0)

ggplot(data = chicks_Tukey, aes(x = pairs, y = diff))+
  geom_segment(aes(x = lwr, xend = upr, y = pairs, yend = pairs))+
  geom_vline(xintercept = 0, linetype = "dotted",
             colour = "blue", size = 0.8)

# Or just plot confidence intervals the base R way 
plot(TukeyHSD(aov(weight ~ Diet, data = chicks_21)))


# Multiple factor ANOVA --------------------------------------------------------


library(tidyverse)

# H0: there is no change in chicken mass (kg) from the day 0 to day 21

chicks_0_21 <- ChickWeight %>%  
  filter(Time %in% c(0, 2, 21))

# Visualise the data
ggplot(data = chicks_0_21, aes(x = as.factor(Time), y = weight))+
  geom_boxplot(notch = T, aes(fill = as.factor(Time)))

# Run an ANOVA
summary(aov(weight ~ as.factor(Time), data = chicks_0_21))
# Perform a Tukey post-hoc test
TukeyHSD(aov(weight ~ as.factor(Time), data = chicks_0_21))

# Look at the confidence intervals
plot(TukeyHSD(aov(weight ~ as.factor(Time), data = chicks_0_21)))

#  Look only at day 0 and 21 for both Time and Diet
summary(aov(weight ~ Diet + as.factor(Time), data = filter(ChickWeight, Time %in% c(0, 21))))

# Or simply look at ALL of the Time
# which is NOT the hypothesis
summary(aov(weight ~ Diet + as.factor(Time), data = ChickWeight))

# Note the increase in the degrees of freedom for the time factor
# But no increase for the d.f. for Diet

# Now to look at the interactions BETWEEN factors
summary(aov(weight ~ Diet * as.factor(Time), data = filter(ChickWeight, Time %in% c(0, 21))))

# Lets look at the Tukey results
TukeyHSD(aov(weight ~ Diet * as.factor(Time), data = filter(ChickWeight, Time %in% c(0, 21)))) #compares every possible combination

plot(TukeyHSD(aov(weight ~ Diet * as.factor(Time), data = filter(ChickWeight, Time %in% c(0, 21)))))

# Create a line graph to help explain this concept
# First create mean values by Time and Diet
chicks_mean <- ChickWeight %>% 
  group_by(Diet, Time) %>% 
  summarise(weight_mean = mean(weight, na.rm = T))

ggplot(data = chicks_mean, aes(x = Time, y = weight_mean, colour = Diet))+
  geom_line(size = 2)+
  geom_point(shape = 15, size = 5)


# non-parametric tests ----------------------------------------------------

# But what if we dont have normal data

# Fpr a t-test we rather use a Wilcox rank sun test
wilcox.test() # And the one fills tjis in the same as for t.test()

# And now for the Kruskall-Wallis rank sum test

kruskal.test(weight ~ Diet, data = chicks_0_21) # weight by diet

# Load this for non-parametric post-hoc test
library(pgirmess)

kruskalmc(weight ~ Diet, data = chicks_0_21)

# not really possible to do a non-parametric, multi-level & multi-factor test

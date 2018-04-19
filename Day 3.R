# Day 3
# Distributions
# Shaundre'
# 17 April 2018


library(fitdistrplus)
library(logspline)

r_norm <- rnorm(n = 1000, mean = 3, sd = 1) # create normal random data

# create histogram
hist(r_norm)
descdist(r_norm, discrete = FALSE, boot = 100) #creates cullen and frey graph

# uniform data
y <- runif(100)
par(mfrow = c(1, 3)) # creates collage of graphs
plot(x = c(1:100), y = y)
hist(y)
descdist(y, discrete = FALSE)



# t-tests -----------------------------------------------------------------

# Load libraries
library(tidyverse)

# Random normal data

r_dat <- data.frame(dat = c(rnorm(n = 1000, mean = 10, sd = 3),
                            rnorm(n = 1000, mean = 8, sd = 2)),
                    sample = c(rep("A", 1000), rep("B", 1000)))

# Check assumptions

# Normality
# For this we may use the Shapiro-Wilk test
shapiro.test(r_dat$dat)

# But that is testing all of the data together we must be a bit more clever about how we make this test
r_dat %>% 
  group_by(sample) %>% 
  summarise(r_norm_dist = as.numeric(shapiro.test(dat)[2])) # as.numeric, gives us a numeric response
#this tests normality
# Remember, the data are normal when p > 0.05
# The data are non-normal when p < 0.05


# Check homoscedasticity --------------------------------------------------

# There are many ways to check for homoscedasticity
# Which is the similarity of variance between sample sets
# for now we will simply say that those assumptions are met when
# the variance of the samples are not more than 2 - 4 times greater than one another

# check the variance for everything
var(r_dat$dat)

# how to do this in a more clever way / or do it the tidy
r_dat %>% 
  group_by(sample) %>% 
  summarise(r_norm_dist = as.numeric(shapiro.test(dat)[2]),
            r_norm_var = var(dat))
# shows the normality of the ditribution and variance, and the p values there of

# we looked at these two assumptions:
# the data are normally distributed, and
# that the data are homoscedastic, and in particular, that there are no outliers



# one sample t-test -------------------------------------------------------

r_one <- data.frame(dat = rnorm(n = 20, mean = 20, sd = 5),
                    sample = "A")

# Test normality of distribution
shapiro.test(r_dat$dat)


# Perhaps a visualisation? using density plot or histogram (line 123)

# Run the test
t.test(r_one$dat, mu = 20) # against a population mean of 20 (mu)

# Run a tets we know will produce a significant result
t.test(r_one$dat, mu = 30)


# Pick a side -------------------------------------------------------------
# one sample t tests
# Are these dat smaller/less than the population mean
t.test(r_one$dat, mu = 20, alternative = "greater")

# But what about for the larger population mean?
# Are the samples less than the population of 30?
t.test(r_one$dat, mu = 30, alternative = "less")

# What about the greater than?
t.test(r_one$dat, mu = 30, alternative = "greater")




# two sample t tests ------------------------------------------------------

# Create another dataframe
r_two <- data.frame(dat = c(rnorm(n = 20, mean = 4, sd = 1),
                            rnorm(n = 20, mean = 5, sd = 1)),
                    sample = c(rep("A", 20), rep("B", 20)))

# Run a default/basic test
t.test(dat ~ sample, data = r_two, var.equal = TRUE)

# Pick a side
# Is A less than B?
t.test(dat ~ sample, data = r_two, var.equal = TRUE, alternative = "less")



# Bonus exercise ----------------------------------------------------------

# Load libraries
library(tidyverse)
library(ggpubr)

# Create random normal data/ dataframe
r_one <- data.frame(dat = rnorm(n = 20, mean = 20, sd = 5),
                    sample = "A")

# Calculate mean dat
r_one %>% 
  summarize(mean_d = mean(dat))

#  Be more specific
r_one %>% 
  group_by(sample) %>% 
  summarize(mean_d = mean(dat),
            median_d = median(dat))

# Visualize the density of the data with a desity plot
ggplot(data = filter(r_one), 
       aes(x = dat, fill = sample))+
  geom_density(alpha = 0.4)



#  Exercise 1 -------------------------------------------------------------


# Find or create your own normally distributed data and think of 
# a hypothesis you could use a t-test for. Write out the hypothesis, test it, 
# and write a one sentence conclusion for it. Provide all of the code used to accomplish this.

# Hypothesis: dog mean is less cat mean

# Load libraries
library(tidyverse)


# Random normal data
r_stuff <- data.frame(value = c(rnorm(n = 1000, mean = 12, sd = 5),
                            rnorm(n = 1000, mean = 10, sd = 4)),
                    sample = c(rep("dog", 1000), rep("cat", 1000)))


# Check assumptions

shapiro.test(r_stuff$value)


# summarise and check variance
r_stuff %>% 
  group_by(sample) %>% 
  summarise(r_norm_dist = as.numeric(shapiro.test(value)[2]),
            r_norm_var = var(value))

# two sample t-test -------------------------------------------------------

t.test(value ~ sample, data = r_stuff, var.equal = TRUE, alternative = "less")


# Conclusion: The p value is < 2.2e-16, df = 1998, t = -10.963, thus the null hypothesis is not rejected.






# t test workflow ---------------------------------------------------------


# Load the data
ecklonia <- read_csv("C:/Users/BCB User/Desktop/Intro_R_Workshop-master/data/ecklonia.csv") %>% 
  gather(key = "variable", value = "value", -species, -site, -ID)

# Visualising the data
ggplot(data = ecklonia, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip()


# filter the data
ecklonia_sub <- ecklonia %>% 
  filter(variable == "frond_mass")

# then create a new figure
ggplot(data = ecklonia_sub, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip() +
  labs(y = "stipe mass (kg)", x = "") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# Checking assumptions
ecklonia_sub %>% 
  group_by(site) %>% 
  summarise(frond_mass_var = var(value)[1],
            frond_mass_norm = as.numeric(value)[2])

# traditional output
t.test(value ~ site, data = ecklonia_sub, var.equal = TRUE, alternative = "greater")

# dataframe output
compare_means(value ~ site, data = ecklonia_sub, method = "t.test", var.equal = TRUE, 
              alternative = "greater")

# results show that Ecklonia maxima is significantly greater at Batsata Rock than at 
# Boulders Beach (p = 0, t = 6.5312, df = 24)


# End of Day 3 ------------------------------------------------------------



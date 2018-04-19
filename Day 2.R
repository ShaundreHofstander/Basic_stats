# DAY 2
# Shaundre
# 13 Aoril 2018
# The second day of stats class
# in which we discuss data visualisations and distributions


# Load libraries ----------------------------------------------------------

library(tidyverse)


# Manual calculations -----------------------------------------------------

r_dat <- data.frame(dat = rnorm(n = 600, mean = 372, sd = 50), sample = "A") # this is random r data

# Quick visualisations
ggplot(data = r_dat, aes(x = dat))+
  geom_density()


# The mean is the sum of all the points divided by the number of all the points
r_dat %>% 
  summarise(r_sum = sum(dat),
            r_n = n(),
            r_mean = r_sum/r_n,
            r_mean_func = mean(dat)) #summary if the data

# The median
# Brute force with base R
r_dat$dat[length(r_dat$dat)/2]
# or use tidy
r_dat %>% 
  arrange(dat) %>% 
  slice(12) # slice gives the whole row, use square brackets to get just one value
# or the tidy automagic way
r_dat %>%
 summarise(r_median = median(dat)) #spaces in arith doesnt not have impact

# its giving different values becase the data are not ordered
# calculate mean manually - first order the data

# Variance
# The sum of each value minus the mean squared
# Divided by the count of samples minus one 
r_dat %>% 
  mutate(r_error = dat-mean(dat),
         r_error_square = r_error * r_error)%>% 
  summarise(r_squared_sum = sum(r_error_square),
            r_var = r_squared_sum/(n()-1),
            # or use the built in function
            r_var_func = var(dat))

# The standard variation
r_dat %>% 
  summarise(r_var = var(dat),
            r_sd = sqrt(r_var),
            r_sd_func = sd(dat))
# all calcualtions needed to know


# Exercise 1 --------------------------------------------------------------
# Notice how the data summary for chicken weights contained within wt_summary is very similar to the summary 
# returned for weight when we apply summary(chicks). Please use the summarise() approach and 
# construct a data summary with exactly the same summary statistics for weight as that which summary() returns.

summary(ChickWeight$weight)

ChickWeight %>% 
  summarise(min_wt = min(weight),
            quart1 = quantile(weight, 0.25),
            med_wt = median(weight),
            mean_wt = mean(weight),
            quart3 = quantile(weight, 0.75),
            max_wt = max(weight))
  
# End of exercise


# Chapter 4: Graphical data displays --------------------------------------


# producing figures that are ready for publishing

# Load libraries
library(tidyverse)
library(ggpubr)
library(RColorBrewer)
library(viridis) #independent to R, specialised colour palettes 

# Load our SA time data
sa_time <- read_csv("SA_time.csv")

# Edit the data 
sa_time <- sa_time %>% 
mutate(human = seq(1, n(), 1))

sa_time <- sa_time %>% 
  mutate(human = seq(1, n(), 1),
                     geo = c(rep(c("cape town", "george", "PE"), times = 6), 
                             rep("joburg", 2)))
                               

# Create long data
sa_long <- sa_time %>%
   gather(key = "time_type", value = "minutes", -human, -geo)

# Qualitative data --------------------------------------------------------

# create a count of qualitative values
sa_count <- sa_long %>% 
  count(time_type) %>%  
  mutate(prop = n/sum(n))

#stacked bar graphs
ggplot(data = sa_count, aes(x = "", y = n, fill = time_type)) +
  geom_bar(width = 1, stat = "identity") +
  labs(title = "Stacked bar graph", subtitle = "cumulative sum",
       x = NULL, y = "Count") +
  theme_minimal()

# stacked proportion bar graph
ggplot(data = sa_count, aes(x = "", y = prop, fill = time_type)) +
  geom_bar(width = 1, stat = "identity") +
  scale_y_continuous(breaks = c(0.00, 0.33, 0.66, 1.00)) +
  labs(title = "Stacked bar graph", subtitle = "relative proportions",
       x = NULL, y = "Proportion") +
  theme_minimal()

# a pie chart
ggplot(data = sa_count, aes(x = "", y = n, fill = time_type)) +
  geom_bar(width = 1, stat = "identity") +
  labs(title = "Pie chart", subtitle = "but why though?",
       x = NULL, y = NULL) +
  coord_polar("y", start = 0)+
  theme_minimal()

# Continuous data --------------------------------------------------------------

# Histograms

ggplot(data = sa_long, aes(x = minutes))+
  geom_histogram()

# let's get rid of that one value
sa_clean <- sa_long %>% 
  filter(minutes < 300)

# try again
# A facted graph
ggplot(data = sa_clean, aes (x = minutes))+
  geom_histogram(aes(fill = time_type), position = "dodge")+
  facet_wrap(~time_type, ncol = 1, scales = "free_x")

# Relative proportion histogram
ggplot(data = sa_clean, aes (x = minutes))+
  geom_histogram(aes(y = ..density.., fill = time_type), 
                 position = "dodge", binwidth = 1)+
  facet_wrap(~time_type, ncol = 1, scales = "free_x")

# binwidth is the length along the x axis
# Boxplots
ggplot(data = sa_clean, aes(x = time_type, y = minutes))+
  geom_boxplot(aes(fill = time_type))

# upper and lower quartiles, the outlier is the maximum
# between the first and thrid quartile shows the interquartile range
# the smaller the space between them the smaller the distribution
# the line in the middle is the median
# the whiskers shows all data that exists from max to the quartile 

#  Notched boxplots
# no statistical diff in the time that people associate with just now and now now because the notches are overlapping
ggplot(data = sa_clean, aes(x = time_type, y = minutes))+
  geom_boxplot(aes(fill = time_type), notch = TRUE)
# use a boxplot rather than a bar graph
# calculate mean and overlay onto boxplot

# calculate summary stats for plotting over the boxplots
sa_summary_stats <- sa_clean %>%
  group_by(time_type) %>% 
  summarise(time_type_mean = mean(minutes))

# Plot mean on boxplot
ggplot(data = sa_clean, aes(x = time_type, y = minutes))+
  geom_boxplot(aes(fill = time_type), notch = TRUE)+
  geom_point(data = sa_summary_stats, size = 3, shape = 18,
             aes(y = time_type_mean), colour = "goldenrod")


# Relationships -----------------------------------------------------------


# A basic scatterplot
ggplot(data = sa_time, aes(y = now_now, x = just_now))+
  geom_point()+
  coord_equal(xlim = c(0, 60), ylim = c(0, 60))
# plot shows that now_now is closely related to just_now

# Adding trend lines
ggplot(data = sa_time, aes(y = now_now, x = just_now))+
  geom_point(aes(colour = geo))+
  geom_smooth(aes(colour = geo), method = "lm")+
   coord_equal(xlim = c(0, 60), ylim = c(0, 60))

# End of Day 2







# Bonus exercise ----------------------------------------------------------


# ANOVA test hypotheses:
# Null hypothesis: just_now and now_now are the same
# Alternative hypothesis: just_now and now_now are different

# Load our SA time data
sa_time <- read_csv("SA_time.csv")

# Load libraries
library(tidyverse)
library(dplyr)
library(ggpubr)

# Edit the data 
sa_time <- sa_time %>% 
  mutate(human = seq(1, n(), 1))

sa_time <- sa_time %>% 
  mutate(human = seq(1, n(), 1),
         geo = c(rep(c("cape town", "george", "PE"), times = 6), 
                 rep("joburg", 2)))

sa_new <- select(sa_time, human, just_now, now_now)

# Create long data
sa_type <- sa_new %>%
  gather(key = "time_type", value = "minutes", -human)

levels = c("just_now", "now_now")

levels(sa_type$time_type)


sa_new <- sa_type %>%
  group_by(time_type) %>%
  summarise(count = n(),
    mean = mean(minutes, na.rm = TRUE),
    sd = sd(minutes, na.rm = TRUE))
sa_new


ggline(sa_type, x = "time_type", y = "minutes",
       add = c("mean_se", "jitter"), 
       order = c("just_now", "now_now"),
       ylab = "Minutes", xlab = "Time type")

# Compute the analysis of variance

sa_aov <- aov(minutes ~ time_type, data = sa_type)

# Summary of the analysis

summary(sa_aov)

# the results show that the p value (0.327) is greater than the significance level of 0.05
# therefore there are no significant differences between just_now and now_now
# thus accepting the null hypothesis

# the end of exercise

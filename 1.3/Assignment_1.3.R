# 1. Import, Plot, Summarize, and Save Data
# 
# Using the Bureau of Labor Statistics data, choose a dataset that interest you. Then generate summary statistics 
# for 2 variables, plot some of the features (e.g., histograms, box plots, density plots, etc.) of several variables, 
# and save the data locally as CSV files.
# 
# 2. Explore Some Bivariate Relations
# 
# Use the same dataset within the same website to explore some bivariate relations (e.g. bivariate plot, correlation, 
# table cross table etc.)
# 
# 3. Organize a Data Report
# 
# Generate a summary report. Make sure to include: summary for every variable, structure and type of data elements, 
# discuss four results of your data.

library(tidyverse)
library(blscrapeR)
library(dplyr)
library(ggplot2)
library(broom)
library(gridExtra)

# Items of interest
# APU0000702212 - Whole Wheat Bread, lb
# APU0000703111 - Ground Chuck, lb
# APU0000706111 - Chicken, lb
# APU0000708112 - Grade AA Large Eggs, Dozen
# APU0000709112 - Whole Milk, 1 gal
# APU0000710111 - Butter, lb
# APU0000710211 - American Cheese, lb
# APU0000711111 - Red Delicious Apples, lb

# For those times when you don't have blscrapeR

url <- 'https://download.bls.gov/pub/time.series/ap/ap.data.3.Food'
data <- read.table(url, sep='\t', header = TRUE)
data$value <- as.numeric(levels(data$value))[data$value]

df1 <- spread(data, series_id, value)

cols <- c('year',
          'period',
          'date',
          Bread = 'APU0000702212    ',
          Beef = 'APU0000703111    ',
          Chicken = 'APU0000706111    ',
          Eggs = 'APU0000708111    ',
          Milk = 'APU0000709112    ',
          Butter = 'APU0000710111    ',
          Cheese = 'APU0000710211    ',
          Apples = 'APU0000711111    ')

df2 <- dateCast(df1)
df <- select(df2, cols)
# df <- rename(df3,
#               Bread = 'APU0000702212    ',
#               Beef = 'APU0000703111    ',
#               Chicken = 'APU0000706111    ',
#               Eggs = 'APU0000708111    ',
#               Milk = 'APU0000709112    ',
#               Butter = 'APU0000710111    ',
#               Cheese = 'APU0000710211    ',
#               Apples = 'APU0000711111    ')

write.csv(df, 'bls_ap_food.csv')

# For those times when you do have blscrapeR
# 
# data2 <- bls_api(c("APU0000702212    ",
#                   "APU0000703111    ",
#                   "APU0000706111    ",
#                   "APU0000708111    ",
#                   "APU0000709112    ",
#                   "APU0000710111    ",
#                   "APU0000710211    ",
#                   "APU0000711111    ")) %>%
#   spread(seriesID, value) %>%
#   dateCast() %>%
#   rename(Bread = APU0000702212,
#          Beef = APU0000703111,
#          Chicken = APU0000706111,
#          Eggs = APU0000708111,
#          Milk = APU0000709112,
#          Butter = APU0000710111,
#          Cheese = APU0000710211,
#          Apples = APU0000711111)


ggplot(data = df, aes(x = date)) +
  geom_line(aes(y = Bread, colour = 'Bread')) +
  geom_line(aes(y = Beef, colour = 'Beef')) +
  geom_line(aes(y = Chicken, colour = 'Chicken')) +
  geom_line(aes(y = Eggs, colour = 'Eggs')) +
  geom_line(aes(y = Milk, colour = 'Milk')) +
  geom_line(aes(y = Butter, colour = 'Butter')) +
  geom_line(aes(y = Cheese, colour = 'Cheese')) +
  geom_line(aes(y = Apples, colour = 'Apples')) +
  labs(title = "Cost of Food Staples") +
  ylab("value") +
  theme(legend.position = "top", plot.title = element_text(hjust = .5))

df_recent <- df %>% filter(dplyr::between(df$year, 2008, 2019))

hist1 <- ggplot(df_recent, aes(x = Milk)) +
  geom_histogram(binwidth = .2,
                 colour = "black",
                 fill = "white") +
  xlab("Cost of Milk ($/gal)") +
  geom_vline(aes(xintercept = mean(Milk)),
             colour = "blue",
             size = .5) +
  geom_density(alpha = .2,
               fill = '#FF6666')

hist2 <- ggplot(df_recent, aes(x = Cheese)) +
  geom_histogram(binwidth = .25,
                 colour = "black",
                 fill = "white") +
  xlab("Cost of Cheese ($/lb)") +
  geom_vline(aes(xintercept = mean(Cheese, na.rm = TRUE)),
             colour = "blue",
             size = .5) +
  geom_density(alpha = .2,
               fill = '#FF6666')

grid.arrange(hist1, hist2, ncol = 2)

summary(df_recent$Bread)

sd(df_recent$Bread)

ggplot(df, aes(x = date, y = Beef)) +
  geom_smooth(method = 'lm') +
  geom_point(color = 'black')

box <- ggplot(df_recent, aes(x = year, y = Oranges, group = year)) +
  geom_boxplot() +
  ylab("Cost of Oranges ($/lb)") +
  scale_x_continuous(breaks = c(2009, 2012, 2015, 2018))

cor(df$Milk, df$Cheese, use = "complete.obs", method = "pearson")

ggplot(df, aes(x = Milk, y = Cheese)) +
  geom_point() +
  ggtitle("Cost of Milk vs. Cost of Cheese") +
  geom_smooth(method = lm)

paste("DataFrame Structure: ")
str(df)
'\  '
paste("Summary of Variables :")
clean_sum <- summary(df[c('Bread', 'Beef', 'Chicken', 'Eggs', 'Milk', 'Cheese', 'Apples', 'Oranges')])

clean_sum %>% kable %>% kable_styling()

do.call(cbind, lapply(df[4:11], summary))

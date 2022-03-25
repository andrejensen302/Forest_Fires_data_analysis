
library(tidyverse)
library(GGally)
library(ggplot2)

setwd("~/Documents/UW Data Analytics")
ff = read.delim('forestfires.tsv', header = TRUE, sep = '\t')
head(ff)
tail(ff)

#Create log_area
ff$log_area = log10(ff$area + 1)

#1)


#Evaluate correlation of temp and area using ggpairs

ggpairs(select(ff, temp, log_area))
summary(ff$temp)

#Evaluate correlation of wind and area using ggpairs

ggpairs(select(ff, wind, log_area))

#2)

#Evaluate correlation between FFMC and burn area.
ggpairs(select(ff, FFMC, log_area))

#Evaluate correlation between RH and burn area.

ggpairs(select(ff, RH, log_area))

#Evaluate correlation between DC and burn area.

ggpairs(select(ff, DC, log_area))


#3)

#Make 'month' into a factor variable and change the order.

ff$month = factor(ff$month, levels = c('jan', 'feb', 'mar', 'apr', 'may', 'jun',
                                       'jul', 'aug', 'sep', 'oct', 'nov', 'dec'))

burn_area_by_month = ff %>% group_by(month) %>% summarize(burn_area_mean = mean(area), n =n ()) 
burn_log_area_mean_by_month = ff %>% group_by(month) %>% summarize(burn_log_area_mean_by_month = mean(log_area), n = n())






#Load libraries

library(ggplot2)
library(dplyr)
library(GGally)
library(caret)

#Load and name forestfires.tsv file

setwd("~/Documents/UW Data Analytics")
ff = forestfires
ff = read.delim('forestfires.tsv', header = TRUE, sep = '\t')

#skim ff file

head(ff)
tail(ff)

#Convert X and Y coordinates to factor variable

ff$X = factor(ff$X)
ff$Y = factor(ff$Y)

#Create log_area
ff$log_area = log10(ff$area + 1)

#is winter factor. Hypothesis that the burn area will be the lowest since it is the coldest
#time of the year in the Northern Hemisphere

#create is_winter column
ff$is_winter = ifelse(ff$month %in% c('dec', 'jan', 'feb'), 1, 0)
head(ff)
summary(ff)

#convert is_winter column into a factor variable
ff$is_winter = factor(ff$is_winter)
is(ff$is_winter)

#plot temp, RH, wind, and burn_area comparing it against non-winter months

plot_winter = select(ff, temp, RH, wind, log_area, is_winter) %>% filter(log_area > 0)

ggpairs(plot_winter, mapping = aes(color = is_winter, alpha = 0.5))

#Split Data into test and train sets
in_train = createDataPartition(y = ff$log_area, p = 0.80, list = FALSE)

head(in_train)

#Display all columns in both training and test sets
ff_train = ff[in_train, ]
ff_test = ff[-in_train, ]


#Pre-processing steps, centering and scaling

preprocessing_steps = preProcess(select(ff, FFMC,DMC, DC, ISI, temp, RH, wind, rain),
                                 method = c('center', 'scale'))

ff_train_processing = predict(preprocessing_steps, newdata = ff_train)
head(ff_train_processing)

ff_test_processing = predict(preprocessing_steps, newdata = ff_test)
head(ff_test_processing)

help(predict)

#Near zero variances

nearZeroVar(ff_train_processing, saveMetrics = TRUE)

table(ff_train_processing$rain)

#Update to pre-processing steps above to remove near zero variance variables

preprocessing_steps = preProcess(select(ff, FFMC,DMC, DC, ISI, temp, RH, wind, rain),
                                 method = c('center', 'scale', 'nzv'))

ff_train_processing = predict(preprocessing_ dsteps, newdata = ff_train)
head(ff_train_processing)

ff_test_processing = predict(preprocessing_steps, newdata = ff_test)
head(ff_test_processing)

nearZeroVar(ff_train_processing, saveMetrics = TRUE)

preprocessing_steps

#The rain variable was removed for near zero variance.



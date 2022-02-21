#Maching Learning Analysis 1: Feature Engineering

#load libraries

library(ggplot2)
library(dplyr)
library(GGally)
library(caret)

#load and name tabbles
setwd("~/Documents/UW Data Analytics")
ff = read.delim('forestfires.tsv', header = TRUE, sep = '\t')

head(ff)

#Look at what kind of data types the columns are.
is(ff$X)

ff$X = factor(ff$X)
ff$Y = factor(ff$Y)
is(ff$X)
is(ff$Y)


#is weekend factor & convert it to a factor variable
ff$is_weekend = ifelse(ff$day %in% c('sat', 'sun'), 1, 0)
head(ff)
ff$is_weekend = factor(ff$is_weekend)
is(ff$is_weekend)

#Add log_area column to table.

ff$log_area = log10(ff$area + 1)
head(ff)

plot_ff = select(ff,temp, RH, wind, log_area, is_weekend) %>% filter(log_area > 0)
head(select(ff,temp, RH, wind, log_area, is_weekend) %>% filter(log_area > 0))

ggpairs(plot_ff, mapping = aes(color = is_weekend, alpha  = 0.50))


#Machine Learning Analysis 2: Binarizing Categories

#create binary features
month = model.matrix(~ month - 1, data = ff)
day = model.matrix(~ day - 1, data = ff)
x = model.matrix(~ X - 1, data = ff)
y = model.matrix(~ Y -1, data = ff)

head(month)
head(day)

ff = cbind(ff, month, day, x, y)

help(model.matrix)
help(cbind)

#Machine Learning Analysis 3: Splitting into Test and Train

in_train = createDataPartition(y = ff$log_area, p = 0.80, list = FALSE)

ff_train = ff[in_train, ]
ff_test = ff[-in_train, ]

summary(ff_test)

#Machine Learning Analysis 4: Preprocessing 1:Centering and Scaling

#Pre-Process
# centering and scaling

preprocessing_steps = preProcess(select(ff, FFMC, DMC, DC, ISI, temp, RH, wind, rain),
                                 method = c('center', 'scale'))

help(preProcess)

ff_train_proc = predict(preprocessing_steps, newdata = ff_train)
ff_test_proc = predict(preprocessing_steps, newdata = ff_test)

head(ff_train_proc)
head(ff_test_proc)

#Machine Learning Analysis 5: Preprocessing 2: Removing Near Zero Variance Features

#near zero variance

nearZeroVar(ff_train_proc, saveMetrics = TRUE)

preprocessing_steps = preProcess(ff, method = c('center', 'scale', 'nzv'))
preprocessing_steps

nrow(ff)

#Tutorial: Regression with the LM function (continuation of the tutorial data) from the previous module)

#Regression, y ~ x + x2 + x3

july_ff = filter(ff_train_proc, monthjul == 1)

model = lm(log_area ~ DMC, data = july_ff)

#observe attributes of the model
attributes(model)

#get model output
summary(model)

ggplot(july_ff, aes(x = DMC, y = log_area)) + geom_point() + 
  geom_abline(intercept = 0.5327, slope = 0.1408, col = 'red')

multi_model = lm(log_area ~ DMC + wind, data = ff_train_proc)
summary(multi_model)

#Tutorial: Regression with the Train Function
#method = 'lm' is specifying the train function to use linear regression

model_fit = train(log_area ~ DMC + is_weekend,
                  data = ff_train_proc,
                  method = 'lm',
                  metric = 'RMSE')

model_fit

#Use test set to generate the predictions
pred = predict(model_fit, newdata = ff_test_proc)

head(pred)
  
#view RMSE
postResample(pred = pred, obs = ff_test_proc$log_area)

#Create new dataframe for errors
errors = data.frame(predicted = pred, observed = ff_test_proc$log_area,
                    error = pred - ff_test_proc$log_area)

head(errors)

#plot with the assumption that it rises as fast as it goes forward (slope  = 1)
ggplot(data = errors, aes(x = predicted, y = observed)) +
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, color = 'red')

#Tutorial 3 - The full model (a regression model is every available predictor in it)

#full model
full_model = train(log_area ~., data = ff_train_proc, method = 'lm')


pred = predict(full_model, newdata = ff_test_proc$log_area)

#view the RMSE
postResample(pred = full_model, obs = ff_test_proc$log_area)

errors = data.frame(predicted = pred,
                    observed = ff_test_proc$log_area,
                    error = pred - ff_test_proc$log_area)

ggplot(data = errors, aes(x = predicted, y = observed)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, color = 'red')

help(train)
help(postResample)
help("resamples")

#Tutorial: Forward selection

forward_model = train(log_area ~., data = ff_train_proc, method = 'leapForward', 
                      tuneGrid = expand.grid(nvmax = 1:20),
                      trControl = trainControl(method = 'cv', number = 10))

attributes(forward_model)
forward_model$bestTune

#diagnostic plots
plot(forward_model)

#above plot shows the number of predictors where the errors were the lowest.

plot(varImp(forward_model))
#above plot (variable importance plot) show the most important variables for predicting burn area.

#Tutorial: Ridge Regression
#ridge regression is a model tuning method that is meant to analyze data that suffers from
#multicollinearity.

ridge_model = train(log_area ~., data = ff_train_proc, method = 'ridge',
                    tuneLength = 10,
                    trControl = trainControl(method = 'cv', number = 10))

plot(ridge_model)

#Tutorial: Model Comparison and Selection of a Final Model

results = resamples(list(full_model = full_model,
                         forward_model = forward_model,
                         ridge_model = ridge_model))

dotplot(results)
#dotplot can be used to generate confidence intervals for the data models



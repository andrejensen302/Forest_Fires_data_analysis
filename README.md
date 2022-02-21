# Forest Fires Data Exploration and Analysis #
Forest Fires Data Set (UCI repository)

The purpose of this analysis is to use create a model to predict burn area. After some data wrangling and variable type conversions, the data was split into training and testing sets to create and test the model.

Libraries used

```
#Load libraries
library(ggplot2)
library(dplyr)
library(GGally)
library(caret)
```

Glimpse at the dataset

```
Rows: 517
Columns: 15
$ X         <fct> 7, 7, 7, 8, 8, 8, 8, 8, 8, 7, 7, 7, 6, 6, 6, 6, 5, 8, 6, 6, 6, 5, 7, 7, 7, 7, 7, 7, 6, 6, 6, 6, 6…
$ Y         <fct> 5, 4, 4, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3…
$ month     <chr> "mar", "oct", "oct", "mar", "mar", "aug", "aug", "aug", "sep", "sep", "sep", "sep", "aug", "sep",…
$ day       <chr> "fri", "tue", "sat", "fri", "sun", "sun", "mon", "mon", "tue", "sat", "sat", "sat", "fri", "mon",…
$ FFMC      <dbl> 86.2, 90.6, 90.6, 91.7, 89.3, 92.3, 92.3, 91.5, 91.0, 92.5, 92.5, 92.8, 63.5, 90.9, 92.9, 93.3, 9…
$ DMC       <dbl> 26.2, 35.4, 43.7, 33.3, 51.3, 85.3, 88.9, 145.4, 129.5, 88.0, 88.0, 73.2, 70.8, 126.5, 133.3, 141…
$ DC        <dbl> 94.3, 669.1, 686.9, 77.5, 102.2, 488.0, 495.6, 608.2, 692.6, 698.6, 698.6, 713.0, 665.3, 686.5, 6…
$ ISI       <dbl> 5.1, 6.7, 6.7, 9.0, 9.6, 14.7, 8.5, 10.7, 7.0, 7.1, 7.1, 22.6, 0.8, 7.0, 9.2, 13.9, 7.8, 3.0, 6.3…
$ temp      <dbl> 8.2, 18.0, 14.6, 8.3, 11.4, 22.2, 24.1, 8.0, 13.1, 22.8, 17.8, 19.3, 17.0, 21.3, 26.4, 22.9, 15.1…
$ RH        <int> 51, 33, 33, 97, 99, 29, 27, 86, 63, 40, 51, 38, 72, 42, 21, 44, 27, 47, 35, 44, 40, 38, 44, 43, 3…
$ wind      <dbl> 6.7, 0.9, 1.3, 4.0, 1.8, 5.4, 3.1, 2.2, 5.4, 4.0, 7.2, 4.0, 6.7, 2.2, 4.5, 5.4, 5.4, 4.9, 4.0, 4.…
$ rain      <dbl> 0.0, 0.0, 0.0, 0.2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.…
$ area      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
$ log_area  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
$ is_winter <fct> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
```

Data Wrangling

- Convert X and Y Coordinates into factor variable.
- Create a new column, is_winter, to test our hypothesis that the burn area will be the lowest in the coldest months of the year since the dataset is for burn data in Europe.

```
#create is_winter column
ff$is_winter = ifelse(ff$month %in% c('dec', 'jan', 'feb'), 1, 0)
```
<img src ="https://github.com/andrejensen302/Forest_Fires_data_analysis/blob/7fa1a9d4e85c4e97d0924a56d36b2476272de54e/forest-fires-Rmarkdown_files/figure-gfm/unnamed-chunk-5-1.png">


Source: https://archive.ics.uci.edu/ml/datasets/forest+fires

Citation Request:
This dataset is public available for research. The details are described in [Cortez and Morais, 2007].
Please include this citation if you plan to use this database:

P. Cortez and A. Morais. A Data Mining Approach to Predict Forest Fires using Meteorological Data.
In J. Neves, M. F. Santos and J. Machado Eds., New Trends in Artificial Intelligence,
Proceedings of the 13th EPIA 2007 - Portuguese Conference on Artificial Intelligence, December,
Guimaraes, Portugal, pp. 512-523, 2007. APPIA, ISBN-13 978-989-95618-0-9.
Available at: http://www.dsi.uminho.pt/~pcortez/fires.pdf

Title: Forest Fires

Sources
Created by: Paulo Cortez and An�bal Morais (Univ. Minho) @ 2007

Past Usage:

P. Cortez and A. Morais. A Data Mining Approach to Predict Forest Fires using Meteorological Data.
In Proceedings of the 13th EPIA 2007 - Portuguese Conference on Artificial Intelligence,
December, 2007. (http://www.dsi.uminho.pt/~pcortez/fires.pdf)

In the above reference, the output "area" was first transformed with a ln(x+1) function.
Then, several Data Mining methods were applied. After fitting the models, the outputs were
post-processed with the inverse of the ln(x+1) transform. Four different input setups were
used. The experiments were conducted using a 10-fold (cross-validation) x 30 runs. Two
regression metrics were measured: MAD and RMSE. A Gaussian support vector machine (SVM) fed
with only 4 direct weather conditions (temp, RH, wind and rain) obtained the best MAD value:
12.71 +- 0.01 (mean and confidence interval within 95% using a t-student distribution). The
best RMSE was attained by the naive mean predictor. An analysis to the regression error curve
(REC) shows that the SVM model predicts more examples within a lower admitted error. In effect,
the SVM model predicts better small fires, which are the majority.

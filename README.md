# Forest_Fires_data_analysis
Forest Fires Data Set (UCI repository)

The purpose of this analysis is to use create a model to predict burn area. After some data wrangling and variable type conversions, the data was split into training and testing sets to create and test the model.

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

## ----Setup, include=FALSE-------------------------------------------------------------------------------------------------
#Specifying the knitr global options for chunks of code
knitr::opts_chunk$set(echo = FALSE, 
                      eval=TRUE,
                      cache=FALSE, 
                      cache.lazy = FALSE,
                      message=FALSE,
                      warning=FALSE,
                      error = FALSE,
                      fig.align="center",
                      out.width = "100%",
                      out.height = "50%",
                      tidy.opts = list(width.cutoff = 20),
                      tidy = TRUE,
                      purl=TRUE
                      )


## ----Installing required packages-----------------------------------------------------------------------------------------
#removing cache/__packages
if (file.exists("cache/__packages")) unlink("cache/__packages")
#Installing required packages if there are not already installed
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(Rborist)) install.packages("Rborist", repos = "http://cran.us.r-project.org")
if(!require(xgboost)) install.packages("xgboost", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(patchwork)) install.packages("patchwork", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(psych)) install.packages("psych", repos = "http://cran.us.r-project.org")
if(!require(cowplot)) install.packages("cowplot", repos = "http://cran.us.r-project.org")
if(!require(formatR)) install.packages("formatR", repos = "http://cran.us.r-project.org")


## ----Loading packages-----------------------------------------------------------------------------------------------------
#Loading required packages
library(tidyverse)
library(caret)
library(Rborist)
library(xgboost)
library(data.table)
library(lubridate)
library(kableExtra)
library(ggplot2)
library(patchwork)
library(ggthemes)
library(psych)
library(cowplot)
library(formatR)


## ----Download dataset-----------------------------------------------------------------------------------------------------
##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

# Concrete Compressive Strength dataset:
# https://archive.ics.uci.edu/ml/machine-learning-databases/concrete/compressive/Concrete_Data.xls

if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
library(data.table)

dl <- tempfile()
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/concrete/compressive/Concrete_Data.xls", dl)

concrete <- as.data.table(readxl::read_xls(dl, col_names = TRUE))

rm(dl)



## ----Gathering Info About Data--------------------------------------------------------------------------------------------
#Finding column names in the data
cnames <- colnames(concrete)
#Finding total number of admixture instances (i.e. rows) in concrete dataset
no_rows <- nrow(concrete)

rm(cnames, no_rows)


## ----Rename column names--------------------------------------------------------------------------------------------------
#Rename column names
colnames(concrete) <- c("cement", "slag", "ash", "water", "superplasticizer", "coarse_agg", "fine_agg", "age", "strength")
head(concrete, 5) %>%
  kable(caption = "First 5 Rows of Concrete Data Set with Renamed Columns", align = "cl") %>%
  column_spec(column = 1:9, border_left = TRUE, border_right = TRUE) %>%
  kable_styling(full_width=FALSE,
                font_size = 11,
                latex_options = "HOLD_position",
                position = "center")


## ----Create train and test sets, echo = FALSE-----------------------------------------------------------------------------
#Test set will be 20% of concrete data set
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = concrete$strength, times = 1, p = 0.2, list = FALSE)
train_set <- concrete[-test_index,]
test_set <- concrete[test_index,]


## ----RMSE function--------------------------------------------------------------------------------------------------------
#Define the RMSE function
rmse <- function(actual_ratings, predicted_ratings){
  sqrt(mean((actual_ratings - predicted_ratings)^2, na.rm=TRUE))
}



## ----Table 1: Columns names with explanation, tidy=TRUE, tidy.opts = list(width.cutoff = 20)------------------------------
#Write column names & explanations
columns <- c("cement", "slag", "ash", "water", "superplasticizer", "coarse_agg", "fine_agg", "age", "strength")
explanations <- c("Cement used in concrete making measured in kilograms (kg) in a cubic meter (m3) mixture",
                  "Blast furnace slag, a calcium-silicate product used in concrete making, measured in kilograms (kg) in a cubic meter (m3) mixture",
                  "Fly ash, a byproduct from burning coal used in concrete making, measured in kilograms (kg) in a cubic meter (m3) mixture",
                  "Water used in concrete making measured in kilograms (kg) in a cubic meter (m3) mixture",
                  "Superplasticizer, an additive in concrete making to enhance workability and reduce water content, measured in kilograms (kg) in a cubic meter (m3) mixture",
                  "Coarse Aggregate, irregular broken stones having a size of at least 4.75 mm or 3/16 inches used in concrete making, measured in kilograms (kg) in a cubic meter (m3) mixture",
                  "Fine Aggregate, sand or crushed stone particles used in concrete making, measured in kilograms (kg) in a cubic meter (m3) mixture",
                  "Age of concrete measured in days",
                  "Concrete Compressive Strength, the capacity of concrete to withstand compressive weight, measured in megapascals (MPa)")

#Show columns & their explanations in a table
data.frame(columns, explanations) %>%
  kable(format = "latex", caption = "Columns", col.names = c("Column", "Explanations"), align = "cl") %>%
  column_spec(column = 1:2, border_left = TRUE, border_right = TRUE) %>%
  column_spec(column = 2, border_right = TRUE, width = "40em") %>%
  kable_styling(full_width=FALSE,
                font_size = 9,
                latex_options = "HOLD_position",
                position = "center")

#Remove stored data & values
rm(columns, explanations)


## ----Print 5 rows of train_set--------------------------------------------------------------------------------------------
#Print 5 rows of train_set
head(train_set, 5) %>%
  kable(caption = "First 5 Rows of Train Set", align = "ccccccccc") %>%
  column_spec(column = 1:9, border_left = TRUE, border_right = TRUE) %>%
  kable_styling(full_width=FALSE,
                font_size = 11,
                latex_options = "HOLD_position",
                position = "center")


## ----Print 5 rows of test_set---------------------------------------------------------------------------------------------
#Print 5 rows of test_set
head(test_set, 5) %>%
  kable(caption = "First 5 Rows of Test Set", align = "ccccccccc") %>%
  column_spec(column = 1:9, border_left = TRUE, border_right = TRUE) %>%
  kable_styling(full_width=FALSE,
                font_size = 11,
                latex_options = "HOLD_position",
                position = "center")


## ----Table 1 - Summary Statistics for data set, out.width = '200px'-------------------------------------------------------
concrete %>% select(1:5) %>% summary() %>% as.data.frame.matrix(row.names = FALSE) %>%
  kable(caption = "Summary Statistics for Concrete Data Set", align = "ccccc") %>%
  column_spec(column = 1:5, border_left = TRUE, border_right = TRUE) %>%
  kable_styling(full_width=FALSE,
                font_size = 11,
                latex_options = "HOLD_position",
                position = "center")


concrete %>% select(6:9) %>% summary() %>% as.data.frame.matrix(row.names = FALSE) %>%
  kable(align = "cccc") %>%
  column_spec(column = 1:4, border_left = TRUE, border_right = TRUE) %>%
  kable_styling(full_width=FALSE,
                font_size = 11,
                latex_options = "HOLD_position",
                position = "center")



## ----Figures 1 & 2 - Distribution of Cement & Blast Furnace Slag in the Data Set, fig.width = 11, fig.height = 5, fig.fullwidth = TRUE----
#Plot distribution of amount of cement in the concrete data set
fig1 <- concrete %>%
  ggplot(aes(x=cement)) +
  geom_histogram(binwidth = 50, boundary = 0 )+
  labs(title = "Figure 1: Distribution of Cement Amount\n\n", 
       caption = "\n*Width of each bin is 50",
       x = "\nCement (kg in a m3 mixture)", 
       y = "Frequency\n")+
  #scale_x_discrete(breaks=NULL,
  #                 labels=NULL) +
  theme_economist() +
  theme(plot.title = element_text(size = 14),
        plot.caption = element_text(size = 12, hjust = 0))

#Plot distribution of amount of blast furnace slag in the concrete data set
fig2 <- concrete %>%
  ggplot(aes(x=slag)) +
  geom_histogram(binwidth = 50, boundary = 0)+
  labs(title = "Figure 2: Distribution of Blast Furnace Slag\n\n", 
       caption = "\n*Width of each bin is 50",
       x = "\nBlast Furnace Slag (kg in a m3 mixture)", 
       y = "Frequency\n")+
  #scale_x_discrete(breaks=NULL,
  #                 labels=NULL) +
  theme_economist() +
  theme(plot.title = element_text(size = 14),
        plot.caption = element_text(size = 12, hjust = 0))

#plot_grid(fig1, fig2, fig2, fig1, nrow=2)
fig1 + fig2



## ----Figures 3 & 4 - Distribution of Ash & Water in the Data Set, fig.width = 11, fig.height = 5, fig.fullwidth = TRUE----
#Plot distribution of amount of fly ash in the concrete data set
fig3 <- concrete %>%
  ggplot(aes(x=ash)) +
  geom_histogram(binwidth = 20, boundary = 0)+
  labs(title = "Figure 3: Distribution of Fly Ash Amount\n\n", 
       caption = "\n*Width of each bin is 20",
       x = "\nFly Ash (kg in a m3 mixture)", 
       y = "Frequency\n")+
  #scale_x_discrete(breaks=NULL,
  #                 labels=NULL) +
  theme_economist() +
  theme(plot.title = element_text(size = 14),
        plot.caption = element_text(size = 12, hjust = 0))

#Plot distribution of amount of water in the concrete data set
fig4 <- concrete %>%
  ggplot(aes(x=water)) +
  geom_histogram(binwidth = 20, boundary = 0)+
  labs(title = "Figure 4: Distribution of Water Amount\n\n", 
       caption = "\n*Width of each bin is 20",
       x = "\nWater (kg in a m3 mixture)", 
       y = "Frequency\n")+
  #scale_x_discrete(breaks=NULL,
  #                 labels=NULL) +
  theme_economist() +
  theme(plot.title = element_text(size = 14),
        plot.caption = element_text(size = 12, hjust = 0))

fig3 + fig4



## ----Figures 5 & 6 - Distribution of Superplasticizer & Coarse Aggregate in the Data Set, fig.width = 11, fig.height = 5, fig.fullwidth = TRUE----
#Plot distribution of amount of superplasticizer in the concrete data set
fig5 <- concrete %>%
  ggplot(aes(x=superplasticizer)) +
  geom_histogram(binwidth = 2.5, boundary = 0)+
  labs(title = "Figure 5: Distribution of Superplasticizer\n\n", 
       caption = "\n*Width of each bin is 2.5",
       x = "\nSuperplasticizer (kg in a m3 mixture)", 
       y = "Frequency\n")+
  #scale_x_discrete(breaks=NULL,
  #                 labels=NULL) +
  theme_economist() +
  theme(plot.title = element_text(size = 14),
        plot.caption = element_text(size = 12, hjust = 0))

#Plot distribution of amount of coarse aggregate in the concrete data set
fig6 <- concrete %>%
  ggplot(aes(x=coarse_agg)) +
  geom_histogram(binwidth = 50, boundary = 0)+
  labs(title = "Figure 6: Distribution of Coarse Aggregate\n\n", 
       caption = "\n*Width of each bin is 50",
       x = "\nCoarse Aggregate (kg in a m3 mixture)", 
       y = "Frequency\n")+
  #scale_x_discrete(breaks=NULL,
  #                 labels=NULL) +
  theme_economist() +
  theme(plot.title = element_text(size = 14),
        plot.caption = element_text(size = 12, hjust = 0))

fig5 + fig6



## ----Figures 7 & 8 - Distribution of Fine Aggregate & Age of Concrete in the Data Set, fig.width = 11, fig.height = 5, fig.fullwidth = TRUE----
#Plot distribution of amount of fine aggregate in the concrete data set
fig7 <- concrete %>%
  ggplot(aes(x=fine_agg)) +
  geom_histogram(binwidth = 50, boundary = 0)+
  labs(title = "Figure 7: Distribution of Fine Aggregate\n\n", 
       caption = "\n*Width of each bin is 50",
       x = "\nFine Aggregate (kg in a m3 mixture)", 
       y = "Frequency\n")+
  #scale_x_discrete(breaks=NULL,
  #                 labels=NULL) +
  theme_economist() +
  theme(plot.title = element_text(size = 14),
        plot.caption = element_text(size = 12, hjust = 0))

#Plot distribution of age of concrete in the concrete data set
fig8 <- concrete %>%
  ggplot(aes(x=age)) +
  geom_histogram(binwidth = 25, boundary = 0)+
  labs(title = "Figure 8: Distribution of Age of Concrete\n\n", 
       caption = "\n*Width of each bin is 25",
       x = "\nAge of Concrete", 
       y = "Frequency\n")+
  #scale_x_discrete(breaks=NULL,
  #                 labels=NULL) +
  theme_economist() +
  theme(plot.title = element_text(size = 14),
        plot.caption = element_text(size = 12, hjust = 0))

fig7 + fig8



## ----Figure 9 - Distribution of Compressive Strength of Concrete in the Data Set, fig.width = 11, fig.height = 5, fig.fullwidth = TRUE----
#Plot distribution of amount of fine aggregate in the concrete data set
fig9 <- concrete %>%
  ggplot(aes(x=strength)) +
  geom_histogram(binwidth = 10, boundary = 0)+
  labs(title = "Figure 9: Distribution of Concrete Compressive Strength\n\n", 
       caption = "\n*Width of each bin is 10",
       x = "\nConcrete Compressive Strength (MPa, i.e. MegaPascals)", 
       y = "Frequency\n")+
  #scale_x_discrete(breaks=NULL,
  #                 labels=NULL) +
  theme_economist() +
  theme(plot.title = element_text(size = 14),
        plot.caption = element_text(size = 12, hjust = 0))

fig9



## ----Figure 10 - Correlation matrix of the Data Set, fig.width = 10, fig.height = 10, fig.fullwidth = TRUE----------------
#Plot distribution of amount of fine aggregate in the concrete data set
fig10 <- corPlot(concrete,
                 main = "Figure 10: Correlation Matrix",
                 cex = 1.25,
                 scale = FALSE,
                 xlas=2)



## ----Standardization of train and test sets-------------------------------------------------------------------------------
y_train <- train_set$strength
y_test <- test_set$strength

#train_set[,1:8] %>% mutate_all(scale)
col_means <- colMeans(train_set[,1:8])
col_stds <- apply(train_set[,1:8], 2, sd)
train_scaled <- train_set[,1:8]
for(i in 1:ncol(train_scaled)) {
  train_scaled[,i] <- (train_scaled[,..i]-col_means[i])/col_stds[i]
}

test_scaled <- test_set[,1:8]
for(i in 1:ncol(train_scaled)) {
  test_scaled[,i] <- (test_scaled[,..i]-col_means[i])/col_stds[i]
}

train_scaled <- cbind(train_scaled, y_train)


## ----Results Table for test set-------------------------------------------------------------------------------------------
#Create Results table for the test set to store RMSE of each model
results_test <- data.frame(matrix(ncol = 2, nrow=0))
#Give column names
colnames(results_test) <- c("Model", "RMSE")
#Change type of column Model to character
results_test$Model <- as.character(results_test$Model)
#Change type of column RMSE to double
results_test$RMSE <- as.double(results_test$RMSE)



## ----Linear Regression----------------------------------------------------------------------------------------------------
#This model uses the 8 predictors to predict the concrete compressive strength
lr_model <- lm(y_train ~ ., data = train_scaled)


## ----Linear Regression Coefficients Table---------------------------------------------------------------------------------
summary(lr_model)$coefficients %>% 
  kable(caption = "Linear Regression Coefficients Table",align = "ccccc") %>%
  column_spec(column = 1:5, border_left = TRUE, border_right = TRUE) %>%
  kable_styling(full_width=FALSE,
                font_size = 11,
                latex_options = "HOLD_position",
                position = "center")


## ----Results - Linear Regression------------------------------------------------------------------------------------------
#This model uses the 8 predictors to predict the concrete compressive strength
pred_lr= predict(lr_model,test_scaled)
lr_rmse <- RMSE(y_test, pred_lr)
#Store RMSE for test set in Results table
results_test <- bind_rows(results_test, 
                              data.frame(
                                Model = "Linear Regression",
                                RMSE = lr_rmse)
                              )
#Remove duplicates from Results table in case code is run more than once
results_test <- results_test[!duplicated(results_test[, c("Model","RMSE")]),]


## ----KNN fitting----------------------------------------------------------------------------------------------------------
#This KNN model uses the 8 predictors to predict the concrete compressive strength
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
knn_model <- train(y_train ~ ., 
                   method = "knn", 
                   data = train_scaled)


## ----Results - KNN--------------------------------------------------------------------------------------------------------
#Predict concrete compressive strength using the KNN model
pred_knn= predict(knn_model,test_scaled)
knn_rmse <- RMSE(y_test, pred_knn)
#Store RMSE for test set in Results table
results_test <- bind_rows(results_test, 
                              data.frame(
                                Model = "KNN",
                                RMSE = knn_rmse)
                              )
#Remove duplicates from Results table in case code is run more than once
results_test <- results_test[!duplicated(results_test[, c("Model","RMSE")]),]



## ----KNN+CV fitting-------------------------------------------------------------------------------------------------------
#This KNN+CV model uses the 8 predictors to predict the concrete compressive strength
control <- trainControl(method = "cv", search="grid", number = 10, p = .9)
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)
knn_cv_model <- train(y_train ~ .,
                      method = "knn",
                      data = train_scaled,
                      tuneGrid = data.frame(k = seq(1, 51, 2)),
                      trControl = control)



## ----Results - KNN+CV-----------------------------------------------------------------------------------------------------
#Predict concrete compressive strength using the KNN+CV model
pred_knn_cv= predict(knn_cv_model,test_scaled)
knn_cv_rmse <- RMSE(y_test, pred_knn_cv)
#Store RMSE for test set in Results table
results_test <- bind_rows(results_test, 
                              data.frame(
                                Model = "KNN+CV",
                                RMSE = knn_cv_rmse)
                              )
#Remove duplicates from Results table in case code is run more than once
results_test <- results_test[!duplicated(results_test[, c("Model","RMSE")]),]



## ----Random Forest fitting------------------------------------------------------------------------------------------------
#Random Forest
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)
rf_model <-  train(y_train ~ .,
                   method = "Rborist",
                   data = train_scaled)


## ----Results - RF---------------------------------------------------------------------------------------------------------
#Predict concrete compressive strength using the Random Forest model
pred_rf= predict(rf_model,test_scaled)
rf_rmse <- RMSE(y_test, pred_rf)
#Store RMSE for test set in Results table
results_test <- bind_rows(results_test, 
                              data.frame(
                                Model = "Random Forest",
                                RMSE = rf_rmse)
                              )
#Remove duplicates from Results table in case code is run more than once
results_test <- results_test[!duplicated(results_test[, c("Model","RMSE")]),]



## ----Variable Importance - RF---------------------------------------------------------------------------------------------
#Feature Importance
varImp_rf<- varImp(rf_model, scale = FALSE)[["importance"]]
varImp_rf$Overall <- varImp_rf$Overall / max(varImp_rf$Overall)
varImp_rf %>% arrange(desc(Overall)) %>% 
  kable(caption = "Relative Variable Importance of Random Forest Model",align = "cc") %>%
  column_spec(column = 1:2, border_left = TRUE, border_right = TRUE) %>%
  kable_styling(full_width=FALSE,
                font_size = 11,
                latex_options = "HOLD_position",
                position = "center")



## ----Random Forest + CV fitting-------------------------------------------------------------------------------------------
#Random Forest with grid search and cross-validation
control <- trainControl(method="cv", search = "grid", number = 10, p = 0.9)
grid <- expand.grid(minNode = seq(2, 5) , predFixed = seq(2, 8))
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)
rf_cv_model <-  train(y_train ~ .,
                   method = "Rborist",
                   data = train_scaled,
                   trControl = control,
                   tuneGrid = grid)



## ----Results - RF+CV------------------------------------------------------------------------------------------------------
#Predict concrete compressive strength using Random Forest+CV model
pred_rf_cv= predict(rf_cv_model,test_scaled)
rf_cv_rmse <- RMSE(y_test, pred_rf_cv)
#Store RMSE for test set in Results table
results_test <- bind_rows(results_test, 
                              data.frame(
                                Model = "Random Forest+CV",
                                RMSE = rf_cv_rmse)
                              )
#Remove duplicates from Results table in case code is run more than once
results_test <- results_test[!duplicated(results_test[, c("Model","RMSE")]),]



## ----Variable Importance - RF+CV------------------------------------------------------------------------------------------
#Feature Importance
varImp_rf_cv<- varImp(rf_cv_model, scale = FALSE)[["importance"]]
varImp_rf_cv$Overall <- varImp_rf_cv$Overall / max(varImp_rf_cv$Overall)
varImp_rf_cv %>% arrange(desc(Overall)) %>% 
  kable(caption = "Relative Variable Importance of Random Forest+CV Model",align = "cc") %>%
  column_spec(column = 1:2, border_left = TRUE, border_right = TRUE) %>%
  kable_styling(full_width=FALSE,
                font_size = 11,
                latex_options = "HOLD_position",
                position = "center")



## ----XGB fitting----------------------------------------------------------------------------------------------------------
#XGBoost
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)
xgb_model <-  train(y_train ~ .,
                   method = "xgbTree",
                   data = train_scaled,
                   verbosity = 0)



## ----Results - XGB--------------------------------------------------------------------------------------------------------
#Predict concrete compressive strength using the XGBoost model
pred_xgb= predict(xgb_model,test_scaled)
xgb_rmse <- RMSE(y_test, pred_xgb)
#Store RMSE for test set in Results table
results_test <- bind_rows(results_test, 
                              data.frame(
                                Model = "XGBoost",
                                RMSE = xgb_rmse)
                              )
#Remove duplicates from Results table in case code is run more than once
results_test <- results_test[!duplicated(results_test[, c("Model","RMSE")]),]



## ----Variable Importance - XGB--------------------------------------------------------------------------------------------
#Feature Importance
varImp_xgb<- varImp(xgb_model, scale = FALSE)[["importance"]]
varImp_xgb$Overall <- varImp_xgb$Overall / max(varImp_xgb$Overall)
varImp_xgb %>% arrange(desc(Overall)) %>% 
  kable(caption = "Relative Variable Importance of XGBoost Model",align = "cc") %>%
  column_spec(column = 1:2, border_left = TRUE, border_right = TRUE) %>%
  kable_styling(full_width=FALSE,
                font_size = 11,
                latex_options = "HOLD_position",
                position = "center")



## ----XGBoost + CV fitting-------------------------------------------------------------------------------------------------
#XGBoost with grid search and cross-validation
control <- trainControl(method="cv", search = "grid", number = 10, p = 0.9)
grid <- expand.grid(nrounds=seq(100, 500, 50), max_depth = seq(2, 7), eta = seq(0.05, 0.3, 0.05), gamma = 0, colsample_bytree = 0.6, min_child_weight = 1, subsample = 1)
options(warn = -1)
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)
suppressWarnings(
xgb_cv_model <-  train(y_train ~ .,
                   method = "xgbTree",
                   data = train_scaled,
                   trControl = control,
                   tuneGrid = grid,
                   verbosity = 0)
)



## ----Results - XGB+CV-----------------------------------------------------------------------------------------------------
#Predict concrete compressive strength using the XGBoost+CV model
pred_xgb_cv= predict(xgb_cv_model,test_scaled)
xgb_cv_rmse <- RMSE(y_test, pred_xgb_cv)
#Store RMSE for test set in Results table
results_test <- bind_rows(results_test, 
                              data.frame(
                                Model = "XGBoost+CV",
                                RMSE = xgb_cv_rmse)
                              )
#Remove duplicates from Results table in case code is run more than once
results_test <- results_test[!duplicated(results_test[, c("Model","RMSE")]),]



## ----Variable Importance - XGB+CV-----------------------------------------------------------------------------------------
#Feature Importance
varImp_xgb_cv<- varImp(xgb_cv_model, scale = FALSE)[["importance"]]
varImp_xgb_cv$Overall <- varImp_xgb_cv$Overall / max(varImp_xgb_cv$Overall)
varImp_xgb_cv %>% arrange(desc(Overall)) %>% 
  kable(caption = "Relative Variable Importance of XGBoost+CV Model",align = "cc") %>%
  column_spec(column = 1:2, border_left = TRUE, border_right = TRUE) %>%
  kable_styling(full_width=FALSE,
                font_size = 11,
                latex_options = "HOLD_position",
                position = "center")



## ----Table 7: Results Table for All models--------------------------------------------------------------------------------
#Show Validation Results Table for All models
results_test %>%
  kable(caption = "RMSEs of All Models Using Test Set", align="c") %>%
  column_spec(column = 1: ncol(results_test), border_left = TRUE, border_right = TRUE) %>%
  kable_styling(full_width=FALSE,
                font_size = 13,
                latex_options = c("HOLD_position","striped"),
                position = "center")


## ----Remove stored models and values--------------------------------------------------------------------------------------
##Remove stored models and values if they exist
if(exists("lr_rmse")) rm(lr_rmse)
if(exists("pred_lr")) rm(pred_lr)
if(exists("lr_model")) rm(lr_model)
if(exists("knn_rmse")) rm(knn_rmse)
if(exists("pred_knn")) rm(pred_knn)
if(exists("knn_model")) rm(knn_model)
if(exists("knn_cv_rmse")) rm(knn_cv_rmse)
if(exists("pred_knn_cv")) rm(pred_knn_cv)
if(exists("knn_cv_model")) rm(knn_cv_model)
if(exists("rf_rmse")) rm(rf_rmse)
if(exists("pred_rf")) rm(pred_rf)
if(exists("rf_model")) rm(rf_model)
if(exists("rf_cv_rmse")) rm(rf_cv_rmse)
if(exists("pred_rf_cv")) rm(pred_rf_cv)
if(exists("rf_cv_model")) rm(rf_cv_model)
if(exists("xgb_rmse")) rm(xgb_rmse)
if(exists("pred_xgb")) rm(pred_xgb)
if(exists("xgb_model")) rm(xgb_model)
if(exists("xgb_cv_rmse")) rm(xgb_cv_rmse)
if(exists("pred_xgb_cv")) rm(pred_xgb_cv)
if(exists("xgb_cv_model")) rm(xgb_cv_model)
if(exists("varImp_rf")) rm(xgb_cv_model)
if(exists("varImp_rf_cv")) rm(xgb_cv_model)
if(exists("varImp_xgb")) rm(xgb_cv_model)
if(exists("varImp_xgb_cv")) rm(xgb_cv_model)



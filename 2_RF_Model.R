
########### Initializations ###############

# Load Libraries
library(h2o)
library(dplyr)
library(caret)


# Initialize the h2o cluster
h2o.init(nthreads=-1)

# set seed for reproducibility
set.seed(123) 

# Load in the cleaned data set
source("1_Data_Cleaning.R")


########### Functions ###############

# Function to train rf models and tune hyperparameters, selecting the best one based on logloss
random_forest = function(train,
                         response,
                         predictors,
                         params){
  
  
  # Create a training set from the 1st dataset in the split
  all_data_train <- as.h2o(train)
  
  
  # Initialize a list to store the best models & performance
  best_rf_models <- list()
  rf_performance <- list()
  
  
  # Train the random forest model
  rf_grid <- h2o.grid(
    algorithm = "randomForest",
    grid_id = paste0("rf_grid_",response),
    hyper_params = params,
    training_frame = all_data_train,
    x = predictors,
    y = response,
    fold_assignment = "Stratified",
    nfolds = 10,
    parallelism = 0,
    seed = 123
  )
  
  
  # Get the best performing random forest model, sorted by logloss
  rf_grid_perf <- h2o.getGrid(grid_id = paste0("rf_grid_",response), sort_by = "logloss", decreasing = F)
  best_rf_model <- h2o.getModel(rf_grid_perf@model_ids[[1]])
  
  return(best_rf_model)
}


# Function to determine correct and incorrect predictions
model_accuracy = function(model,
                          test,
                          response){
  
  # Create a training set
  all_data_test <- as.h2o(test)
  
  # Make predictions on the test set
  predictions <- h2o.predict(model, newdata = all_data_test)
  predictions<-as.data.frame(predictions$predict)
  
  # get the predictions and actual results and combined them in the test partition
  accuracy_test_df = as.data.frame(all_data_test)
  accuracy_test_df$predictions = as.character(unlist(predictions))
  accuracy_test_df[[response]]  = as.character(accuracy_test_df[[response]])
  
  # Calculate the number of correct predictions
  correct_predictions <- ifelse(accuracy_test_df$predictions == accuracy_test_df[[response]], 1, 0)
  
  # Calculate the overall accuracy
  accuracy = sum(correct_predictions)/nrow(accuracy_test_df)
  print(paste0("accuracy = ",accuracy))
  
  # add column indicating a correct or incorrect prediction
  accuracy_test_df$correct = as.numeric(correct_predictions)
  
  return(accuracy_test_df)
}


########### Random Forest Prediction Model ###############

# NOTE - running separate models requires H2O to be restarted. For replicability, all models were trained from a 
#        fresh reset of R

# NOTE - Must comment out two greenhouse sections below so only one model is created at a time


#  Predictors & Grid parameters ----

# predictors - can be adjusted to replicate specific models
predictors <- c("Soil_Moisture",	"Soil_Temperature", "Grass_Height", "NO3_N",	"NH4_N",
                "Clay",	"Total_Silt",	"Total_Sand",	"Soil_BD", "Total_C",	"Total_N",	"Slope",	"Distance_to_nearest_tree",
                "Distance_to_nearest_water","pH")


# grid parameters - can be adjusted to replicate specific models
rf_params <- list(
  ntrees = c(50, 100, 200),  # Number of trees
  max_depth = c(3, 5, 7),  # Maximum depth of the trees
  sample_rate = c(0.7, 0.8, 0.9),  # Row sampling rate
  mtries = c(1:8,10,15))
 



# N2O model ----

# Run grid search & select best model on current oversampling/predictor configurations
best_rf_model_N2O <- random_forest(train_data_oversampled_N2O,"N2O_N_Bin",predictors,rf_params)

# Get the variable importance
varimp_N2O <- h2o.varimp(best_rf_model_N2O)

# View the hyperparameters of the selected model
rf_grid_perf <- h2o.getGrid(grid_id = paste0("rf_grid_","N2O_N_Bin"), sort_by = "logloss", decreasing = F)
View(as.data.frame(rf_grid_perf@summary_table))

# retrieve the model performance to view Log loss
perf <- h2o.performance(best_rf_model_N2O, as.h2o(test_data_N2O))
perf


# N2O accuracy function
accuracy_test_df_N2O <- model_accuracy(best_rf_model_N2O,test_data_N2O,"N2O_N_Bin")

# view N2O class accuracies
accuracy_test_df_N2O %>%
  group_by(N2O_N_Bin) %>%
  summarize(sum(correct)/n())



# # CO2 model ----
# 
# # Run grid search & select best model on current oversampling/predictor configurations
# best_rf_model_CO2 <- random_forest(train_data_oversampled_CO2,"CO2_C_Bin",predictors,rf_params)
# 
# # Get the variable importance
# varimp_CO2 <- h2o.varimp(best_rf_model_CO2)
# 
# # View the hyperparameters of the selected model
# rf_grid_perf <- h2o.getGrid(grid_id = paste0("rf_grid_","CO2_C_Bin"), sort_by = "logloss", decreasing = F)
# View(as.data.frame(rf_grid_perf@summary_table))
# 
# # retrieve the model performance to view Log loss
# perf <- h2o.performance(best_rf_model_CO2, testh2o)
# perf
# 
# # CO2 accuracy function
# accuracy_test_df_CO2 <- model_accuracy(best_rf_model_CO2,test_data_CO2,"CO2_C_Bin")
# 
# # show CO2 class accuracies
# accuracy_test_df_CO2 %>%
#   group_by(CO2_C_Bin) %>%
#   summarize(sum(correct)/n())



# # CH4 model ----
# 
# # Run grid search & select best model on current oversampling/predictor configurations
# best_rf_model_CH4 <- random_forest(train_data_oversampled_CH4,"CH4_C_Bin",predictors,rf_params)
# 
# # Get the variable importance
# varimp_CH4 <- h2o.varimp(best_rf_model_CH4)
# 
# # View the hyperparameters of the selected model
# rf_grid_perf <- h2o.getGrid(grid_id = paste0("rf_grid_","CH4_C_Bin"), sort_by = "logloss", decreasing = F)
# View(as.data.frame(rf_grid_perf@summary_table))
# 
# 
# # retrieve the model performance to view Log loss
# perf <- h2o.performance(best_rf_model_CH4, as.h2o(test_data_CH4))
# perf
# 
# 
# # CH4 accuracy function
# accuracy_test_df_CH4 <- model_accuracy(best_rf_model_CH4,test_data_CH4,"CH4_C_Bin")
# 
# # show CH4 class accuracies
# accuracy_test_df_CH4 %>%
#   group_by(CH4_C_Bin) %>%
#   summarize(sum(correct)/n())





# Shut down the H2O cluster
h2o.shutdown(prompt = FALSE)
















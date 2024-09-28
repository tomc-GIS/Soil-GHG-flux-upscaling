## Soil-GHG-flux-upscaling

This repository contains the R scripts and source data that was used to train the random forest prediction models in a masters dissertation where the aim was to use machine learning to upscale greenhouse gas fluxes in an urban greenspace. The code for the spatial predictions is not included due to the data limits of a GitHub repository being too low to include the raster files required for the process.

# Instructions

All files must be within the same folder. To reproduce the models, execute 2_RF_Model.R in R studio after installing the necessary packages, this will automatically run the data cleaning script. Due to the H2O memory limitations, only one greenhouse gas prediction model can be created when running the script. The sections of the other two greenhouse gasses should be commented out. Each new model requires a fresh reset of R for replicability.

# Repository Contents

1_Data_Cleaning.R (R Script) - 	
Contains the code for all the data cleaning and oversampling rates. If specific models are trying to be reproduced, the oversampling rates in this file need to be manually adjusted and saved before executing the 2_RF_Model.R script.

2_RF_Model.R (R Script) -		
Contains the code for training the random forest models to predict hot spots and cold spots of greenhouse gas fluxes. If specific models are trying to be reproduced, the hyperparameters and predictor variables in this file need to be manually adjusted and saved before executing the 2_RF_Model.R script.

Collected data.xlsx (Excel workbook) - 		
Contains the source data used to train the models.


# Necessary Packages
The following R packages are required for this project:

- `h2o`
- `readxl`
- `dplyr`
- `caret`
- `lubridate`

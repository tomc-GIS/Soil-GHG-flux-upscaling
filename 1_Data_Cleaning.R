
########### Initializations ###############

# Load Libraries
library(readxl)
library(dplyr)
library(caret)
library(lubridate)


# Read in the study data
all_data_df <- read_excel("Collected data.xlsx")

# set seed for reproducibility
set.seed(123) 

########### Functions ###############


# Define seasons based on the day of the year
get_season <- function(day_of_year) {
  if (day_of_year >= 152 & day_of_year <= 243) {
    return("Summer")
  } else if (day_of_year >= 244 & day_of_year <= 334) {
    return("Fall")
  } else if (day_of_year >= 335 | day_of_year <= 59) {
    return("Winter")
  } else if (day_of_year >= 60 & day_of_year <= 151) {
    return("Spring")
  }
}

# Function for grass height imputation to replace NA with the mean of the column grouped by Site_Number 
# and season, excluding specific columns 
fill_na_with_mean_season <- function(df, exclude_cols = c("CO2_C", "N2O_N", "CH4_C")) {
  df %>%
    group_by(Site_Number, season) %>%
    mutate(across(-all_of(exclude_cols), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
    ungroup() %>%
    group_by(Site_Number) %>%
    mutate(across(-all_of(exclude_cols), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
    ungroup()
}

# Function to calculate thresholds and categorize into hot spots & cold spots while ignoring NA values
categorize_spots <- function(data, column) {
  
  # Get the Lower quartile, Upper quartile & Median
  Q1 <- quantile(data[[column]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[column]], 0.75, na.rm = TRUE)
  M <- median(data[[column]], na.rm = TRUE)
  
  # set the threshold values as per wangari et al.
  hot_spot_threshold <- M + (Q3 - Q1)
  cold_spot_threshold <- M - (Q3 - Q1)
  
  # create new bin column for the categories
  bin_column <- paste(column, "Bin", sep = "_")
  
  # categorise
  data <- data %>%
    mutate(!!bin_column := case_when(
      .data[[column]] > hot_spot_threshold ~ "hot spot",
      .data[[column]] < cold_spot_threshold ~ "cold spot",
      is.na(.data[[column]]) ~ NA_character_,
      TRUE ~ "normal"
    ))
  
  # print the threshold values
  print(paste(column, hot_spot_threshold))
  print(paste(column, cold_spot_threshold))
  return(data)
  
}




########### Data Cleaning & Gap Filling ###############

# create clean data dataframe
clean_data_df <- all_data_df


# # Omit rows where all GHG fluxes are NA
clean_data_df <- clean_data_df[!(is.na(clean_data_df$N2O_N) & is.na(clean_data_df$CH4_C) & is.na(clean_data_df$CO2_C)), ]


# Add season column to the dataset
clean_data_df <- clean_data_df %>%
  mutate(season = sapply(Day_of_year, get_season))


# create week & month of year columns
clean_data_df <- clean_data_df %>%
  mutate(week_of_year = lubridate::isoweek(Date),
         month_of_year = lubridate::month(Date))


# remove impossible values
clean_data_df <- clean_data_df %>%
  filter(CO2_C >= 1) %>%
  filter(CH4_C >= -70)


# remove the last 7 measurement dates (all data after 21st November)
# Add date column
clean_data_df$Date <- as.Date(clean_data_df$Date)

# Find the unique dates and sort them
unique_dates <- sort(unique(clean_data_df$Date), decreasing = TRUE)

# Identify the last 7 unique dates
last_7_dates <- unique_dates[1:7]

# Filter out rows that have a date within the last 7 unique dates
clean_data_df <- clean_data_df[!clean_data_df$Date %in% last_7_dates, ]


# create linear regression models to fill soil temp & soil moisutre na values
temp_lm = lm(Soil_Temperature~Average_Air_Temperature+Average_Wind_Speed+Precipitation+Clay+Silt+Coarse_silt+Coarse_sand+Elevation+Slope +
               Distance_to_nearest_tree + Distance_to_nearest_water+Distance_to_nearest_bush+Day_of_year+Soil_classification, clean_data_df)
mois_lm = lm(Soil_Moisture~Average_Air_Temperature+Average_Wind_Speed+Precipitation+Clay+Silt+Coarse_silt+Coarse_sand+Elevation+Slope +
               Distance_to_nearest_tree + Distance_to_nearest_water+Distance_to_nearest_bush+Day_of_year+Soil_classification, clean_data_df)

# check r^2 of models
summary(temp_lm)
summary(mois_lm)


# predict soil temp & soil moisture na values
pred_temp = predict(temp_lm, clean_data_df)
pred_moisture = predict(mois_lm, clean_data_df)

clean_data_df$inter_sm = 0; clean_data_df$inter_st = 0

for (i in 1:nrow(clean_data_df)) {
  sm = clean_data_df[i,'Soil_Moisture']; st = clean_data_df[i,'Soil_Temperature']
  
  if (is.na(sm)==T) { clean_data_df[i,'Soil_Moisture'] = pred_moisture[i]; clean_data_df[i,'inter_sm'] = 1 }
  if (is.na(st)==T) { clean_data_df[i,'Soil_Temperature'] = pred_temp[i]; clean_data_df[i,'inter_st'] = 1 }
}


# Calculate the missing soil_wfps values
clean_data_df <- clean_data_df %>%
  mutate(Soil_WFPS = Soil_Moisture / (1 - (Soil_BD / 2.65)))


# Fill in missing grass height values
clean_data_df <- fill_na_with_mean_season(clean_data_df)




########### Oversampling ##############

# NOTE 1 - Oversampling rates were iterated through by manually adjusting the rates for each iteration
# NOTE 2 - Due to where the seed was set. To properly replicate CH4 oversampling, N2O & CO2 must be set 
#          to their final configurations (2x hotspot oversampling for CO2 & 6x hot spot oversampling for CO2).
#          Likewise to properly replicate CO2, N2O must be set to its final configuration.

# N2O ----

# Create new N2O dataframe
N2O_cleaned <- clean_data_df

# Omit rows where N2O is na
N2O_cleaned <- N2O_cleaned[!is.na(N2O_cleaned$N2O_N),]

# apply the categorisation function
N2O_cleaned <- categorize_spots(N2O_cleaned, "N2O_N")

# convert the bin columns to factors
N2O_cleaned$N2O_N_Bin <- as.factor(N2O_cleaned$N2O_N_Bin)

# remove cold spots from data - only 5 so not enough for analysis
N2O_cleaned <- N2O_cleaned %>% filter(N2O_N_Bin != "cold spot")

# Split the data into training and test sets (80% training, 20% testing)
train_index_N2O <- createDataPartition(N2O_cleaned$N2O_N_Bin, p = 0.8, list = FALSE)
train_data_N2O <- N2O_cleaned[train_index_N2O, ]
test_data_N2O <- N2O_cleaned[-train_index_N2O, ]

# Separate the training data into "hot spot" and other categories
hot_spot_data_N2O <- train_data_N2O %>% filter(N2O_N_Bin == "hot spot")
other_data_N2O <- train_data_N2O %>% filter(N2O_N_Bin != "hot spot")


# Perform 2x random oversampling on the "hot spot" data
hot_spot_oversampled_N2O <- hot_spot_data_N2O %>% sample_n(nrow(hot_spot_data_N2O) * 2, replace = TRUE)


# Randomly undersample the non-hotspot data to reduce it (undersampling not actually used)
other_data_undersampled_N2O <- other_data_N2O %>% sample_frac(1)


# Combine the oversampled "hot spot" data with the other training data
train_data_oversampled_N2O <- bind_rows(hot_spot_oversampled_N2O, other_data_undersampled_N2O)



# CO2 ----

# Create new CO2 dataframe
CO2_cleaned <- clean_data_df

# Omit rows where CO2 is na
CO2_cleaned <- CO2_cleaned[!is.na(CO2_cleaned$CO2_C),]

# apply the categorisation function
CO2_cleaned <- categorize_spots(CO2_cleaned, "CO2_C")

# convert the bin columns to factors
CO2_cleaned$CO2_C_Bin <- as.factor(CO2_cleaned$CO2_C_Bin)

# Split the data into training and test sets (80% training, 20% testing)
train_index_CO2 <- createDataPartition(CO2_cleaned$CO2_C_Bin, p = 0.8, list = FALSE)
train_data_CO2 <- CO2_cleaned[train_index_CO2, ]
test_data_CO2 <- CO2_cleaned[-train_index_CO2, ]

# Separate the training data into "hot spot" and other categories
hot_spot_data_CO2 <- train_data_CO2 %>% filter(CO2_C_Bin == "hot spot")
other_data_CO2 <- train_data_CO2 %>% filter(CO2_C_Bin != "hot spot")

# Perform 6x random oversampling on the "hot spot" data
hot_spot_oversampled_CO2 <- hot_spot_data_CO2 %>% sample_n(nrow(hot_spot_data_CO2) * 6, replace = TRUE)

# Randomly undersample the non-hotspot data (undersampling not actually used)
other_data_undersampled_CO2 <- other_data_CO2 %>% sample_frac(1)

# Combine the oversampled "hot spot" data with the undersampled other training data
train_data_oversampled_CO2 <- bind_rows(hot_spot_oversampled_CO2, other_data_undersampled_CO2)



# CH4 ----

# Create new CH4 dataframe
CH4_cleaned <- clean_data_df

# Omit rows where CH4 is na
CH4_cleaned <- CH4_cleaned[!is.na(CH4_cleaned$CH4_C),]

# apply the categorisation function
CH4_cleaned <- categorize_spots(CH4_cleaned, "CH4_C")

# convert the bin columns to factors
CH4_cleaned$CH4_C_Bin <- as.factor(CH4_cleaned$CH4_C_Bin)

# Split the data into training and test sets (80% training, 20% testing)
train_index_CH4 <- createDataPartition(CH4_cleaned$CH4_C_Bin, p = 0.8, list = FALSE)
train_data_CH4 <- CH4_cleaned[train_index_CH4, ]
test_data_CH4 <- CH4_cleaned[-train_index_CH4, ]

# Separate the training data into "hot spot", "cold spot" and "normal"
hot_spot_data_CH4 <- train_data_CH4 %>% filter(CH4_C_Bin == "hot spot")
cold_spot_data_CH4 <- train_data_CH4 %>% filter(CH4_C_Bin == "cold spot")
normal_data_CH4 <- train_data_CH4 %>% filter(CH4_C_Bin == "normal")

# Perform 7x random oversampling on the "hot spot" data
hot_spot_oversampled_CH4 <- hot_spot_data_CH4 %>% sample_n(nrow(hot_spot_data_CH4) * 7, replace = TRUE)

# Perform 2x random oversampling on the "cold spot" data
cold_spot_oversampled_CH4 <- cold_spot_data_CH4 %>% sample_n(nrow(cold_spot_data_CH4) * 2, replace = TRUE)

# Randomly undersample the non-hotspot data (undersampling not actually used)
normal_data_undersampled_CH4 <- normal_data_CH4 %>% sample_frac(1)

# Combine the oversampled hot spot data with the oversampled cold spot data and other training data
train_data_oversampled_CH4 <- bind_rows(hot_spot_oversampled_CH4, cold_spot_oversampled_CH4, normal_data_undersampled_CH4)





































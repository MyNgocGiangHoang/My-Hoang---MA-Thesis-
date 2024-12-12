rm(list = ls())

# Install and load necessary packages
install.packages("readr")
install.packages(c("rlang", "cli", "readr"), dependencies = TRUE)
#install.packages("readxl", dependencies = TRUE)
install.packages("dplyr")
install.packages("writexl")
install.packages("tidyverse")
install.packages("minqa", type = "binary")
install.packages("lme4", type = "binary")
install.packages("SparseM", type = "binary")
install.packages("afex", type = "binary")
install.packages("mvtnorm", type = "binary")
install.packages("emmeans", type = "binary")
install.packages("gridExtra")
install.packages("car")
# Load the necessary libraries
library(readxl)
library(dplyr)
library(readr)
library(tidyverse)
library(janitor)
library(readxl)
library(tidyr)
library(stringr)
library(writexl)
library(lme4)
library(afex)
library(ggplot2)
library(emmeans)
library(sjPlot)
library(gridExtra)
library(car)
library(grid)  
library(lmerTest)

# import behavioral data
# Spain and Norway session 2

# Define file paths for Spain participants
spain_files <- c(
"/Users/hoangmy/Desktop/thesis workspace/data/Spain data/Session 2/subject-1.csv",
"/Users/hoangmy/Desktop/thesis workspace/data/Spain data/Session 2/subject-2.csv",
"/Users/hoangmy/Desktop/thesis workspace/data/Spain data/Session 2/subject-3.csv",
"/Users/hoangmy/Desktop/thesis workspace/data/Spain data/Session 2/subject-4.csv",
"/Users/hoangmy/Desktop/thesis workspace/data/Spain data/Session 2/subject-5.csv",
"/Users/hoangmy/Desktop/thesis workspace/data/Spain data/Session 2/subject-6.csv",
"/Users/hoangmy/Desktop/thesis workspace/data/Spain data/Session 2/subject-7.csv",
"/Users/hoangmy/Desktop/thesis workspace/data/Spain data/Session 2/subject-8.csv",
"/Users/hoangmy/Desktop/thesis workspace/data/Spain data/Session 2/subject-9.csv",
"/Users/hoangmy/Desktop/thesis workspace/data/Spain data/Session 2/subject-10.csv",
"/Users/hoangmy/Desktop/thesis workspace/data/Spain data/Session 2/subject-11.csv",
"/Users/hoangmy/Desktop/thesis workspace/data/Spain data/Session 2/subject-12.csv",
"/Users/hoangmy/Desktop/thesis workspace/data/Spain data/Session 2/subject-13.csv",
"/Users/hoangmy/Desktop/thesis workspace/data/Spain data/Session 2/subject-14.csv",
"/Users/hoangmy/Desktop/thesis workspace/data/Spain data/Session 2/subject-15.csv",
"/Users/hoangmy/Desktop/thesis workspace/data/Spain data/Session 2/subject-17.csv",
"/Users/hoangmy/Desktop/thesis workspace/data/Spain data/Session 2/subject-18.csv"
)

# Define file paths for Norway participants
norway_files <- c(
"/Users/hoangmy/Desktop/thesis workspace/data/Norway data/Session 2/subject-1.csv",
"/Users/hoangmy/Desktop/thesis workspace/data/Norway data/Session 2/subject-2.csv",
"/Users/hoangmy/Desktop/thesis workspace/data/Norway data/Session 2/subject-3.csv",
"/Users/hoangmy/Desktop/thesis workspace/data/Norway data/Session 2/subject-4.csv",
"/Users/hoangmy/Desktop/thesis workspace/data/Norway data/Session 2/subject-5.csv",
"/Users/hoangmy/Desktop/thesis workspace/data/Norway data/Session 2/subject-6.csv",
"/Users/hoangmy/Desktop/thesis workspace/data/Norway data/Session 2/subject-7.csv",
"/Users/hoangmy/Desktop/thesis workspace/data/Norway data/Session 2/subject-8.csv",
"/Users/hoangmy/Desktop/thesis workspace/data/Norway data/Session 2/subject-9.csv",
"/Users/hoangmy/Desktop/thesis workspace/data/Norway data/Session 2/subject-10.csv",
"/Users/hoangmy/Desktop/thesis workspace/data/Norway data/Session 2/subject-11.csv",
"/Users/hoangmy/Desktop/thesis workspace/data/Norway data/Session 2/subject-12.csv",
"/Users/hoangmy/Desktop/thesis workspace/data/Norway data/Session 2/subject-13.csv",
"/Users/hoangmy/Desktop/thesis workspace/data/Norway data/Session 2/subject-14.csv",
"/Users/hoangmy/Desktop/thesis workspace/data/Norway data/Session 2/subject-15.csv",
"/Users/hoangmy/Desktop/thesis workspace/data/Norway data/Session 2/subject-16.csv"
)
# Define subject IDs for Spain and Norway
spain_subject_ids <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 17, 18)
norway_subject_ids <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)

# Function to load, filter, and add subject identifier for a participant
load_and_prepare_data <- function(file_path, subject_id) {
  
# Load the CSV file
data <- read.csv(file_path)
  
# Remove 'Test' rows by filtering for "experiment"
data <- data[data$session_part != "test", ]
  
# Check if the data frame is empty after filtering
if (nrow(data) == 0) {
warning(paste("File", file_path, "has no experiment data after filtering. Skipping."))
return(NULL)  # Skip empty data frames
}

# Add subject identifier
data$subject <- subject_id

# REMOVE NA values in the 'grammaticality' column
data <- data[!is.na(data$grammaticality), ]
return(data)
} 

# Load and prepare data for all Spain and Norway participants
spain_data_list <- mapply(load_and_prepare_data, spain_files, spain_subject_ids, SIMPLIFY = FALSE)
norway_data_list <- mapply(load_and_prepare_data, norway_files, norway_subject_ids, SIMPLIFY = FALSE)

# Remove NULL and empty data frames
spain_data_list <- Filter(function(x) !is.null(x) && nrow(x) > 0, spain_data_list)
norway_data_list <- Filter(function(x) !is.null(x) && nrow(x) > 0, norway_data_list)

# Combine all columns from both Spain and Norway datasets
all_columns_combined <- unique(c(
unlist(lapply(spain_data_list, colnames)),
unlist(lapply(norway_data_list, colnames))
))

# Function to standardize columns
standardize_columns <- function(df, all_columns) {
missing_cols <- setdiff(all_columns, colnames(df))  # Identify missing columns
df[missing_cols] <- NA  # Add missing columns with NA values
return(df)
}

# Standardize columns for both Spain and Norway data using combined columns
spain_data_list <- lapply(spain_data_list, standardize_columns, all_columns = all_columns_combined)
norway_data_list <- lapply(norway_data_list, standardize_columns, all_columns = all_columns_combined)

# Merge Spain and Norway datasets
merged_data_spain <- do.call(rbind, spain_data_list)
merged_data_norway <- do.call(rbind, norway_data_list)

# Add a 'country' column to identify data from Norway and Spain
merged_data_norway$country <- "Norway"
merged_data_spain$country <- "Spain"

# Combine both datasets into one
Behavioral_session2_combined_data <- rbind(merged_data_spain, merged_data_norway)

# Check the merged dataset
head(Behavioral_session2_combined_data)
str(Behavioral_session2_combined_data)

###############################
### Calculate mean accuracy:Behavioral data Session 2 ###

# Ensure the 'correct_grammaticality_judgement_response' column is treated as numeric
Behavioral_session2_combined_data$correct_grammaticality_judgement_response <- as.numeric(
Behavioral_session2_combined_data$correct_grammaticality_judgement_response
)

# Calculate mean accuracy per participant per condition
mean_accuracy_by_participant <- Behavioral_session2_combined_data %>%
group_by(subject, country, grammaticality) %>%  # Group by participant (subject), country, and grammaticality
summarise(
correct_responses = sum(correct_grammaticality_judgement_response, na.rm = TRUE),  # Sum of correct responses
total_responses = n(),  # Total number of responses
accuracy_percentage = (correct_responses / total_responses) * 100  # Calculate percentage accuracy
)

# Check the accuracy dataset
head(mean_accuracy_by_participant)

# Save the mean_accuracy_by_participant dataframe to a CSV file
write.csv(mean_accuracy_by_participant, "/Users/hoangmy/Desktop/thesis workspace/data/mean_accuracy_by_participant.csv", row.names = FALSE)

# Save the combined behavioral dataset
write.csv(Behavioral_session2_combined_data, "/Users/hoangmy/Desktop/thesis workspace/data/Behavioral_session2_combined_data.csv", row.names = FALSE)

####################
### Data Session 1 ### 
# List of Excel file paths
file_paths <- c(
"/Users/hoangmy/Desktop/thesis workspace/spain_session1_fixed_4_ASRT_DGS.xlsx",
"/Users/hoangmy/Desktop/thesis workspace/spain_session1_fixed_8_ASRT_ASRT.xlsx"
)

# Specify the directory to save CSV files
output_dir <- "/Users/hoangmy/Desktop/thesis workspace/"

# Function to convert Excel sheets to CSV
convert_excel_to_csv <- function(file_path) {
sheets <- excel_sheets(file_path)  # Get sheet names
  
for (sheet in sheets) {  # Loop through each sheet
data <- read_excel(file_path, sheet = sheet)  # Read the sheet
csv_file <- file.path(output_dir, paste0(basename(file_path), "_", sheet, ".csv"))  # CSV name
write.csv(data, file = csv_file, row.names = FALSE)  # Write CSV
}
}

# Convert each Excel file
for (file_path in file_paths) {
  convert_excel_to_csv(file_path)
}
#############################
### SPAIN DATA_SESSION 1 ###
### Digit Span Task ###

# Read the data
# Define the path
path_to_DGS <- "C:\\Users/hoangmy/Desktop/thesis workspace"

Spain_DGS_1 <- read_excel("/Users/hoangmy/Desktop/thesis workspace/data/Spain data/Session 1/1_DGS_STROOP.xlsx")
Spain_DGS_2 <- read_excel("/Users/hoangmy/Desktop/thesis workspace/spain_session1_fixed_3_DGS_ASRT.xlsx")
# Load the Excel file
Spain_DGS_2 <- read_excel("/Users/hoangmy/Desktop/thesis workspace/spain_session1_fixed_3_DGS_ASRT.xlsx")

# Remove the unwanted column named `...58`
Spain_DGS_2 <- Spain_DGS_2 %>% select(-`...58`)

# Confirm the column is removed by checking the data
head(Spain_DGS_2)

Spain_DGS_3 <- read_excel("/Users/hoangmy/Desktop/thesis workspace/spain_session1_fixed_5_DGS_DGS.xlsx")
Spain_DGS_4 <- read_excel("/Users/hoangmy/Desktop/thesis workspace/data/Spain data/Session 1/10_DGS_STROOP.xlsx")
Spain_DGS_5 <- read_excel("/Users/hoangmy/Desktop/thesis workspace/data/Spain data/Session 1/12_DGS_ASRT.xlsx")
Spain_DGS_6 <- read_excel("/Users/hoangmy/Desktop/thesis workspace/data/Spain data/Session 1/14_DGS_DGS.xlsx")
Spain_DGS_7 <- read_excel("/Users/hoangmy/Desktop/thesis workspace/data/Spain data/Session 1/21_DGS_STROOP.xlsx")

# Test the read_excel function with a known path or example
example_file <- system.file("extdata", "datasets.xlsx", package = "readxl")
test_data <- readxl::read_excel(example_file)
print(head(test_data))

# Load your actual data files
Spain_DGS_1 <- readxl::read_excel("/Users/hoangmy/Desktop/thesis workspace/data/Spain data/Session 1/1_DGS_STROOP.xlsx")
Spain_DGS_2 <- readxl::read_excel("/Users/hoangmy/Desktop/thesis workspace/spain_session1_fixed_3_DGS_ASRT.xlsx")
# Load the Excel file
Spain_DGS_2 <- read_excel("/Users/hoangmy/Desktop/thesis workspace/spain_session1_fixed_3_DGS_ASRT.xlsx")

# Remove the unwanted column named `...58`
Spain_DGS_2 <- Spain_DGS_2 %>% select(-`...58`)

# Confirm the column is removed by checking the data
head(Spain_DGS_2)

Spain_DGS_3 <- readxl::read_excel("//Users/hoangmy/Desktop/thesis workspace/spain_session1_fixed_5_DGS_DGS.xlsx")
Spain_DGS_4 <- readxl::read_excel("/Users/hoangmy/Desktop/thesis workspace/data/Spain data/Session 1/10_DGS_STROOP.xlsx")
Spain_DGS_5 <- readxl::read_excel("/Users/hoangmy/Desktop/thesis workspace/data/Spain data/Session 1/12_DGS_ASRT.xlsx")
Spain_DGS_6 <- readxl::read_excel("/Users/hoangmy/Desktop/thesis workspace/data/Spain data/Session 1/14_DGS_DGS.xlsx")
Spain_DGS_7 <- readxl::read_excel("/Users/hoangmy/Desktop/thesis workspace/data/Spain data/Session 1/21_DGS_STROOP.xlsx")

# Clear column names
colnames(Spain_DGS_1) <- make.names(colnames(Spain_DGS_1))
colnames(Spain_DGS_2) <- make.names(colnames(Spain_DGS_2))
colnames(Spain_DGS_3) <- make.names(colnames(Spain_DGS_3))
colnames(Spain_DGS_4) <- make.names(colnames(Spain_DGS_4))
colnames(Spain_DGS_5) <- make.names(colnames(Spain_DGS_5))
colnames(Spain_DGS_6) <- make.names(colnames(Spain_DGS_6))
colnames(Spain_DGS_7) <- make.names(colnames(Spain_DGS_7))
# Select columns to merge
Spain_DGS_1 <- Spain_DGS_1 %>%
select(Attempt, Correct, display, listLength, Reaction.Time, Participant.Public.ID)

Spain_DGS_2 <- Spain_DGS_2 %>%
select(Attempt, Correct, display, listLength, Reaction.Time, Participant.Public.ID)

Spain_DGS_3 <- Spain_DGS_3 %>%
select(Attempt, Correct, display, listLength, Reaction.Time, Participant.Public.ID)

Spain_DGS_4 <- Spain_DGS_4 %>%
select(Attempt, Correct, display, listLength, Reaction.Time, Participant.Public.ID)

Spain_DGS_5 <- Spain_DGS_5 %>%
select(Attempt, Correct, display, listLength, Reaction.Time, Participant.Public.ID)

Spain_DGS_6 <- Spain_DGS_6 %>%
select(Attempt, Correct, display, listLength, Reaction.Time, Participant.Public.ID)

Spain_DGS_7 <- Spain_DGS_7 %>%
select(Attempt, Correct, display, listLength, Reaction.Time, Participant.Public.ID)

# Merge dataframes
Spain_merged_DGS <- bind_rows(Spain_DGS_1, Spain_DGS_2, Spain_DGS_3, Spain_DGS_4, Spain_DGS_5, Spain_DGS_6, Spain_DGS_7)

# Group by participant and extract the highest number of digits recalled (max listLength where Correct == 1)
Spain_DGS_max_digits <- Spain_merged_DGS %>%
filter(Correct == 1) %>%  # Keep only correct attempts
group_by(Participant.Public.ID) %>%  # Group by participant
summarize(max_digits_recalled = max(listLength))  # Get highest list length

# Save the result to CSV
write_csv(Spain_DGS_max_digits, "/Users/hoangmy/Desktop/thesis workspace/Spain_DGS_max_digits.csv")
##########################
### ASRT ###

# Define the path
path_to_ASRT <- "C:\\Users/hoangmy/Desktop/thesis workspace"

# Read the data
Spain_ASRT_1 <- read_csv("/Users/hoangmy/Desktop/thesis workspace/spain_fixed_4_ASRT_DGS.csv")
Spain_ASRT_2 <- read_csv("/Users/hoangmy/Desktop/thesis workspace/spain_fixed_8_ASRT_ASRT.csv")
Spain_ASRT_3 <- read_csv("/Users/hoangmy/Desktop/thesis workspace/data/Spain data/Session 1/6_ASRT_STROOP.csv")
Spain_ASRT_4 <- read_csv("/Users/hoangmy/Desktop/thesis workspace/data/Spain data/Session 1/13_ASRT_DGS.csv")
Spain_ASRT_5 <- read_csv("/Users/hoangmy/Desktop/thesis workspace/data/Spain data/Session 1/15_ASRT_STROOP.csv")
Spain_ASRT_6 <- read_csv("/Users/hoangmy/Desktop/thesis workspace/data/Spain data/Session 1/16_ASRT_STROOP.csv")
Spain_ASRT_7 <- read_csv("/Users/hoangmy/Desktop/thesis workspace/data/Spain data/Session 1/19_ASRT_ASRT.csv")
# Check for parsing issues
problems(Spain_ASRT_1)
problems(Spain_ASRT_2)
problems(Spain_ASRT_3)
problems(Spain_ASRT_4)
problems(Spain_ASRT_5)
problems(Spain_ASRT_6)
problems(Spain_ASRT_7)

# Select relevant columns
columns_to_select <- c('Participant Public ID', 'time_elapsed', 'rt', 'correct', 'triplet_type', 
                       'p_or_r', 'block', 'sequence', 'is_practice', 'first_response', 
                       'trial_number', 'correct_pos', 'correct_resp_button', 'resp_button', 
                       'cumulative_RT', 'actual_triplet')    

# Select relevant columns and assign to new objects
Spain_ASRT_1 <- Spain_ASRT_1 %>% select(all_of(columns_to_select))
Spain_ASRT_2 <- Spain_ASRT_2 %>% select(all_of(columns_to_select))
Spain_ASRT_3 <- Spain_ASRT_3 %>% select(all_of(columns_to_select))
Spain_ASRT_4 <- Spain_ASRT_4 %>% select(all_of(columns_to_select))
Spain_ASRT_5 <- Spain_ASRT_5 %>% select(all_of(columns_to_select))
Spain_ASRT_6 <- Spain_ASRT_6 %>% select(all_of(columns_to_select))
Spain_ASRT_7 <- Spain_ASRT_7 %>% select(all_of(columns_to_select))

# Merge dataframes
Spain_merged_ASRT <- bind_rows(Spain_ASRT_1, Spain_ASRT_2, Spain_ASRT_3, Spain_ASRT_4, Spain_ASRT_5, Spain_ASRT_6, Spain_ASRT_7)

# Convert string values to numeric and handle NAs
Spain_merged_ASRT <- Spain_merged_ASRT %>%
mutate(across(c(cumulative_RT, trial_number), as.numeric)) %>%
replace_na(list(cumulative_RT = 0))

# Filter out practice trials and missing reaction times (rt)
Spain_merged_ASRT <- Spain_merged_ASRT %>%
filter(is_practice == 0 & !is.na(rt))  # Removes practice trials and rows with missing RT

# Define epochs in a new column 'epoch'
Spain_merged_ASRT <- Spain_merged_ASRT %>%
mutate(epoch = case_when(
block <= 5 ~ 1,
block >= 6 & block <= 10 ~ 2,
block >= 11 & block <= 15 ~ 3,
block >= 16 & block <= 20 ~ 4,
block >= 21 & block <= 25 ~ 5,
TRUE ~ NA_real_
))

# Calculate the median RT for each participant, epoch, and triplet type (L: Low, H: High)
Spain_TL_RT <- Spain_merged_ASRT %>%
group_by(`Participant Public ID`, epoch, triplet_type) %>%
summarize(median_RT = median(cumulative_RT, na.rm = TRUE)) %>%
pivot_wider(names_from = triplet_type, values_from = median_RT) %>%
ungroup()

# Calculate the difference between low and high probability for epoch 1 and epoch 5
Spain_TL_RT <- Spain_TL_RT %>%
mutate(
epoch1_TL = if_else(epoch == 1, L - H, NA_real_),  # RT difference in epoch 1
epoch5_TL = if_else(epoch == 5, L - H, NA_real_)   # RT difference in epoch 5
)
write_csv(Spain_TL_RT, "/Users/hoangmy/Desktop/thesis workspace/Spain_TL_RT.csv")

# Group by Participant Public ID and calculate the final ASRT score (difference between epoch 5 and epoch 1)
Spain_ASRT_progress <- Spain_TL_RT %>%
group_by(`Participant Public ID`) %>%
summarize(
epoch1_TL = max(epoch1_TL, na.rm = TRUE),  # Get the value for epoch 1
epoch5_TL = max(epoch5_TL, na.rm = TRUE),  # Get the value for epoch 5
ASRT_score = epoch5_TL - epoch1_TL         # Subtract epoch 1 from epoch 5
) %>%
ungroup()

# View the result
print(Spain_ASRT_progress)

# Save the result to a CSV file
write_csv(Spain_ASRT_progress, "/Users/hoangmy/Desktop/thesis workspace/Spain_ASRT_scores.csv")

####################
### Stroop Task ###

# Read the data
path_to_STROOP <-  "C:\\Users/hoangmy/Desktop/thesis workspace"
Spain_STROOP_1 <- read_excel("/Users/hoangmy/Desktop/thesis workspace/spain_session1_fixed_2_STROOP_DGS.xlsx")
Spain_STROOP_2 <- read_excel("/Users/hoangmy/Desktop/thesis workspace/data/Spain data/Session 1/7_STROOP_STROOP.xlsx")
Spain_STROOP_3 <- read_excel("/Users/hoangmy/Desktop/thesis workspace/spain_session1_fixed_9_STROOP_ASRT.xlsx")
Spain_STROOP_4 <- read_excel("/Users/hoangmy/Desktop/thesis workspace/data/Spain data/Session 1/11_STROOP_DGS.xlsx")
Spain_STROOP_5 <- read_excel("/Users/hoangmy/Desktop/thesis workspace/data/Spain data/Session 1/17_STROOP_STROOP.xlsx")
Spain_STROOP_6 <- read_excel("/Users/hoangmy/Desktop/thesis workspace/data/Spain data/Session 1/18_STROOP_STROOP.xlsx")
Spain_STROOP_7 <- read_excel("/Users/hoangmy/Desktop/thesis workspace/data/Spain data/20_STROOP_ASRT.xlsx")

# Clear column names
colnames(Spain_STROOP_1) <- make.names(colnames(Spain_STROOP_1))
colnames(Spain_STROOP_2) <- make.names(colnames(Spain_STROOP_2))
colnames(Spain_STROOP_3) <- make.names(colnames(Spain_STROOP_3))
colnames(Spain_STROOP_4) <- make.names(colnames(Spain_STROOP_4))
colnames(Spain_STROOP_5) <- make.names(colnames(Spain_STROOP_5))
colnames(Spain_STROOP_6) <- make.names(colnames(Spain_STROOP_6))
colnames(Spain_STROOP_7) <- make.names(colnames(Spain_STROOP_7))

print(colnames(Spain_STROOP_1))

# Select relevant columns
Spain_STROOP_1 <- Spain_STROOP_1 %>%
select(Reaction.Time, Participant.Public.ID, Correct, Incorrect, Congruency, Zone.Type)

Spain_STROOP_2 <- Spain_STROOP_2 %>%
select(Reaction.Time, Participant.Public.ID, Correct, Incorrect, Congruency,Zone.Type)

Spain_STROOP_3 <- Spain_STROOP_3 %>%
select(Reaction.Time, Participant.Public.ID, Correct, Incorrect, Congruency, Zone.Type)

Spain_STROOP_4 <- Spain_STROOP_4 %>%
select(Reaction.Time, Participant.Public.ID, Correct, Incorrect, Congruency, Zone.Type)

Spain_STROOP_5 <- Spain_STROOP_5 %>%
select(Reaction.Time, Participant.Public.ID, Correct, Incorrect, Congruency, Zone.Type)

Spain_STROOP_6 <- Spain_STROOP_6 %>%
select(Reaction.Time, Participant.Public.ID, Correct, Incorrect, Congruency, Zone.Type)

Spain_STROOP_7 <- Spain_STROOP_7 %>%
select(Reaction.Time, Participant.Public.ID, Correct, Incorrect, Congruency, Zone.Type)

# Merge dataframes
Spain_merged_STROOP <- bind_rows(Spain_STROOP_1, Spain_STROOP_2, Spain_STROOP_3, Spain_STROOP_4, Spain_STROOP_5, Spain_STROOP_6, Spain_STROOP_7)

# Filter to remove rows where Zone.Type is not "response"
Spain_merged_STROOP <- Spain_merged_STROOP %>%
filter(Zone.Type == "response_keyboard")

# Filter and clean the data by removing NA and ensuring Congruency is a factor
Spain_cleaned_data_STROOP <- Spain_merged_STROOP %>%
drop_na(Reaction.Time, Congruency) %>%
mutate(Congruency = factor(Congruency, levels = c(0, 1), labels = c("incongruent", "congruent")))

# Calculate the average reaction time according to congruence for each participant
Spain_average_reaction_time_stroop <- Spain_cleaned_data_STROOP %>%
group_by(Participant.Public.ID, Congruency) %>%
summarize(mean_reaction_time = mean(Reaction.Time, na.rm = TRUE))

# See the result
print(Spain_average_reaction_time_stroop)

# Calculate the difference between the reaction times of each congruence for each participant
Spain_difference_reaction_time <- Spain_average_reaction_time_stroop %>%
group_by(Participant.Public.ID) %>%
pivot_wider(names_from = Congruency, values_from = mean_reaction_time) %>%
mutate(difference_reaction_time = congruent - incongruent)

# See the result
View(Spain_difference_reaction_time)

# Save the result to a CSV file
write_csv(Spain_difference_reaction_time, "Spain_difference_reaction_time_stroop.csv")

##################################

### Norway data_Session 1 ###
### Digit Span Task ###

# Read the data
# Define the path
path_to_DGS <- "C:\\Users/hoangmy/Desktop/thesis workspace"

Norway_DGS_1 <- read_excel("/Users/hoangmy/Desktop/thesis workspace/data/Norway data/Session 1/1_DGS_STROOP.xlsx")
Norway_DGS_2 <- read_excel("/Users/hoangmy/Desktop/thesis workspace/data/Norway data/Session 1/2_DGS_ASRT.xlsx")
Norway_DGS_3 <- read_excel("/Users/hoangmy/Desktop/thesis workspace/data/Norway data/Session 1/7_DGS_DGS.xlsx")
Norway_DGS_4 <- read_excel("/Users/hoangmy/Desktop/thesis workspace/data/Norway data/Session 1/14_DGS_STROOP.xlsx")
Norway_DGS_5 <- read_excel("/Users/hoangmy/Desktop/thesis workspace/data/Norway data/Session 1/15_DGS_DGS.xlsx")
Norway_DGS_6 <- read_excel("/Users/hoangmy/Desktop/thesis workspace/data/Norway data/Session 1/19_DGS_DGS.xlsx")
Norway_DGS_7 <- read_excel("/Users/hoangmy/Desktop/thesis workspace/data/Norway data/Session 1/26_DGS_ASRT.xlsx")

# Clear column names
colnames(Norway_DGS_1) <- make.names(colnames(Norway_DGS_1))
colnames(Norway_DGS_2) <- make.names(colnames(Norway_DGS_2))
colnames(Norway_DGS_3) <- make.names(colnames(Norway_DGS_3))
colnames(Norway_DGS_4) <- make.names(colnames(Norway_DGS_4))
colnames(Norway_DGS_5) <- make.names(colnames(Norway_DGS_5))
colnames(Norway_DGS_6) <- make.names(colnames(Norway_DGS_6))
colnames(Norway_DGS_7) <- make.names(colnames(Norway_DGS_7))
# Select columns to merge
Norway_DGS_1 <- Norway_DGS_1 %>%
select(Attempt, Correct, display, listLength, Reaction.Time, Participant.Public.ID)

Norway_DGS_2 <- Norway_DGS_2 %>%
select(Attempt, Correct, display, listLength, Reaction.Time, Participant.Public.ID)

Norway_DGS_3 <- Norway_DGS_3 %>%
select(Attempt, Correct, display, listLength, Reaction.Time, Participant.Public.ID)

Norway_DGS_4 <- Norway_DGS_4 %>%
select(Attempt, Correct, display, listLength, Reaction.Time, Participant.Public.ID)

Norway_DGS_5 <- Norway_DGS_5 %>%
select(Attempt, Correct, display, listLength, Reaction.Time, Participant.Public.ID)

Norway_DGS_6 <- Norway_DGS_6 %>%
select(Attempt, Correct, display, listLength, Reaction.Time, Participant.Public.ID)

Norway_DGS_7 <- Norway_DGS_7 %>%
select(Attempt, Correct, display, listLength, Reaction.Time, Participant.Public.ID)


# Merge dataframes
merged_Norway_DGS <- bind_rows(Norway_DGS_1, Norway_DGS_2, Norway_DGS_3, Norway_DGS_4, Norway_DGS_5, Norway_DGS_6, Norway_DGS_7)

# Group data by Participant.Private.ID
grouped_Norway_DGS <- merged_Norway_DGS %>%
group_by(Participant.Public.ID)

# Calculate the highest number of correct digits per participant
Norway_DGS_max_digits <- grouped_Norway_DGS %>%
filter(Correct == 1) %>%  # Only consider correct responses
summarize(max_digits_recalled = max(listLength))  # Get highest list length

# View the results
print(Norway_DGS_max_digits)

# Save the result to a CSV file
write_csv(Norway_DGS_max_digits, "/Users/hoangmy/Desktop/thesis workspace/Norway_DGS_max_digits.csv")

##################
### ASRT ###

# Read the ASRT files
# Try different delimiters for Norway_ASRT_1 if parsing incorrectly
Norway_ASRT_1 <- read_csv("/Users/hoangmy/Desktop/thesis workspace/data/Norway data/Session 1/convert_csv/3_ASRT_DGS.csv", show_col_types = FALSE)

# If it's parsed into a single column, use semicolon delimiter
if (ncol(Norway_ASRT_1) == 1) {
Norway_ASRT_1 <- read_delim("/Users/hoangmy/Desktop/thesis workspace/data/Norway data/Session 1/convert_csv/3_ASRT_DGS.csv", delim = ";", show_col_types = FALSE)
}

# Read other datasets (Norway_ASRT_2 and Norway_ASRT_3 are semicolon-delimited)
Norway_ASRT_2 <- read_delim("/Users/hoangmy/Desktop/thesis workspace/data/Norway data/Session 1/convert_csv/9_ASRT_STROOP.csv", delim = ";", show_col_types = FALSE)
Norway_ASRT_3 <- read_delim("/Users/hoangmy/Desktop/thesis workspace/data/Norway data/Session 1/convert_csv/10_ASRT_ASRT.csv", delim = ";", show_col_types = FALSE)

# Read other datasets that use standard CSV format (comma-separated)
Norway_ASRT_4 <- read_csv("/Users/hoangmy/Desktop/thesis workspace/data/Norway data/Session 1/11_ASRT_STROOP.csv", show_col_types = FALSE)
Norway_ASRT_5 <- read_csv("/Users/hoangmy/Desktop/thesis workspace/data/Norway data/Session 1/18_ASRT_DGS.csv", show_col_types = FALSE)
Norway_ASRT_6 <- read_csv("/Users/hoangmy/Desktop/thesis workspace/data/Norway data/Session 1/22_ASRT_DGS.csv", show_col_types = FALSE)
Norway_ASRT_7 <- read_csv("/Users/hoangmy/Desktop/thesis workspace/data/Norway data/Session 1/23_ASRT_ASRT.csv", show_col_types = FALSE)

# Clean the 'rt' column by removing non-numeric values
# Remove rows with non-numeric 'rt' values
Norway_ASRT_1 <- Norway_ASRT_1 %>% filter(grepl("^\\d*\\.?\\d+$", rt))  # Keep only numeric values
Norway_ASRT_2 <- Norway_ASRT_2 %>% filter(grepl("^\\d*\\.?\\d+$", rt))
Norway_ASRT_3 <- Norway_ASRT_3 %>% filter(grepl("^\\d*\\.?\\d+$", rt))
Norway_ASRT_4 <- Norway_ASRT_4 %>% filter(grepl("^\\d*\\.?\\d+$", rt))
Norway_ASRT_5 <- Norway_ASRT_5 %>% filter(grepl("^\\d*\\.?\\d+$", rt))
Norway_ASRT_6 <- Norway_ASRT_6 %>% filter(grepl("^\\d*\\.?\\d+$", rt))
Norway_ASRT_7 <- Norway_ASRT_7 %>% filter(grepl("^\\d*\\.?\\d+$", rt))

# Convert 'rt' to numeric after cleaning
Norway_ASRT_1 <- Norway_ASRT_1 %>% mutate(rt = as.numeric(rt)) %>% filter(!is.na(rt))
Norway_ASRT_2 <- Norway_ASRT_2 %>% mutate(rt = as.numeric(rt)) %>% filter(!is.na(rt))
Norway_ASRT_3 <- Norway_ASRT_3 %>% mutate(rt = as.numeric(rt)) %>% filter(!is.na(rt))
Norway_ASRT_4 <- Norway_ASRT_4 %>% mutate(rt = as.numeric(rt)) %>% filter(!is.na(rt))
Norway_ASRT_5 <- Norway_ASRT_5 %>% mutate(rt = as.numeric(rt)) %>% filter(!is.na(rt))
Norway_ASRT_6 <- Norway_ASRT_6 %>% mutate(rt = as.numeric(rt)) %>% filter(!is.na(rt))
Norway_ASRT_7 <- Norway_ASRT_7 %>% mutate(rt = as.numeric(rt)) %>% filter(!is.na(rt))

# Select relevant columns for consistency
Norway_columns_to_select <- c('Participant Public ID', 'time_elapsed', 'rt', 'correct', 'triplet_type',
                              'p_or_r', 'block', 'sequence', 'is_practice', 'first_response',
                              'trial_number', 'correct_pos', 'correct_resp_button', 'resp_button',
                              'cumulative_RT', 'actual_triplet')

# Apply the selection to each dataset
Norway_ASRT_1 <- Norway_ASRT_1 %>% select(all_of(Norway_columns_to_select))
Norway_ASRT_2 <- Norway_ASRT_2 %>% select(all_of(Norway_columns_to_select))
Norway_ASRT_3 <- Norway_ASRT_3 %>% select(all_of(Norway_columns_to_select))
Norway_ASRT_4 <- Norway_ASRT_4 %>% select(all_of(Norway_columns_to_select))
Norway_ASRT_5 <- Norway_ASRT_5 %>% select(all_of(Norway_columns_to_select))
Norway_ASRT_6 <- Norway_ASRT_6 %>% select(all_of(Norway_columns_to_select))
Norway_ASRT_7 <- Norway_ASRT_7 %>% select(all_of(Norway_columns_to_select))

# Merge the dataframes
merged_Norway_ASRT <- bind_rows(Norway_ASRT_1, Norway_ASRT_2, Norway_ASRT_3, Norway_ASRT_4, Norway_ASRT_5, Norway_ASRT_6, Norway_ASRT_7)

# Convert string values to numeric and handle NAs
merged_Norway_ASRT <- merged_Norway_ASRT %>%
mutate(across(c(cumulative_RT, trial_number), as.numeric)) %>%    
replace_na(list(cumulative_RT = 0))                               

# Filter out practice trials and missing reaction times (rt)
merged_Norway_ASRT <- merged_Norway_ASRT %>% 
filter(is_practice == 0 & !is.na(rt))  # Removes practice trials and rows with missing RT

# Define epochs in a new column 'epoch'
merged_Norway_ASRT <- merged_Norway_ASRT %>% mutate(
epoch = case_when(
block <= 5 ~ 1,
block >= 6 & block <= 10 ~ 2,
block >= 11 & block <= 15 ~ 3,
block >= 16 & block <= 20 ~ 4,
block >= 21 & block <= 25 ~ 5,
TRUE ~ NA_real_
)
)

# Calculate the median RT for each participant, epoch, and triplet type (L: Low, H: High)
Norway_TL_RT <- merged_Norway_ASRT %>%
group_by(`Participant Public ID`, epoch, triplet_type) %>%
summarize(median_RT = median(rt, na.rm = TRUE)) %>%
pivot_wider(names_from = triplet_type, values_from = median_RT) %>%
ungroup()

# Calculate the difference between low and high probability RT for each epoch
Norway_TL_RT <- Norway_TL_RT %>% mutate(
epoch1_TL = if_else(epoch == 1, L - H, NA_real_),  # RT difference in epoch 1
epoch5_TL = if_else(epoch == 5, L - H, NA_real_)   # RT difference in epoch 5
)

# Calculate the final ASRT score (RT difference between epoch 5 and epoch 1)
Norway_ASRT_progress <- Norway_TL_RT %>%
group_by(`Participant Public ID`) %>%
summarize(
epoch1_TL = max(epoch1_TL, na.rm = TRUE),  # Get the value for epoch 1
epoch5_TL = max(epoch5_TL, na.rm = TRUE),  # Get the value for epoch 5
ASRT_score = epoch5_TL - epoch1_TL         # Subtract epoch 1 from epoch 5
) %>%
ungroup()

# View the final ASRT scores for Norway data
print(Norway_ASRT_progress)

# Save the result to a CSV file
write_csv(Norway_ASRT_progress, "/Users/hoangmy/Desktop/thesis workspace/Norway_ASRT_scores.csv")

#####################
### Stroop Task ###

# Read the data
path_to_STROOP <-  "C:\\Users/hoangmy/Desktop/thesis workspace"
Norway_STROOP_1 <- read_excel("/Users/hoangmy/Desktop/thesis workspace/data/Norway data/Session 1/4_STROOP_DGS.xlsx")
Norway_STROOP_2 <- read_excel("/Users/hoangmy/Desktop/thesis workspace/data/Norway data/Session 1/5_STROOP_STROOP.xlsx")
Norway_STROOP_3 <- read_excel("/Users/hoangmy/Desktop/thesis workspace/data/Norway data/Session 1/8_STROOP_ASRT.xlsx")
Norway_STROOP_4 <- read_excel("/Users/hoangmy/Desktop/thesis workspace/data/Norway data/Session 1/13_STROOP_STROOP.xlsx")
Norway_STROOP_5 <- read_excel("/Users/hoangmy/Desktop/thesis workspace/data/Norway data/Session 1/17_STROOP_DGS.xlsx")
Norway_STROOP_6 <- read_excel("/Users/hoangmy/Desktop/thesis workspace/data/Norway data/Session 1/21_STROOP_DGS.xlsx")
Norway_STROOP_7 <- read_excel("/Users/hoangmy/Desktop/thesis workspace/data/Norway data/Session 1/24_STROOP_ASRT.xlsx")

# Clear column names
colnames(Norway_STROOP_1) <- make.names(colnames(Norway_STROOP_1))
colnames(Norway_STROOP_2) <- make.names(colnames(Norway_STROOP_2))
colnames(Norway_STROOP_3) <- make.names(colnames(Norway_STROOP_3))
colnames(Norway_STROOP_4) <- make.names(colnames(Norway_STROOP_4))
colnames(Norway_STROOP_5) <- make.names(colnames(Norway_STROOP_5))
colnames(Norway_STROOP_6) <- make.names(colnames(Norway_STROOP_6))
colnames(Norway_STROOP_7) <- make.names(colnames(Norway_STROOP_7))

print(colnames(Norway_STROOP_1))
# Select relevant columns
Norway_STROOP_1 <- Norway_STROOP_1 %>%
select(Reaction.Time, Participant.Public.ID, Correct, Incorrect, Congruency, Zone.Type)

Norway_STROOP_2 <- Norway_STROOP_2 %>%
select(Reaction.Time, Participant.Public.ID, Correct, Incorrect, Congruency,Zone.Type)

Norway_STROOP_3 <- Norway_STROOP_3 %>%
select(Reaction.Time, Participant.Public.ID, Correct, Incorrect, Congruency, Zone.Type)

Norway_STROOP_4 <- Norway_STROOP_4 %>%
select(Reaction.Time, Participant.Public.ID, Correct, Incorrect, Congruency, Zone.Type)

Norway_STROOP_5 <- Norway_STROOP_5 %>%
select(Reaction.Time, Participant.Public.ID, Correct, Incorrect, Congruency, Zone.Type)

Norway_STROOP_6 <- Norway_STROOP_6 %>%
select(Reaction.Time, Participant.Public.ID, Correct, Incorrect, Congruency, Zone.Type)

Norway_STROOP_7 <- Norway_STROOP_7 %>%
select(Reaction.Time, Participant.Public.ID, Correct, Incorrect, Congruency, Zone.Type)

# Merge dataframes
merged_Norway_STROOP <- bind_rows(Norway_STROOP_1, Norway_STROOP_2, Norway_STROOP_3, Norway_STROOP_4, Norway_STROOP_5, Norway_STROOP_6, Norway_STROOP_7)

# Filter to remove rows where Zone.Type is not "response"
merged_Norway_STROOP <- merged_Norway_STROOP %>%
filter(Zone.Type == "response_keyboard")

# Filter and clean the data by removing NA and ensuring Congruency is a factor
cleaned_Norway_data_STROOP <- merged_Norway_STROOP %>%
drop_na(Reaction.Time, Congruency) %>%
mutate(Congruency = factor(Congruency, levels = c(0, 1), labels = c("incongruent", "congruent")))

# Calculate the average reaction time according to congruence for each participant
mean_reaction_time_Norway_stroop <- cleaned_Norway_data_STROOP %>%
group_by(Participant.Public.ID, Congruency) %>%
summarize(mean_reaction_time = mean(Reaction.Time, na.rm = TRUE))

# See the result
print(mean_reaction_time_Norway_stroop)

# Calculate the difference between the reaction times of each congruence for each participant
Norway_difference_reaction_time <- mean_reaction_time_Norway_stroop %>%
group_by(Participant.Public.ID) %>%
pivot_wider(names_from = Congruency, values_from = mean_reaction_time) %>%
mutate(difference_reaction_time = congruent - incongruent)

# See the result
View(Norway_difference_reaction_time)

# Save the result to a CSV file
write_csv(Norway_difference_reaction_time, "Norway_difference_reaction_time_stroop.csv")

#######################################
### Merge the Norway and Spain DGS scores ###
combined_DGS_scores <- bind_rows(Norway_DGS_max_digits, Spain_DGS_max_digits)

# View the combined data
print(combined_DGS_scores)

# Save the combined data to a CSV file
write_csv(combined_DGS_scores, "/Users/hoangmy/Desktop/thesis workspace/data/combined_DGS_scores.csv")

##################################
### Merge the Norway and Spain ASRT scores ###
combined_ASRT_scores <- bind_rows(Norway_ASRT_progress, Spain_ASRT_progress)

# View the merged data
head(combined_ASRT_scores)

# Save the merged dataset to a CSV file
write_csv(combined_ASRT_scores, "/Users/hoangmy/Desktop/thesis workspace/data/combined_ASRT_scores.csv")

##################################
### Merge the Norway and Spain STROOP scores ###
combined_STROOP_scores <- bind_rows(Norway_difference_reaction_time, Spain_difference_reaction_time)

# Print the result to check
print(combined_STROOP_scores)

# Save the merged dataset to a CSV file
write_csv(combined_STROOP_scores, "/Users/hoangmy/Desktop/thesis workspace/data/combined_STROOP_scores.csv")

###########################
###EEG data###

# Set the directory path where all your EEG .txt files are located
path_to_files <- "/Users/hoangmy/Desktop/thesis workspace/data/EEG data/"

# Get a list of all .txt files in that directory
EEG_text_files <- list.files(path = path_to_files, pattern = "\\.txt$", full.names = TRUE)

# Define the required column names
required_columns <- c("Time", "Voltage", "Condition")  # Adjust based on your actual columns

# Standardize columns across all files
standardized_files <- lapply(EEG_text_files, function(file) {
  data <- read.table(file, header = TRUE, sep = "")
  # Add missing columns with NA
  missing_columns <- setdiff(required_columns, colnames(data))
  for (col in missing_columns) {
    data[[col]] <- NA
  }
  # Select and order columns based on the required structure
  data <- data[, required_columns, drop = FALSE]
  return(data)
})

EEG_text <- do.call(rbind, standardized_files)

write.csv(EEG_text, "/Users/hoangmy/Desktop/thesis workspace/data/EEG_data.csv", row.names = FALSE)

# Read all .txt files and combine them into a single data frame
#EEG_text <- do.call(rbind, lapply(EEG_text_files, function(file) read.table(file, header = TRUE, sep = "")))

# Save the merged EEG data as a CSV file
#write.csv(EEG_text, "/Users/hoangmy/Desktop/thesis workspace/data/EEG_text.csv", row.names = FALSE)

# Check the merged data
#print(head(EEG_text))
#########################
### Calculate the mean amplitude ###

# Read the merged EEG data 
merged_EEG_dataset <- read_csv("/Users/hoangmy/Desktop/thesis workspace/data/EEG_text.csv")

# Calculate the mean amplitude for each participant by condition, grouping by 'File' instead of 'Participant_ID'
mean_amplitude_data <- merged_EEG_dataset %>%
group_by(File) %>% # Group by 'File' since it marks participants
summarize(
mean_correct_amplitude = mean(c_across(ends_with("Correct")), na.rm = TRUE),
mean_violation_amplitude = mean(c_across(ends_with("Violation")), na.rm = TRUE))

# Standardize participant names using a mapping table
mapping_table <- data.frame(
home_IDs = c("crar9", "exdq7", "ezje7", "finc3", "fqrf6", "gxac7", "hcqa8", "hnzm9", "jpxp4", "pson3", "qdvg4", "qgsl9", "rqed8", "rvax5", "uyxx3", "xqls8", "bozt1", "cglm9", "cprf4", "fqiu0", "hvbn2", "jbug1", "pjuk8", "prmg0", "rtdd4", "sgua7", "tgfb6", "tpxp2", "vhyg4", "vlsi3", "xgnk5", "zuix2", "zurx0"),  
lab_IDs = c("10", "9", "13", "14", "12", "15", "8", "1", "6", "5", "7", "4", "2", "16", "11", "3", "12_s02_task_cell", "14_s02_task_cell", "11_s02_task_cell", "06_s02_exp_cell", "15_s02_task_cell", "17_s02_task_cell", "1_Spain", "09_s02_task_cell", "10_s02_task_cell", "5_Spain", "3_Spain", "18_s02_task_cell", "13_s02_task_cell", "4_Spain", "08_s02_task_cell", "07_s02_task_cell", "2_Spain"))

# Ensure all columns are character type
mapping_table <- mapping_table %>% mutate(across(everything(), as.character))

# Save the mapping table as a CSV file
write_csv(mapping_table, "/Users/hoangmy/Desktop/thesis workspace/data/mapping_table.csv")

# Load the mapping table
mapping_table <- read_csv("/Users/hoangmy/Desktop/thesis workspace/data/mapping_table.csv")

# Identify missing participants (those in mapping_table but not in mean_amplitude_data)
missing_participants <- mapping_table %>%
filter(!lab_IDs %in% mean_amplitude_data$File)

# Print missing participants
print(missing_participants)

# Remove missing participants from the mapping table
mapping_table_filtered <- mapping_table %>%
filter(!home_IDs %in% c("rqed8", "qgsl9"))

# Save the filtered mapping table as a CSV file for future use
write_csv(mapping_table_filtered, "/Users/hoangmy/Desktop/thesis workspace/data/filtered_mapping_table.csv")

# Merge the EEG data with the filtered mapping table to link behavioral IDs
final_EEG_data <- mean_amplitude_data %>%
left_join(mapping_table_filtered, by = c("File" = "lab_IDs")) %>%
rename(lab_IDs = File)  # Rename 'File' to 'lab_IDs'

# Save the final workable EEG data to a CSV file
write_csv(final_EEG_data, "/Users/hoangmy/Desktop/thesis workspace/data/final_workable_EEG_data.csv")

########################
### Merge all the data ###

# Load the necessary files
DGS_data <- read_csv("/Users/hoangmy/Desktop/thesis workspace/data/combined_DGS_scores.csv")
ASRT_data <- read_csv("/Users/hoangmy/Desktop/thesis workspace/data/combined_ASRT_scores.csv")
Stroop_data <- read_csv("/Users/hoangmy/Desktop/thesis workspace/data/combined_STROOP_scores.csv")
mean_accuracy_data <- read_csv("/Users/hoangmy/Desktop/thesis workspace/data/mean_accuracy_by_participant.csv")
EEG_data <- read_csv("/Users/hoangmy/Desktop/thesis workspace/data/final_workable_EEG_data.csv")
mapping_table <- read_csv("/Users/hoangmy/Desktop/thesis workspace/data/mapping_table.csv")

# Rename Participant.Public.ID to home_IDs in DGS, ASRT, and Stroop data
DGS_data <- DGS_data %>% rename(home_IDs = Participant.Public.ID)
ASRT_data <- ASRT_data %>% rename(home_IDs = `Participant Public ID`)
Stroop_data <- Stroop_data %>% rename(home_IDs = Participant.Public.ID)

# Merge DGS, ASRT, Stroop, and EEG data based on 'home_IDs'
merged_EF_EEG_data <- DGS_data %>%
full_join(ASRT_data, by = "home_IDs") %>%
full_join(Stroop_data, by = "home_IDs") %>%
full_join(EEG_data, by = "home_IDs")

# Save the merged data to a CSV file
write_csv(merged_EF_EEG_data, "/Users/hoangmy/Desktop/thesis workspace/data/merged_EF_EEG_data.csv")

# Load the files
merged_EF_EEG_data <- read_csv("/Users/hoangmy/Desktop/thesis workspace/data/merged_EF_EEG_data.csv")

# Create a data frame that represents the 'subject', 'country', and 'home_IDs' mapping
lookup_table <- data.frame(
subject = c(1:16, 1:15, 17, 18),
country = c(rep("Norway", 16), rep("Spain", 17)),
home_IDs = c("hnzm9", "rqed8", "xqls8", "qgsl9", "pson3", "jpxp4", "qdvg4", "hcqa8", 
               "exdq7", "crar9", "uyxx3", "fqrf6", "ezje7", "finc3", "gxac7", "rvax5",
               "pjuk8", "zurx0", "tgfb6", "vlsi3", "sgua7", "fqiu0", "zuix2", "xgnk5", 
               "prmg0", "rtdd4", "cprf4", "bozt1", "vhyg4", "cglm9", "hvbn2", "jbug1", "tpxp2")
)

# Merge with mean_accuracy_wide
mean_accuracy_wide <- mean_accuracy_data %>%
pivot_wider(
names_from = grammaticality,  # Pivot on the grammaticality condition
values_from = c(correct_responses, total_responses, accuracy_percentage),  # Pivot the three columns for each condition
names_sep = "_"
) %>%
left_join(lookup_table, by = c("subject", "country"))

# Merge the updated mean_accuracy_wide with the rest of data
final_merged_data <- merged_EF_EEG_data %>%
full_join(mean_accuracy_wide, by = "home_IDs")

# Filter out participant 'qgsl9' before saving the final merged data
final_merged_data <- final_merged_data %>%
filter(home_IDs != "qgsl9")

# Read LHQ3 files for Norway and Spain
LHQ3_Norway<- read_delim("/Users/hoangmy/Desktop/thesis workspace/Norway_LHQ3.csv", delim = ";", skip = 1, locale = locale(decimal_mark = ","))
LHQ3_Spain <- read_delim("/Users/hoangmy/Desktop/thesis workspace/Spain_LHQ3.csv", delim = ";", skip = 1, locale = locale(decimal_mark = ","))

# Select only the necessary columns from both datasets
Norway_LHQ3_selected <- LHQ3_Norway %>% select(`Participant ID`, `Multilingual Language Diversity\nScore`)
Spain_LHQ3_selected <- LHQ3_Spain %>% select(`Participant ID`, `Multilingual Language Diversity\nScore`)

# Optionally combine the two LHQ3 datasets (if you want to merge them into one)
combined_LHQ3 <- bind_rows(Norway_LHQ3_selected, Spain_LHQ3_selected)

# Merge with final_merged_data using 'home_IDs' from final_merged_data and 'Participant ID' from combined_lhq3
merged_data <- merge(final_merged_data, combined_LHQ3, by.x = "home_IDs", by.y = "Participant ID", all.x = TRUE)

# Save the merged data 
write.csv(merged_data, "/Users/hoangmy/Desktop/thesis workspace/merged_data.csv", row.names = FALSE)


##########################
### RUN PLOT ### 

# Load dataset
data <- read_csv("/Users/hoangmy/Desktop/thesis workspace/data/final_merged_data.csv")

# Reshape the dataset to long format for analysis, including home_IDs
long_data <- merged_data %>%
  select(home_IDs, subject, accuracy_percentage_grammatical, `accuracy_percentage_gender violation`) %>%
  pivot_longer(
    cols = c(accuracy_percentage_grammatical, `accuracy_percentage_gender violation`),
    names_to = "grammaticality",
    values_to = "accuracy"
  ) %>%
  mutate(grammaticality = ifelse(grammaticality == "accuracy_percentage_grammatical", 
                                 "grammatical", "gender violation"))

# Create a boxplot to compare grammatical and gender violation conditions
ggplot(long_data, aes(x = grammaticality, y = accuracy, fill = grammaticality)) +
  geom_boxplot() +
  labs(y = "Accuracy Percentage (%)", x = "Condition", fill = "Condition") +
  theme_minimal()

# Run a mixed-effects model with home_IDs as the random effect
mixed_model <- lmer(accuracy ~ grammaticality + (1 | home_IDs), data = long_data)
summary(mixed_model)

# Convert long_data to wide format
wide_data <- long_data %>%
  pivot_wider(names_from = grammaticality, values_from = accuracy)

# Run a paired t-test
t_test <- t.test(wide_data$grammatical, wide_data$`gender violation`, paired = TRUE)
print("Paired t-test comparing Grammatical and Gender Violation Accuracy")
print(t_test)

# Reshape the dataset to long format for analysis of both conditions
accuracy_data <- merged_data %>%
  select(max_digits_recalled, ASRT_score, difference_reaction_time, 
         accuracy_percentage_grammatical, `accuracy_percentage_gender violation`) %>%
  pivot_longer(cols = c(accuracy_percentage_grammatical, `accuracy_percentage_gender violation`), 
               names_to = "Condition", values_to = "Accuracy") %>%
  mutate(Condition = ifelse(Condition == "accuracy_percentage_grammatical", 
                            "Grammatical", "Gender Violation"))

# Set the same Y-axis limits for all plots
y_limits <- c(0, 100)

# Scatter plot for DGS (Working Memory) vs Accuracy for both conditions
plot_dgs_accuracy <- ggplot(accuracy_data, aes(x = max_digits_recalled, y = Accuracy, color = Condition)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) + 
  labs(title = "Correlation between Working Memory and Accuracy",
       x = "DGS (Working Memory)", y = NULL) +
  theme_minimal()

# Scatter plot for ASRT (Implicit Learning) vs Accuracy for both conditions
plot_asrt_accuracy <- ggplot(accuracy_data, aes(x = ASRT_score, y = Accuracy, color = Condition)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +  
  labs(title = "Correlation between Implicit Learning and Accuracy",
       x = "ASRT (Implicit Learning)", y = NULL) +
  theme_minimal()

# Scatter plot for Stroop (Inhibitory Control) vs Accuracy for both conditions
plot_stroop_accuracy <- ggplot(accuracy_data, aes(x = difference_reaction_time, y = Accuracy, color = Condition)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +  
  labs(title = "Correlation between Inhibitory Control and Accuracy",
       x = "Stroop (Inhibitory Control)", y = NULL) +
  theme_minimal()

# Create a shared Y-axis label
y_axis_label <- textGrob("Accuracy (%)", rot = 90, vjust = 1, gp = gpar(fontsize = 12))

# Arrange the plots with the shared Y-axis
grid.arrange(
  arrangeGrob(plot_dgs_accuracy, plot_asrt_accuracy, plot_stroop_accuracy, ncol = 1),
  left = y_axis_label  # Place the shared Y-axis label on the left
)

# Subset data for Grammatical condition
accuracy_grammatical <- accuracy_data %>% filter(Condition == "Grammatical")

# Subset data for Gender Violation condition
accuracy_gender_violation <- accuracy_data %>% filter(Condition == "Gender Violation")

# Spearman correlation for DGS (Working Memory) vs Accuracy for Grammatical condition
spearman_dgs_grammatical <- cor.test(accuracy_grammatical$max_digits_recalled, accuracy_grammatical$Accuracy, method = "spearman")
print(spearman_dgs_grammatical)

# Spearman correlation for DGS (Working Memory) vs Accuracy for Gender Violation condition
spearman_dgs_gender <- cor.test(accuracy_gender_violation$max_digits_recalled, accuracy_gender_violation$Accuracy, method = "spearman")
print(spearman_dgs_gender)

# Spearman correlation for ASRT (Implicit Learning) vs Accuracy for Grammatical condition
spearman_asrt_grammatical <- cor.test(accuracy_grammatical$ASRT_score, accuracy_grammatical$Accuracy, method = "spearman")
print(spearman_asrt_grammatical)

# Spearman correlation for ASRT (Implicit Learning) vs Accuracy for Gender Violation condition
spearman_asrt_gender <- cor.test(accuracy_gender_violation$ASRT_score, accuracy_gender_violation$Accuracy, method = "spearman")
print(spearman_asrt_gender)

# Spearman correlation for Stroop (Inhibitory Control) vs Accuracy for Grammatical condition
spearman_stroop_grammatical <- cor.test(accuracy_grammatical$difference_reaction_time, accuracy_grammatical$Accuracy, method = "spearman")
print(spearman_stroop_grammatical)

# Spearman correlation for Stroop (Inhibitory Control) vs Accuracy for Gender Violation condition
spearman_stroop_gender <- cor.test(accuracy_gender_violation$difference_reaction_time, accuracy_gender_violation$Accuracy, method = "spearman")
print(spearman_stroop_gender)

# Calculate the difference in amplitude between the two conditions
merged_data <- merged_data %>%
  mutate(amplitude_difference = mean_violation_amplitude - mean_correct_amplitude)

##########################
### Import EEG data for each time window ### 
### Import EEG data and reshape it to long format with Condition and Brain Region ###

# Import EEG data for 400-900 ms
EEGdata_400_900 <- read.csv("/Users/hoangmy/Desktop/thesis workspace/data/EEG data/Fz_400_900.txt", header = TRUE, sep = "")

# Reshape data to long format and add Condition labels
EEGdata_400_900_long <- EEGdata_400_900 %>%
  pivot_longer(
    cols = -File, # Assuming 'File' represents participant IDs 
    names_to = c("Electrode", "Condition"),  
    names_sep = "\\.",  
    values_to = "Voltage"
  ) %>%
  mutate(
    Condition = ifelse(Condition == "AverageCorrect_2", "Grammatical", "Gender Violation")  # Properly label Condition
  )

# Define the function to assign brain regions based on electrode names
assign_brain_region <- function(electrode) {
  case_when(
    electrode %in% c("F3", "F7", "FC1", "FC5") ~ "Left Anterior",
    electrode %in% c("F4", "F8", "FC2", "FC6") ~ "Right Anterior",
    electrode %in% c("CP1", "CP5") ~ "Left Medial",
    electrode %in% c("CP2", "CP6") ~ "Right Medial",
    electrode %in% c("P3", "P7") ~ "Left Posterior",
    electrode %in% c("P4", "P8") ~ "Right Posterior",
    electrode == "Fz" ~ "Middle Anterior",
    electrode == "FCz" ~ "Middle Medial",
    electrode == "Pz" ~ "Middle Posterior",
    TRUE ~ NA_character_
  )
}

# Apply function to add Brain Region column
EEGdata_400_900_long <- EEGdata_400_900_long %>%
  mutate(Brain_Region = assign_brain_region(Electrode))

# Check structure to ensure columns are correctly assigned
str(EEGdata_400_900_long)  # Verify `Condition`, `Voltage`, and `Brain_Region` are present

### Merge EEG data with mapping_table to add `home_IDs` ###

# Merge EEG data with mapping_table to get `home_IDs`
EEG_with_home_IDs_400_900 <- EEGdata_400_900_long %>%
  left_join(mapping_table, by = c("File" = "lab_IDs"))

# Verify merge success
str(EEG_with_home_IDs_400_900)  # `home_IDs` should now be present

### Prepare EF_data and Merge with EEG Data ###

# Example: Select and rename columns for EF data, ensuring `home_IDs` is present
EF_data <- data %>% 
  select(home_IDs, max_digits_recalled, ASRT_score, difference_reaction_time)

# Merge EEG data (with home_IDs) with EF data
combined_data_400_900 <- EF_data %>%
  left_join(EEG_with_home_IDs_400_900, by = "home_IDs")

# Ensure `Condition`, `Voltage`, `Brain_Region`, and EF scores are in `combined_data`
str(combined_data_400_900)  # Verify all necessary columns are included

### Filter Combined Data and Plot ###

# Filter out rows with NA values in important columns
combined_data_400_900_filtered <- combined_data_400_900 %>%
  filter(!is.na(Brain_Region) & !is.na(Voltage) & !is.na(Condition))

# Plot DGS (Working Memory)
plot_dgs_400_900 <- ggplot(combined_data_400_900_filtered, aes(x = max_digits_recalled, y = Voltage, color = Condition)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ Condition) +
  labs(x = "DGS (Working Memory)", y = NULL, color = NULL) +  # Remove Y-axis title
  theme_minimal() +
  theme(
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
    legend.position = "none"
  )

# Plot ASRT (Implicit Learning)
plot_asrt_400_900 <- ggplot(combined_data_400_900_filtered, aes(x = ASRT_score, y = Voltage, color = Condition)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ Condition) +
  labs(x = "ASRT (Implicit Learning)", y = NULL, color = NULL) +  # Remove Y-axis title
  theme_minimal() +
  theme(
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
    legend.position = "none"
  )

# Plot Stroop (Inhibitory Control)
plot_stroop_400_900 <- ggplot(combined_data_400_900_filtered, aes(x = difference_reaction_time, y = Voltage, color = Condition)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ Condition) +
  labs(x = "Stroop (Inhibitory Control)", y = NULL, color = NULL) +  # Remove Y-axis title
  theme_minimal() +
  theme(
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
    legend.position = "none"
  )

# Combine Plots with Shared Y-axis Title
grid.arrange(
  plot_dgs_400_900,
  plot_asrt_400_900,
  plot_stroop_400_900,
  ncol = 1,
  left = textGrob("EEG Amplitude (µV)", rot = 90, vjust = 1, gp = gpar(fontsize = 12))  # Shared Y-axis title
)

### Import EEG Data for 300–600 ms and Analyze ###

##########################
### Import EEG Data for 300-600 ms Time Window ###

### Import EEG data and reshape it to long format with Condition and Brain Region ###

# Import EEG data for 300-600 ms
EEGdata_300_600 <- read.csv("/Users/hoangmy/Desktop/thesis workspace/data/EEG data/Fz_300_600.txt", header = TRUE, sep = "")

# Reshape data to long format and add Condition labels
EEGdata_300_600_long <- EEGdata_300_600 %>%
  pivot_longer(
    cols = -File, # Assuming 'File' represents participant IDs
    names_to = c("Electrode", "Condition"),
    names_sep = "\\.",
    values_to = "Voltage"
  ) %>%
  mutate(
    Condition = ifelse(Condition == "AverageCorrect_2", "Grammatical", "Gender Violation")  # Properly label Condition
  )

# Define the function to assign brain regions based on electrode names
assign_brain_region <- function(electrode) {
  case_when(
    electrode %in% c("F3", "F7", "FC1", "FC5") ~ "Left Anterior",
    electrode %in% c("F4", "F8", "FC2", "FC6") ~ "Right Anterior",
    electrode %in% c("CP1", "CP5") ~ "Left Medial",
    electrode %in% c("CP2", "CP6") ~ "Right Medial",
    electrode %in% c("P3", "P7") ~ "Left Posterior",
    electrode %in% c("P4", "P8") ~ "Right Posterior",
    electrode == "Fz" ~ "Middle Anterior",
    electrode == "FCz" ~ "Middle Medial",
    electrode == "Pz" ~ "Middle Posterior",
    TRUE ~ NA_character_
  )
}

# Apply function to add Brain Region column
EEGdata_300_600_long <- EEGdata_300_600_long %>%
  mutate(Brain_Region = assign_brain_region(Electrode))

# Check structure to ensure columns are correctly assigned
str(EEGdata_300_600_long)  # Verify `Condition`, `Voltage`, and `Brain_Region` are present

### Merge EEG data with mapping_table to add `home_IDs` ###

# Merge EEG data with mapping_table to get `home_IDs`
EEG_with_home_IDs_300_600 <- EEGdata_300_600_long %>%
  left_join(mapping_table, by = c("File" = "lab_IDs"))

# Verify merge success
str(EEG_with_home_IDs_300_600)  # `home_IDs` should now be present

### Prepare EF_data and Merge with EEG Data ###

# Example: Select and rename columns for EF data, ensuring `home_IDs` is present
EF_data <- data %>%
  select(home_IDs, max_digits_recalled, ASRT_score, difference_reaction_time)

# Merge EEG data (with home_IDs) with EF data
combined_data_300_600 <- EF_data %>%
  left_join(EEG_with_home_IDs_300_600, by = "home_IDs")

# Ensure `Condition`, `Voltage`, `Brain_Region`, and EF scores are in `combined_data_300_600`
str(combined_data_300_600)  # Verify all necessary columns are included

### Filter Combined Data and Plot ###

# Filter out rows with NA values in important columns
combined_data_300_600_filtered <- combined_data_300_600 %>%
  filter(!is.na(Brain_Region) & !is.na(Voltage) & !is.na(Condition))

# Plot DGS (Working Memory)
plot_dgs_300_600 <- ggplot(combined_data_300_600_filtered, aes(x = max_digits_recalled, y = Voltage, color = Condition)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ Condition) +
  labs(x = "DGS (Working Memory)", y = NULL, color = NULL) +  # Remove Y-axis title
  theme_minimal() +
  theme(
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
    legend.position = "none"
  )

# Plot ASRT (Implicit Learning)
plot_asrt_300_600 <- ggplot(combined_data_300_600_filtered, aes(x = ASRT_score, y = Voltage, color = Condition)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ Condition) +
  labs(x = "ASRT (Implicit Learning)", y = NULL, color = NULL) +  # Remove Y-axis title
  theme_minimal() +
  theme(
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
    legend.position = "none"
  )

# Plot Stroop (Inhibitory Control)
plot_stroop_300_600 <- ggplot(combined_data_300_600_filtered, aes(x = difference_reaction_time, y = Voltage, color = Condition)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ Condition) +
  labs(x = "Stroop (Inhibitory Control)", y = NULL, color = NULL) +  # Remove Y-axis title
  theme_minimal() +
  theme(
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
    legend.position = "none"
  )

# Combine Plots with Shared Y-axis Title
grid.arrange(
  plot_dgs_300_600,
  plot_asrt_300_600,
  plot_stroop_300_600,
  ncol = 1,
  left = textGrob("EEG Amplitude (µV)", rot = 90, vjust = 1, gp = gpar(fontsize = 12))  # Shared Y-axis title
)

##########################
### Import EEG Data for 200-500 ms Time Window ###

### Import EEG data and reshape it to long format with Condition and Brain Region ###

# Import EEG data for 200-500 ms
EEGdata_200_500 <- read.csv("/Users/hoangmy/Desktop/thesis workspace/data/EEG data/Fz_200_500.txt", header = TRUE, sep = "")

# Reshape data to long format and add Condition labels
EEGdata_200_500_long <- EEGdata_200_500 %>%
  pivot_longer(
    cols = -File, # Assuming 'File' represents participant IDs
    names_to = c("Electrode", "Condition"),
    names_sep = "\\.",
    values_to = "Voltage"
  ) %>%
  mutate(
    Condition = ifelse(Condition == "AverageCorrect_2", "Grammatical", "Gender Violation")  # Properly label Condition
  )

# Define the function to assign brain regions based on electrode names
assign_brain_region <- function(electrode) {
  case_when(
    electrode %in% c("F3", "F7", "FC1", "FC5") ~ "Left Anterior",
    electrode %in% c("F4", "F8", "FC2", "FC6") ~ "Right Anterior",
    electrode %in% c("CP1", "CP5") ~ "Left Medial",
    electrode %in% c("CP2", "CP6") ~ "Right Medial",
    electrode %in% c("P3", "P7") ~ "Left Posterior",
    electrode %in% c("P4", "P8") ~ "Right Posterior",
    electrode == "Fz" ~ "Middle Anterior",
    electrode == "FCz" ~ "Middle Medial",
    electrode == "Pz" ~ "Middle Posterior",
    TRUE ~ NA_character_
  )
}

# Apply function to add Brain Region column
EEGdata_200_500_long <- EEGdata_200_500_long %>%
  mutate(Brain_Region = assign_brain_region(Electrode))

# Check structure to ensure columns are correctly assigned
str(EEGdata_200_500_long)  # Verify `Condition`, `Voltage`, and `Brain_Region` are present

### Merge EEG data with mapping_table to add `home_IDs` ###

# Merge EEG data with mapping_table to get `home_IDs`
EEG_with_home_IDs_200_500 <- EEGdata_200_500_long %>%
  left_join(mapping_table, by = c("File" = "lab_IDs"))

# Verify merge success
str(EEG_with_home_IDs_200_500)  # `home_IDs` should now be present

### Prepare EF_data and Merge with EEG Data ###

# Example: Select and rename columns for EF data, ensuring `home_IDs` is present
EF_data <- data %>%
  select(home_IDs, max_digits_recalled, ASRT_score, difference_reaction_time)

# Merge EEG data (with home_IDs) with EF data
combined_data_200_500 <- EF_data %>%
  left_join(EEG_with_home_IDs_200_500, by = "home_IDs")

# Ensure `Condition`, `Voltage`, `Brain_Region`, and EF scores are in `combined_data_200_500`
str(combined_data_200_500)  # Verify all necessary columns are included

### Filter Combined Data and Plot ###

# Filter out rows with NA values in important columns
combined_data_200_500_filtered <- combined_data_200_500 %>%
  filter(!is.na(Brain_Region) & !is.na(Voltage) & !is.na(Condition))

# Plot DGS (Working Memory)
plot_dgs_200_500 <- ggplot(combined_data_200_500_filtered, aes(x = max_digits_recalled, y = Voltage, color = Condition)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ Condition) +
  labs(x = "DGS (Working Memory)", y = NULL, color = NULL) +  # Remove Y-axis title
  theme_minimal() +
  theme(
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
    legend.position = "none"
  )

# Plot ASRT (Implicit Learning)
plot_asrt_200_500 <- ggplot(combined_data_200_500_filtered, aes(x = ASRT_score, y = Voltage, color = Condition)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ Condition) +
  labs(x = "ASRT (Implicit Learning)", y = NULL, color = NULL) +  # Remove Y-axis title
  theme_minimal() +
  theme(
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
    legend.position = "none"
  )

# Plot Stroop (Inhibitory Control)
plot_stroop_200_500 <- ggplot(combined_data_200_500_filtered, aes(x = difference_reaction_time, y = Voltage, color = Condition)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ Condition) +
  labs(x = "Stroop (Inhibitory Control)", y = NULL, color = NULL) +  # Remove Y-axis title
  theme_minimal() +
  theme(
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
    legend.position = "none"
  )

# Combine Plots with Shared Y-axis Title
grid.arrange(
  plot_dgs_200_500,
  plot_asrt_200_500,
  plot_stroop_200_500,
  ncol = 1,
  left = textGrob("EEG Amplitude (µV)", rot = 90, vjust = 1, gp = gpar(fontsize = 12))  # Shared Y-axis title
)

##########################
### Analysis: Omnibus Model for 200-500 ms ###
##########################

# Merge combined_LHQ3 with combined_data_200_500_filtered
combined_data_200_500_filtered <- combined_data_200_500_filtered %>%
  left_join(combined_LHQ3, by = c("home_IDs" = "Participant ID"))

# Verify the structure
str(combined_data_200_500_filtered)

# Remove the newline character from the colum
combined_data_200_500_filtered <- combined_data_200_500_filtered %>%
  rename(`Multilingual Language Diversity Score` = `Multilingual Language Diversity\nScore`)

# Filter the data for analysis
analysis_data_200_500 <- combined_data_200_500_filtered %>%
  filter(!is.na(Voltage) & !is.na(Condition) & !is.na(Brain_Region) &
           !is.na(max_digits_recalled) & !is.na(ASRT_score) & 
           !is.na(difference_reaction_time) & !is.na(`Multilingual Language Diversity Score`))

# Verify the filtered data
str(analysis_data_200_500)

# Create a grammaticality column
analysis_data_200_500 <- analysis_data_200_500 %>%
  mutate(grammaticality = ifelse(Condition == "Grammatical", "grammatical", "violation"))

# Omnibus model with grammaticality and other predictors
omnibus_model_200_500 <- lmer(
  Voltage ~ grammaticality +
    `Multilingual Language Diversity Score` +
    difference_reaction_time +
    ASRT_score +
    max_digits_recalled +
    Brain_Region +
    grammaticality:`Multilingual Language Diversity Score` +
    grammaticality:difference_reaction_time +
    grammaticality:ASRT_score +
    grammaticality:max_digits_recalled +
    grammaticality:Brain_Region +
    (1 | home_IDs),  # Random intercept for participants
  data = analysis_data_200_500
)

# View the model summary
summary(omnibus_model)


# Display the model summary
summary(omnibus_model)

##########################
### Follow-Up Analyses ###
##########################

# If significant interactions are identified, subset data for further analyses
# Example: Subset by significant interactions (e.g., grammaticality and brain_region)

# Subset data for Grammaticality condition
grammatical_data <- analysis_data_200_500 %>% filter(Condition == "Grammatical")
gender_violation_data <- analysis_data_200_500 %>% filter(Condition == "Gender Violation")

# Scale the predictors
# Create scaled versions of predictors for grammatical condition
grammatical_data <- grammatical_data %>%
  mutate(
    scaled_multilingual_language_diversity = scale(`Multilingual Language Diversity Score`),
    scaled_difference_reaction_time = scale(difference_reaction_time),
    scaled_ASRT_score = scale(ASRT_score),
    scaled_max_digits_recalled = scale(max_digits_recalled)
  )

# Create scaled versions of predictors for gender violation condition
gender_violation_data <- gender_violation_data %>%
  mutate(
    scaled_multilingual_language_diversity = scale(`Multilingual Language Diversity Score`),
    scaled_difference_reaction_time = scale(difference_reaction_time),
    scaled_ASRT_score = scale(ASRT_score),
    scaled_max_digits_recalled = scale(max_digits_recalled)
  )

# Run models with scaled predictors
# Grammatical Condition
grammatical_model <- lmer(
  Voltage ~ scaled_multilingual_language_diversity +
    scaled_difference_reaction_time +
    scaled_ASRT_score +
    scaled_max_digits_recalled +
    Brain_Region +
    scaled_multilingual_language_diversity:Brain_Region +
    scaled_difference_reaction_time:Brain_Region +
    scaled_ASRT_score:Brain_Region +
    scaled_max_digits_recalled:Brain_Region +
    (1 | home_IDs),
  data = grammatical_data
)
summary(grammatical_model)

# Gender Violation Condition
gender_violation_model <- lmer(
  Voltage ~ scaled_multilingual_language_diversity +
    scaled_difference_reaction_time +
    scaled_ASRT_score +
    scaled_max_digits_recalled +
    Brain_Region +
    scaled_multilingual_language_diversity:Brain_Region +
    scaled_difference_reaction_time:Brain_Region +
    scaled_ASRT_score:Brain_Region +
    scaled_max_digits_recalled:Brain_Region +
    (1 | home_IDs),
  data = gender_violation_data
)
summary(gender_violation_model)

##########################
### Post-Hoc Pairwise Comparisons ###
##########################
# Pairwise comparisons for grammaticality within brain regions
pairwise_grammaticality_200_500 <- emmeans(omnibus_model_200_500, pairwise ~ grammaticality | Brain_Region)
# Print results
print(pairwise_grammaticality_200_500)

##########################
### Analysis: Omnibus Model for 300-600 ms ###
##########################

# Merge combined_LHQ3 with combined_data_300_600_filtered
combined_data_300_600_filtered <- combined_data_300_600_filtered %>%
  left_join(combined_LHQ3, by = c("home_IDs" = "Participant ID"))

# Verify the structure
str(combined_data_300_600_filtered)

# Remove the newline character from the column
combined_data_300_600_filtered <- combined_data_300_600_filtered %>%
  rename(`Multilingual Language Diversity Score` = `Multilingual Language Diversity\nScore`)

# Filter the data for analysis
analysis_data_300_600 <- combined_data_300_600_filtered %>%
  filter(!is.na(Voltage) & !is.na(Condition) & !is.na(Brain_Region) &
           !is.na(max_digits_recalled) & !is.na(ASRT_score) &
           !is.na(difference_reaction_time) & !is.na(`Multilingual Language Diversity Score`))

# Verify the filtered data
str(analysis_data_300_600)

# Create a grammaticality column
analysis_data_300_600 <- analysis_data_300_600 %>%
  mutate(grammaticality = ifelse(Condition == "Grammatical", "grammatical", "violation"))

# Omnibus model with grammaticality and other predictors
omnibus_model_300_600 <- lmer(
  Voltage ~ grammaticality +
    `Multilingual Language Diversity Score` +
    difference_reaction_time +
    ASRT_score +
    max_digits_recalled +
    Brain_Region +
    grammaticality:`Multilingual Language Diversity Score` +
    grammaticality:difference_reaction_time +
    grammaticality:ASRT_score +
    grammaticality:max_digits_recalled +
    grammaticality:Brain_Region +
    (1 | home_IDs),  # Random intercept for participants
  data = analysis_data_300_600
)

# View the model summary
summary(omnibus_model_300_600)

##########################
### Follow-Up Analyses ###
##########################

# Subset data for Grammaticality condition
grammatical_data_300_600 <- analysis_data_300_600 %>% filter(Condition == "Grammatical")
gender_violation_data_300_600 <- analysis_data_300_600 %>% filter(Condition == "Gender Violation")

# Scale the predictors
grammatical_data_300_600 <- grammatical_data_300_600 %>%
  mutate(
    scaled_multilingual_language_diversity = scale(`Multilingual Language Diversity Score`),
    scaled_difference_reaction_time = scale(difference_reaction_time),
    scaled_ASRT_score = scale(ASRT_score),
    scaled_max_digits_recalled = scale(max_digits_recalled)
  )

gender_violation_data_300_600 <- gender_violation_data_300_600 %>%
  mutate(
    scaled_multilingual_language_diversity = scale(`Multilingual Language Diversity Score`),
    scaled_difference_reaction_time = scale(difference_reaction_time),
    scaled_ASRT_score = scale(ASRT_score),
    scaled_max_digits_recalled = scale(max_digits_recalled)
  )

# Grammatical Condition
grammatical_model_300_600 <- lmer(
  Voltage ~ scaled_multilingual_language_diversity +
    scaled_difference_reaction_time +
    scaled_ASRT_score +
    scaled_max_digits_recalled +
    Brain_Region +
    scaled_multilingual_language_diversity:Brain_Region +
    scaled_difference_reaction_time:Brain_Region +
    scaled_ASRT_score:Brain_Region +
    scaled_max_digits_recalled:Brain_Region +
    (1 | home_IDs),
  data = grammatical_data_300_600
)
summary(grammatical_model_300_600)

# Gender Violation Condition
gender_violation_model_300_600 <- lmer(
  Voltage ~ scaled_multilingual_language_diversity +
    scaled_difference_reaction_time +
    scaled_ASRT_score +
    scaled_max_digits_recalled +
    Brain_Region +
    scaled_multilingual_language_diversity:Brain_Region +
    scaled_difference_reaction_time:Brain_Region +
    scaled_ASRT_score:Brain_Region +
    scaled_max_digits_recalled:Brain_Region +
    (1 | home_IDs),
  data = gender_violation_data_300_600
)
summary(gender_violation_model_300_600)

##########################
### Post-Hoc Pairwise Comparisons ###
##########################
# Pairwise comparisons for grammaticality within brain regions
pairwise_grammaticality_300_600 <- emmeans(omnibus_model_300_600, pairwise ~ grammaticality | Brain_Region)
# Print results
print(pairwise_grammaticality_300_600)

##########################
### Analysis: Omnibus Model for 400-900 ms ###
##########################

# Merge combined_LHQ3 with combined_data_400_900_filtered
combined_data_400_900_filtered <- combined_data_400_900_filtered %>%
  left_join(combined_LHQ3, by = c("home_IDs" = "Participant ID"))

# Verify the structure
str(combined_data_400_900_filtered)

# Remove the newline character from the column
combined_data_400_900_filtered <- combined_data_400_900_filtered %>%
  rename(`Multilingual Language Diversity Score` = `Multilingual Language Diversity\nScore`)

# Filter the data for analysis
analysis_data_400_900 <- combined_data_400_900_filtered %>%
  filter(!is.na(Voltage) & !is.na(Condition) & !is.na(Brain_Region) &
           !is.na(max_digits_recalled) & !is.na(ASRT_score) &
           !is.na(difference_reaction_time) & !is.na(`Multilingual Language Diversity Score`))

# Verify the filtered data
str(analysis_data_400_900)

# Create a grammaticality column
analysis_data_400_900 <- analysis_data_400_900 %>%
  mutate(grammaticality = ifelse(Condition == "Grammatical", "grammatical", "violation"))

# Omnibus model with grammaticality and other predictors
omnibus_model_400_900 <- lmer(
  Voltage ~ grammaticality +
    `Multilingual Language Diversity Score` +
    difference_reaction_time +
    ASRT_score +
    max_digits_recalled +
    Brain_Region +
    grammaticality:`Multilingual Language Diversity Score` +
    grammaticality:difference_reaction_time +
    grammaticality:ASRT_score +
    grammaticality:max_digits_recalled +
    grammaticality:Brain_Region +
    (1 | home_IDs),  # Random intercept for participants
  data = analysis_data_400_900
)

# View the model summary
summary(omnibus_model_400_900)

##########################
### Follow-Up Analyses ###
##########################

# Subset data for Grammaticality condition
grammatical_data_400_900 <- analysis_data_400_900 %>% filter(Condition == "Grammatical")
gender_violation_data_400_900 <- analysis_data_400_900 %>% filter(Condition == "Gender Violation")

# Scale the predictors
grammatical_data_400_900 <- grammatical_data_400_900 %>%
  mutate(
    scaled_multilingual_language_diversity = scale(`Multilingual Language Diversity Score`),
    scaled_difference_reaction_time = scale(difference_reaction_time),
    scaled_ASRT_score = scale(ASRT_score),
    scaled_max_digits_recalled = scale(max_digits_recalled)
  )

gender_violation_data_400_900 <- gender_violation_data_400_900 %>%
  mutate(
    scaled_multilingual_language_diversity = scale(`Multilingual Language Diversity Score`),
    scaled_difference_reaction_time = scale(difference_reaction_time),
    scaled_ASRT_score = scale(ASRT_score),
    scaled_max_digits_recalled = scale(max_digits_recalled)
  )

# Grammatical Condition
grammatical_model_400_900 <- lmer(
  Voltage ~ scaled_multilingual_language_diversity +
    scaled_difference_reaction_time +
    scaled_ASRT_score +
    scaled_max_digits_recalled +
    Brain_Region +
    scaled_multilingual_language_diversity:Brain_Region +
    scaled_difference_reaction_time:Brain_Region +
    scaled_ASRT_score:Brain_Region +
    scaled_max_digits_recalled:Brain_Region +
    (1 | home_IDs),
  data = grammatical_data_400_900
)
summary(grammatical_model_400_900)

# Gender Violation Condition
gender_violation_model_400_900 <- lmer(
  Voltage ~ scaled_multilingual_language_diversity +
    scaled_difference_reaction_time +
    scaled_ASRT_score +
    scaled_max_digits_recalled +
    Brain_Region +
    scaled_multilingual_language_diversity:Brain_Region +
    scaled_difference_reaction_time:Brain_Region +
    scaled_ASRT_score:Brain_Region +
    scaled_max_digits_recalled:Brain_Region +
    (1 | home_IDs),
  data = gender_violation_data_400_900
)
summary(gender_violation_model_400_900)

##########################
### Post-Hoc Pairwise Comparisons ###
##########################
# Pairwise comparisons for grammaticality within brain regions
pairwise_grammaticality_400_900 <- emmeans(omnibus_model_400_900, pairwise ~ grammaticality | Brain_Region)
# Print results
print(pairwise_grammaticality_400_900)

##########################
### Plot voltage by brain region ###
# Data for visualization
emmeans_data <- as.data.frame(pairwise_grammatical$emmeans)

# Plot
ggplot(emmeans_data, aes(x = Brain_Region, y = emmean)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  labs(
    title = "Voltage by Brain Region",
    x = "Brain Region",
    y = "Estimated Voltage (EMM)"
  ) +
  theme_minimal()






















































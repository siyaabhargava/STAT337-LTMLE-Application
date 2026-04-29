# Retrieving Sports Dataset

path <- 'C:/Users/Laleksonis1/OneDrive - Loyola University Chicago/Documents/Class Documents/qBioStats_Project/R_Codes/'
file_name <- 'processed_sports_dataset_2012_2013_2014.csv'

full_path <- paste0(path, file_name)

score_df_read <- read.csv(full_path, header = TRUE)

# Change the path above as needed!
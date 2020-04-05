library(dplyr)
# ----------------------------------------------
# Functions for use with NI Crime data
# ----------------------------------------------

combine_crime_data <- function(path){
  # a) Using R, amalgamate all of the crime data from each csv file into one dataset. 
  # Get all csv file names
  cat("\n\n------------------\nCrime data files:\n------------------\n")
  csv_files <- dir(path = "data/NI Crime Data", 
                   pattern=".*[.]csv", recursive = T, full.names = TRUE)
  print(csv_files)
  
  # bind_rows() from dplyr is used to build a dataframe from the csv files by 
  # passing lapply with the list of file names and the read_csv function
  crime_data <- bind_rows(lapply(csv_files, read.csv))
  
  cat("\n\n------------------\nCrime dataframe:\n------------------\n")
  print(head(crime_data))
  
  # Save this dataset into a csv file called AllNICrimeData. 
  write.csv(crime_data, "data/AllNICrimeData.csv")
  
  # Count and show the number of rows in the AllNICrimeData dataset.
  cat("\n\n------------------\nTotal rows:\n------------------\n")
  print(nrow(crime_data))
  return(crime_data)
}


# ----------------------------------------------
# Execute code
# ----------------------------------------------
crime_data <- combine_crime_data("data/NI Crime Data/")
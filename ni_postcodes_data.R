library(mice)
library(VIM)
# ----------------------------------------------
# Functions for use with the NI Postcode dataset
# ----------------------------------------------

describe_data <- function(postcodes){
  # a) 
  # Show the total number of rows
  cat("\n\n------------------\nTotal rows:\n------------------\n")
  print(nrow(postcodes))
  # structure of the data frame
  cat("\n\n------------------\nDataframe structure:\n------------------\n")
  str(postcodes)
  # First 10 rows of the data frame containing all of the NIPostcode data.
  cat("\n\n------------------\nFirst 10 rows:\n------------------\n")
  head(postcodes, 10)
}

rename_columns <- function(postcodes){
  # b) Add a suitable title for each attribute of the data.
  cat("\n\n------------------\nBefore update:\n------------------\n")
  print(colnames(postcodes))
  
  colnames(postcodes) <- c("Organisation Name",
                           "Sub-building Name",
                           "Building Name", 
                           "Number",
                           "Primary Thorofare",
                           "Alt Thorofare",
                           "Secondary Thorofare",
                           "Locality",
                           "Townland",
                           "Town",
                           "County",
                           "Postcode",
                           "x-coordinates",
                           "y-coordinates",
                           "Primary Key")
  
  cat("\n\n------------------\nAfter update:\n------------------\n")
  print(colnames(postcodes))
  return(postcodes)
}



show_missing_value_counts <- function(){
  # d) Show the total number of missing values for each column in the 
  # postcode data frame
}

move_primary_key <- function(){
  # e) Move the primary key identifier to the start of the dataset.
}

extract_limavady_data <- function(){
  # f) Create a new dataset called Limavady_data. 
  # Store within it only information where locality, townland
  # and town contain the name Limavady. Count and display the number of rows.
  # Store this information in a csv file called Limavady
}

create_cleaned_csv <- function(){
  # g) Save the modified NIPostcode dataset in a csv file called CleanNIPostcodeData
  
}


# ----------------------------------------------
# Execute code
# ----------------------------------------------

# load postcode csv data to dataframe
ni_postcodes <- read.csv("data/NIPostcodes.csv")
describe_data(ni_postcodes)

ni_postcodes <- rename_columns(ni_postcodes)

# c) Remove or replace missing entries with a suitable identifier.
# Decide whether it is best to remove missing data or to recode it. 
# Discuss missing data and justify your decision in detail.

# From inspecting the dataframe, there are a lot of empty cells
# Replacing these with NA to get a clearer idea of what is missing
ni_postcodes[ni_postcodes == ""] <- NA

md.pattern(ni_postcodes)
missing_values <- aggr(ni_postcodes, 
                       prop = FALSE, 
                       numbers = TRUE, 
                       plot = FALSE)

# show summary of missing values
missing_values[["missings"]]

# Missing values of note are Town (19872) and Postcode (8900)

# rows with both Town and Postcode missing will be removed 
# because it leaves too much abiquity for a record, 
# if only Postcode is missing the row will be kept.
ni_postcodes <- ni_postcodes[!(is.na(ni_postcodes$Town) & 
                                 is.na(ni_postcodes$Postcode)),]

# check missing values again
missing_values_after_removal <- aggr(ni_postcodes, 
                                     prop = FALSE, 
                                     numbers = TRUE, 
                                     plot = FALSE)
missing_values_after_removal[["missings"]]
# After the rows with NA for both Town and Postcode were removed 
# The dataset now has records with Town missing in 19446 and Postcode missing in 8474 

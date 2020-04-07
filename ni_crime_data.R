library(dplyr)
library(plyr)
library(collections)
library(stringr)
# ----------------------------------------------
# Functions for use with NI Crime data
# ----------------------------------------------
combine_crime_data <- function(path){
  # a) Amalgamate all of the crime data from each csv file into one dataset. 
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

modify_crime_data_structure <- function(dataframe){
  # b) Modify the structure of dataframe by 
  # removing the attributes: CrimeID, Reported by, Falls within, 
  # LSOA code, LSOA name, last outcome and context. 
  updated_df <- dataframe[, !(colnames(dataframe) %in% c("Crime.ID", 
                                                         "Reported.by", 
                                                         "Falls.within", 
                                                         "LSOA.code", 
                                                         "LSOA.name"))]
  
  # Show the new structure
  cat("\n\n------------------\nAll crime dataframe structure:\n------------------\n")
  str(updated_df, width = 80, strict.width = "cut")
  
  # Save dataframe to file
  write.csv(updated_df, "data/AllNICrimeData.csv")
  return(updated_df)
}

abbreviate_crime_types <- function(dataframe){
  # show all crime type
  cat("\n\n------------------\nNon-abbreviated crime types:\n------------------\n")
  print(unique(dataframe$Crime.type))
  
  # create dictionary of full text and abbreviated crime types
  crime_types_dict <- Dict(list("Anti-social behaviour" = "ASBO", 
                                "Bicycle theft" = "BITH",
                                "Burglary" = "BURG",                    
                                "Criminal damage and arson" = "CDAR",
                                "Drugs" = "DRUG",                        
                                "Other theft" = "OTTH",             
                                "Possession of weapons" = "POW",        
                                "Public order" = "PUBO",             
                                "Robbery" = "ROBY",                      
                                "Shoplifting" = "SHOP",                 
                                "Theft from the person" = "THPR",        
                                "Vehicle crime" = "VECR",               
                                "Violence and sexual offences" = "VISO", 
                                "Other crime" = "OTCR"))
  # convert keys/values to vectors
  keys_list <- as.vector(unlist(crime_types_dict$keys()))
  values_list <- as.vector(unlist(crime_types_dict$values()))
  
  # map dictionary values to original text
  dataframe$Crime.type <- mapvalues(dataframe$Crime.type, 
                                    from = keys_list, 
                                    to = values_list)
  
  # confirm all crime types are abbreviated
  cat("\n\n------------------\nAbbreviated crime types:\n------------------\n")
  print(unique(dataframe$Crime.type))
  return(dataframe)
}

plot_crime_frequency <- function(dataframe){
  # plot crime type frequency (d), horizontak bar chart
  # get count of each crime type and sort
  type_occurences <- sort(table(unlist(dataframe$Crime.type)))
  labels <- as.vector(names(type_occurences))
  blues <- colorRampPalette(colors = c("pink", "purple"))(15)
  
  par(mar = c(5, 6, 4, 2) + 0.1) # set margins, 
  # horizontal labels throughs out spacing for y-axis label
  barplot(type_occurences, 
          main = "Frequency of Crime by Type", 
          ylab = "",
          xlab = "Count",
          horiz = TRUE,
          names.arg = labels,
          las = 1, # horizontal labels
          col = blues,
          mgp = c(3, 1, 0))
  axis(1, at = 
         seq(0, range(type_occurences)[2], 
             by=10000), labels=FALSE)
  mtext("Type", side=2, line=4) # add in y-axis label
}

find_a_town <- function(crime_data, postcode_data){
  
  crime_data$Town <- postcode_data %>%
    filter(grepl(crime_data$Location, 
                 postcode_data$Primary.Thorofare, 
                 ignore.case = TRUE))
  return(with_town_column)
}

# ----------------------------------------------
# Execute code
# ----------------------------------------------
crime_data <- combine_crime_data("data/NI Crime Data/")

# modify structure of dataset
crime_data <- modify_crime_data_structure(crime_data)

# shorten crime type text
crime_data <- abbreviate_crime_types(crime_data)

# plot crime type frequency
plot_crime_frequency(crime_data)

# remove "On or near " from location column (e)
# before update
head(crime_data, n = 10)
crime_data$Location <- str_replace(crime_data$Location, pattern = "On or near ", "")
# replace blank location with NA
crime_data$Location[crime_data$Location == ""] <- NA
# after update
head(crime_data, n = 10)

# load postcode data
ni_postcodes <- read.csv("data/CleanNIPostcodeData.csv")

# (f) get 5000 random samples from crime data, where location is not NA, 
# using sample_n from dplyr
set.seed(100)
random_crime_sample <- crime_data %>%
  filter(!is.na(crime_data$Location)) %>%
  sample_n(5000)

# find location information from postcode data


crime_data$Town <- NA 
crime_data$Town <- ni_postcodes %>%
  filter(grepl(crime_data$Location, 
               Primary.Thorofare, 
               ignore.case = TRUE))

crime_data$Town <- ni_postcodes  %>%
  filter(ni_postcodes$Primary.Thorofare %in% accessions40$V1)

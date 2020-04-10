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
  
  # bind_rows() from dplyr is used to build a dataframe from the csv files by 
  # passing lapply with the list of file names and the read_csv function
  crime_data <- bind_rows(lapply(csv_files, read.csv))
  
  cat("\n\n------------------\nCrime dataframe:\n------------------\n")
  print(head(crime_data, n = 5))
  
  # Save this dataset into a csv file called AllNICrimeData. 
  write.csv(crime_data, "data/AllNICrimeData.csv")
  
  # Count and show the number of rows in the AllNICrimeData dataset.
  cat("\n\n------------------\nTotal rows:\n------------------\n")
  print(nrow(crime_data))
  return(crime_data)
}

modify_crime_data_structure <- function(dataframe, 
                                        remove_columns, 
                                        save = TRUE,
                                        file_name = "crime_data.csv"){
  # Show the new structure
  cat("\n\n------------------\nExisting crime dataframe structure:\n------------------\n")
  str(dataframe, width = 80, strict.width = "cut")
  
  # new datafram, excluding remove_columns list
  updated_df <- dataframe[, !(colnames(dataframe) %in% remove_columns)]
  
  # Show the new structure
  cat("\n\n------------------\nNew crime dataframe structure:\n------------------\n")
  str(updated_df, width = 80, strict.width = "cut")
  
  # Save dataframe to file
  write.csv(updated_df, file_name)
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
  # plot crime type frequency (d), horizontal bar chart
  # get count of each crime type and sort
  type_occurences <- sort(table(unlist(dataframe$Crime.type)))
  labels <- as.vector(names(type_occurences))
  colour <- colorRampPalette(colors = c("pink", "purple"))(15)
  
  par(mar = c(5, 6, 4, 2) + 0.1) # set margins, 
  # horizontal labels throughs out spacing for y-axis label
  barplot(type_occurences, 
          main = "Frequency of Crime by Type", 
          ylab = "",
          xlab = "Count",
          horiz = TRUE,
          names.arg = labels,
          las = 1, # horizontal labels
          col = colour,
          mgp = c(3, 1, 0))
  axis(1, at = 
         seq(0, range(type_occurences)[2], 
             by=10000), labels=FALSE)
  mtext("Type", side=2, line=4) # add in y-axis label
}

find_a_town <- function(crime_data, postcode_data){
  # (f) find location information from postcode data
  # get town from postcode data based on crime.location matching postcode.PrimaryThorofare
  crime_data$Town <- with(postcode_data, 
                          Town[match(
                            toupper(crime_data$Location), 
                            Primary.Thorofare)])
  cat("\n\n------------------\nTown Included:\n------------------\n")
  print(head(crime_data, n = 5))
  return(crime_data)
}

add_town_data <- function(crime_data, village_data){
  # (g) Add population to crime dataframe
  crime_data$Population <- with(village_data, 
                                POPULATION[match(
                                  crime_data$Town, 
                                  toupper(CITY.TOWN.VILLAGE))])
  cat("\n\n------------------\nPopulation Included:\n------------------\n")
  print(head(crime_data))
  return(crime_data)
}

plot_derry_belfast_crime <- function(derry_data, belfast_data){
  # plot derry/belfast crime type frequency (i), horizontal bar charts side-by-side
  # get count of each crime type and sort
  type_occurences_derry <- sort(table(unlist(derry_data$Crime.type)))
  labels_derry <- as.vector(names(type_occurences_derry))
  colour1 <- colorRampPalette(colors = c("lightblue", "blue"))(15)
  type_occurences_belfast <- sort(table(unlist(belfast_data$Crime.type)))
  labels_belfast <- as.vector(names(type_occurences_belfast))
  colour2 <- colorRampPalette(colors = c("lightgreen", "green"))(15)
  
  # set columns:2/rows:1, margins, oma set for overall title space, 
  # cex.* for setting font sizes
  par(mfrow = c(1, 2), 
      mar = c(5, 6, 4, 2), 
      oma = c(0, 0, 2, 0),
      cex.axis = 0.75,
      cex.lab=1,
      cex.main=1.25)
  barplot(type_occurences_derry, 
          main = "Derry", 
          ylab = "",
          xlab = "Count",
          horiz = TRUE,
          names.arg = labels_derry,
          las = 1, # horizontal labels
          col = colour1,
          mgp = c(3, 1, 0),
          xlim = c(0, 450))
  mtext("Type", side=2, line=4) # add in y-axis label
  
  barplot(type_occurences_belfast, 
          main = "Belfast", 
          ylab = "",
          xlab = "Count",
          horiz = TRUE,
          names.arg = labels_belfast,
          las = 1, # horizontal labels
          col = colour2,
          mgp = c(3, 1, 0),
          xlim = c(0, 450))
  mtext("Type", side=2, line=4) # add in y-axis label
  mtext("Derry vs Belfast Crime Rates", outer = TRUE, cex = 1.5)
}

# ----------------------------------------------
# Execute code
# ----------------------------------------------
crime_data <- combine_crime_data("data/NI Crime Data/")

# b) Modify the structure of dataframe by 
# removing the attributes: CrimeID, Reported by, Falls within, 
# LSOA code, LSOA name, last outcome and context. 
crime_data <- modify_crime_data_structure(crime_data, 
                                          remove_columns = c("Crime.ID", 
                                                             "Reported.by", 
                                                             "Falls.within", 
                                                             "LSOA.code", 
                                                             "LSOA.name",
                                                             "Last.outcome.category", 
                                                             "Context"),
                                          file_name = "data/AllNICrimeData.csv")

# shorten crime type text
crime_data <- abbreviate_crime_types(crime_data)

# plot crime type frequency
plot_crime_frequency(crime_data)

# remove "On or near " from location column (e)
# before update
head(crime_data, n = 5)
crime_data$Location <- str_replace(crime_data$Location, pattern = "On or near ", "")
# replace blank location with NA
crime_data$Location[crime_data$Location == ""] <- NA
# after update
head(crime_data, n = 5)

# load postcode data
ni_postcodes <- read.csv("data/CleanNIPostcodeData.csv")

# f) get 5000 random samples from crime data, where location is not NA or "No Location", 
# using sample_n from dplyr
set.seed(100)
random_crime_sample <- crime_data %>%
  filter(!is.na(crime_data$Location) & crime_data$Location != "No Location") %>%
  sample_n(5000)
head(random_crime_sample, n = 5)

# find location information from postcode data
random_crime_sample <- find_a_town(random_crime_sample, ni_postcodes)

# match population from village dataset to crime sample
village_data <- read.csv("data/VillageList.csv")

# update londonderry to derry so population can be found from lookup dataset
random_crime_sample$Town <- as.character(random_crime_sample$Town)
random_crime_sample$Town[random_crime_sample$Town == "LONDONDERRY"] <- "DERRY"

random_crime_sample <- add_town_data(random_crime_sample, village_data)

# h) renaming Town to City-Town-Village, save to csv
colnames(random_crime_sample)[
  colnames(random_crime_sample) == "Town"] <- "City-Town-Village"
colnames(random_crime_sample)
write.csv(random_crime_sample, "data/random_crime_sample.csv")

# plot Derry vs Belfast crime data
# first create a dataframe for each city
derry_data <- random_crime_sample[
  which(random_crime_sample$"City-Town-Village" == "DERRY"), ]
head(derry_data)
belfast_data <- random_crime_sample[
  which(random_crime_sample$"City-Town-Village" == "BELFAST"), ]
head(belfast_data)

plot_derry_belfast_crime(derry_data, belfast_data)





##Hello World, this is a data cleaning script for loading resighting data into R.
##Rodrigo Martin de Oliveira

##install.packages("dplyr")
##install.packages("lubridate")
##install.packages("ggplot2")

## Load necessary libraries
library(dplyr)
library(lubridate)
library(ggplot2)
renv::snapshot()


# Load resighting data from CSV files into R data frames
resight_binder4 <- read.csv(file = "RAWDATA/binder4-resightings.csv", header = TRUE, sep = ",")
resight_binder3 <- read.csv(file = "RAWDATA/binder3-resightings.csv", header = TRUE, sep = ",")
resight_binder2 <- read.csv(file = "RAWDATA/binder2-resightings.csv", header = TRUE, sep = ",")
resight_binder1 <- read.csv(file = "RAWDATA/binder1-resightings.csv", header = TRUE, sep = ",")
resight_binder0 <- read.csv(file = "RAWDATA/binder0-resightings.csv", header = TRUE, sep = ",")


#load radio-tagging data from CSV file into R data frame
radio_tagging_binder6 <- read.csv(file = "RAWDATA/radio_tagging_binder6.csv", header = TRUE, sep = ",")
radio_tagging_binder5 <- read.csv(file = "RAWDATA/radio_tagging_binder5.csv", header = TRUE, sep = ",")
radio_tagging_binder4 <- read.csv(file = "RAWDATA/radio_tagging_binder4.csv", header = TRUE, sep = ",")
radio_tagging_binder3 <- read.csv(file = "RAWDATA/radio_tagging_binder3.csv", header = TRUE, sep = ",")
radio_tagging_binder2 <- read.csv(file = "RAWDATA/radio_tagging_binder2.csv", header = TRUE, sep = ",")
radio_tagging_binder1 <- read.csv(file = "RAWDATA/radio_tagging_binder1.csv", header = TRUE, sep = ",")
radio_tagging_binder0 <- read.csv(file = "RAWDATA/radio_tagging_binder0.csv", header = TRUE, sep = ",")

# Load banding info data provided by Mercy from St. Dennis
SD_pond_coords_in <- read.csv(file = "RAWDATA/SD_pond_coords_in.csv", header = TRUE, sep = ",")
band_num_species_MH_12.08.2025 <- read.csv(file = "RAWDATA/band_num_species_MH_12.08.2025.csv", header = TRUE, sep = ",")


#add a title to first column of binder2
resight_binder2 <- resight_binder2 %>%
  rename(band_num = X)

# check the first row and column names of binder2
head(resight_binder2)

#####################################################################################
###Code from 2026-01-03 starts here##################################################
#####################################################################################


## Create a new data frame with columns from "band_num_species_MH_12.08.2025" data frame, selecting only "band_num", "species", "DATE" and "COORDINATES" columns
  banding_info <- band_num_species_MH_12.08.2025 %>%
  select(band_num, SPECIES, DATE, COORDINATES)
# Export "banding_info" data frame to CSV file
write.csv(banding_info, file = "CLEANDATA/banding_info.csv", row.names = TRUE)
  

## Create a new data frame called "pond_coords" with columns from "SD_pond_coords_in" data frame, selecting only "POND_NAME", "LATITUDE" and "LONGITUDE" columns
pond_coords <- SD_pond_coords_in %>%
  select(Pond.ID, Easting, Northing)
# Export "pond_coords" data frame to CSV file
write.csv(pond_coords, file = "CLEANDATA/pond_coords.csv", row.names = TRUE)

## Create a new data frame called "resights_all_binders" combining all resight_binder data frames "resight_binder0" to "resight_binder4"
resights_all_binders <- bind_rows(
  resight_binder0,
  resight_binder1,
  resight_binder2,
  resight_binder3,
  resight_binder4
)
# Export "resights_all_binders" data frame to CSV file  
write.csv(resights_all_binders, file = "CLEANDATA/resights_all_binders.csv", row.names = TRUE)

#####################################################################################
###Code from 2026-01-03 ends here##################################################
#####################################################################################


#Combine columns x.1, x.2 and x.3 of binder2 into a new column called date and then incorporate it into date_resighted column
resight_binder2 <- resight_binder2 %>%
  mutate(date = paste(X.3, X.2, X.1, sep = "-")) %>% 
  mutate(date_combined = paste(date_resighted, date))

sight_binder2 <- resight_binder2 %>%
  mutate(
    # Combine X.3, X.2, X.1 into a new column 'date'
    date = paste(X.3, X.2, X.1, sep = "-"),
    
    # Merge 'date' with existing 'date_resighted' row-wise
    date_resighted = paste(date_resighted, date, sep = "-"))

head(resight_binder2)
#resight_binder2$date_resighted <- as.Date(resight_binder2$date_combined, format="%Y-%d-%m")

# remove NA values from date_combined column in binder2 data frame
#resight_binder2 <- resight_binder2 %>%
# filter(!is.na(date_combined))



# Convert date_resighted columns to Date format
resight_binder4$date_resighted <- as.Date(resight_binder4$date_resighted, format="%Y-%d-%m")
resight_binder3$date_resighted <- as.Date(resight_binder3$date_resighted, format="%Y-%d-%m")
resight_binder2$date_resighted <- as.Date(resight_binder2$date_resighted, format="%Y-%d-%m")
resight_binder1$date_resighted <- as.Date(resight_binder1$date_resighted, format="%Y-%d-%m")
resight_binder0$date_resighted <- as.Date(resight_binder0$date_resighted, format="%Y-%d-%m")



# get unique values from "band_num" column in all binders into a single data frame
unique_bands <- bind_rows(
  resight_binder4 %>% select(band_num),
  resight_binder3 %>% select(band_num),
  resight_binder2 %>% select(band_num),
  resight_binder1 %>% select(band_num),
  resight_binder0 %>% select(band_num),
  radio_tagging_binder0 %>% select(band_num),
  radio_tagging_binder1 %>% select(band_num),
  radio_tagging_binder2 %>% select(band_num),
  radio_tagging_binder3 %>% select(band_num),
  radio_tagging_binder4 %>% select(band_num),
  radio_tagging_binder5 %>% select(band_num),
  radio_tagging_binder6 %>% select(band_num)
) %>%
  distinct()

resight_binder4
band_num\

nrow(unique_band_species)
length(unique(unique_band_species$band_num))


# unique_band_species data frame with unique band_num and species combinations
unique_band_species <- bind_rows(
  resight_binder4 %>% select(band_num),
  resight_binder3 %>% select(band_num),
  resight_binder2 %>% select(band_num),
  resight_binder1 %>% select(band_num),
  resight_binder0 %>% select(band_num),
    radio_tagging_binder0 %>% select(band_num, species),
    radio_tagging_binder1 %>% select(band_num, species),
    radio_tagging_binder2 %>% select(band_num, species),
    radio_tagging_binder3 %>% select(band_num, species),
    radio_tagging_binder4 %>% select(band_num, species),
    radio_tagging_binder5 %>% select(band_num, species),
    radio_tagging_binder6 %>% select(band_num, species)
  ) %>%
  distinct()

unique_band_species$species [is.na(unique_band_species$species)] <- "Unknown"

# export "unique_band_species" data frame to CSV file
write.csv(unique_band_species, file = "CLEANDATA//band_num_species.csv", row.names = TRUE, col.names = NA)

str(band_num_species)
str(unique_bands)

# compairing band_num_species to unique_bands
du <- intersect(band_num_species$band_num, unique_bands$band_num)

# Select the six first characters from "band_num" column and crate a new column called "just_band"
unique_bands <- unique_bands %>%
  mutate(just_band = substr(band_num, 1, 10))


# Export "unique_bands" data frame to CSV 
write.csv(unique_bands, file = "CLEANDATA/unique_bands.csv")



# export "unique_bands" data frame to CSV file
#write.csv(unique_bands, file = "CLEANDATA//unique_bands.csv")


# get unique values from "location" column in all binders into a single data frame
unique_locations <- bind_rows(
  resight_binder4 %>% select(location),
  resight_binder3 %>% select(location),
  resight_binder2 %>% select(location),
  resight_binder1 %>% select(location),
  resight_binder0 %>% select(location)
) %>%
  distinct()

# export "location" data frame to CSV file
#write.csv(unique_locations, file = "CLEANDATA//unique_locations.csv")


# Plot the distribution of resightings over time for each binder using ggplot2
ggplot() +
  geom_histogram(data = resight_binder4, aes(x = date_resighted), binwidth = 30, fill = "blue", alpha = 0.5) +
  geom_histogram(data = resight_binder3, aes(x = date_resighted), binwidth = 30, fill = "red", alpha = 0.5) +
  geom_histogram(data = resight_binder2, aes(x = date_resighted), binwidth = 30, fill = "green", alpha = 0.5) +
  geom_histogram(data = resight_binder1, aes(x = date_resighted), binwidth = 30, fill = "purple", alpha = 0.5) +
  geom_histogram(data = resight_binder0, aes(x = date_resighted), binwidth = 30, fill = "orange", alpha = 0.5) +
  labs(title = "Distribution of Resightings Over Time",
       x = "Date Resighted",
       y = "Count of Resightings") +
  theme_minimal()
renv::snapshot()

# Save the plot to a file "PICTURES"
ggsave("PICTURES/resightings_distribution.png")
renv::snapshot()

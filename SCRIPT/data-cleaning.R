##Hello World, this is a data cleaning script for loading resighting data into R.
##Rodrigo Martin de Oliveira

##install.packages("dplyr")

## Load necessary libraries
library(dplyr)
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


# View the first few rows of each data frame to verify successful loading
head(resight_binder4)
head(resight_binder3)
head(resight_binder2)
head(resight_binder1)
head(resight_binder0)
# Check the structure of each data frame
str(resight_binder4)
str(resight_binder3)
str(resight_binder2)
str(resight_binder1)
str(resight_binder0)

resight_binder4$date_resighted <- as.Date(resight_binder4$date_resighted, format="%Y-%d-%m")
resight_binder3$date_resighted <- as.Date(resight_binder3$date_resighted, format="%Y-%d-%m")
resight_binder2$date_resighted <- as.Date(resight_binder2$date_resighted, format="%Y-%d-%m")
resight_binder1$date_resighted <- as.Date(resight_binder1$date_resighted, format="%Y-%d-%m")
resight_binder0$date_resighted <- as.Date(resight_binder0$date_resighted, format="%Y-%d-%m")

#add a title to first column of binder2
resight_binder2 <- resight_binder2 %>%
  rename(band_num = X)

#Combine columns x.1, x.2 and x.3 of binder2 into a new column called date and then incorporate it into date_resighted column
resight_binder2 <- resight_binder2 %>%
  mutate(date = paste(X.3, X.2, X.1, sep = "-")) %>% 
  mutate(date_combined = paste(date_resighted, date))
  
#head(resight_binder2)
#resight_binder2$date_resighted <- as.Date(resight_binder2$date_combined, format="%Y-%d-%m")

# remove NA values from date_combined column in binder2 data frame
resight_binder2 <- resight_binder2 %>%
  filter(!is.na(date_combined))

# combine data from "band_num" column in all binders into a single data frame
all_band_num_data <- bind_rows(resight_binder4$band_num, resight_binder3$band_num, resight_binder2$band_num, resight_binder1$band_num, resight_binder0$band_num)


##Hello World, this is a data cleaning script for loading resighting data into R.
##Rodrigo Martin de Oliveira

##install.packages("dplyr")

## Load necessary libraries
library(dplyr)
renv::snapshot()


# Load resighting data from CSV files into R data frames
binder4 <- read.csv(file = "RAWDATA/binder4-resightings.csv", header = TRUE, sep = ",")
binder3 <- read.csv(file = "RAWDATA/binder3-resightings.csv", header = TRUE, sep = ",")
binder2 <- read.csv(file = "RAWDATA/binder2-resightings.csv", header = TRUE, sep = ",")
binder0 <- read.csv(file = "RAWDATA/binder0-resightings.csv", header = TRUE, sep = ",")




# View the first few rows of each data frame to verify successful loading
head(binder4)
head(binder3)
head(binder2)
head(binder0)
# Check the structure of each data frame
str(binder4)
str(binder3)
str(binder2)
str(binder0)

binder3$date_resighted <- as.Date(binder3$date_resighted, format="%Y-%d-%m")
binder4$date_resighted <- as.Date(binder4$date_resighted, format="%Y-%d-%m")
binder2$date_resighted <- as.Date(binder2$date_resighted, format="%Y-%d-%m")

#add a title to first column of binder2
binder2 <- binder2 %>%
  rename(band_num = X)

#Combine columns x.1, x.2 and x.3 of binder2 into a new column called date and then incorporate it into date_resighted column
binder2 <- binder2 %>%
  mutate(date = paste(X.3, X.2, X.1, sep = "-")) %>%
  mutate(combine(date_resighted, date))
  


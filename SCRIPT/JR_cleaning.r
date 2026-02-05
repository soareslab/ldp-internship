# updated cleaning code written by Jessica Reemeyer to fix issues with data cleaning 

library(tidyverse)

#### Original Banding Event Information ####
#loading in banding information
raw_banding_info <- read.csv(file = "RAWDATA/band_num_species_MH_12.08.2025.csv", header = TRUE, sep = ",")

banding_info <- raw_banding_info %>%
  filter(EVENT == "original banding" | EVENT == "orig bndg") %>%
  mutate(SPECIES = if_else(SPECIES=="", species, SPECIES), #if SPECIES column is blank, fill with values from species
         SPECIES = str_replace_all(SPECIES, "GAD", "GADW"), #replacing GAD with GADW
         SPECIES = str_trunc(SPECIES, 4, ellipsis=""), #to fix instance of GADWALL entered instead of GADW 
         date = as.Date(DATE, format = "%m/%d/%Y", optional=TRUE) 
  ) %>%
  select(band_num, date, SPECIES, COORDINATES) %>%
  distinct(band_num, .keep_all = TRUE) #removes about 200 duplicate banding numbers

write.csv(banding_info, "CLEANDATA/JR_cleaned/banding_event_information.csv")

#### Resighting Data ####

#loading in the binder data (one file per physical binder)
binder4 <- read.csv(file = "RAWDATA/binder4-resightings.csv", header = TRUE, sep = ",") %>% filter(!is.na(date_resighted) & !is.na(band_num)) 

binder3 <- read.csv(file = "RAWDATA/binder3-resightings.csv", header = TRUE, sep = ",") %>% filter(!is.na(date_resighted) & !is.na(band_num))

binder2 <- read.csv(file = "RAWDATA/binder2-resightings.csv", header = TRUE, sep = ",") %>% rename(band_num = X) %>% filter(!is.na(band_num)) %>%
  mutate(date = paste(X.3, X.2, X.1, sep = "-"),
         date_resighted = if_else(date_resighted=="", date, date_resighted)) # some dates are in separate columns for month day year

binder1 <- read.csv(file = "RAWDATA/binder1-resightings.csv", header = TRUE, sep = ",") %>% filter(!is.na(date_resighted) & !is.na(band_num))

binder0 <- read.csv(file = "RAWDATA/binder0-resightings.csv", header = TRUE, sep = ",") %>% filter(!is.na(date_resighted) & !is.na(band_num))

#combining binder dataframes into one dataframe (warnings are for the date cleaning and should be expected)
all_resight <- bind_rows(binder0, binder1, binder2, binder3, binder4) %>% 
  filter(!is.na(date_resighted) & !is.na(location) & band_num!="DONE" & band_num !="") %>% 
  select(band_num, date_resighted, location, observer, status, comments) %>%
  #cleaning the date column by parsing those in ymd and those in dmy into separate columns and then coalescing them
  mutate(ymd = ymd(date_resighted),
         dmy = dmy(date_resighted),
         date_resighted = coalesce(ymd,dmy)) %>%
  select(-ymd, -dmy) %>%
  filter(date_resighted >= "1980-01-01") #filtering 3 dates with typos

unique_bands_resight$band_num <- unique(all_resight$band_num)
unique_bands_resight$banding_info <- unique_bands_resight$band_num %in% banding_info$band_num
length(unique_bands_resight$band_num)-sum(unique_bands_resight$banding_info) # 17 band numbers not found in the banding_info datasheet, also ~200 in the banding info datasheet were not resighted

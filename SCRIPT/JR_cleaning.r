# updated cleaning code written by Jessica Reemeyer to fix issues with data cleaning 

library(tidyverse)
library(sf)

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
  distinct(band_num, .keep_all = TRUE) %>% #removes about 200 duplicate banding numbers 
  separate(COORDINATES, c("latitude", "longitude"), sep = " ", remove = TRUE) %>%
  rename(species=SPECIES)

#write.csv(banding_info, "CLEANDATA/JR_cleaned/banding_event_information.csv")

#### Resighting Data ####

#loading in the binder data (one file per physical binder)
binder4 <- read.csv(file = "RAWDATA/binder4-resightings.csv", header = TRUE, sep = ",") %>% filter(!is.na(date_resighted) & !is.na(band_num)) 

binder3 <- read.csv(file = "RAWDATA/binder3-resightings.csv", header = TRUE, sep = ",") %>% filter(!is.na(date_resighted) & !is.na(band_num))

binder2 <- read.csv(file = "RAWDATA/binder2-resightings.csv", header = TRUE, sep = ",") %>% rename(band_num = X) %>% filter(!is.na(band_num)) %>%
  mutate(date = paste(X.3, X.2, X.1, sep = "-"),
         date_resighted = if_else(date_resighted=="", date, date_resighted)) # some dates are in separate columns for month day year

binder1 <- read.csv(file = "RAWDATA/binder1-resightings.csv", header = TRUE, sep = ",") %>% filter(!is.na(date_resighted) & !is.na(band_num))

binder0 <- read.csv(file = "RAWDATA/binder0-resightings.csv", header = TRUE, sep = ",") %>% filter(!is.na(date_resighted) & !is.na(band_num))

#combining binder dataframes into one dataframe (warnings pop up for the date cleaning and should be expected)
all_resight <- bind_rows(binder0, binder1, binder2, binder3, binder4) %>% 
  filter(!is.na(date_resighted) & !is.na(location) & band_num!="DONE" & band_num !="") %>% 
  select(band_num, date_resighted, location, observer, status, comments) %>%
  #cleaning the date column by parsing those in ymd and those in dmy into separate columns and then coalescing them
  mutate(ymd = ymd(date_resighted),
         dmy = dmy(date_resighted),
         date_resighted = coalesce(ymd,dmy)) %>%
  select(-ymd, -dmy) %>%
  filter(date_resighted >= "1980-01-01") #filtering 3 dates with typos in the year making them impossible (e.g. 1901)


## seeing which bands are in the banding info 
unique_bands_resight <- data.frame(unique(all_resight$band_num))
unique_bands_resight$band_num <- unique(all_resight$band_num)
unique_bands_resight$banding_info <- unique_bands_resight$band_num %in% banding_info$band_num
missing_bands <- unique_bands_resight %>% filter(banding_info==FALSE) #the band numbers not found in the banding_info dataset
length(unique_bands_resight$band_num)-sum(unique_bands_resight$banding_info) # 17 band numbers not found in the banding_info datasheet


#Checking for the missing resight band_num that were not in the banding_info dataframe
# read in the radiotag data and combine
radio_tagging_binder6 <- read.csv(file = "RAWDATA/radio_tagging_binder6.csv", header = TRUE, sep = ",")
radio_tagging_binder5 <- read.csv(file = "RAWDATA/radio_tagging_binder5.csv", header = TRUE, sep = ",")
radio_tagging_binder4 <- read.csv(file = "RAWDATA/radio_tagging_binder4.csv", header = TRUE, sep = ",")
radio_tagging_binder3 <- read.csv(file = "RAWDATA/radio_tagging_binder3.csv", header = TRUE, sep = ",")
radio_tagging_binder2 <- read.csv(file = "RAWDATA/radio_tagging_binder2.csv", header = TRUE, sep = ",")
radio_tagging_binder1 <- read.csv(file = "RAWDATA/radio_tagging_binder1.csv", header = TRUE, sep = ",")
radio_tagging_binder0 <- read.csv(file = "RAWDATA/radio_tagging_binder0.csv", header = TRUE, sep = ",")

#combine binders and filter out rows without band_num and no species listed (these were caused by comments being added haphazardly to the end of the binder spreadsheets)
radio_data <- rbind(radio_tagging_binder0, 
                    radio_tagging_binder1,
                    radio_tagging_binder2,
                    radio_tagging_binder3,
                    radio_tagging_binder4,
                    radio_tagging_binder5,
                    radio_tagging_binder6) %>% 
  filter(!is.na(band_num)&species!="") 

## Seeing if 17 missing band nums are in the radiotag dataset
missing_bands$band_num %in% radio_data$band_num #3 are in there, 14 are not
#NEED TO APPEND THOSE 3 to banding info dataframe for final output (with "unknown tagging date")


## make decimal lat long from SD_pond_coords_in.csv dataset 
pond_coords <- read.csv(file = "RAWDATA/SD_pond_coords_in.csv", header = TRUE, sep = ",") %>%
  mutate(pond_name1 = paste0("p ",Pond.ID),
         pond_name2 = paste0("p", Pond.ID),
         pond_name3 = paste0("pd ", Pond.ID),
         pond_name4 = paste0("pd", Pond.ID),
         pond_name5 = paste0("pond", Pond.ID),
         pond_name6 = paste0("pond ", Pond.ID))

df_sf <- st_as_sf(pond_coords, coords = c("Easting", "Northing"), crs = 32613) 
df_sf_longlat <- st_transform(df_sf, crs = 4326)
df_longlat_final <- df_sf_longlat %>%
  mutate(
    longitude = st_coordinates(.)[, 1],
    latitude = st_coordinates(.)[, 2]
  ) %>%
  # Convert back to a standard data frame
  st_drop_geometry() 

unique_locations <- distinct(all_resight, location) %>% 
  rename(orig_location = location) %>% 
  mutate(location_found = NA,
         new_location_name = NA,
         gen_location = NA,
         cleaned_location = trimws(str_to_lower(str_remove_all(orig_location, "#"))))

for(i in 1:length(unique_locations$orig_location)){
  
  location <- unique_locations$cleaned_location[i]
  
  for(j in length(pond_coords$Pond.ID):1){
    if(str_detect(location, pond_coords$pond_name1[j]) ||
       str_detect(location, pond_coords$pond_name2[j]) ||
       str_detect(location, pond_coords$pond_name3[j]) ||
       str_detect(location, pond_coords$pond_name4[j]) ||
       str_detect(location, pond_coords$pond_name5[j]) ||
       str_detect(location, pond_coords$pond_name6[j])){
      unique_locations$location_found[i] <- TRUE
      unique_locations$new_location_name[i] <- pond_coords$pond_name6[j]
      unique_locations$gen_location[i] <- "within park"
      break
    } else {
      unique_locations$location_found[i] <- FALSE
      unique_locations$new_location_name[i] <- "unknown"
    }
  }
}

write.csv(unique_locations, "CLEANDATA/JR_cleaned/site_ID_info.csv")


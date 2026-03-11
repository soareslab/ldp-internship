# Code to clean raw data input from physical data sheets
# written by Jessica Reemeyer in Spring 2026 
# contact jessica.reemeyer[at]mail.mcgill.ca for questions

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
  mutate(band_num = gsub("[+qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM ]", "", band_num)) %>% #removing the extra text from some of the band_num
  mutate(band_num = gsub("--", "-", band_num)) %>% #fixing band_num with double --
  select(band_num, date, SPECIES, COORDINATES) %>%
  distinct(band_num, .keep_all = TRUE) %>% #removes about 200 duplicate banding numbers 
  separate(COORDINATES, c("latitude", "longitude"), sep = " ", remove = TRUE) %>%
  rename(species=SPECIES)

#uncomment below to save the banding info
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
  mutate(band_num = gsub("[+qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM ]", "", band_num)) %>% #removing the extra text from some of the band_num
  mutate(band_num = gsub("--", "-", band_num)) %>% #fixing band_nums that had extra "-" between digits
  #cleaning the date column by parsing those in ymd and those in dmy into separate columns and then coalescing them
  mutate(ymd = ymd(date_resighted),
         dmy = dmy(date_resighted),
         date_resighted = coalesce(ymd,dmy)) %>%
  select(-ymd, -dmy) %>%
  filter(date_resighted >= "1980-01-01" & #filtering 3 dates with typos in the year making them impossible (e.g. 1901)
         !str_detect(comments,"not seen") & #removing entries where a bird wasn't seen
        !str_detect(comments, "not observed") & #removing entries where a bird wasn't seen
          !str_detect(status, "not seen") & #removing entries where a bird wasn't seen
          !status=="?") #removing entries where a bird wasn't seen


## seeing which bands are in the banding info 
unique_bands_resight <- data.frame(unique(all_resight$band_num))
unique_bands_resight$band_num <- unique(all_resight$band_num)
unique_bands_resight$banding_info <- unique_bands_resight$band_num %in% banding_info$band_num
missing_bands <- unique_bands_resight %>% filter(banding_info==FALSE) #the band numbers not found in the banding_info dataset
bands_keep <- unique_bands_resight %>% filter(banding_info==TRUE)
length(unique_bands_resight$band_num)-sum(unique_bands_resight$banding_info) # 9 band numbers not found in the banding_info datasheet


#### pond coordinates ####

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

pond_coords_clean <- df_longlat_final %>% select(pond_name6, latitude, longitude) %>% 
  rename(location_name = pond_name6)
write.csv(pond_coords_clean, "BOREALIS/pond_coords.csv")



#### Trying to clean the locations in resightings ####
# a bit of cleaning was accomplished with the code below, but most of the cleaning was done manually
# most of the exact locations were not decipherable, but were reasonably assumed to be in the St Denis NWA or immediately surrounding 

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

#### Actually cleaning the locations in resightings ####

cleaned_locations <- read.csv("CLEANDATA/JR_cleaned/site_ID_info_04Mar2026.csv") %>% 
  mutate(gen_location = case_when(general_location =="within NWA" ~ "NWA",
                                      general_location == "surrounding area" ~ "NWA",
                                  general_location == "outside area" ~ "outside area")) %>%
  select(-general_location) %>%
  rename(location = original_location_description, 
         general_location = gen_location)

all_resight_locations <- left_join(all_resight,cleaned_locations, by=join_by(location)) %>% 
  rename(original_location_description = location) %>%
  select(band_num, date_resighted, original_location_description, general_location, exact_location, status, comments)


ggplot(data=all_resight_locations, aes(date_resighted)) + geom_histogram()

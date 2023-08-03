# packages
library(tidyverse)
library(janitor)
library(vegan)

## plan

#I think the best way to do this will be to combine file skills and tidyverse skills

#1. pull in filenames from all human photos (Zooniverse > people_images)
#2. add in underscores to filenames where necessary, like before (will have to add to the existing code)
#3. count
#4. divide by 8
#5. make sure dates and sites match those in the zooniverse export that we are using human data for (right now we only have one export - 24oct22)


############################
# data import from filenames
############################
filenames_8 <- list.files("/Users/zoe/Library/CloudStorage/GoogleDrive-zilz@ucsb.edu/My Drive/SCHOOL/PHD_AT_UCSB/GaviotaCameraTrapping/Zooniverse/people_images",
                        include.dirs = FALSE,
                        full.names = FALSE,
                        all.files = FALSE)

##############################################################
# then clean and make summary table of human seconds per site
##############################################################

hooman_8 <- read.table(text = filenames_8)  %>% # turns filename char vec into dataframe
  distinct() %>%     # removes dups which happen when drive creates temp files
  filter(!row_number() %in% 1 ) %>%  #why is THIS THE ONLY WAY TO REMOVE ONE FUCKING ROW
  filter(!grepl("IMG", V1)) %>%  # i guess grepl() is an alternative to str_detect?? taking out all unnamed (oops)
  
  ## need to fix the fact that filenames are in two different formats because I'm an idiot and it's hard to batch fix shit like this outside of command line:
  mutate(V2 = str_replace_all(V1, c("bc17apr22" = "bc_17apr22",
                                    "bc28apr22" = "bc_28apr22",
                                    "cove17apr22" = "cov_17apr22",
                                    "gov17apr22" = "gov_17apr22", 
                                    "nvs28apr22" = "nvs_28apr22",
                                    "pb28apr22" = "pb_28apr22",
                                    "pl17apr22" = "pl_17apr22",
                                    "pp17apr22" = "pp_17apr22",
                                    "gc28apr22" = "gc_28apr22",
                                    "pd28apr22" = "pd_28apr22",
                                    "boatcam17apr22" = "boat_17apr22")
  )
  ) %>%  # not sure why this is the syntax but for str_replace for a dataframe this is what works?
  
  separate(V2, c("site", "date", "photonum"), sep = "_", remove = FALSE) %>%  # separates filename into site/date identifier and orig photo number
  mutate(photonum = str_remove(photonum, ".JPG")) %>%   # remove the the .JPG from photonum
  mutate(photonum = as.numeric(photonum)) %>% 
  arrange(site, date, photonum) %>% 
  select(site, date, photonum)

#we can ignore the below, even though it would make things slightly easier. later we can just divide human seconds by 8

#mutate(imgnum = rep_len(1:8, nrow(.))) %>%  # adds a column repeating 1:8 # CAROLINE SAYS YOU NEED THE DOT I HATE HER
#mutate(subject = rep(1:(nrow(.)/8), each = 8)) %>%   # adds a column linking all images related to one subj together
### now need to expand the dataframe long ways so that 1:8 are 8 different columns
# need to reorder the dataframe by sitedate and THEN photonum
#select(!photonum) %>% # have to take photonum out because it adds a unique identifier to each photo which messes up pivot_wider
#pivot_wider(names_from = imgnum, values_from = V1, names_prefix = "image") # creates a new column (image1-image8) for each of the 8 images per subject (V1 is the col of filenames)


# get a nice summary table of counts of humans per site

hooman_count_8 <- hooman_8 %>% 
  count(site, date) %>% 
  mutate(seconds = n/8)

############################
############################
#     cameras w 10 burst  ##
############################
############################


############################
# data import from filenames
############################
filenames_10 <- list.files("/Users/zoe/Library/CloudStorage/GoogleDrive-zilz@ucsb.edu/My Drive/SCHOOL/PHD_AT_UCSB/GaviotaCameraTrapping/Zooniverse/people_images/10",
                           include.dirs = FALSE,
                           full.names = FALSE,
                           all.files = FALSE)

##############################################################
# then clean and make summary table of human seconds per site
##############################################################

hooman_10 <- read.table(text = filenames_10)  %>% # turns filename char vec into dataframe
  distinct() %>%     # removes dups which happen when drive creates temp files
  filter(!row_number() %in% 1 ) %>%  #why is THIS THE ONLY WAY TO REMOVE ONE FUCKING ROW
  filter(!grepl("IMG", V1)) %>%  # i guess grepl() is an alternative to str_detect?? taking out all unnamed (oops)
  
  separate(V1, c("site", "date", "photonum"), sep = "_", remove = FALSE) %>%  # separates filename into site/date identifier and orig photo number
  mutate(photonum = str_remove(photonum, ".JPG")) %>%   # remove the the .JPG from photonum
  mutate(photonum = as.numeric(photonum)) %>% 
  arrange(site, date, photonum) %>% 
  select(site, date, photonum) %>%   
  mutate(site = case_when(site == "coj" ~ "coja",
                          site == "vrizn" ~ "rizn",
                          site == "vrizs" ~ "rizs",
                          site == "wall" ~ "sea",
                          site == "vnwc" ~ "nwc",
                          .default = as.character(site)
  )
  )


#we can ignore the below, even though it would make things slightly easier. later we can just divide human seconds by 8

#mutate(imgnum = rep_len(1:8, nrow(.))) %>%  # adds a column repeating 1:8 # CAROLINE SAYS YOU NEED THE DOT I HATE HER
#mutate(subject = rep(1:(nrow(.)/8), each = 8)) %>%   # adds a column linking all images related to one subj together
### now need to expand the dataframe long ways so that 1:8 are 8 different columns
# need to reorder the dataframe by sitedate and THEN photonum
#select(!photonum) %>% # have to take photonum out because it adds a unique identifier to each photo which messes up pivot_wider
#pivot_wider(names_from = imgnum, values_from = V1, names_prefix = "image") # creates a new column (image1-image8) for each of the 8 images per subject (V1 is the col of filenames)


# get a nice summary table of counts of humans per site

hooman_count_10 <- hooman_10 %>% 
  count(site, date) %>% 
  mutate(seconds = n/10)

# combine the two, still need to clean up some naming errors, like gov was what we used to call big cojo
## gov in april, may, sep is big
## gov in nov is gov2
## perb needs to be pb

human_activity <- rbind(hooman_count_10, hooman_count_8) %>% 
  mutate(site = case_when(site == "gc2" | site == "gov2" ~ "Government Point",
                         site == "gc" ~ "Big Cojo",
                         site == "cov" | site == "Cova" ~ "Cove", 
                         site == "gov" & date == "12nov22" ~ "Government Point",
                         .default = as.character(site))) %>% 
  mutate(site = case_when(site == "gov" | site == "big" ~ "Big Cojo", # now rename em all so they look nice and match stuff
                          site == "coca" ~ "Cojo Canyon",
                          site == "coga" ~ "Cojo Gate",
                          site == "coja" ~ "Cojalama",
                          site == "jal" ~ "Jalama",
                          site == "jal2" ~ "Jalama 2",
                          site == "nbc" ~ "North Beach Canyon",
                          site == "nvs" ~ "North Vista Spring",
                          site == "nwc" | site == "vnwc" ~ "Not Water Canyon",
                          site == "bone" ~ "Boneyard",
                          site == "riznback" | site == "vriznback" ~ "RIZ N Backup",
                          site == "sea" ~ "Seawall",
                          site == "vj" ~ "Jolluru",
                          site == "vlad" ~ "Ladrones",
                          site == "vmor" ~ "Morida",
                          site == "vrizs" | site == "rizs" ~ "RIZ South",
                          site == "vrizn" | site == "rizn" ~ "RIZ North",
                          site == "vshc" ~ "Short Canyon",
                          site == "vsito" ~ "Saucito",
                          site == "bc" ~ "Black Canyon",
                          site == "boat" ~ "Percos Boat",
                          site == "dam" ~ "Damsite",
                          site == "fort" ~ "North Beach Fort",
                          site == "pb" | site == "perb" ~ "Percos Beach",
                          site == "pd" ~ "Percos Driftwood",
                          site == "pl" ~ "Percos Log",
                          site == "pp" ~ "Percos Post",
                          site == "vbh" ~ "Boathouse",
                          site == "vof" ~ "Old Fencepost",
                          site == "vsc" ~ "Sudden Canyon",
                          site == "wood" ~ "Wood Canyon",
                          site == "wc" | site == "vwc" ~ "Water Canyon",
                            
                         .default = as.character(site))) #just making sure this is on a second line so it runs after the previous

write_csv(human_activity, "human_activity.csv")

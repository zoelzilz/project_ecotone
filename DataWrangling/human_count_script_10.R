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
filenames <- list.files("/Users/zoe/Library/CloudStorage/GoogleDrive-zilz@ucsb.edu/My Drive/SCHOOL/PHD_AT_UCSB/GaviotaCameraTrapping/Zooniverse/people_images/10",
                        include.dirs = FALSE,
                        full.names = FALSE,
                        all.files = FALSE)

##############################################################
# then clean and make summary table of human seconds per site
##############################################################

hooman <- read.table(text = filenames)  %>% # turns filename char vec into dataframe
  distinct() %>%     # removes dups which happen when drive creates temp files
  filter(!row_number() %in% 1 ) %>%  #why is THIS THE ONLY WAY TO REMOVE ONE FUCKING ROW
  filter(!grepl("IMG", V1)) %>%  # i guess grepl() is an alternative to str_detect?? taking out all unnamed (oops)
  
  separate(V1, c("site", "date", "photonum"), sep = "_", remove = FALSE) %>%  # separates filename into site/date identifier and orig photo number
  mutate(photonum = str_remove(photonum, ".JPG")) %>%   # remove the the .JPG from photonum
  mutate(photonum = as.numeric(photonum)) %>% 
  arrange(site, date, photonum) %>% 
  select(site, date, photonum) %>%   
  mutate(site = case_when(site == "coj" ~ "coja",
                          site == "rizn" ~ "vrizn",
                          site == "rizs" ~ "vrizs",
                          site == "wall" ~ "sea",
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

hooman_count <- hooman %>% 
  count(site, date) %>% 
  mutate(seconds = n/10)

write_csv(hooman_count, "human_activity_10.csv")

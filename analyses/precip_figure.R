####### Generating Precipitation Time Series Figure #######
## 15 Sep 2024 ##
## Data from PRISM: https://prism.oregonstate.edu/explorer/ ##

#### libraries ####
library(tidyverse)
library(here)
library(janitor)
library(lubridate)

#### import data ####

### precipitation data from the temporal extent of study
monthly_precip_ecotone <- read_csv(here("data/PRISM_ppt_tmean_stable_4km_202203_202304_34.4613_-120.4624.csv")) %>% 
  slice_head(n = 14) %>% # only first 13 rows have data
  clean_names() #%>% 
  #mutate(month = month(ym(date), label = TRUE, abbr = FALSE)) # need a character month column
  

### 30 year averages

## to bind these two dataframes, we will need to create a dummy date with the same year as the other df
## DID THIS BY HAND

monthly_30ynorm <- read_csv(here("data/PRISM_ppt_tmean_stable_4km_monthly_normals_34.4613_-120.4624_edited.csv")) %>% 
  slice_head(n = 15) %>% #  first 15 rows have data
  clean_names() %>% 
  filter(date != "Annual") %>% select(-x5) %>% 
  rename(y30norm = ppt_inches)


all_precip = full_join(monthly_30ynorm, monthly_precip_ecotone, 
                       by = join_by(dummy_date == date)) # joining by telling it that date and dummy date are same

#### make figures ####

## first just check out precip during study
ecotone_precip <- ggplot(monthly_precip_ecotone, aes(x = date, y = ppt_inches, group = 1)) +
  geom_line()

## now figure for manuscript

precip <- all_precip %>% 
  ggplot(., aes(x = dummy_date, y = y30norm, group = 1))+
  geom_line(color = "dodgerblue", linetype = 2)+
  geom_line(aes(x = dummy_date, y = ppt_inches, group = 1), color = "red")+
  xlab("Month Sampled")+
  ylab("Precipitation (inches)")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1, size = 7),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 10)
        )
  
#png(here("figures/monthly_precip.png"), width = 10, height = 8, units = "in", pointsize = 12, res = 1000)
#precip
#dev.off() 
  
  
  
########## GRAVEYARD ############
## we could do this by hand but I hate myself so here we go

# function to convert month to dummy date
#name_to_date <- function(x) {
#  lubridate::mdy(ifelse(x %in% c(month.name, month.abb), paste0(x, "/01/2022"), x)) #appends a day and year to any lubridate object that is month name or abbreviation
#}

#monthly_30ynorm <- read_csv(here("data/PRISM_ppt_tmean_stable_4km_monthly_normals_34.4613_-120.4624.csv")) %>% 
#  slice_head(n = 13) %>% # only first 13 rows have data
#  clean_names()%>% 
#  rename(month = date) %>%   # need a character month column
#  filter(month != "Annual") %>% # we don't need annual 30y norm
#mutate(month = fct_relevel(month, c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", 
#                                    "December"))) %>% 
#  mutate(dummy_date = name_to_date(month)) # applies function to convert to ymd but then just keeps ym
  
  
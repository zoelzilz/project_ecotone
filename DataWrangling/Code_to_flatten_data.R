library(tidyjson)
library(magrittr)
library(jsonlite)
library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)

#This project has subquestions that vary by species, including a howmany and select-all behaviors
jdata_unfiltered <- read.csv(file = "parasite-safari-classifications_06_02_20.csv", stringsAsFactors = F)


jdata_unfiltered <- read.csv(file = "parasite-safari-classifications_4_26_19.csv", stringsAsFactors = F)

# So, need to limit to final workflow version and ideally split by task. 
jdata_unfiltered %>% mutate(., created_at = ymd_hms(created_at)) %>% 
  group_by(., workflow_id, workflow_version) %>% summarise(., max(created_at), n()) %>% View

jdata <- jdata_unfiltered

############### SURVEY TASK
head(jdata)
dim(jdata)

## subset for parasite safari, not needed for sagehen
jdata2 <- subset(jdata, workflow_id == 13216) # Mpala survey only


## subset
jdata2 <- subset(jdata, workflow_id == 8574 | workflow_version > 100)
dim(jdata2)
head(jdata2)

# 61 for psafari
for (i in 1:14) {
  jdata2$annotations[i] %>% prettify %>% print
}

# preliminary flat

basic_flat_with_values <- jdata2 %>% 
  dplyr::select(., subject_ids, classification_id, workflow_version, annotations) %>%
  as.tbl_json(json.column = "annotations") %>%
  gather_array(column.name = "task_index") %>% # really important for joining later
  spread_values(task = jstring("task"), task_label = jstring("task_label"), value = jstring("value")) 

View(basic_flat_with_values)

basic_summary <-  basic_flat_with_values %>% 
  gather_keys %>%
  append_values_string()

basic_summary %>% View # this will have all the classification IDs; if Value is empty, then the field will be null. This will have multiple rows per classification if there are multiple tasks completed

basic_summary %>% data.frame %>% group_by(., workflow_version, key, task) %>% summarise(., n())

# quick check the filtered original data
jdata2 %>% summarise(., n_distinct(subject_ids), n_distinct(classification_id), n_distinct(workflow_version))

#--------------------------------------------------------------------------------#
# split into survey vs. non-survey data frames. Question is flattened and can be exported as a separate file now.
survey <- basic_flat_with_values 


###----------------------------### SURVEY FLATTENING ###----------------------------### 

# grab choices; append embedded array values just for tracking
with_choices <- survey %>% enter_object("value") %>% json_lengths(column.name = "total_species")%>% 
  gather_array(column.name = "species_index") %>% spread_values(choice = jstring("choice")) 
  


# if there are multiple species ID'd, there will be multiple rows and array.index will be >1
with_choices %>% View
with_choices %>% summarise(., n_distinct(subject_ids), n_distinct(classification_id))
with_choices %>% group_by(., classification_id) %>% summarise(., count = n(), max(species_index)) %>% arrange(., -count)


### FLATTEN WITH SUBQUESTIONS. NEED INPUT HERE.
#Single Choice Qs
# only count string needed for sagehen
count_string <- "HOWMANYTOTAL"
grazing2 <- "HOWMANYGRAZING"
grazing1 <- "HOWMANYGRAZINGMOUTHTOUCHINGGROUND"
drinking <- "HOWMANYDRINKING"

#Multi-Choice Qs
#behavior_string <- "WHTBHVRSDS"



# grab answers - for some reason, this keeps rows even if there are no answers! 
# Note that this last bit is the part that would need to be customized per team, I think

with_answers_list <- with_choices %>%  #annoyingly, there's no easy way to unlist these. hence the hoop jumping in the next block.
  # only count_string for sagehen
  enter_object("answers") %>% 
  spread_values(how_many = jstring(count_string), 
                grazing2 <- jstring(grazing2),
                grazing1 = jstring(grazing1), 
                drinking = jstring(drinking))# , 
               # behavior = jstring(behavior_string))
#use for psafari
names(with_answers_list)[12] <- "grazing2"

# use for psafari only
with_answers <- with_answers_list %>%
  mutate(grazing = coalesce(grazing1, grazing2))

with_answers <- with_answers[,-c(12,13)]



## I don't need this part because I don't have multi choice questions
# spread answers (into separate columns): have to drop behavior index or else the rows won't combine!
#with_answers_spread <- with_answers %>% data.frame %>% 
#  select(., -behavior_index) %>%
#  mutate(., behavior_present = 1) %>%
#  spread(., key = behavior, value = behavior_present, fill = 0)

# spread answers (into a list)
#test <- with_answers %>% data.frame %>% 
#  select(., -behavior_index) %>% nest(behavior)


with_answers %>% View
#with_answers_spread %>% View
#From here on, the original code used with_answers_spread, but I replaced with with_answers
with_answers %>% summarise(., n_distinct(subject_ids), n_distinct(classification_id))

# the number of rows where count >1 should be the same as the difference between the row count for add_counces and basic_flat
with_answers %>% group_by(classification_id) %>% summarise(., count = n()) %>% arrange(., -count) %>% View   

# in theory, you want to tie all of these back together just in case there are missing values
add_choices <- left_join(survey, with_choices)
#use with_answers_list for sagehen
tot <- left_join(add_choices, with_answers)
flat_data <- tot %>% dplyr::select(., -task_index, -task_label, -value)

#check that the number of distinct subject IDs and classification IDs is still the same
flat_data %>% summarise(., n_distinct(subject_ids), n_distinct(classification_id), n()) #flattened,
jdata2 %>% summarise(., n_distinct(subject_ids), n_distinct(classification_id), n()) #original

View(flat_data)


flat_data %>% group_by(subject_ids) %>% summarize(count=n()) %>% ggplot(aes(x=count))+geom_histogram(binwidth = 1)

#write.csv(flat_data, file="flattened_mpala_survey_june2020.csv")

# this only needed for Sagehen:
flat_data$how_many2 <- NULL
for(i in 1:length(flat_data$how_many)){
  flat_data$how_many2[i]= scan(text=flat_data$how_many[i],sep='"',what="", quiet=T)[2]
  
}
flat_data$how_many2 <- as.numeric(flat_data$how_many2)
View(flat_data)


#write.csv(flat_data, file = "sagehen_flat_feb20.csv")

#write.csv(flat_data, file = "test_flattened_4_26_2019.csv")

# So this really only works with the latest workflow (118.18). And even then, I need to change the numerals...or figure out how to designate a range? Oh wait, no, I think I can back-convert that later. Regarding the stats, I guess I would have to model that carefully in a certain way... But the cost of having people count all the animals in every single picture is challenging.

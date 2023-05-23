rm(list = ls())

library(tidyjson)
library(magrittr)
library(jsonlite)
library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)

source(file = "DataWrangling/flattening_script.R") 

############## INTERACTIVE - CLEANING OF CLASSIFICATION DATA AND SPECIFYING FIELDS ###################

# You will need to define variables that will be used in the run_json_parsing function. They need the names as below.

# CHANGE THESE VARIABLES: You NEED TO DEFINE THESE each time you are dealing with a new data export or the script could break.
# jdata <- "character" (defined below, is the original unflat data export from zooniverse)
#survey_id <- "T0" #character, usually starts with T, can be found in the "annotations col of the unflat dataset before and is the name zooniverse gives your survey Q
#workflow_id_num <- 20748 #numeric, will change
#workflow_version_num <- 84.11 #numeric (e.g. 45.01). will change, you need to include the entire version (even if it's 45.00)
## we're starting with 84.11 because it has the most classifications


# THESE VARIABLES SHOULD NOT CHANGE UNLESS YOU ADD NEW QUESTIONS OR ANSWERS
#single_choice_Qs <- "HOWMANY" #"character" or c("character", "character")
#single_choice_colnames  <- "count" #"character" or c("character", "character")
#multi_choice_Qs  <- C("WHATBEHAVIORSDOYOUSEESELECTALLTHATAPPLY", "WHATTIMEOFDAYISIT") #"character" or c("character", "character")
#multi_choice_colnames <- c("behavior", "tod") #"character" or c("character", "character")
  
  


# Specify Project
project_name <- "ECOTONE"
classifications_file <- "data/project-ecotone-classifications_13oct22.csv"


# Examine data
jdata <- read.csv(classifications_file, stringsAsFactors = F)

# Set project-specific details
survey_id <- "T0"
check_workflow(jdata) %>% 
  View
workflow_id_num <- 20748 #numeric, will change
workflow_version_num <- 84.11

# limit to relevant workflow id and version
# alternatively, I think you can ignore this step if you want to aggregate multiple versions, as long as 
# ... they have the same questions?
jdata <- jdata %>% filter(., workflow_id == workflow_id_num, workflow_version == workflow_version_num)

# Identify task-specific details. These variable names are important, because I haven't figured out how to define them in the function call 
# (there's some weird referencing. I don't know. The function definitions and scripts could be improved, but things seem to generally work.)
# how these need to be chosen and formatted are defined in frontmatter above
View_json(jdata)
single_choice_Qs <- "HOWMANY" 
single_choice_colnames  <- "count" 
multi_choice_Qs  <- c("WHATBEHAVIORSDOYOUSEESELECTALLTHATAPPLY", "WHATTIMEOFDAYISIT") 
multi_choice_colnames <- c("behavior", "tod") #tod is time of day



# Flatten by calling the code from the flattening_functions file. This isn't the cleanest approach, but it'll have to do.
# If you want to combine multiple workflows or multiple tasks before aggregating, this is the time to do it. 
# HOW DO I COMBINE MULTIPLE WORKFLOWS?!
final_data <- run_json_parsing(data = jdata) # currently not working - error in coercing list to character somewhere

View(final_data)
write.csv(final_data, file = paste0(classifications_file, "-flattened.csv"), row.names = F)


### The Data out will be in the following format: 
# Metadata
# Species (Choice)
# single choice questions
# multiple choice questions, with the colnames prepended to the values

# For example:
# subject_ids: unique Zooniverse subject identifier. you will need to link this back to your primary key via subject metadata.
# user_name: registered user name or "not-logged-in-hash" where hash is the user's hashed IP address
# classification_id: a unique key representing that classification. This will be unique to the user and subject, but can encompass multiple tasks
# workflow_version: the major and minor version of the workflow
# task_index: an index of tasks. Usually will be 1 if this is your only task.
# task: the task identifier, e.g. "T0"
# task_label: <NA> for survey tasks, but for follow-up questions, this would be the text of the question itself
# value: the annotation data in an embedded list (that is saved as a character). This is really just for double checking against.
# total_submissions: The total number of submissions a user made for a give species/choice. So, if they said lion, 1, standing and leopard, 1, standing, this = 2.
# submission_index: Reflects the index of the particular choice. Not really important.
# choice: Your species choice. NOTE that if you have multiple workflow versions and change species names, you'll need to reconcile these.
# how_many: note that this is not actually a NUMBER, and be careful that you don't treat it as one, especially if you have ranges like 3-5 that get saved as 35.
# behavior_EATING: Every possible answer for a "select all that apply" question gets it's own column, so that you can calculate the proportion of users who marked them present.
# behavior_INTERACTING
# behavior_MOVING
# behavior_RESTING
# behavior_STANDING
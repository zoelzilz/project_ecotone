######### script to turn camera trap data into list of matrices for iNEXT ##########

### packages
library(tidyverse)
library(car)
library(ggpubr)
library(ggprism) # paste pvalues
library(geosphere)
library(lubridate)
library(hms)
library(janitor)
library(kableExtra)
library(formattable)
library(webshot2)
library(vegan)
library(here)

### raw data

# starting from clean df of every day sampled, made in markdowns
mammals_df_orig <- read_csv(here("data/activity_day_matrix.csv"))

mammals_df <- mammals_df_orig %>% 
  # need to first convert all counts in cells to 0s and 1s
  mutate(across(starts_with("sp"), ~case_when(. == 0 ~ 0,
                                              TRUE ~ 1))) %>%  # make everything a 1 unless it's a zero
  # there are one or two duplicates of dates within site month, so lets take care of those:
  distinct(sitemonth_id, date, .keep_all = TRUE)

### make the data into a nice, clean matrix in the shape we want
## needs a column for sitemonth_id (for splitting later), columns for each day sampled, and rownames that are species names
mammals_matrix <- mammals_df %>% 
  select(sitemonth_id, date, starts_with("sp")) %>% 
  #column_to_rownames(var = "sitemonth_id") %>% duplicate rownames aren't allowed, so much of this is going to have to be done downstream on the list of matrices ugh
  #as.matrix() 
  as.data.frame()
  

## can't do any pivoting yet because each sitemonth has a different number of rows, need to split them first
  
### split up the giant df into one df per "sample" = site x month or sitemonth_id, creates a list of dfs!

mammals_list_df <- split.data.frame(mammals_matrix, mammals_matrix$sitemonth_id) 


mammals_list_1 <- purrr::map(.x = mammals_list_df, ~ as.matrix(.x[-c(1:2)])) # best way i found to do this is to just remove first two columns (sitemonth and date) and just let the index be the rowname

### split up the giant matrix into one matrix per "sample" = site x month or sitemonth_id, creates a list of matrices!

#splitter <- mammals_matrix[,'sitemonth_id']

#mammals_list_1 <- split.data.frame(mammals_matrix, splitter)

#mammals_list_1

### now we need to transform these matrices a bit... 
## - remove the first column ()
#mammals_list_2 <- lapply(mammals_list_1, "[", TRUE, -c(1)) 
# syntax: apply what's after the comma to all in mammals_list_1, (no idea what the bracket in quotes means), "TRUE" as the first argument to get all of the rows followed by negative indexing to remove the columns

## - change the date column into rownames (column_to_rownames don't work bc it's not a df)
#mammals_list_3 <- lapply(mammals_list_2, function(x){rownames(x) <- x[,1]; x}) 

## - transpose

inext_sitemonth_incraw <- purrr::map(.x = mammals_list_1, ~ t(.x)) # only way to get this to work is to map t() transpose across the whole list

inext_sitemonth_incfreq <- purrr::map(.x = inext_sitemonth_incraw, ~rowSums(.x))

### Kayla's code to get the correct form of matrix for frequency:
m_in <- list(matrix(sample(1:100, 100, replace=T), nrow=20, ncol=5),
             matrix(sample(1:100, 200, replace=T), nrow=20, ncol=10))
m_in

m_out <- lapply(m_in, function(x) {
  mc = ncol(x)
  rs = rowSums(x)
  return(c(mc, rs))
})       

m_out
do.call(rbind, m_out)


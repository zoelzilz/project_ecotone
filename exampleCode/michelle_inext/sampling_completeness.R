### Interaction sampling completeness across islets ###
# created by M. Lee 24 February 2021
# the purpose of this code is to better understand the completeness of sampling of plant-pollinator interactions across islets





# This code does the following ---------------------------------------
# organize data for analyses
# rarefaction curve
# species accumulation curve
# iNEXT versions of species accumulation curves




# Libraries and working directory  -----------------------

library(iNEXT)
library(tidyverse)
library(gridExtra)
library(grid)
library(vegan)


# set working directory

setwd("~/Desktop/palmyra_Rproject/Network_Analyses/11_createnetworks_code/")
source("../11_createnetworks_code/interaction_matrix_function.R")
setwd("~/Desktop/palmyra_Rproject/Network_Analyses/01_cleanData/")


# Main csv files for manipulation ------------------------

int17 <- readRDS("interactions2017_cleaned.rds") %>% 
  mutate(year = 2017)
int19 <- readRDS("interactions2019_cleaned.rds") %>% 
  mutate(year = 2019)





# Format data for both years -------------------------------

# what are the columns that we need for this analysis
# islet, waypoint, poll_species

ready17 <- int17 %>% 
  dplyr::select(year, islet, waypoint, trial_time, poll_species)

ready19 <- int19 %>% 
  dplyr::select(year, islet, waypoint, trial_time, poll_species)

bothyears <- rbind(ready17, ready19) %>% 
  filter(poll_species != "NA",
         poll_species != "Diptera sp") %>% 
  mutate(poll_species_ = str_replace(poll_species, " ", "_"))












# WRANGLE | whole atoll | both years | vegan(species accumulation curve) -------------------------------

# format dataset for matrix
spac_df <- unite(bothyears, "site", c("year","islet", "waypoint", "trial_time"), remove = TRUE, na.rm = TRUE)

# make site matrix for specaccum
spac_matrix <- as.data.frame(df2intmatrix(spac_df, varnames = c("site", "poll_species"), type.out = "array"))


# species accumulation for whole atoll
spac1 <- specaccum(spac_matrix)

# create dataframe from species accumulation to use in ggplot
spac1_df <- data.frame(ObservationSites=spac1$sites, SpeciesRichness=spac1$richness, SD=spac1$sd)





# WRANGLE | whole atoll | 2017 | vegan(species accumulation curve) -------------------------------



# format dataset for matrix
spac_df17 <- unite(ready17, "site", c("year","islet", "waypoint", "trial_time"), remove = TRUE, na.rm = TRUE)

# make site matrix for specaccum
spac_matrix17 <- as.data.frame(df2intmatrix(spac_df17, varnames = c("site", "poll_species"), type.out = "array"))

# species accumulation for whole atoll
spac17 <- specaccum(spac_matrix17)

# create dataframe from species accumulation to use in ggplot
spac17_df <- data.frame(ObservationSites=spac17$sites, SpeciesRichness=spac17$richness, SD=spac17$sd)






# WRANGLE | whole atoll | 2019 | vegan(species accumulation curve) -------------------------------


# format dataset for matrix
spac_df19 <- unite(ready19, "site", c("year","islet", "waypoint", "trial_time"), remove = TRUE, na.rm = TRUE)

# make site matrix for specaccum
spac_matrix19 <- as.data.frame(df2intmatrix(spac_df19, varnames = c("site", "poll_species"), type.out = "array"))

# species accumulation for whole atoll
spac19 <- specaccum(spac_matrix19)

# create dataframe from species accumulation to use in ggplot
spac19_df <- data.frame(ObservationSites=spac19$sites, SpeciesRichness=spac19$richness, SD=spac19$sd)






# PLOT | whole atoll | ggplot | vegan (species accumulations curves) --------------------------



cols <- c("Both years"="black","2017"="darkgreen","2019"="blue")
whole_atoll_sampling <- ggplot() +
 
  # whole atoll | both years
  geom_point(data=spac1_df, aes(x=ObservationSites, y=SpeciesRichness, colour = "Both years")) +
  geom_ribbon(data=spac1_df ,aes(x=ObservationSites, ymin=(SpeciesRichness-2*SD),ymax=(SpeciesRichness+2*SD)),alpha=0.2) +
  
  # whole atoll | 2017
  geom_point(data=spac17_df, aes(x=ObservationSites, y=SpeciesRichness, colour = "2017")) +
  geom_ribbon(data=spac17_df ,aes(x=ObservationSites, ymin=(SpeciesRichness-2*SD),ymax=(SpeciesRichness+2*SD)),alpha=0.2, fill = "darkgreen") +
  
  # whole atoll | 2019
  geom_point(data=spac19_df, aes(x=ObservationSites, y=SpeciesRichness, colour = "2019")) +
  geom_ribbon(data=spac19_df ,aes(x=ObservationSites, ymin=(SpeciesRichness-2*SD),ymax=(SpeciesRichness+2*SD)),alpha=0.2, fill = "blue") +
 
  # formatting
  labs(x = "Observations Sites",
       y = "Species Richness") +
  scale_color_manual(name = "Sampling Year", values = cols)

whole_atoll_sampling


setwd("~/Desktop/2020PalmyraNetworks/Network_Analyses/figures")

ggsave(plot = whole_atoll_sampling,
       filename = "specaccum_whole_atoll.png",
       width = 6,
       height = 4,
       units = "in",
       dpi = 500)












# WRANGLE | plants | vegan(species accumulation curve) ---------------------



# pull each plant from interaction datasets
# what columns do we need? year, islet, waypoint, plant_species, poll_species

plant17 <- int17 %>% 
  select(year, islet, waypoint, trial_time, plant_species, poll_species)

plant19 <- int19 %>% 
  select(year, islet, waypoint, trial_time, plant_species, poll_species)

plantboth <- rbind(plant17, plant19)


# first make alist of all plant species in dataset
# make a matrix for each plant species
# main plant species: CN, HF, ST, PG

cn_df <- plantboth %>% 
  filter(plant_species == "Cocos nucifera") %>% 
  select(-plant_species) %>% 
  unite("site", c("year","islet", "waypoint", "trial_time"), remove = TRUE, na.rm = TRUE)
spac_matrix_cn <- as.data.frame(df2intmatrix(cn_df, varnames = c("site", "poll_species"), type.out = "array"))
spac_cn <- specaccum(spac_matrix_cn)
spac_cndf <- data.frame(ObservationSites=spac_cn$sites, SpeciesRichness=spac_cn$richness, SD=spac_cn$sd)


hf_df <- plantboth %>% 
  filter(plant_species == "Heliotropium foertherianum") %>% 
  select(-plant_species) %>% 
  unite("site", c("year","islet", "waypoint", "trial_time"), remove = TRUE, na.rm = TRUE)
spac_matrix_hf <- as.data.frame(df2intmatrix(hf_df, varnames = c("site", "poll_species"), type.out = "array"))
spac_hf <- specaccum(spac_matrix_hf)
spac_hfdf <- data.frame(ObservationSites=spac_hf$sites, SpeciesRichness=spac_hf$richness, SD=spac_hf$sd)


st_df <- plantboth %>% 
  filter(plant_species == "Scaevola taccada") %>% 
  select(-plant_species) %>% 
  unite("site", c("year","islet", "waypoint", "trial_time"), remove = TRUE, na.rm = TRUE)
spac_matrix_st <- as.data.frame(df2intmatrix(st_df, varnames = c("site", "poll_species"), type.out = "array"))
spac_st <- specaccum(spac_matrix_st)
spac_stdf <- data.frame(ObservationSites=spac_st$sites, SpeciesRichness=spac_st$richness, SD=spac_st$sd)


pg_df <- plantboth %>% 
  filter(plant_species == "Pisonia grandis") %>% 
  select(-plant_species) %>% 
  unite("site", c("year","islet", "waypoint", "trial_time"), remove = TRUE, na.rm = TRUE)
spac_matrix_pg <- as.data.frame(df2intmatrix(pg_df, varnames = c("site", "poll_species"), type.out = "array"))
spac_pg <- specaccum(spac_matrix_pg)
spac_pgdf <- data.frame(ObservationSites=spac_pg$sites, SpeciesRichness=spac_pg$richness, SD=spac_pg$sd)




# PLOT | plants | vegan(species accumulation curve) ------------------------------------



cols_plants <- c("Cocos nucifera"="skyblue","Heliotropium foertherianum"="purple","Scaevola taccada"="yellowgreen", "Pisonia grandis" = "darkred")
all_plants_sampling <- ggplot() +
  
  # Cocos nucifera | both years
  geom_point(data=spac_cndf, aes(x=ObservationSites, y=SpeciesRichness, colour = "Cocos nucifera")) +
  geom_ribbon(data=spac_cndf ,aes(x=ObservationSites, ymin=(SpeciesRichness-2*SD),ymax=(SpeciesRichness+2*SD)),alpha=0.2, fill = "skyblue") +
  
  # Heliotropium foertherianum | both years
  geom_point(data=spac_hfdf, aes(x=ObservationSites, y=SpeciesRichness, colour = "Heliotropium foertherianum")) +
  geom_ribbon(data=spac_hfdf ,aes(x=ObservationSites, ymin=(SpeciesRichness-2*SD),ymax=(SpeciesRichness+2*SD)),alpha=0.2, fill = "purple") +
  
  # Scaevola taccada | both years
  geom_point(data=spac_stdf, aes(x=ObservationSites, y=SpeciesRichness, colour = "Scaevola taccada")) +
  geom_ribbon(data=spac_stdf ,aes(x=ObservationSites, ymin=(SpeciesRichness-2*SD),ymax=(SpeciesRichness+2*SD)),alpha=0.2, fill = "yellowgreen") +
  
  # Pisonia grandis | both years
  geom_point(data=spac_pgdf, aes(x=ObservationSites, y=SpeciesRichness, colour = "Pisonia grandis")) +
  geom_ribbon(data=spac_pgdf ,aes(x=ObservationSites, ymin=(SpeciesRichness-2*SD),ymax=(SpeciesRichness+2*SD)),alpha=0.2, fill = "darkred") +
  
  # formatting
  labs(x = "Observation sites per plant species",
       y = "Species Richness") +
  scale_color_manual(name = "Plant Species", values = cols_plants)

all_plants_sampling





setwd("~/Desktop/2020PalmyraNetworks/Network_Analyses/figures")

ggsave(plot = all_plants_sampling,
       filename = "specaccum_plants.png",
       width = 8,
       height = 4,
       units = "in",
       dpi = 500)













# WRANGLE | whole islets | fossil(chao estimators) ----------------------------

# chao estimators approximate projected species richness or diversity
# chao1 makes this estimate based on vectors or matrices of abundance data (only)
# chao2 makes these estimates based on incidence data























# WRANGLE&PLOT | whole atoll -- split by year | iNEXT(rarefaction curves) -------------------------------

# "the first entry of each list must be the total number of sampling units" (Hsieh et al. 2016)



# compress the data for the two years
inext_df <- unite(bothyears, "site", c("year"), remove = TRUE, na.rm = TRUE)




### IMPORTANT iNEXT FORMATTING INFORMATION
# make site matrix for rarefaction curve with iNEXT
# key to this process: flip the axes of the matrix for 'abundance' data
# here, each row is a species
# each column is a site
inext_matrix <- as.data.frame(df2intmatrix(inext_df, varnames = c("poll_species", "site"), type.out = "array"))



# take matrix for 2017/2019 and give it to iNEXT
# with 'q = 0' we are calculating species diversity
out <- iNEXT(inext_matrix, q = 0, datatype = 'abundance')



# use ggiNEXT to graph the figure
iNEXTplot_bothyears <- ggiNEXT(out, type = 1)
ggiNEXT(out, type = 2)
ggiNEXT(out, type = 3)


setwd("~/Desktop/palmyra_Rproject/Network_Analyses/robustness_output/")
ggsave(plot = iNEXTplot_bothyears,
       filename = "iNEXTplot_bothyears.png",
       width = 8,
       height = 4,
       units = "in",
       dpi = 500)




# WRANGLE&PLOT | whole atoll | iNEXT(rarefaction curves) -------------------------------

# "the first entry of each list must be the total number of sampling units" (Hsieh et al. 2016)



# compress the data for the two years
bothyears_atoll_label <- bothyears %>% 
  mutate(site = "Palmyra")
  
  


### IMPORTANT iNEXT FORMATTING INFORMATION
# make site matrix for rarefaction curve with iNEXT
# key to this process: flip the axes of the matrix for 'abundance' data
# here, each row is a species
# each column is a site
inext_matrix_atoll <- as.data.frame(df2intmatrix(bothyears_atoll_label, varnames = c("poll_species", "site"), type.out = "array"))



# take matrix for 2017/2019 and give it to iNEXT
# with 'q = 0' we are calculating species diversity
out_atoll <- iNEXT(inext_matrix_atoll, q = 0, datatype = 'abundance')



# use ggiNEXT to graph the figure
iNEXTplot_wholeatoll <- ggiNEXT(out_atoll, type = 1) +
  theme_bw() +
  ylab("Species diversity across the atoll")
ggiNEXT(out_atoll, type = 2)
ggiNEXT(out_atoll, type = 3)


setwd("~/Desktop/2020PalmyraNetworks/Network_Analyses/figures")
ggsave(plot = iNEXTplot_wholeatoll,
       filename = "iNEXTplot_wholeatoll.png",
       width = 8,
       height = 4,
       units = "in",
       dpi = 500)







# WRANGLE&PLOT | by islet | iNEXT(rarefaction curves) -------------------------------


inext_islet_matrix <- as.data.frame(df2intmatrix(bothyears, varnames = c("poll_species", "islet"), type.out = "array"))
inext_islet_matrix <- inext_islet_matrix[, which(colSums(inext_islet_matrix) >= 100)]

out_islet <- iNEXT(inext_islet_matrix, q = 0, datatype = 'abundance')

iNEXTplot_islets_over100 <- ggiNEXT(out_islet, type=1)


setwd("~/Desktop/2020PalmyraNetworks/Network_Analyses/figures")
ggsave(plot = iNEXTplot_islets_over100,
       filename = "iNEXTplot_islets_over100.png",
       width = 8,
       height = 4,
       units = "in",
       dpi = 500)













# WRANGLE&PLOT | by plant species | iNEXT(rarefaction curves) -------------------------------

inext_plants_matrix <- as.data.frame(df2intmatrix(plantboth, varnames = c("poll_species", "plant_species"), type.out = "array"))
inext_plants_matrix <- inext_plants_matrix[, which(colSums(inext_plants_matrix) >= 30)]



out_plants <- iNEXT(inext_plants_matrix, q = 0, datatype = 'abundance')


iNEXTplot_plants_over30 <- ggiNEXT(out_plants, type=1)


setwd("~/Desktop/2020PalmyraNetworks/Network_Analyses/figures")
ggsave(plot = iNEXTplot_plants_over30,
       filename = "iNEXTplot_plants_over30.png",
       width = 8,
       height = 4,
       units = "in",
       dpi = 500)



# WRANGLE&PLOT | by islet subset (more) | iNEXT(rarefaction curves) -------------------------------
pollsp_df_more <- bothyears %>% 
  filter(islet == c("Cooper", "Eastern", "Sand", "Kaula", "Leslie"))

pollsp_df_more_matrix <- as.data.frame(df2intmatrix(pollsp_df_more, varnames = c("poll_species", "islet"), type.out = "array"))

out_islet_more <- iNEXT(pollsp_df_more_matrix, q = 0, datatype = 'abundance')

out_islet_more_plot <- ggiNEXT(out_islet_more, type=1, facet.var = "site")
out_islet_more_plot

setwd("~/Desktop/palmyra_Rproject/Network_Analyses/robustness_output")
ggsave(plot = out_islet_more_plot,
       filename = "iNEXT_polldiv_moreIslets.png",
       width = 10,
       height = 4,
       units = "in",
       dpi = 500)


# WRANGLE&PLOT | by islet subset (less) | iNEXT(rarefaction curves) -------------------------------

pollsp_df_less <- bothyears %>% 
  filter(islet == c("Lost", "Portsmouth", "Bunker", "Whippoorwill", "Fern"))

pollsp_df_less_matrix <- as.data.frame(df2intmatrix(pollsp_df_less, varnames = c("poll_species", "islet"), type.out = "array"))

out_islet_less <- iNEXT(pollsp_df_less_matrix, q = 0, datatype = 'abundance')

out_islet_less_plot <- ggiNEXT(out_islet_less, type=1, facet.var = "site")
out_islet_less_plot

setwd("~/Desktop/palmyra_Rproject/Network_Analyses/robustness_output")
ggsave(plot = out_islet_less_plot,
       filename = "iNEXT_polldiv_lessIslets.png",
       width = 10,
       height = 4,
       units = "in",
       dpi = 500)





# MATCH with 2016 data ---------

#1412 interactions to...
filtered.specList <- dplyr::filter(bothyears, 
                            bothyears$poll_species_ %in% full16$species_name)
#filtered down to 724

# unique species for Palmyra dataset:
unique(filtered.specList$poll_species_)
# only 18 shared pollinator species between the two datasets

# islets with less data
pollsp_df_less <- filtered.specList %>% 
  filter(islet == c("Lost", "Portsmouth", "Bunker", "Whippoorwill", "Fern"))

matchmatrix_less <- as.data.frame(df2intmatrix(pollsp_df_less, varnames = c("poll_species", "islet"), type.out = "array"))

match_less <- iNEXT(matchmatrix_less, q = 0, datatype = 'abundance')

matchless_plot <- ggiNEXT(match_less, type=1, facet.var = "site") +
  geom_hline(yintercept=18, linetype="solid", color = "red") +
  theme_bw(base_size = 12)+
  theme(legend.position="none")
matchless_plot

setwd("~/Desktop/palmyra_Rproject/Network_Analyses/robustness_output")
ggsave(plot = matchless_plot,
       filename = "iNEXT_polldiv_lessIslets_matched2016.png",
       width = 10,
       height = 2.75,
       units = "in",
       dpi = 500)







# islets with more data
pollsp_df_more <- filtered.specList %>% 
  filter(islet == c("Cooper", "Eastern", "Sand", "Kaula", "Leslie"))

matchmatrix_more <- as.data.frame(df2intmatrix(pollsp_df_more, varnames = c("poll_species", "islet"), type.out = "array"))

match_more <- iNEXT(matchmatrix_more, q = 0, datatype = 'abundance')

matchmore_plot <- ggiNEXT(match_more, type=1, facet.var = "site") +
  geom_hline(yintercept=18, linetype="solid", color = "red") +
  theme_bw(base_size = 12)+
  theme(legend.position="none")
matchmore_plot

setwd("~/Desktop/palmyra_Rproject/Network_Analyses/robustness_output")
ggsave(plot = matchmore_plot,
       filename = "iNEXT_polldiv_moreIslets_matched2016.png",
       width = 10,
       height = 2.75,
       units = "in",
       dpi = 500)

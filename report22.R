#2022 data analysis for report

#goals
#Bees
#rarefy?

#plants
#provide list of plants
#compare phenology and abundance both years

#temperature
#compare 21 and 22, include precip

#provide list of pollinator plants: need network data - plant-pol interactions - from linc spreadsheet

#NMDS of plant & pollinator commmunity - plant abundance/spp (transect list), bee abundance/spp

#tasks
#check names - plants & bees
rm(list = ls(all = TRUE))
library(tidyverse)

##### Bee spp info ----
#describe spp richness, compare 2022,2021,2020?
bees22<- read_csv() %>% 
  unite("BeeName", Genus:species, sep = " ", remove = F)

#check names with gbif
gbifnamesin<- allbees %>% summarise(scientificName = unique(BeeName))
write_csv(gbifnames, file = "beeNamesbgif.csv")

gbifnamesout <- read_csv("normalized.csv")

bees22 %>% left_join(gbifnamesout) 

allbees %>% group_by(Start_Year) %>% summarise(species = n_distinct(BeeName), genera = n_distinct(Genus))

genusList <- allbees %>% 
  group_by(Genus) %>% 
  summarise()
sppList <- allbees %>% 
  group_by(BeeName) %>% 
  summarise()

##### Separating morphospp ----
morSpp<- allbees %>% filter(grepl("sp." , x = allbees$BeeName))

fullSpp <- allbees %>% anti_join(morSpp)

morphList <- morSpp %>% 
  group_by(BeeName) %>% 
  summarise()
morGen <- morSpp %>% 
  group_by(Genus) %>% 
  summarise()
#species lists
fsppList <- fullSpp %>% 
  group_by(BeeName) %>% 
  summarise()
fsppGen <- fullSpp %>% 
  group_by(Genus) %>% 
  summarise()
  
##### Recommended pollinator plants ----
#put into code for leithen models

##### 


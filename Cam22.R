rm(list = ls(all = TRUE))
library(tidyverse)
library(lubridate)
library(forcats)

file.choose()
# Names ----
names <- read_csv("C:\\Users\\Rowan\\Desktop\\Currently Using\\Most up to date data\\Plant Names\\Cam22PlantList.csv")
code2name <- select(names, Code, verbatimScientificName,natinv) %>% # only scientific name, codes, 
mutate(plantspecies = verbatimScientificName) %>% 
  rename(plantsppCode=Code)

tomerge<- code2name %>% select(plantsppCode,plantspecies,natinv)

sitenames <- read_csv("Sites22.csv")

# Floral abundance ----
#2022 floral abundance
cam22flor <- read_csv("C:\\Users\\Rowan\\Desktop\\Currently Using\\Most up to date data\\Floral Abundance\\Cam22Transect.csv") %>% 
  mutate(across(ACHIMIL:VIOLARV, ~ replace_na(.x, 0)))

cam22flor<- pivot_longer(cam22flor, cols = ACHIMIL:VIOLARV) %>% 
  rename(stems = value) %>%
  left_join(tomerge, by = c("name" = "plantsppCode")) # add names

categFlow22<- filter(cam22flor, stems<=0.99, stems>=0.01) #find which were counted using presence absence
categname22<- unique(categoricalFlowers$name) #get names
categNames22<- as.data.frame(categname) %>% rename(name = categname)

catflow22 <- cam22flor %>% semi_join(.,categNames22, by = "name") #separate counts and categorical
countflow22 <- cam22flor %>% anti_join(.,categNames22, by = "name")

nrow(catflow22)+nrow(countflow22) == nrow(cam22flor)
#2021 floral abundance
cam21florwide <- read_csv("C:\\Users\\Rowan\\Desktop\\Currently Using\\Most up to date data\\Floral Abundance\\Cam21Transect.csv")

cam21flor <- pivot_longer(cam21florwide, cols = ACHIMIL:VEROOFF) %>%  #move from wide to long, only for the plant columns
  pivot_wider(., names_from = "Count", values_from = "value") %>%  #make stems and flowers their own columns
  left_join(tomerge,by = c("name"="plantsppCode")) #add names

categFlow21<- filter(cam21flor, stems<=0.99, stems>=0.01) #find names of categorical
categname21<- unique(categoricalFlowers$name) #get names
categNames21<- as.data.frame(categname) %>% rename(name = categname) #

catflow21 <- cam21flor %>% semi_join(.,categNames21, by = "name") #separate count and categorical
countflow21 <- cam21flor %>% anti_join(.,categNames21, by = "name")

nrow(catflow21)+nrow(countflow21) == nrow(cam21flor) #verification
#2022 bees----

bees22 <- read_csv("C:\\Users\\Rowan\\Desktop\\Currently Using\\Most up to date data\\Bee Data\\RowanKNPS2022CamasBeesRRID.csv") %>% 
  filter(!is.na(Genus)) %>% 
  unite("BeeName", Genus:species, sep = " ", remove = F)

  bees22$BeeName<- bees22$BeeName %>% str_remove("NA") %>% 
  str_trim()

bees22 %>% group_by(BeeName) %>% 
  summarise(n = n()) %>% arrange(desc(n))

bees22 %>% 
  group_by(Locality) %>% 
  summarise(n_distinct(BeeName))

bees22 %>% 
  filter(AssociatedTaxa == "Camassia quamash") %>% 
  group_by(Locality) %>% 
  summarise(n_distinct(BeeName))
 
  



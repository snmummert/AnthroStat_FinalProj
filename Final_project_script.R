##########################################
############# Final Proj #################
########### Sophia Mummert ###############
############## 3/4/26 ####################
##########################################


#load libraries#
library(readxl)
library(dplyr)


#import datasets#
NVBP_bintervention_feeding = read_excel("NVBP_Bintervention_Feeding.xlsx", 
                                       sheet = "Focal_Feeding")
demography_table = read_excel("NV_DemographyTable.xlsx", 
                              sheet = "nv_demography_table", 
                              skip = 1, col_names = TRUE)

#Cleaning DF#
NVBP_bintervention_feeding = NVBP_bintervention_feeding %>%
  select(Day, Month, Year, Date, Focal_ID, Start_time, End_time, 
         Food_ID, Food_part, Context_and_comments)

demography_table = demography_table %>%
  select(Name, Code, Sex, NV_Original, `alive_now?`, age_categeory_first_seen)

names(demography_table)[names(demography_table) == "alive_now?"] <- "Status"
names(demography_table)[names(demography_table) == "age_categeory_first_seen"] <- "Age_Cat"

#explore#
focal_list = NVBP_bintervention_feeding %>%
  group_by(Focal_ID) %>%
  summarize(n = n()) %>%
  arrange(desc(n))

alive = demography_table %>%
  filter(Status != "dead")

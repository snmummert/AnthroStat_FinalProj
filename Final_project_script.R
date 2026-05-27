##########################################
############# Final Proj #################
########### Sophia Mummert ###############
############## 3/4/26 ####################
##########################################


#load libraries#
library(readxl)
library(dplyr)
library(hms)
library(tidyverse) 
library(readxl) 
library(ggplot2)
library(lubridate)
library(stringr)
library(lme4)
library(lmerTest)


#import datasets#
bintervention = read_excel("NVBP_Bintervention_Feeding.xlsx", 
                                       sheet = "Focal_Feeding")
binter_focal = read_excel("NVBP_Bintervention_Feeding.xlsx", 
                          sheet = "Focal_Behavior_Scans")

demography_table = read_excel("NV_DemographyTable.xlsx", 
                              sheet = "nv_demography_table", 
                              skip = 1, col_names = TRUE)
food_list = read_excel("List of foods observed eaten by NVT baboons.xlsx")

#----------------------------------------------------#
#                        Cleaning                    #
#----------------------------------------------------#

#cleaning time function taken from previous project
convert_mixed_time = function(x) {
  if (is.factor(x)) x = as.character(x)
  
  if (grepl("^1899-12-31", x)) {
    time_part = sub("1899-12-31\\s+", "", x)
    try_excel_hms = suppressWarnings(as.numeric(lubridate::hms(time_part)))
    if (!is.na(try_excel_hms)) return(try_excel_hms)
  }
  
  try_hms = suppressWarnings(as.numeric(lubridate::hms(x)))
  if (!is.na(try_hms)) return(try_hms)
  
  num = suppressWarnings(as.numeric(x))
  if (!is.na(num)) return(num * 86400)
  
  return(NA_real_)
}


### binter_focal bc I need to get the TMF start times ###
binter_focal$Focal_behavior = tolower(binter_focal$Focal_behavior)

binter_focal = binter_focal %>%
  select(Focal_Index, Date, ScanNumber, ScanTime, Focal_behavior) %>%
  filter(ScanNumber == 1) %>%
  filter(Focal_behavior == "feed")


binter_focal = binter_focal %>%
  mutate(
    ScanTime = sapply(ScanTime, convert_mixed_time)) %>%
  mutate(ScanEnd = ScanTime + 600)

#remove duplicate groups in binter_focal that need to be cleaned
time_binter = binter_focal %>%
  group_by(Focal_Index) %>%
  summarise(
    ScanTime = first(ScanTime),
    ScanEnd  = first(ScanEnd),
    .groups = "drop"
  )

#demography table
names(demography_table)[names(demography_table) == "alive_now?"] = "Status"
names(demography_table)[names(demography_table) == "age_categeory_first_seen"] = "Age_Cat"
names(demography_table)[names(demography_table) == "Code"] = "Focal_ID"

demog = demography_table %>%
  select(Name, Focal_ID, Sex, NV_Original, Status, Age_Cat) %>%
  filter(Focal_ID %in% c("ES", "MS", "ZN", "RB", "LK", "GR", "KB", "BV", "SU"))

demog[1,6] = "Parous"

#####------- plant foods df --------#####
food_list$Food_ID = tolower(food_list$Species)
names(food_list)[names(food_list) == "Native, Alien, or Alien Invasive?"] = "Status"
food_list$Status = tolower(food_list$Status)

food_list = food_list %>%
  distinct(Food_ID, .keep_all = TRUE)

######--------- full dataset -------#####
bintervention$Food_ID = tolower(bintervention$Food_ID)
bintervention$Food_part = tolower(bintervention$Food_part)

#clean time columns to convert to seconds, and use TMF as start times when 
#start times are missing
bintervention = bintervention %>% 
  mutate(start_time_sec = sapply(Start_time, convert_mixed_time), 
         end_time_sec = sapply(End_time, convert_mixed_time), 
        TMF_start = sapply(TMF_start, convert_mixed_time), 
        TMF_end = sapply(TMF_end, convert_mixed_time))

bintervention = bintervention %>%
  mutate(
    start_time_sec = coalesce(start_time_sec, TMF_start),
    end_time_sec   = coalesce(end_time_sec, TMF_end),
    Duration_sec = end_time_sec - start_time_sec
  )

#adding demography and food status
bintervention = bintervention %>%
  select(Focal_Index, Day, Month, Year, Date, Focal_ID, 
         Food_ID, Food_part, Context_and_comments, 
         TMF_start, start_time_sec, end_time_sec, 
         TMF_end, Duration_sec) %>%
  left_join(demog %>% select(Focal_ID, Sex, Age_Cat),
            by = "Focal_ID") %>%
  left_join(food_list %>% select(Food_ID, Status),
            by = "Food_ID")

#fixing some "other"
bintervention = bintervention %>%
  mutate(Status = if_else(str_detect(Food_part, "trash"), "human food", Status)) %>%
  mutate(Status = if_else(str_detect(Food_part, "head|heads|body"), "other", Status)) %>%
  mutate(Status = if_else(str_detect(Food_ID, "termites"), "other", Status)) %>%
  #mushroom can go into other instead of alien (will discuss this)
  mutate(Status = if_else(str_detect(Food_ID, "stereum ostrea"), "other", Status))
 #this is the last instance of "alien non-invasive, so I'm changing things up

#now just going to be alien versus indigenous, so changing all the "alien invasive" to just "alien"
bintervention = bintervention %>%
  mutate(Status = if_else(str_detect(Status, "alien invasive"), "alien", Status))

#making the rest of Status NAs unknown
bintervention = bintervention %>%
  mutate(Status = coalesce(Status, "unknown"))

#honestly this whole time_binter and time_bintervention fix took legitimately 
#2 days to figure out and I just kept throwing things at the code until I got it
#to work work and I'm almost positive there's a better way to do it but this is
#what I got. 

#basically, if the start and end times are there, keep
#next, if the TMF start or end was there (I added to my local excel file), then
#use those to fill in missing start or end time. 
#next, if bout start or end is empty AND TMF start or end time is empty, pull
#from the focal behavior scan with corresponding focal index (which I input for
#those missing focal_index in feeding tab on local file). Therefore, binterfocal 
#is filtered for: behavior = feed, scan = 1. SO that SHOULD account for all the 
#missing TMF start, then automatically adds 600 seconds for TMF end. I just 
#manually fill in the TMF ends for entries that had a start time but "in prog"
#for end. This accounts for all entries marked "in prog". The only 10 leftover 
#were from TMFs that have weird discrepancies in the field datasheets or are 
#missing the field sheets entirely.

time_bintervention = bintervention %>%
  left_join(time_binter, by = "Focal_Index") %>%
  mutate(
    TMF_start = ifelse(
      is.na(TMF_start) & is.na(start_time_sec),
      ScanTime,
      TMF_start
    ),
    TMF_end = ifelse(
      is.na(TMF_end) & is.na(end_time_sec),
      ScanEnd,
      TMF_end
    )
  ) %>%
  select(-ScanTime, -ScanEnd) %>%
  mutate(
    start_time_sec = coalesce(start_time_sec, TMF_start),
    end_time_sec   = coalesce(end_time_sec, TMF_end),
    Duration_sec = end_time_sec - start_time_sec
  )

time_bintervention = time_bintervention %>%
  mutate(
    Sex_Age = case_when(
      Sex == "female" & Age_Cat %in% c("Parous", "Nulliparous") ~ "Adult female",
      Age_Cat == "Juvenile" ~ "Juvenile",
      TRUE ~ paste(Age_Cat, Sex)
    )
  )

#I removed the 10 entries that still have NA in the duration section
#because I cannpt "clean" them in my dataset (missing, etc)
clean_bintervention = time_bintervention %>%
  select(Focal_Index, Date, Focal_ID, 
         Food_ID, Food_part, Status,
         Context_and_comments, 
         Duration_sec, Sex, Age_Cat, Sex_Age) %>%
  drop_na(Duration_sec)

# food part cleaning #
unique(bintervention$Food_part)

clean_food_part = clean_bintervention %>%
  select(Focal_Index, Focal_ID, Date, Food_ID, Food_part, Context_and_comments, 
         Duration_sec, Sex_Age) %>%
  mutate(
    Food_part_raw = str_to_lower(Food_part),
    Food_part_raw = str_trim(Food_part_raw),
    
    #all of this part is cleaning but in R because it seemed easier to catch things 
    Food_part_raw = str_replace_all(Food_part_raw, "\\(", ""),
    Food_part_raw = str_replace_all(Food_part_raw, "\\)", ""),
    
    Food_part_raw = str_replace_all(Food_part_raw, "leaves - mature", "mature leaf"),
    Food_part_raw = str_replace_all(Food_part_raw, "mature and young leaves", "mature leaf and young leaf"),
    Food_part_raw = str_replace_all(Food_part_raw, "fruti", "fruit"),
    Food_part_raw = str_replace_all(Food_part_raw, "fruits", "fruit"),
    Food_part_raw = str_replace_all(Food_part_raw, "leafs", "leaf"),
    Food_part_raw = str_replace_all(Food_part_raw, "leaves", "leaf"),
    Food_part_raw = str_replace_all(Food_part_raw, "leave", "leaf"),
    Food_part_raw = str_replace_all(Food_part_raw, "young sapling", "young leaf"),
    Food_part_raw = str_replace_all(Food_part_raw, "leavee", "leaf"),
    Food_part_raw = str_replace_all(Food_part_raw, "shoots?|short shoot|shoots", "shoot"),
    Food_part_raw = str_replace_all(Food_part_raw, "roots?", "root"),
    Food_part_raw = str_replace_all(Food_part_raw, "seeds?|brown seeds", "seed"),
    Food_part_raw = str_replace_all(Food_part_raw, "seed pods", "seed"),
    Food_part_raw = str_replace_all(Food_part_raw, "flowers?", "flower"),
    Food_part_raw = str_replace_all(Food_part_raw, "outer ?bark|inner bark", "bark"),
    Food_part_raw = str_replace_all(Food_part_raw, "matures leaf", "mature leaf"),
    Food_part_raw = str_replace_all(Food_part_raw, "mature leafs", "mature leaf"),
    Food_part_raw = str_replace_all(Food_part_raw, "immature leafs|immature leaf", "young leaf"),
    Food_part_raw = str_replace_all(Food_part_raw, "leaf,young", "young leaf"),
    Food_part_raw = str_replace_all(Food_part_raw, "matures leaf", "mature leaf"),
    Food_part_raw = str_replace_all(Food_part_raw, "mature leafs", "mature leaf"),
    Food_part_raw = str_replace_all(Food_part_raw, "immature leafs|immature leaf", "young leaf"),
    Food_part_raw = str_replace_all(Food_part_raw, "leaf,young", "young leaf"),
    Food_part_raw = str_replace_all(Food_part_raw, "leaf-young", "young leaf"),
    Food_part_raw = str_replace_all(Food_part_raw, "leaf-mature", "mature leaf"),
    Food_part_raw = str_replace_all(Food_part_raw, "mature leavee", "mature leaf"),
    Food_part_raw = str_replace_all(Food_part_raw, "fruit unripe", "unripe fruit"),
    Food_part_raw = str_replace_all(Food_part_raw, "stem base", "stem"),
    Food_part_raw = str_replace_all(Food_part_raw, "dirt|soil", "soil"),
    Food_part_raw = str_replace_all(Food_part_raw, "head|heads|body", "insect"),
    Food_part_raw = str_replace_all(Food_part_raw, "vine in tree", "vine"),
    Food_part_raw = str_replace_all(Food_part_raw, "peel", "other"),
    Food_part_raw = str_replace_all(Food_part_raw, "leaves/stick", "other"),
    Food_part_raw = str_replace_all(Food_part_raw, "na|unknown|unk", "other"),
    
    Food_part_raw = str_replace_all(Food_part_raw, "\\s*,\\s*", ","),
    
    Food_part_raw = str_replace_all(Food_part_raw, " and |/|&|-|\\.", ","),
    Food_part_raw = str_replace_all(Food_part_raw, ",+", ","),
    
    Food_part_raw = str_trim(Food_part_raw)) %>%
  
  #some have more than 1 plant part, separate them to be counted as separate ?
  mutate(
    n_parts = ifelse(is.na(Food_part_raw), 1,
                     str_count(Food_part_raw, ",") + 1)
  ) %>%
  
  separate_rows(Food_part_raw, sep = ",") %>%
  mutate(Food_part_raw = str_trim(Food_part_raw)) %>%
  
  #can't keep rewriting this cleaning code so adding breakthrough typos here
  mutate(
    Food_part_raw = str_replace_all(Food_part_raw, "shoots", "shoot"),
    Food_part_raw = str_replace_all(Food_part_raw, "insects", "insect"),
    Food_part_raw = str_replace_all(Food_part_raw, "mature leafe", "mature leaf"),
    Food_part_raw = str_replace_all(Food_part_raw, "shrub", "vine"),
    Food_part_raw = str_replace_all(Food_part_raw, "stick", "other"),
    
    Food_part_raw = str_trim(Food_part_raw)
  ) %>%
  
  #also want to distinguish between mature and young leaves + ripe fruit
  mutate(
    state = case_when(
      str_detect(Food_part_raw, "unripe") ~ "unripe",
      str_detect(Food_part_raw, "ripe") ~ "ripe",
      str_detect(Food_part_raw, "young|immature") ~ "young",
      str_detect(Food_part_raw, "mature") ~ "mature",
      TRUE ~ NA_character_
    ), 
    Food_part_clean = str_remove_all(
      Food_part_raw,
      "young|immature|mature|ripe|unripe"
    ),
    Food_part_clean = str_trim(Food_part_clean)) %>%
  
  mutate(
    duration_part = Duration_sec / n_parts
  )


unique(clean_food_part$Food_part_clean)

#---------------------------------------------------#
#                 Analyses                          #
#---------------------------------------------------#

#Exploring Food status by Sex and Age Class

food_status_summary = clean_bintervention %>%
  group_by(Sex_Age, Status) %>%  # Status = indigenous/exotic
  summarise(total_time = sum(Duration_sec, na.rm = TRUE)) %>%
  group_by(Sex_Age) %>%
  mutate(prop = total_time / sum(total_time))

ggplot(food_status_summary, 
       aes(x = "", y = prop, fill = Status)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  facet_wrap(~ Sex_Age) +
  geom_text(aes(label = scales::percent(prop)),
            position = position_stack(vjust = 0.5),
            size = 3) +
  scale_fill_manual(values = c(
    "indigenous" = "#1b9e77",
    "alien" = "#7570b3",
    "unknown" = "grey70", 
    "other" = "steelblue",
    "human food" = "tomato"
  )) +
  labs(fill = "Food Status") +
  theme_void()

ggplot(food_status_summary, 
       aes(x = Sex_Age, y = prop, fill = Status)) +
  geom_col(position = "fill") +
  labs(y = "Proportion", fill = "Status") +
  scale_fill_manual(values = c(
    "indigenous" = "#1b9e77",
    "alien" = "#7570b3",
    "unknown" = "grey70", 
    "other" = "steelblue",
    "human food" = "tomato"
  )) +
  labs(fill = "Food Status") +
  theme_void()

#Exploring Food part usage by Sex and Age class

food_part_summary = clean_food_part %>%
  group_by(Sex_Age, Food_part_clean) %>%
  summarise(total_time = sum(duration_part, na.rm = TRUE)) %>%
  group_by(Sex_Age) %>%
  mutate(prop = total_time / sum(total_time))

ggplot(food_part_summary,
       aes(x = "", y = prop, fill = Food_part_clean)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  facet_wrap(~ Sex_Age) +
  geom_text(aes(label = scales::percent(prop)),
            position = position_stack(vjust = 0.5),
            size = 3) +
  scale_fill_manual(values = c(
    "bark" = "#8c510a",
    "bulb" = "blue",
    "flower" = "orchid",
    "fruit" = "orange",
    "insect" = "yellow",
    "leaf" = "chartreuse4",
    "moss" = "magenta",
    "seed" = "bisque3",
    "shoot" = "darkolivegreen1",
    "soil" = "sienna",
    "stem" = "steelblue1",
    "trash" = "tomato",
    "vine" = "lavender",
    "other" = "grey70",
    "root" = "pink"
  )) +
  labs(fill = "Food Part") +
  theme_void() 

ggplot(food_part_summary, 
       aes(x = Sex_Age, y = prop, fill = Food_part_clean)) +
  geom_col(position = "fill") +
  labs(y = "Proportion", fill = "Status") +
  scale_fill_manual(values = c(
    "bark" = "#8c510a",
    "bulb" = "blue",
    "flower" = "orchid",
    "fruit" = "orange",
    "insect" = "yellow",
    "leaf" = "chartreuse4",
    "moss" = "magenta",
    "seed" = "bisque3",
    "shoot" = "darkolivegreen1",
    "soil" = "sienna",
    "stem" = "steelblue1",
    "trash" = "tomato",
    "vine" = "lavender",
    "other" = "grey70",
    "root" = "pink"
  )) +
  labs(fill = "Food Part") +
  theme_void()

#----------------------------------------------------------#


#### Comparison of Indigenous vs Alien Duration for AF and J
indiv_status = clean_bintervention %>%
  group_by(Focal_ID, Sex_Age, Sex, Status) %>%
  summarise(total_duration = sum(Duration_sec, na.rm = TRUE)) %>%
  group_by(Focal_ID) %>%
  mutate(prop_duration = total_duration / sum(total_duration)) %>%
  ungroup()

prop_alien_table = clean_bintervention %>%
  group_by(Focal_ID, Sex_Age) %>%
  summarise(
    total_time = sum(Duration_sec, na.rm = TRUE),
    alien_time = sum(Duration_sec[Status == "alien"], na.rm = TRUE),
    prop_alien = alien_time / total_time
  ) %>%
  ungroup()

prop_indigenous_table = clean_bintervention %>%
  group_by(Focal_ID, Sex_Age) %>%
  summarise(
    total_time = sum(Duration_sec, na.rm = TRUE),
    indigenous_time = sum(Duration_sec[Status == "indigenous"], na.rm = TRUE),
    prop_indigenous = indigenous_time / total_time
  ) %>%
  ungroup()

t.test(prop_alien ~ Sex_Age, data = prop_alien_table)
t.test(prop_indigenous ~ Sex_Age, data = prop_indigenous_table)


#Comparison of Alien vs Indigenous of Nulli vs Parous AF
adult_females = clean_bintervention %>%
  filter(Sex == "female", Age_Cat %in% c("Parous", "Nulliparous"))

adult_alien_prop = adult_females %>%
  group_by(Focal_ID, Age_Cat) %>%
  summarise(
    total_time = sum(Duration_sec, na.rm = TRUE),
    alien_time = sum(Duration_sec[Status == "alien"], na.rm = TRUE),
    prop_alien = alien_time / total_time
  ) %>%
  ungroup()

wilcox.test(prop_alien ~ Age_Cat, data = adult_alien_prop)

adult_indigenous_prop = adult_females %>%
  group_by(Focal_ID, Age_Cat) %>%
  summarise(
    total_time = sum(Duration_sec, na.rm = TRUE),
    indigenous_time = sum(Duration_sec[Status == "indigenous"], na.rm = TRUE),
    prop_indigenous = indigenous_time / total_time
  ) %>%
  ungroup()

wilcox.test(prop_alien ~ Age_Cat, data = adult_alien_prop)
wilcox.test(prop_indigenous ~ Age_Cat, data = adult_indigenous_prop)



#### Comparison of Fruit, Leaf, Insect, and Trash Duration for AF and J
indiv_food = clean_food_part %>%
  group_by(Focal_ID, Sex_Age, Food_part_clean) %>%
  summarise(n_bouts = n(),
            AverageBoutLength_ForID = mean(duration_part, na.rm = TRUE),
            TotalDuration_ForID = sum(duration_part, na.rm = TRUE)) %>%
  ungroup()

#compare means of fruit duration
fruit_duration = indiv_food %>%
  filter(Food_part_clean == "fruit")
t.test(AverageBoutLength_ForID ~ Sex_Age, data = fruit_duration)

#compare means of leaf duration
leaf_duration = indiv_food %>%
  filter(Food_part_clean == "leaf")
t.test(AverageBoutLength_ForID ~ Sex_Age, data = leaf_duration)

#compare means of insect duration *** (significant)
insect_duration = indiv_food %>%
  filter(Food_part_clean == "insect")
t.test(AverageBoutLength_ForID ~ Sex_Age, data = insect_duration)

#compare means of "human food" duration (grouped as Trash)
trash_duration = indiv_food %>%
  filter(Food_part_clean == "trash")
t.test(AverageBoutLength_ForID ~ Sex_Age, data = trash_duration)


#linear mixed models that account for Focal ID as random effect

#LMM fruit - not significant
lmm_fruit = lmer(duration_part ~ Sex_Age + (1 | Focal_ID), data = clean_food_part%>%
                   filter(Food_part_clean == "fruit"))
summary(lmm_fruit)

#LMM leaf - not significant
lmm_leaf = lmer(duration_part ~ Sex_Age + (1 | Focal_ID), data = clean_food_part%>%
                   filter(Food_part_clean == "leaf"))
summary(lmm_leaf)

#LMM shoots - not significant
lmm_shoot = lmer(duration_part ~ Sex_Age + (1 | Focal_ID), data = clean_food_part%>%
                   filter(Food_part_clean == "shoot"))
summary(lmm_shoot)

#LMM insect - significant 
lmm_insect = lmer(duration_part ~ Sex_Age + (1 | Focal_ID), data = clean_food_part%>%
                  filter(Food_part_clean == "insect"))
summary(lmm_insect)


#LMM trash - significant 
lmm_trash = lmer(duration_part ~ Sex_Age + (1 | Focal_ID), data = clean_food_part%>%
                  filter(Food_part_clean == "trash"))
summary(lmm_trash)

#Results table 
food_types = c("fruit", "leaf", "insect", "trash")
results_list = lapply(food_types, function(food) {
  model = lmer(
    duration_part ~ Sex_Age + (1 | Focal_ID),
    data = clean_food_part %>% filter(Food_part_clean == food)
  )
  coef_table = summary(model)$coefficients
  
  if ("Sex_AgeJuvenile" %in% rownames(coef_table)) {
    juvenile_row = data.frame(
      Food_part = food,
      Effect_Juvenile = coef_table["Sex_AgeJuvenile", "Estimate"],
      SE = coef_table["Sex_AgeJuvenile", "Std. Error"],
      t_value = coef_table["Sex_AgeJuvenile", "t value"],
      p_value = coef_table["Sex_AgeJuvenile", "Pr(>|t|)"]
    )
  } else {
    juvenile_row = data.frame(
      Food_part = food,
      Effect_Juvenile = NA,
      SE = NA,
      t_value = NA,
      p_value = NA
    )
  }
  return(juvenile_row)
})
food_part_LMM_results = bind_rows(results_list)

#individual diet: food part percentage table 
indiv_food_part_percentage = clean_food_part %>%
  group_by(Focal_ID, Sex_Age, Food_part_clean) %>%
  summarise(total_time = sum(duration_part, na.rm = TRUE)) %>%
  group_by(Focal_ID) %>%
  mutate(prop_time = total_time / sum(total_time)) %>%
  mutate(percent = round(prop_time * 100, 2)) %>%
  ungroup()
  

indiv_food_part_table = indiv_food_part_percentage %>%
  select(Focal_ID, Food_part_clean, Sex_Age, percent) %>%
  pivot_wider(names_from = Food_part_clean, values_from = percent) %>%
  replace(is.na(.), 0)

food_order = c("fruit", "leaf", "shoot", "seed", "root", "insect", "flower",
               "bark", "soil", "stem", "vine", "bulb", "moss", "trash", "other")

indiv_food_part_table = indiv_food_part_table %>%
  select(Focal_ID, Sex_Age, all_of(food_order)) %>%
  rename_with(~ paste0(str_to_title(.x), " (%)"),
              -c(Focal_ID, Sex_Age)) %>%
  rename(Class = Sex_Age) %>%
  arrange(Class)








### plot play
ggplot(clean_food_part, aes(x = duration_part)) +
  geom_histogram(bins = 30, fill = "steelblue")

summary(clean_food_part$duration_part)

#results: histogram shows skew in duration data which is expected for feeding
#times, and a high count of the 600 seconds (10 min entirety)

ggplot(clean_food_part, aes(x = duration_part)) +
  geom_histogram(bins = 30, fill = "steelblue") +
  scale_x_log10()

count_data = clean_food_part %>%
  group_by(Food_part_clean, Sex_Age) %>%
  summarise(n = n(), .groups = "drop")

clean_food_part$Food_part_clean = factor(clean_food_part$Food_part_clean, 
                                    levels = c("leaf", "fruit", "shoot", "seed", "root", 
                                               "bark", "flower", "insect", "trash", 
                                               "stem", "vine", "bulb", "moss", 
                                               "soil", "other"))

#is using log10 helpful here? 

ggplot(clean_food_part, aes(x = Food_part_clean, y = duration_part, fill = Sex_Age)) +
  geom_boxplot(alpha = 0.7) + 
  geom_jitter(aes(color = Sex_Age),
              position = position_jitterdodge(jitter.width = 0.2, 
                                              dodge.width = 0.8),
              alpha = 0.5) +
  geom_text (
    data = count_data,
    aes(x = Food_part_clean, y = 600 * 1.05, label = n),
    position = position_dodge(width = 0.8),
    size = 3) +
  
  labs(
    title = "Feeding Effort by Class and Food Part",
    x = "Food Part",
    y = "Log10 Duration (seconds)", 
    fill = "Class", 
    color = "Class") +
  
  theme_minimal() +
  scale_y_log10()


#versus 

clean_food_part$Food_part_clean = factor(clean_food_part$Food_part_clean, 
                                         levels = c("leaf", "fruit", "shoot", "seed", "root", 
                                                    "bark", "flower", "insect", "trash", 
                                                    "stem", "vine", "bulb", "moss", 
                                                    "soil", "other"))
                  
ggplot(clean_food_part, aes(x = Food_part_clean, y = duration_part, fill = Sex_Age)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(aes(color = Sex_Age),
              position = position_jitterdodge(jitter.width = 0.2, 
                                              dodge.width = 0.8),
                alpha = 0.5) +
  geom_text (
    data = count_data,
    aes(x = Food_part_clean, y = 600 * 1.05, label = n),
    position = position_dodge(width = 0.8),
    size = 3) +
  labs(
    title = "Feeding Effort by Class and Food Part",
    x = "Food Part",
    y = "Duration (seconds)", 
    fill = "Class", 
    color = "Class"
  ) +
  theme_minimal()

#Feeding bout durations varied across food categories and showed substantial 
#individual-level variation between age classes (Fig. X), suggesting 
#heterogeneity in feeding behavior not captured by proportional diet measures.



##############Wilcoxon Tests####################


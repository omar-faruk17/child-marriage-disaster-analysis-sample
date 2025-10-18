rm(list=ls())
# set directory
setwd("~/Documents/natural disaster paper_code")

library(tidyverse)
library(haven)
library(gtsummary)
library(labelled)
# Load wm file
load("~/Documents/natural disaster paper_code/wm_r.RData")

# ---
# --- Selecting Variables from wm table
# ---
wm_selection <- wm %>% 
  select(HH1, HH2, WM_line = LN, WM_birthcmc = WDOB, WM_age = WB4, WM_unioncmc = WDOM, WM_marriage_age = WAGEM,
         WM_edlevel = welevel, WM_age_of_husband = MA2, location = HH6, division = HH7, district = HH7A, ethnicity, internet_ever = MT9,
         newspaper_frq = MT1, radio_freq = MT2, television_freq = MT3, wealth_level = windex5, wmweight)

# UID creation
wm_selection <- wm_selection %>% 
  mutate(UID = paste(HH1, HH2, sep ="_"))
wm_selection <- wm_selection %>% 
  mutate(UID_1 = paste(UID,WM_line, sep ="_"))
wm_selection <- wm_selection %>% 
  select(UID_1, UID, everything())
# 68,709 Observations

# keeping women who are interviewed completely and also whose weighted sample greater than zero
wm_selection <- wm_selection %>%
  filter(wmweight != 0)
# 64,378 observations.

# Keeping the label for variables
wm_selection$location <- as_factor(wm_selection$location)
wm_selection$division <- as_factor(wm_selection$division)
wm_selection$internet_ever <- as_factor(wm_selection$internet_ever)
wm_selection$radio_freq <- as_factor(wm_selection$radio_freq)
wm_selection$television_freq <- as_factor(wm_selection$television_freq)
wm_selection$newspaper_frq <- as_factor(wm_selection$newspaper_frq)
wm_selection$WM_edlevel <- as_factor(wm_selection$WM_edlevel)
wm_selection$wealth_level <- as_factor(wm_selection$wealth_level)

# Creating a new education variable for our study.
#.............................................................
wm_selection %>% 
  select(WM_edlevel) %>% 
  tbl_summary()

# Changing the label name of wealth_level variable.
wm_selection %>% 
  select(wealth_level) %>% 
  tbl_summary()
wm_selection <- wm_selection %>%
  mutate(wealth_level = case_when(wealth_level == "Poorest" ~ "Poorest", 
                                  wealth_level == "Second" ~ "Poor",
                                  wealth_level == "Middle" ~ "Middle",
                                  wealth_level== "Fourth" ~ "Rich",
                                  wealth_level== "Richest" ~ "Richest"))
wm_selection %>% 
  select(wealth_level) %>% 
  tbl_summary()

wm_selection <- wm_selection %>% 
  mutate(ethnicity = case_when(
    ethnicity == 1 ~ "Bengali",
    TRUE ~ "Other"
  ))
wm_selection %>% 
  select(ethnicity) %>% 
  tbl_summary()

# Changing the label name of internet variable.
wm_selection %>% 
  select(internet_ever) %>% 
  tbl_summary()
wm_selection <- wm_selection %>%
  mutate(RA_internet_ever = case_when(
    internet_ever == "YES" ~ "yes", 
    is.na(internet_ever) ~ "no", 
    TRUE ~ "no" 
  ))
table(wm_selection$RA_internet_ever)

#............................. ................................................................................................................
# Creating Media Exposure variable: Creating Respondents' Exposure to Television, Newspaper, or Radio at Least Once a Week (Yes/No)
#............................................................................................................................................

# Creating the RA_newspaper_frequency Variable (At Least Once a Week) for Media Exposure variable
wm_selection %>% 
  select(newspaper_frq) %>% 
  tbl_summary()
wm_selection <- wm_selection %>%
  mutate(RA_newspaper_frq = case_when(
    newspaper_frq == "AT LEAST ONCE A WEEK" ~ "yes", 
    newspaper_frq == "AALMOST EVERY DAY" ~ "yes",
    is.na(newspaper_frq) ~ "no", 
    TRUE ~ "no" 
  ))
table(wm_selection$RA_newspaper_frq)

# Recreating the RA_radio_freq Variable (At Least Once a Week) for Media Exposure variable
wm_selection %>% 
  select(radio_freq) %>% 
  tbl_summary()

wm_selection <- wm_selection %>%
  mutate(RA_radio_freq = case_when(
    radio_freq == "AT LEAST ONCE A WEEK" ~ "yes", 
    radio_freq == "ALMOST EVERY DAY" ~ "yes",
    is.na(radio_freq) ~ "no", 
    TRUE ~ "no" 
  ))
table(wm_selection$RA_radio_freq)

# Recreating the RA_television_freq Variable (At Least Once a Week) for Media Exposure variable
wm_selection %>% 
  select(television_freq) %>% 
  tbl_summary()

wm_selection <- wm_selection %>%
  mutate(RA_television_freq = case_when(
    television_freq == "AT LEAST ONCE A WEEK" ~ "yes", 
    television_freq == "ALMOST EVERY DAY" ~ "yes",
    is.na(television_freq) ~ "no", 
    TRUE ~ "no" 
  ))
table(wm_selection$RA_television_freq)

# Media Exposure Variable (yes/no)
wm_selection <- wm_selection %>%
  mutate(media_exposure = ifelse(rowSums(select(., RA_newspaper_frq, RA_radio_freq, RA_television_freq, RA_internet_ever) == "yes") > 0, "yes", "no"))
wm_selection %>% 
  select(RA_newspaper_frq, RA_radio_freq, RA_television_freq, RA_internet_ever, media_exposure) %>% 
  View()
table(wm_selection$media_exposure)

#save(wm_selection, file = "wm_selection_r.RData")



rm(list=ls())
# Load hl file
load("~/Documents/natural disaster paper_code/hl_r.RData")
# Counting the number of HHs in HL table.
#.......................................
hl_count <- hl %>% 
  mutate(UID = paste(HH1, HH2, sep ="_"))
unique_count <- hl_count %>% 
  distinct(UID) %>% 
  nrow()
print(unique_count)
# 61,242 HHs

#............................................
# Keeping the relevant variables for our study
#............................................
# HH1 = Cluster number, HH2 = Household number in that cluster, HL1 = Line number, ED12 (School tuition in the current school year)
# Does natural mother live in the hh = HL13, where does natural mother live = HL15
# Does natural father live in the hh = HL17, where does natural father live = HL19,
hl_selection <- hl %>% 
  select(HH1, HH2, HL_line = HL1, ED12, HL13, HL15, HL17, HL19)

hl_selection <- hl_selection %>% 
  mutate(UID = paste(HH1, HH2, sep ="_"))

hl_selection <- hl_selection %>% 
  mutate(UID_1 = paste(UID, HL_line, sep ="_"))

# Creating stipend variable (yes/no)
#...........................................................
hl_selection$ED12 <- as_factor(hl_selection$ED12)
hl_selection %>% 
  select(ED12) %>% 
  tbl_summary()
hl_selection <- hl_selection %>%
  mutate(stipend = case_when(
    ED12 == "YES" ~ "yes", 
    ED12 == "NO" ~ "no",
    is.na(ED12) ~ "no", 
    TRUE ~ "no" 
  ))
table(hl_selection$stipend)

#save(hl_selection, file = "hl_selection_r.RData")




rm(list=ls())
# Load hh file
load("~/Documents/natural disaster paper_code/hh_r.RData")

#............................................
# Keeping the relevant variables for our study
#............................................
hh_selection <- hh %>% 
  select(HH1, HH2, RA_head_education = helevel, HHreligion = HC1A, 
         HHAGE, HHSEX, assistance_employment = `ST3$2`, assistance_food = `ST3$3`)
# `ST3$2` = Employment generation Program and `ST3$3` = Food Support (VGD/VGF)

# Creating unique id
hh_selection <- hh_selection %>% 
  mutate(UID = paste(HH1, HH2, sep ="_"))

# Labelling the religion variable
hh_selection %>% 
  select(HHreligion) %>% 
  tbl_summary()

hh_selection <- hh_selection %>% 
  mutate(RA_religion = case_when(
    HHreligion == 1 ~ "Islam",
    HHreligion == 2 ~ "Hindu",
    TRUE ~ "Other"
  ))
hh_selection %>% 
  select(RA_religion) %>% 
  tbl_summary()

hh_selection %>% 
  select(RA_head_education) %>% 
  tbl_summary()

# Labelling  HHSEX variable (male/female)
hh_selection %>% 
  select(HHSEX) %>% 
  tbl_summary()

hh_selection <- hh_selection %>%
  mutate(HHSEX = case_when(
    HHSEX == 1 ~ "Male",
    HHSEX == 2 ~ "Female",
    TRUE       ~ NA))

# Labelling  assistance_employment (as RA_assistance_employment) variable (yes/no)
#...............................................................
hh_selection$assistance_employment <- as_factor(hh_selection$assistance_employment)
hh_selection %>% 
  select(assistance_employment) %>% 
  tbl_summary()

hh_selection <- hh_selection %>%
  mutate(RA_assistance_employment = case_when(
    assistance_employment %in% c("NO", "DK", "NO RESPONSE") ~ "no", 
    assistance_employment == "YES" ~ "yes",
    is.na(assistance_employment) ~ "no",
    TRUE ~ "no"
  ))
table(hh_selection$RA_assistance_employment)


# Labelling  assistance_food (as RA_assistance_food) variable (yes/no)
#...............................................................
hh_selection$assistance_food <- as_factor(hh_selection$assistance_food)
hh_selection %>% 
  select(assistance_food) %>% 
  tbl_summary()
hh_selection <- hh_selection %>%
  mutate(RA_assistance_food = case_when(
    assistance_food %in% c("NO", "DK", "NO RESPONSE") ~ "no", 
    assistance_food == "YES" ~ "yes",
    is.na(assistance_food) ~ "no",
    TRUE ~ "no"
  ))
table(hh_selection$RA_assistance_food)

#save(hh_selection, file = "hh_selection_r.RData")





#............................................................................................................................
#.......................Join hh_selection, hl_selection table with wm_selection table
#............................................................................................................................
rm(list=ls())
# set directory
setwd("~/Documents/natural disaster paper_code")

library(tidyverse)
library(haven)
library(gtsummary)
library(labelled)
load("~/Documents/natural disaster paper_code/wm_selection_r.RData")
load("~/Documents/natural disaster paper_code/hl_selection_r.RData")
load("~/Documents/natural disaster paper_code/hh_selection_r.RData")

wm_hl <- merge(wm_selection, hl_selection, by = "UID_1", all.x = TRUE, all.y = FALSE)
sum(is.na(wm_hl$location))
# A quick check to verify if the join was done appropriately. There is no missing entries in location.

# Delete duplicate (created after joining the two tables) and unnecessary variable (from these variables we created a necessary variable)
wm_hl <- wm_hl %>% 
  rename(UID = UID.x, RA_newspaper_freq = RA_newspaper_frq) %>% 
  select(-c(HH1.x, HH2.x, radio_freq, television_freq, newspaper_frq, 
            HH1.y, HH2.y, HL_line, ED12, UID.y))

women_table <- merge(wm_hl, hh_selection, by = "UID", all.x = TRUE, all.y = FALSE)
sum(is.na(women_table$location))
sum(is.na(women_table$division))
sum(is.na(women_table$district))
# No missing observations occured due to joining tables.

# Load inform hazard dataset (excel file). The INFORM Risk Index is a global, open-source risk assessment for humanitarian crises and disasters. 
library(readxl)
inform_datset_2022 <- read_excel("inform_datset_2022.xlsx")

inform_dataset_2022 <- inform_datset_2022 %>% 
  mutate(natural_hazard_class = ifelse(natural_hazard_inform >= 6.2, "High",
                                       ifelse(natural_hazard_inform < 6.2 & natural_hazard_inform >= 6, "Medium", 
                                              ifelse(natural_hazard_inform < 6, "Low", NA))))
inform_dataset_2022 <- inform_dataset_2022 %>% 
  mutate(hazard_exposure_class = ifelse(Hazard_exposure_inform >= 6.2, "High",
                                        ifelse(Hazard_exposure_inform < 6.2 & Hazard_exposure_inform >= 6, "Medium", 
                                               ifelse(Hazard_exposure_inform < 6, "Low", NA))))

# Creating group_natural and group_hazard variable
# Step 1: Rank districts based on natural_hazard_inform and Hazard_exposure_inform (highest = rank 1)
inform_dataset_2022 <- inform_dataset_2022 %>%
  arrange(desc(natural_hazard_inform)) %>%
  mutate(
    rank = row_number(),
    group_natural = case_when(
      rank >= 1  & rank <= 21 ~ "High",
      rank >= 22 & rank <= 43 ~ "Medium",
      rank >= 44 & rank <= 64 ~ "Low"))

inform_dataset_2022 <- inform_dataset_2022 %>%
  arrange(desc(Hazard_exposure_inform)) %>%
  mutate(
    rank = row_number(),
    group_hazard = case_when(
      rank >= 1  & rank <= 21 ~ "High",
      rank >= 22 & rank <= 43 ~ "Medium",
      rank >= 44 & rank <= 64 ~ "Low"))

# Step 2: Check distribution
table(inform_dataset_2022$group_natural)
table(inform_dataset_2022$group_hazard)

inform_dataset_2022$Hazard_exposure_class <- NULL
inform_dataset_2022$rank <- NULL

# Merging hazard table with women table
women_inform_table <- merge(women_table, inform_dataset_2022, by = "district", all.x = TRUE, all.y = FALSE)
# Checking whether the join creates any na observation 
sum(is.na(women_inform_table$district))
sum(is.na(women_inform_table$location))
sum(is.na(women_inform_table$natural_hazard_inform))

# Checking whether the women table district and excel file's district is same or different.
women_inform_table$district <- as_factor(women_inform_table$district)
women_inform_table %>% 
  select(district) %>%
  tbl_summary()
women_inform_table %>% 
  select(District_name) %>%
  tbl_summary()
#Result: Both district variable is same.

#save(women_inform_table, file = "women_inform_table_r.RData")


rm(list=ls())
# set directory
setwd("~/Documents/natural disaster paper_code")

library(tidyverse)
library(haven)
library(gtsummary)
library(labelled)
library(survey)
load("~/Documents/natural disaster paper_code/women_inform_table_r.RData")

#......................................................
# Create Parental Migration Status variable
#......................................................
# HL13 = Does natural mother live in hh and HL17 = Does natural father live in hh
# Code HL13 and HL17 variable: 1 = lives in the household, 2 = does not live in household
# HL15 = where does natural mother live and HL19 = where does natural father live 
women_inform_table %>% 
  select(HL13) %>% 
  tbl_summary()
women_inform_table %>% 
  select(HL17) %>% 
  tbl_summary()
women_inform_table$HL13 <- ifelse(women_inform_table$HL13 == 9, NA, women_inform_table$HL13)
women_inform_table$HL17 <- ifelse((women_inform_table$HL13 %in% c(1, 2)) & 
                                    is.na(women_inform_table$HL17),2,women_inform_table$HL17)
women_inform_table$HL13 <- ifelse((women_inform_table$HL17 %in% c(1, 2)) & 
                                    is.na(women_inform_table$HL13),2,women_inform_table$HL13)
women_inform_table <- women_inform_table %>%
  mutate(
    both_parent_hh = case_when(
      HL13 == 1 & HL17 == 1 ~ "Both",      # both parents
      HL13 == 2 & HL17 == 2 ~ "None",      # no parent
      HL13 == 1 & HL17 == 2 ~ "Single",    # one parent
      HL13 == 2 & HL17 == 1 ~ "Single",    # one parent
      TRUE ~ NA_character_                 # catch anything unexpected
    )
  )
women_inform_table %>% 
  select(both_parent_hh) %>% 
  tbl_summary()

# Viewing the HL15 (Where does natural mother live) variable
women_inform_table %>% 
  select(HL15) %>%
  mutate(HL15 = as_factor(HL15)) %>%  
  tbl_summary()

# Viewing the HL19 (Where does natural father live) variable
women_inform_table %>% 
  select(HL19) %>%
  mutate(HL19 = as_factor(HL19)) %>%   
  tbl_summary()

# Creating Parent Migration Variable.
women_inform_table <- women_inform_table %>% 
  mutate(parent_migration = case_when(
    both_parent_hh != "Both" & !is.na(both_parent_hh) & (HL15 == 1 | HL19 == 1) ~ "Abroad", 
    both_parent_hh != "Both" & !is.na(both_parent_hh) & (HL15 == 2 & HL19 == 2) ~ "Near", 
    both_parent_hh != "Both" & !is.na(both_parent_hh) ~ "Distant", 
    both_parent_hh == "Both" ~ "Together", 
    TRUE ~ NA_character_
  ))
women_inform_table %>% 
  select(parent_migration) %>% 
  tbl_summary()

# Create a RA_assit_any variable by combining RA_assistance_employment and RA_assistance_food variables
#............................................................................................................................
women_inform_table <- women_inform_table %>% 
  mutate(RA_assit_any = ifelse(RA_assistance_employment == "yes" | RA_assistance_food == "yes", "yes", "no"))
table(women_inform_table$RA_assit_any)

#save(women_inform_table, file = "women_inform_table_r.RData")


rm(list = ls())
setwd("~/Documents/natural disaster paper_code")

# ---------- load data ----------
load("~/Documents/natural disaster paper_code/women_inform_table_r.RData")

# Load required packages
library(gtsummary)
library(flextable)
library(officer)
library(dplyr)
library(survival)
library(survey)
library(broom)

# -----------------------------
# Create survey design for aged 15-17
# -----------------------------
aged_15_17 <- women_inform_table %>% 
  filter(WM_age >= 15 & WM_age < 18)

#...................................................................................................
# Convert variables to factors and set reference levels for summary statistics and regression
#...................................................................................................

is.factor(aged_15_17$location)
aged_15_17$location <- relevel(aged_15_17$location,ref="RURAL")

is.factor(aged_15_17$hazard_exposure_class)
aged_15_17$hazard_exposure_class <- factor(
  aged_15_17$hazard_exposure_class,
  levels = c("Low", "Medium", "High"))
aged_15_17$hazard_exposure_class <- relevel(aged_15_17$hazard_exposure_class,ref="Low")

is.factor(aged_15_17$WM_edlevel)
aged_15_17$WM_edlevel <- droplevels(aged_15_17$WM_edlevel, exclude = "Missing/DK")
aged_15_17$WM_edlevel <- relevel(aged_15_17$WM_edlevel,ref="Pre-primary or none")

is.factor(aged_15_17$wealth_level)
aged_15_17$wealth_level <- factor(
  aged_15_17$wealth_level,
  levels = c("Poorest", "Poor", "Middle", "Rich", "Richest"))
aged_15_17$wealth_level <- relevel(aged_15_17$wealth_level,ref="Poorest")

is.factor(aged_15_17$RA_religion)
aged_15_17$RA_religion <- factor(
  aged_15_17$RA_religion,
  levels = c("Islam", "Hindu", "Other"))
aged_15_17$RA_religion <- relevel(aged_15_17$RA_religion,ref="Islam")

is.factor(aged_15_17$ethnicity)
aged_15_17$ethnicity <- factor(aged_15_17$ethnicity)
aged_15_17$ethnicity <- relevel(aged_15_17$ethnicity,ref="Bengali")

is.factor(aged_15_17$RA_internet_ever)
aged_15_17$RA_internet_ever <- as.factor(aged_15_17$RA_internet_ever)
aged_15_17$RA_internet_ever <- relevel(aged_15_17$RA_internet_ever,ref="yes")

is.factor(aged_15_17$media_exposure)
aged_15_17$media_exposure <- as.factor(aged_15_17$media_exposure)
aged_15_17$media_exposure <- relevel(aged_15_17$media_exposure,ref="yes")

is.factor(aged_15_17$division)
aged_15_17$division <- relevel(aged_15_17$division,ref="Barishal")

is.factor(aged_15_17$stipend)
aged_15_17$stipend <- as.factor(aged_15_17$stipend)
aged_15_17$stipend <- relevel(aged_15_17$stipend,ref="yes")

is.factor(aged_15_17$RA_assit_any)
aged_15_17$RA_assit_any <- as.factor(aged_15_17$RA_assit_any)
aged_15_17$RA_assit_any <- relevel(aged_15_17$RA_assit_any,ref="yes")

is.factor(aged_15_17$RA_head_education)
table(aged_15_17$RA_head_education)
aged_15_17 <- aged_15_17 %>%
  mutate(RA_head_education = case_when(
    RA_head_education %in% c(0, 9) ~ "Pre-primary or none",
    RA_head_education == 1 ~ "Primary",
    RA_head_education == 2 ~ "Secondary",
    RA_head_education == 3 ~ "Higher+"))

aged_15_17$RA_head_education <- factor(
  aged_15_17$RA_head_education,
  levels = c("Pre-primary or none", "Primary", "Secondary", "Higher+"))
aged_15_17$RA_head_education <- relevel(aged_15_17$RA_head_education, ref = "Pre-primary or none")

is.factor(aged_15_17$HHSEX)
table(aged_15_17$HHSEX)
aged_15_17$HHSEX <- as.factor(aged_15_17$HHSEX)
aged_15_17$HHSEX <- relevel(aged_15_17$HHSEX,ref="Male")

# Standardize Hazard_exposure_inform 
sum(is.na(aged_15_17$Hazard_exposure_inform))
aged_15_17 <- aged_15_17 %>%
  mutate(Hazard_exposure_inform_std = (Hazard_exposure_inform - mean(Hazard_exposure_inform)) /
           sd(Hazard_exposure_inform))
summary(aged_15_17$Hazard_exposure_inform_std)

# Releveling parent_migration variable 
aged_15_17 <- aged_15_17 %>%
  mutate(Parental_Migration_status = case_when(
    parent_migration == "Together" ~ "Together",
    parent_migration == "Abroad" ~ "Abroad",
    parent_migration %in% c("Distant", "Near") ~ "Temporary Domestic Migration"))

# Convert to factor and set baseline
aged_15_17$Parental_Migration_status <- factor(aged_15_17$Parental_Migration_status,
                                               levels = c("Together", "Temporary Domestic Migration","Abroad"))

# -----------------------------
# Create child marriage variable
# -----------------------------
aged_15_17 <- aged_15_17 %>%
  mutate(child_marriage = ifelse(is.na(WM_marriage_age), "no", "yes"))
aged_15_17$child_marriage <- as.factor(aged_15_17$child_marriage)

# -----------------------------
# Create survey design with weights
# -----------------------------
design_15_17 <- svydesign(ids = ~1, weights = ~wmweight, data = aged_15_17)

#............................................
# Creating Graph
#............................................

library(ggplot2)
# Compute child marriage proportion by age
child_marriage_age <- svyby(
  ~I(child_marriage == "yes"),
  ~WM_age,
  design = design_15_17,
  svymean,
  na.rm = TRUE
)

# Convert to data frame
child_marriage_age <- as.data.frame(child_marriage_age) %>%
  rename(child_marriage_rate = `I(child_marriage == "yes")TRUE`) %>%
  mutate(child_marriage_rate = child_marriage_rate * 100)  # percent

# -----------------------------
# Scatter plot
# -----------------------------
p <- ggplot(child_marriage_age, aes(x = WM_age, y = child_marriage_rate)) +
  geom_point(color = "blue", size = 3) +
  scale_y_continuous(labels = function(x) paste0(round(x, 1), "%")) +
  labs(
    x = "Women's Age",
    y = "Child Marriage Rate (%)",
    title = "Child Marriage Rate by Age 15 to <18"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold")
  )
# Display the plot
p

# -----------------------------
# Save the plot P
# -----------------------------
ggsave(filename = "Graph 3 Weighted Child Marriage 15-17.png", plot = p, path = "~/Documents/natural disaster paper_code",
       width = 8,
       height = 6,
       dpi = 300)



# -----------------------------
# Compute weighted child marriage proportion by age AND hazard exposure class
# -----------------------------
child_marriage_age_hazard <- svyby(
  ~I(child_marriage == "yes"),
  ~WM_age + hazard_exposure_class,
  design = design_15_17,
  svymean,
  na.rm = TRUE
)

# Convert to data frame and percentage
child_marriage_age_hazard <- as.data.frame(child_marriage_age_hazard) %>%
  rename(child_marriage_rate = `I(child_marriage == "yes")TRUE`) %>%
  mutate(
    WM_age = round(WM_age),  # round to integer
    child_marriage_rate = child_marriage_rate * 100
  )

# -----------------------------
# Scatter plot by hazard exposure class
# -----------------------------
q <- ggplot(child_marriage_age_hazard, aes(x = WM_age, y = child_marriage_rate, color = hazard_exposure_class)) +
  geom_point(size = 3) +
  scale_y_continuous(labels = function(x) paste0(round(x, 1), "%")) +
  scale_color_manual(values = c("Low" = "#2c7fb8", "Medium" = "#7fcdbb", "High" = "#edf8b1")) +
  labs(
    x = "Women's Age",
    y = "Child Marriage Rate (%)",
    color = "Hazard Exposure Class",
    title = "Child Marriage Rate (Age 15 to <18) by Hazard Exposure Class"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )

# Display the plot
q

# -----------------------------
# Save the plot q
# -----------------------------
ggsave(
  filename = "Graph 3.1 weighted Child Marriage 15-17_by_Hazard.png",
  plot = q, path = "~/Documents/natural disaster paper_code",
  width = 8,
  height = 6,
  dpi = 300)




# -----------------------------
# Function to create weighted row percent tables
# -----------------------------
row_percent_table <- function(var, design, outcome){
  f <- as.formula(paste0("~", outcome, "+", var))
  tab <- svytable(f, design)
  
  df <- as.data.frame(tab) %>%
    group_by_at(var) %>%
    mutate(
      total = sum(Freq),
      perc = round((Freq / total) * 100, 1),
      Freq = round(Freq, 0)
    ) %>%
    ungroup() %>%
    mutate(val = paste0(Freq, " (", perc, "%)")) %>%
    select(all_of(var), all_of(outcome), val) %>%
    pivot_wider(names_from = all_of(outcome), values_from = val)
  
  names(df)[1] <- "Category"
  
  # p-value
  pval <- tryCatch({
    chisq <- suppressWarnings(svychisq(f, design))
    ifelse(chisq$p.value < 0.001, "<0.001", formatC(chisq$p.value, format="f", digits=3))
  }, error = function(e) NA)
  
  # Insert variable row first, then categories
  df_out <- bind_rows(
    tibble(Category = paste0("**", var, "**"),
           no = "", yes = "", p.value = pval),
    df %>% mutate(p.value = "")
  )
  
  df_out
}

# -----------------------------
# Apply to variables
# -----------------------------
vars <- c("location", "division", "wealth_level", "WM_edlevel", 
          "RA_head_education", "stipend", "ethnicity", "RA_religion", 
          "media_exposure", "HHSEX", "hazard_exposure_class", 
          "Parental_Migration_status")

summary_list <- lapply(vars, function(v) row_percent_table(v, design_15_17, "child_marriage"))
summary_df <- bind_rows(summary_list)

# -----------------------------
# Calculate weighted N for Child Marriage
# -----------------------------
N_weighted <- round(sum(weights(design_15_17)[!is.na(aged_15_17$child_marriage)]), 0)

# -----------------------------
# Format flextable with weighted N in header
# -----------------------------
ft <- flextable(summary_df) %>%
  set_header_labels(
    Category = "Characteristics",
    no = "No (%)",
    yes = "Yes (%)",
    p.value = "P value"
  ) %>%
  add_header_row(
    values = c("Characteristics", paste0("Child Marriage (<18 years), N = ", N_weighted), "P value"),
    colwidths = c(1, 2, 1)
  ) %>%
  bold(i = ~ grepl("\\*\\*", Category), j = "Category", bold = TRUE) %>%
  compose(i = ~ grepl("\\*\\*", Category), j = "Category",
          value = as_paragraph(gsub("\\*","",Category))) %>%
  fontsize(size = 9, part = "all") %>%
  autofit() %>%
  align(align="center", part="header") %>%
  align(align="left", part="body", j=1) %>%
  align(align="center", part="body", j=2:4) %>%
  set_caption("Table 1: Characteristics of Women Aged 15 to <18")

# -----------------------------
# Export to Word
# -----------------------------
#save_as_docx(ft, path = "Table 4 Summary_Statistics_Table for Cox Proportional Hazard regression.docx")
ft



# -----------------------------------------------------------------
# Cox Proportional Hazard Regression
# -----------------------------------------------------------------
# create a married or not married  variable (0 is not married and 1 is married)
aged_15_17 <- aged_15_17 %>%
  mutate(RA_married_dummy = ifelse(is.na(WM_marriage_age), 0, 1))

aged_15_17 %>% 
  select(RA_married_dummy) %>%
  mutate(RA_married_dummy = factor(RA_married_dummy)) %>% 
  tbl_summary()

# Create event_age variable for cox proportional regression
aged_15_17 <- aged_15_17 %>%
  mutate(event_age = ifelse(RA_married_dummy == 1, WM_marriage_age, 
                            WM_age))

# Create a event_dummy variable Where 1 indicates the event (Marriage) occurred and 0 indicates it did not occur.".
aged_15_17 <- aged_15_17 %>% 
  mutate(event_dummy = ifelse(RA_married_dummy == 1, 1, 0))

aged_15_17 %>% 
  select(event_dummy) %>%
  mutate(event_dummy = factor(event_dummy)) %>% 
  tbl_summary()

aged_15_17 %>% 
  select(event_age) %>%
  mutate(event_age = factor(event_age)) %>% 
  tbl_summary()

aged_15_17$event_age <- as.numeric(aged_15_17$event_age)
aged_15_17$event_dummy <- as.numeric(aged_15_17$event_dummy)

reg_1_weighted <- coxph(Surv(event_age, event_dummy) ~ location + division + wealth_level + WM_edlevel+ RA_head_education + 
                          stipend+ ethnicity + RA_religion + media_exposure + HHSEX +
                          hazard_exposure_class + Parental_Migration_status, data = aged_15_17, weights = wmweight)
summary(reg_1_weighted)

reg_2_weighted <- coxph(Surv(event_age, event_dummy) ~ location + division + wealth_level + WM_edlevel+ RA_head_education + stipend
                        + ethnicity + RA_religion + media_exposure + HHSEX +
                          Hazard_exposure_inform_std + Parental_Migration_status, data = aged_15_17, weights = wmweight)
summary(reg_2_weighted)



# Tidy both models
reg1_tbl <- tidy(reg_1_weighted, exponentiate = TRUE, conf.int = TRUE)
reg2_tbl <- tidy(reg_2_weighted, exponentiate = TRUE, conf.int = TRUE)

# Function for significance star
sig_star <- function(p) {
  if (p < 0.001) return("***")
  else if (p < 0.01) return("**")
  else if (p < 0.05) return("*")
  else return("")
}

# Create columns with HR, CI, and star
reg1_tbl$HR <- sprintf("%.2f%s", reg1_tbl$estimate, sapply(reg1_tbl$p.value, sig_star))
reg1_tbl$CI <- sprintf("(%.2f–%.2f)", reg1_tbl$conf.low, reg1_tbl$conf.high)
reg2_tbl$HR <- sprintf("%.2f%s", reg2_tbl$estimate, sapply(reg2_tbl$p.value, sig_star))
reg2_tbl$CI <- sprintf("(%.2f–%.2f)", reg2_tbl$conf.low, reg2_tbl$conf.high)

# Merge, keeping all variables
all_terms <- unique(c(reg1_tbl$term, reg2_tbl$term))
reg1_row <- reg1_tbl[match(all_terms, reg1_tbl$term), c("HR", "CI")]
reg2_row <- reg2_tbl[match(all_terms, reg2_tbl$term), c("HR", "CI")]

final_tbl <- data.frame(
  Characteristics = all_terms,
  `Model 1 HR` = reg1_row$HR,
  `Model 1 95% CI` = reg1_row$CI,
  `Model 2 HR` = reg2_row$HR,
  `Model 2 95% CI` = reg2_row$CI
)

# Optionally, improve variable labels for publication

# Create flextable
ft <- flextable(final_tbl)
ft <- set_caption(ft, "Table X: Comparison of Weighted Cox Proportional Hazards Models — Hazard Ratios (HR) and 95% Confidence Intervals (CI) for Key Characteristics")
ft <- add_footer_lines(ft, values = c(
  "*p < 0.05; **p < 0.01; ***p < 0.001",
  "HR = Hazard Ratio; CI = Confidence Interval"
))

# Export to Word
doc <- read_docx()
doc <- body_add_flextable(doc, ft)
#print(doc, target = "Table 5 Cox_Regression_Results.docx")





#.....................................
# Kaplan Meir Graph
#.......................................
library(survminer)
library(flexsurv)

# Kaplan Meier Model 
km_fit <- survfit(Surv(event_age, event_dummy) ~ 1, data = aged_15_17)
summary(km_fit)

#...KM model based on hazard_exposure_class
#..................................

# Kaplan-Meier stratified by hazard exposure
km_hazard <- survfit(Surv(event_age, event_dummy) ~ hazard_exposure_class, data = aged_15_17)
summary(km_hazard)

# p-value from log-rank test
logrank_test <- survdiff(Surv(event_age, event_dummy) ~ hazard_exposure_class, data = aged_15_17)
p_value <- 1 - pchisq(logrank_test$chisq, length(logrank_test$n) - 1)

# Plot with customization
surv_plot <- ggsurvplot(
  km_hazard, data = aged_15_17,
  xlim = c(10, max(aged_15_17$event_age, na.rm = TRUE)),
  ylim = c(0.7, 1),
  break.x.by = 1,
  conf.int = FALSE,
  pval = FALSE,         # No p-value
  pval.method = FALSE,  # No p-value method
  risk.table = TRUE,
  risk.table.height = 0.2,
  tables.y.text = FALSE,
  legend.labs = c("Low", "Medium", "High"),
  legend.title = "Hazard Exposure",
  xlab = "Age",
  ylab = "Probability of Not Being Married",
  title = "Marriage Timing by Hazard Exposure Level",
  # subtitle is removed
  palette = c("#00BA38", "#619CFF", "#F8766D"),
  ggtheme = theme_bw()
)
surv_plot

# Save plot with risk table
png("Graph 2 survival_curve_with_risktable.png", width = 1000, height = 800, res = 120)
print(surv_plot)
dev.off()

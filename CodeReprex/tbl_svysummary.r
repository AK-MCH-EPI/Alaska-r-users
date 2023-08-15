# Complex Survey Data Tables in gtsummary - NHANES (2017) Example
# R User Group
# 08/12/2023

library(gtsummary)
library(haven)
library(tidyverse)
library(survey)

# Demographics
demos <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DEMO_J.XPT")

# C-Reactive Protein (Inflammation Biomarker in Liver)
creact <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/HSCRP_J.XPT")

# Patient Health Questionnaire/Depression Screening
depression <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DPQ_J.XPT")

# Replace 9 and 7 with 0 in the specified columns
columns_to_replace <- c("DPQ010", "DPQ020", "DPQ030", "DPQ040", "DPQ050", 
                        "DPQ060", "DPQ070", "DPQ080", "DPQ090", "DPQ100")

# Set "Don't Know" and "Refused" to 0
depression[columns_to_replace][depression[columns_to_replace] == 9 |
                                 depression[columns_to_replace] == 7] <- 0

# Calculate PHQ_Total using rowSums
depression$PHQ_Total <- rowSums(depression[, columns_to_replace], na.rm = TRUE)

# Join NHANES Data
data_frames <- list(demos, creact, depression)

nhanes <- data_frames %>% reduce(full_join, by='SEQN')

# Filter to 18+ and Drop Non-MEC Examined Respondents
nhanes <- nhanes %>%
  filter(RIDAGEYR >=18 & WTMEC2YR != 0)

# Create Depression and C-Reactive Protein Levels
nhanes <- nhanes %>%
  mutate(depression = case_when(
    is.na(PHQ_Total) ~ NA_integer_,
    PHQ_Total <= 5 ~ 1,
    PHQ_Total >= 5 & PHQ_Total <= 9 ~ 2,
    PHQ_Total >= 10 & PHQ_Total <= 14 ~ 3,
    PHQ_Total >=15 & PHQ_Total <=19 ~ 4,
    TRUE ~ 5
  )) %>%
  mutate(creact = case_when(
    is.na(LBXHSCRP) ~ NA_integer_,
    LBXHSCRP <= 1 ~ 1,
    LBXHSCRP >= 1 & LBXHSCRP <= 3.01 ~ 2,
    LBXHSCRP >= 3.01 & LBXHSCRP <= 10 ~ 3,
    TRUE ~ 4
  ))

# Label Variables + Set As Factors 

# Gender
nhanes_clean <- data.frame(gender = factor(nhanes$RIAGENDR, levels = c(1, 2), 
                                           labels = c("Male", "Female")))

# Age 
nhanes_clean <- nhanes_clean %>%
  mutate(agecat = case_when(
    nhanes$RIDAGEYR >= 18 & nhanes$RIDAGEYR <= 29 ~ 1,
    nhanes$RIDAGEYR >= 30 & nhanes$RIDAGEYR <= 39 ~ 2,
    nhanes$RIDAGEYR >= 40 & nhanes$RIDAGEYR <= 49 ~ 3,
    nhanes$RIDAGEYR >= 50 & nhanes$RIDAGEYR <= 59 ~ 4,
    nhanes$RIDAGEYR >= 60 & nhanes$RIDAGEYR <= 69 ~ 5,
    nhanes$RIDAGEYR >= 70 ~ 6,
    TRUE ~ as.numeric(NA)  # Handle other cases with NA if needed
  ))

nhanes_clean$agecat <- factor(nhanes_clean$agecat, levels = c(1, 2, 3, 4, 5, 6),
                              labels = c("18-29", "30-39", "40-49", "50-59", "60-69",
                                         "70+"))


# Race 
nhanes_clean$race <- factor(nhanes$RIDRETH3, levels = c(1, 2, 3, 4, 6, 7),
                            labels = c("Mexican American", "Hispanic - Other",
                                       "White (Non-Hispanic)", "Black (Non-Hispanic)",
                                       "Asian (Non-Hispanic)", "Other or Multiracial"))
# Education
nhanes_clean$educat <- ifelse(nhanes$DMDEDUC2 %in% c(1, 2), 1,
                              ifelse(nhanes$DMDEDUC2 == 3, 2,
                                     ifelse(nhanes$DMDEDUC2 == 4, 3,
                                            ifelse(nhanes$DMDEDUC2 == 5, 4,
                                                   NA))))

nhanes_clean$educat <- factor(nhanes_clean$educat, levels = c(1, 2, 3, 4),
                              labels = c("Less than High School", "High School Diploma/GED",
                                         "Some College/Associates Degree", 
                                         "Bachelors Degree or Above"))
# Income
nhanes_clean$income <- factor(nhanes$INDHHIN2, levels = c(1, 2, 3, 4, 5, 6, 7, 
                                                          8, 9, 10, 12, 13, 14, 
                                                          15, 77, 99),
                              labels = c("$ 0-$4,999", "$ 5,000-$9,999", 
                                         "$10,000-$14,999", "$15,000-$19,999", 
                                         "$20,000-$24,999", "$25,000-$34,999",
                                         "$35,000-$44,999", "$45,000-$54,999", 
                                         "$55,000-$64,999", "$65,000-$74,999", 
                                         "$20,000 and Over", "Under $20,000", 
                                         "$75,000-$99,999", "$100,000 and Over", 
                                         "Refused", "Don't know"))
# Depression/PHQ

nhanes_clean$depression <- factor(nhanes$PHQ_Total, 
                           levels = c(1, 2, 3, 4, 5),
                           labels = c("Not Depressed", "Minimal Symptoms",
                           "Mild", "Moderate", "Severe"))

# C-Reactive Protein Levels 

nhanes_clean$creact <- factor(nhanes$creact, levels = c(1, 2, 3, 4),
                              labels = c("Less or Equal to 1mg/L", "1.01 to 3.00mg/L",
                                         "3.01 to 10.00mg/L", "Greater or Equal to 10mg/L"))

nhanes_clean$psu <- nhanes$SDMVPSU
nhanes_clean$strata <- nhanes$SDMVSTRA
nhanes_clean$weights <- nhanes$WTMEC2YR

nhanes_clean <- nhanes_clean %>% filter(!is.na(creact))

# Create survey design object

nhanes.design <- svydesign(
  id = ~psu,            # Primary sampling unit
  strata = ~strata,     # Strata
  weights = ~weights,   # Sampling weights
  nest = TRUE,
  data = nhanes_clean,
)

# Create tbl_svysummary object

nhanes_table <- tbl_svysummary(
  data = nhanes.design,
  by = "creact",
  percent = "column",
  include = c(agecat, educat, gender, race, income, depression),
  label = list(
    agecat = "Age",
    gender = "Gender",
    race = "Race",
    educat = "Education",
    income = "Income",
    depression = "Depression Level (PHQ Score)"
  )
 )  %>%
  add_p(all_categorical() ~ "svy.adj.chisq.test") 

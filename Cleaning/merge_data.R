# load libraries
library(tidyverse)
library(Hmisc)
library(lubridate)
library(readxl)

# set working directory
setwd("/Users/sackd/Library/CloudStorage/Box-Box/Vanderbilt University/PhD/Dissertation/Thesis/Analysis/Cleaning/")

# set load options for Load and Save
options(LoadPath = "../Data/")

# pull in redcap data
Load(redcap_cleaned)

# pull in drive intervention adherence data
Load(int_cleaned)

# pull in drive clinical data
Load(clindrive_cleaned)

# pull in openMRS data
Load(clin_openrms)

# pull in open mrs clinical data
## TBD Load(openmrs_cleaned)

# redcap and intervention data are long, with males and females individual, whereas clindrive data are wide (open MRS??)
# eventually want data to be wide, since I only care about outcomes in female participants

#### first merge redcap data with intervention adherence data

# want to left_join, but first check that all int_cleaned codigos are in redcap codigos
table(int_cleaned$codigo %in% redcap_cleaned$codigo)

# examine dataframe of all codigos not in the redcap tibble
extra_int_df <- int_cleaned[int_cleaned$codigo %nin% redcap_cleaned$codigo, ]

# these are all rows that that include 0 sessions, so either dropped out early (i.e. not included in the final study population)
# or individuals who where never recruited (but who have rows in the intervention adherence data in case we recruited that many)
## I am therefore comfortable proceeding with joining

# check for duplicates in redcap_cleaned codigo (should be 0)
redcap_cleaned$codigo[duplicated(redcap_cleaned$codigo)]

# check for duplicates in int_cleaned codigo
int_cleaned$codigo[duplicated(int_cleaned$codigo)]

# redcap and intervention merge
red_int_merge <- left_join(redcap_cleaned, int_cleaned, by = "codigo")

### now need to split from long to wide by sex

# start by splitting into female and male dfs
# I considered using the pivot commands, but this is one extra line of code that seemed more interpretable
fem_df <- red_int_merge %>% filter(sex == "Female")
m_df <- red_int_merge %>% filter(sex == "Male")

# now merge by record_id
wide_red_int <- left_join(fem_df, m_df,
                          by = "record_id",
                          suffix = c(".female", ".male"))

#### now join with clinical data from drive

# first check overlap in codigos for males and females
table(clindrive_cleaned$codigo.female %in% wide_red_int$codigo.female)
table(clindrive_cleaned$codigo.male %in% wide_red_int$codigo.male)

# take a look at the data missing
extra_clindrive_df <- clindrive_cleaned[(clindrive_cleaned$codigo.female %nin% wide_red_int$codigo.female |
                                          clindrive_cleaned$codigo.male %nin% wide_red_int$codigo.male), ]

# these are all rows that that include no information, so either dropped out early (i.e. not included in the final study population)
# or individuals who where never recruited (but who have rows in the intervention adherence data in case we recruited that many)
## I am therefore comfortable proceeding with joining by codigo.female and codigo.male

red_int_clindrive <- left_join(wide_red_int, clindrive_cleaned,
                               by = c("codigo.female", "codigo.male"))

# now just have to add in openmrs clinical data!

# start with some checks
sum(clin_openrms$codigo %in% red_int_clindrive$codigo.female)
sum(clin_openrms$pt_id %in% red_int_clindrive$pt_id.female)

red_int_clin_open <- left_join(red_int_clindrive, clin_openrms,
                               by = c("codigo.female" = "codigo",
                                      "pt_id.female" = "pt_id"))

# get some missingness information
table(red_int_clin_open$fp_cont_method, useNA = "always")
table(red_int_clin_open$fp_cont_date, useNA = "always")
table(red_int_clin_open$fem_fp_use, useNA = "always")
table(red_int_clin_open$fem_fp_start_date, useNA = "always")

### UPDATE OUTCOME ###

# will create a deidentified dataset for manual adjudication

# first add an unidentified id to red_int_clin_open
red_int_clin_open <- red_int_clin_open %>%
  rowid_to_column(var = "ID")

# now subset to only relevant columns for adjudication
# delivery date, 12 months from delivery date, fp_cont_date, fp_cont_method, fem_fp_use, fem_fp_method, fem_fp_start_date
fp_check <- red_int_clin_open %>%
  mutate(outcome_time = delivery_date + 365) %>%
  select(ID, delivery_date, outcome_time, fem_fp_use:fem_fp_start_date, fp_cont_method, fp_cont_date) %>%
  mutate(fp_cont_date = as.Date(format(fp_cont_date, "%Y-%m-%d")))

# rename columns as appropriate
fp_check <- upData(fp_check,
                   rename = c(fem_fp_use = "Drive_FP_Use_F",
                              fem_fp_type = "Drive_FP_Type_F",
                              fem_fp_start_date = "Drive_FP_Date",
                              fp_cont_date = "OpenMRS_FP_Date",
                              fp_cont_method = "OpenMRS_FP_Method"),
                   labels = c(outcome_time = "Closest Outcome Date"))

describe(fp_check)

# add some colums for adjudication
fp_check <- fp_check %>%
  mutate(FP_use_final = "",
         FP_method_final = "",
         FP_date_final = "",
         drive_to_outcome = as.numeric(Drive_FP_Date - outcome_time),
         open_to_outcome = as.numeric(OpenMRS_FP_Date - outcome_time))

# write to csv
write_csv(fp_check, "../Data/Outcome Adjudication/fp_check.csv")

# will make it a CSV and send it around

# then read in new version
# subset to only ID and *_final columns
fp_adj <- read_excel("../Data/Outcome Adjudication/fp_check_final.xlsx") %>%
  select(ID, ends_with("_final")) %>%
  mutate(FP_date_final = as.Date(format(FP_date_final, "%Y-%m-%d")))

describe(fp_adj)

# left_join back with red_int_clin_open

red_int_clin_open <- left_join(red_int_clin_open, fp_adj, by = "ID")

# add in outcome columns

# first figure out the timing of the outcomes..

# see how many of the start dates are before the delivery dates
table(red_int_clin_open$fp_initiation < red_int_clin_open$delivery_date)
# before enrollment dates
table(red_int_clin_open$fp_initiation < red_int_clin_open$reg_date.female)

# so clearly not a useful column, since 855 of them are before the delivery date

# what about the second FP variable
table(red_int_clin_open$fp_cont_date < red_int_clin_open$delivery_date)
# so 22 of these are also before the delivery date...better, but not great..

# so I will need to create a two column with useful information about an outcome
red_int_clin_open <- red_int_clin_open %>%
  mutate(fp_cont_valid = ifelse(FP_date_final > delivery_date | is.na(FP_date_final), 1, 0), # column to tell me if FP continuation date is after delivery date
         fp_final_binary = ifelse(fp_cont_valid == 1, FP_use_final, NA), # column of FP use that happens after the delivery date (Y/N)
         fp_final_date = if_else(fp_cont_valid == 1, as.Date(FP_date_final), as.Date(NA)), # column of FP start date that happens after the delivery date (date)
         fp_final_type = ifelse(fp_cont_valid == 1, FP_method_final, NA), # column of FP type that happens after the delivery date (factor)
         fp_final_type = factor(fp_final_type, levels = c("None", "Injectable", "Pill", "IUD", "Implant", "Condom"))) 

# now recheck timing
# see how many of the start dates are before the delivery dates
table(red_int_clin_open$fp_final_date < red_int_clin_open$delivery_date) # should all be false
# before enrollment dates
table(red_int_clin_open$fp_final_date < red_int_clin_open$reg_date.female) # should all be false

# now add time to family planning start (from delivery date) and time to repeat pregnancy
red_int_clin_open <- red_int_clin_open %>%
  mutate(time_to_fp = as.numeric(fp_final_date - delivery_date),
         time_to_rep_preg = ifelse(as.numeric(as.Date(rep_preg_date) - delivery_date) > 0 & !is.na(as.numeric(as.Date(rep_preg_date) - delivery_date)),
                                   as.numeric(as.Date(rep_preg_date) - delivery_date), NA),
         rep_preg_12_mo = ifelse(rep_preg == "Yes" & time_to_rep_preg < 366, 1, 0),
         time_to_rep_preg_12_mo = ifelse(rep_preg == "Yes" & time_to_rep_preg < 366, time_to_rep_preg, NA))

# check the numbers

# Final version of intervention attendance for aim 3
red_int_clin_open <- red_int_clin_open %>%
  mutate(fem_int_tot = peer_comp.female + skills_comp.female,
         fem_int_tot_sch = peer_sch.female + skills_sch.female,
         fem_prop_int = (peer_comp.female + skills_comp.female) / (peer_sch.female + skills_sch.female),
         m_int_tot = peer_comp.male + skills_comp.male,
         m_int_tot_sch = peer_sch.male + skills_sch.male,
         m_prop_int = (peer_comp.male + skills_comp.male) / (peer_sch.male + skills_sch.male))

# remove folks who did not want to be followed
red_int_clin_open_fin <- red_int_clin_open %>%
  filter(study_status.female != "Formally withdrew") %>%
  filter(study_status.male != "Formally withdrew")

# print a list of folks who withdrew for reference (for aim 1)
red_int_clin_open %>%
  filter(study_status.female == "Formally withdrew" | study_status.male == "Formally withdrew") %>%
  select(codigo.female, codigo.male, study_status.female, study_status.male)

# now rename final version as final
final <- red_int_clin_open_fin 

# then can save final cleaned tibble
Save(final)
# can pull this file into each subsequent chapter RMarkdown file and edit as appropriate


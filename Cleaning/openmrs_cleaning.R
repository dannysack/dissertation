# load libraries
library(tidyverse)
library(Hmisc)
library(lubridate)
library(readxl)

# set working directory
setwd("/Users/sackd/Library/CloudStorage/Box-Box/Vanderbilt University/PhD/Dissertation/Thesis/Analysis/Cleaning/")

# first I have to pull in clinical data with ART information
art <- read_excel("../Data/OpenMRS/baseline_clinical.xlsx")

# read in master linkage dataset
master <- read_csv("../Data/OpenMRS/master.csv")

# now confirm overlap
sum(master$nid %in% art$nid)
# should be length of both datasets, so we are all good to continue!!

# now have to write a function for cleaning each subfolder
# will need a path as an input, and various functions within it that are specific to each type of file
# will need to make each file wide (from long) and then match with master file in the subfolder with patient IDs and nids
# finally will have to row-bind all of those files together

# first have to move all empty csv files into thier own folder in each subfolder otherwise it breaks

# function to calculate instance number
inst_num <- function(df, id){
  df$num <- NA
  for(i in 1:nrow(df)){
    df$num[i] <- sum(df[[id]][i] == df[[id]][1:i])
  }
  df
}

# pull in all files from each sub-folder
clean_openmrs <- function(path) {
  files <- map(list.files(path, pattern = "*.csv"),
               ~ read_csv(paste0(path, .x)))
  # referent file (don't transform to wide)
  ref <- files[[1]]
  # transform others to wide, may not do anything
  # family planning dates
  fp_dates <- files[[2]] %>% inst_num(., "patient_id") %>%
    mutate(fp_number = paste0("fp_vis_", num)) %>%
    select(patient_id, fp_date, fp_number) %>%
    pivot_wider(id_cols = patient_id, names_from = fp_number, values_from = fp_date)
  # lmp
  lmp <- files[[3]] %>% inst_num(., "patient_id") %>%
    mutate(lmp_number = paste0("lmp_", num)) %>%
    select(patient_id, hosp_cpn_last_menstrual_period_date, lmp_number) %>%
    pivot_wider(id_cols = patient_id, names_from = lmp_number, values_from = hosp_cpn_last_menstrual_period_date)
  # status ... NOT QUITE SURE WHAT TO DO WITH THIS ONE YET...
  stat <- files[[4]] %>% inst_num(., "patient_id") %>%
    mutate(stat_number = paste0("stat_", num)) %>%
    select(patient_id, patient__status_date, stat_number) %>%
    pivot_wider(id_cols = patient_id, names_from = stat_number, values_from = patient__status_date)
  # method_type
  fp_met <- files[[5]] %>% inst_num(., "patient_id") %>%
    mutate(fp_met_number = paste0("fp_met_", num)) %>%
    select(patient_id, type_date, type_of_method, fp_met_number) %>%
    pivot_wider(id_cols = patient_id, names_from = fp_met_number, values_from = c(type_date, type_of_method))
  # hc visit dates
  visit_date <- files[[6]] %>% inst_num(., "patient_id") %>%
    mutate(visit_number = paste0("visit_", num)) %>%
    select(patient_id, visit_date, visit_number) %>%
    pivot_wider(id_cols = patient_id, names_from = visit_number, values_from = visit_date)
  
  # now have to bind all files with ref with left_join as output file
  final <- ref %>%
    left_join(fp_dates, by = "patient_id") %>%
    left_join(lmp, by = "patient_id") %>%
    left_join(stat, by = "patient_id") %>%
    left_join(fp_met, by = "patient_id") %>%
    left_join(visit_date, by = "patient_id")
  final
}

# now have to create a function to run through all paths to data and combine them all in rowbind
paths <- c("../Data/OpenMRS/Gile/", 
           "../Data/OpenMRS/Inhassunge/Inhassunge_Palane/",
           "../Data/OpenMRS/Inhassunge/Inhassunge_Sede/",
           "../Data/OpenMRS/Maganja da Costa/",
           "../Data/OpenMRS/Mocubela/",
           "../Data/OpenMRS/Namacurra/Namacurra_malei/",
           "../Data/OpenMRS/Namacurra/Namacurra_Mbaua/",
           "../Data/OpenMRS/Namacurra/Namacurra_Mixixine/",
           "../Data/OpenMRS/Namacurra/Namacurra_Muebele/",
           "../Data/OpenMRS/Namacurra/Namacurra_Mugubia/",
           "../Data/OpenMRS/Pebane/")

# create list
openmrs_mainls <- map(paths, ~ clean_openmrs(.x))

# now fix one instance where colum read as logical instead of character
openmrs_mainls[[4]]$openmrs_gender <- "F"
openmrs_mainls[[6]]$openmrs_gender <- "F"
openmrs_mainls[[7]]$openmrs_gender <- "F"
openmrs_mainls[[8]]$openmrs_gender <- "F"
openmrs_mainls[[9]]$openmrs_gender <- "F"
openmrs_mainls[[10]]$openmrs_gender <- "F"

# now bring lists together
openmrs_main <- map_df(openmrs_mainls, ~bind_rows(.x))

# now some special functions for troubled paths
# need special treatment
# "../Data/OpenMRS/Inhassunge/Inhassunge_Gonhane/" # no lmp
# "../Data/OpenMRS/Inhassunge/Inhassunge_Bingagira/" # no lmp
# "../Data/OpenMRS/Quelimane_Madal/" # no lmp or type of method

# function for no lmps
clean_openmrs_nolmp <- function(path) {
  files <- map(list.files(path, pattern = "*.csv"),
               ~ read_csv(paste0(path, .x)))
  # referent file (don't transform to wide)
  ref <- files[[1]]
  # transform others to wide, may not do anything
  # family planning dates
  fp_dates <- files[[2]] %>% inst_num(., "patient_id") %>%
    mutate(fp_number = paste0("fp_vis_", num)) %>%
    select(patient_id, fp_date, fp_number) %>%
    pivot_wider(id_cols = patient_id, names_from = fp_number, values_from = fp_date)
  # status ... NOT QUITE SURE WHAT TO DO WITH THIS ONE YET...
  stat <- files[[3]] %>% inst_num(., "patient_id") %>%
    mutate(stat_number = paste0("stat_", num)) %>%
    select(patient_id, patient__status_date, stat_number) %>%
    pivot_wider(id_cols = patient_id, names_from = stat_number, values_from = patient__status_date)
  # method_type
  fp_met <- files[[4]] %>% inst_num(., "patient_id") %>%
    mutate(fp_met_number = paste0("fp_met_", num)) %>%
    select(patient_id, type_date, type_of_method, fp_met_number) %>%
    pivot_wider(id_cols = patient_id, names_from = fp_met_number, values_from = c(type_date, type_of_method))
  # hc visit dates
  visit_date <- files[[5]] %>% inst_num(., "patient_id") %>%
    mutate(visit_number = paste0("visit_", num)) %>%
    select(patient_id, visit_date, visit_number) %>%
    pivot_wider(id_cols = patient_id, names_from = visit_number, values_from = visit_date)
  
  # now have to bind all files with ref with left_join as output file
  final <- ref %>%
    left_join(fp_dates, by = "patient_id") %>%
    left_join(stat, by = "patient_id") %>%
    left_join(fp_met, by = "patient_id") %>%
    left_join(visit_date, by = "patient_id")
  final
}

no_lmps <- map_df(c("../Data/OpenMRS/Inhassunge/Inhassunge_Gonhane/",
                 "../Data/OpenMRS/Inhassunge/Inhassunge_Bingagira/"), ~ clean_openmrs_nolmp(.x))

# now make one for no lmp or type of method
clean_openmrs_quel <- function(path) {
  files <- map(list.files(path, pattern = "*.csv"),
               ~ read_csv(paste0(path, .x)))
  # referent file (don't transform to wide)
  ref <- files[[1]]
  # transform others to wide, may not do anything
  # family planning dates
  fp_dates <- files[[2]] %>% inst_num(., "patient_id") %>%
    mutate(fp_number = paste0("fp_vis_", num)) %>%
    select(patient_id, fp_date, fp_number) %>%
    pivot_wider(id_cols = patient_id, names_from = fp_number, values_from = fp_date)
  # status ... NOT QUITE SURE WHAT TO DO WITH THIS ONE YET...
  stat <- files[[3]] %>% inst_num(., "patient_id") %>%
    mutate(stat_number = paste0("stat_", num)) %>%
    select(patient_id, patient__status_date, stat_number) %>%
    pivot_wider(id_cols = patient_id, names_from = stat_number, values_from = patient__status_date)
  # hc visit dates
  visit_date <- files[[4]] %>% inst_num(., "patient_id") %>%
    mutate(visit_number = paste0("visit_", num)) %>%
    select(patient_id, visit_date, visit_number) %>%
    pivot_wider(id_cols = patient_id, names_from = visit_number, values_from = visit_date)
  
  # now have to bind all files with ref with left_join as output file
  final <- ref %>%
    left_join(fp_dates, by = "patient_id") %>%
    left_join(stat, by = "patient_id") %>%
    left_join(visit_date, by = "patient_id")
  final
}

quel <- clean_openmrs_quel("../Data/OpenMRS/Quelimane_Madal/") %>%
  mutate(openmrs_gender = "F")

# now bind them all together (sex is all female, there seem to be some typos)
openmrs_cleaned <- openmrs_main %>%
  bind_rows(no_lmps) %>%
  bind_rows(quel) %>%
  mutate(openmrs_gender = "F")

# now need to combine it with art, but only for females
fem_art <- art %>% filter(sex == "F")

# now need to combine fem_art with openmrs_cleaned by nid
sum(fem_art$nid %in% openmrs_cleaned$nid)
# only one case where nids do not match...

# find instance
for(i in 1:nrow(fem_art)){
  if(fem_art$nid[i] %nin% openmrs_cleaned$nid){
    print(fem_art[i, ])
  }
}
# it is for ALTF046
for(i in 1:nrow(openmrs_cleaned)){
  if(openmrs_cleaned$nid[i] %nin% fem_art$nid){
    print(openmrs_cleaned[i, ])
  }
}
# it is a participant without an id, so I am just going to take the loss

clin_openrms <- left_join(fem_art, openmrs_cleaned, by = "nid") %>% # now subset to key columns
  select(`Codigo do estudo`, Novo_id_Participante, nid, WHO_clinical_stage_at_art_initiation,
         weight, height...22, bmi, date_of_birth, family_planning, fp_vis_1:visit_2)

# rename columns and give them labels
clin_openrms <- upData(clin_openrms,
                       rename = c(`Codigo do estudo` = "codigo",
                                  Novo_id_Participante = "pt_id",
                                  WHO_clinical_stage_at_art_initiation = "who_stage",
                                  `height...22` = "height",
                                  fp_vis_1 = "fp_initiation",
                                  lmp_1 = "lmp_date",
                                  stat_1 = "status_date",
                                  type_date_fp_met_1 = "fp_cont_date",
                                  type_of_method_fp_met_1 = "fp_cont_method"),
                       labels = c(codigo = "Patient ID for Drive Matching",
                                  pt_id = "Patient ID for OpenMRS Matching",
                                  nid = "NID for OpenMRS Matching",
                                  who_stage = "WHO HIV Clinical Stage",
                                  weight = "Weight",
                                  height = "Height",
                                  bmi = "Body Mass Index",
                                  date_of_birth = "Date of Birth",
                                  family_planning = "Initiated FP",
                                  fp_initiation = "FP Intiation Date",
                                  lmp_date = "Last Menstrual Period",
                                  status_date = "Unclear Status Date",
                                  fp_cont_date = "FP Continuation Visit Date",
                                  fp_cont_method = "FP Continuation Method",
                                  visit_1 = "Visit Date 1",
                                  visit_2 = "Visit Date 2"),
                       units = c(weight = "kg",
                                 height = "meters",
                                 bmi = "kg/m^2"))

# save
options(LoadPath = "../Data/")
Save(clin_openrms)

# also need dataset with male art and male height, weight, and bmi
male_clin <- art %>% filter(sex == "M") %>%
  select(`Codigo do estudo`, Novo_id_Participante, nid, WHO_clinical_stage_at_art_initiation,
         weight, height...22, bmi)

# rename columns
male_clin <- upData(male_clin,
       rename = c(`Codigo do estudo` = "codigo",
                  Novo_id_Participante = "pt_id",
                  WHO_clinical_stage_at_art_initiation = "who_stage",
                  `height...22` = "height"),
       labels = c(codigo = "Patient ID for Drive Matching",
                  pt_id = "Patient ID for OpenMRS Matching",
                  nid = "NID for OpenMRS Matching",
                  who_stage = "WHO HIV Clinical Stage",
                  weight = "Weight",
                  height = "Height",
                  bmi = "Body Mass Index"),
       units = c(weight = "kg",
                 height = "meters",
                 bmi = "kg/m^2"))

# now need to save
Save(male_clin)

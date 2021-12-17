# load libraries
library(tidyverse)
library(Hmisc)
library(lubridate)

# set working directory
setwd("/Users/sackd/Library/CloudStorage/Box-Box/Vanderbilt University/PhD/Dissertation/Thesis/Analysis/Cleaning/")

# first have to move all columns onto one long csv file because of how messy column names are...
# made column names on that for simplicity, also deleted unnecessary columns
# this is obviously not the most reproducible way to do this, but the messy nature of the 
# original files makes it a necessity 

# keep the following columns
# female codigo, male codigo, 
# estimated delivery date, delivery date, delivery outcome, number of babies, baby sex
# female FP use, female FP type, female FP start date, male FP use, male FP type, male FP date

# use the following column names
#"codigo.female", "codigo.male",
# "est_deliv_date", "delivery_date", "delivery_outcome", "num_babies", "baby_sex", "rep_preg", "rep_preg_date",
# "fem_fp_use", "fem_fp_type", "fem_fp_start_date", "m_fp_use", "m_fp_type", "m_fp_date" 

# All data from September 2021

clindrive_df <- read_csv("../Data/Clinical Drive Data/November 2021/clin_drive_nov2021.csv")

# function to parse multiple dates
# found on stack overflow https://stackoverflow.com/questions/13764514/how-to-change-multiple-date-formats-in-same-column
multidate <- function(data, formats){
  a<-list()
  for(i in 1:length(formats)){
    a[[i]]<- as.Date(data,format=formats[i])
    a[[1]][!is.na(a[[i]])]<-a[[i]][!is.na(a[[i]])]
  }
  a[[1]]
}

# vector of date options in priority order
dateopts <- c("%d-%m-%Y", "%d/%m/%y")

# parse a few of the columns as dates
clindrive_df <- clindrive_df %>%
  mutate(est_deliv_date = multidate(est_deliv_date, dateopts),
         delivery_date = multidate(delivery_date, dateopts),
         rep_preg_date = multidate(rep_preg_date, c("%d-%m-%Y", "%d/%m/%y", "%d/%m/%Y")),
         fem_fp_start_date = multidate(fem_fp_start_date, dateopts),
         m_fp_date = multidate(m_fp_date, dateopts))

# convert outcome from Portuguese to English

# start with delivery_outcome
table(clindrive_df$delivery_outcome, useNA = "always")
clindrive_df$delivery_outcome[clindrive_df$delivery_outcome == "Aborto"] <- "Miscarriage"
clindrive_df$delivery_outcome[clindrive_df$delivery_outcome == "Não disponível"] <- NA
clindrive_df$delivery_outcome[clindrive_df$delivery_outcome == "Nascido morto"] <- "Stillborn"
clindrive_df$delivery_outcome[clindrive_df$delivery_outcome == "Nascido vivo"] <- "Live Birth"
table(clindrive_df$delivery_outcome, useNA = "always")

# female FP use
table(clindrive_df$fem_fp_use, useNA = "always")
clindrive_df$fem_fp_use[clindrive_df$fem_fp_use == "Informação não disponível"] <- NA
clindrive_df$fem_fp_use[clindrive_df$fem_fp_use == "Não"] <- "No"
clindrive_df$fem_fp_use[clindrive_df$fem_fp_use == "Sim"] <- "Yes"

# make it a factor
clindrive_df$fem_fp_use <- factor(clindrive_df$fem_fp_use, levels = c("No", "Yes"))
table(clindrive_df$fem_fp_use, useNA = "always")

# female FP type
table(clindrive_df$fem_fp_type, useNA = "always")
clindrive_df$fem_fp_type[clindrive_df$fem_fp_type == "DIU"] <- "IUD"
clindrive_df$fem_fp_type[clindrive_df$fem_fp_type == "Implante"] <- "Implant"
clindrive_df$fem_fp_type[clindrive_df$fem_fp_type == "Injectável"] <- "Injectable"
clindrive_df$fem_fp_type[clindrive_df$fem_fp_type == "Pílula"] <- "Pill"
clindrive_df$fem_fp_type[clindrive_df$fem_fp_type == "Preservativos"] <- "Condom"

# make it a factor
clindrive_df$fem_fp_type <- factor(clindrive_df$fem_fp_type, levels = c("Pill", "Injectable", "IUD", "Condom", "Implant"))
table(clindrive_df$fem_fp_type, useNA = "always")

# male FP use
table(clindrive_df$m_fp_use, useNA = "always")
clindrive_df$m_fp_use[clindrive_df$m_fp_use == "Informação não disponível"] <- NA
clindrive_df$m_fp_use[clindrive_df$m_fp_use == "Não"] <- "No"
clindrive_df$m_fp_use[clindrive_df$m_fp_use == "Sim"] <- "Yes"

# make it a factor
clindrive_df$m_fp_use <- factor(clindrive_df$m_fp_use, levels = c("No", "Yes"))
table(clindrive_df$m_fp_use, useNA = "always")

# male FP type
table(clindrive_df$m_fp_type, useNA = "always")
clindrive_df$m_fp_type[clindrive_df$m_fp_type == "Preservativos"] <- "Condom"
table(clindrive_df$m_fp_type, useNA = "always")

# repeat pregnancy
table(clindrive_df$rep_preg, useNA = "always")
clindrive_df$rep_preg[clindrive_df$rep_preg == "Sim"] <- "Yes"
clindrive_df$rep_preg[clindrive_df$rep_preg == "Nao"] <- "No"
clindrive_df$rep_preg <- factor(clindrive_df$rep_preg, levels = c("No", "Yes"))
table(clindrive_df$rep_preg, useNA = "always")

# save to different tibble for export
clindrive_cleaned <- clindrive_df

# add labels
label(clindrive_cleaned$codigo.female) <- "Patient ID for Drive Matching"
label(clindrive_cleaned$codigo.male) <- "Patient ID for Drive Matching"
label(clindrive_cleaned$est_deliv_date) <- "Estimated Delivery Date"
label(clindrive_cleaned$delivery_date) <- "Actual Delivery Date"
label(clindrive_cleaned$delivery_outcome) <- "Delivery Outcome"
label(clindrive_cleaned$num_babies) <- "Number of Children Delivered"
label(clindrive_cleaned$baby_sex) <- "Child's Sex"
label(clindrive_cleaned$rep_preg) <- "Repeat Pregnancy"
label(clindrive_cleaned$rep_preg_date) <- "Repeat Pregnancy Date"
label(clindrive_cleaned$fem_fp_use) <- "Female Contraceptive Use"
label(clindrive_cleaned$fem_fp_type) <- "Female Contraceptive Type"
label(clindrive_cleaned$fem_fp_start_date) <- "Female Contraceptive Start Date"
label(clindrive_cleaned$m_fp_use) <- "Male Contraceptive Use"
label(clindrive_cleaned$m_fp_type) <- "Male Contraceptive Type"
label(clindrive_cleaned$m_fp_date) <- "Male Contraceptive Date"

# save .rda file for merging to "Date" folder
options(LoadPath = "../Data/")
Save(clindrive_cleaned)

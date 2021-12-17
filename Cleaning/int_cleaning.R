# load libraries
library(tidyverse)
library(Hmisc)
library(lubridate)
library(readxl)

# set working directory
setwd("/Users/sackd/Library/CloudStorage/Box-Box/Vanderbilt University/PhD/Dissertation/Thesis/Analysis/Cleaning/")

# All data from September 2021

# path name for each workbook
path_nam <- "../Data/Intervention Drive Data/November 2021/Monitoria das Entrevistas e Sessoes Outubro-Novembro 2021 Namacurra.xlsx"
path_gil <- "../Data/Intervention Drive Data/November 2021/Monitoria de Entrevistas e Sessoes Outubro e Novemtbro 2021 Gile_eg.xlsx"
path_mag <- "../Data/Intervention Drive Data/November 2021/Monitoria de Entrevistas e Sessoes de Outubro e Novembro  2021_Maganja.xlsx"
path_moc <- "../Data/Intervention Drive Data/November 2021/Monitoria de Entrevistas e Sessoes de Outubro e Novembro 2021_Mocubela.xlsx"
path_inh <- "../Data/Intervention Drive Data/November 2021/Monitoria de entrevistas e Sessoes  Outubro e Novembro 2021_Inhassunge.xlsx"
path_que <- "../Data/Intervention Drive Data/November 2021/Monitoria Entrevista  e Sessoes_ Outubro e Novembro de 2021_Quelimane.xlsx"
path_peb <- "../Data/Intervention Drive Data/November 2021/Monitoria Entrevistas e sessoes Outubro e Novembro de 2021 Pebane.xlsx"

# different types of sheets
cons <- "Acons"
peer <- "Apoio"

# function to get a vector of sheet names
s_names <- function(path, type){
  sheets <- excel_sheets(path)
  sheet_names <- grep(type, sheets, value = TRUE)
  sheet_names
}

# function to create dataframe from one path
path_df <- function(path, type) {
  map2_df(s_names(path, type), # list of sheet names
          rep(path, length(s_names(path, type))), # repeat the path the sheet number of times
          function(.x, .y) {
            # read excel file with matching sheets
            read_excel(.y, .x)
          })
}


# skills sessions "Acons", have to go through and edit dates in excel file to work to remove misentered dates and "missing" written as text
# after each successful line, check that all date columns are pulled in as dates!
cons_nam <- path_df(path_nam, cons)
str(cons_nam %>% select(starts_with("Data")))
# should all have type "POSIXct"

cons_gil <- path_df(path_gil, cons)
str(cons_gil %>% select(starts_with("Data")))
# should all have type "POSIXct"

cons_mag <- path_df(path_mag, cons)
str(cons_mag %>% select(starts_with("Data")))
# should all have type "POSIXct"

cons_moc <- path_df(path_moc, cons)
str(cons_moc %>% select(starts_with("Data")))
# should all have type "POSIXct"

cons_inh <- path_df(path_inh, cons)
str(cons_inh %>% select(starts_with("Data")))
# should all have type "POSIXct"

cons_que <- path_df(path_que, cons)
str(cons_que %>% select(starts_with("Data")))
# should all have type "POSIXct"

cons_peb <- path_df(path_peb, cons)
str(cons_peb %>% select(starts_with("Data")))
# should all have type "POSIXct"
# To check, change _prefix to appropriate clinic

# now row bind them all
skills_df <- bind_rows(cons_nam, cons_gil, cons_mag, cons_moc, cons_inh, cons_que, cons_peb)

# can now check all for type "POSIXct"
str(skills_df %>% select(starts_with("Data")))

# peer support sessions "Apoio", have to go through and edit dates in excel file to work to remove mis-entered dates and "missing" written as text
# after each successful line, check that all date columns are pulled in as dates!
peer_nam <- path_df(path_nam, peer)
str(peer_nam %>% select(starts_with("Data")))
# should all have type "POSIXct"

peer_gil <- path_df(path_gil, peer)
str(peer_gil %>% select(starts_with("Data")))
# should all have type "POSIXct"

peer_mag <- path_df(path_mag, peer)
str(peer_mag %>% select(starts_with("Data")))
# should all have type "POSIXct"

peer_moc <- path_df(path_moc, peer)
str(peer_moc %>% select(starts_with("Data")))
# should all have type "POSIXct"

peer_inh <- path_df(path_inh, peer)
str(peer_inh %>% select(starts_with("Data")))
# should all have type "POSIXct"

peer_que <- path_df(path_que, peer)
str(peer_que %>% select(starts_with("Data")))
# should all have type "POSIXct"

peer_peb <- path_df(path_peb, peer)
str(peer_peb %>% select(starts_with("Data")))
# should all have type "POSIXct"

# now row bind them all
peer_df <- bind_rows(peer_nam, peer_gil, peer_mag, peer_moc, peer_inh, peer_que, peer_peb)

# can now check all for type "POSIXct"
str(peer_df %>% select(starts_with("Data")))

# now need to classify things as scheduled and who attended

# skills sessions
skills_df <- skills_df %>%
  mutate(sch1 = ifelse(!is.na(`Data marcada para 1ª sessão de Acons.`), 1, 0),
         fem1 = ifelse(str_detect(`Quem participou na 1ª sessão de Acons.`, "Ambos|Mulher"), 1, 0),
         m1 = ifelse(str_detect(`Quem participou na 1ª sessão de Acons.`, "Ambos|Homem"), 1, 0),
         sch2 = ifelse(!is.na(`Data marcada para 2ª sessão de Acons.`), 1, 0),
         fem2 = ifelse(str_detect(`Quem participou na 2ª sessão de Acons.`, "Ambos|Mulher"), 1, 0),
         m2 = ifelse(str_detect(`Quem participou na 2ª sessão de Acons.`, "Ambos|Homem"), 1, 0),
         sch3 = ifelse(!is.na(`Data marcada para 3ª sessão de Acons.`), 1, 0),
         fem3 = ifelse(str_detect(`Quem participou na 3ª sessão de Acons.`, "Ambos|Mulher"), 1, 0),
         m3 = ifelse(str_detect(`Quem participou na 3ª sessão de Acons.`, "Ambos|Homem"), 1, 0),
         sch4 = ifelse(!is.na(`Data marcada para 4ª sessão de Acons.`), 1, 0),
         fem4 = ifelse(str_detect(`Quem participou na 4ª sessão de Acons.`, "Ambos|Mulher"), 1, 0),
         m4 = ifelse(str_detect(`Quem participou na 4ª sessão de Acons.`, "Ambos|Homem"), 1, 0),
         sch5 = ifelse(!is.na(`Data marcada para 5ª sessão de Acons.`), 1, 0),
         fem5 = ifelse(str_detect(`Quem participou na 5ª sessão de Acons.`, "Ambos|Mulher"), 1, 0),
         m5 = ifelse(str_detect(`Quem participou na 5ª sessão de Acons.`, "Ambos|Homem"), 1, 0),
         sch6 = ifelse(!is.na(`Data marcada para 6ª sessão de Acons.`), 1, 0),
         fem6 = ifelse(str_detect(`Quem participou na 6ª sessão de Acons.`, "Ambos|Mulher"), 1, 0),
         m6 = ifelse(str_detect(`Quem participou na 6ª sessão de Acons.`, "Ambos|Homem"), 1, 0)) %>%
  rowwise() %>%
  mutate(tot_sch = sum(sch1, sch2, sch3, sch4, sch5, sch6, na.rm = TRUE),
         tot_fem = sum(fem1, fem2, fem3, fem4, fem5, fem6, na.rm = TRUE),
         tot_m = sum(m1, m2, m3, m4, m5, m6, na.rm = TRUE),
         fem_prop = tot_fem / tot_sch,
         m_prop = tot_m / tot_sch)

# subset and then split to merge
skills_merge_prep <- skills_df %>% select(`Código de estudo da participante feminina`,
                                          `Código de estudo do participante masculino`, 
                                          sch1:m_prop) %>%
  rename(codigo_f = `Código de estudo da participante feminina`,
         codigo_m = `Código de estudo do participante masculino`)

skills_f <- skills_merge_prep %>%
  select(codigo_f, sch1, fem1, sch2, fem2, sch3, fem3, sch4, fem4, sch5, fem5, sch6, fem6, tot_sch, tot_fem, fem_prop) %>%
  rename(codigo = codigo_f)
skills_m <- skills_merge_prep %>%
  select(codigo_m, sch1, m1, sch2, m2, sch3, m3, sch4, m4, sch5, m5, sch6, m6, tot_sch, tot_m, m_prop) %>%
  rename(codigo = codigo_m)

# tibble to merge with other data
skills_merge <- bind_rows(skills_f, skills_m)

# now do the same with peer sessions
peer_df <- peer_df %>%
  mutate(sch1p = ifelse(!is.na(`Data marcada para 1ª sessão de Apoio?`), 1, 0),
         fem1p = ifelse(str_detect(`Quem participou na 1ª sessão de Apoio?`, "Ambos|Mulher"), 1, 0),
         m1p = ifelse(str_detect(`Quem participou na 1ª sessão de Apoio?`, "Ambos|Homem"), 1, 0),
         sch2p = ifelse(!is.na(`Data marcada para 2ª sessão de Apoio?`), 1, 0),
         fem2p = ifelse(str_detect(`Quem participou na 2ª sessão de Apoio?`, "Ambos|Mulher"), 1, 0),
         m2p = ifelse(str_detect(`Quem participou na 2ª sessão de Apoio?`, "Ambos|Homem"), 1, 0),
         sch3p = ifelse(!is.na(`Data marcada para 3ª sessão de Apoio?`), 1, 0),
         fem3p = ifelse(str_detect(`Quem participou na 3ª sessão de Apoio?`, "Ambos|Mulher"), 1, 0),
         m3p = ifelse(str_detect(`Quem participou na 3ª sessão de Apoio?`, "Ambos|Homem"), 1, 0),
         sch4p = ifelse(!is.na(`Data marcada para 4ª sessão de Apoio?`), 1, 0),
         fem4p = ifelse(str_detect(`Quem participou na 4ª sessão de Apoio?`, "Ambos|Mulher"), 1, 0),
         m4p = ifelse(str_detect(`Quem participou na 4ª sessão de Apoio?`, "Ambos|Homem"), 1, 0),
         sch5p = ifelse(!is.na(`Data marcada para 5ª sessão de Apoio?`), 1, 0),
         fem5p = ifelse(str_detect(`Quem participou na 5ª sessão de Apoio?`, "Ambos|Mulher"), 1, 0),
         m5p = ifelse(str_detect(`Quem participou na 5ª sessão de Apoio?`, "Ambos|Homem"), 1, 0),
         sch6p = ifelse(!is.na(`Data marcada para 6ª sessão de Apoio?`), 1, 0),
         fem6p = ifelse(str_detect(`Quem participou na 6ª sessão de Apoio?`, "Ambos|Mulher"), 1, 0),
         m6p = ifelse(str_detect(`Quem participou na 6ª sessão de Apoio?`, "Ambos|Homem"), 1, 0),
         sch7p = ifelse(!is.na(`Data marcada para 7ª sessão de Apoio?`), 1, 0),
         fem7p = ifelse(str_detect(`Quem participou na 7ª sessão de Apoio?`, "Ambos|Mulher"), 1, 0),
         m7p = ifelse(str_detect(`Quem participou na 7ª sessão de Apoio?`, "Ambos|Homem"), 1, 0),
         sch8p = ifelse(!is.na(`Data marcada para 8ª sessão de Apoio?`), 1, 0),
         fem8p = ifelse(str_detect(`Quem participou na 8ª sessão de Apoio?`, "Ambos|Mulher"), 1, 0),
         m8p = ifelse(str_detect(`Quem participou na 8ª sessão de Apoio?`, "Ambos|Homem"), 1, 0),
         sch9p = ifelse(!is.na(`Data marcada para 9ª sessão de Apoio?`), 1, 0),
         fem9p = ifelse(str_detect(`Quem participou na 9ª sessão de Apoio?`, "Ambos|Mulher"), 1, 0),
         m9p = ifelse(str_detect(`Quem participou na 9ª sessão de Apoio?`, "Ambos|Homem"), 1, 0),) %>%
  rowwise() %>%
  mutate(ptot_sch = sum(sch1p, sch2p, sch3p, sch4p, sch5p, sch6p, sch7p, sch8p, sch9p, na.rm = TRUE),
         ptot_fem = sum(fem1p, fem2p, fem3p, fem4p, fem5p, fem6p, fem7p, fem8p, fem9p, na.rm = TRUE),
         ptot_m = sum(m1p, m2p, m3p, m4p, m5p, m6p, m7p, m8p, m9p, na.rm = TRUE),
         pfem_prop = ptot_fem / ptot_sch,
         pm_prop = ptot_m / ptot_sch)

# subset and then split to merge
peer_merge_prep <- peer_df %>% select(`Código de estudo da participante feminina`,
                                      `Código de estudo do participante masculino`, 
                                      sch1p:pm_prop) %>%
  rename(codigo_f = `Código de estudo da participante feminina`,
         codigo_m = `Código de estudo do participante masculino`)

peer_f <- peer_merge_prep %>%
  select(codigo_f, sch1p, fem1p, sch2p, fem2p, sch3p, fem3p, sch4p, fem4p, 
         sch5p, fem5p, sch6p, fem6p, sch7p, fem7p, sch8p, fem8p, sch9p, fem9p,
         ptot_sch, ptot_fem, pfem_prop) %>%
  rename(codigo = codigo_f)
peer_m <- peer_merge_prep %>%
  select(codigo_m, sch1p, m1p, sch2p, m2p, sch3p, m3p, sch4p, m4p, 
         sch5p, m5p, sch6p, m6p, sch7p, m7p, sch8p, m8p, sch9p, m9p,
         ptot_sch, ptot_m, pm_prop) %>%
  rename(codigo = codigo_m)

# tibble to merge with int
peer_merge <- bind_rows(peer_f, peer_m)

# merge exposures by codigo
exp_merge <- full_join(skills_merge, peer_merge, by = "codigo") %>%
  filter(!is.na(codigo))

# save and export data for sankey diagram in chapter 5
sank <- exp_merge %>%
  select(codigo, 
         fem1, fem2, fem3, fem4, fem5, fem6,
         m1, m2, m3, m4, m5, m6,
         fem1p, fem2p, fem3p, fem4p, fem5p, fem6p, fem7p, fem8p, fem9p,
         m1p, m2p, m3p, m4p, m5p, m6p, m7p, m8p, m9p)
# save .rda file for merging to "Data" folder
options(LoadPath = "../Data/")
Save(sank)

# for the sake of this analysis, '
#I just need the number of completed sessions by females and males, 
#the total scheduled sessions, 
# and the proportion of sessions completed

int_cleaned <- exp_merge %>%
  rowwise() %>%
  mutate(skills_sch = sum(tot_sch, na.rm = TRUE),
         skills_comp = sum(tot_fem, tot_m, na.rm = TRUE),
         skills_prop = sum(fem_prop, m_prop, na.rm = TRUE),
         peer_sch = sum(ptot_sch, na.rm = TRUE),
         peer_comp = sum(ptot_fem, ptot_m, na.rm = TRUE),
         peer_prop = sum(pfem_prop, pm_prop, na.rm = TRUE)) %>%
  select(codigo, skills_sch:peer_prop)

# add labels
label(int_cleaned$codigo) <- "Patient ID for Drive Matching"
label(int_cleaned$skills_sch) <- "Scheduled Skills Sessions"
label(int_cleaned$skills_comp) <- "Completed Skills Sessions"
label(int_cleaned$skills_prop) <- "Proportion Completed Skills Sessions"
label(int_cleaned$peer_sch) <- "Scheduled Peer Sessions"
label(int_cleaned$peer_comp) <- "Completed Peer Sessions"
label(int_cleaned$peer_prop) <- "Proportion Completed Peer Sessions"

# save .rda file for merging to "Data" folder
options(LoadPath = "../Data/")
Save(int_cleaned)
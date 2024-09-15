setwd("Desktop/Report2/")
#library(tidyverse)
library(readxl)
library(magrittr)
library(dplyr)
library(stringr)

# list xlsx files ####
list.files(pattern = "xlsx", full.names = T) %>% 
  lapply(read_excel, skip = 0, col_types = "text") -> xlsx
  # all ds equal length 27
names(xlsx) <- list.files(pattern = "xlsx", full.names = T)
bind_rows(xlsx, .id="fileID") -> xlsx
xlsx <- xlsx[,-c(7,10)]
# columns'names
#colnames(xlsx) <- c("fileID","farmID","nationalID","daEv","age","daNa","lacN",
                    "DIM","condAD","condAS","condPD","condPS","SCC","lactose",
                    "milkTemp","milkTimeAD","milkTimeAS","milkTimePD","milkTimePS","highSCC_count",
                    "colorAD","colorPS","colorPD","colorAS","milkSpeed","milkingN")
# transform as.Date daEv and daNa with the mutate function
xlsx %>% mutate(across(.cols=c("Data","Data di nascita"),
                       .fns=~as.Date(as.numeric(.x),origin="1899-12-30")))

# list csv files ####
list.files(pattern = "csv", full.names = T) %>% 
  lapply(read.csv, skip = 0, colClasses = "character", row.names = NULL) -> csv
names(csv) <- list.files(pattern = "csv", full.names = T)
csv[sapply(csv, length) == 27] %>% bind_rows(.id="fileID") -> csv27
csv[sapply(csv, length) == 38] %>% bind_rows(.id="fileID") -> csv38

# mutate(csv27[str_detect(csv27$data, "/"), ], across( # la prima parte va migliorata
#   .cols = c(data, daNa),
#   .fns = ~ as.character(as.Date(.x, format = "%d/%m/%y"))
# )) -> csv27_1

#csv27 %>% mutate(matricola = str_trim(matricola)) -> csv27

#filter(csv27[setdiff(rownames(csv27), rownames(csv27_1)), ], matricola != "Matricola") -> csv27_2

#bind_rows(csv27_1, csv27_2) %>% 
# mutate(across(.cols = everything(), ~ ifelse(. == "", NA, .))) %>% 
# unique() -> csv27_3

# change cols'names

bind_rows(csv27, xlsx) -> report2

#csv38
# check if the elements in the rows of info are equals across columns?
sapply(csv[sapply(csv, length) == 38], function(x){x[1,]}) %>% as.data.frame() -> info
info %>%
  rowwise() %>%
  mutate(all_equal = all(c_across(1:35) == first(c_across(1:35)))) %>%
  select(1,36) %>% 
  ungroup() -> info1 #rows are not equal among columns
# bind rows per report 3
bind_rows(csv[sapply(csv, length) == 38], .id="fileID") -> report3
#create a list of those csv related with report3
csv[sapply(csv, length) == 38] -> csv38
#sapply(csv38, colnames) -> info
csv38 <- bind_rows(csv38, .id="fileID")
#assign columns'names
as.character(info) -> colNames_csv38 #manually edited
colNames_csv38 <- edit(colNames_csv38)
colnames(csv38) <- colNames_csv38

#before to bind report2 with csv38, edit tha data (repetitions)
bind_rows(report2, csv38, .id = "prov") %>%
  filter(! str_detect(numAz, pattern = "[A-Z]|[a-z]")) %>% 
  mutate(across(.cols = everything(), .fns = str_trim)) %>% 
  mutate(across(.cols = everything(), ~ ifelse(. == "", NA, .))) -> reportFinale

# PER CONTROLLI
# j <- unique(report3$matricola)
# report3 %>% distinct %>% 
#   filter(matricola == sample(j, 1)) %>%
#   arrange(numLatt,as.numeric(DIM)) %>% View()
# 
# report3 %>% 
#   select(- provenienza) %>% 
#   distinct %>% 
#   filter(matricola == "IT 015990575976") %>%
#   arrange(numLatt,as.numeric(DIM)) %>% View()

write.csv(reportFinale, "report3 gen2017-24mag2024 - lavorato 30mag2024.csv", row.names = F)

# if the group contains both the possible observations in the column, keep only the rows belonging to the second observation
reportFinale %>% 
  group_by(matricola, numLatt, DIM) %>% 
  filter(all(c(1, 2) %in% prov)) %>% 
  arrange(matricola, numLatt, DIM) %>% 
  filter(prov == 2) %>% 
  ungroup() -> r1 

setdiff(reportFinale$matricola %>% unique(), r1$matricola %>% unique()) -> matricole

reportFinale %>% 
  filter(matricola %in% matricole) -> r2

rbind(r1, r2) %>% 
  select(- c(età,gestazione,peso,coloreAD:coloreAS, destinoLatte, motivo.destLatte)) %>% 
  mutate(across(
    .cols = c(data, daNa),
    .fns = ~ ifelse(str_detect(data, "/"), as.character(as.Date(.x, format = "%d/%m/%y")), .x))) %>%
  mutate(across(
    .cols = c(tempLatte,velMungitura,mungiture,resaMungitura,assunzione),
    .fns =  as.double)) %>%
  unique() %>% 
  arrange(matricola, numLatt, DIM) -> r3

write.csv(r3, "report3 gen2017-24mag2024 - lavorato 31mag2024.csv", row.names = F)

save.image("Environment.RData")
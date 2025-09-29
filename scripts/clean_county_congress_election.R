rm(list = ls()); gc()
library(data.table)
library(dplyr)
library(fixest)
library(ggplot2)
library(readxl)
library(tools)
library(tidyverse)
library(stringr)
library(fs)

# ====================================== #
# file folder
# target files



setwd("C:\\Users\\user\\Desktop\\C2L2 Research\\選舉資料\\")


file_list_09 <- dir_ls("2009縣市議員選舉") %>% as.character()

file_list_14 <- dir_ls("2014縣市議員選舉") %>% as.character()

file_list_18 <- dir_ls("2018縣市議員選舉") %>% as.character()

file_list_22 <- dir_ls("2022縣市議員選舉") %>% as.character()

file_list_10_muni <- dir_ls("2010直轄市議員選舉") %>% as.character()
file_list_14_muni <- dir_ls("2014直轄市議員選舉") %>% as.character()
file_list_18_muni <- dir_ls("2018直轄市議員選舉") %>% as.character()
file_list_22_muni <- dir_ls("2022直轄市議員選舉") %>% as.character()


file_list <- c(file_list_09, file_list_14, file_list_18, file_list_22, 
               file_list_10_muni, file_list_14_muni, file_list_18_muni, file_list_22_muni)
complete_dt <- data.table()
# cols <- c("TOWN_NM", "VILL_NM", "DPP", "KMT", "total_vote", "eligible_vote") 

for (file_nm in file_list){
  for (k in seq_along(excel_sheets(file_nm))){
    hsn <- sub(".*\\((.*)\\).*", "\\1",file_nm) 

    tmp_dt <- read_excel(file_nm, sheet = k, skip = 1, col_names = T) %>%
      select(1:grep("有效票數", names(.)), grep("選舉人數", names(.))) %>% 
      rename_with(~ "TOWN_NM", .cols = 1) %>%
      rename_with(~ "VILL_NM", .cols = 2) %>% 
      setDT()

    tmp_dt[,':='(DPP = 0, KMT = 0)]
    
    for (j in seq_along(names(tmp_dt))) {
      col_values <- tmp_dt[1:10, ..j] %>% unlist() %>% as.character()
      
      detected_dpp <- str_detect(col_values, "民主進步黨") %>% replace_na(FALSE)
      detected_kmt <- str_detect(col_values, "中國國民黨") %>% replace_na(FALSE)
      
      if (any(detected_dpp)) {
        tmp_dt[, DPP := DPP + as.numeric(unlist(.SD)), .SDcols = j]
      }
      
      if (any(detected_kmt)) {
        tmp_dt[, KMT := KMT + as.numeric(unlist(.SD)), .SDcols = j]
      }
    }
    
    
    target_cols <- names(tmp_dt)[str_detect(names(tmp_dt), "TOWN_NM|VILL_NM|DPP|KMT|有效票數|選舉人數")]
    
    
    dt <- tmp_dt %>% select(all_of(target_cols)) %>% setDT()
    
    setnames(dt, names(dt)[str_detect(names(dt), "有效票數")], "total_vote")
    setnames(dt, names(dt)[str_detect(names(dt), "選舉人數")], "eligible_vote")
    
    dt <- dt[!is.na(total_vote)]
    
    
    dt[grepl("^\\s*$", trimws(TOWN_NM, which = "both", whitespace = "[\u3000\u0020]+")), 
       TOWN_NM := NA_character_]
    dt[, TOWN_NM := zoo::na.locf(TOWN_NM, na.rm = FALSE)]
    
    dt <- dt[!is.na(VILL_NM)]
  
    
    dt[, ':=' (HSN_NM = hsn,
               year = as.numeric(substr(file_nm,1,4)),   
               total_vote = as.numeric(gsub(",", "", total_vote)),
               eligible_vote = as.numeric(gsub(",", "", eligible_vote)))]
    
    complete_dt <- rbindlist(list(complete_dt, dt), fill = TRUE)
  }
  print(paste(hsn,"in year",as.numeric(substr(file_nm,1,4)),"is done."))
  rm(tmp_dt)
  rm(hsn)
  gc()
}


county_congress_dt <-  copy(complete_dt)

county_congress_dt <- 
  county_congress_dt[,':='(KMT = sum(KMT, na.rm = T),
                                DPP = sum(DPP, na.rm = T),
                                total_vote = sum(total_vote, na.rm = T),
                                eligible_vote = sum(eligible_vote, na.rm = T)),
                          .(HSN_NM, TOWN_NM, VILL_NM, year)] %>% unique()


county_congress_dt[,.N,.(year)]



fwrite(county_congress_dt, "C:\\Users\\user\\Desktop\\github-local-repo\\clean-election-data\\data\\tmp_congress_vote_dt.csv",
       encoding = "UTF-8",
       bom = T)

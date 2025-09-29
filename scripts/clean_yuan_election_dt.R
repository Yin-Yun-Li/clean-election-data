rm(list = ls()); gc()
library(data.table)
library(dplyr)
library(fixest)
library(ggplot2)
library(readxl)
library(tools)
library(tidyverse)
library(fs)
 
# ====================================== #
# file folder
# target files


# single-member district --------------------------------------------------



setwd("C:\\Users\\user\\Desktop\\C2L2 Research\\選舉資料\\")


file_list_08 <- dir_ls("第07屆立法委員選舉得票概況") %>% 
  grep("/p07_T1_.*", ., value = T) %>% as.character()

file_list_12 <- dir_ls("第08屆立法委員選舉得票概況") %>% 
  grep("/立委-A05-2.*", ., value = T) %>% as.character()

file_list_16 <- dir_ls("第09屆立法委員選舉得票概況") %>% 
  grep("/立委-A05-2V.*", ., value = T) %>% as.character()

file_list_20 <- dir_ls("第10屆立法委員選舉得票概況") %>% 
  grep("/區域立委-A05-2V-得票數一覽表-各村里.*", ., value = T) %>% as.character()

file_list_24 <- dir_ls("第11屆立法委員選舉得票概況/區域立委") %>% as.character()


file_list <- c(file_list_08, file_list_12, file_list_16, file_list_20, file_list_24)
complete_dt <- data.table()
# cols <- c("TOWN_NM", "VILL_NM", "DPP", "KMT", "total_vote", "eligible_vote") 

for (file_nm in file_list){
  for (k in seq_along(excel_sheets(file_nm))){
    hsn <- read_excel(file_nm, sheet = k, range="A1", col_names=F) %>% as.character() %>% 
      sub(".*([一-龥]{2,3}[縣市]).*", "\\1", .)
    
    tmp_dt <- read_excel(file_nm, sheet = k, skip = 1, col_names = T) %>%
      select(1:which(names(.) == "有效票數A\nA=1+2+...+N"), "選舉人數G\nG=E+F") %>% 
      rename_with(~ "TOWN_NM", .cols = 1) %>%
      rename_with(~ "VILL_NM", .cols = 2) %>% 
      setDT()

    
    
    for (j in seq_along(names(tmp_dt))) {
      col_values <- tmp_dt[1:10, ..j] %>% unlist() %>% as.character()
      
      # 偵測政黨名稱
      detected_dpp <- str_detect(col_values, "民主進步黨") %>% replace_na(FALSE)
      detected_kmt <- str_detect(col_values, "中國國民黨") %>% replace_na(FALSE)
      
      if (any(detected_dpp)) setnames(tmp_dt, j, "DPP")
      if (any(detected_kmt)) setnames(tmp_dt, j, "KMT")
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
    
    
    election_session <-  as.numeric(sub("第([0-9]+)屆.*", "\\1", file_nm))

    dt[, ':=' (HSN_NM = hsn,
               year = 2008 + (election_session - 7) * 4,   
               total_vote = as.numeric(gsub(",", "", total_vote)),
               eligible_vote = as.numeric(gsub(",", "", eligible_vote)))]

    complete_dt <- rbindlist(list(complete_dt, dt), fill = TRUE)
  }
 print(paste(hsn,"in session",election_session,"is done."))
 rm(tmp_dt)
 rm(hsn)
 gc()
}

complete_dt[,':='(KMT = as.numeric(gsub(",", "", KMT)),
                  DPP = as.numeric(gsub(",", "", DPP)))]

single_district_yuan_dt <-  copy(complete_dt)

single_district_yuan_dt <- 
  single_district_yuan_dt[,':='(KMT = sum(KMT, na.rm = T),
                              DPP = sum(DPP, na.rm = T),
                              total_vote = sum(total_vote, na.rm = T),
                              eligible_vote = sum(eligible_vote, na.rm = T)),
                        .(HSN_NM, TOWN_NM, VILL_NM, year)] %>% unique()


single_district_yuan_dt[,.N,.(year)]

fwrite(single_district_yuan_dt, "C:\\Users\\user\\Desktop\\github-local-repo\\clean-election-data\\data\\tmp_single_district_yuan_dt.csv",
       encoding = "UTF-8",
       bom = T)

# party list --------------------------------------------------------------


file_list_08 <- dir_ls("第07屆立法委員選舉得票概況") %>% 
  grep("/p10_T4_.*", ., value = T) %>% as.character()

file_list_12 <- dir_ls("第08屆立法委員選舉得票概況") %>% 
  grep("/立委-A05-6.*", ., value = T) %>% as.character()

file_list_16 <- dir_ls("第09屆立法委員選舉得票概況") %>% 
  grep("/立委-A05-6.*", ., value = T) %>% as.character()

file_list_20 <- dir_ls("第10屆立法委員選舉得票概況") %>% 
  grep("/不分區立委-A05-6-得票數一覽表.*", ., value = T) %>% as.character()

file_list_24 <- dir_ls("第11屆立法委員選舉得票概況/不分區立委") %>% as.character()


file_list_part2 <- c(file_list_08, file_list_12, file_list_16, file_list_20, file_list_24)
complete_dt_part2 <- data.table()
# cols <- c("TOWN_NM", "VILL_NM", "DPP", "KMT", "total_vote", "eligible_vote") 

for (file_nm in file_list_part2){
  for (k in seq_along(excel_sheets(file_nm))){
    hsn <- read_excel(file_nm, sheet = k, range="A1", col_names=F) %>% as.character() %>% 
      sub(".*([一-龥]{2,3}[縣市]).*", "\\1", .)
    
    tmp_dt <- read_excel(file_nm, sheet = k, skip = 1, col_names = T) %>%
      select(1:which(names(.) == "有效票數A\nA=1+2+...+N"), "選舉人數G\nG=E+F") %>% 
      rename_with(~ "TOWN_NM", .cols = 1) %>%
      rename_with(~ "VILL_NM", .cols = 2) %>% 
      setDT()
    
    
    
    for (j in seq_along(names(tmp_dt))) {
      col_values <- tmp_dt[1:10, ..j] %>% unlist() %>% as.character()
      
      # 偵測政黨名稱
      detected_dpp <- str_detect(col_values, "民主進步黨") %>% replace_na(FALSE)
      detected_kmt <- str_detect(col_values, "中國國民黨") %>% replace_na(FALSE)
      
      if (any(detected_dpp)) setnames(tmp_dt, j, "DPP")
      if (any(detected_kmt)) setnames(tmp_dt, j, "KMT")
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
    
    
    election_session <-  as.numeric(sub("第([0-9]+)屆.*", "\\1", file_nm))
    
    dt[, ':=' (HSN_NM = hsn,
               year = 2008 + (election_session - 7) * 4,   
               total_vote = as.numeric(gsub(",", "", total_vote)),
               eligible_vote = as.numeric(gsub(",", "", eligible_vote)))]
    
    complete_dt_part2 <- rbindlist(list(complete_dt_part2, dt), fill = TRUE)
  }
  print(paste(hsn,"in session",election_session,"is done."))
  rm(tmp_dt)
  rm(hsn)
  gc()
}

complete_dt_part2[,':='(KMT = as.numeric(gsub(",", "", KMT)),
                  DPP = as.numeric(gsub(",", "", DPP)))]

party_yuan_dt <-  copy(complete_dt_part2)

party_yuan_dt <- 
  party_yuan_dt[,':='(KMT = sum(KMT, na.rm = T),
                      DPP = sum(DPP, na.rm = T),
                      total_vote = sum(total_vote, na.rm = T),
                      eligible_vote = sum(eligible_vote, na.rm = T)),
                      .(HSN_NM, TOWN_NM, VILL_NM, year)] %>% unique()


party_yuan_dt[,.N,.(year)]

fwrite(party_yuan_dt, "C:\\Users\\user\\Desktop\\github-local-repo\\clean-election-data\\data\\tmp_party_yuan_dt.csv",
       encoding = "UTF-8",
       bom = T)

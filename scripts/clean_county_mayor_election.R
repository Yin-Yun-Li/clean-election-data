rm(list = ls()); gc()
library(data.table)
library(tidyverse)
library(dplyr)
library(tools)
library(readxl)

vote_dt_path <- "C:\\Users\\user\\Desktop\\C2L2 Research\\選舉資料\\直轄市長_"
county_vote_dt <- data.table()

for (yr in c(2010, 2014, 2018, 2022)){
  
  if (yr == 2010){
    
    for (county in c("臺北市","新北市","臺中市","臺南市","高雄市")){
      dt <- read_xls(paste0(vote_dt_path,yr,"\\",county,".xls"), skip = 1, col_names = T) 
      
      original_names <- trimws(names(dt))
      new_header_row <- as.character(dt[1, ])
      new_header_row <- str_extract(new_header_row, "[^\n]+$")
      new_names <- ifelse(is.na(new_header_row) | new_header_row == "NA", original_names, new_header_row)
      names(dt) <- new_names
      
      target_cols <- names(dt)[str_detect(names(dt), "行政區別|里別|中國國民黨|民主進步黨|有效票數|選舉人數")]
      
      dt <- dt %>% select(all_of(target_cols)) %>% setDT()
      
      setnames(dt, names(dt)[str_detect(names(dt), "行政區")], "TOWN_NM")
      setnames(dt, names(dt)[str_detect(names(dt), "里別")], "VILL_NM")
      setnames(dt, names(dt)[str_detect(names(dt), "中國國民黨")], "county_kmt_vote")
      setnames(dt, names(dt)[str_detect(names(dt), "民主進步黨")], "county_dpp_vote")
      setnames(dt, names(dt)[str_detect(names(dt), "有效票數")], "county_total_vote")
      setnames(dt, names(dt)[str_detect(names(dt), "選舉人數")], "county_eligible_vote")
      
      dt <- dt[!is.na(county_total_vote)]
      df <- copy(dt)
      
      df[, `:=`(
        county_kmt_vote = as.numeric(gsub(",", "", county_kmt_vote)),
        county_dpp_vote = as.numeric(gsub(",", "", county_dpp_vote)),
        county_total_vote = as.numeric(gsub(",", "", county_total_vote)),
        county_eligible_vote = as.numeric(gsub(",", "", county_eligible_vote))
      )]
      
      df[grepl("^\\s*$", trimws(TOWN_NM, which = "both", whitespace = "[\u3000\u0020]+")), 
         TOWN_NM := NA_character_]
      df[, TOWN_NM := zoo::na.locf(TOWN_NM, na.rm = FALSE)]
      df <- df[!is.na(VILL_NM)]
      df[, ':=' (HSN_NM = county, year = yr)]
      
      county_vote_dt <- county_vote_dt %>% rbind(df)
      
      rm(dt)
      rm(df)
      print(paste("Year = ", yr,";", county, "is done."))
    }
    
  }else{
    
    for (county in c("臺北市","新北市","臺中市","臺南市","高雄市", "桃園市")){
      dt <- read_xls(paste0(vote_dt_path,yr,"\\",county,".xls"), skip = 1, col_names = T) 
      
      original_names <- trimws(names(dt))
      new_header_row <- as.character(dt[1, ])
      new_header_row <- str_extract(new_header_row, "[^\n]+$")
      new_names <- ifelse(is.na(new_header_row) | new_header_row == "NA", original_names, new_header_row)
      names(dt) <- new_names
      
      target_cols <- names(dt)[str_detect(names(dt), "行政區別|村里別|中國國民黨|民主進步黨|有效票數|選舉人數")]
      
      dt <- dt %>% select(all_of(target_cols)) %>% setDT()
      
      setnames(dt, names(dt)[str_detect(names(dt), "行政區")], "TOWN_NM")
      setnames(dt, names(dt)[str_detect(names(dt), "村里別")], "VILL_NM")
      setnames(dt, names(dt)[str_detect(names(dt), "中國國民黨")], "county_kmt_vote")
      setnames(dt, names(dt)[str_detect(names(dt), "民主進步黨")], "county_dpp_vote")
      setnames(dt, names(dt)[str_detect(names(dt), "有效票數")], "county_total_vote")
      setnames(dt, names(dt)[str_detect(names(dt), "選舉人數")], "county_eligible_vote")
      
      dt <- dt[!is.na(county_total_vote)]
      df <- copy(dt)
      
      
      df[, `:=`(
        county_kmt_vote = as.numeric(gsub(",", "", county_kmt_vote)),
        county_dpp_vote = as.numeric(gsub(",", "", county_dpp_vote)),
        county_total_vote = as.numeric(gsub(",", "", county_total_vote)),
        county_eligible_vote = as.numeric(gsub(",", "", county_eligible_vote))
      )]
      
      
      
      df[grepl("^\\s*$", trimws(TOWN_NM, which = "both", whitespace = "[\u3000\u0020]+")), 
         TOWN_NM := NA_character_]
      df[, TOWN_NM := zoo::na.locf(TOWN_NM, na.rm = FALSE)]
      df <- df[!is.na(VILL_NM)]
      df[,county_total_vote := trimws(county_total_vote)]
      df[, ':=' (HSN_NM = county, year = yr)]
      
      county_vote_dt <- county_vote_dt %>% rbind(df)
      
      rm(dt)
      rm(df)
      print(paste("Year = ", yr,";", county, "is done."))
    }
  }
}


##################################################

county_vote_dt_part2 <- data.table()

for (yr in c(2009,2014,2018,2022)){
  
  file_in_yr <- list.files(path = paste0("C:/Users/user/Desktop/C2L2 Research/選舉資料/縣市長/",yr), full.names = T)
  
  for (i in file_in_yr){
    
    dt <- read_excel(i, skip = 1, col_names = T) 
    
    original_names <- trimws(names(dt))
    new_header_row <- as.character(dt[1, ])
    new_header_row <- str_extract(new_header_row, "[^\n]+$")
    new_names <- ifelse(is.na(new_header_row) | new_header_row == "NA", original_names, new_header_row)
    names(dt) <- new_names
    
    
    target_cols <- names(dt)[str_detect(names(dt),
                                        "^(鄉鎮市區別|行政區別)|村里別|中國國民黨|民主進步黨|有效票數|選舉人數")]
    
    dt <- dt %>% select(all_of(target_cols)) %>% setDT()
    
    setnames(dt, names(dt)[str_detect(names(dt), "^(鄉鎮市區別|行政區別)")], "TOWN_NM")
    setnames(dt, names(dt)[str_detect(names(dt), "村里別")], "VILL_NM")
    setnames(dt, names(dt)[str_detect(names(dt), "中國國民黨")], "county_kmt_vote")
    setnames(dt, names(dt)[str_detect(names(dt), "民主進步黨")], "county_dpp_vote")
    setnames(dt, names(dt)[str_detect(names(dt), "有效票數")], "county_total_vote")
    setnames(dt, names(dt)[str_detect(names(dt), "選舉人數")], "county_eligible_vote")
    
    dt <- dt[!is.na(county_total_vote)]
    df <- copy(dt)
    
    df[, `:=`(
      county_kmt_vote = as.numeric(gsub(",", "", county_kmt_vote)),
      county_dpp_vote = as.numeric(gsub(",", "", county_dpp_vote)),
      county_total_vote = as.numeric(gsub(",", "", county_total_vote)),
      county_eligible_vote = as.numeric(gsub(",", "", county_eligible_vote))
    )]
    
    df[grepl("^\\s*$", trimws(TOWN_NM, which = "both", whitespace = "[\u3000\u0020]+")), 
       TOWN_NM := NA_character_]
    df[, TOWN_NM := zoo::na.locf(TOWN_NM, na.rm = FALSE)]
    df <- df[!is.na(VILL_NM)]
    df[, ':=' (HSN_NM = file_path_sans_ext(basename(i)) , year = yr)]
    county_vote_dt_part2 <- county_vote_dt_part2 %>% rbind(df)
    
    rm(dt)
    rm(df)
    print(paste("Year = ", yr,";", file_path_sans_ext(basename(i)), "is done."))
  }
  
  
  
}




county_vote_dt[, ':='( county_total_vote = gsub(",", "", county_total_vote) )]

county_vote_dt_part2[, ':='( county_total_vote = gsub(",", "", county_total_vote) )]

county_vote_dt[, ':='(county_kmt_vote = as.numeric(county_kmt_vote),
                      county_dpp_vote = as.numeric(county_dpp_vote),
                      county_total_vote = as.numeric(county_total_vote),
                      county_eligible_vote = as.numeric(county_eligible_vote))]

county_vote_dt_part2[, ':='(county_kmt_vote = as.numeric(county_kmt_vote),
                            county_dpp_vote = as.numeric(county_dpp_vote),
                            county_total_vote = as.numeric(county_total_vote),
                            county_eligible_vote = as.numeric(county_eligible_vote))]




winning_region <- county_vote_dt[,.(kmt_tol = sum(county_kmt_vote),
                                    dpp_tol = sum(county_dpp_vote)), .(HSN_NM,year)]
winning_region[,  winning_party := fifelse(kmt_tol>dpp_tol,"kmt","dpp")]


county_vote_dt <- county_vote_dt %>% left_join(winning_region[,.(HSN_NM, year, winning_party)], by=c("HSN_NM","year"))
county_vote_dt[, county_ruling_vote := ifelse(winning_party=="kmt", county_kmt_vote, county_dpp_vote)]



winning_region_part2 <- county_vote_dt_part2[,.(kmt_tol = sum(county_kmt_vote),
                                                dpp_tol = sum(county_dpp_vote)), .(HSN_NM,year)]
winning_region_part2[,  winning_party := fifelse(kmt_tol>dpp_tol,"kmt","dpp")]


county_vote_dt_part2 <- county_vote_dt_part2 %>% left_join(winning_region_part2[,.(HSN_NM, year, winning_party)], by=c("HSN_NM","year"))
county_vote_dt_part2[, county_ruling_vote := ifelse(winning_party=="kmt", county_kmt_vote, county_dpp_vote)]

county_vote_dt_part2[HSN_NM=="連江縣" & year==2022, VILL_NM := gsub("、", "", VILL_NM)]
county_vote_dt_part2[grepl("、", VILL_NM)]



##################################################

special_case_dt_clean <- function(county, year, extract_vec){
  
  tmp <- read_excel(paste0("C:\\Users\\user\\Desktop\\C2L2 Research\\選舉資料\\縣市長\\",county,"_",year,".xls"),
                    skip = 1, col_names = T)
  
  original_names <- trimws(names(tmp))
  new_header_row <- as.character(tmp[1, ])
  new_header_row <- str_extract(new_header_row, "[^\n]+$")
  new_names <- ifelse(is.na(new_header_row) | new_header_row == "NA", original_names, new_header_row)
  names(tmp) <- new_names
  setDT(tmp)
  
  tmp <- tmp[,..extract_vec]
  setnames(tmp, c("TOWN_NM", "VILL_NM", "county_kmt_vote", "county_total_vote", "county_eligible_vote"))
  
  tmp <- tmp[!is.na(county_total_vote)]
  
  tmp[, `:=`(
    county_kmt_vote = as.numeric(gsub(",", "", county_kmt_vote)),
    county_total_vote = as.numeric(gsub(",", "", county_total_vote)),
    county_eligible_vote = as.numeric(gsub(",", "", county_eligible_vote))
  )]
  
  tmp[grepl("^\\s*$", trimws(TOWN_NM, which = "both", whitespace = "[\u3000\u0020]+")), 
      TOWN_NM := NA_character_]
  tmp[, TOWN_NM := zoo::na.locf(TOWN_NM, na.rm = FALSE)]
  tmp <- tmp[!is.na(VILL_NM)]
  tmp[, ':=' (HSN_NM = county , year = year)]
  tmp[, county_ruling_vote := ifelse(HSN_NM %in% c("金門縣", "連江縣", "花蓮縣"),county_total_vote, county_kmt_vote)]
  return(tmp)
}



special_case_dt_clean_v2 <- function(county, year, extract_vec){
  
  tmp <- read_excel(paste0("C:\\Users\\user\\Desktop\\C2L2 Research\\選舉資料\\縣市長\\",county,"_",year,".xls"),
                    skip = 1, col_names = T)
  
  original_names <- trimws(names(tmp))
  new_header_row <- as.character(tmp[1, ])
  new_header_row <- str_extract(new_header_row, "[^\n]+$")
  new_names <- ifelse(is.na(new_header_row) | new_header_row == "NA", original_names, new_header_row)
  names(tmp) <- new_names
  setDT(tmp)
  
  tmp <- tmp[,..extract_vec]
  setnames(tmp, c("TOWN_NM", "VILL_NM", "county_kmt_vote_num1",
                  "county_kmt_vote_num2", "county_total_vote", "county_eligible_vote"))
  
  tmp <- tmp[!is.na(county_total_vote)]
  
  tmp[grepl("^\\s*$", trimws(TOWN_NM, which = "both", whitespace = "[\u3000\u0020]+")), 
      TOWN_NM := NA_character_]
  tmp[, TOWN_NM := zoo::na.locf(TOWN_NM, na.rm = FALSE)]
  tmp <- tmp[!is.na(VILL_NM)]
  tmp[, `:=`(
    county_kmt_vote_num1 = as.numeric(gsub(",", "", county_kmt_vote_num1)),
    county_kmt_vote_num2 = as.numeric(gsub(",", "", county_kmt_vote_num2)),
    county_total_vote = as.numeric(gsub(",", "", county_total_vote)),
    county_eligible_vote = as.numeric(gsub(",", "", county_eligible_vote))
  )]
  tmp[, ':=' (HSN_NM = county , year = year, county_kmt_vote = county_kmt_vote_num1 + county_kmt_vote_num2)]
  
  tmp[, county_ruling_vote := ifelse(HSN_NM %in% c("金門縣", "連江縣", "花蓮縣"),county_total_vote, county_kmt_vote)]
  return(tmp)
}




花蓮縣_2009 <- special_case_dt_clean("花蓮縣",2009,c(1,2,6,7,13))
花蓮縣_2014 <- special_case_dt_clean("花蓮縣",2014,c(1,2,9,10,16))



金門縣_2009 <- special_case_dt_clean("金門縣",2009,c(1,2,8,11,17))
金門縣_2014 <- special_case_dt_clean("金門縣",2014,c(1,2,7,14,20))
金門縣_2018 <- special_case_dt_clean("金門縣",2018,c(1,2,4,10,16))
金門縣_2022 <- special_case_dt_clean("金門縣",2022,c(1,2,7,10,16))

連江縣_2018 <- special_case_dt_clean("連江縣",2018,c(1,2,7,8,14))

連江縣_2009 <- special_case_dt_clean_v2("連江縣",2009,c(1,2,4,6,7,13))
連江縣_2014 <- special_case_dt_clean_v2("連江縣",2014,c(1,2,4,5,6,12))

嘉義市_2022 <- special_case_dt_clean("嘉義市",2022,c(1,2,4,9,15))
嘉義市_2022 <- 嘉義市_2022[!1]


新竹縣_2014 <- special_case_dt_clean("新竹縣",2014,c(1,2,5,8,14))


苗栗縣_2018 <- special_case_dt_clean("苗栗縣",2018,c(1,2,7,8,14))

苗栗縣_2022 <- special_case_dt_clean_v2("苗栗縣",2022,c(1,2,4,5,9,15))

新竹市_2022 <- special_case_dt_clean_v2("新竹市",2022,c(1,2,6,9,10,16))



county_vote_dt_part3 <- rbindlist(list(花蓮縣_2009, 花蓮縣_2014,
                                       金門縣_2009, 金門縣_2014, 金門縣_2018, 金門縣_2022,
                                       連江縣_2009[,.(TOWN_NM, VILL_NM, HSN_NM, year, county_kmt_vote, county_total_vote, county_ruling_vote, county_eligible_vote )], 
                                       連江縣_2014[,.(TOWN_NM, VILL_NM, HSN_NM, year, county_kmt_vote, county_total_vote, county_ruling_vote, county_eligible_vote )], 
                                       連江縣_2018,
                                       苗栗縣_2018,
                                       苗栗縣_2022[,.(TOWN_NM, VILL_NM, HSN_NM, year, county_kmt_vote, county_total_vote, county_ruling_vote, county_eligible_vote )],
                                       新竹市_2022[,.(TOWN_NM, VILL_NM, HSN_NM, year, county_kmt_vote, county_total_vote, county_ruling_vote, county_eligible_vote )],
                                       新竹縣_2014,
                                       嘉義市_2022), use.names = TRUE)

county_vote_dt_part3[, winning_party := "kmt"]

#############################################################

county_vote_v1 <- rbindlist(list(county_vote_dt[,.(HSN_NM, TOWN_NM, VILL_NM, year, county_kmt_vote, county_dpp_vote, county_total_vote, county_eligible_vote, county_ruling_vote, winning_party)],
                                 county_vote_dt_part2[,.(HSN_NM, TOWN_NM, VILL_NM, year, county_kmt_vote, county_dpp_vote, county_total_vote, county_eligible_vote, county_ruling_vote, winning_party)],
                                 county_vote_dt_part3[,.(HSN_NM, TOWN_NM, VILL_NM, year, county_kmt_vote, county_total_vote, county_eligible_vote, county_ruling_vote, winning_party)]),
                            fill = TRUE)


fwrite(county_vote_v1, "C:\\Users\\user\\Desktop\\github-local-repo\\clean-election-data\\data\\tmp_county_mayor_dt.csv",
       encoding = "UTF-8",
       bom = T)


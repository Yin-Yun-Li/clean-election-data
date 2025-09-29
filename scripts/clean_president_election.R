rm(list=ls()); gc()
library(data.table)
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyr)
library(stringr)


top_path <- "C:\\Users\\user\\Desktop\\C2L2 Research\\選舉資料\\"


region_chr <- c("臺北市", "臺中市", "基隆市", "臺南市", "高雄市",
                "新北市", "宜蘭縣", "桃園市", "嘉義市", "新竹縣", "苗栗縣", "南投縣",
                "彰化縣", "新竹市", "雲林縣", "嘉義縣", "屏東縣", "花蓮縣", "臺東縣",
                "金門縣", "澎湖縣", "連江縣")

old_region <- c("臺北縣", "桃園縣", "臺中縣", "臺南縣", "高雄縣") ### 舊制


############
election_08 <- data.table()
hsn_id <- c(100,200,301:321,401:402)

for (id in hsn_id){
  
  setwd(paste0(top_path,"第12任總統(副總統)得票概況"))
  tmp_dt <- read_excel(paste0("rpt06-3_",id,".xls"))
  hsn_str <- str_extract(names(tmp_dt)[1],paste( c(region_chr, old_region), collapse = "|"))
  tmp_dt <- tmp_dt %>% select(c(1,2,3,4,5,11)) %>% 
      rename(TOWN_NM=1, VILL_NM=2, dpp_vote=3, kmt_vote=4, total_vote=5, eligible_vote=6) %>% setDT()
  
  tmp_region <- tmp_dt[2,1]
  tmp_dt[, HSN_NM := hsn_str]
  
  tmp_dt[, HSN_NM := str_trim(HSN_NM)]
  tmp_dt[, TOWN_NM := str_trim(TOWN_NM)]
  tmp_dt[, VILL_NM := str_trim(VILL_NM)]
  
  tmp_dt[,TOWN_NM := zoo::na.locf(TOWN_NM, na.rm = FALSE)]
  tmp_dt <- tmp_dt[-c(1:6)]
  tmp_dt[, ':='(election_yr = 2008, 
                total_vote = as.numeric(total_vote),
                dpp_vote = as.numeric(dpp_vote),
                kmt_vote = as.numeric(kmt_vote),
                eligible_vote = as.numeric(eligible_vote)) ]

  election_08 <- election_08 %>% rbind(tmp_dt)

  print(paste(tmp_region, "done"))
  rm(tmp_region)
  rm(tmp_dt)
  gc()
}



election_08[HSN_NM=="臺北縣", ':=' ( HSN_NM = "新北市", TOWN_NM = sub(".{1}$", "區", TOWN_NM))]
election_08[HSN_NM=="桃園縣", ':=' ( HSN_NM = "桃園市", TOWN_NM = sub(".{1}$", "區", TOWN_NM))]
election_08[HSN_NM=="臺中縣", ':=' ( HSN_NM = "臺中市", TOWN_NM = sub(".{1}$", "區", TOWN_NM))]
election_08[HSN_NM=="臺南縣", ':=' ( HSN_NM = "臺南市", TOWN_NM = sub(".{1}$", "區", TOWN_NM))]
election_08[HSN_NM=="高雄縣", ':=' ( HSN_NM = "高雄市", TOWN_NM = sub(".{1}$", "區", TOWN_NM))]
###################

election_combine_dt <- data.table()

for (t in c(2012,2016,2020,2024)){
  for (hsn in region_chr){
    if (t==2012){
      if (hsn=="桃園市"){ hsn <- "桃園縣"}
      setwd("C:\\Users\\user\\Desktop\\C2L2 Research\\選舉資料\\第13任總統(副總統)得票概況")
      tmp_dt <- read_excel(paste0("總統-A05-3-(",hsn,").xls"), skip = 1) %>% select(c(1,2,3,4,6,12)) %>% 
        rename(TOWN_NM=1, VILL_NM=2, dpp_vote=3, kmt_vote=4,total_vote=5, eligible_vote=6) %>% setDT()
      
      tmp_dt[,TOWN_NM := zoo::na.locf(TOWN_NM, na.rm = FALSE)]
      tmp_dt <- tmp_dt[!is.na(VILL_NM)]
      tmp_dt[, ':='(election_yr = t, dpp_vote = as.numeric(dpp_vote), kmt_vote = as.numeric(kmt_vote) , 
                    total_vote = as.numeric(total_vote), eligible_vote = as.numeric(eligible_vote), HSN_NM = hsn) ]
      
    }else if (t==2016){
      setwd("C:\\Users\\user\\Desktop\\C2L2 Research\\選舉資料\\第14任總統(副總統)得票概況")
      tmp_dt <- read_excel(paste0("總統-A05-3-(",hsn,").xls"), skip = 1) %>% select(c(1,2,3,4,6,12)) %>% 
        rename(TOWN_NM=1, VILL_NM=2, kmt_vote=3, dpp_vote=4, total_vote=5, eligible_vote=6) %>% setDT()
      
      tmp_dt[,TOWN_NM := zoo::na.locf(TOWN_NM, na.rm = FALSE)]
      tmp_dt <- tmp_dt[!is.na(VILL_NM)]
      tmp_dt[, ':='(election_yr = t, dpp_vote = as.numeric(dpp_vote), kmt_vote = as.numeric(kmt_vote) , 
                    total_vote = as.numeric(total_vote), eligible_vote = as.numeric(eligible_vote), HSN_NM = hsn) ]
    }else if (t==2020){
      setwd("C:\\Users\\user\\Desktop\\C2L2 Research\\選舉資料\\第15任總統(副總統)得票概況\\總統-各投票所得票明細及概況(Excel檔)")
      tmp_dt <- read_excel(paste0("總統-A05-3-候選人得票數一覽表-各村里(",hsn,").xls"), skip = 1) %>% select(c(1,2,4,5,6,12)) %>% 
        rename(TOWN_NM=1, VILL_NM=2, kmt_vote=3, dpp_vote=4, total_vote=5, eligible_vote=6) %>% setDT()
      
      tmp_dt[,TOWN_NM := zoo::na.locf(TOWN_NM, na.rm = FALSE)]
      tmp_dt <- tmp_dt[!is.na(VILL_NM)]
      
      tmp_dt[,  ':=' (dpp_vote = gsub("\\,","",dpp_vote), kmt_vote = gsub("\\,","",kmt_vote), total_vote = gsub("\\,","",total_vote))]
      tmp_dt[, ':='(election_yr = t, dpp_vote = as.numeric(dpp_vote), kmt_vote = as.numeric(kmt_vote) , 
                    total_vote = as.numeric(total_vote), eligible_vote = as.numeric(gsub(",", "", eligible_vote)), HSN_NM = hsn) ]
    }else{
      setwd("C:\\Users\\user\\Desktop\\C2L2 Research\\選舉資料\\第16任總統(副總統)得票概況")
      tmp_dt <- read_excel(paste0("總統-A05-3-候選人得票數一覽表-各村里(",hsn,").xlsx"), skip = 1) %>% select(c(1,2,4,5,6,12)) %>% 
        rename(TOWN_NM=1, VILL_NM=2, dpp_vote=3, kmt_vote=4 ,total_vote=5, eligible_vote=6) %>% setDT()
      
      tmp_dt[,TOWN_NM := zoo::na.locf(TOWN_NM, na.rm = FALSE)]
      tmp_dt <- tmp_dt[!is.na(VILL_NM)]
      
      tmp_dt[, ':='(election_yr = t, dpp_vote = as.numeric(dpp_vote), kmt_vote = as.numeric(kmt_vote) , 
                    total_vote = as.numeric(total_vote), eligible_vote = as.numeric(eligible_vote), HSN_NM = hsn) ]
    }
    
    
    tmp_dt[, HSN_NM := str_trim(HSN_NM)]
    tmp_dt[, TOWN_NM := str_trim(TOWN_NM)]
    tmp_dt[, VILL_NM := str_trim(VILL_NM)]
    
    
    election_combine_dt <- rbindlist(list(election_combine_dt, tmp_dt), use.names = TRUE)

    rm(tmp_dt)
    gc()
    print(paste(hsn, "in year", t, "is done"))
  }
}

election_dt_all <- rbindlist(list(election_08[,c(7,1,2,8,5,3,4,6)], election_combine_dt[,c(8,1,2,7,5,3,4,6)]), use.names = T, fill = T)
# election_dt_all[, HSN_NM := str_trim(HSN_NM)]
# election_dt_all[, TOWN_NM := str_trim(TOWN_NM)]
# election_dt_all[, VILL_NM := str_trim(VILL_NM)]

election_dt_all[, .(unique_count = uniqueN(HSN_NM)), by = election_yr]


election_dt_all[HSN_NM == "新北市" & election_yr == 2008, VILL_NM := gsub("村$", "里", VILL_NM)]
election_dt_all[HSN_NM == "新北市" & !grepl("區", TOWN_NM)]
election_dt_all[HSN_NM == "新北市" &  grepl("村", VILL_NM)]

election_dt_all[HSN_NM == "臺中市" & election_yr == 2008, VILL_NM := gsub("村$", "里", VILL_NM)]
election_dt_all[HSN_NM == "臺中市" & !grepl("區", TOWN_NM)]
election_dt_all[HSN_NM == "臺中市" &  grepl("村$", VILL_NM)]

election_dt_all[HSN_NM == "臺南市" & election_yr == 2008, VILL_NM := gsub("村$", "里", VILL_NM)]
election_dt_all[HSN_NM == "臺南市" & !grepl("區", TOWN_NM)]
election_dt_all[HSN_NM == "臺南市" &  grepl("村", VILL_NM)]
# dt[HSN_NM == "臺南市" & VILL_NM=="南海村、永樂村、新復里"]

election_dt_all[HSN_NM == "高雄市" & election_yr == 2008, VILL_NM := gsub("村$", "里", VILL_NM)]
election_dt_all[HSN_NM == "高雄市" & !grepl("區", TOWN_NM)]
election_dt_all[HSN_NM == "高雄市" &  grepl("村", VILL_NM)]


election_dt_all[HSN_NM == "桃園市" & election_yr %in% c(2008,2012), VILL_NM := gsub("村$", "里", VILL_NM)]
election_dt_all[HSN_NM == "桃園市" & !grepl("區", TOWN_NM)]
election_dt_all[HSN_NM == "桃園市" &  grepl("村", VILL_NM)]




# 不能取unique，有些是分一個里分成2個以上的投票所
# wrong_unique_election_dt <- copy(election_dt_all)
# wrong_unique_election_dt <- election_dt_all[!duplicated(election_dt_all[, .(HSN_NM, TOWN_NM, VILL_NM, election_yr)])]
# wrong_unique_election_dt <- unique(election_dt_all, by = c("HSN_NM", "TOWN_NM", "VILL_NM", "election_yr"))


##### Case1: 先處理一個村(或一個里)內切成兩個以上的投票所、一個投票所對應一個村里
election_dt_all %>% 
  dplyr::group_by(HSN_NM, TOWN_NM, VILL_NM, election_yr) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L) 
### 南投縣魚池鄉共和村屬於case2

dt <- copy(election_dt_all)

dt <- dt[, .(total_vote = sum(total_vote), dpp_vote = sum(dpp_vote), kmt_vote = sum(kmt_vote), eligible_vote = sum(eligible_vote)), 
             by = .(HSN_NM, TOWN_NM, VILL_NM, election_yr)]

dt[, ':=' (dpp_voteshare = 100*dpp_vote/total_vote, kmt_voteshare = 100*kmt_vote/total_vote,
           true_dpp_voteshare = 100*dpp_vote/eligible_vote, true_kmt_voteshare = 100*kmt_vote/eligible_vote)]



#### 土法煉鋼、最笨的方法
# dt <- election_dt_all[HSN_NM == "南投縣" & TOWN_NM == "魚池鄉" & VILL_NM=="共和村" | 
#                     !duplicated(election_dt_all, by = c("HSN_NM", "TOWN_NM", "VILL_NM", "election_yr"))]
# 
# dt[HSN_NM == "嘉義市" & TOWN_NM == "東區" & VILL_NM=="民族里" & election_yr==2012, dpp_voteshare := 48.75974]
# dt[HSN_NM == "嘉義市" & TOWN_NM == "西區" & VILL_NM=="湖邊里" & election_yr==2012, dpp_voteshare := 55.23371]
# dt[HSN_NM == "臺中市" & TOWN_NM == "西屯區" & VILL_NM=="協和里" & election_yr==2012, dpp_voteshare := 46.17004]
# dt[HSN_NM == "臺中市" & TOWN_NM == "西屯區" & VILL_NM=="福恩里" & election_yr==2012, dpp_voteshare := 36.89320]
# dt[HSN_NM == "花蓮縣" & TOWN_NM == "秀林鄉" & VILL_NM=="秀林村" & election_yr==2012, dpp_voteshare := 13.81443]
# dt[HSN_NM == "新竹縣" & TOWN_NM == "尖石鄉" & VILL_NM=="玉峰村" & election_yr==2008, dpp_voteshare := 17.33800]
# dt[HSN_NM == "臺中市" & TOWN_NM == "大肚區" & VILL_NM=="頂街里" & election_yr==2008, dpp_voteshare := 35.72823]



##### Case2: 再處理一個村(或一個里)內切成兩個以上的投票所、一個投票所對應多個村里


dt[grepl("、", VILL_NM)  & HSN_NM=="高雄市"]

dt <- dt %>%  
  filter(HSN_NM == "高雄市" & 
              TOWN_NM == "小港區" & 
              VILL_NM %like% paste(c("海澄", "海昌", "海豐", "海原", "海城","鳳森") , collapse = "|"),
              election_yr==2008) %>% 
  summarize(HSN_NM = "高雄市", 
            TOWN_NM = "小港區", 
            VILL_NM = "鳳森里",  
            election_yr = 2008,
            total_vote = sum(total_vote), 
            dpp_vote = sum(dpp_vote), 
            kmt_vote = sum(kmt_vote),
            eligible_vote = sum(eligible_vote),
            dpp_voteshare = sum(dpp_vote) / sum(total_vote) * 100,
            kmt_voteshare = sum(kmt_vote) / sum(total_vote) * 100,
            true_dpp_voteshare = sum(dpp_vote)/sum(eligible_vote) * 100,
            true_kmt_voteshare = sum(kmt_vote)/sum(eligible_vote) * 100) %>% 
  
  rbind( dt[!(HSN_NM == "高雄市" & 
             TOWN_NM == "小港區" & 
             VILL_NM %like% paste(c("海澄", "海昌", "海豐", "海原", "海城", "鳳森"), collapse = "|")&
             election_yr==2008)] ) %>% 
  setDT()



dt[is.na(HSN_NM) & VILL_NM=="南海村、永樂村、新復村"]
election_08[is.na(HSN_NM)]



#fwrite(dt,"C:\\Users\\user\\Desktop\\C2L2 Research\\Data\\全國村里民進黨得票率.csv", encoding = "UTF-8",bom = T)



dt %>%  
  filter(HSN_NM == "臺中市" & 
           TOWN_NM == "北區" & 
           VILL_NM %like% paste(c("樂英", "金華") , collapse = "|"))


dt %>%  
  filter(HSN_NM == "臺中市" & 
           TOWN_NM == "西屯區" & 
           VILL_NM %like% paste(c("福恩", "協和") , collapse = "|"))

### missing value
na_dt <- unique(election_dt_all, by = c("HSN_NM", "TOWN_NM", "VILL_NM", "election_yr")) %>% 
  pivot_wider(names_from = election_yr, values_from = dpp_vote, names_prefix = "election_") %>% 
  filter(complete.cases(.)==FALSE) %>% 
  setDT() %>% show()


dt[!complete.cases(dt),.N,.(election_yr)]

fwrite(dt,"C:\\Users\\user\\Desktop\\github-local-repo\\clean-election-data\\data\\tmp_president_election_dt.csv", encoding = "UTF-8",bom = T)



### 下一步為清理村里資料請參閱，clean_multi_region_presidential_election.R



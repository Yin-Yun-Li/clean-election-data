rm(list=ls()); gc()
library(data.table)
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyr)
library(stringr)

########### 此份文件目的在於清理總統大選資料，尤其是村里層級
### 清理縣市與鄉鎮的紀錄，請回看 clean_president_election.R


# 原始的選舉資料已大致完備，
# 包含縣市、鄉鎮、村里、近五屆選舉年、總投票人數、民進黨得票人數、民進黨得票率
# 如果直接從中選會網站抓下來rbind，會出現以下幾個問題：
# 
# Q1. 一個村里可能被分成多個投票所
# Q2. 多個村里被合併在一個投票所
# Q3. 村里改制
# 
# 這些問題都會導致不同年度的區域單位並不一致，因此清理的原則/解法如下：
# A1：直接把同年度不同投票所、卻屬於相同村里的直接合併，且確保每年都有合併
# A2：比如abc三區在2008年合併在一起，之後就分開計票，必須確保每年都是以abc的單位出現
# A3：比照解法1與2


dt <- fread("C:\\Users\\user\\Desktop\\github-local-repo\\clean-election-data\\data\\tmp_president_election_dt.csv", encoding = "UTF-8")
### 苗栗縣：done
dt[grepl("、", VILL_NM)& HSN_NM=="苗栗縣"]
dt <- dt %>% group_by(election_yr) %>% 
  filter(HSN_NM == "苗栗縣" & 
           TOWN_NM == "卓蘭鎮" & 
           VILL_NM %like% paste(c("西坪", "坪林") , collapse = "|")) %>% 
  summarize(HSN_NM = "苗栗縣", 
            TOWN_NM = "卓蘭鎮", 
            VILL_NM = "西坪里、坪林里",  
            total_vote = sum(total_vote), 
            dpp_vote = sum(dpp_vote), 
            kmt_vote = sum(kmt_vote),
            eligible_vote = sum(eligible_vote),
            dpp_voteshare = sum(dpp_vote) / sum(total_vote) * 100,
            kmt_voteshare = sum(kmt_vote) / sum(total_vote) * 100,
            true_dpp_voteshare = sum(dpp_vote)/sum(eligible_vote) * 100,
            true_kmt_voteshare = sum(kmt_vote)/sum(eligible_vote) * 100) %>% 
  ungroup() %>% 
  select(c(2:4,1,5:12)) %>% 
  setDT() %>% 
  rbind( dt[!(HSN_NM == "苗栗縣" & 
                TOWN_NM == "卓蘭鎮" & 
                VILL_NM %like% paste(c("西坪", "坪林") , collapse = "|"))])
### 嘉義市：done
dt[grepl("、", VILL_NM)& HSN_NM=="嘉義市", VILL_NM := "各里平地、山地原住民"]

### 新竹縣：done
dt[grepl("、", VILL_NM)& HSN_NM=="新竹縣"]

dt <- dt %>%  filter(HSN_NM == "新竹縣" & 
                 TOWN_NM == "尖石鄉" & 
                 VILL_NM %like% paste(c("義興", "嘉樂") , collapse = "|")) %>% 
  group_by(election_yr) %>% 
  summarize(HSN_NM = "新竹縣", 
            TOWN_NM = "尖石鄉", 
            VILL_NM = "義興村、嘉樂村",  
            total_vote = sum(total_vote), 
            dpp_vote = sum(dpp_vote), 
            kmt_vote = sum(kmt_vote),
            eligible_vote = sum(eligible_vote),
            dpp_voteshare = sum(dpp_vote) / sum(total_vote) * 100,
            kmt_voteshare = sum(kmt_vote) / sum(total_vote) * 100,
            true_dpp_voteshare = sum(dpp_vote)/sum(eligible_vote) * 100,
            true_kmt_voteshare = sum(kmt_vote)/sum(eligible_vote) * 100) %>% 
  ungroup() %>% 
  select(c(2:4,1,5:12)) %>% 
  setDT() %>% 
  rbind( dt[!(HSN_NM == "新竹縣" & 
              TOWN_NM == "尖石鄉" & 
              VILL_NM %like% paste(c("義興", "嘉樂") , collapse = "|"))])
  

### 臺中市：done
dt[grepl("、", VILL_NM) & HSN_NM=="臺中市"]

taichung_multi_region <- dt %>%
  filter(HSN_NM=="臺中市") %>% 
  mutate(VILL_GROUP = case_when(
    grepl("樂英|金華", VILL_NM) ~ "樂英里、金華里",
    grepl("福恩|協和", VILL_NM) ~ "福恩里、協和里",
    TRUE ~ VILL_NM  
  ))


taichung_multi_region <- taichung_multi_region[, .(total_vote = sum(total_vote),dpp_vote = sum(dpp_vote),kmt_vote=sum(kmt_vote),
                                                   eligible_vote = sum(eligible_vote)), 
                      by = .(HSN_NM, TOWN_NM, VILL_GROUP, election_yr)]


taichung_multi_region[, ':=' (dpp_voteshare = dpp_vote/total_vote * 100, kmt_voteshare = kmt_vote/total_vote*100,
                              true_dpp_voteshare = dpp_vote/eligible_vote * 100, true_kmt_voteshare = kmt_vote/eligible_vote * 100)]
setnames(taichung_multi_region,"VILL_GROUP","VILL_NM")

dt <- rbind(dt[HSN_NM!="臺中市"],taichung_multi_region)


### 南投縣：done
dt[grepl("、", VILL_NM) & HSN_NM=="南投縣"]

nantou_multi_region <- dt %>%
  filter(HSN_NM=="南投縣") %>% 
  mutate(VILL_GROUP = case_when(
    grepl("富山|廣明", VILL_NM) ~ "富山里、廣明里",
    grepl("秀峰|永隆", VILL_NM) ~ "秀峰村、永隆村",
    TOWN_NM == "魚池鄉"& grepl("共和|新城", VILL_NM) ~ "共和村、新城村",
    TRUE ~ VILL_NM  
  ))


nantou_multi_region <- nantou_multi_region[, .(total_vote = sum(total_vote),dpp_vote = sum(dpp_vote),kmt_vote=sum(kmt_vote),
                                               eligible_vote = sum(eligible_vote)), 
                                           by = .(HSN_NM, TOWN_NM, VILL_GROUP, election_yr)]


nantou_multi_region[, ':=' (dpp_voteshare = dpp_vote/total_vote * 100, kmt_voteshare = kmt_vote/total_vote*100,
                            true_dpp_voteshare = dpp_vote/eligible_vote * 100, true_kmt_voteshare = kmt_vote/eligible_vote * 100)]
setnames(nantou_multi_region,"VILL_GROUP","VILL_NM")

dt <- rbind(dt[HSN_NM!="南投縣"],nantou_multi_region)


### 臺南市：done
dt[grepl("、", VILL_NM) & HSN_NM=="臺南市"]

tainan_multi_region <- dt %>%
  filter(HSN_NM=="臺南市") %>% 
  mutate(VILL_GROUP = case_when(
    TOWN_NM == "西港區" & grepl("南海|永樂|新復", VILL_NM) ~ "南海里、永樂里、新復里",
    TOWN_NM == "北區" & grepl("大仁|大山", VILL_NM) ~ "大仁里、大山里",
    TOWN_NM == "東區"& grepl("復國|中興", VILL_NM) ~ "復國里、中興里",
    TRUE ~ VILL_NM  
  ))


tainan_multi_region <- tainan_multi_region[, .(total_vote = sum(total_vote),dpp_vote = sum(dpp_vote),kmt_vote=sum(kmt_vote),
                                               eligible_vote = sum(eligible_vote)), 
                                           by = .(HSN_NM, TOWN_NM, VILL_GROUP, election_yr)]


tainan_multi_region[, ':=' (dpp_voteshare = dpp_vote/total_vote * 100, kmt_voteshare = kmt_vote/total_vote*100,
                            true_dpp_voteshare = dpp_vote/eligible_vote * 100, true_kmt_voteshare = kmt_vote/eligible_vote * 100)]
setnames(tainan_multi_region,"VILL_GROUP","VILL_NM")



dt <- rbind(dt[HSN_NM!="臺南市"],tainan_multi_region)


### 臺東縣：done
dt[grepl("、", VILL_NM) & HSN_NM=="臺東縣"]

taitung_multi_region <- dt %>%
  filter(HSN_NM=="臺東縣") %>% 
  mutate(VILL_GROUP = case_when(
    TOWN_NM == "成功鎮" & grepl("忠仁|忠智", VILL_NM) ~ "忠仁里、忠智里",
    TOWN_NM == "長濱鄉" & grepl("樟原|三間", VILL_NM) ~ "樟原村、三間村",
    TRUE ~ VILL_NM  
  ))


taitung_multi_region <- taitung_multi_region[, .(total_vote = sum(total_vote),dpp_vote = sum(dpp_vote),kmt_vote=sum(kmt_vote),
                                                 eligible_vote = sum(eligible_vote)), 
                                             by = .(HSN_NM, TOWN_NM, VILL_GROUP, election_yr)]


taitung_multi_region[, ':=' (dpp_voteshare = dpp_vote/total_vote * 100, kmt_voteshare = kmt_vote/total_vote*100,
                             true_dpp_voteshare = dpp_vote/eligible_vote * 100, true_kmt_voteshare = kmt_vote/eligible_vote * 100)]
setnames(taitung_multi_region,"VILL_GROUP","VILL_NM")
taitung_multi_region[grepl("、", VILL_NM) & HSN_NM=="臺東縣"]

dt <- rbind(dt[HSN_NM!="臺東縣"],taitung_multi_region)

### 花蓮縣：done
dt[grepl("、", VILL_NM) & HSN_NM=="花蓮縣"]

hualien_multi_region <- dt %>%
  filter(HSN_NM=="花蓮縣") %>% 
  mutate(VILL_GROUP = case_when(
    TOWN_NM == "花蓮市" & grepl("民主|民治", VILL_NM) ~ "民主里、民治里",
    TOWN_NM == "花蓮市" & grepl("民族|民權", VILL_NM) ~ "民族里、民權里",
    TOWN_NM == "花蓮市" & grepl("主義|主睦", VILL_NM) ~ "主義里、主睦里",
    TOWN_NM == "壽豐鄉" & grepl("志學|池南", VILL_NM) ~ "志學村、池南村",
    TRUE ~ VILL_NM  
  ))


hualien_multi_region <- hualien_multi_region[, .(total_vote = sum(total_vote),dpp_vote = sum(dpp_vote),kmt_vote=sum(kmt_vote),
                                                 eligible_vote = sum(eligible_vote)), 
                                             by = .(HSN_NM, TOWN_NM, VILL_GROUP, election_yr)]


hualien_multi_region[, ':=' (dpp_voteshare = dpp_vote/total_vote * 100, kmt_voteshare = kmt_vote/total_vote*100,
                             true_dpp_voteshare = dpp_vote/eligible_vote * 100, true_kmt_voteshare = kmt_vote/eligible_vote * 100)]
setnames(hualien_multi_region,"VILL_GROUP","VILL_NM")
hualien_multi_region[grepl("、", VILL_NM) & HSN_NM=="花蓮縣"]

dt <- rbind(dt[HSN_NM!="花蓮縣"],hualien_multi_region)


### 新竹市：done
dt[grepl("、", VILL_NM) & HSN_NM=="新竹市"]

bamboo_multi_region <- dt %>%
  filter(HSN_NM=="新竹市") %>% 
  mutate(VILL_GROUP = case_when(
    TOWN_NM == "東區" & grepl("育賢|三民", VILL_NM) ~ "育賢里、三民里",
    TOWN_NM == "北區" & grepl("湳雅|金雅", VILL_NM) ~ "湳雅里、金雅里",
    TOWN_NM == "北區" & grepl("西門|中央", VILL_NM) ~ "西門里、中央里",
    TOWN_NM == "北區" & grepl("南勢|大鵬", VILL_NM) ~ "南勢里、大鵬里",
    TOWN_NM == "北區" & grepl("石坊|興南", VILL_NM) ~ "石坊里、興南里",
    TOWN_NM == "東區" & grepl("榮光|親仁", VILL_NM) ~ "榮光里、親仁里",
    TOWN_NM == "東區" & grepl("下竹|寺前", VILL_NM) ~ "下竹里、寺前里",
    TRUE ~ VILL_NM  
  ))


bamboo_multi_region <- bamboo_multi_region[, .(total_vote = sum(total_vote),dpp_vote = sum(dpp_vote),kmt_vote=sum(kmt_vote),
                                               eligible_vote = sum(eligible_vote)), 
                                           by = .(HSN_NM, TOWN_NM, VILL_GROUP, election_yr)]


bamboo_multi_region[, ':=' (dpp_voteshare = dpp_vote/total_vote * 100, kmt_voteshare = kmt_vote/total_vote*100,
                            true_dpp_voteshare = dpp_vote/eligible_vote * 100, true_kmt_voteshare = kmt_vote/eligible_vote * 100)]
setnames(bamboo_multi_region,"VILL_GROUP","VILL_NM")
bamboo_multi_region[grepl("、", VILL_NM) & HSN_NM=="新竹市"]

dt <- rbind(dt[HSN_NM!="新竹市"],bamboo_multi_region)


### 屏東縣屏東市：done
dt[grepl("、", VILL_NM)  & HSN_NM=="屏東縣"][TOWN_NM!="屏東市"]

# 屏東市行政區域調整修正案109年7月9日經屏東市民代表會第19屆第6次臨時會審議通過，本市79里減為59里。

# (三) 里別合併：11里。（原33里合併為11里）
# 一心里（一心、萬年，２里合併）
# 建國里（建國、擇仁，２里合併）
# 義勇里（義勇、光華，２里合併）
# 大埔里（大埔、崇智、必信，３里合併）
# 光榮里（光榮、崇禮、民權，３里合併）
# 永光里（永光、永順、永昌，３里合併）
# 長安里（長安、凌雲、鵬程，３里合併）
# 公園里（明正、興樂、太平，３里合併）
# 大同里（大同、泰安、文明、端正、武廟，５里合併）
# 楠樹里（安樂里、扶風、平和、楠樹，４里合併，納入民生路以北之慶春里）
# 金泉里（金泉、勝豐，２里合併，納入民生路以南之慶春里）
 
# 四、同里分割：４里。（原２里分割為４里）
# 潭墘里、崇陽里。（分割自原潭墘里）
# 大連里、豐年里。（分割自原大連里） 


pingtung_multi_region <- dt %>%
  filter(TOWN_NM=="屏東市") %>% 
  mutate(VILL_GROUP = case_when(
    grepl("一心|萬年|義勇|光華", VILL_NM) ~ "一心里、義勇里",
    grepl("建國|擇仁", VILL_NM) ~ "建國里",
    grepl("大埔|崇智|必信|光榮|崇禮|民權", VILL_NM) ~ "大埔里、光榮里",
    grepl("永光|永順|永昌", VILL_NM) ~ "永光里",
    grepl("明正|興樂|太平", VILL_NM) ~ "公園里",
    grepl("大同|泰安|文明|端正|武廟", VILL_NM) ~ "大同里",
    grepl("安樂|扶風|平和|楠樹|金泉|勝豐|慶春", VILL_NM) ~ "楠樹里、金泉里",
    grepl("長安|凌雲|鵬程", VILL_NM) ~ "長安里",
    grepl("潭墘|崇陽", VILL_NM) ~ "潭墘里",
    grepl("大連|豐年", VILL_NM) ~ "大連里",
    TRUE ~ VILL_NM  
  ))


pingtung_multi_region <- pingtung_multi_region[, .(total_vote = sum(total_vote),dpp_vote = sum(dpp_vote),kmt_vote=sum(kmt_vote),
                                                   eligible_vote = sum(eligible_vote)), 
                                               by = .(HSN_NM, TOWN_NM, VILL_GROUP, election_yr)]

pingtung_multi_region[, ':=' (dpp_voteshare = dpp_vote/total_vote * 100, kmt_voteshare = kmt_vote/total_vote*100,
                              true_dpp_voteshare = dpp_vote/eligible_vote * 100, true_kmt_voteshare = kmt_vote/eligible_vote * 100)]
setnames(pingtung_multi_region,"VILL_GROUP","VILL_NM")

pingtung_multi_region[grepl("、", VILL_NM)]

dt <- rbind(dt[TOWN_NM!="屏東市"],pingtung_multi_region)


### 連江縣：done

dt[grepl("、", VILL_NM)  & HSN_NM=="連江縣"]$TOWN_NM %>% unique()
dt[grepl("、", VILL_NM)  & TOWN_NM=="北竿鄉"][,c("VILL_NM","election_yr")]

lienchiang_multi_region <- dt %>%
  filter(HSN_NM=="連江縣") %>% 
  mutate(VILL_GROUP = case_when(
    grepl("清水|仁愛|津沙|馬祖|珠螺|四維", VILL_NM) ~ "清水村、仁愛村、津沙村、馬祖村、珠螺村、四維村",
    TOWN_NM=="北竿鄉" ~ "塘岐村、后沃村、橋仔村、芹壁村、坂里村、白沙村",
    TRUE ~ VILL_NM  
  ))

lienchiang_multi_region <- lienchiang_multi_region[, .(total_vote = sum(total_vote),dpp_vote = sum(dpp_vote),kmt_vote=sum(kmt_vote),
                                                       eligible_vote = sum(eligible_vote)), 
                                                   by = .(HSN_NM, TOWN_NM, VILL_GROUP, election_yr)]

lienchiang_multi_region[, ':=' (dpp_voteshare = dpp_vote/total_vote * 100, kmt_voteshare = kmt_vote/total_vote*100,
                                true_dpp_voteshare = dpp_vote/eligible_vote * 100, true_kmt_voteshare = kmt_vote/eligible_vote * 100)]
setnames(lienchiang_multi_region,"VILL_GROUP","VILL_NM")


dt <- rbind(dt[HSN_NM!="連江縣"],lienchiang_multi_region)


#### 最後一些清理
dt[HSN_NM=="桃園縣", ':='(HSN_NM = "桃園市",
                          TOWN_NM = gsub(".$", "區", TOWN_NM),
                          VILL_NM = gsub(".$", "里", VILL_NM))]
dt[HSN_NM == "桃園市" & !grepl("區", TOWN_NM)]
dt[HSN_NM == "桃園市" &  grepl("村", VILL_NM)]

dt[, HSN_CD := case_when(
  HSN_NM == "臺北市"  ~ "A",
  HSN_NM == "臺中市"  ~ "B",
  HSN_NM == "基隆市"  ~ "C",
  HSN_NM == "臺南市"  ~ "D",
  HSN_NM == "高雄市"  ~ "E",
  HSN_NM == "新北市"  ~ "F",
  HSN_NM == "宜蘭縣"  ~ "G",
  HSN_NM == "桃園市"  ~ "H",
  HSN_NM == "嘉義市"  ~ "I",
  HSN_NM == "新竹縣"  ~ "J",
  HSN_NM == "苗栗縣"  ~ "K",
  HSN_NM == "南投縣"  ~ "M",
  HSN_NM == "彰化縣"  ~ "N",
  HSN_NM == "新竹市"  ~ "O",
  HSN_NM == "雲林縣"  ~ "P",
  HSN_NM == "嘉義縣"  ~ "Q",
  HSN_NM == "屏東縣"  ~ "T",
  HSN_NM == "花蓮縣"  ~ "U",
  HSN_NM == "臺東縣"  ~ "V",
  HSN_NM == "金門縣"  ~ "W",
  HSN_NM == "澎湖縣"  ~ "X",
  HSN_NM == "連江縣"  ~ "Z")]

setorder(dt,HSN_CD)



# fwrite(dt,"nationwide_village_level_vote_cleanData.csv",encoding = "UTF-8", bom = T)
fwrite(relocate(dt, 'HSN_CD', 1),"C:\\Users\\user\\Desktop\\github-local-repo\\clean-election-data\\data\\final_president_election_dt.csv",encoding = "UTF-8", bom = T)




# multiple_region_list <- data.table()
# for (k in target_hsn){
#   tmp_vill_nm <- multiple_region_dt[HSN_NM==k]$VILL_NM %>% unique()
#   tmp_vill_nm <- unlist(strsplit(tmp_vill_nm, "、"))
#   tmp_vill_nm <- gsub("(村|里)$", "", tmp_vill_nm)
#   tmp_out <- taiwan_region[HSN_NM==k & VILL_NM %like% paste(tmp_vill_nm, collapse = "|")]
#   multiple_region_list <- multiple_region_list %>% rbind(tmp_out)
#   rm(tmp_out)
#   rm(tmp_vill_nm)
#   print(paste(k,"is done"))
# }
# 
# multiple_region_list$HSN_NM %>% unique()
# taiwan_region <- fread("C:\\Users\\user\\Desktop\\C2L2 Research\\Data\\臺灣各縣市村里代碼.csv")

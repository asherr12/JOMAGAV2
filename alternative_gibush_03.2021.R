
# Alternative to the Gibush.    $$$$$$$$$$$$$$$$$$$$$$$$$$

#Tali code

# End of Tali code.
# Beginning of civil code.****************************

library(readr)
locale("he")
mac_datets_and_scores_civil<-read_csv("C:/Users/Asher/Documents/MAMDA/JOMAGAV/alternative_gibush/mac_datets_and_scores_civil.csv",locale = locale(date_names = "he", encoding = "ISO-8859-8"))
mac_datets_and_scores_civil<-mac_datets_and_scores_civil[-1]

library(descr)
library(psych)
round(describe(as.numeric(unlist(mac_datets_and_scores_civil$final_mac_course_score))),2)
freq(ordered(round(mac_datets_and_scores_civil$final_mac_course_score,2)), plot = F,main=colnames(mac_datets_and_scores_civil$final_mac_course_score),font=2)

library(readr)
locale("he")
JOMAG_predictores_criteria_merged_civil<-read_csv("C:/Users/Asher/Documents/MAMDA/JOMAGAV/JOMAG_predictores_criteria_merged_civil_old.csv",locale = locale(date_names = "he", encoding = "ISO-8859-8"))
JOMAG_predictores_criteria_merged_civil<-JOMAG_predictores_criteria_merged_civil[-1]

JOMAG_predictores_criteria_merged_civil$VaadaGrade_completed <-NA
JOMAG_predictores_criteria_merged_civil$VaadaGrade_completed <-
  as.numeric(ifelse(is.na(JOMAG_predictores_criteria_merged_civil$VaadaGrade),JOMAG_predictores_criteria_merged_civil$FileGrade,JOMAG_predictores_criteria_merged_civil$VaadaGrade))

library(dplyr)
filtered_FileGrade3=JOMAG_predictores_criteria_merged_civil %>%
  filter(FileGrade==3 & VaadaGrade_completed==3.5)
nrow(filtered_FileGrade3)

filtered_FileGrade35=JOMAG_predictores_criteria_merged_civil %>%
  filter(FileGrade==3.5 & VaadaGrade_completed==4.5)
nrow(filtered_FileGrade35)

JOMAG_predictores_criteria_merged_civil$VaadaGrade_completed[JOMAG_predictores_criteria_merged_civil$FileGrade==3.5 & JOMAG_predictores_criteria_merged_civil$VaadaGrade_completed==4.5] <- 4

filtered_FileGrade5=JOMAG_predictores_criteria_merged_civil %>%
  filter(FileGrade==5 & VaadaGrade_completed==4.5)
nrow(filtered_FileGrade5)

JOMAG_predictores_criteria_merged_civil$VaadaGrade_completed[JOMAG_predictores_criteria_merged_civil$FileGrade==5 & JOMAG_predictores_criteria_merged_civil$VaadaGrade_completed==4.5] <- 5

filtered_FileGrade55=JOMAG_predictores_criteria_merged_civil %>%
  filter(FileGrade==5.5 & VaadaGrade_completed==4.5)
nrow(filtered_FileGrade55)

JOMAG_predictores_criteria_merged_civil$VaadaGrade_completed[JOMAG_predictores_criteria_merged_civil$FileGrade==5.5 & JOMAG_predictores_criteria_merged_civil$VaadaGrade_completed==4.5] <- 5.5

head(JOMAG_predictores_criteria_merged_civil$VaadaGrade_completed)

class(JOMAG_predictores_criteria_merged_civil$VaadaGrade_completed)

JOMAG_predictores_criteria_merged_civil = JOMAG_predictores_criteria_merged_civil %>%
  rowwise() %>%
  mutate(gender_new = ifelse(!is.na(gender),gender,
                             ifelse(!is.na(Sex),Sex,NA)))

JOMAG_predictores_criteria_merged_civil$gender<-JOMAG_predictores_criteria_merged_civil$gender_new

filtered_vars<-JOMAG_predictores_criteria_merged_civil %>%
  select(personal_number,FileGrade,VaadaGrade_completed,officer,gender_new)
mac_datets_and_scores_civil <-
  merge(mac_datets_and_scores_civil,filtered_vars,by=c("personal_number"), all.x=T, all.y=F,sort = FALSE)

round(describe(as.numeric(unlist(mac_datets_and_scores_civil$FileGrade))),2)
freq(ordered(round(mac_datets_and_scores_civil$FileGrade,2)), plot = F,main=colnames(mac_datets_and_scores_civil$FileGrade),font=2)

# filtered_mac_datets_and_scores_civil

filtered_mac_datets_and_scores_civil_2014=mac_datets_and_scores_civil%>%
  filter(end_mac_course_date<date.tkufatit_14 & date.tkufatit_14<GibDate)
filtered_mac_datets_and_scores_civil_2014$tkufatit_date<-
  filtered_mac_datets_and_scores_civil_2014$date.tkufatit_14
filtered_mac_datets_and_scores_civil_2014$tkufatit<-
  filtered_mac_datets_and_scores_civil_2014$tkufatit_14
nrow(filtered_mac_datets_and_scores_civil_2014)
colnames(filtered_mac_datets_and_scores_civil_2014)

filtered_mac_datets_and_scores_civil_2015=mac_datets_and_scores_civil%>%
  filter(end_mac_course_date<date.period.eval.2015 & date.period.eval.2015<GibDate)
filtered_mac_datets_and_scores_civil_2015$tkufatit_date<-
  filtered_mac_datets_and_scores_civil_2015$date.period.eval.2015
filtered_mac_datets_and_scores_civil_2015$tkufatit<-
  filtered_mac_datets_and_scores_civil_2015$final.score.2015
nrow(filtered_mac_datets_and_scores_civil_2015)

filtered_mac_datets_and_scores_civil_2017=mac_datets_and_scores_civil%>%
  filter(end_mac_course_date<date.period.eval.2017 & date.period.eval.2017<GibDate)
filtered_mac_datets_and_scores_civil_2017$tkufatit_date<-
  filtered_mac_datets_and_scores_civil_2017$date.period.eval.2017
filtered_mac_datets_and_scores_civil_2017$tkufatit<-
  filtered_mac_datets_and_scores_civil_2017$final.score.2017
nrow(filtered_mac_datets_and_scores_civil_2017)

filtered_mac_datets_and_scores_civil_2018=mac_datets_and_scores_civil%>%
  filter(end_mac_course_date<date.period.eval.2018 & date.period.eval.2018<GibDate)
filtered_mac_datets_and_scores_civil_2018$tkufatit_date<-
  filtered_mac_datets_and_scores_civil_2018$date.period.eval.2018
filtered_mac_datets_and_scores_civil_2018$tkufatit<-
  filtered_mac_datets_and_scores_civil_2018$final.score.2018
nrow(filtered_mac_datets_and_scores_civil_2018)

filtered_mac_datets_and_scores_civil<-rbind(filtered_mac_datets_and_scores_civil_2014,
                                            filtered_mac_datets_and_scores_civil_2015,
                                            filtered_mac_datets_and_scores_civil_2017,
                                            filtered_mac_datets_and_scores_civil_2018)

nrow(filtered_mac_datets_and_scores_civil)
n_occur<-data.frame(table(filtered_mac_datets_and_scores_civil$personal_number))
n_occur[n_occur$Freq>1,]
colnames(filtered_mac_datets_and_scores_civil)
library(data.table)
class(filtered_mac_datets_and_scores_civil$tkufatit_date)
head(filtered_mac_datets_and_scores_civil$tkufatit_date)
filtered_mac_datets_and_scores_civil<-setDT(filtered_mac_datets_and_scores_civil)[,.SD[which.max(tkufatit_date)],keyby=personal_number]
n_occur<-data.frame(table(filtered_mac_datets_and_scores_civil$personal_number))
n_occur[n_occur$Freq>1,]

JOMAG_predictores_criteria_merged_civil_qv<-JOMAG_predictores_criteria_merged_civil[which(!is.na(JOMAG_predictores_criteria_merged_civil$FileGrade)
                                                                                          & JOMAG_predictores_criteria_merged_civil$critria_count>1),]

filtered_mac_datets_and_scores_civil$matching<-match(as.numeric(filtered_mac_datets_and_scores_civil$personal_number),as.numeric(JOMAG_predictores_criteria_merged_civil$personal_number))
filtered_mac_datets_and_scores_civil$matching_qv<-match(as.numeric(filtered_mac_datets_and_scores_civil$personal_number),as.numeric(JOMAG_predictores_criteria_merged_civil_qv$personal_number))

library(descr)
library(psych)
#options(width = 71,max.print=30000)
round(freq(ordered(as.numeric(unlist(filtered_mac_datets_and_scores_civil$matching))), plot = F,main=colnames(filtered_mac_datets_and_scores_civil$matching),font=2),2)
round(freq(ordered(as.numeric(unlist(filtered_mac_datets_and_scores_civil$matching_qv))), plot = F,main=colnames(filtered_mac_datets_and_scores_civil$matching_qv),font=2),2)
freq(ordered(filtered_mac_datets_and_scores_civil$tkufatit_date), plot = F,main=colnames(filtered_mac_datets_and_scores_civil$tkufatit_date),font=2)
round(freq(ordered(as.numeric(unlist(filtered_mac_datets_and_scores_civil$tkufatit))), plot = F,main=colnames(filtered_mac_datets_and_scores_civil$tkufatit),font=2),2)

round(describe(as.numeric(unlist(JOMAG_predictores_criteria_merged_civil_qv$VaadaGrade_completed))),2)

#QA
# library(dplyr)

filtered_mac_datets_and_scores_civi=filtered_mac_datets_and_scores_civil%>%
  select(personal_number,end_mac_course_date,tkufatit_date,GibDate)

round(freq(ordered(filtered_mac_datets_and_scores_civil$end_mac_course_date), plot = F,main=colnames(filtered_mac_datets_and_scores_civil$end_mac_course_date),font=2),2)

library(dplyr)
filtered_tkufatit_date=filtered_mac_datets_and_scores_civil%>%
  select(personal_number,tkufatit_date,tkufatit)
mac_datets_and_scores_civil$personal_number<-as.numeric(mac_datets_and_scores_civil$personal_number)
filtered_tkufatit_date$personal_number<-as.numeric(filtered_tkufatit_date$personal_number)
mac_datets_and_scores_civil <- merge(mac_datets_and_scores_civil, filtered_tkufatit_date,by=c("personal_number"), all.x=T, all.y=F,sort = FALSE)
mac_datets_and_scores_civil<-as.data.frame(mac_datets_and_scores_civil)

library(descr)
library(psych)
#options(width = 71,max.print=30000)
freq(ordered(mac_datets_and_scores_civil$tkufatit_date), plot = F,main=colnames(mac_datets_and_scores_civil$tkufatit_date),font=2)
round(freq(ordered(as.numeric(unlist(mac_datets_and_scores_civil$tkufatit))), plot = F,main=colnames(mac_datets_and_scores_civil$tkufatit),font=2),2)

library(readr)
locale("he")
# ranks_new_civil<-read_csv("C:/Users/Administrator.MAMADA-777/Documents/Junior officers MAGAV validation/ranks_new_civil.csv",locale = locale(date_names = "he", encoding = "ISO-8859-8"))
ranks_new_civil<-read_csv("C:/Users/Asher/Documents/MAMDA/JOMAGAV/alternative_gibush/ranks_new_civil.csv",locale = locale(date_names = "he", encoding = "ISO-8859-8"))
ranks_new_civil<-ranks_new_civil[-1]
colnames(ranks_new_civil)[3]<-"rank_new"
colnames(ranks_new_civil)[4]<-"rank_new_date"
colnames(ranks_new_civil)[5]<-"rank_new_final_date"
class(ranks_new_civil$rank_new_date)
head(ranks_new_civil$rank_new_date)
ranks_new_civil$rank_new_date<-as.Date(as.character(ranks_new_civil$rank_new_date),format="%d/%m/%Y")
ranks_new_civil$rank_new<-gsub('["]','',ranks_new_civil$rank_new)
class(ranks_new_civil$rank_new_final_date)
head(ranks_new_civil$rank_new_final_date)
ranks_new_civil$rank_new_final_date<-as.Date(as.character(ranks_new_civil$rank_new_final_date),format="%d/%m/%Y")
ranks_new_civil$rank_new_final_date<-gsub('["]','',ranks_new_civil$rank_new_final_date)

# library(dplyr)
filtered_GibDate<-mac_datets_and_scores_civil %>%
  select(personal_number,GibDate)
ranks_new_civil <-
  merge(ranks_new_civil,filtered_GibDate,by=c("personal_number"), all.x=T, all.y=F,sort = FALSE)
ranks_new_civil=ranks_new_civil %>%
  filter(!is.na(GibDate)) %>%
  filter(rank_new_date<GibDate) %>%
  filter(rank_new=="סמר"|
           rank_new=="סמש"|
           rank_new=="רסב"|
           rank_new=="רסל"|
           rank_new=="רסמ"|
           rank_new=="רסר"|
           rank_new=="רשט"|
           rank_new=="שוטר")

n_occur<-data.frame(table(ranks_new_civil$id))
n_occur[n_occur$Freq>1,]
library (data.table)
ranks_new_civil<-setDT(ranks_new_civil)[,.SD[which.max(rank_new_date)],keyby=id]
n_occur<-data.frame(table(ranks_new_civil$id))
n_occur[n_occur$Freq>1,]
ranks_new_civil$GibDate<-NULL
ranks_new_civil$id<-NULL

mac_datets_and_scores_civil_ranks_new_civil <-
  merge(mac_datets_and_scores_civil,ranks_new_civil,by=c("personal_number"), all.x=T, all.y=F,sort = FALSE)

class(mac_datets_and_scores_civil_ranks_new_civil$GibDate)
head(mac_datets_and_scores_civil_ranks_new_civil$GibDate)
class(mac_datets_and_scores_civil_ranks_new_civil$rank_new_date)
head(mac_datets_and_scores_civil_ranks_new_civil$rank_new_date)

mac_datets_and_scores_civil_ranks_new_civil$GibDate_rank_new_date_gap<-
  as.numeric(mac_datets_and_scores_civil_ranks_new_civil$GibDate-mac_datets_and_scores_civil_ranks_new_civil$rank_new_date)
mac_datets_and_scores_civil_ranks_new_civil$keva<-ifelse((mac_datets_and_scores_civil_ranks_new_civil$rank_new=="סמר" & mac_datets_and_scores_civil_ranks_new_civil$GibDate_rank_new_date_gap>540) |
                                                           mac_datets_and_scores_civil_ranks_new_civil$rank_new=="סמש" |
                                                           mac_datets_and_scores_civil_ranks_new_civil$rank_new=="רסב" |
                                                           mac_datets_and_scores_civil_ranks_new_civil$rank_new=="רסל"|
                                                           mac_datets_and_scores_civil_ranks_new_civil$rank_new=="רסמ"|
                                                           mac_datets_and_scores_civil_ranks_new_civil$rank_new=="רסר",
                                                         2,ifelse(mac_datets_and_scores_civil_ranks_new_civil$rank_new=="סמר" & mac_datets_and_scores_civil_ranks_new_civil$GibDate_rank_new_date_gap<=540,
                                                                  1,0))

library(descr)
library(psych)
freq(ranks_new_civil$rank_new, plot = F,main=colnames(ranks_new_civil$rank_new),font=2)
round(freq(ordered(as.numeric(unlist(mac_datets_and_scores_civil_ranks_new_civil$keva))), plot = F,main=colnames(mac_datets_and_scores_civil_ranks_new_civil$keva),font=2),2)
mac_datets_and_scores_civil_ranks_new_civil$keva[mac_datets_and_scores_civil_ranks_new_civil$keva==1] <- 0
round(freq(ordered(as.numeric(unlist(mac_datets_and_scores_civil_ranks_new_civil$keva))), plot = F,main=colnames(mac_datets_and_scores_civil_ranks_new_civil$keva),font=2),2)
freq(mac_datets_and_scores_civil_ranks_new_civil$rank_new, plot = F,main=colnames(mac_datets_and_scores_civil_ranks_new_civil$rank_new),font=2)
freq(mac_datets_and_scores_civil_ranks_new_civil$tkufatit_date, plot = F,main=colnames(mac_datets_and_scores_civil_ranks_new_civil$tkufatit_date),font=2)
# write.csv(mac_datets_and_scores_civil_ranks_new_civil, file="C:/Users/Asher/Documents/MAMDA/JOMAGAV/alternative_gibush/mac_datets_and_scores_civil_ranks_new_civil_qa.csv")

#kaba
library(readr)
locale("he")
kaba_civil<-read_csv("C:/Users/Asher/Documents/MAMDA/JOMAGAV/alternative_gibush/kaba_civil.csv",locale = locale(date_names = "he", encoding = "ISO-8859-8"))
kaba_civil<-kaba_civil[-1]
n_occur<-data.frame(table(kaba_civil$personal_number))
n_occur[n_occur$Freq>1,]
kaba_civil$id<-NULL

mac_datets_and_scores_civil_ranks_new_civil_kaba_civil <-
  merge(mac_datets_and_scores_civil_ranks_new_civil,kaba_civil,by=c("personal_number"), all.x=T, all.y=F,sort = FALSE)

round(freq(ordered(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil$kaba), plot = F,main=colnames(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil$kaba),font=2),2)
round(describe(as.numeric(unlist(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil$kaba))),2)

# soc_mac
library(readr)
locale("he")
soc_mac_civil<-read_csv("C:/Users/Asher/Documents/MAMDA/JOMAGAV/alternative_gibush/courses_soc_mac_civil.csv",locale = locale(date_names = "he", encoding = "ISO-8859-8"))
soc_mac_civil<-soc_mac_civil[-1]
colnames(soc_mac_civil)

nrow(soc_mac_civil)

soc_mac_civil <- soc_mac_civil %>%
  filter(GroupId_courses_soc_mac==147 |
           GroupId_courses_soc_mac==148 |
           GroupId_courses_soc_mac==149 |
           GroupId_courses_soc_mac==150 |
           GroupId_courses_soc_mac==151 |
           GroupId_courses_soc_mac==178 |
           GroupId_courses_soc_mac==179 |
           GroupId_courses_soc_mac==182 |
           GroupId_courses_soc_mac==183 |
           GroupId_courses_soc_mac==184 |
           GroupId_courses_soc_mac==200 |
           GroupId_courses_soc_mac==201 |
           GroupId_courses_soc_mac==202 |
           GroupId_courses_soc_mac==211 |
           GroupId_courses_soc_mac==212 |
           GroupId_courses_soc_mac==213 |
           GroupId_courses_soc_mac==216 |
           GroupId_courses_soc_mac==217 |
           GroupId_courses_soc_mac==218 |
           GroupId_courses_soc_mac==222 |
           GroupId_courses_soc_mac==223 |
           GroupId_courses_soc_mac==224 |
           GroupId_courses_soc_mac==301 |
           GroupId_courses_soc_mac==302 |
           GroupId_courses_soc_mac==303 |
           GroupId_courses_soc_mac==304 |
           GroupId_courses_soc_mac==307 |
           GroupId_courses_soc_mac==308 |
           GroupId_courses_soc_mac==309 |
           GroupId_courses_soc_mac==310 |
           GroupId_courses_soc_mac==311 |
           GroupId_courses_soc_mac==312 |
           GroupId_courses_soc_mac==318 |
           GroupId_courses_soc_mac==319 |
           GroupId_courses_soc_mac==320 |
           GroupId_courses_soc_mac==325 |
           GroupId_courses_soc_mac==326 |
           GroupId_courses_soc_mac==327 |
           GroupId_courses_soc_mac==328 |
           GroupId_courses_soc_mac==329 |
           GroupId_courses_soc_mac==330 |
           GroupId_courses_soc_mac==579 |
           GroupId_courses_soc_mac==580 |
           GroupId_courses_soc_mac==581 |
           GroupId_courses_soc_mac==605 |
           GroupId_courses_soc_mac==606 |
           GroupId_courses_soc_mac==642 |
           GroupId_courses_soc_mac==643 |
           GroupId_courses_soc_mac==708 |
           GroupId_courses_soc_mac==784 |
           GroupId_courses_soc_mac==785 |
           GroupId_courses_soc_mac==786 |
           GroupId_courses_soc_mac==787 |
           GroupId_courses_soc_mac==792 |
           GroupId_courses_soc_mac==793 |
           GroupId_courses_soc_mac==708 |
           GroupId_courses_soc_mac==709 |
           GroupId_courses_soc_mac==784 |
           GroupId_courses_soc_mac==785 |
           GroupId_courses_soc_mac==786 |
           GroupId_courses_soc_mac==787 |
           GroupId_courses_soc_mac==792 |
           GroupId_courses_soc_mac==793 |
           GroupId_courses_soc_mac==814 |
           GroupId_courses_soc_mac==815 |
           GroupId_courses_soc_mac==816 |
           GroupId_courses_soc_mac==817 |
           GroupId_courses_soc_mac==825 |
           GroupId_courses_soc_mac==826 |
           GroupId_courses_soc_mac==827 |
           GroupId_courses_soc_mac==828 |
           GroupId_courses_soc_mac==832 |
           GroupId_courses_soc_mac==833 |
           GroupId_courses_soc_mac==834 |
           GroupId_courses_soc_mac==835 |
           GroupId_courses_soc_mac==836 |
           GroupId_courses_soc_mac==837 |
           GroupId_courses_soc_mac==838 |
           GroupId_courses_soc_mac==839 |
           GroupId_courses_soc_mac==840 |
           GroupId_courses_soc_mac==847 |
           GroupId_courses_soc_mac==848 |
           GroupId_courses_soc_mac==849 |
           GroupId_courses_soc_mac==850 |
           GroupId_courses_soc_mac==851 |
           GroupId_courses_soc_mac==852 |
           GroupId_courses_soc_mac==853 |
           GroupId_courses_soc_mac==854 |
           GroupId_courses_soc_mac==855 |
           GroupId_courses_soc_mac==856 |
           GroupId_courses_soc_mac==857 |
           GroupId_courses_soc_mac==858 |
           GroupId_courses_soc_mac==1118 |
           GroupId_courses_soc_mac==1119 |
           GroupId_courses_soc_mac==1120 |
           GroupId_courses_soc_mac==1121 |
           GroupId_courses_soc_mac==1128 |
           GroupId_courses_soc_mac==1129 |
           GroupId_courses_soc_mac==1130 |
           GroupId_courses_soc_mac==1180 |
           GroupId_courses_soc_mac==1181 |
           GroupId_courses_soc_mac==1182 |
           GroupId_courses_soc_mac==1184 |
           GroupId_courses_soc_mac==1245 |
           GroupId_courses_soc_mac==1246 |
           GroupId_courses_soc_mac==1247 |
           GroupId_courses_soc_mac==1248 |
           GroupId_courses_soc_mac==1297 |
           GroupId_courses_soc_mac==1298 |
           GroupId_courses_soc_mac==1299 |
           GroupId_courses_soc_mac==1302 |
           GroupId_courses_soc_mac==1343 |
           GroupId_courses_soc_mac==1344 |
           GroupId_courses_soc_mac==1345 |
           GroupId_courses_soc_mac==1346 |
           GroupId_courses_soc_mac==1443 |
           GroupId_courses_soc_mac==1445 |
           GroupId_courses_soc_mac==1446 |
           GroupId_courses_soc_mac==1496 |
           GroupId_courses_soc_mac==1497 |
           GroupId_courses_soc_mac==1525 |
           GroupId_courses_soc_mac==1526 |
           GroupId_courses_soc_mac==1528 |
           GroupId_courses_soc_mac==1562 |
           GroupId_courses_soc_mac==1605 |
           GroupId_courses_soc_mac==1675 |
           GroupId_courses_soc_mac==1681 |
           GroupId_courses_soc_mac==1682 |
           GroupId_courses_soc_mac==1776 |
           GroupId_courses_soc_mac==1782 |
           GroupId_courses_soc_mac==1783 |
           GroupId_courses_soc_mac==1784 |
           GroupId_courses_soc_mac==1867 |
           GroupId_courses_soc_mac==1868 |
           GroupId_courses_soc_mac==1869 |
           GroupId_courses_soc_mac==1948 |
           GroupId_courses_soc_mac==1949 |
           GroupId_courses_soc_mac==1959 |
           GroupId_courses_soc_mac==1960 |
           GroupId_courses_soc_mac==1966 |
           GroupId_courses_soc_mac==1995 |
           GroupId_courses_soc_mac==2011 |
           GroupId_courses_soc_mac==2012 |
           GroupId_courses_soc_mac==2013 |
           GroupId_courses_soc_mac==2014 |
           GroupId_courses_soc_mac==2015 |
           GroupId_courses_soc_mac==2016 |
           GroupId_courses_soc_mac==2017 |
           GroupId_courses_soc_mac==2029 |
           GroupId_courses_soc_mac==2087 |
           GroupId_courses_soc_mac==2088 |
           GroupId_courses_soc_mac==2090 |
           GroupId_courses_soc_mac==2171 |
           GroupId_courses_soc_mac==2172 |
           GroupId_courses_soc_mac==2173 |
           GroupId_courses_soc_mac==2174 |
           GroupId_courses_soc_mac==2232 |
           GroupId_courses_soc_mac==2233 |
           GroupId_courses_soc_mac==2236 |
           GroupId_courses_soc_mac==2237 |
           GroupId_courses_soc_mac==2240 |
           GroupId_courses_soc_mac==2346 |
           GroupId_courses_soc_mac==2347 |
           GroupId_courses_soc_mac==2348 |
           GroupId_courses_soc_mac==2349 |
           GroupId_courses_soc_mac==2421 |
           GroupId_courses_soc_mac==2422 |
           GroupId_courses_soc_mac==2423 |
           GroupId_courses_soc_mac==2425 |
           GroupId_courses_soc_mac==2491 |
           GroupId_courses_soc_mac==2492 |
           GroupId_courses_soc_mac==2548 |
           GroupId_courses_soc_mac==2551 |
           GroupId_courses_soc_mac==2565 |
           GroupId_courses_soc_mac==2615 |
           GroupId_courses_soc_mac==2637 |
           GroupId_courses_soc_mac==2638 |
           GroupId_courses_soc_mac==2639 |
           GroupId_courses_soc_mac==2640 |
           GroupId_courses_soc_mac==2703 |
           GroupId_courses_soc_mac==2704 |
           GroupId_courses_soc_mac==2705 |
           GroupId_courses_soc_mac==2707 |
           GroupId_courses_soc_mac==2773 |
           GroupId_courses_soc_mac==2778 |
           GroupId_courses_soc_mac==2791 |
           GroupId_courses_soc_mac==2796 |
           GroupId_courses_soc_mac==2867 |
           GroupId_courses_soc_mac==2869 |
           GroupId_courses_soc_mac==2870 |
           GroupId_courses_soc_mac==2871 |
           GroupId_courses_soc_mac==2927 |
           GroupId_courses_soc_mac==2930 |
           GroupId_courses_soc_mac==2932 |
           GroupId_courses_soc_mac==2934 |
           GroupId_courses_soc_mac==3009 |
           GroupId_courses_soc_mac==3011 |
           GroupId_courses_soc_mac==3013 |
           GroupId_courses_soc_mac==3014 |
           GroupId_courses_soc_mac==3078 |
           GroupId_courses_soc_mac==3079 |
           GroupId_courses_soc_mac==3080 |
           GroupId_courses_soc_mac==3081 |
           GroupId_courses_soc_mac==3164 |
           GroupId_courses_soc_mac==3165 |
           GroupId_courses_soc_mac==3166 |
           GroupId_courses_soc_mac==3167 |
           GroupId_courses_soc_mac==3245 |
           GroupId_courses_soc_mac==3246 |
           GroupId_courses_soc_mac==3247 |
           GroupId_courses_soc_mac==3252 |
           GroupId_courses_soc_mac==3334 |
           GroupId_courses_soc_mac==3335 |
           GroupId_courses_soc_mac==3336 |
           GroupId_courses_soc_mac==3337 |
           GroupId_courses_soc_mac==3415 |
           GroupId_courses_soc_mac==3417 |
           GroupId_courses_soc_mac==3419 |
           GroupId_courses_soc_mac==3420 |
           GroupId_courses_soc_mac==3504 |
           GroupId_courses_soc_mac==3507 |
           GroupId_courses_soc_mac==3508 |
           GroupId_courses_soc_mac==3509 |
           GroupId_courses_soc_mac==3615 |
           GroupId_courses_soc_mac==3616 |
           GroupId_courses_soc_mac==3617 |
           GroupId_courses_soc_mac==3618 |
           GroupId_courses_soc_mac==3675 |
           GroupId_courses_soc_mac==3676 |
           GroupId_courses_soc_mac==3677 |
           GroupId_courses_soc_mac==3678 |
           GroupId_courses_soc_mac==3756 |
           GroupId_courses_soc_mac==3757 |
           GroupId_courses_soc_mac==3758 |
           GroupId_courses_soc_mac==3759 |
           GroupId_courses_soc_mac==3760 |
           GroupId_courses_soc_mac==3829 |
           GroupId_courses_soc_mac==3830 |
           GroupId_courses_soc_mac==3831 |
           GroupId_courses_soc_mac==3832 |
           GroupId_courses_soc_mac==3914 |
           GroupId_courses_soc_mac==3915 |
           GroupId_courses_soc_mac==3916 |
           GroupId_courses_soc_mac==3917 |
           GroupId_courses_soc_mac==4019 |
           GroupId_courses_soc_mac==4020 |
           GroupId_courses_soc_mac==4021 |
           GroupId_courses_soc_mac==4022 |
           GroupId_courses_soc_mac==4134 |
           GroupId_courses_soc_mac==4135 |
           GroupId_courses_soc_mac==4136 |
           GroupId_courses_soc_mac==4137 |
           GroupId_courses_soc_mac==4253 |
           GroupId_courses_soc_mac==4254 |
           GroupId_courses_soc_mac==4255 |
           GroupId_courses_soc_mac==4256 |
           GroupId_courses_soc_mac==4274 |
           GroupId_courses_soc_mac==4277 |
           GroupId_courses_soc_mac==4387 |
           GroupId_courses_soc_mac==4388 |
           GroupId_courses_soc_mac==4389 |
           GroupId_courses_soc_mac==4390 |
           GroupId_courses_soc_mac==4498 |
           GroupId_courses_soc_mac==4499 |
           GroupId_courses_soc_mac==4500 |
           GroupId_courses_soc_mac==4501 |
           GroupId_courses_soc_mac==4600 |
           GroupId_courses_soc_mac==4601 |
           GroupId_courses_soc_mac==4602 |
           GroupId_courses_soc_mac==4603 |
           GroupId_courses_soc_mac==4665 |
           GroupId_courses_soc_mac==4666 |
           GroupId_courses_soc_mac==4667 |
           GroupId_courses_soc_mac==4668 |
           GroupId_courses_soc_mac==4781 |
           GroupId_courses_soc_mac==4782 |
           GroupId_courses_soc_mac==4783 |
           GroupId_courses_soc_mac==4784 |
           GroupId_courses_soc_mac==4787 |
           GroupId_courses_soc_mac==4889 |
           GroupId_courses_soc_mac==4890 |
           GroupId_courses_soc_mac==4891 |
           GroupId_courses_soc_mac==4892 |
           GroupId_courses_soc_mac==4973 |
           GroupId_courses_soc_mac==4974 |
           GroupId_courses_soc_mac==4975 |
           GroupId_courses_soc_mac==4976 |
           GroupId_courses_soc_mac==5031 |
           GroupId_courses_soc_mac==5071 |
           GroupId_courses_soc_mac==5072 |
           GroupId_courses_soc_mac==5073 |
           GroupId_courses_soc_mac==5074 |
           GroupId_courses_soc_mac==5118 |
           GroupId_courses_soc_mac==5117 |
           GroupId_courses_soc_mac==5118 |
           GroupId_courses_soc_mac==5120)

nrow(soc_mac_civil)

n_occur<-data.frame(table(soc_mac_civil$personal_number))
n_occur[n_occur$Freq>1,]

class(soc_mac_civil$CreateDate_courses_soc_mac)
head(soc_mac_civil$CreateDate_courses_soc_mac)
soc_mac_civil$CreateDate_courses_soc_mac<-as.Date(as.character(soc_mac_civil$CreateDate_courses_soc_mac),format="%d/%m/%Y")
class(soc_mac_civil$CreateDate_courses_soc_mac)
head(soc_mac_civil$CreateDate_courses_soc_mac)

library(data.table)
soc_mac_civil<-setDT(soc_mac_civil)[,.SD[which.max(CreateDate_courses_soc_mac)],keyby=personal_number]

n_occur<-data.frame(table(soc_mac_civil$personal_number))
n_occur[n_occur$Freq>1,]

soc_mac_civil$id<-NULL
nrow(soc_mac_civil)

colnames(soc_mac_civil)

# library(dplyr)

soc_mac_civil_am = soc_mac_civil %>%
  select(personal_number,GroupId_courses_soc_mac,GroupName_courses_soc_mac,c(15:21,23:29,31:37,39:45,47:53,55:61))
colnames(soc_mac_civil_am)[c(4:45)]<-paste(colnames(soc_mac_civil_am)[c(4:45)],"zscore",sep = "_")
colnames(soc_mac_civil_am)

class(soc_mac_civil_am)
soc_mac_civil_am<-as.data.frame(soc_mac_civil_am)
class(soc_mac_civil_am)

soc_mac_civil_am <- soc_mac_civil_am %>% 
  mutate_at(c(4:45), funs(c(scale(.))))

soc_mac_civil_am<-as.data.frame(soc_mac_civil_am)

# library(dplyr)
soc_mac_civil_am <- soc_mac_civil_am %>% 
  mutate(RAvg_courses_soc_mac = rowMeans(select(.,RAvg1_courses_soc_mac_zscore,
                                                  RAvg2_courses_soc_mac_zscore,
                                                  RAvg3_courses_soc_mac_zscore,
                                                  RAvg4_courses_soc_mac_zscore,
                                                  RAvg5_courses_soc_mac_zscore)))
soc_mac_civil_am <- soc_mac_civil_am %>% 
  mutate(RTeken_courses_soc_mac = rowMeans(select(.,RTeken1_courses_soc_mac_zscore,
                                                    RTeken2_courses_soc_mac_zscore,
                                                    RTeken3_courses_soc_mac_zscore,
                                                    RTeken4_courses_soc_mac_zscore,
                                                    RTeken5_courses_soc_mac_zscore)))
soc_mac_civil_am <- soc_mac_civil_am %>% 
  mutate(NPct_courses_soc_mac = rowMeans(select(.,NPct1_courses_soc_mac_zscore,
                                                  NPct2_courses_soc_mac_zscore)))

soc_mac_civil_am <- soc_mac_civil_am %>% 
  mutate(am_courses_soc_mac = rowMeans(select(.,RAvg_courses_soc_mac,RTeken_courses_soc_mac,NPct_courses_soc_mac)))

filtered_soc_mac_civil_am <- soc_mac_civil_am %>%
  select(personal_number,am_courses_soc_mac,GroupName_courses_soc_mac)

mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am <-
  merge(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil,filtered_soc_mac_civil_am,by=c("personal_number"), all.x=T, all.y=F,sort = FALSE)

library(descr)
library(psych)
round(describe(as.numeric(unlist(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am$am_courses_soc_mac))),2)
freq(ordered(round(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am$am_courses_soc_mac,2)), plot = F,main=colnames(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am$am_courses_soc_mac),font=2)
round(freq(ordered(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am$GroupName_courses_soc_mac), plot = F,main=colnames(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am$GroupName_courses_soc_mac),font=2),2)
round(freq(ordered(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am$tkufatit), plot = F,main=colnames(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am$tkufatit),font=2),2)

#DAPAR

DAPAR_GIBUSH_MAGAV_civil<-read.csv("C:/Users/Asher/Documents/MAMDA/JOMAGAV/DAPAR_GIBUSH_MAGAV_civil.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
DAPAR_GIBUSH_MAGAV_civil<-DAPAR_GIBUSH_MAGAV_civil[-c(1,4,5)]
colnames(DAPAR_GIBUSH_MAGAV_civil)[2]<-paste(colnames(DAPAR_GIBUSH_MAGAV_civil)[2],"DAPAR",sep="_")
n_occur<-data.frame(table(DAPAR_GIBUSH_MAGAV_civil$id))
n_occur[n_occur$Freq>1,]
DAPAR_GIBUSH_MAGAV_civil$personal_number<-NULL

mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar<- merge(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am ,DAPAR_GIBUSH_MAGAV_civil,by=c("id"), all.x=T, all.y=F,sort = FALSE)
round(freq(ordered(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar$dapar_copmlete), plot = F,main=colnames(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar$dapar_copmlete),font=2),2)

#tenure

colnames(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar)[8]<- "GiyusDate1"
colnames(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar)[9]<- "GiyusDate2"

class(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar$GibDate)
head(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar$GibDate)
soc_mac_civil$CreateDate_courses_soc_mac<-as.Date(as.character(soc_mac_civil$CreateDate_courses_soc_mac),format="%d/%m/%Y")
class(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar$GiyusDate1)
head(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar$GiyusDate1)
mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar$GiyusDate1<-
  as.Date(as.character(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar$GiyusDate1),format="%d/%m/%Y")
class(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar$GiyusDate1)
head(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar$GiyusDate1)

mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar$tenure <- 
  mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar$GibDate-
  mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar$GiyusDate1
round(freq(ordered(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar$tenure), plot = F,main=colnames(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar$tenure),font=2),2)

colnames(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar)

nrow(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar)

colnames(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar)

# sadir/keva on FileGrade

filtered_keva_FileGrade=mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar %>%
  filter(!is.na(keva) & !is.na(FileGrade))
filtered_keva_FileGrade %>% 
  group_by(keva) %>%  
  summarise_at(vars(FileGrade),funs(mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),n())) 

# sadir/keva on VaadaGrade_completed

filtered_keva_VaadaGrade_completed=mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar %>%
  filter(!is.na(keva) & !is.na(VaadaGrade_completed))
filtered_keva_VaadaGrade_completed %>% 
  group_by(keva) %>%  
  summarise_at(vars(VaadaGrade_completed),funs(mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),n())) 

# Regression analysis

library(QuantPsyc)  # lm.beta
library(car)  # vif, durbinWatsonTest
library(MASS)  # studres
library(lmSupport)  #lm.sumSquares
library(perturb)  # colldiag
library(regtools)  # pairwise

#**********prefered
reg_FileGrade1 <- lm(FileGrade ~ final_mac_course_score
                     + kaba
                     + am_courses_soc_mac,
                     data=mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar)
summary(reg_FileGrade1)

# standardised coefficients
round(lm.beta(reg_FileGrade1),2)

# R
R<-round(sqrt(0.09933),2)
R

# predicted_FileGrade
mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar$predicted_FileGrade <- 
  round(predict(reg_FileGrade1, mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar),2)

library(dplyr)

filtered_residual=mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar %>%
  filter(!is.na(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar$predicted_FileGrade)
         & !is.na(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar$FileGrade))

nrow(filtered_residual)
round(freq(ordered(filtered_residual$gender_new), plot = F,main=colnames(filtered_residual$gender_new),font=2),2)


filtered_residual$residual = round(resid(reg_FileGrade1),2)
filtered_residual$residual_abs = abs(round(resid(reg_FileGrade1),2))

plot(predict(reg_FileGrade1),filtered_residual$FileGrade,
     xlab="Predicted_FileGrade",ylab="FileGrade",col="blue")
abline(a=0,b=1)

plot(predict(reg_FileGrade1),resid(reg_FileGrade1),
     xlab="Predicted_FileGrade",ylab="Residuals (errors)",col="blue")
abline(a=0,b=0)

cor.test(as.numeric(filtered_residual$predicted_FileGrade),as.numeric(filtered_residual$FileGrade),use="pairwise.complete.obs")

cor.test(as.numeric(filtered_residual$residual_abs),as.numeric(filtered_residual$FileGrade),use="pairwise.complete.obs")

filtered_residual$residual_direction<-ifelse(filtered_residual$residual<0,1,2)

filtered_residual_negative=filtered_residual %>%
  filter(residual_direction==1)
filtered_residual_positive=filtered_residual %>%
  filter(residual_direction==2)

round(describe(as.numeric(unlist(filtered_residual_negative$residual))),2)
round(describe(as.numeric(unlist(filtered_residual_positive$residual))),2)

library(descr)
library(psych)
round(freq(ordered(filtered_residual$residual), plot = F,main=colnames(filtered_residual$residual),font=2),2)
round(freq(ordered(filtered_residual$residual_abs), plot = F,main=colnames(filtered_residual$residual_abs),font=2),2)
round(describe(as.numeric(unlist(filtered_residual$residual_abs))),2)
round(freq(ordered(filtered_residual$predicted_FileGrade), plot = F,main=colnames(filtered_residual$predicted_FileGrade),font=2),2)
round(describe(as.numeric(unlist(filtered_residual$predicted_FileGrade))),2)
round(freq(ordered(filtered_residual$FileGrade), plot = F,main=colnames(filtered_residual$FileGrade),font=2),2)
round(describe(as.numeric(unlist(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar$FileGrade))),2)
round(freq(ordered(filtered_residual$residual_direction), plot = F,main=colnames(filtered_residual$residual_direction),font=2),2)

#predict FileGrade of new candidates 03.2021 $$$$$$$$$$$$$$$$$$$$$$$$$$$$$

library(readr)
locale("he")
gibush_candidates_kakatz_03.2021_civil<-read_csv("C:/Users/Asher/Documents/MAMDA/JOMAGAV/alternative_gibush/gibush_candidates_kakatz_03.2021_civil.csv",locale = locale(date_names = "he", encoding = "ISO-8859-8"))
#gibush_candidates_kakatz_03.2021_civil<-gibush_candidates_kakatz_03.2021_civil[-1]
colnames(gibush_candidates_kakatz_03.2021_civil)[1]<-"order_num"
colnames(gibush_candidates_kakatz_03.2021_civil)[2]<-"personal_number"
colnames(gibush_candidates_kakatz_03.2021_civil)[6]<-"kaba"
colnames(gibush_candidates_kakatz_03.2021_civil)[8]<-"final_mac_course_score"
colnames(gibush_candidates_kakatz_03.2021_civil)[12]<-"apptitudes"

# soc_mac_civil
# class(gibush_candidates_kakatz_03.2021_civil)
# gibush_candidates_kakatz_03.2021_civil<-as.data.frame(gibush_candidates_kakatz_03.2021_civil)
# class(gibush_candidates_kakatz_03.2021_civil$final_mac_course_score)
# 
# class(filtered_soc_mac_civil_am)
# class(gibush_candidates_kakatz_03.2021_civil$personal_number)
# class(filtered_soc_mac_civil_am$personal_number)
# 
# gibush_candidates_kakatz_03.2021_civil <- 
# merge(gibush_candidates_kakatz_03.2021_civil,filtered_soc_mac_civil_am,by=c("personal_number"), all.x=T, all.y=F,sort = FALSE)
# 
# gibush_candidates_kakatz_03.2021_civil$am_courses_soc_mac<-as.numeric(gibush_candidates_kakatz_03.2021_civil$am_courses_soc_mac)

# courses_soc file on TALI includes candidates until 01.2020. 
# Hence I've got the data for the candidates manually from MEGAMA.

# soc_mac_civil_03.2021
library(readr)
locale("he")
soc_mac_civil_am_03.2021<-read_csv("C:/Users/Asher/Documents/MAMDA/JOMAGAV/alternative_gibush/courses_soc_mac_megama_civil_03.2021.csv",locale = locale(date_names = "he", encoding = "ISO-8859-8"))
colnames(soc_mac_civil_am_03.2021)

nrow(soc_mac_civil_am_03.2021)

class(soc_mac_civil_am_03.2021)

soc_mac_civil_am_03.2021<-as.data.frame(soc_mac_civil_am_03.2021)

soc_mac_civil_am_03.2021 <- soc_mac_civil_am_03.2021 %>% 
  mutate(RAvg_courses_soc_mac = rowMeans(select(.,RAvg1_courses_soc_mac,
                                                RAvg2_courses_soc_mac,
                                                RAvg3_courses_soc_mac,
                                                RAvg4_courses_soc_mac,
                                                RAvg5_courses_soc_mac)))
soc_mac_civil_am_03.2021 <- soc_mac_civil_am_03.2021 %>% 
  mutate(RTeken_courses_soc_mac = rowMeans(select(.,RTeken1_courses_soc_mac,
                                                  RTeken2_courses_soc_mac,
                                                  RTeken3_courses_soc_mac,
                                                  RTeken4_courses_soc_mac,
                                                  RTeken5_courses_soc_mac)))
soc_mac_civil_am_03.2021 <- soc_mac_civil_am_03.2021 %>% 
  mutate(NPct_courses_soc_mac = rowMeans(select(.,NPct1_courses_soc_mac,
                                                NPct2_courses_soc_mac)))

soc_mac_civil_am_03.2021 <- soc_mac_civil_am_03.2021 %>% 
  mutate(am_courses_soc_mac = rowMeans(select(.,RAvg_courses_soc_mac,RTeken_courses_soc_mac,NPct_courses_soc_mac)))

filtered_soc_mac_civil_am_03.2021 <- soc_mac_civil_am_03.2021 %>%
  select(order_num,am_courses_soc_mac,GroupName_courses_soc_mac)

gibush_candidates_kakatz_03.2021_civil <-
  merge(gibush_candidates_kakatz_03.2021_civil,filtered_soc_mac_civil_am_03.2021,by=c("order_num"), all.x=T, all.y=F,sort = FALSE)

library(descr)
library(psych)
describe(as.numeric(unlist(gibush_candidates_kakatz_03.2021_civil$am_courses_soc_mac)))
freq(ordered(round(gibush_candidates_kakatz_03.2021_civil$am_courses_soc_mac,2)), plot = F,main=colnames(gibush_candidates_kakatz_03.2021_civil$am_courses_soc_mac),font=2)
freq(ordered(gibush_candidates_kakatz_03.2021_civil$GroupName_courses_soc_mac), plot = F,main=colnames(gibush_candidates_kakatz_03.2021_civil$GroupName_courses_soc_mac),font=2)

gibush_candidates_kakatz_03.2021_civil[1][gibush_candidates_kakatz_03.2021_civil[2]==1] <- 1076
gibush_candidates_kakatz_03.2021_civil[1][gibush_candidates_kakatz_03.2021_civil[2]==2] <- 1034
gibush_candidates_kakatz_03.2021_civil[1][gibush_candidates_kakatz_03.2021_civil[2]==3] <- 1570
gibush_candidates_kakatz_03.2021_civil[1][gibush_candidates_kakatz_03.2021_civil[2]==4] <- 238
gibush_candidates_kakatz_03.2021_civil[1][gibush_candidates_kakatz_03.2021_civil[2]==5] <- 1604
gibush_candidates_kakatz_03.2021_civil[1][gibush_candidates_kakatz_03.2021_civil[2]==6] <- 1928
gibush_candidates_kakatz_03.2021_civil[1][gibush_candidates_kakatz_03.2021_civil[2]==7] <- 124
gibush_candidates_kakatz_03.2021_civil[1][gibush_candidates_kakatz_03.2021_civil[2]==8] <- 1156
gibush_candidates_kakatz_03.2021_civil[1][gibush_candidates_kakatz_03.2021_civil[2]==9] <- 492
gibush_candidates_kakatz_03.2021_civil[1][gibush_candidates_kakatz_03.2021_civil[2]==10] <- 1370
gibush_candidates_kakatz_03.2021_civil[1][gibush_candidates_kakatz_03.2021_civil[2]==11] <- 156
gibush_candidates_kakatz_03.2021_civil[1][gibush_candidates_kakatz_03.2021_civil[2]==12] <- 1756
gibush_candidates_kakatz_03.2021_civil[1][gibush_candidates_kakatz_03.2021_civil[2]==13] <- 358
gibush_candidates_kakatz_03.2021_civil[1][gibush_candidates_kakatz_03.2021_civil[2]==14] <- 982
gibush_candidates_kakatz_03.2021_civil[1][gibush_candidates_kakatz_03.2021_civil[2]==15] <- 66
gibush_candidates_kakatz_03.2021_civil[1][gibush_candidates_kakatz_03.2021_civil[2]==16] <- 722
gibush_candidates_kakatz_03.2021_civil[1][gibush_candidates_kakatz_03.2021_civil[2]==17] <- 10
gibush_candidates_kakatz_03.2021_civil[1][gibush_candidates_kakatz_03.2021_civil[2]==18] <- 1050
gibush_candidates_kakatz_03.2021_civil[1][gibush_candidates_kakatz_03.2021_civil[2]==19] <- 1150
gibush_candidates_kakatz_03.2021_civil[1][gibush_candidates_kakatz_03.2021_civil[2]==20] <- 916
gibush_candidates_kakatz_03.2021_civil[1][gibush_candidates_kakatz_03.2021_civil[2]==21] <- 310
gibush_candidates_kakatz_03.2021_civil[1][gibush_candidates_kakatz_03.2021_civil[2]==22] <- 1326
gibush_candidates_kakatz_03.2021_civil[1][gibush_candidates_kakatz_03.2021_civil[2]==23] <- 248
gibush_candidates_kakatz_03.2021_civil[1][gibush_candidates_kakatz_03.2021_civil[2]==24] <- 410
gibush_candidates_kakatz_03.2021_civil[1][gibush_candidates_kakatz_03.2021_civil[2]==25] <- 444
gibush_candidates_kakatz_03.2021_civil[1][gibush_candidates_kakatz_03.2021_civil[2]==26] <- 1178
gibush_candidates_kakatz_03.2021_civil[1][gibush_candidates_kakatz_03.2021_civil[2]==27] <- 692
gibush_candidates_kakatz_03.2021_civil[1][gibush_candidates_kakatz_03.2021_civil[2]==28] <- 1322
gibush_candidates_kakatz_03.2021_civil[1][gibush_candidates_kakatz_03.2021_civil[2]==29] <- 1142
gibush_candidates_kakatz_03.2021_civil[1][gibush_candidates_kakatz_03.2021_civil[2]==30] <- 1654
gibush_candidates_kakatz_03.2021_civil[1][gibush_candidates_kakatz_03.2021_civil[2]==31] <- 484
gibush_candidates_kakatz_03.2021_civil[1][gibush_candidates_kakatz_03.2021_civil[2]==32] <- 1722

class(gibush_candidates_kakatz_03.2021_civil$final_mac_course_score)
# gibush_candidates_kakatz_03.2021_civil$final_mac_course_score <-
#   as.numeric(gibush_candidates_kakatz_03.2021_civil$final_mac_course_score)

# predicted_FileGrade
gibush_candidates_kakatz_03.2021_civil$predicted_FileGrade <- format(round(predict(reg_FileGrade1, gibush_candidates_kakatz_03.2021_civil),2), nsmall = 2)
freq(ordered(gibush_candidates_kakatz_03.2021_civil$predicted_FileGrade), plot = F,main=colnames(gibush_candidates_kakatz_03.2021_civil$predicted_FileGrade),font=2)
options(digits = 2)
round(describe(as.numeric(unlist(gibush_candidates_kakatz_03.2021_civil$predicted_FileGrade))),digits=2)
class(gibush_candidates_kakatz_03.2021_civil$predicted_FileGrade)
options(digits = 3)
gibush_candidates_kakatz_03.2021_civil$predicted_FileGrade<-as.numeric(gibush_candidates_kakatz_03.2021_civil$predicted_FileGrade)
class(gibush_candidates_kakatz_03.2021_civil$predicted_FileGrade)
gibush_candidates_kakatz_03.2021_civil$personality <- NA
gibush_candidates_kakatz_03.2021_civil$command_exercise <- NA
gibush_candidates_kakatz_03.2021_civil$interview <- NA

colnames(gibush_candidates_kakatz_03.2021_civil)
class(gibush_candidates_kakatz_03.2021_civil)
gibush_candidates_kakatz_03.2021_civil<-as.data.frame(gibush_candidates_kakatz_03.2021_civil)
class(gibush_candidates_kakatz_03.2021_civil)
gibush_candidates_kakatz_03.2021_civil<-gibush_candidates_kakatz_03.2021_civil[order(gibush_candidates_kakatz_03.2021_civil$order_num,na.last=F),]
# gibush_candidates_kakatz_03.2021_civil_final_scores_gd<-gibush_candidates_kakatz_03.2021_civil
# gibush_candidates_kakatz_03.2021_civil_final_scores_gd[c(1,3:11,13:14)]<-NULL
# colnames(gibush_candidates_kakatz_03.2021_civil_final_scores_gd)[1]<-"מסד"
# colnames(gibush_candidates_kakatz_03.2021_civil_final_scores_gd)[2]<-"כשרים"
# colnames(gibush_candidates_kakatz_03.2021_civil_final_scores_gd)[3]<-"ציון מנובא"
# colnames(gibush_candidates_kakatz_03.2021_civil_final_scores_gd)[4]<-"אישיות"
# colnames(gibush_candidates_kakatz_03.2021_civil_final_scores_gd)[5]<-"תרגיל פיקודי"
# colnames(gibush_candidates_kakatz_03.2021_civil_final_scores_gd)[5]<-"ראיון"
# write.csv(gibush_candidates_kakatz_03.2021_civil_final_scores_gd, file="C:/Users/Asher/Documents/MAMDA/JOMAGAV/alternative_gibush/gibush_candidates_kakatz_03.2021_civil_final_scores_gd.csv")

#**********************get and share scores***************************


# https://www.youtube.com/watch?v=yjRrsha0TdQ
# https://googledrive.tidyverse.org/reference/drive_auth.html
# https://cran.r-project.org/web/packages/googledrive/googledrive.pdf
#  The access can be removed, through my google account. preferred by drive_deauth() bellow.
library(googledrive)
drive_auth(email = "asherr1211@gmail.com")
# drive_find(n_max = 10,type = "csv")
library(readr)
locale("he")

# use this*****************
# https://stackoverflow.com/questions/30993185/set-a-timer-in-r-to-execute-a-program


#**********************get and share scores - loop starts***************************

#run the following rows only once-when creating the Google sheet.
#save it as a google sheet.
# Set sharing settings of the file in Google drive.
# drive_download("gibush_candidates_kakatz_03.2021_civil_final_scores****",path = "C:/Users/Asher/Documents/MAMDA/JOMAGAV/alternative_gibush/gibush_candidates_kakatz_03.2021_civil_final_scores_gd",type = "csv",overwrite=T)
# gibush_candidates_kakatz_03.2021_civil_final_scores_gd<-read_csv("C:/Users/Asher/Documents/MAMDA/JOMAGAV/alternative_gibush/gibush_candidates_kakatz_03.2021_civil_final_scores_gd.csv")
# gibush_candidates_kakatz_03.2021_civil_final_scores_gd<-gibush_candidates_kakatz_03.2021_civil_final_scores_gd[-1]gibush_candidates_kakatz_03.2021_civil_final_scores_gd[2]<- gibush_candidates_kakatz_03.2021_civil$personal_number
# write.csv(gibush_candidates_kakatz_03.2021_civil_final_scores_gd, file="C:/Users/Asher/Documents/MAMDA/JOMAGAV/alternative_gibush/gibush_candidates_kakatz_03.2021_civil_final_scores_gd.csv")

# in the beginning, once, share the Google sheet with Talia and Miki, by running the next code rows.

# drive_share("https://docs.google.com/spreadsheets/d/1fSNTu2PxKGeabiAWh5GTaAqzSacHHfDWit_Zz4dsvHo/edit?usp=sharing",
#             role = "writer",
#             type = "user",
#             emailAddress = "asherr1211@gmail.com",
#             emailMessage = "Hi Asher. This file was sent to Talia and Miki"
# )


# drive_share("https://docs.google.com/spreadsheets/d/1fSNTu2PxKGeabiAWh5GTaAqzSacHHfDWit_Zz4dsvHo/edit?usp=sharing",
#             role = "writer",
#             type = "user",
#             emailAddress = "talyashmueli@gmail.com",
#             emailMessage = "Hi Talia. Please add data."
# )
 

# drive_share("https://docs.google.com/spreadsheets/d/1fSNTu2PxKGeabiAWh5GTaAqzSacHHfDWit_Zz4dsvHo/edit?usp=sharing",
#             role = "writer",
#             type = "user",
#             emailAddress = "Ramikey1@gmail.com",
#             emailMessage = "Hi Miki. This file was sent to Talia. You also can add data."
# )


# drive_share("https://docs.google.com/spreadsheets/d/1fSNTu2PxKGeabiAWh5GTaAqzSacHHfDWit_Zz4dsvHo/edit?usp=sharing",
#             role = "writer",
#             type = "user",
#             emailAddress = "Lizhazan10@gmail.com",
#             emailMessage = "Hi Liz. This file was sent to Talia. You also can add data."
# )


# drive_share("https://docs.google.com/spreadsheets/d/1fSNTu2PxKGeabiAWh5GTaAqzSacHHfDWit_Zz4dsvHo/edit?usp=sharing",
#             role = "writer",
#             type = "user",
#             emailAddress = "adi.einely@gmail.com",
#             emailMessage = "Hi Adi. This file was sent to Talia. You also can add data."
# )


# End of code for sharing the google sheet

# n=0
# repeat {
drive_auth(email = "asherr1211@gmail.com")
drive_download("gibush_candidates_kakatz_03.2021_civil_final_scores****",path = "C:/Users/Asher/Documents/MAMDA/JOMAGAV/alternative_gibush/gibush_candidates_kakatz_03.2021_civil_final_scores_gd",type = "csv",overwrite=T)
gibush_candidates_kakatz_03.2021_civil_final_scores_gd<-read_csv("C:/Users/Asher/Documents/MAMDA/JOMAGAV/alternative_gibush/gibush_candidates_kakatz_03.2021_civil_final_scores_gd.csv")
gibush_candidates_kakatz_03.2021_civil_final_scores_gd<-gibush_candidates_kakatz_03.2021_civil_final_scores_gd[-1]
gibush_candidates_kakatz_03.2021_civil_final_scores_gd[6]<-gibush_candidates_kakatz_03.2021_civil$predicted_FileGrade
gibush_candidates_kakatz_03.2021_civil$personality <-gibush_candidates_kakatz_03.2021_civil_final_scores_gd$"אישיות"
gibush_candidates_kakatz_03.2021_civil$command_exercise <-gibush_candidates_kakatz_03.2021_civil_final_scores_gd$"תרגיל פיקודי"
gibush_candidates_kakatz_03.2021_civil$interview <-gibush_candidates_kakatz_03.2021_civil_final_scores_gd$"ראיון"
gibush_candidates_kakatz_03.2021_civil <- gibush_candidates_kakatz_03.2021_civil %>% 
mutate(alternative_weighted_score = format(round(0.05*apptitudes + .10*personality + .40*command_exercise + .45*interview,2), nsmall = 2))
class(gibush_candidates_kakatz_03.2021_civil$alternative_weighted_score)
options(digits = 3)
gibush_candidates_kakatz_03.2021_civil$alternative_weighted_score<-as.numeric(gibush_candidates_kakatz_03.2021_civil$alternative_weighted_score)
class(gibush_candidates_kakatz_03.2021_civil$alternative_weighted_score)
gibush_candidates_kakatz_03.2021_civil_final_scores_gd[7]<-gibush_candidates_kakatz_03.2021_civil$alternative_weighted_score
gibush_candidates_kakatz_03.2021_civil$gap <- gibush_candidates_kakatz_03.2021_civil$alternative_weighted_score-gibush_candidates_kakatz_03.2021_civil$predicted_FileGrade
gibush_candidates_kakatz_03.2021_civil_final_scores_gd[8]<-gibush_candidates_kakatz_03.2021_civil$gap
gibush_candidates_kakatz_03.2021_civil$gap_abs <- abs(gibush_candidates_kakatz_03.2021_civil$alternative_weighted_score-
                                                      gibush_candidates_kakatz_03.2021_civil$predicted_FileGrade)
gibush_candidates_kakatz_03.2021_civil$gap_warning<-ifelse(round(gibush_candidates_kakatz_03.2021_civil$gap_abs,2)>.64,1,0)
gibush_candidates_kakatz_03.2021_civil_final_scores_gd[9]<-gibush_candidates_kakatz_03.2021_civil$gap_warning
class(gibush_candidates_kakatz_03.2021_civil_final_scores_gd)
gibush_candidates_kakatz_03.2021_civil_final_scores_gd<-as.data.frame(gibush_candidates_kakatz_03.2021_civil_final_scores_gd)
gibush_candidates_kakatz_03.2021_civil_final_scores_gd[9][gibush_candidates_kakatz_03.2021_civil_final_scores_gd[9] == 1] <- "יש"
gibush_candidates_kakatz_03.2021_civil_final_scores_gd[9][gibush_candidates_kakatz_03.2021_civil_final_scores_gd[9] == 0] <- "אין"
gibush_candidates_kakatz_03.2021_civil$altenative_weighted_score_final<-gibush_candidates_kakatz_03.2021_civil_final_scores_gd$"ציון גיבוש סופי"
gibush_candidates_kakatz_03.2021_civil$altenative_weighted_score_final_categorial<-gibush_candidates_kakatz_03.2021_civil_final_scores_gd$"ציון גיבוש סופי מעוגל"
  
write.csv(gibush_candidates_kakatz_03.2021_civil_final_scores_gd, file="C:/Users/Asher/Documents/MAMDA/JOMAGAV/alternative_gibush/gibush_candidates_kakatz_03.2021_civil_final_scores_gd.csv")
drive_update("https://docs.google.com/spreadsheets/d/1fSNTu2PxKGeabiAWh5GTaAqzSacHHfDWit_Zz4dsvHo/edit?usp=sharing","C:/Users/Asher/Documents/MAMDA/JOMAGAV/alternative_gibush/gibush_candidates_kakatz_03.2021_civil_final_scores_gd.csv")

#   timestamp()
#   
#   n <- n+1
#   if (n == 3){ # set thetimes the code should r n(n un)
#     break
#   
# Sys.sleep(600)  # set the time (in sec.)
# 
#   }
# }


#**********************stop get and share scores***************************

# stop sharing Google Drive with R
drive_deauth()

# gibush_candidates_kakatz_03.2021_civil_final_two_scores<-read_csv("C:/Users/Asher/Documents/MAMDA/JOMAGAV/alternative_gibush/gibush_candidates_kakatz_03.2021_civil_final_two_scores.csv",locale = locale(date_names = "he", encoding = "ISO-8859-8"))
# gibush_candidates_kakatz_03.2021_civil_final_two_scores<-gibush_candidates_kakatz_03.2021_civil_final_two_scores[-1]
# gibush_candidates_kakatz_03.2021_civil <-
#   merge(gibush_candidates_kakatz_03.2021_civil,gibush_candidates_kakatz_03.2021_civil_final_two_scores,by=c("order_num"), all.x=T, all.y=F,sort = FALSE)

# When I'll have the real scores update accordingly************
# gibush_candidates_kakatz_03.2021_civil$altenative_weighted_score_final<-NA
# gibush_candidates_kakatz_03.2021_civil$altenative_weighted_score_final_categorial<-NA

library(descr)
library(psych)
options(width = 71,max.print=30000)

round(freq(ordered(gibush_candidates_kakatz_03.2021_civil$predicted_FileGrade), plot = F,main=colnames(gibush_candidates_kakatz_03.2021_civil$predicted_FileGrade),font=2),2)
round(describe(as.numeric(unlist(gibush_candidates_kakatz_03.2021_civil$predicted_FileGrade))),2)
round(describe(as.numeric(unlist(gibush_candidates_kakatz_03.2021_civil$final_mac_course_score))),2)


round(describe(as.numeric(unlist(gibush_candidates_kakatz_03.2021_civil$apptitudes))),2)
round(describe(as.numeric(unlist(gibush_candidates_kakatz_03.2021_civil$personality))),2)
round(describe(as.numeric(unlist(gibush_candidates_kakatz_03.2021_civil$command_exercise))),2)
round(describe(as.numeric(unlist(gibush_candidates_kakatz_03.2021_civil$interview))),2)

round(describe(as.numeric(unlist(gibush_candidates_kakatz_03.2021_civil$predicted_FileGrade))),2)
round(freq(ordered(gibush_candidates_kakatz_03.2021_civil$alternative_weighted_score), plot = F,main=colnames(gibush_candidates_kakatz_03.2021_civil$alternative_weighted_score),font=2),2)
round(describe(as.numeric(unlist(gibush_candidates_kakatz_03.2021_civil$alternative_weighted_score))),2)
round(freq(ordered(gibush_candidates_kakatz_03.2021_civil$altenative_weighted_score_final), plot = F,main=colnames(gibush_candidates_kakatz_03.2021_civil$altenative_weighted_score_final),font=2),2)
round(describe(as.numeric(unlist(gibush_candidates_kakatz_03.2021_civil$altenative_weighted_score_final))),2)
round(freq(ordered(gibush_candidates_kakatz_03.2021_civil$altenative_weighted_score_final_categorial), plot = F,main=colnames(gibush_candidates_kakatz_03.2021_civil$altenative_weighted_score_final_categorial),font=2),2)

round(freq(ordered(gibush_candidates_kakatz_03.2021_civil$gap), plot = F,main=colnames(gibush_candidates_kakatz_03.2021_civil$gap),font=2),2)
round(freq(ordered(gibush_candidates_kakatz_03.2021_civil$gap_abs), plot = F,main=colnames(gibush_candidates_kakatz_03.2021_civil$gap_abs),font=2),2)
round(describe(as.numeric(unlist(gibush_candidates_kakatz_03.2021_civil$gap_abs))),2)

round(freq(ordered(gibush_candidates_kakatz_03.2021_civil$gap_warning), plot = F,main=colnames(gibush_candidates_kakatz_03.2021_civil$gap_warning),font=2),2)

cor.test(as.numeric(gibush_candidates_kakatz_03.2021_civil$alternative_weighted_score),as.numeric(gibush_candidates_kakatz_03.2021_civil$predicted_FileGrade),use="pairwise.complete.obs",na.action = "na.exclude")
cor.test(as.numeric(gibush_candidates_kakatz_03.2021_civil$altenative_weighted_score_final),as.numeric(gibush_candidates_kakatz_03.2021_civil$predicted_FileGrade),use="pairwise.complete.obs",na.action = "na.exclude")

gibush_candidates_kakatz_03.2021_civil$gap_direction<-ifelse(gibush_candidates_kakatz_03.2021_civil$gap<0,1,2)

gibush_candidates_kakatz_03.2021_civil_negative=gibush_candidates_kakatz_03.2021_civil %>%
  filter(gap_direction==1)
gibush_candidates_kakatz_03.2021_civil_positive=gibush_candidates_kakatz_03.2021_civil %>%
  filter(gap_direction==2)

round(describe(as.numeric(unlist(gibush_candidates_kakatz_03.2021_civil_negative$gap))),2)
round(describe(as.numeric(unlist(gibush_candidates_kakatz_03.2021_civil_positive$gap))),2)

# library(formattable)

# formattable(gibush_candidates_kakatz_03.2021_civil, list(
#   warning = formatter("span", style = x ~ ifelse(x == 1, style(color = "red", font.weight = "bold"), NA))))
# 
# col.rownames="Darkblue"
# col.data=ifelse(gibush_candidates_kakatz_03.2021_civil$warning==1,'red','black')

write.csv(gibush_candidates_kakatz_03.2021_civil, file="C:/Users/Asher/Documents/MAMDA/JOMAGAV/alternative_gibush/gibush_candidates_kakatz_03.2021_civil_final_scores_new.csv")






#presentation

presentation_filtered_vars<-JOMAG_predictores_criteria_merged_civil %>%
  select(personal_number,final_apptitudes_new,tkufatitam)
presentation <-
  merge(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar,presentation_filtered_vars,by=c("personal_number"), all.x=T, all.y=F,sort = FALSE)


cor.test(as.numeric(presentation$kaba),as.numeric(presentation$final_apptitudes_new),use="pairwise.complete.obs",na.action = "na.exclude")
cor.test(as.numeric(presentation$kaba),as.numeric(presentation$tkufatitam),use="pairwise.complete.obs",na.action = "na.exclude")
cor.test(as.numeric(presentation$am_courses_soc_mac),as.numeric(presentation$tkufatitam),use="pairwise.complete.obs",na.action = "na.exclude")
cor.test(as.numeric(presentation$final_mac_course_score),as.numeric(presentation$tkufatitam),use="pairwise.complete.obs",na.action = "na.exclude")


# analysis for ISPA conference
# filtered_residual

colnames(filtered_residual)

round(freq(ordered(filtered_residual$FileGrade), plot = F,main=colnames(filtered_residual$FileGrade),font=2),2)
round(freq(ordered(filtered_residual$predicted_FileGrade), plot = F,main=colnames(filtered_residual$predicted_FileGrade),font=2),2)

filtered_residual$FileGrade3_3.5<-ifelse(filtered_residual$FileGrade==3.5,3.5,
                                  ifelse(filtered_residual$FileGrade<3.5,3,"NA"))

filtered_residual$predicted_FileGrade3_3.5<-ifelse(round(filtered_residual$predicted_FileGrade,1)>=3.5 & round(filtered_residual$predicted_FileGrade,1)<4,3.5,
                                            ifelse(round(filtered_residual$predicted_FileGrade,1)<3.5,3,"NA"))
round(freq(ordered(filtered_residual$FileGrade3_3.5), plot = F,main=colnames(filtered_residual$FileGrade3_3.5),font=2),2)
round(freq(ordered(filtered_residual$predicted_FileGrade3_3.5), plot = F,main=colnames(filtered_residual$predicted_FileGrade3_3.5),font=2),2)
CrossTable(filtered_residual$FileGrade3_3.5,filtered_residual$predicted_FileGrade3_3.5,row.labels = T,format = c("SPSS"),cell.layout=T)

filtered_residual$FileGrade4_4.5<-ifelse(filtered_residual$FileGrade>=4.5,4.5,
                                  ifelse(filtered_residual$FileGrade==4,4,"NA"))
filtered_residual$predicted_FileGrade4_4.5<-ifelse(round(filtered_residual$predicted_FileGrade,1)>=4.5,4.5,
                                            ifelse(round(filtered_residual$predicted_FileGrade,1)<4.5 & round(filtered_residual$predicted_FileGrade,1)>=4,4,"NA"))
round(freq(ordered(filtered_residual$FileGrade4_4.5), plot = F,main=colnames(filtered_residual$FileGrade4_4.5),font=2),2)
round(freq(ordered(filtered_residual$predicted_FileGrade4_4.5), plot = F,main=colnames(filtered_residual$predicted_FileGrade4_4.5),font=2),2)
CrossTable(filtered_residual$FileGrade4_4.5,filtered_residual$predicted_FileGrade4_4.5,filtered_residual$FileGrade4_4.5,row.labels = T,format = c("SPSS"),cell.layout=T)

write.csv(filtered_residual, file="C:/Users/Asher/Documents/MAMDA/JOMAGAV/alternative_gibush/filtered_residual.csv")

# analysis for ISPA conference
# gibush_candidates_kakatz_03.2021_civil

round(freq(ordered(gibush_candidates_kakatz_03.2021_civil$alternative_weighted_score), plot = F,main=colnames(gibush_candidates_kakatz_03.2021_civil$alternative_weighted_score),font=2),2)
round(freq(ordered(gibush_candidates_kakatz_03.2021_civil$predicted_FileGrade), plot = F,main=colnames(gibush_candidates_kakatz_03.2021_civil$predicted_FileGrade),font=2),2)

gibush_candidates_kakatz_03.2021_civil$alternative_weighted_score3_3.5<-ifelse(round(gibush_candidates_kakatz_03.2021_civil$alternative_weighted_score,1)>=3.5 & round(gibush_candidates_kakatz_03.2021_civil$alternative_weighted_score,1)<4,3.5,
                                                                        ifelse(round(gibush_candidates_kakatz_03.2021_civil$predicted_FileGrade,1)<3.5,3,"NA"))
gibush_candidates_kakatz_03.2021_civil$predicted_FileGrade3_3.5<-ifelse(round(gibush_candidates_kakatz_03.2021_civil$predicted_FileGrade,1)>=3.5 & round(gibush_candidates_kakatz_03.2021_civil$predicted_FileGrade,1)<4,3.5,
                                                   ifelse(round(gibush_candidates_kakatz_03.2021_civil$predicted_FileGrade,1)<3.5,3,"NA"))
round(freq(ordered(gibush_candidates_kakatz_03.2021_civil$alternative_weighted_score3_3.5), plot = F,main=colnames(gibush_candidates_kakatz_03.2021_civil$alternative_weighted_score3_3.5),font=2),2)
round(freq(ordered(gibush_candidates_kakatz_03.2021_civil$predicted_FileGrade3_3.5), plot = F,main=colnames(gibush_candidates_kakatz_03.2021_civil$predicted_FileGrade3_3.5),font=2),2)
CrossTable(gibush_candidates_kakatz_03.2021_civil$alternative_weighted_score3_3.5,gibush_candidates_kakatz_03.2021_civil$predicted_FileGrade3_3.5,row.labels = T,format = c("SPSS"),cell.layout=T)

gibush_candidates_kakatz_03.2021_civil$alternative_weighted_score4_4.5<-ifelse(round(gibush_candidates_kakatz_03.2021_civil$alternative_weighted_score,1)>=4.5,4.5,
                                                                        ifelse(round(gibush_candidates_kakatz_03.2021_civil$alternative_weighted_score,1)<4.5 & round(gibush_candidates_kakatz_03.2021_civil$alternative_weighted_score,1)>=4,4,"NA"))
gibush_candidates_kakatz_03.2021_civil$predicted_FileGrade4_4.5<-ifelse(round(gibush_candidates_kakatz_03.2021_civil$predicted_FileGrade,1)>=4.5,4.5,
                                                   ifelse(round(gibush_candidates_kakatz_03.2021_civil$predicted_FileGrade,1)<4.5 & round(gibush_candidates_kakatz_03.2021_civil$predicted_FileGrade,1)>=4,4,"NA"))
round(freq(ordered(gibush_candidates_kakatz_03.2021_civil$alternative_weighted_score4_4.5), plot = F,main=colnames(gibush_candidates_kakatz_03.2021_civil$alternative_weighted_score4_4.5),font=2),2)
round(freq(ordered(gibush_candidates_kakatz_03.2021_civil$predicted_FileGrade4_4.5), plot = F,main=colnames(gibush_candidates_kakatz_03.2021_civil$predicted_FileGrade4_4.5),font=2),2)
CrossTable(gibush_candidates_kakatz_03.2021_civil$alternative_weighted_score4_4.5,gibush_candidates_kakatz_03.2021_civil$predicted_FileGrade4_4.5,row.labels = T,format = c("SPSS"),cell.layout=T)

round(describe(as.numeric(gibush_candidates_kakatz_03.2021_civil$alternative_weighted_score)),2)
round(describe(as.numeric(gibush_candidates_kakatz_03.2021_civil$predicted_FileGrade)),2)
round(describe(as.numeric(JOMAG_predictores_criteria_merged_civil$FileGrade)),2)
               

#shiny app

install.packages("shiny")



#***********************assisstence commands******************************

#install.packages("installr")

# It is prefered to run the next 2 commands from R gui.
# library(installr)
# updateR()

library(descr)
library(psych)
options(width = 71,max.print=30000)
round(freq(ordered(tifkud_civil$tifkud), plot = F,main=colnames(tifkud_civil$tifkud),font=2),2)

mode<-function(X)
{
  temp<-table (as.vector(X))
  names (temp)[temp==max(temp)]
}
mode(ranks_new_civil_combined$GibDate_rank_new_date_gap)
round(describe(as.numeric(unlist(tifkud_civil$tifkud))),2)


freq(ordered(JOMAG_predictores_criteria_merged_civil_officers$tifkud), plot = F,main=colnames(JOMAG_predictores_criteria_merged_civil_officers$tifkud),font=2)
freq(ordered(JOMAG_predictores_criteria_merged_civil$blue), plot = F,main=colnames(JOMAG_predictores_criteria_merged_civil$blue),font=2)
freq(ordered(JOMAG_predictores_criteria_merged_civil_qv$ethiopian), plot = F,main=colnames(JOMAG_predictores_criteria_merged_civil_qv$ethiopian),font=2)
freq(ordered(JOMAG_predictores_criteria_merged_civil_qv$jewish), plot = F,main=colnames(JOMAG_predictores_criteria_merged_civil_qv$jewish),font=2)

freq(ordered(JOMAG_predictores_criteria_merged_civil$mYachasimBefore2_zscore), plot = F,main=colnames(JOMAG_predictores_criteria_merged_civil$mYachasimBefore2_zscore),font=2)



round(describe(as.numeric(unlist(JOMAG_predictores_criteria_merged_civil$exercises))),2)
colnames(JOMAG_predictores_criteria_merged_civil_qv[1:1000])
colnames(JOMAG_predictores_criteria_merged_civil_qv[1001:ncol(JOMAG_predictores_criteria_merged_civil_qv)])
colnames(JOMAG_predictores_criteria_merged_civil[1:1000])
colnames(JOMAG_predictores_criteria_merged_civil[1001:ncol(JOMAG_predictores_criteria_merged_civil)])
colnames(JOMAG_predictores_criteria_merged_civil_officers[1:1000])
colnames(JOMAG_predictores_criteria_merged_civil_officers[1001:ncol(JOMAG_predictores_criteria_merged_civil_officers)])

colnames(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar)
colnames(JOMAG_predictores_criteria_merged_civil[1:1000])


colnames(JOMAG_predictores_criteria_merged_civil_old[1001:ncol(JOMAG_predictores_criteria_merged_civil_old)])
JOMAG_predictores_criteria_merged_civil_qv<-JOMAG_predictores_criteria_merged_civil_qv[-1475]
colnames(JOMAG_predictores_criteria_merged_civil_qv)
# head(JOMAG_predictores_criteria_merged_civil$final_apptitudes_new)
head(JOMAG_predictores_criteria_merged_civil_qv_complete)
nrow(JOMAG_predictores_criteria_merged_civil_qv)
cor.test(as.numeric(JOMAG_predictores_criteria_merged_civil_qv$FileGrade),as.numeric(JOMAG_predictores_criteria_merged_civil_qv$tkufatit_am_regular_special),use="pairwise.complete.obs")
# cor.test(as.numeric(JOMAG_predictores_criteria_merged_civil$FileGrade),as.numeric(JOMAG_predictores_criteria_merged_civil$pum_zscore),use="pairwise.complete.obs")

completedata<-c()
completedata$FileGrade<-JOMAG_predictores_criteria_merged_civil_qv$FileGrade
completedata$tkufatitam<-JOMAG_predictores_criteria_merged_civil_qv$tkufatitam
completedata$socio_zscore<-JOMAG_predictores_criteria_merged_civil_qv$socio_zscore
completedata$seniority_days<-JOMAG_predictores_criteria_merged_civil_qv$seniority_days
completedata<-as.data.frame(completedata)
completedata <- completedata[complete.cases(completedata),]
library(ppcor)
spcor.test(as.numeric(completedata$FileGrade),as.numeric(completedata$tkufatitam),as.numeric(completedata$seniority_days))
CrossTable(JOMAG_predictores_criteria_merged_civil$officer,JOMAG_predictores_criteria_merged_civil$gender,simulate.p.value=TRUE)
write.csv(JOMAG_predictores_criteria_merged_civil_qv, file="C:/Users/Administrator.MAMADA-777/Documents/Junior officers MAGAV validation/JOMAG_predictores_criteria_merged_civil_qv.csv")
write.csv(JOMAG_predictores_criteria_merged_civil, file="C:/Users/Administrator.MAMADA-777/Documents/Junior officers MAGAV validation/JOMAG_predictores_criteria_merged_civil.csv")
write.csv(JOMAG_predictores_criteria_merged_civil_qv, file="C:/Users/Asher/Documents/MAMDA/JOMAGAV/JOMAG_predictores_criteria_merged_civil_qv.csv")
write.csv(gibush_candidates_kakatz_03.2021_civil, file="C:/Users/Asher/Documents/MAMDA/JOMAGAV/gibush_candidates_kakatz_03.2021_civil_new22.csv")

# https://www.r-bloggers.com/hebrew-using-hebrew-in-r/
library(readr)
locale("he")
JOMAG_predictores_criteria_merged_civil<-read_csv("C:/Users/Asher/Documents/MAMDA/JOMAGAV/JOMAG_predictores_criteria_merged_civil_old.csv",locale = locale(date_names = "he", encoding = "ISO-8859-8"))
JOMAG_predictores_criteria_merged_civil<-JOMAG_predictores_criteria_merged_civil[-1]
JOMAG_predictores_criteria_merged_civil_qv<-read_csv("C:/Users/Asher/Documents/MAMDA/JOMAGAV/JOMAG_predictores_criteria_merged_civil_qv_old.csv",locale = locale(date_names = "he", encoding = "ISO-8859-8"))
JOMAG_predictores_criteria_merged_civil_qv<-JOMAG_predictores_criteria_merged_civil_qv[-1]


library(readr)
locale("he")
JOMAG_predictores_criteria_merged_civil<-read_csv("C:/Users/Administrator.MAMADA-777/Documents/Junior officers MAGAV validation/JOMAG_predictores_criteria_merged_civil_old.csv",locale = locale(date_names = "he", encoding = "ISO-8859-8"))
JOMAG_predictores_criteria_merged_civil<-JOMAG_predictores_criteria_merged_civil[-1]
JOMAG_predictores_criteria_merged_civil_qv<-read_csv("C:/Users/Administrator.MAMADA-777/Documents/Junior officers MAGAV validation/JOMAG_predictores_criteria_merged_civil_qv_old.csv",locale = locale(date_names = "he", encoding = "ISO-8859-8"))
JOMAG_predictores_criteria_merged_civil_qv<-JOMAG_predictores_criteria_merged_civil_qv[-1]


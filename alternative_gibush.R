
# Alternative to the Gibush.    $$$$$$$$$$$$$$$$$$$$$$$$$$

#Tali code

mac_datets_and_scores<-read.csv("Q:/04_Mehkar/18_asher/Junior officers MAGAV validation/Files from MAGAV/mac_datets_and_scores.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
colnames(mac_datets_and_scores)[1]<-"personal_number"
colnames(mac_datets_and_scores)[13]<-"end_mac_course_date"
colnames(mac_datets_and_scores)[15]<-"final_mac_course_score"
mac_datets_and_scores$end_mac_course_date<-as.Date(as.character(mac_datets_and_scores$end_mac_course_date),format="%d/%m/%Y")
n_occur<-data.frame(table(mac_datets_and_scores$personal_number))
n_occur[n_occur$Freq>1,]
library (data.table)
mac_datets_and_scores<-setDT(mac_datets_and_scores)[,.SD[which.max(end_mac_course_date)],keyby=personal_number]
n_occur<-data.frame(table(mac_datets_and_scores$personal_number))
n_occur[n_occur$Freq>1,]

all_policemen_08.03.2020<-read.csv("Q:/04_Mehkar/18_asher/all_policemen_08.03.2020.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
all_policemen_08.03.2020<-as.data.frame(all_policemen_08.03.2020)
colnames(all_policemen_08.03.2020)[1]<-"personal_number"
colnames(all_policemen_08.03.2020)[2]<-"id"
n_occur<-data.frame(table(all_policemen_08.03.2020$personal_number))
n_occur[n_occur$Freq>1,]
filtered_id=all_policemen_08.03.2020%>%
  select(personal_number,id)
mac_datets_and_scores<-merge(mac_datets_and_scores,filtered_id,by=c("personal_number"), all.x=T, all.y=F,sort = FALSE)

JOMAG_predictores_criteria_merged<-read.csv("Q:/04_Mehkar/18_asher/Junior officers MAGAV validation/JOMAG_predictores_criteria_merged.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
JOMAG_predictores_criteria_merged<-JOMAG_predictores_criteria_merged[-1]

library(dplyr)

filtered_GibDate=JOMAG_predictores_criteria_merged %>%
  select(personal_number,GibDate)
mac_datets_and_scores<-merge(mac_datets_and_scores,filtered_GibDate,by=c("personal_number"), all.x=T, all.y=T,sort = FALSE)
mac_datets_and_scores=mac_datets_and_scores%>%
  filter(!is.na(GibDate))
nrow(mac_datets_and_scores)

period_eval._2015<-read.csv("Q:/04_Mehkar/18_asher/Junior officers MAGAV validation/Files from Ronen/period_eval._2015.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
period_eval._2017<-read.csv("Q:/04_Mehkar/18_asher/Junior officers MAGAV validation/Files from Ronen/period_eval._2017.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
tkufatit_2014_2015<-read.csv("Q:/04_Mehkar/18_asher/Junior officers MAGAV validation/Files from Ronen/tkufatit_2014_2015.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
tkufatit_2018<-read.csv("Q:/04_Mehkar/18_asher/Junior officers MAGAV validation/Files from Ronen/tkufatit_2018.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
colnames(period_eval._2015)[7]<-"personal_number"
colnames(period_eval._2017)[7]<-"personal_number"
colnames(tkufatit_2014_2015)[1]<-"personal_number"
colnames(tkufatit_2014_2015)[1]<-"personal_number"
colnames(tkufatit_2018)[1]<-"evaluated2018"
colnames(tkufatit_2018)[2]<-"personal_number"
colnames(tkufatit_2018)[3]<-"valid.from.date2018"
colnames(tkufatit_2018)[4]<-"valid.until.date2018"
colnames(tkufatit_2018)[5]<-"form2018"
colnames(tkufatit_2018)[6]<-"key.form2018"
colnames(tkufatit_2018)[7]<-"status2018"
colnames(tkufatit_2018)[8]<-"sub.status2018"
colnames(tkufatit_2018)[9]<-"shoulderrank2018"
colnames(tkufatit_2018)[10]<-"workers.group2018"
colnames(tkufatit_2018)[11]<-"workers.sub.group2018"
colnames(tkufatit_2018)[12]<-"organiztional.unit2018"
colnames(tkufatit_2018)[13]<-"organiztional.level22018"
colnames(tkufatit_2018)[14]<-"direct.commander.score2018"
colnames(tkufatit_2018)[15]<-"apointed.commander.score2018"
colnames(tkufatit_2018)[16]<-"superior.relative.score2018"
colnames(tkufatit_2018)[17]<-"direct.relative.score2018"
colnames(tkufatit_2018)[18]<-"group.size.2018"

library(stringr)
tkufatit_2018[]<-lapply(tkufatit_2018,str_trim)
is.na(tkufatit_2018)<-tkufatit_2018==''

tkufatit_2018$direct.commander.score2018[tkufatit_2018$direct.commander.score2018==0]<-NA
tkufatit_2018$apointed.commander.score2018[tkufatit_2018$apointed.commander.score2018==0]<-NA
tkufatit_2018$group.size.2018[tkufatit_2018$group.size.2018==0]<-NA
tkufatit_2018$final.score.2018<-NA
for(i in 1:nrow(tkufatit_2018)){
  tkufatit_2018[i,]$final.score.2018<-
    ifelse(tkufatit_2018[i,]$status2018=="הושלם",
           ifelse(is.na(tkufatit_2018[i,]$apointed.commander.score2018),tkufatit_2018[i,]$direct.commander.score2018,tkufatit_2018[i,]$apointed.commander.score2018),NA)       
}
# Remove duplicate rows from tkufatit_2018.
n_occur<-data.frame(table(tkufatit_2018$personal_number))
n_occur[n_occur$Freq>1,]
tkufatit_2018<-subset(tkufatit_2018,status2018=="הושלם" & !is.na(final.score.2018))
n_occur<-data.frame(table(tkufatit_2018$personal_number))
n_occur[n_occur$Freq>1,]

period_eval._2015_2017 <- merge(period_eval._2015, period_eval._2017,by=c("personal_number"), all.x=T, all.y=T,sort = FALSE)
period_eval._2015_2017_tkufatit_2014_2015 <- merge(period_eval._2015_2017, tkufatit_2014_2015,by=c("personal_number"), all.x=T, all.y=T,sort = FALSE)
criteria_merged<-merge(period_eval._2015_2017_tkufatit_2014_2015,tkufatit_2018,by=c("personal_number"), all.x=T, all.y=T,sort = FALSE)

for(i in 1:ncol(criteria_merged)){
  colnames(criteria_merged)[i]<-gsub(".x","2015",colnames(criteria_merged)[i])}
for(i in 1:ncol(criteria_merged)){
  colnames(criteria_merged)[i]<-gsub(".y","2017",colnames(criteria_merged)[i])}

# Uncheck dplyr because it's in conflict with plyr package.*********
library (plyr)
criteria_merged<-rename(criteria_merged,c("מעריך2015"="evaluator2015","עובד.מעריך...מ.א2015"="worker evaluator p.n.2015",
                                          "יחידה.מעריך2015"="unit.evaluator2015", "מעריך.נוסף2015"="additional.evaluator2015",
                                          "מעריך.נוסף...מ.א2015"="additional.evaluator.p.n2015",
                                          "מוערך2015"="evaluated2015","תקף.מתאריך2015"="valid.from.date.2015",
                                          "תקף.עד.תאריך2015"="valid.until.date.2015","טופס2015"="form2015",
                                          "טופס...מפתח2015"="keyform2015","סטטוס2015"="status2015","תת.סטטוס2015"="substatus2015",
                                          "דרגה.כתף2015"="shoulderrank2015","קב..עובדים2015"="workers.group2015",
                                          "תת.קבוצה.עובדים2015"="workers.sub.group2015","יחידה.ארגונית2015"="organizational.unit2015",
                                          "רמה.אירגונית.22015"="organizational.level22015","רמה.היררכית.032015"="hirarchic.level032015",
                                          "רמה.היררכית.052015"="hirarchic.level052015","רמה.היררכית.042015"="hirarchic.level042015",
                                          "ציון.מפקד.ישיר2015"="direct.commander.score2015", "ציון.מפקד.ממונה2015"="apointed.commander.score2015",
                                          "ציון.יחסי.ממונה2015"="superior.relative.score2015",
                                          "ציון.יחסי.ישיר2015"="direct.relative.score2015", "גודל.קבוצה2015"="group.size.2015",
                                          "ציון.סופי2015"="final.score.2015",
                                          "מעריך2017"="evaluator2017","עובד.מעריך...מ.א2017"="worker evaluator p.n.2017",
                                          "יחידה.מעריך2017"="unit.evaluator2017","מעריך.נוסף2017"="additional.evaluator2017",
                                          "מעריך.נוסף...מ.א2017"="additional.evaluator.p.n2017",
                                          "מוערך2017"="evaluated2017","תקף.מתאריך2017"="valid.from.date.2017",
                                          "תקף.עד.תאריך2017"="valid.until.date.2017","טופס2017"="form2017",
                                          "טופס...מפתח2017"="keyform2017","סטטוס2017"="status2017","תת.סטטוס2017"="substatus2017",
                                          "דרגה.כתף2017"="shoulderrank2017","קב..עובדים2017"="workers.group2017",
                                          "תת.קבוצה.עובדים2017"="workers.sub.group2017","יחידה.ארגונית2017"="organizational.unit2017",
                                          "רמה.אירגונית.22017"="organizational.level2017","רמה.היררכית.032017"="hirarchic.level032017",
                                          "רמה.היררכית.052017"="hirarchic.level052017","רמה.היררכית.042017"="hirarchic.level042017",
                                          "ציון.מפקד.ישיר2017"="direct.commander.score2017", "ציון.מפקד.ממונה2017"="apointed.commander.score2017",
                                          "ציון.יחסי.ממונה2017"="superior.relative.score2017",
                                          "ציון.יחסי.ישיר2017"="direct.relative.score2017", "גודל.קבוצה2017"="group.size.2017",
                                          "ציון.סופי2017"="final.score.2017"))

#Remove duplicate rows from criteria_merged.
n_occur<-data.frame(table(criteria_merged$personal_number))
n_occur[n_occur$Freq>1,]
criteria_merged <- criteria_merged[!duplicated(criteria_merged[,c("personal_number")]),]
n_occur<-data.frame(table(criteria_merged$personal_number))
n_occur[n_occur$Freq>1,]

# Merge predictors and criteria.
mac_datets_and_scores$personal_number<-as.numeric(mac_datets_and_scores$personal_number)
criteria_merged$personal_number<-as.numeric(criteria_merged$personal_number)
mac_datets_and_scores <- merge(mac_datets_and_scores, criteria_merged,by=c("personal_number"), all.x=T, all.y=F,sort = FALSE)
mac_datets_and_scores<-as.data.frame(mac_datets_and_scores)

# Check duplicate rows in mac_datets_and_scores.
n_occur<-data.frame(table(mac_datets_and_scores$personal_number))
n_occur[n_occur$Freq>1,]
nrow(mac_datets_and_scores)

# Clean mac_datets_and_scores file from erreous data.
mac_datets_and_scores$direct.commander.score2015[mac_datets_and_scores$direct.commander.score2015==0]<-NA
mac_datets_and_scores$apointed.commander.score2015[mac_datets_and_scores$apointed.commander.score2015==0]<-NA
mac_datets_and_scores$group.size.2015[mac_datets_and_scores$group.size.2015==0]<-NA
mac_datets_and_scores$final.score.2015[mac_datets_and_scores$final.score.2015==0]<-NA
mac_datets_and_scores$direct.commander.score2017[mac_datets_and_scores$direct.commander.score2017==0]<-NA
mac_datets_and_scores$apointed.commander.score2017[mac_datets_and_scores$apointed.commander.score2017==0]<-NA
mac_datets_and_scores$group.size.2017[mac_datets_and_scores$group.size.2017==0]<-NA
mac_datets_and_scores$final.score.2017[mac_datets_and_scores$final.score.2017==0]<-NA

mac_datets_and_scores$date.tkufatit_14<-"26/01/2015"
mac_datets_and_scores$date.tkufatit_14<-as.Date(unlist(mac_datets_and_scores$date.tkufatit_14),format="%d/%m/%Y")
mac_datets_and_scores$date.tkufatit_15<-"01/06/2016"
mac_datets_and_scores$date.tkufatit_15<-as.Date(mac_datets_and_scores$date.tkufatit_15,format="%d/%m/%Y")
mac_datets_and_scores$date.period.eval.2015<-"01/12/2015"
mac_datets_and_scores$date.period.eval.2015<-as.Date(mac_datets_and_scores$date.period.eval.2015,format="%d/%m/%Y")
mac_datets_and_scores$date.period.eval.2017<-"01/05/2017"
mac_datets_and_scores$date.period.eval.2017<-as.Date(mac_datets_and_scores$date.period.eval.2017,format="%d/%m/%Y")
mac_datets_and_scores$date.period.eval.2018<-"31/12/2018"
mac_datets_and_scores$date.period.eval.2018<-as.Date(mac_datets_and_scores$date.period.eval.2018,format="%d/%m/%Y")
mac_datets_and_scores<-as.data.frame(mac_datets_and_scores)
colnames(mac_datets_and_scores)

class(mac_datets_and_scores$end_mac_course_date)
class(mac_datets_and_scores$GibDate)
head(mac_datets_and_scores$GibDate)
mac_datets_and_scores$GibDate<-as.Date(unlist(mac_datets_and_scores$GibDate),format="%d/%m/%Y")
class(mac_datets_and_scores$GibDate)
head(mac_datets_and_scores$GibDate)

#End of Tali code.
# Begin of civil cose.

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
JOMAG_predictores_criteria_merged_civil<-read_csv("C:/Users/Asher/Documents/MAMDA/JOMAGAV/alternative_gibush/JOMAG_predictores_criteria_merged_civil_old.csv",locale = locale(date_names = "he", encoding = "ISO-8859-8"))
JOMAG_predictores_criteria_merged_civil<-JOMAG_predictores_criteria_merged_civil[-1]
mac_datets_and_scores_civil$id<-NULL

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
  select(personal_number,id,FileGrade,VaadaGrade_completed,officer,gender_new)
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
           GroupId_courses_soc_mac==5031)

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

soc_mac_civil_am <- soc_mac_civil_am %>% 
  mutate_at(c(4:45), funs(c(scale(.))))

soc_mac_civil_am<-as.data.frame(soc_mac_civil_am)
#soc_mac_civil_am[c(4:45)][abs(soc_mac_civil_am[c(4:45)])>4]<-NA

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

DAPAR_GIBUSH_MAGAV_civil<-read.csv("C:/Users/Asher/Documents/MAMDA/JOMAGAV/alternative_gibush/DAPAR_GIBUSH_MAGAV_civil.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
DAPAR_GIBUSH_MAGAV_civil<-DAPAR_GIBUSH_MAGAV_civil[-c(1,4,5)]
colnames(DAPAR_GIBUSH_MAGAV_civil)[2]<-paste(colnames(DAPAR_GIBUSH_MAGAV_civil)[2],"DAPAR",sep="_")
n_occur<-data.frame(table(DAPAR_GIBUSH_MAGAV_civil$id))
n_occur[n_occur$Freq>1,]
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

# Correlations predictors-criteria
mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar_relevant_predictors_columns_for_correlations <- mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar[c(16,106,114,115,118,119)]
mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar_relevant_predictors_columns_names_for_correlations <- c(colnames(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar[c(16,106,114,115,118,119)]))
mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar_relevant_criteria_columns_for_correlations <- mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar[c(101,102)]
mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar_relevant_criteria_columns_names_for_correlations <- c(colnames(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar[c(101,102)]))
mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar_corr_output<-data.frame()[1:2,]

for(j in 1:length(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar_relevant_predictors_columns_names_for_correlations)){
  corr_output_temp<-c()
  for(i in 1:length(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar_relevant_criteria_columns_names_for_correlations)){
    corr_temp<-c()
    corr_try <- try(cor.test(as.numeric(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar_relevant_predictors_columns_for_correlations[[j]]),as.numeric(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar_relevant_criteria_columns_for_correlations[[i]]),use="pairwise.complete.obs"), silent=T)
    corr_temp$"predictor" <-ifelse(class(corr_try)=="try-error", NA, corr_try$estimate)
    corr_temp$p.value <-ifelse(class(corr_try)=="try-error", NA, corr_try$p.value)
    corr_temp$n <-(ifelse(class(corr_try)=="try-error", NA, corr_try$parameter+2))
    corr_temp<-data.frame(corr_temp)
    colnames(corr_temp)[1]<-mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar_relevant_predictors_columns_names_for_correlations[[j]]
    row.names(corr_temp)<-mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar_relevant_criteria_columns_names_for_correlations[i]
    corr_output_temp<-rbind (corr_output_temp,corr_temp)
    corr_output_temp <-round(corr_output_temp,2)
  }
  corr_output_temp$""<-"|"
  mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar_corr_output<-cbind(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar_corr_output,corr_output_temp)
}
row.names(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar_corr_output)<-mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar_relevant_criteria_columns_names_for_correlations

for(i in 1:(ncol(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar_corr_output)/4)){
  colnames(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar_corr_output)[i*4] <- ""
}
write.csv(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar_corr_output,file = "C:/Users/Asher/Documents/MAMDA/JOMAGAV/alternative_gibush/mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar_p_c_corr_output.csv")

# Correlations within predictors.
mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar_relevant_predictors_columns_for_correlations <- mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar[c(16,106,114,115,118,119)]
mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar_relevant_predictors_columns_names_for_correlations <- c(colnames(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar[c(16,106,114,115,118,119)]))
mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar_corr_output<-data.frame()[1:6,]

for(j in 1:length(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar_relevant_predictors_columns_names_for_correlations)){
  corr_output_temp<-c()
  for(i in 1:length(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar_relevant_predictors_columns_names_for_correlations)){
    corr_temp<-c()
    corr_try <- try(cor.test(as.numeric(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar_relevant_predictors_columns_for_correlations[[j]]),as.numeric(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar_relevant_predictors_columns_for_correlations[[i]]),use="pairwise.complete.obs"), silent=T)
    corr_temp$"predictor" <-ifelse(class(corr_try)=="try-error", NA, corr_try$estimate)
    corr_temp$p.value <-ifelse(class(corr_try)=="try-error", NA, corr_try$p.value)
    corr_temp$n <-(ifelse(class(corr_try)=="try-error", NA, corr_try$parameter+2))
    corr_temp<-data.frame(corr_temp)
    colnames(corr_temp)[1]<-mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar_relevant_predictors_columns_names_for_correlations[[j]]
    row.names(corr_temp)<-mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar_relevant_predictors_columns_names_for_correlations[i]
    corr_output_temp<-rbind (corr_output_temp,corr_temp)
    corr_output_temp <-round(corr_output_temp,2)
  }
  corr_output_temp$""<-"|"
  mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar_corr_output<-cbind(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar_corr_output,corr_output_temp)
}
row.names(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar_corr_output)<-mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar_relevant_predictors_columns_names_for_correlations

for(i in 1:(ncol(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar_corr_output)/4)){
  colnames(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar_corr_output)[i*4] <- ""
}
write.csv(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar_corr_output,file = "C:/Users/Asher/Documents/MAMDA/JOMAGAV/alternative_gibush/mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar_corr_output_p-p.csv")

# sadir/keva on FileGrade

filtered_keva_FileGrade=mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar %>%
  filter(!is.na(keva) & !is.na(FileGrade))
filtered_keva_FileGrade %>% 
  group_by(keva) %>%  
  summarise_at(vars(FileGrade),funs(mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),n())) 

t_test_temp<-c()
t_test_try <- try(t.test(as.numeric(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar$FileGrade)~as.numeric(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar$keva),use="pairwise.complete.obs"), silent=T)
t_test_temp$statistic <-ifelse(class(t_test_try)=="try-error", NA, t_test_try$statistic)
t_test_temp$parameter <-ifelse(class(t_test_try)=="try-error", NA, t_test_try$parameter)
t_test_temp$p.value <-ifelse(class(t_test_try)=="try-error", NA, t_test_try$p.value)
t_test_temp$"mean in group 1" <-ifelse(class(t_test_try)=="try-error", NA, t_test_try$estimate[[1]])
t_test_temp$"mean in group 2" <-ifelse(class(t_test_try)=="try-error", NA, t_test_try$estimate[[2]])
t_test_temp<-data.frame(t_test_temp)
row.names(t_test_temp)<-"FileGrade"
t_test_temp

# sadir/keva on VaadaGrade_completed

filtered_keva_VaadaGrade_completed=mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar %>%
  filter(!is.na(keva) & !is.na(VaadaGrade_completed))
filtered_keva_VaadaGrade_completed %>% 
  group_by(keva) %>%  
  summarise_at(vars(VaadaGrade_completed),funs(mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),n())) 

t_test_temp<-c()
t_test_try <- try(t.test(as.numeric(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar$VaadaGrade_completed)~as.numeric(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar$keva),use="pairwise.complete.obs"), silent=T)
t_test_temp$statistic <-ifelse(class(t_test_try)=="try-error", NA, t_test_try$statistic)
t_test_temp$parameter <-ifelse(class(t_test_try)=="try-error", NA, t_test_try$parameter)
t_test_temp$p.value <-ifelse(class(t_test_try)=="try-error", NA, t_test_try$p.value)
t_test_temp$"mean in group 1" <-ifelse(class(t_test_try)=="try-error", NA, t_test_try$estimate[[1]])
t_test_temp$"mean in group 2" <-ifelse(class(t_test_try)=="try-error", NA, t_test_try$estimate[[2]])
t_test_temp<-data.frame(t_test_temp)
row.names(t_test_temp)<-"VaadaGrade_completed"
t_test_temp


round(freq(ordered(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar$officer), plot = F,main=colnames(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar$officer),font=2),2)

logitofficer <- glm(officer ~ final_mac_course_score 
                    + keva
                    + kaba
                    + am_courses_soc_mac
                    + dapar_copmlete
                    + tenure, 
                    data=mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar, 
                    family=binomial(link="logit"))
summary(logitofficer)

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

#B
# round(0.01790,2)
# round(0.02880,2)
# round(0.16181,2)
# round(0.16900,2)

# reg_FileGrade2 <- lm(FileGrade ~ final_mac_course_score
#                      + dapar_copmlete
#                      + am_courses_soc_mac,
#                      data=mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar)
# summary(reg_FileGrade2)
# 
# 
# reg_FileGrade3 <- lm(FileGrade ~ final_mac_course_score
#                      + keva
#                      + kaba
#                      + am_courses_soc_mac,
#                      data=mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar)
# summary(reg_FileGrade3)
# 
# 
# reg_FileGrade4 <- lm(FileGrade ~ final_mac_course_score
#                      + kaba
#                      + am_courses_soc_mac
#                      + tenure,
#                      data=mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar)
# summary(reg_FileGrade4)
# 
# 
# reg_FileGrade5 <- lm(FileGrade ~ final_mac_course_score
#                      + keva
#                      + kaba
#                      + am_courses_soc_mac
#                      + dapar_copmlete,
#                      data=mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar)
# summary(reg_FileGrade5)
# 
# 
# reg_FileGrade6 <- lm(FileGrade ~ final_mac_course_score
#                      + kaba
#                      + am_courses_soc_mac
#                      + dapar_copmlete
#                      + tenure,
#                      data=mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar)
# summary(reg_FileGrade6)
# 
# 
# reg_VaadaGrade_completed1 <- lm(VaadaGrade_completed ~ final_mac_course_score
#                                 + kaba
#                                 + am_courses_soc_mac,
#                                 data=mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar)
# summary(reg_VaadaGrade_completed1)
# 
# # standardised coefficients
# round(lm.beta(reg_VaadaGrade_completed1),2)

# R
# R<-round(sqrt(0.1034),2)
# R

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

# # library(ggplot2)
# # # SIZE AND COLOR
# # # Same coloring as above, size corresponding as well
# ggplot(filtered_residual, aes(x = final_mac_course_score, y = FileGrade)) +
#   geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
#   geom_segment(aes(xend = final_mac_course_score, yend = predicted_FileGrade), alpha = .2) +
# 
#   # > Color AND size adjustments made here...
#   geom_point(aes(color = abs(residual), size = abs(residual))) + # size also mapped
#   scale_color_continuous(low = "green", high = "red") +
#   guides(color = FALSE, size = FALSE) +  # Size legend also removed
#   
# 
#   geom_point(aes(y = predicted_FileGrade), shape = 1) +
#   theme_bw()

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

# combine new and old predicroes

JOMAG_predictores_criteria_merged_civil$final_apptitudes_new_dic<-ifelse(JOMAG_predictores_criteria_merged_civil$final_apptitudes_new< -0.028327486,0,1)

JOMAG_predictores_criteria_merged_civil_qv<-JOMAG_predictores_criteria_merged_civil[which(!is.na(JOMAG_predictores_criteria_merged_civil$FileGrade)
                                                                                          & JOMAG_predictores_criteria_merged_civil$critria_count>1),]

library(dplyr)
filtered_field_criteria = filtered_residual %>%
  select(personal_number, final_mac_course_score, kaba, am_courses_soc_mac,FileGrade, predicted_FileGrade)

count_matching<-match(JOMAG_predictores_criteria_merged_civil_qv$personal_number,filtered_field_criteria$personal_number)
round(freq(ordered(count_matching), plot = F,main=colnames(count_matching),font=2),2)
JOMAG_predictores_criteria_merged_civil_qv$FileGrade<-NULL
JOMAG_predictores_criteria_merged_civil_qv_field<- merge(JOMAG_predictores_criteria_merged_civil_qv,filtered_field_criteria,by=c("personal_number"), all.x=T, all.y=F,sort = FALSE)

round(freq(ordered(filtered_field_criteria$FileGrade), plot = F,main=colnames(filtered_field_criteria$FileGrade),font=2),2)
round(freq(ordered(filtered_field_criteria$predicted_FileGrade), plot = F,main=colnames(filtered_field_criteria$predicted_FileGrade),font=2),2)
round(freq(ordered(JOMAG_predictores_criteria_merged_civil_qv_field$FileGrade), plot = F,main=colnames(JOMAG_predictores_criteria_merged_civil_qv_field$FileGrade),font=2),2)
round(freq(ordered(JOMAG_predictores_criteria_merged_civil_qv_field$predicted_FileGrade), plot = F,main=colnames(JOMAG_predictores_criteria_merged_civil_qv_field$predicted_FileGrade),font=2),2)
round(freq(ordered(JOMAG_predictores_criteria_merged_civil_qv_field$final_mac_course_score), plot = F,main=colnames(JOMAG_predictores_criteria_merged_civil_qv_field$final_mac_course_score),font=2),2)


reg_tkufatitam <- lm(tkufatitam ~ final_apptitudes_new_dic
                     + TsiunSofiAfter
                     + SocioGrade
                     + PersonalityGrade,
                     data=JOMAG_predictores_criteria_merged_civil_qv_field)
summary(reg_tkufatitam)


#good*************************
reg_tkufatitam <- lm(tkufatitam ~ TsiunSofiAfter
                     + PersonalityGrade
                     + am_courses_soc_mac,
                     data=JOMAG_predictores_criteria_merged_civil_qv_field)
summary(reg_tkufatitam)

round(lm.beta(reg_tkufatitam),2)

# R
R<-round(sqrt(0.1917),2)
R

JOMAG_predictores_criteria_merged_civil_qv_field <- JOMAG_predictores_criteria_merged_civil_qv_field %>% 
  mutate(predicted_with_field = .25*TsiunSofiAfter + .23*PersonalityGrade + .52*am_courses_soc_mac)%>%
  mutate(predicted_without_field = .05*final_apptitudes_new + .31*TsiunSofiAfter_zscore + .32*SocioGrade_zscore + .32*PersonalityGrade_zscore)


# colnames(JOMAG_predictores_criteria_merged_civil_qv_field[1:1000])
# colnames(JOMAG_predictores_criteria_merged_civil_qv_field[1001:ncol(JOMAG_predictores_criteria_merged_civil_qv_field)])
# 
# grep("TsiunSofiAfter", colnames(JOMAG_predictores_criteria_merged_civil_qv_field))
# grep("PersonalityGrade", colnames(JOMAG_predictores_criteria_merged_civil_qv_field))
# grep("FileGrade", colnames(JOMAG_predictores_criteria_merged_civil_qv_field))
# grep("VaadaGrade_complete", colnames(JOMAG_predictores_criteria_merged_civil_qv_field))
# grep("tkufatitam", colnames(JOMAG_predictores_criteria_merged_civil_qv_field))


# Correlations predictors-criteria
JOMAG_predictores_criteria_merged_civil_qv_field_relevant_predictors_columns_for_correlations <- JOMAG_predictores_criteria_merged_civil_qv_field[c(60,61,64,1460:1463,1457,1464,1465)]
JOMAG_predictores_criteria_merged_civil_qv_field_relevant_predictors_columns_names_for_correlations <- c(colnames(JOMAG_predictores_criteria_merged_civil_qv_field[c(60,61,64,1460:1463,1457,1464,1465)]))
JOMAG_predictores_criteria_merged_civil_qv_field_relevant_criteria_columns_for_correlations <- JOMAG_predictores_criteria_merged_civil_qv_field[c(1440)]
JOMAG_predictores_criteria_merged_civil_qv_field_relevant_criteria_columns_names_for_correlations <- c(colnames(JOMAG_predictores_criteria_merged_civil_qv_field[c(1440)]))
JOMAG_predictores_criteria_merged_civil_qv_field_corr_output<-data.frame()[1,]

for(j in 1:length(JOMAG_predictores_criteria_merged_civil_qv_field_relevant_predictors_columns_names_for_correlations)){
  corr_output_temp<-c()
  for(i in 1:length(JOMAG_predictores_criteria_merged_civil_qv_field_relevant_criteria_columns_names_for_correlations)){
    corr_temp<-c()
    corr_try <- try(cor.test(as.numeric(JOMAG_predictores_criteria_merged_civil_qv_field_relevant_predictors_columns_for_correlations[[j]]),as.numeric(JOMAG_predictores_criteria_merged_civil_qv_field_relevant_criteria_columns_for_correlations[[i]]),use="pairwise.complete.obs"), silent=T)
    corr_temp$"predictor" <-ifelse(class(corr_try)=="try-error", NA, corr_try$estimate)
    corr_temp$p.value <-ifelse(class(corr_try)=="try-error", NA, corr_try$p.value)
    corr_temp$n <-(ifelse(class(corr_try)=="try-error", NA, corr_try$parameter+2))
    corr_temp<-data.frame(corr_temp)
    colnames(corr_temp)[1]<-JOMAG_predictores_criteria_merged_civil_qv_field_relevant_predictors_columns_names_for_correlations[[j]]
    row.names(corr_temp)<-JOMAG_predictores_criteria_merged_civil_qv_field_relevant_criteria_columns_names_for_correlations[i]
    corr_output_temp<-rbind (corr_output_temp,corr_temp)
    corr_output_temp <-round(corr_output_temp,2)
  }
  corr_output_temp$""<-"|"
  JOMAG_predictores_criteria_merged_civil_qv_field_corr_output<-cbind(JOMAG_predictores_criteria_merged_civil_qv_field_corr_output,corr_output_temp)
}
row.names(JOMAG_predictores_criteria_merged_civil_qv_field_corr_output)<-JOMAG_predictores_criteria_merged_civil_qv_field_relevant_criteria_columns_names_for_correlations

for(i in 1:(ncol(JOMAG_predictores_criteria_merged_civil_qv_field_corr_output)/4)){
  colnames(JOMAG_predictores_criteria_merged_civil_qv_field_corr_output)[i*4] <- ""
}
write.csv(JOMAG_predictores_criteria_merged_civil_qv_field_corr_output,file = "C:/Users/Asher/Documents/MAMDA/JOMAGAV/alternative_gibush/JOMAG_predictores_criteria_merged_civil_qv_field_p_c_corr_output.csv")

# Correlations within predictors.
JOMAG_predictores_criteria_merged_civil_qv_field_relevant_predictors_columns_for_correlations <- JOMAG_predictores_criteria_merged_civil_qv_field[c(60,61,64,1460:1463,1457,1464,1465)]
JOMAG_predictores_criteria_merged_civil_qv_field_relevant_predictors_columns_names_for_correlations <- c(colnames(JOMAG_predictores_criteria_merged_civil_qv_field[c(60,61,64,1460:1463,1457,1464,1465)]))
JOMAG_predictores_criteria_merged_civil_qv_field_corr_output<-data.frame()[1:10,]

for(j in 1:length(JOMAG_predictores_criteria_merged_civil_qv_field_relevant_predictors_columns_names_for_correlations)){
  corr_output_temp<-c()
  for(i in 1:length(JOMAG_predictores_criteria_merged_civil_qv_field_relevant_predictors_columns_names_for_correlations)){
    corr_temp<-c()
    corr_try <- try(cor.test(as.numeric(JOMAG_predictores_criteria_merged_civil_qv_field_relevant_predictors_columns_for_correlations[[j]]),as.numeric(JOMAG_predictores_criteria_merged_civil_qv_field_relevant_predictors_columns_for_correlations[[i]]),use="pairwise.complete.obs"), silent=T)
    corr_temp$"predictor" <-ifelse(class(corr_try)=="try-error", NA, corr_try$estimate)
    corr_temp$p.value <-ifelse(class(corr_try)=="try-error", NA, corr_try$p.value)
    corr_temp$n <-(ifelse(class(corr_try)=="try-error", NA, corr_try$parameter+2))
    corr_temp<-data.frame(corr_temp)
    colnames(corr_temp)[1]<-JOMAG_predictores_criteria_merged_civil_qv_field_relevant_predictors_columns_names_for_correlations[[j]]
    row.names(corr_temp)<-JOMAG_predictores_criteria_merged_civil_qv_field_relevant_predictors_columns_names_for_correlations[i]
    corr_output_temp<-rbind (corr_output_temp,corr_temp)
    corr_output_temp <-round(corr_output_temp,2)
  }
  corr_output_temp$""<-"|"
  JOMAG_predictores_criteria_merged_civil_qv_field_corr_output<-cbind(JOMAG_predictores_criteria_merged_civil_qv_field_corr_output,corr_output_temp)
}
row.names(JOMAG_predictores_criteria_merged_civil_qv_field_corr_output)<-JOMAG_predictores_criteria_merged_civil_qv_field_relevant_predictors_columns_names_for_correlations

for(i in 1:(ncol(JOMAG_predictores_criteria_merged_civil_qv_field_corr_output)/4)){
  colnames(JOMAG_predictores_criteria_merged_civil_qv_field_corr_output)[i*4] <- ""
}
write.csv(JOMAG_predictores_criteria_merged_civil_qv_field_corr_output,file = "C:/Users/Asher/Documents/MAMDA/JOMAGAV/alternative_gibush/JOMAG_predictores_criteria_merged_civil_qv_field_corr_output_p-p.csv")

#predict FileGrade of new candidates 07.2020 $$$$$$$$$$$$$$$$$$$$$$$$$$$$$

library(readr)
locale("he")
gibush_candidates_kakatz_07.2020_civil<-read_csv("C:/Users/Asher/Documents/MAMDA/JOMAGAV/alternative_gibush/gibush_candidates_kakatz_07.2020_civil.csv",locale = locale(date_names = "he", encoding = "ISO-8859-8"))
gibush_candidates_kakatz_07.2020_civil<-gibush_candidates_kakatz_07.2020_civil[-1]
colnames(gibush_candidates_kakatz_07.2020_civil)[1]<-"order_num"
colnames(gibush_candidates_kakatz_07.2020_civil)[13]<-"kaba"
colnames(gibush_candidates_kakatz_07.2020_civil)[16]<-"apptitudes"

gibush_candidates_kakatz_07.2020_civil <-
  merge(gibush_candidates_kakatz_07.2020_civil,filtered_soc_mac_civil_am,by=c("personal_number"), all.x=T, all.y=F,sort = FALSE)
gibush_candidates_kakatz_07.2020_civil$am_courses_soc_mac<-as.numeric(gibush_candidates_kakatz_07.2020_civil$am_courses_soc_mac)

library(descr)
library(psych)
describe(as.numeric(unlist(gibush_candidates_kakatz_07.2020_civil$am_courses_soc_mac)))
freq(ordered(round(gibush_candidates_kakatz_07.2020_civil$am_courses_soc_mac,2)), plot = F,main=colnames(gibush_candidates_kakatz_07.2020_civil$am_courses_soc_mac),font=2)
freq(ordered(gibush_candidates_kakatz_07.2020_civil$GroupName_courses_soc_mac), plot = F,main=colnames(gibush_candidates_kakatz_07.2020_civil$GroupName_courses_soc_mac),font=2)

#final_mac_course_score
library(readr)
locale("he")
mac_datets_and_scores_full_civil<-read_csv("C:/Users/Asher/Documents/MAMDA/JOMAGAV/alternative_gibush/mac_datets_and_scores_full_civil.csv",locale = locale(date_names = "he", encoding = "ISO-8859-8"))
mac_datets_and_scores_full_civil<-mac_datets_and_scores_full_civil[-1]

class(mac_datets_and_scores_full_civil$end_mac_course_date)
head(mac_datets_and_scores_full_civil$end_mac_course_date)
n_occur<-data.frame(table(mac_datets_and_scores_full_civil$personal_number))
n_occur[n_occur$Freq>1,]
library(dplyr)
filtered_mac_datets_and_scores_full_civil=mac_datets_and_scores_full_civil %>%
  select(personal_number,end_mac_course_date,final_mac_course_score)
gibush_candidates_kakatz_07.2020_civil <-
  merge(gibush_candidates_kakatz_07.2020_civil,filtered_mac_datets_and_scores_full_civil,by=c("personal_number"), all.x=T, all.y=F,sort = FALSE)

gibush_candidates_kakatz_07.2020_civil[24][gibush_candidates_kakatz_07.2020_civil[2]==15] <- 96
gibush_candidates_kakatz_07.2020_civil[24][gibush_candidates_kakatz_07.2020_civil[2]==37] <- 92
gibush_candidates_kakatz_07.2020_civil[24][gibush_candidates_kakatz_07.2020_civil[2]==43] <- 70
class(gibush_candidates_kakatz_07.2020_civil$final_mac_course_score)
gibush_candidates_kakatz_07.2020_civil$final_mac_course_score <-
  as.numeric(gibush_candidates_kakatz_07.2020_civil$final_mac_course_score)

# predicted_FileGrade
gibush_candidates_kakatz_07.2020_civil$predicted_FileGrade <- round(predict(reg_FileGrade1, gibush_candidates_kakatz_07.2020_civil),2)
freq(ordered(gibush_candidates_kakatz_07.2020_civil$predicted_FileGrade), plot = F,main=colnames(gibush_candidates_kakatz_07.2020_civil$predicted_FileGrade),font=2)
round(describe(as.numeric(unlist(gibush_candidates_kakatz_07.2020_civil$predicted_FileGrade))),2)

gibush_candidates_kakatz_07.2020_civil$personality <- NA
gibush_candidates_kakatz_07.2020_civil$command_exercise <- NA
gibush_candidates_kakatz_07.2020_civil$interview <- NA

# personality
# updated in candidate 14 from 4 to 4.5
gibush_candidates_kakatz_07.2020_civil[26][gibush_candidates_kakatz_07.2020_civil[2]==1] <- 3
gibush_candidates_kakatz_07.2020_civil[26][gibush_candidates_kakatz_07.2020_civil[2]==2] <- 4
gibush_candidates_kakatz_07.2020_civil[26][gibush_candidates_kakatz_07.2020_civil[2]==3] <- 4
gibush_candidates_kakatz_07.2020_civil[26][gibush_candidates_kakatz_07.2020_civil[2]==4] <- 3
gibush_candidates_kakatz_07.2020_civil[26][gibush_candidates_kakatz_07.2020_civil[2]==5] <- 2.5
gibush_candidates_kakatz_07.2020_civil[26][gibush_candidates_kakatz_07.2020_civil[2]==6] <- 4.5
gibush_candidates_kakatz_07.2020_civil[26][gibush_candidates_kakatz_07.2020_civil[2]==7] <- 3
gibush_candidates_kakatz_07.2020_civil[26][gibush_candidates_kakatz_07.2020_civil[2]==8] <- 3.5
gibush_candidates_kakatz_07.2020_civil[26][gibush_candidates_kakatz_07.2020_civil[2]==9] <- 4
gibush_candidates_kakatz_07.2020_civil[26][gibush_candidates_kakatz_07.2020_civil[2]==10] <- 4.5
gibush_candidates_kakatz_07.2020_civil[26][gibush_candidates_kakatz_07.2020_civil[2]==11] <- 4
gibush_candidates_kakatz_07.2020_civil[26][gibush_candidates_kakatz_07.2020_civil[2]==12] <- 4
gibush_candidates_kakatz_07.2020_civil[26][gibush_candidates_kakatz_07.2020_civil[2]==13] <- 4
gibush_candidates_kakatz_07.2020_civil[26][gibush_candidates_kakatz_07.2020_civil[2]==14] <- 4.5
gibush_candidates_kakatz_07.2020_civil[26][gibush_candidates_kakatz_07.2020_civil[2]==15] <- 5
gibush_candidates_kakatz_07.2020_civil[26][gibush_candidates_kakatz_07.2020_civil[2]==16] <- 3
gibush_candidates_kakatz_07.2020_civil[26][gibush_candidates_kakatz_07.2020_civil[2]==17] <- 4.5
gibush_candidates_kakatz_07.2020_civil[26][gibush_candidates_kakatz_07.2020_civil[2]==18] <- 3.5
gibush_candidates_kakatz_07.2020_civil[26][gibush_candidates_kakatz_07.2020_civil[2]==19] <- 4
gibush_candidates_kakatz_07.2020_civil[26][gibush_candidates_kakatz_07.2020_civil[2]==20] <- 3
gibush_candidates_kakatz_07.2020_civil[26][gibush_candidates_kakatz_07.2020_civil[2]==21] <- 3.5
gibush_candidates_kakatz_07.2020_civil[26][gibush_candidates_kakatz_07.2020_civil[2]==22] <- 4.5
gibush_candidates_kakatz_07.2020_civil[26][gibush_candidates_kakatz_07.2020_civil[2]==23] <- 4.5
gibush_candidates_kakatz_07.2020_civil[26][gibush_candidates_kakatz_07.2020_civil[2]==24] <- 3
gibush_candidates_kakatz_07.2020_civil[26][gibush_candidates_kakatz_07.2020_civil[2]==25] <- 3.5
gibush_candidates_kakatz_07.2020_civil[26][gibush_candidates_kakatz_07.2020_civil[2]==26] <- 3
gibush_candidates_kakatz_07.2020_civil[26][gibush_candidates_kakatz_07.2020_civil[2]==27] <- 5
gibush_candidates_kakatz_07.2020_civil[26][gibush_candidates_kakatz_07.2020_civil[2]==28] <- 2.5
gibush_candidates_kakatz_07.2020_civil[26][gibush_candidates_kakatz_07.2020_civil[2]==29] <- 3.5
gibush_candidates_kakatz_07.2020_civil[26][gibush_candidates_kakatz_07.2020_civil[2]==30] <- 3
gibush_candidates_kakatz_07.2020_civil[26][gibush_candidates_kakatz_07.2020_civil[2]==31] <- 5
gibush_candidates_kakatz_07.2020_civil[26][gibush_candidates_kakatz_07.2020_civil[2]==32] <- 3
gibush_candidates_kakatz_07.2020_civil[26][gibush_candidates_kakatz_07.2020_civil[2]==33] <- 3
gibush_candidates_kakatz_07.2020_civil[26][gibush_candidates_kakatz_07.2020_civil[2]==34] <- 2.5
gibush_candidates_kakatz_07.2020_civil[26][gibush_candidates_kakatz_07.2020_civil[2]==35] <- 4
gibush_candidates_kakatz_07.2020_civil[26][gibush_candidates_kakatz_07.2020_civil[2]==36] <- 4.5
gibush_candidates_kakatz_07.2020_civil[26][gibush_candidates_kakatz_07.2020_civil[2]==37] <- 4
gibush_candidates_kakatz_07.2020_civil[26][gibush_candidates_kakatz_07.2020_civil[2]==38] <- 4
gibush_candidates_kakatz_07.2020_civil[26][gibush_candidates_kakatz_07.2020_civil[2]==39] <- 5
gibush_candidates_kakatz_07.2020_civil[26][gibush_candidates_kakatz_07.2020_civil[2]==40] <- 4
gibush_candidates_kakatz_07.2020_civil[26][gibush_candidates_kakatz_07.2020_civil[2]==41] <- 5
gibush_candidates_kakatz_07.2020_civil[26][gibush_candidates_kakatz_07.2020_civil[2]==42] <- 3.5
gibush_candidates_kakatz_07.2020_civil[26][gibush_candidates_kakatz_07.2020_civil[2]==43] <- 4.5
gibush_candidates_kakatz_07.2020_civil[26][gibush_candidates_kakatz_07.2020_civil[2]==44] <- 4

# command_exercise
# updated in candidate 9 from 3.45 to 3.75
gibush_candidates_kakatz_07.2020_civil[27][gibush_candidates_kakatz_07.2020_civil[2]==1] <- 2
gibush_candidates_kakatz_07.2020_civil[27][gibush_candidates_kakatz_07.2020_civil[2]==2] <- 3
gibush_candidates_kakatz_07.2020_civil[27][gibush_candidates_kakatz_07.2020_civil[2]==3] <- 3.5
gibush_candidates_kakatz_07.2020_civil[27][gibush_candidates_kakatz_07.2020_civil[2]==4] <- 5
gibush_candidates_kakatz_07.2020_civil[27][gibush_candidates_kakatz_07.2020_civil[2]==5] <- 4
gibush_candidates_kakatz_07.2020_civil[27][gibush_candidates_kakatz_07.2020_civil[2]==6] <- 4.5
gibush_candidates_kakatz_07.2020_civil[27][gibush_candidates_kakatz_07.2020_civil[2]==7] <- 3.75
gibush_candidates_kakatz_07.2020_civil[27][gibush_candidates_kakatz_07.2020_civil[2]==8] <- 5.25
gibush_candidates_kakatz_07.2020_civil[27][gibush_candidates_kakatz_07.2020_civil[2]==9] <- 3.75
gibush_candidates_kakatz_07.2020_civil[27][gibush_candidates_kakatz_07.2020_civil[2]==10] <- 2.25
gibush_candidates_kakatz_07.2020_civil[27][gibush_candidates_kakatz_07.2020_civil[2]==11] <- 2.5
gibush_candidates_kakatz_07.2020_civil[27][gibush_candidates_kakatz_07.2020_civil[2]==12] <- 5.75
gibush_candidates_kakatz_07.2020_civil[27][gibush_candidates_kakatz_07.2020_civil[2]==13] <- 3.5
gibush_candidates_kakatz_07.2020_civil[27][gibush_candidates_kakatz_07.2020_civil[2]==14] <- 3.25
gibush_candidates_kakatz_07.2020_civil[27][gibush_candidates_kakatz_07.2020_civil[2]==15] <- 4
gibush_candidates_kakatz_07.2020_civil[27][gibush_candidates_kakatz_07.2020_civil[2]==16] <- 5.25
gibush_candidates_kakatz_07.2020_civil[27][gibush_candidates_kakatz_07.2020_civil[2]==17] <- 5.5
gibush_candidates_kakatz_07.2020_civil[27][gibush_candidates_kakatz_07.2020_civil[2]==18] <- 3.5
gibush_candidates_kakatz_07.2020_civil[27][gibush_candidates_kakatz_07.2020_civil[2]==19] <- 3.75
gibush_candidates_kakatz_07.2020_civil[27][gibush_candidates_kakatz_07.2020_civil[2]==20] <- 4.25
gibush_candidates_kakatz_07.2020_civil[27][gibush_candidates_kakatz_07.2020_civil[2]==21] <- 4.75
gibush_candidates_kakatz_07.2020_civil[27][gibush_candidates_kakatz_07.2020_civil[2]==22] <- 2.5
gibush_candidates_kakatz_07.2020_civil[27][gibush_candidates_kakatz_07.2020_civil[2]==23] <- 5
gibush_candidates_kakatz_07.2020_civil[27][gibush_candidates_kakatz_07.2020_civil[2]==24] <- 2.75
gibush_candidates_kakatz_07.2020_civil[27][gibush_candidates_kakatz_07.2020_civil[2]==25] <- 4.75
gibush_candidates_kakatz_07.2020_civil[27][gibush_candidates_kakatz_07.2020_civil[2]==26] <- 5
gibush_candidates_kakatz_07.2020_civil[27][gibush_candidates_kakatz_07.2020_civil[2]==27] <- 5.25
gibush_candidates_kakatz_07.2020_civil[27][gibush_candidates_kakatz_07.2020_civil[2]==28] <- 4.25
gibush_candidates_kakatz_07.2020_civil[27][gibush_candidates_kakatz_07.2020_civil[2]==29] <- 3.5
gibush_candidates_kakatz_07.2020_civil[27][gibush_candidates_kakatz_07.2020_civil[2]==30] <- 5.25
gibush_candidates_kakatz_07.2020_civil[27][gibush_candidates_kakatz_07.2020_civil[2]==31] <- 4
gibush_candidates_kakatz_07.2020_civil[27][gibush_candidates_kakatz_07.2020_civil[2]==32] <- 4.5
gibush_candidates_kakatz_07.2020_civil[27][gibush_candidates_kakatz_07.2020_civil[2]==33] <- 5.5
gibush_candidates_kakatz_07.2020_civil[27][gibush_candidates_kakatz_07.2020_civil[2]==34] <- 2.5
gibush_candidates_kakatz_07.2020_civil[27][gibush_candidates_kakatz_07.2020_civil[2]==35] <- 3.75
gibush_candidates_kakatz_07.2020_civil[27][gibush_candidates_kakatz_07.2020_civil[2]==36] <- 3.25
gibush_candidates_kakatz_07.2020_civil[27][gibush_candidates_kakatz_07.2020_civil[2]==37] <- 5.25
gibush_candidates_kakatz_07.2020_civil[27][gibush_candidates_kakatz_07.2020_civil[2]==38] <- 4
gibush_candidates_kakatz_07.2020_civil[27][gibush_candidates_kakatz_07.2020_civil[2]==39] <- 4.25
gibush_candidates_kakatz_07.2020_civil[27][gibush_candidates_kakatz_07.2020_civil[2]==40] <- 4.24
gibush_candidates_kakatz_07.2020_civil[27][gibush_candidates_kakatz_07.2020_civil[2]==41] <- 5.5
gibush_candidates_kakatz_07.2020_civil[27][gibush_candidates_kakatz_07.2020_civil[2]==42] <- 5.25
gibush_candidates_kakatz_07.2020_civil[27][gibush_candidates_kakatz_07.2020_civil[2]==43] <- 2.5
gibush_candidates_kakatz_07.2020_civil[27][gibush_candidates_kakatz_07.2020_civil[2]==44] <- 3.75


# interview
gibush_candidates_kakatz_07.2020_civil[28][gibush_candidates_kakatz_07.2020_civil[2]==1] <- 3.5
gibush_candidates_kakatz_07.2020_civil[28][gibush_candidates_kakatz_07.2020_civil[2]==2] <- 3.5
gibush_candidates_kakatz_07.2020_civil[28][gibush_candidates_kakatz_07.2020_civil[2]==3] <- 4
gibush_candidates_kakatz_07.2020_civil[28][gibush_candidates_kakatz_07.2020_civil[2]==4] <- 3.5
gibush_candidates_kakatz_07.2020_civil[28][gibush_candidates_kakatz_07.2020_civil[2]==5] <- 3.5
gibush_candidates_kakatz_07.2020_civil[28][gibush_candidates_kakatz_07.2020_civil[2]==6] <- 5
gibush_candidates_kakatz_07.2020_civil[28][gibush_candidates_kakatz_07.2020_civil[2]==7] <- 4
gibush_candidates_kakatz_07.2020_civil[28][gibush_candidates_kakatz_07.2020_civil[2]==8] <- 4
gibush_candidates_kakatz_07.2020_civil[28][gibush_candidates_kakatz_07.2020_civil[2]==9] <- 4.5
gibush_candidates_kakatz_07.2020_civil[28][gibush_candidates_kakatz_07.2020_civil[2]==10] <- 4
gibush_candidates_kakatz_07.2020_civil[28][gibush_candidates_kakatz_07.2020_civil[2]==11] <- 4.5
gibush_candidates_kakatz_07.2020_civil[28][gibush_candidates_kakatz_07.2020_civil[2]==12] <- 4.5
gibush_candidates_kakatz_07.2020_civil[28][gibush_candidates_kakatz_07.2020_civil[2]==13] <- 3.5
gibush_candidates_kakatz_07.2020_civil[28][gibush_candidates_kakatz_07.2020_civil[2]==14] <- 3.5
gibush_candidates_kakatz_07.2020_civil[28][gibush_candidates_kakatz_07.2020_civil[2]==15] <- 5.5
gibush_candidates_kakatz_07.2020_civil[28][gibush_candidates_kakatz_07.2020_civil[2]==16] <- 3
gibush_candidates_kakatz_07.2020_civil[28][gibush_candidates_kakatz_07.2020_civil[2]==17] <- 5
gibush_candidates_kakatz_07.2020_civil[28][gibush_candidates_kakatz_07.2020_civil[2]==18] <- 3
gibush_candidates_kakatz_07.2020_civil[28][gibush_candidates_kakatz_07.2020_civil[2]==19] <- 5
gibush_candidates_kakatz_07.2020_civil[28][gibush_candidates_kakatz_07.2020_civil[2]==20] <- 3
gibush_candidates_kakatz_07.2020_civil[28][gibush_candidates_kakatz_07.2020_civil[2]==21] <- 4.5
gibush_candidates_kakatz_07.2020_civil[28][gibush_candidates_kakatz_07.2020_civil[2]==22] <- 3.5
gibush_candidates_kakatz_07.2020_civil[28][gibush_candidates_kakatz_07.2020_civil[2]==23] <- 4.5
gibush_candidates_kakatz_07.2020_civil[28][gibush_candidates_kakatz_07.2020_civil[2]==24] <- 3.5
gibush_candidates_kakatz_07.2020_civil[28][gibush_candidates_kakatz_07.2020_civil[2]==25] <- 3
gibush_candidates_kakatz_07.2020_civil[28][gibush_candidates_kakatz_07.2020_civil[2]==26] <- 5
gibush_candidates_kakatz_07.2020_civil[28][gibush_candidates_kakatz_07.2020_civil[2]==27] <- 4.5
gibush_candidates_kakatz_07.2020_civil[28][gibush_candidates_kakatz_07.2020_civil[2]==28] <- 2.5
gibush_candidates_kakatz_07.2020_civil[28][gibush_candidates_kakatz_07.2020_civil[2]==29] <- 3.5
gibush_candidates_kakatz_07.2020_civil[28][gibush_candidates_kakatz_07.2020_civil[2]==30] <- 4.5
gibush_candidates_kakatz_07.2020_civil[28][gibush_candidates_kakatz_07.2020_civil[2]==31] <- 4.5
gibush_candidates_kakatz_07.2020_civil[28][gibush_candidates_kakatz_07.2020_civil[2]==32] <- 3.5
gibush_candidates_kakatz_07.2020_civil[28][gibush_candidates_kakatz_07.2020_civil[2]==33] <- 3.5
gibush_candidates_kakatz_07.2020_civil[28][gibush_candidates_kakatz_07.2020_civil[2]==34] <- 3
gibush_candidates_kakatz_07.2020_civil[28][gibush_candidates_kakatz_07.2020_civil[2]==35] <- 3.5
gibush_candidates_kakatz_07.2020_civil[28][gibush_candidates_kakatz_07.2020_civil[2]==36] <- 4.5
gibush_candidates_kakatz_07.2020_civil[28][gibush_candidates_kakatz_07.2020_civil[2]==37] <- 4
gibush_candidates_kakatz_07.2020_civil[28][gibush_candidates_kakatz_07.2020_civil[2]==38] <- 3
gibush_candidates_kakatz_07.2020_civil[28][gibush_candidates_kakatz_07.2020_civil[2]==39] <- 5.5
gibush_candidates_kakatz_07.2020_civil[28][gibush_candidates_kakatz_07.2020_civil[2]==40] <- 4.5
gibush_candidates_kakatz_07.2020_civil[28][gibush_candidates_kakatz_07.2020_civil[2]==41] <- 5
gibush_candidates_kakatz_07.2020_civil[28][gibush_candidates_kakatz_07.2020_civil[2]==42] <- 4
gibush_candidates_kakatz_07.2020_civil[28][gibush_candidates_kakatz_07.2020_civil[2]==43] <- 5
gibush_candidates_kakatz_07.2020_civil[28][gibush_candidates_kakatz_07.2020_civil[2]==44] <- 3.5

gibush_candidates_kakatz_07.2020_civil <- gibush_candidates_kakatz_07.2020_civil %>% 
  mutate(alternative_weighted_score = round(0.05*apptitudes + .10*personality + .40*command_exercise + .45*interview,2))

gibush_candidates_kakatz_07.2020_civil$gap <- gibush_candidates_kakatz_07.2020_civil$alternative_weighted_score-
  gibush_candidates_kakatz_07.2020_civil$predicted_FileGrade

gibush_candidates_kakatz_07.2020_civil$gap_abs <- abs(gibush_candidates_kakatz_07.2020_civil$alternative_weighted_score-
                                                        gibush_candidates_kakatz_07.2020_civil$predicted_FileGrade)

gibush_candidates_kakatz_07.2020_civil$gap_warning<-ifelse(round(gibush_candidates_kakatz_07.2020_civil$gap_abs,2)>.64,1,0)

gibush_candidates_kakatz_07.2020_civil_final_two_scores<-read_csv("C:/Users/Asher/Documents/MAMDA/JOMAGAV/alternative_gibush/gibush_candidates_kakatz_07.2020_civil_final_two_scores.csv",locale = locale(date_names = "he", encoding = "ISO-8859-8"))
gibush_candidates_kakatz_07.2020_civil_final_two_scores<-gibush_candidates_kakatz_07.2020_civil_final_two_scores[-1]
gibush_candidates_kakatz_07.2020_civil <-
  merge(gibush_candidates_kakatz_07.2020_civil,gibush_candidates_kakatz_07.2020_civil_final_two_scores,by=c("order_num"), all.x=T, all.y=F,sort = FALSE)

library(descr)
library(psych)
options(width = 71,max.print=30000)

round(freq(ordered(gibush_candidates_kakatz_07.2020_civil$predicted_FileGrade), plot = F,main=colnames(gibush_candidates_kakatz_07.2020_civil$predicted_FileGrade),font=2),2)
round(describe(as.numeric(unlist(gibush_candidates_kakatz_07.2020_civil$predicted_FileGrade))),2)

round(describe(as.numeric(unlist(gibush_candidates_kakatz_07.2020_civil$apptitudes))),2)
round(describe(as.numeric(unlist(gibush_candidates_kakatz_07.2020_civil$personality))),2)
round(describe(as.numeric(unlist(gibush_candidates_kakatz_07.2020_civil$command_exercise))),2)
round(describe(as.numeric(unlist(gibush_candidates_kakatz_07.2020_civil$interview))),2)

round(describe(as.numeric(unlist(gibush_candidates_kakatz_07.2020_civil$predicted_FileGrade))),2)
round(freq(ordered(gibush_candidates_kakatz_07.2020_civil$alternative_weighted_score), plot = F,main=colnames(gibush_candidates_kakatz_07.2020_civil$alternative_weighted_score),font=2),2)
round(describe(as.numeric(unlist(gibush_candidates_kakatz_07.2020_civil$alternative_weighted_score))),2)
round(freq(ordered(gibush_candidates_kakatz_07.2020_civil$altenative_weighted_score_final), plot = F,main=colnames(gibush_candidates_kakatz_07.2020_civil$altenative_weighted_score_final),font=2),2)
round(describe(as.numeric(unlist(gibush_candidates_kakatz_07.2020_civil$altenative_weighted_score_final))),2)
round(freq(ordered(gibush_candidates_kakatz_07.2020_civil$altenative_weighted_score_final_categorial), plot = F,main=colnames(gibush_candidates_kakatz_07.2020_civil$altenative_weighted_score_final_categorial),font=2),2)


round(freq(ordered(gibush_candidates_kakatz_07.2020_civil$gap), plot = F,main=colnames(gibush_candidates_kakatz_07.2020_civil$gap),font=2),2)
round(freq(ordered(gibush_candidates_kakatz_07.2020_civil$gap_abs), plot = F,main=colnames(gibush_candidates_kakatz_07.2020_civil$gap_abs),font=2),2)
round(describe(as.numeric(unlist(gibush_candidates_kakatz_07.2020_civil$gap_abs))),2)

round(freq(ordered(gibush_candidates_kakatz_07.2020_civil$gap_warning), plot = F,main=colnames(gibush_candidates_kakatz_07.2020_civil$gap_warning),font=2),2)

cor.test(as.numeric(gibush_candidates_kakatz_07.2020_civil$alternative_weighted_score),as.numeric(gibush_candidates_kakatz_07.2020_civil$predicted_FileGrade),use="pairwise.complete.obs",na.action = "na.exclude")
cor.test(as.numeric(gibush_candidates_kakatz_07.2020_civil$altenative_weighted_score_final),as.numeric(gibush_candidates_kakatz_07.2020_civil$predicted_FileGrade),use="pairwise.complete.obs",na.action = "na.exclude")

gibush_candidates_kakatz_07.2020_civil$gap_direction<-ifelse(gibush_candidates_kakatz_07.2020_civil$gap<0,1,2)

gibush_candidates_kakatz_07.2020_civil_negative=gibush_candidates_kakatz_07.2020_civil %>%
  filter(gap_direction==1)
gibush_candidates_kakatz_07.2020_civil_positive=gibush_candidates_kakatz_07.2020_civil %>%
  filter(gap_direction==2)

round(describe(as.numeric(unlist(gibush_candidates_kakatz_07.2020_civil_negative$gap))),2)
round(describe(as.numeric(unlist(gibush_candidates_kakatz_07.2020_civil_positive$gap))),2)

# library(formattable)

# formattable(gibush_candidates_kakatz_07.2020_civil, list(
#   warning = formatter("span", style = x ~ ifelse(x == 1, style(color = "red", font.weight = "bold"), NA))))
# 
# col.rownames="Darkblue"
# col.data=ifelse(gibush_candidates_kakatz_07.2020_civil$warning==1,'red','black')

write.csv(gibush_candidates_kakatz_07.2020_civil, file="C:/Users/Asher/Documents/MAMDA/JOMAGAV/alternative_gibush/gibush_candidates_kakatz_07.2020_civil_final_scores_new.csv")



#predict FileGrade of new candidates 11.2020 $$$$$$$$$$$$$$$$$$$$$$$$$$$$$

library(readr)
locale("he")
gibush_candidates_kakatz_11.2020_civil<-read_csv("C:/Users/Asher/Documents/MAMDA/JOMAGAV/alternative_gibush/gibush_candidates_kakatz_11.2020_civil.csv",locale = locale(date_names = "he", encoding = "ISO-8859-8"))
gibush_candidates_kakatz_11.2020_civil<-gibush_candidates_kakatz_11.2020_civil[-1]
colnames(gibush_candidates_kakatz_11.2020_civil)[1]<-"order_num"
colnames(gibush_candidates_kakatz_11.2020_civil)[14]<-"kaba"
colnames(gibush_candidates_kakatz_11.2020_civil)[16]<-"apptitudes"
colnames(gibush_candidates_kakatz_11.2020_civil)[24]<-"begin_mac_course_date"
colnames(gibush_candidates_kakatz_11.2020_civil)[25]<-"end_mac_course_date"
colnames(gibush_candidates_kakatz_11.2020_civil)[26]<-"final_mac_course_score"

gibush_candidates_kakatz_11.2020_civil <-
  merge(gibush_candidates_kakatz_11.2020_civil,filtered_soc_mac_civil_am,by=c("personal_number"), all.x=T, all.y=F,sort = FALSE)
# gibush_candidates_kakatz_11.2020_civil$am_courses_soc_mac<-1
# gibush_candidates_kakatz_11.2020_civil$GroupName_courses_soc_mac<-NA
gibush_candidates_kakatz_11.2020_civil$am_courses_soc_mac<-as.numeric(gibush_candidates_kakatz_11.2020_civil$am_courses_soc_mac)

library(descr)
library(psych)
describe(as.numeric(unlist(gibush_candidates_kakatz_11.2020_civil$am_courses_soc_mac)))
freq(ordered(round(gibush_candidates_kakatz_11.2020_civil$am_courses_soc_mac,2)), plot = F,main=colnames(gibush_candidates_kakatz_11.2020_civil$am_courses_soc_mac),font=2)
freq(ordered(gibush_candidates_kakatz_11.2020_civil$GroupName_courses_soc_mac), plot = F,main=colnames(gibush_candidates_kakatz_11.2020_civil$GroupName_courses_soc_mac),font=2)

# #final_mac_course_score
# library(readr)
# locale("he")
# mac_datets_and_scores_full_civil<-read_csv("C:/Users/Asher/Documents/MAMDA/JOMAGAV/alternative_gibush/mac_datets_and_scores_full_civil.csv",locale = locale(date_names = "he", encoding = "ISO-8859-8"))
# mac_datets_and_scores_full_civil<-mac_datets_and_scores_full_civil[-1]
# 
# class(mac_datets_and_scores_full_civil$end_mac_course_date)
# head(mac_datets_and_scores_full_civil$end_mac_course_date)
# n_occur<-data.frame(table(mac_datets_and_scores_full_civil$personal_number))
# n_occur[n_occur$Freq>1,]
# library(dplyr)
# filtered_mac_datets_and_scores_full_civil=mac_datets_and_scores_full_civil %>%
#   select(personal_number,end_mac_course_date,final_mac_course_score)
# gibush_candidates_kakatz_11.2020_civil <-
#   merge(gibush_candidates_kakatz_11.2020_civil,filtered_mac_datets_and_scores_full_civil,by=c("personal_number"), all.x=T, all.y=F,sort = FALSE)

gibush_candidates_kakatz_11.2020_civil[24][gibush_candidates_kakatz_11.2020_civil[2]==12] <- NA
gibush_candidates_kakatz_11.2020_civil[25][gibush_candidates_kakatz_11.2020_civil[2]==12] <- NA
gibush_candidates_kakatz_11.2020_civil[24][gibush_candidates_kakatz_11.2020_civil[2]==29] <- NA
gibush_candidates_kakatz_11.2020_civil[25][gibush_candidates_kakatz_11.2020_civil[2]==29] <- NA

# Ask Carmela the scores of the next 2 candidates.*********
# When I'll have the real scores update accordingly************
gibush_candidates_kakatz_11.2020_civil[26][gibush_candidates_kakatz_11.2020_civil[2]==37] <- NA
gibush_candidates_kakatz_11.2020_civil[26][gibush_candidates_kakatz_11.2020_civil[2]==44] <- NA

class(gibush_candidates_kakatz_11.2020_civil$final_mac_course_score)
gibush_candidates_kakatz_11.2020_civil$final_mac_course_score <-
  as.numeric(gibush_candidates_kakatz_11.2020_civil$final_mac_course_score)

# predicted_FileGrade
gibush_candidates_kakatz_11.2020_civil$predicted_FileGrade <- round(predict(reg_FileGrade1, gibush_candidates_kakatz_11.2020_civil),2)
freq(ordered(gibush_candidates_kakatz_11.2020_civil$predicted_FileGrade), plot = F,main=colnames(gibush_candidates_kakatz_11.2020_civil$predicted_FileGrade),font=2)
round(describe(as.numeric(unlist(gibush_candidates_kakatz_11.2020_civil$predicted_FileGrade))),2)

gibush_candidates_kakatz_11.2020_civil$personality <- NA
gibush_candidates_kakatz_11.2020_civil$command_exercise <- NA
gibush_candidates_kakatz_11.2020_civil$interview <- NA

colnames(gibush_candidates_kakatz_11.2020_civil)
class(gibush_candidates_kakatz_11.2020_civil)
gibush_candidates_kakatz_11.2020_civil<-as.data.frame(gibush_candidates_kakatz_11.2020_civil)

# personality
# When I'll have the real scores update accordingly************

gibush_candidates_kakatz_11.2020_civil[30][gibush_candidates_kakatz_11.2020_civil[1]==1] <- 3
gibush_candidates_kakatz_11.2020_civil[30][gibush_candidates_kakatz_11.2020_civil[1]==2] <- 4
gibush_candidates_kakatz_11.2020_civil[30][gibush_candidates_kakatz_11.2020_civil[1]==3] <- 4
gibush_candidates_kakatz_11.2020_civil[30][gibush_candidates_kakatz_11.2020_civil[1]==4] <- 3
gibush_candidates_kakatz_11.2020_civil[30][gibush_candidates_kakatz_11.2020_civil[1]==5] <- 2.5
gibush_candidates_kakatz_11.2020_civil[30][gibush_candidates_kakatz_11.2020_civil[1]==6] <- 4.5
gibush_candidates_kakatz_11.2020_civil[30][gibush_candidates_kakatz_11.2020_civil[1]==7] <- 3
gibush_candidates_kakatz_11.2020_civil[30][gibush_candidates_kakatz_11.2020_civil[1]==8] <- 3.5
gibush_candidates_kakatz_11.2020_civil[30][gibush_candidates_kakatz_11.2020_civil[1]==9] <- 4
gibush_candidates_kakatz_11.2020_civil[30][gibush_candidates_kakatz_11.2020_civil[1]==10] <- 4.5
gibush_candidates_kakatz_11.2020_civil[30][gibush_candidates_kakatz_11.2020_civil[1]==11] <- 4
gibush_candidates_kakatz_11.2020_civil[30][gibush_candidates_kakatz_11.2020_civil[1]==12] <- 4
gibush_candidates_kakatz_11.2020_civil[30][gibush_candidates_kakatz_11.2020_civil[1]==13] <- 4
gibush_candidates_kakatz_11.2020_civil[30][gibush_candidates_kakatz_11.2020_civil[1]==14] <- 4.5
gibush_candidates_kakatz_11.2020_civil[30][gibush_candidates_kakatz_11.2020_civil[1]==15] <- 5
gibush_candidates_kakatz_11.2020_civil[30][gibush_candidates_kakatz_11.2020_civil[1]==16] <- 3
gibush_candidates_kakatz_11.2020_civil[30][gibush_candidates_kakatz_11.2020_civil[1]==17] <- 4.5
gibush_candidates_kakatz_11.2020_civil[30][gibush_candidates_kakatz_11.2020_civil[1]==18] <- 3.5
gibush_candidates_kakatz_11.2020_civil[30][gibush_candidates_kakatz_11.2020_civil[1]==19] <- 4
gibush_candidates_kakatz_11.2020_civil[30][gibush_candidates_kakatz_11.2020_civil[1]==20] <- 3
gibush_candidates_kakatz_11.2020_civil[30][gibush_candidates_kakatz_11.2020_civil[1]==21] <- 3.5
gibush_candidates_kakatz_11.2020_civil[30][gibush_candidates_kakatz_11.2020_civil[1]==22] <- 4.5
gibush_candidates_kakatz_11.2020_civil[30][gibush_candidates_kakatz_11.2020_civil[1]==23] <- 4.5
gibush_candidates_kakatz_11.2020_civil[30][gibush_candidates_kakatz_11.2020_civil[1]==24] <- 3
gibush_candidates_kakatz_11.2020_civil[30][gibush_candidates_kakatz_11.2020_civil[1]==25] <- 3.5
gibush_candidates_kakatz_11.2020_civil[30][gibush_candidates_kakatz_11.2020_civil[1]==26] <- 3
gibush_candidates_kakatz_11.2020_civil[30][gibush_candidates_kakatz_11.2020_civil[1]==27] <- 5
gibush_candidates_kakatz_11.2020_civil[30][gibush_candidates_kakatz_11.2020_civil[1]==28] <- 2.5
gibush_candidates_kakatz_11.2020_civil[30][gibush_candidates_kakatz_11.2020_civil[1]==29] <- 3.5
gibush_candidates_kakatz_11.2020_civil[30][gibush_candidates_kakatz_11.2020_civil[1]==30] <- 3
gibush_candidates_kakatz_11.2020_civil[30][gibush_candidates_kakatz_11.2020_civil[1]==31] <- 5
gibush_candidates_kakatz_11.2020_civil[30][gibush_candidates_kakatz_11.2020_civil[1]==32] <- 3
gibush_candidates_kakatz_11.2020_civil[30][gibush_candidates_kakatz_11.2020_civil[1]==33] <- 3
gibush_candidates_kakatz_11.2020_civil[30][gibush_candidates_kakatz_11.2020_civil[1]==34] <- 2.5
gibush_candidates_kakatz_11.2020_civil[30][gibush_candidates_kakatz_11.2020_civil[1]==35] <- 4
gibush_candidates_kakatz_11.2020_civil[30][gibush_candidates_kakatz_11.2020_civil[1]==36] <- 4.5
gibush_candidates_kakatz_11.2020_civil[30][gibush_candidates_kakatz_11.2020_civil[1]==37] <- 4
gibush_candidates_kakatz_11.2020_civil[30][gibush_candidates_kakatz_11.2020_civil[1]==38] <- 4
gibush_candidates_kakatz_11.2020_civil[30][gibush_candidates_kakatz_11.2020_civil[1]==39] <- 5
gibush_candidates_kakatz_11.2020_civil[30][gibush_candidates_kakatz_11.2020_civil[1]==40] <- 4
gibush_candidates_kakatz_11.2020_civil[30][gibush_candidates_kakatz_11.2020_civil[1]==41] <- 5
gibush_candidates_kakatz_11.2020_civil[30][gibush_candidates_kakatz_11.2020_civil[1]==42] <- 3.5
gibush_candidates_kakatz_11.2020_civil[30][gibush_candidates_kakatz_11.2020_civil[1]==43] <- 4.5
gibush_candidates_kakatz_11.2020_civil[30][gibush_candidates_kakatz_11.2020_civil[1]==44] <- 4

# command_exercise
# When I'll have the real scores update accordingly************

gibush_candidates_kakatz_11.2020_civil[31][gibush_candidates_kakatz_11.2020_civil[1]==1] <- 2
gibush_candidates_kakatz_11.2020_civil[31][gibush_candidates_kakatz_11.2020_civil[1]==2] <- 3
gibush_candidates_kakatz_11.2020_civil[31][gibush_candidates_kakatz_11.2020_civil[1]==3] <- 3.5
gibush_candidates_kakatz_11.2020_civil[31][gibush_candidates_kakatz_11.2020_civil[1]==4] <- 5
gibush_candidates_kakatz_11.2020_civil[31][gibush_candidates_kakatz_11.2020_civil[1]==5] <- 4
gibush_candidates_kakatz_11.2020_civil[31][gibush_candidates_kakatz_11.2020_civil[1]==6] <- 4.5
gibush_candidates_kakatz_11.2020_civil[31][gibush_candidates_kakatz_11.2020_civil[1]==7] <- 3.75
gibush_candidates_kakatz_11.2020_civil[31][gibush_candidates_kakatz_11.2020_civil[1]==8] <- 5.25
gibush_candidates_kakatz_11.2020_civil[31][gibush_candidates_kakatz_11.2020_civil[1]==9] <- 3.75
gibush_candidates_kakatz_11.2020_civil[31][gibush_candidates_kakatz_11.2020_civil[1]==10] <- 2.25
gibush_candidates_kakatz_11.2020_civil[31][gibush_candidates_kakatz_11.2020_civil[1]==11] <- 2.5
gibush_candidates_kakatz_11.2020_civil[31][gibush_candidates_kakatz_11.2020_civil[1]==12] <- 5.75
gibush_candidates_kakatz_11.2020_civil[31][gibush_candidates_kakatz_11.2020_civil[1]==13] <- 3.5
gibush_candidates_kakatz_11.2020_civil[31][gibush_candidates_kakatz_11.2020_civil[1]==14] <- 3.25
gibush_candidates_kakatz_11.2020_civil[31][gibush_candidates_kakatz_11.2020_civil[1]==15] <- 4
gibush_candidates_kakatz_11.2020_civil[31][gibush_candidates_kakatz_11.2020_civil[1]==16] <- 5.25
gibush_candidates_kakatz_11.2020_civil[31][gibush_candidates_kakatz_11.2020_civil[1]==17] <- 5.5
gibush_candidates_kakatz_11.2020_civil[31][gibush_candidates_kakatz_11.2020_civil[1]==18] <- 3.5
gibush_candidates_kakatz_11.2020_civil[31][gibush_candidates_kakatz_11.2020_civil[1]==19] <- 3.75
gibush_candidates_kakatz_11.2020_civil[31][gibush_candidates_kakatz_11.2020_civil[1]==20] <- 4.25
gibush_candidates_kakatz_11.2020_civil[31][gibush_candidates_kakatz_11.2020_civil[1]==21] <- 4.75
gibush_candidates_kakatz_11.2020_civil[31][gibush_candidates_kakatz_11.2020_civil[1]==22] <- 2.5
gibush_candidates_kakatz_11.2020_civil[31][gibush_candidates_kakatz_11.2020_civil[1]==23] <- 5
gibush_candidates_kakatz_11.2020_civil[31][gibush_candidates_kakatz_11.2020_civil[1]==24] <- 2.75
gibush_candidates_kakatz_11.2020_civil[31][gibush_candidates_kakatz_11.2020_civil[1]==25] <- 4.75
gibush_candidates_kakatz_11.2020_civil[31][gibush_candidates_kakatz_11.2020_civil[1]==26] <- 5
gibush_candidates_kakatz_11.2020_civil[31][gibush_candidates_kakatz_11.2020_civil[1]==27] <- 5.25
gibush_candidates_kakatz_11.2020_civil[31][gibush_candidates_kakatz_11.2020_civil[1]==28] <- 4.25
gibush_candidates_kakatz_11.2020_civil[31][gibush_candidates_kakatz_11.2020_civil[1]==29] <- 3.5
gibush_candidates_kakatz_11.2020_civil[31][gibush_candidates_kakatz_11.2020_civil[1]==30] <- 5.25
gibush_candidates_kakatz_11.2020_civil[31][gibush_candidates_kakatz_11.2020_civil[1]==31] <- 4
gibush_candidates_kakatz_11.2020_civil[31][gibush_candidates_kakatz_11.2020_civil[1]==32] <- 4.5
gibush_candidates_kakatz_11.2020_civil[31][gibush_candidates_kakatz_11.2020_civil[1]==33] <- 5.5
gibush_candidates_kakatz_11.2020_civil[31][gibush_candidates_kakatz_11.2020_civil[1]==34] <- 2.5
gibush_candidates_kakatz_11.2020_civil[31][gibush_candidates_kakatz_11.2020_civil[1]==35] <- 3.75
gibush_candidates_kakatz_11.2020_civil[31][gibush_candidates_kakatz_11.2020_civil[1]==36] <- 3.25
gibush_candidates_kakatz_11.2020_civil[31][gibush_candidates_kakatz_11.2020_civil[1]==37] <- 5.25
gibush_candidates_kakatz_11.2020_civil[31][gibush_candidates_kakatz_11.2020_civil[1]==38] <- 4
gibush_candidates_kakatz_11.2020_civil[31][gibush_candidates_kakatz_11.2020_civil[1]==39] <- 4.25
gibush_candidates_kakatz_11.2020_civil[31][gibush_candidates_kakatz_11.2020_civil[1]==40] <- 4.24
gibush_candidates_kakatz_11.2020_civil[31][gibush_candidates_kakatz_11.2020_civil[1]==41] <- 5.5
gibush_candidates_kakatz_11.2020_civil[31][gibush_candidates_kakatz_11.2020_civil[1]==42] <- 5.25
gibush_candidates_kakatz_11.2020_civil[31][gibush_candidates_kakatz_11.2020_civil[1]==43] <- 2.5
gibush_candidates_kakatz_11.2020_civil[31][gibush_candidates_kakatz_11.2020_civil[1]==44] <- 3.75


# interview
# When I'll have the real scores update accordingly************

gibush_candidates_kakatz_11.2020_civil[32][gibush_candidates_kakatz_11.2020_civil[1]==1] <- 3.5
gibush_candidates_kakatz_11.2020_civil[32][gibush_candidates_kakatz_11.2020_civil[1]==2] <- 3.5
gibush_candidates_kakatz_11.2020_civil[32][gibush_candidates_kakatz_11.2020_civil[1]==3] <- 4
gibush_candidates_kakatz_11.2020_civil[32][gibush_candidates_kakatz_11.2020_civil[1]==4] <- 3.5
gibush_candidates_kakatz_11.2020_civil[32][gibush_candidates_kakatz_11.2020_civil[1]==5] <- 3.5
gibush_candidates_kakatz_11.2020_civil[32][gibush_candidates_kakatz_11.2020_civil[1]==6] <- 5
gibush_candidates_kakatz_11.2020_civil[32][gibush_candidates_kakatz_11.2020_civil[1]==7] <- 4
gibush_candidates_kakatz_11.2020_civil[32][gibush_candidates_kakatz_11.2020_civil[1]==8] <- 4
gibush_candidates_kakatz_11.2020_civil[32][gibush_candidates_kakatz_11.2020_civil[1]==9] <- 4.5
gibush_candidates_kakatz_11.2020_civil[32][gibush_candidates_kakatz_11.2020_civil[1]==10] <- 4
gibush_candidates_kakatz_11.2020_civil[32][gibush_candidates_kakatz_11.2020_civil[1]==11] <- 4.5
gibush_candidates_kakatz_11.2020_civil[32][gibush_candidates_kakatz_11.2020_civil[1]==12] <- 4.5
gibush_candidates_kakatz_11.2020_civil[32][gibush_candidates_kakatz_11.2020_civil[1]==13] <- 3.5
gibush_candidates_kakatz_11.2020_civil[32][gibush_candidates_kakatz_11.2020_civil[1]==14] <- 3.5
gibush_candidates_kakatz_11.2020_civil[32][gibush_candidates_kakatz_11.2020_civil[1]==15] <- 5.5
gibush_candidates_kakatz_11.2020_civil[32][gibush_candidates_kakatz_11.2020_civil[1]==16] <- 3
gibush_candidates_kakatz_11.2020_civil[32][gibush_candidates_kakatz_11.2020_civil[1]==17] <- 5
gibush_candidates_kakatz_11.2020_civil[32][gibush_candidates_kakatz_11.2020_civil[1]==18] <- 3
gibush_candidates_kakatz_11.2020_civil[32][gibush_candidates_kakatz_11.2020_civil[1]==19] <- 5
gibush_candidates_kakatz_11.2020_civil[32][gibush_candidates_kakatz_11.2020_civil[1]==20] <- 3
gibush_candidates_kakatz_11.2020_civil[32][gibush_candidates_kakatz_11.2020_civil[1]==21] <- 4.5
gibush_candidates_kakatz_11.2020_civil[32][gibush_candidates_kakatz_11.2020_civil[1]==22] <- 3.5
gibush_candidates_kakatz_11.2020_civil[32][gibush_candidates_kakatz_11.2020_civil[1]==23] <- 4.5
gibush_candidates_kakatz_11.2020_civil[32][gibush_candidates_kakatz_11.2020_civil[1]==24] <- 3.5
gibush_candidates_kakatz_11.2020_civil[32][gibush_candidates_kakatz_11.2020_civil[1]==25] <- 3
gibush_candidates_kakatz_11.2020_civil[32][gibush_candidates_kakatz_11.2020_civil[1]==26] <- 5
gibush_candidates_kakatz_11.2020_civil[32][gibush_candidates_kakatz_11.2020_civil[1]==27] <- 4.5
gibush_candidates_kakatz_11.2020_civil[32][gibush_candidates_kakatz_11.2020_civil[1]==28] <- 2.5
gibush_candidates_kakatz_11.2020_civil[32][gibush_candidates_kakatz_11.2020_civil[1]==29] <- 3.5
gibush_candidates_kakatz_11.2020_civil[32][gibush_candidates_kakatz_11.2020_civil[1]==30] <- 4.5
gibush_candidates_kakatz_11.2020_civil[32][gibush_candidates_kakatz_11.2020_civil[1]==31] <- 4.5
gibush_candidates_kakatz_11.2020_civil[32][gibush_candidates_kakatz_11.2020_civil[1]==32] <- 3.5
gibush_candidates_kakatz_11.2020_civil[32][gibush_candidates_kakatz_11.2020_civil[1]==33] <- 3.5
gibush_candidates_kakatz_11.2020_civil[32][gibush_candidates_kakatz_11.2020_civil[1]==34] <- 3
gibush_candidates_kakatz_11.2020_civil[32][gibush_candidates_kakatz_11.2020_civil[1]==35] <- 3.5
gibush_candidates_kakatz_11.2020_civil[32][gibush_candidates_kakatz_11.2020_civil[1]==36] <- 4.5
gibush_candidates_kakatz_11.2020_civil[32][gibush_candidates_kakatz_11.2020_civil[1]==37] <- 4
gibush_candidates_kakatz_11.2020_civil[32][gibush_candidates_kakatz_11.2020_civil[1]==38] <- 3
gibush_candidates_kakatz_11.2020_civil[32][gibush_candidates_kakatz_11.2020_civil[1]==39] <- 5.5
gibush_candidates_kakatz_11.2020_civil[32][gibush_candidates_kakatz_11.2020_civil[1]==40] <- 4.5
gibush_candidates_kakatz_11.2020_civil[32][gibush_candidates_kakatz_11.2020_civil[1]==41] <- 5
gibush_candidates_kakatz_11.2020_civil[32][gibush_candidates_kakatz_11.2020_civil[1]==42] <- 4
gibush_candidates_kakatz_11.2020_civil[32][gibush_candidates_kakatz_11.2020_civil[1]==43] <- 5
gibush_candidates_kakatz_11.2020_civil[32][gibush_candidates_kakatz_11.2020_civil[1]==44] <- 3.5

gibush_candidates_kakatz_11.2020_civil <- gibush_candidates_kakatz_11.2020_civil %>% 
  mutate(alternative_weighted_score = round(0.05*apptitudes + .10*personality + .40*command_exercise + .45*interview,2))

gibush_candidates_kakatz_11.2020_civil$gap <- gibush_candidates_kakatz_11.2020_civil$alternative_weighted_score-
  gibush_candidates_kakatz_11.2020_civil$predicted_FileGrade

gibush_candidates_kakatz_11.2020_civil$gap_abs <- abs(gibush_candidates_kakatz_11.2020_civil$alternative_weighted_score-
                                                        gibush_candidates_kakatz_11.2020_civil$predicted_FileGrade)

gibush_candidates_kakatz_11.2020_civil$gap_warning<-ifelse(round(gibush_candidates_kakatz_11.2020_civil$gap_abs,2)>.64,1,0)

# gibush_candidates_kakatz_11.2020_civil_final_two_scores<-read_csv("C:/Users/Asher/Documents/MAMDA/JOMAGAV/alternative_gibush/gibush_candidates_kakatz_11.2020_civil_final_two_scores.csv",locale = locale(date_names = "he", encoding = "ISO-8859-8"))
# gibush_candidates_kakatz_11.2020_civil_final_two_scores<-gibush_candidates_kakatz_11.2020_civil_final_two_scores[-1]
# gibush_candidates_kakatz_11.2020_civil <-
#   merge(gibush_candidates_kakatz_11.2020_civil,gibush_candidates_kakatz_11.2020_civil_final_two_scores,by=c("order_num"), all.x=T, all.y=F,sort = FALSE)

# When I'll have the real scores update accordingly************

gibush_candidates_kakatz_11.2020_civil$altenative_weighted_score_final<-NA
gibush_candidates_kakatz_11.2020_civil$altenative_weighted_score_final_categorial<-NA

library(descr)
library(psych)
options(width = 71,max.print=30000)

round(freq(ordered(gibush_candidates_kakatz_11.2020_civil$predicted_FileGrade), plot = F,main=colnames(gibush_candidates_kakatz_11.2020_civil$predicted_FileGrade),font=2),2)
round(describe(as.numeric(unlist(gibush_candidates_kakatz_11.2020_civil$predicted_FileGrade))),2)

round(describe(as.numeric(unlist(gibush_candidates_kakatz_11.2020_civil$apptitudes))),2)
round(describe(as.numeric(unlist(gibush_candidates_kakatz_11.2020_civil$personality))),2)
round(describe(as.numeric(unlist(gibush_candidates_kakatz_11.2020_civil$command_exercise))),2)
round(describe(as.numeric(unlist(gibush_candidates_kakatz_11.2020_civil$interview))),2)

round(describe(as.numeric(unlist(gibush_candidates_kakatz_11.2020_civil$predicted_FileGrade))),2)
round(freq(ordered(gibush_candidates_kakatz_11.2020_civil$alternative_weighted_score), plot = F,main=colnames(gibush_candidates_kakatz_11.2020_civil$alternative_weighted_score),font=2),2)
round(describe(as.numeric(unlist(gibush_candidates_kakatz_11.2020_civil$alternative_weighted_score))),2)
round(freq(ordered(gibush_candidates_kakatz_11.2020_civil$altenative_weighted_score_final), plot = F,main=colnames(gibush_candidates_kakatz_11.2020_civil$altenative_weighted_score_final),font=2),2)
round(describe(as.numeric(unlist(gibush_candidates_kakatz_11.2020_civil$altenative_weighted_score_final))),2)
round(freq(ordered(gibush_candidates_kakatz_11.2020_civil$altenative_weighted_score_final_categorial), plot = F,main=colnames(gibush_candidates_kakatz_11.2020_civil$altenative_weighted_score_final_categorial),font=2),2)


round(freq(ordered(gibush_candidates_kakatz_11.2020_civil$gap), plot = F,main=colnames(gibush_candidates_kakatz_11.2020_civil$gap),font=2),2)
round(freq(ordered(gibush_candidates_kakatz_11.2020_civil$gap_abs), plot = F,main=colnames(gibush_candidates_kakatz_11.2020_civil$gap_abs),font=2),2)
round(describe(as.numeric(unlist(gibush_candidates_kakatz_11.2020_civil$gap_abs))),2)

round(freq(ordered(gibush_candidates_kakatz_11.2020_civil$gap_warning), plot = F,main=colnames(gibush_candidates_kakatz_11.2020_civil$gap_warning),font=2),2)

cor.test(as.numeric(gibush_candidates_kakatz_11.2020_civil$alternative_weighted_score),as.numeric(gibush_candidates_kakatz_11.2020_civil$predicted_FileGrade),use="pairwise.complete.obs",na.action = "na.exclude")
cor.test(as.numeric(gibush_candidates_kakatz_11.2020_civil$altenative_weighted_score_final),as.numeric(gibush_candidates_kakatz_11.2020_civil$predicted_FileGrade),use="pairwise.complete.obs",na.action = "na.exclude")

gibush_candidates_kakatz_11.2020_civil$gap_direction<-ifelse(gibush_candidates_kakatz_11.2020_civil$gap<0,1,2)

gibush_candidates_kakatz_11.2020_civil_negative=gibush_candidates_kakatz_11.2020_civil %>%
  filter(gap_direction==1)
gibush_candidates_kakatz_11.2020_civil_positive=gibush_candidates_kakatz_11.2020_civil %>%
  filter(gap_direction==2)

round(describe(as.numeric(unlist(gibush_candidates_kakatz_11.2020_civil_negative$gap))),2)
round(describe(as.numeric(unlist(gibush_candidates_kakatz_11.2020_civil_positive$gap))),2)

# library(formattable)

# formattable(gibush_candidates_kakatz_11.2020_civil, list(
#   warning = formatter("span", style = x ~ ifelse(x == 1, style(color = "red", font.weight = "bold"), NA))))
# 
# col.rownames="Darkblue"
# col.data=ifelse(gibush_candidates_kakatz_11.2020_civil$warning==1,'red','black')

write.csv(gibush_candidates_kakatz_11.2020_civil, file="C:/Users/Asher/Documents/MAMDA/JOMAGAV/alternative_gibush/gibush_candidates_kakatz_11.2020_civil_final_scores_new.csv")

# https://www.youtube.com/watch?v=yjRrsha0TdQ
# https://googledrive.tidyverse.org/reference/drive_auth.html
# https://cran.r-project.org/web/packages/googledrive/googledrive.pdf
#  The access can be removed, through my google account.
library(googledrive)
drive_auth()
drive_find(n_max = 10,type = "csv")

# add a loop with time to run the following commands

# use this*****************
# https://stackoverflow.com/questions/30993185/set-a-timer-in-r-to-execute-a-program

interval = 7
for (i in 1:3) {
  startTime = Sys.time()
  timestamp() #add before this line my code
  sleepTime = startTime + interval - Sys.time()
  if (sleepTime > 0)
    Sys.sleep(sleepTime)
}



drive_download("gibush_candidates_kakatz_11.2020_civil_final_scores****",path = "C:/Users/Asher/Documents/MAMDA/JOMAGAV/alternative_gibush/gibush_candidates_kakatz_11.2020_civil_final_scores_gd",type = "csv",overwrite=T)

library(readr)
locale("he")
gibush_candidates_kakatz_11.2020_civil_final_scores_gd<-read_csv("C:/Users/Asher/Documents/MAMDA/JOMAGAV/alternative_gibush/gibush_candidates_kakatz_11.2020_civil_final_scores_gd.csv")

# gibush_candidates_kakatz_11.2020_civil_final_scores_gd[5]<-gibush_candidates_kakatz_11.2020_civil[31]


# insert here a code that creates the rusults in the imported google sheet


write.csv(gibush_candidates_kakatz_11.2020_civil_final_scores_gd, file="C:/Users/Asher/Documents/MAMDA/JOMAGAV/alternative_gibush/gibush_candidates_kakatz_11.2020_civil_final_scores_gd.csv")

drive_upload("C:/Users/Asher/Documents/MAMDA/JOMAGAV/alternative_gibush/gibush_candidates_kakatz_11.2020_civil_final_scores_gd.csv",path="https://drive.google.com/drive/folders/1vkCCxKNhMo2OmDj4KdsW0n_f_ffPX7TS?usp=sharing", name="gibush_candidates_kakatz_11.2020_civil_final_scores",type = "spreadsheet",overwrite=T)



#presentation

presentation_filtered_vars<-JOMAG_predictores_criteria_merged_civil %>%
  select(personal_number,final_apptitudes_new,tkufatitam)
presentation <-
  merge(mac_datets_and_scores_civil_ranks_new_civil_kaba_civil_soc_mac_civil_am_dapar,presentation_filtered_vars,by=c("personal_number"), all.x=T, all.y=F,sort = FALSE)


cor.test(as.numeric(presentation$kaba),as.numeric(presentation$final_apptitudes_new),use="pairwise.complete.obs",na.action = "na.exclude")
cor.test(as.numeric(presentation$kaba),as.numeric(presentation$tkufatitam),use="pairwise.complete.obs",na.action = "na.exclude")
cor.test(as.numeric(presentation$am_courses_soc_mac),as.numeric(presentation$tkufatitam),use="pairwise.complete.obs",na.action = "na.exclude")
cor.test(as.numeric(presentation$final_mac_course_score),as.numeric(presentation$tkufatitam),use="pairwise.complete.obs",na.action = "na.exclude")



#shiny app

install.packages("shiny")

#***********************arrived here******************************

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



# round(describe(as.numeric(unlist(JOMAG_predictores_criteria_merged_civil$exercises))),2)
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
write.csv(JOMAG_predictores_criteria_merged_civil_qv_field, file="C:/Users/Asher/Documents/MAMDA/JOMAGAV/JOMAG_predictores_criteria_merged_civil_qv_field.csv")

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

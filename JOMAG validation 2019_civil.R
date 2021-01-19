#Preparing the predictors data.

library(sendmailR)
from <- sprintf("<sendmailR@%s>", Sys.info()[4])
to <-"<xxxxx>"
subject <- "JOMAG session has been started"
body <- list(paste(" "))
sendmail(from, to, subject, body,control=list(smtpServer="xxxxx"))

Start_time<-Sys.time()

JOMAG_predictores<-read.csv("Q:/04_Mehkar/18_asher/Junior officers MAGAV validation/JOMAG_predictores.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
colnames(JOMAG_predictores)[13]<-"personal_number"
colnames(JOMAG_predictores)[14]<-"id"
JOMAG_predictores$GibDate<-as.Date(as.character(JOMAG_predictores$GibDate),format="%d/%m/%Y")
JOMAG_predictores<-JOMAG_predictores[-81]

JOMAG_predictores$VaadaGrade[JOMAG_predictores$VaadaGrade==45]<-4.5

# Add missing personal_number

# Check candidates who doesn't have personal_number.
j=0
for (i in 1:nrow(JOMAG_predictores)){
  if(is.na(JOMAG_predictores[i,]$personal_number)){
print(JOMAG_predictores[i,]$id)
    j=j+1
}
}
j

i.d._p.n.<-read.csv("Q:/04_Mehkar/18_asher/Junior officers MAGAV validation/i.d._p.n..csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
JOMAG_predictores <- merge(JOMAG_predictores, i.d._p.n.,by=c("id"), all.x=T, all.y=F,sort = FALSE)
JOMAG_predictores <- unique(JOMAG_predictores, by="id")
JOMAG_predictores$personal_number<-ifelse(!is.na(JOMAG_predictores$personal_number.x),JOMAG_predictores$personal_number.x,JOMAG_predictores$personal_number.y)
JOMAG_predictores<-JOMAG_predictores[,c(-14,-81)]

j=0
for (i in 1:nrow(JOMAG_predictores)){
  if(is.na(JOMAG_predictores[i,]$personal_number)){
    print(JOMAG_predictores[i,]$id)
    j=j+1
  }
}
j

JOMAG_predictores$id[JOMAG_predictores$id=="x"]<-"x"
JOMAG_predictores$id[JOMAG_predictores$id=="x"]<-"x"
JOMAG_predictores$id[JOMAG_predictores$id=="x"]<-"x"
JOMAG_predictores$id[JOMAG_predictores$id=="x"]<-"x"
JOMAG_predictores$id[JOMAG_predictores$id=="x"]<-"x"
JOMAG_predictores$id[JOMAG_predictores$id=="x"]<-"x"
JOMAG_predictores$id[JOMAG_predictores$id=="x"]<-"x"
JOMAG_predictores$id[JOMAG_predictores$id=="x"]<-"x"
JOMAG_predictores$id[JOMAG_predictores$id=="x"]<-"x"
JOMAG_predictores$id[JOMAG_predictores$id=="x"]<-"x"
JOMAG_predictores$id[JOMAG_predictores$id=="x"]<-"x"
JOMAG_predictores$id[JOMAG_predictores$id=="x"]<-"x"
JOMAG_predictores$id[JOMAG_predictores$id=="x"]<-"x"
JOMAG_predictores$id[JOMAG_predictores$id=="x"]<-"x"
JOMAG_predictores$id[JOMAG_predictores$id=="x"]<-"x"
JOMAG_predictores$id[JOMAG_predictores$id=="x"]<-"x"
JOMAG_predictores$id[JOMAG_predictores$id=="x"]<-"x"
JOMAG_predictores$id[JOMAG_predictores$id=="x"]<-"x"
JOMAG_predictores$id[JOMAG_predictores$id=="x"]<-"x"
JOMAG_predictores$id[JOMAG_predictores$id=="x"]<-"x"
JOMAG_predictores$id[JOMAG_predictores$id=="x"]<-"x"
JOMAG_predictores$id[JOMAG_predictores$id=="x"]<-"x"
JOMAG_predictores$id[JOMAG_predictores$id=="x"]<-"x"
JOMAG_predictores$id[JOMAG_predictores$id=="x"]<-"x"
JOMAG_predictores$id[JOMAG_predictores$id=="x"]<-"x"
JOMAG_predictores$id[JOMAG_predictores$id=="x"]<-"x"
JOMAG_predictores$id[JOMAG_predictores$id=="x"]<-"x"
JOMAG_predictores$id[JOMAG_predictores$id=="x"]<-"x"
JOMAG_predictores$id[JOMAG_predictores$id=="x"]<-"x"
JOMAG_predictores$id[JOMAG_predictores$id=="x"]<-"x"
JOMAG_predictores$id[JOMAG_predictores$id=="x"]<-"x"
JOMAG_predictores$id[JOMAG_predictores$id=="x"]<-"x"
JOMAG_predictores$id[JOMAG_predictores$id=="x"]<-"x"
JOMAG_predictores$id[JOMAG_predictores$id=="x"]<-"x"
JOMAG_predictores$id[JOMAG_predictores$id=="x"]<-"x"
JOMAG_predictores$id[JOMAG_predictores$id=="x"]<-"x"
JOMAG_predictores$id[JOMAG_predictores$id=="x"]<-"x"
JOMAG_predictores$id[JOMAG_predictores$id=="x"]<-"x"
JOMAG_predictores$id[JOMAG_predictores$id=="x"]<-"x"
JOMAG_predictores$id[JOMAG_predictores$id=="x"]<-"x"
JOMAG_predictores$id[JOMAG_predictores$id=="x"]<-"x"
JOMAG_predictores$id[JOMAG_predictores$id=="x"]<-"x"
JOMAG_predictores$id[JOMAG_predictores$id=="x"]<-"x"
JOMAG_predictores$id[JOMAG_predictores$id=="x"]<-"x"

JOMAG_predictores$FirstName<-ifelse(JOMAG_predictores$id==x,"x",JOMAG_predictores$FirstName)
JOMAG_predictores$LastName<-ifelse(JOMAG_predictores$id==x,"x",JOMAG_predictores$LastName)
JOMAG_predictores$FirstName<-ifelse(JOMAG_predictores$id==x,"x",JOMAG_predictores$FirstName)
JOMAG_predictores$LastName<-ifelse(JOMAG_predictores$id==x,"x",JOMAG_predictores$LastName)

added_missing_personal_numbers_personal_number<-read.csv("Q:/04_Mehkar/18_asher/Junior officers MAGAV validation/added_missing_personal_numbers_personal_number.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
added_missing_personal_numbers_personal_number<-added_missing_personal_numbers_personal_number
added_missing_personal_numbers_personal_number$id<-as.numeric(added_missing_personal_numbers_personal_number$id)
JOMAG_predictores$id<-as.numeric(JOMAG_predictores$id)
JOMAG_predictores<-merge(JOMAG_predictores,added_missing_personal_numbers_personal_number,by=c("id"), all.x=T, all.y=F,sort = FALSE)
JOMAG_predictores$personal_number<-ifelse(is.na(JOMAG_predictores$personal_number),JOMAG_predictores$personal_number_new,JOMAG_predictores$personal_number)

j=0
for (i in 1:nrow(JOMAG_predictores)){
  if(is.na(JOMAG_predictores[i,]$personal_number)){
    print(JOMAG_predictores[i,]$id)
    j=j+1
  }
}
j

JOMAG_predictores$personal_number_new<-NULL

# Remove duplicate rows from JOMAG_predictores.
n_occur<-data.frame(table(JOMAG_predictores$id))
n_occur[n_occur$Freq>1,]
library (data.table)
JOMAG_predictores<-setDT(JOMAG_predictores)[,.SD[which.max(GibDate)],keyby=id]
n_occur<-data.frame(table(JOMAG_predictores$id))
n_occur[n_occur$Freq>1,]

# leave only candidates who's 30.05.2010>=GibDate<01.01.2019
JOMAG_predictores<-JOMAG_predictores[which(JOMAG_predictores$GibDate>=as.Date("2010/05/30") & JOMAG_predictores$GibDate<as.Date("2019/01/01")),]

# Treat criteria files.
period_eval._2015<-read.csv("Q:/04_Mehkar/18_asher/Junior officers MAGAV validation/Files from Ronen/period_eval._2015.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
period_eval._2017<-read.csv("Q:/04_Mehkar/18_asher/Junior officers MAGAV validation/Files from Ronen/period_eval._2017.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
tkufatit_2014_2015<-read.csv("Q:/04_Mehkar/18_asher/Junior officers MAGAV validation/Files from Ronen/tkufatit_2014_2015.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
tkufatit_2018<-read.csv("Q:/04_Mehkar/18_asher/Junior officers MAGAV validation/Files from Ronen/tkufatit_2018.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
am_2010<-read.csv("Q:/04_Mehkar/18_asher/Junior officers MAGAV validation/Files from Ronen/soc/am_10zz.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
am_2012<-read.csv("Q:/04_Mehkar/18_asher/Junior officers MAGAV validation/Files from Ronen/soc/am_12zz.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
am_2015<-read.csv("Q:/04_Mehkar/18_asher/Junior officers MAGAV validation/Files from Ronen/soc/am_15zzz.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
am_2018<-read.csv("Q:/04_Mehkar/18_asher/Junior officers MAGAV validation/Files from Lev/am_2018.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
cf_2010<-read.csv("Q:/04_Mehkar/18_asher/Junior officers MAGAV validation/Files from Ronen/soc/cf_10zz.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
cf_2012<-read.csv("Q:/04_Mehkar/18_asher/Junior officers MAGAV validation/Files from Ronen/soc/cf_12zz.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
cf_2015<-read.csv("Q:/04_Mehkar/18_asher/Junior officers MAGAV validation/Files from Ronen/soc/cf_15zzz.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
cf_2018<-read.csv("Q:/04_Mehkar/18_asher/Junior officers MAGAV validation/Files from Lev/cf_2018.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
colnames(am_2010)<-paste(colnames(am_2010),"am_2010",sep="_")
colnames(am_2012)<-paste(colnames(am_2012),"am_2012",sep="_")
colnames(am_2015)<-paste(colnames(am_2015),"am_2015",sep="_")
colnames(am_2018)<-paste(colnames(am_2018),"am_2018",sep="_")
colnames(cf_2010)<-paste(colnames(cf_2010),"cf_2010",sep="_")
colnames(cf_2012)<-paste(colnames(cf_2012),"cf_2012",sep="_")
colnames(cf_2015)<-paste(colnames(cf_2015),"cf_2015",sep="_")
colnames(cf_2018)<-paste(colnames(cf_2018),"cf_2018",sep="_")
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

colnames(am_2010)[6]<-"personal_number"
colnames(am_2012)[6]<-"personal_number"
colnames(am_2015)[1]<-"personal_number"
colnames(am_2018)[1]<-"personal_number"
colnames(cf_2010)[6]<-"personal_number"
colnames(cf_2012)[6]<-"personal_number"
colnames(cf_2015)[1]<-"personal_number"
colnames(cf_2018)[1]<-"personal_number"

period_eval._2015_2017 <- merge(period_eval._2015, period_eval._2017,by=c("personal_number"), all.x=T, all.y=T,sort = FALSE)
period_eval._2015_2017_tkufatit_2014_2015 <- merge(period_eval._2015_2017, tkufatit_2014_2015,by=c("personal_number"), all.x=T, all.y=T,sort = FALSE)
am_2010_2012 <- merge(am_2010, am_2012,by=c("personal_number"), all.x=T, all.y=T,sort = FALSE)
am_2010_2012_2015 <- merge(am_2010_2012, am_2015,by=c("personal_number"), all.x=T, all.y=T,sort = FALSE)
am_2010_2012_2015_2018 <- merge(am_2010_2012_2015, am_2018,by=c("personal_number"), all.x=T, all.y=T,sort = FALSE)
cf_2010_2012 <- merge(cf_2010, cf_2012,by=c("personal_number"), all.x=T, all.y=T,sort = FALSE)
cf_2010_2012_2015 <- merge(cf_2010_2012, cf_2015,by=c("personal_number"), all.x=T, all.y=T,sort = FALSE)
cf_2010_2012_2015_2018 <- merge(cf_2010_2012_2015, cf_2018,by=c("personal_number"), all.x=T, all.y=T,sort = FALSE)
am_cf <- merge(am_2010_2012_2015_2018, cf_2010_2012_2015_2018,by=c("personal_number"), all.x=T, all.y=T,sort = FALSE)
period_eval._2015_2017_tkufatit_2014_2015_am_cf <- merge(period_eval._2015_2017_tkufatit_2014_2015,am_cf,by=c("personal_number"), all.x=T, all.y=T,sort = FALSE)
criteria_merged<-merge(period_eval._2015_2017_tkufatit_2014_2015_am_cf,tkufatit_2018,by=c("personal_number"), all.x=T, all.y=T,sort = FALSE)

for(i in 1:ncol(criteria_merged)){
  colnames(criteria_merged)[i]<-gsub(".x","2015",colnames(criteria_merged)[i])}
for(i in 1:ncol(criteria_merged)){
  colnames(criteria_merged)[i]<-gsub(".y","2017",colnames(criteria_merged)[i])}

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
                                                      "ציון.סופי2017"="final.score.2017",
                                                      "kod2017ehidat_al_am_2015"="kodyehidat_al_am_2015","shem2017ehidat_al_am_2015"="shemyehidat_al_am_2015",
                                                      "m2017uhedet_am_2015"="myuhedet_am_2015","sd_mist2017en_am_2015"="sd_mistyen_am_2015",
                                                      "metuknan_mist2017en_am_2015"="metuknan_mistyen_am_2015","memusa_kvusa_mist2017en_am_2015"="memusa_kvusa_mistyen_am_2015",
                                                      "sd_kvusa_mist2017en_am_2015"="sd_kvusa_mistyen_am_2015"))

criteria_merged$TaarichHavara_am_2015<-as.Date(criteria_merged$TaarichHavara_am_2015,format="%d/%m/%Y")
criteria_merged$TaarichHavara_cf_2015<-as.Date(criteria_merged$TaarichHavara_cf_2015,format="%d/%m/%Y")

#Remove duplicate rows from criteria_merged.
n_occur<-data.frame(table(criteria_merged$personal_number))
n_occur[n_occur$Freq>1,]
criteria_merged <- criteria_merged[!duplicated(criteria_merged[,c("personal_number")]),]
n_occur<-data.frame(table(criteria_merged$personal_number))
n_occur[n_occur$Freq>1,]

# Merge predictors and criteria.
JOMAG_predictores$personal_number<-as.numeric(JOMAG_predictores$personal_number)
criteria_merged$personal_number<-as.numeric(criteria_merged$personal_number)
JOMAG_predictores_criteria_merged <- merge(JOMAG_predictores, criteria_merged,by=c("personal_number"), all.x=T, all.y=F,sort = FALSE)
JOMAG_predictores_criteria_merged<-as.data.frame(JOMAG_predictores_criteria_merged)

# Check duplicate rows in JOMAG_predictores_criteria_merged.
n_occur<-data.frame(table(JOMAG_predictores_criteria_merged$personal_number))
n_occur[n_occur$Freq>1,]

# Add first officer rank dates to data (those who are officers have this date).
ranks<-read.csv("Q:/04_Mehkar/18_asher/Junior officers MAGAV validation/ranks.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
ranks$darga_date<-as.Date(ranks$darga_date,format="%d/%m/%Y")

n_occur<-data.frame(table(ranks$personal_number))
n_occur[n_occur$Freq>1,]

# library (data.table)
# ranks<-setDT(ranks)[,.SD[which.min(darga_date)],keyby=personal_number]
# n_occur<-data.frame(table(ranks$personal_number))
# n_occur[n_occur$Freq>1,]

JOMAG_predictores_criteria_merged$personal_number<-as.numeric(JOMAG_predictores_criteria_merged$personal_number)
ranks$personal_number<-as.numeric(ranks$personal_number)
JOMAG_predictores_criteria_merged <- merge(JOMAG_predictores_criteria_merged, ranks,by=c("personal_number"), all.x=T, all.y=F,sort = FALSE)
n_occur<-data.frame(table(JOMAG_predictores_criteria_merged$personal_number))
n_occur[n_occur$Freq>1,]

# frequencies of darga_date.
(descr)
library(psych)
freq(ordered(JOMAG_predictores_criteria_merged$darga_date), plot = F,main=colnames(JOMAG_predictores_criteria_merged$darga_date),font=2)

# 55% of the sample are officers (N=430 out of 780).

# Clean JOMAG_predictores_criteria_merged file from erreous data.
JOMAG_predictores_criteria_merged$direct.commander.score2015[JOMAG_predictores_criteria_merged$direct.commander.score2015==0]<-NA
JOMAG_predictores_criteria_merged$apointed.commander.score2015[JOMAG_predictores_criteria_merged$apointed.commander.score2015==0]<-NA
JOMAG_predictores_criteria_merged$group.size.2015[JOMAG_predictores_criteria_merged$group.size.2015==0]<-NA
JOMAG_predictores_criteria_merged$final.score.2015[JOMAG_predictores_criteria_merged$final.score.2015==0]<-NA
JOMAG_predictores_criteria_merged$direct.commander.score2017[JOMAG_predictores_criteria_merged$direct.commander.score2017==0]<-NA
JOMAG_predictores_criteria_merged$apointed.commander.score2017[JOMAG_predictores_criteria_merged$apointed.commander.score2017==0]<-NA
JOMAG_predictores_criteria_merged$group.size.2017[JOMAG_predictores_criteria_merged$group.size.2017==0]<-NA
JOMAG_predictores_criteria_merged$final.score.2017[JOMAG_predictores_criteria_merged$final.score.2017==0]<-NA

# Locating and handling criteria that their dates are before/not eanough after the appointment date.
JOMAG_predictores_criteria_merged$darga_date<-as.Date(unlist(JOMAG_predictores_criteria_merged$darga_date),format="%d/%m/%Y")
JOMAG_predictores_criteria_merged$date.tkufatit_14<-"26/01/2015"
JOMAG_predictores_criteria_merged$date.tkufatit_14<-as.Date(unlist(JOMAG_predictores_criteria_merged$date.tkufatit_14),format="%d/%m/%Y")
JOMAG_predictores_criteria_merged$date.tkufatit_15<-"01/06/2016"
JOMAG_predictores_criteria_merged$date.tkufatit_15<-as.Date(JOMAG_predictores_criteria_merged$date.tkufatit_15,format="%d/%m/%Y")
JOMAG_predictores_criteria_merged$date.period.eval.2015<-"01/12/2015"
JOMAG_predictores_criteria_merged$date.period.eval.2015<-as.Date(JOMAG_predictores_criteria_merged$date.period.eval.2015,format="%d/%m/%Y")
JOMAG_predictores_criteria_merged$date.period.eval.2017<-"01/05/2017"
JOMAG_predictores_criteria_merged$date.period.eval.2017<-as.Date(JOMAG_predictores_criteria_merged$date.period.eval.2017,format="%d/%m/%Y")
JOMAG_predictores_criteria_merged$date.period.eval.2018<-"31/12/2018"
JOMAG_predictores_criteria_merged$date.period.eval.2018<-as.Date(JOMAG_predictores_criteria_merged$date.period.eval.2018,format="%d/%m/%Y")
JOMAG_predictores_criteria_merged$TaarichHavara_am_2010<-as.Date(unlist(JOMAG_predictores_criteria_merged$TaarichHavara_am_2010),format="%d/%m/%Y")
JOMAG_predictores_criteria_merged$TaarichHavara_am_2012<-as.Date(unlist(JOMAG_predictores_criteria_merged$TaarichHavara_am_2012),format="%d/%m/%Y")
JOMAG_predictores_criteria_merged$TaarichHavara_am_2015<-as.Date(unlist(JOMAG_predictores_criteria_merged$TaarichHavara_am_2015),format="%d/%m/%Y")
JOMAG_predictores_criteria_merged$TaarichHavara_am_2018<-as.Date(unlist(JOMAG_predictores_criteria_merged$TaarichHavara_am_2018),format="%d/%m/%Y")
JOMAG_predictores_criteria_merged$TaarichHavara_cf_2010<-as.Date(unlist(JOMAG_predictores_criteria_merged$TaarichHavara_cf_2010),format="%d/%m/%Y")
JOMAG_predictores_criteria_merged$TaarichHavara_cf_2012<-as.Date(unlist(JOMAG_predictores_criteria_merged$TaarichHavara_cf_2012),format="%d/%m/%Y")
JOMAG_predictores_criteria_merged$TaarichHavara_cf_2015<-as.Date(unlist(JOMAG_predictores_criteria_merged$TaarichHavara_cf_2015),format="%d/%m/%Y")
names(JOMAG_predictores_criteria_merged)[names(JOMAG_predictores_criteria_merged)=="tarich_cf_2018"]<-"TaarichHavara_cf_2018"


JOMAG_predictores_criteria_merged$TaarichHavara_cf_2018<-as.Date(unlist(JOMAG_predictores_criteria_merged$TaarichHavara_cf_2018),format="%d/%m/%Y")

JOMAG_predictores_criteria_merged<-as.data.frame(JOMAG_predictores_criteria_merged)

JOMAG_predictores_criteria_merged$date.tkufatit_14_diff<-JOMAG_predictores_criteria_merged$date.tkufatit_14-JOMAG_predictores_criteria_merged$darga_date
JOMAG_predictores_criteria_merged$date.tkufatit_15_diff<-JOMAG_predictores_criteria_merged$date.tkufatit_15-JOMAG_predictores_criteria_merged$darga_date
JOMAG_predictores_criteria_merged$date.period.eval.2015_diff<-JOMAG_predictores_criteria_merged$date.period.eval.2015-JOMAG_predictores_criteria_merged$darga_date
JOMAG_predictores_criteria_merged$date.period.eval.2017_diff<-JOMAG_predictores_criteria_merged$date.period.eval.2017-JOMAG_predictores_criteria_merged$darga_date
JOMAG_predictores_criteria_merged$date.period.eval.2018_diff<-JOMAG_predictores_criteria_merged$date.period.eval.2018-JOMAG_predictores_criteria_merged$darga_date
JOMAG_predictores_criteria_merged$TaarichHavara_am_2010_diff<-JOMAG_predictores_criteria_merged$TaarichHavara_am_2010-JOMAG_predictores_criteria_merged$darga_date
JOMAG_predictores_criteria_merged$TaarichHavara_am_2012_diff<-JOMAG_predictores_criteria_merged$TaarichHavara_am_2012-JOMAG_predictores_criteria_merged$darga_date
JOMAG_predictores_criteria_merged$TaarichHavara_am_2015_diff<-JOMAG_predictores_criteria_merged$TaarichHavara_am_2015-JOMAG_predictores_criteria_merged$darga_date
JOMAG_predictores_criteria_merged$TaarichHavara_am_2018_diff<-JOMAG_predictores_criteria_merged$TaarichHavara_am_2018-JOMAG_predictores_criteria_merged$darga_date
JOMAG_predictores_criteria_merged$TaarichHavara_cf_2010_diff<-JOMAG_predictores_criteria_merged$TaarichHavara_cf_2010-JOMAG_predictores_criteria_merged$darga_date
JOMAG_predictores_criteria_merged$TaarichHavara_cf_2012_diff<-JOMAG_predictores_criteria_merged$TaarichHavara_cf_2012-JOMAG_predictores_criteria_merged$darga_date
JOMAG_predictores_criteria_merged$TaarichHavara_cf_2015_diff<-JOMAG_predictores_criteria_merged$TaarichHavara_cf_2015-JOMAG_predictores_criteria_merged$darga_date
JOMAG_predictores_criteria_merged$TaarichHavara_cf_2018_diff<-JOMAG_predictores_criteria_merged$TaarichHavara_cf_2018-JOMAG_predictores_criteria_merged$darga_date

#Remove spaces.

library(stringr)
JOMAG_predictores_criteria_merged[]<-lapply(JOMAG_predictores_criteria_merged,str_trim)
is.na(JOMAG_predictores_criteria_merged)<-JOMAG_predictores_criteria_merged==''

End_time<-Sys.time()
Total_time<- round(End_time-Start_time, digits = 2)
Total_time
# Total_time=4.61 mins

library(sendmailR)
from <- sprintf("<sendmailR@%s>", Sys.info()[4])
to <- "<xxxxx>"
subject <- "JOMAG session has been completed"
body <- list(paste("\nLength of time to run the code: ", Total_time,"."))
sendmail(from, to, subject, body,control=list(smtpServer="xxxxx"))

# add aptitudes scores
ksharim<-read.csv("Q:/04_Mehkar/18_asher/Junior officers MAGAV validation/Files from Hila/ksharim1999-2018.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
ksharim<-ksharim[-2]
n_occur<-data.frame(table(ksharim$id))
n_occur[n_occur$Freq>1,]
ksharim$testdate<-as.Date(as.character(ksharim$testdate),format="%m/%d/%Y")
library (data.table)
ksharim<-setDT(ksharim)[,.SD[which.max(testdate)],keyby=id]
n_occur<-data.frame(table(ksharim$id))
n_occur[n_occur$Freq>1,]
ksharim$id<-as.numeric(ksharim$id)
JOMAG_predictores_criteria_merged <- merge(JOMAG_predictores_criteria_merged, ksharim,by=c("id"), all.x=T, all.y=F,sort = FALSE)
JOMAG_predictores_criteria_merged <- as.data.frame(JOMAG_predictores_criteria_merged)

nacol_before_zscores<-ncol(JOMAG_predictores_criteria_merged)

# ********logic QA - check results*******
# frequencies.
library(descr)
library(psych)

mode<-function(X)
{
  temp<-table (as.vector(X))
  names (temp)[temp==max(temp)]
}
options(width = 71,max.print=30000)
# # The 2 commands after the first command, are for cleaning the output file.
JOMAG_predictores_criteria_merged_freq_relevant_columns<-colnames(JOMAG_predictores_criteria_merged[c(18:22,32:64,66:68,70,74:80,843:849)])
out<-""
cat("", out, file="Q:/04_Mehkar/18_asher/Junior officers MAGAV validation/outliers.txt", sep="", append=F,fill = T)
suppressWarnings(for(i in JOMAG_predictores_criteria_merged_freq_relevant_columns) {
  newresult1<-round(freq(ordered(as.numeric(unlist(JOMAG_predictores_criteria_merged[[i]]))), plot = F,main=colnames(JOMAG_predictores_criteria_merged[i]),font=2),2)
  newresult2<-round(describe(as.numeric(unlist(JOMAG_predictores_criteria_merged[[i]]))),2)
  newresult3<-"mode="
  newresult4<-mode(JOMAG_predictores_criteria_merged[[i]])
  newresult5<- "                                                                                               "
  newresult6<- "----------------------------------------------------------------------------"
  out <- capture.output(newresult1,newresult5,newresult2,newresult3,newresult4,newresult5,newresult6)
  out[1]<-""
  cat(colnames(JOMAG_predictores_criteria_merged[i]),out, file="Q:/04_Mehkar/18_asher/Junior officers MAGAV validation/frequencies.txt", append=T,fill = T)
})

JOMAG_predictores_criteria_merged$mYachasimAfter2[JOMAG_predictores_criteria_merged$mYachasimAfter2==45]<-4.5

freq(ordered(JOMAG_predictores_criteria_merged$darga_date), plot = F,main=colnames(JOMAG_predictores_criteria_merged$darga_date),font=2)

# Create z-scores.
JOMAG_predictores_criteria_merged_zscore_relevant_columns <- colnames(JOMAG_predictores_criteria_merged 
                                                             [c(11,12,32:61,63,64,66:68,106,132,134,150,157,159,166,168,175,177,184,186,193,195,199,203,220,
                                                                227,229,236,238,245,247,254,256,263,265,269,273,291,293,297,299,303,305,309,311,315,317,354,
                                                                356,360,362,366,368,372,374,378,380,420,424,428,432,436,440,444,448,452,456,460,464,468,472,
                                                                476,480,484,488,492,496,500,504,508,512,518,520,526,528,534,536,542,544,550,552,558,560,566,
                                                                579,583,587,591,595,599,603,725,734,607,611,615,619,623,627,631,635,639,643,647,651,655,659,
                                                                663,667,671,677,679,685,687,693,695,701,703,709,711,717,719,736,738,740,742,744,746,748,750,
                                                                752,754,756,758,760,778,780,782,784,786,788,790,792,794,796,798,800,802,804,822)])

for(i in JOMAG_predictores_criteria_merged_zscore_relevant_columns) {
  JOMAG_predictores_criteria_merged[ncol(JOMAG_predictores_criteria_merged)+1]<-NA
  names(JOMAG_predictores_criteria_merged)[ncol(JOMAG_predictores_criteria_merged)]<-paste(i,"zscore",sep = "_")
  JOMAG_predictores_criteria_merged[ncol(JOMAG_predictores_criteria_merged)] <-
  as.data.frame(scale(as.numeric(unlist(JOMAG_predictores_criteria_merged[i]))))
}

JOMAG_predictores_criteria_merged_zscore_relevant_columns <- colnames(JOMAG_predictores_criteria_merged [c(844:849)])
for(i in JOMAG_predictores_criteria_merged_zscore_relevant_columns) {
  JOMAG_predictores_criteria_merged[ncol(JOMAG_predictores_criteria_merged)+1]<-NA
  names(JOMAG_predictores_criteria_merged)[ncol(JOMAG_predictores_criteria_merged)]<-paste(i,"zscore",sep = "_")
  JOMAG_predictores_criteria_merged[ncol(JOMAG_predictores_criteria_merged)]<-as.numeric(unlist(JOMAG_predictores_criteria_merged[ncol(JOMAG_predictores_criteria_merged)]))
}

zscore<-function(x,mean,sd)
{
  (x-mean)/sd
}

JOMAG_predictores_criteria_merged$zhak_zscore<-zscore(as.numeric(unlist(JOMAG_predictores_criteria_merged$zhak)),14.49,5.37)
JOMAG_predictores_criteria_merged$zhes_zscore<-zscore(as.numeric(unlist(JOMAG_predictores_criteria_merged$zhes)),13.74,5.90)
JOMAG_predictores_criteria_merged$zhor1_zscore<-zscore(as.numeric(unlist(JOMAG_predictores_criteria_merged$zhor1)),22.99,6.30)
JOMAG_predictores_criteria_merged$zntk_zscore<-zscore(as.numeric(unlist(JOMAG_predictores_criteria_merged$zntk)),12.74,5.53)
JOMAG_predictores_criteria_merged$ztur_zscore<-zscore(as.numeric(unlist(JOMAG_predictores_criteria_merged$ztur)),10.30,3.88)
JOMAG_predictores_criteria_merged$zzur_zscore<-zscore(as.numeric(unlist(JOMAG_predictores_criteria_merged$zzur)),15.44,3.78)

nacol_zscores<-ncol(JOMAG_predictores_criteria_merged)

# Locating and handling outliers.
library(sendmailR)
from <- sprintf("<sendmailR@%s>", Sys.info()[4])
to <-"<xxxxx>"
body <- list(paste(" "))
subject <- "JOMAG session has been started"
sendmail(from, to, subject, body,control=list(smtpServer="xxxxx"))

Start_time<-Sys.time()


JOMAG_predictores_criteria_merged_outliers_relevant_columns<-colnames(JOMAG_predictores_criteria_merged[c((nacol_before_zscores+1):nacol_zscores)])

for(i in JOMAG_predictores_criteria_merged_outliers_relevant_columns) {
 JOMAG_predictores_criteria_merged[ncol(JOMAG_predictores_criteria_merged)+1]<-NA
 names(JOMAG_predictores_criteria_merged)[ncol(JOMAG_predictores_criteria_merged)]<-paste(i,"outlier",sep = "_")
 JOMAG_predictores_criteria_merged[ncol(JOMAG_predictores_criteria_merged)]<-as.numeric(unlist(JOMAG_predictores_criteria_merged[ncol(JOMAG_predictores_criteria_merged)]))
 for(j in 1:nrow(JOMAG_predictores_criteria_merged)){
   if (!is.na(JOMAG_predictores_criteria_merged[j,][i])) {
     JOMAG_predictores_criteria_merged[j,][ncol(JOMAG_predictores_criteria_merged)] <-
     ifelse(abs(JOMAG_predictores_criteria_merged[j,][i])>3.29,JOMAG_predictores_criteria_merged[j,][i],NA)
   }
 }
 if (all(is.na(JOMAG_predictores_criteria_merged[ncol(JOMAG_predictores_criteria_merged)]))) {
   JOMAG_predictores_criteria_merged<-JOMAG_predictores_criteria_merged[,-ncol(JOMAG_predictores_criteria_merged)]
 }
}

nacol_outliers<-ncol(JOMAG_predictores_criteria_merged)

End_time<-Sys.time()
Total_time<- round(End_time-Start_time, digits = 2)
Total_time
# Total_time=38.33 mins

library(sendmailR)
from <- sprintf("<sendmailR@%s>", Sys.info()[4])
to <- "<xxxxx>"
subject <- "JOMAG session has been completed"
body <- list(paste("\nLength of time to run the code: ", Total_time,"."))
sendmail(from, to, subject, body,control=list(smtpServer="xxxxx"))

# freq of main outliers.
library(descr)
library(psych)

mode<-function(X)
{
  temp<-table (as.vector(X))
  names (temp)[temp==max(temp)]
}
options(width = 71,max.print=30000)
# The 2 commands after the first command, are for cleaning the output file.
JOMAG_predictores_criteria_merged_freq_relevant_columns<-colnames(JOMAG_predictores_criteria_merged[c((nacol_zscores+1):nacol_outliers)])
out<-""
cat("", out, file="Q:/04_Mehkar/18_asher/Junior officers MAGAV validation/outliers.txt", sep="", append=F,fill = T)
suppressWarnings(for(i in JOMAG_predictores_criteria_merged_freq_relevant_columns) {
  newresult1<-round(freq(ordered(as.numeric(unlist(JOMAG_predictores_criteria_merged[[i]]))), plot = F,main=colnames(JOMAG_predictores_criteria_merged[i]),font=2),2)
  newresult2<-round(describe(as.numeric(unlist(JOMAG_predictores_criteria_merged[[i]]))),2)
  newresult3<-"mode="
  newresult4<-mode(JOMAG_predictores_criteria_merged[[i]])
  newresult5<- "                                                                                               "
  newresult6<- "----------------------------------------------------------------------------"
  out <- capture.output(newresult1,newresult5,newresult2,newresult3,newresult4,newresult5,newresult6)
  out[1]<-""
  cat(colnames(JOMAG_predictores_criteria_merged[i]),out, file="Q:/04_Mehkar/18_asher/Junior officers MAGAV validation/outliers.txt", append=T,fill = T)
})

JOMAG_predictores_criteria_merged[JOMAG_predictores_criteria_merged_outliers_relevant_columns][abs(JOMAG_predictores_criteria_merged[JOMAG_predictores_criteria_merged_outliers_relevant_columns])>4]<-NA

JOMAG_predictores_criteria_merged$final_apptitudes_new<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
                JOMAG_predictores_criteria_merged[i,]$final_apptitudes_new<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],
                select=c(zhak_zscore,zhes_zscore,zhor1_zscore,zntk_zscore,ztur_zscore,zzur_zscore)),na.rm=T)
}

freq(ordered(JOMAG_predictores_criteria_merged$final_apptitudes_new), plot = F,main=colnames(JOMAG_predictores_criteria_merged$final_apptitudes_new),font=2)

library(sendmailR)
from <- sprintf("<sendmailR@%s>", Sys.info()[4])
to <-"<xxxxx>"
body <- list(paste(" "))
subject <- "JOMAG session has been started"
sendmail(from, to, subject, body,control=list(smtpServer="xxxxx"))

Start_time<-Sys.time()

# Criteria.
JOMAG_predictores_criteria_merged$RAvg1_am_2010_zscore<-as.numeric(JOMAG_predictores_criteria_merged$RAvg1_am_2010_zscore)
JOMAG_predictores_criteria_merged$RAvg2_am_2010_zscore<-as.numeric(JOMAG_predictores_criteria_merged$RAvg2_am_2010_zscore)
JOMAG_predictores_criteria_merged$RAvg3_am_2010_zscore<-as.numeric(JOMAG_predictores_criteria_merged$RAvg3_am_2010_zscore)
JOMAG_predictores_criteria_merged$RAvg4_am_2010_zscore<-as.numeric(JOMAG_predictores_criteria_merged$RAvg4_am_2010_zscore)
JOMAG_predictores_criteria_merged$RAvg5_am_2010_zscore<-as.numeric(JOMAG_predictores_criteria_merged$RAvg5_am_2010_zscore)
JOMAG_predictores_criteria_merged$RAvg_am_2010<-NA
JOMAG_predictores_criteria_merged$RAvg_am_2010_nna<-NA
JOMAG_predictores_criteria_merged$RAvg_am_2010_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
    JOMAG_predictores_criteria_merged[i,]$RAvg_am_2010<-
    rowMeans(subset(JOMAG_predictores_criteria_merged[i,],
                    select=c(RAvg1_am_2010_zscore,RAvg2_am_2010_zscore,RAvg3_am_2010_zscore,RAvg4_am_2010_zscore,RAvg5_am_2010_zscore)),na.rm=T)
  JOMAG_predictores_criteria_merged[i,]$RAvg_am_2010_nna<-
    rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],
                         select=c(RAvg1_am_2010_zscore,RAvg2_am_2010_zscore,RAvg3_am_2010_zscore,RAvg4_am_2010_zscore,RAvg5_am_2010_zscore))))
  JOMAG_predictores_criteria_merged[i,]$RAvg_am_2010_pctna<-
    round(JOMAG_predictores_criteria_merged[i,]$RAvg_am_2010_nna/5*100,2)
}

JOMAG_predictores_criteria_merged$RTeken1_am_2010_zscore<-as.numeric(JOMAG_predictores_criteria_merged$RTeken1_am_2010_zscore)
JOMAG_predictores_criteria_merged$RTeken2_am_2010_zscore<-as.numeric(JOMAG_predictores_criteria_merged$RTeken2_am_2010_zscore)
JOMAG_predictores_criteria_merged$RTeken3_am_2010_zscore<-as.numeric(JOMAG_predictores_criteria_merged$RTeken3_am_2010_zscore)
JOMAG_predictores_criteria_merged$RTeken4_am_2010_zscore<-as.numeric(JOMAG_predictores_criteria_merged$RTeken4_am_2010_zscore)
JOMAG_predictores_criteria_merged$RTeken5_am_2010_zscore<-as.numeric(JOMAG_predictores_criteria_merged$RTeken5_am_2010_zscore)
JOMAG_predictores_criteria_merged$RTeken_am_2010<-NA
JOMAG_predictores_criteria_merged$RTeken_am_2010_nna<-NA
JOMAG_predictores_criteria_merged$RTeken_am_2010_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
  JOMAG_predictores_criteria_merged[i,]$RTeken_am_2010<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],
                    select=c(RTeken1_am_2010_zscore,RTeken2_am_2010_zscore,RTeken3_am_2010_zscore,RTeken4_am_2010_zscore,RTeken5_am_2010_zscore)),na.rm=T)
  JOMAG_predictores_criteria_merged[i,]$RTeken_am_2010_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],
    select=c(RTeken1_am_2010_zscore,RTeken2_am_2010_zscore,RTeken3_am_2010_zscore,RTeken4_am_2010_zscore,RTeken5_am_2010_zscore))))
  JOMAG_predictores_criteria_merged[i,]$RTeken_am_2010_pctna<round(JOMAG_predictores_criteria_merged[i,]$RTeken_am_2010_nna/5*100,2)
}

JOMAG_predictores_criteria_merged$NPct1_am_2010_zscore<-as.numeric(JOMAG_predictores_criteria_merged$NPct1_am_2010_zscore)
JOMAG_predictores_criteria_merged$NPct2_am_2010_zscore<-as.numeric(JOMAG_predictores_criteria_merged$NPct2_am_2010_zscore)
JOMAG_predictores_criteria_merged$NPct3_am_2010_zscore<-as.numeric(JOMAG_predictores_criteria_merged$NPct3_am_2010_zscore)
JOMAG_predictores_criteria_merged$NPct_am_2010<-NA
JOMAG_predictores_criteria_merged$NPct_am_2010_nna<-NA
JOMAG_predictores_criteria_merged$NPct_am_2010_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
  JOMAG_predictores_criteria_merged[i,]$NPct_am_2010<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],
                                                           select=c(NPct1_am_2010_zscore,NPct2_am_2010_zscore,NPct3_am_2010_zscore)),na.rm=T)
  JOMAG_predictores_criteria_merged[i,]$NPct_am_2010_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],
                                                                    select=c(NPct1_am_2010_zscore,NPct2_am_2010_zscore,NPct3_am_2010_zscore))))
  JOMAG_predictores_criteria_merged[i,]$NPct_am_2010_pctna<-round(JOMAG_predictores_criteria_merged[i,]$NPct_am_2010_nna/3*100,2)
}

JOMAG_predictores_criteria_merged$PAvg1_cf_2010_zscore<-as.numeric(JOMAG_predictores_criteria_merged$PAvg1_cf_2010_zscore)
JOMAG_predictores_criteria_merged$PAvg2_cf_2010_zscore<-as.numeric(JOMAG_predictores_criteria_merged$PAvg2_cf_2010_zscore)
JOMAG_predictores_criteria_merged$PAvg3_cf_2010_zscore<-as.numeric(JOMAG_predictores_criteria_merged$PAvg3_cf_2010_zscore)
JOMAG_predictores_criteria_merged$PAvg4_cf_2010_zscore<-as.numeric(JOMAG_predictores_criteria_merged$PAvg4_cf_2010_zscore)
JOMAG_predictores_criteria_merged$PAvg5_cf_2010_zscore<-as.numeric(JOMAG_predictores_criteria_merged$PAvg5_cf_2010_zscore)
JOMAG_predictores_criteria_merged$PAvg6_cf_2010_zscore<-as.numeric(JOMAG_predictores_criteria_merged$PAvg6_cf_2010_zscore)
JOMAG_predictores_criteria_merged$PAvg7_cf_2010_zscore<-as.numeric(JOMAG_predictores_criteria_merged$PAvg7_cf_2010_zscore)
JOMAG_predictores_criteria_merged$PAvg8_cf_2010_zscore<-as.numeric(JOMAG_predictores_criteria_merged$PAvg8_cf_2010_zscore)
JOMAG_predictores_criteria_merged$PAvg9_cf_2010_zscore<-as.numeric(JOMAG_predictores_criteria_merged$PAvg9_cf_2010_zscore)
JOMAG_predictores_criteria_merged$PAvg10_cf_2010_zscore<-as.numeric(JOMAG_predictores_criteria_merged$PAvg10_cf_2010_zscore)
JOMAG_predictores_criteria_merged$PAvg11_cf_2010_zscore<-as.numeric(JOMAG_predictores_criteria_merged$PAvg11_cf_2010_zscore)
JOMAG_predictores_criteria_merged$PAvg12_cf_2010_zscore<-as.numeric(JOMAG_predictores_criteria_merged$PAvg12_cf_2010_zscore)
JOMAG_predictores_criteria_merged$PAvg13_cf_2010_zscore<-as.numeric(JOMAG_predictores_criteria_merged$PAvg13_cf_2010_zscore)
JOMAG_predictores_criteria_merged$PAvg14_cf_2010_zscore<-as.numeric(JOMAG_predictores_criteria_merged$PAvg14_cf_2010_zscore)
JOMAG_predictores_criteria_merged$PAvg15_cf_2010_zscore<-as.numeric(JOMAG_predictores_criteria_merged$PAvg15_cf_2010_zscore)
JOMAG_predictores_criteria_merged$PAvg16_cf_2010_zscore<-as.numeric(JOMAG_predictores_criteria_merged$PAvg16_cf_2010_zscore)
JOMAG_predictores_criteria_merged$PAvg17_cf_2010_zscore<-as.numeric(JOMAG_predictores_criteria_merged$PAvg17_cf_2010_zscore)
JOMAG_predictores_criteria_merged$PAvg18_cf_2010_zscore<-as.numeric(JOMAG_predictores_criteria_merged$PAvg18_cf_2010_zscore)
JOMAG_predictores_criteria_merged$PAvg19_cf_2010_zscore<-as.numeric(JOMAG_predictores_criteria_merged$PAvg19_cf_2010_zscore)
JOMAG_predictores_criteria_merged$PAvg20_cf_2010_zscore<-as.numeric(JOMAG_predictores_criteria_merged$PAvg20_cf_2010_zscore)
JOMAG_predictores_criteria_merged$PAvg21_cf_2010_zscore<-as.numeric(JOMAG_predictores_criteria_merged$PAvg21_cf_2010_zscore)
JOMAG_predictores_criteria_merged$PAvg22_cf_2010_zscore<-as.numeric(JOMAG_predictores_criteria_merged$PAvg22_cf_2010_zscore)
JOMAG_predictores_criteria_merged$PAvg23_cf_2010_zscore<-as.numeric(JOMAG_predictores_criteria_merged$PAvg23_cf_2010_zscore)
JOMAG_predictores_criteria_merged$PAvg_cf_2010<-NA
JOMAG_predictores_criteria_merged$PAvg_cf_2010_nna<-NA
JOMAG_predictores_criteria_merged$PAvg_cf_2010_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
  JOMAG_predictores_criteria_merged[i,]$PAvg_cf_2010<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],
                    select=c(PAvg1_cf_2010_zscore,PAvg2_cf_2010_zscore,PAvg3_cf_2010_zscore,PAvg4_cf_2010_zscore,PAvg5_cf_2010_zscore,
                             PAvg6_cf_2010_zscore,PAvg7_cf_2010_zscore,PAvg8_cf_2010_zscore,PAvg9_cf_2010_zscore,PAvg10_cf_2010_zscore,
                             PAvg11_cf_2010_zscore,PAvg12_cf_2010_zscore,PAvg13_cf_2010_zscore,PAvg14_cf_2010_zscore,PAvg15_cf_2010_zscore,
                             PAvg16_cf_2010_zscore,PAvg17_cf_2010_zscore,PAvg18_cf_2010_zscore,PAvg19_cf_2010_zscore,PAvg20_cf_2010_zscore,
                             PAvg21_cf_2010_zscore,PAvg22_cf_2010_zscore,PAvg23_cf_2010_zscore)),na.rm=T)
  JOMAG_predictores_criteria_merged[i,]$PAvg_cf_2010_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],
                         select=c(PAvg1_cf_2010_zscore,PAvg2_cf_2010_zscore,PAvg3_cf_2010_zscore,PAvg4_cf_2010_zscore,PAvg5_cf_2010_zscore,
                                  PAvg6_cf_2010_zscore,PAvg7_cf_2010_zscore,PAvg8_cf_2010_zscore,PAvg9_cf_2010_zscore,PAvg10_cf_2010_zscore,
                                  PAvg11_cf_2010_zscore,PAvg12_cf_2010_zscore,PAvg13_cf_2010_zscore,PAvg14_cf_2010_zscore,PAvg15_cf_2010_zscore,
                                  PAvg16_cf_2010_zscore,PAvg17_cf_2010_zscore,PAvg18_cf_2010_zscore,PAvg19_cf_2010_zscore,PAvg20_cf_2010_zscore,
                                  PAvg21_cf_2010_zscore,PAvg22_cf_2010_zscore,PAvg23_cf_2010_zscore))))
  JOMAG_predictores_criteria_merged[i,]$PAvg_cf_2010_pctna<-round(JOMAG_predictores_criteria_merged[i,]$PAvg_cf_2010_nna/23*100,2)
}

JOMAG_predictores_criteria_merged$FAvg1_cf_2010_zscore<-as.numeric(JOMAG_predictores_criteria_merged$FAvg1_cf_2010_zscore)
JOMAG_predictores_criteria_merged$FAvg2_cf_2010_zscore<-as.numeric(JOMAG_predictores_criteria_merged$FAvg2_cf_2010_zscore)
JOMAG_predictores_criteria_merged$FAvg3_cf_2010_zscore<-as.numeric(JOMAG_predictores_criteria_merged$FAvg3_cf_2010_zscore)
JOMAG_predictores_criteria_merged$FAvg4_cf_2010_zscore<-as.numeric(JOMAG_predictores_criteria_merged$FAvg4_cf_2010_zscore)
JOMAG_predictores_criteria_merged$FAvg5_cf_2010_zscore<-as.numeric(JOMAG_predictores_criteria_merged$FAvg5_cf_2010_zscore)
JOMAG_predictores_criteria_merged$FAvg6_cf_2010_zscore<-as.numeric(JOMAG_predictores_criteria_merged$FAvg6_cf_2010_zscore)
JOMAG_predictores_criteria_merged$FAvg7_cf_2010_zscore<-as.numeric(JOMAG_predictores_criteria_merged$FAvg7_cf_2010_zscore)
JOMAG_predictores_criteria_merged$FAvg_cf_2010<-NA
JOMAG_predictores_criteria_merged$FAvg_cf_2010_nna<-NA
JOMAG_predictores_criteria_merged$FAvg_cf_2010_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
  JOMAG_predictores_criteria_merged[i,]$FAvg_cf_2010<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],
                    select=c(FAvg1_cf_2010_zscore,FAvg2_cf_2010_zscore,FAvg3_cf_2010_zscore,FAvg4_cf_2010_zscore,FAvg5_cf_2010_zscore,
                             FAvg6_cf_2010_zscore,FAvg7_cf_2010_zscore)),na.rm=T)
  JOMAG_predictores_criteria_merged[i,]$FAvg_cf_2010_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],
                         select=c(FAvg1_cf_2010_zscore,FAvg2_cf_2010_zscore,FAvg3_cf_2010_zscore,FAvg4_cf_2010_zscore,FAvg5_cf_2010_zscore,
                                  FAvg6_cf_2010_zscore,FAvg7_cf_2010_zscore))))
  JOMAG_predictores_criteria_merged[i,]$FAvg_cf_2010_pctna<-round(JOMAG_predictores_criteria_merged[i,]$FAvg_cf_2010_nna/7*100,2)
}

JOMAG_predictores_criteria_merged$FTeken1_cf_2010_zscore<-as.numeric(JOMAG_predictores_criteria_merged$FTeken1_cf_2010_zscore)
JOMAG_predictores_criteria_merged$FTeken2_cf_2010_zscore<-as.numeric(JOMAG_predictores_criteria_merged$FTeken2_cf_2010_zscore)
JOMAG_predictores_criteria_merged$FTeken3_cf_2010_zscore<-as.numeric(JOMAG_predictores_criteria_merged$FTeken3_cf_2010_zscore)
JOMAG_predictores_criteria_merged$FTeken4_cf_2010_zscore<-as.numeric(JOMAG_predictores_criteria_merged$FTeken4_cf_2010_zscore)
JOMAG_predictores_criteria_merged$FTeken5_cf_2010_zscore<-as.numeric(JOMAG_predictores_criteria_merged$FTeken5_cf_2010_zscore)
JOMAG_predictores_criteria_merged$FTeken6_cf_2010_zscore<-as.numeric(JOMAG_predictores_criteria_merged$FTeken6_cf_2010_zscore)
JOMAG_predictores_criteria_merged$FTeken7_cf_2010_zscore<-as.numeric(JOMAG_predictores_criteria_merged$FTeken7_cf_2010_zscore)
JOMAG_predictores_criteria_merged$FTeken_cf_2010<-NA
JOMAG_predictores_criteria_merged$FTeken_cf_2010_nna<-NA
JOMAG_predictores_criteria_merged$FTeken_cf_2010_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
  JOMAG_predictores_criteria_merged[i,]$FTeken_cf_2010<-
    rowMeans(subset(JOMAG_predictores_criteria_merged[i,],
                    select=c(FTeken1_cf_2010_zscore,FTeken2_cf_2010_zscore,FTeken3_cf_2010_zscore,FTeken4_cf_2010_zscore,FTeken5_cf_2010_zscore,
                             FTeken6_cf_2010_zscore,FTeken7_cf_2010_zscore)),na.rm=T)
  JOMAG_predictores_criteria_merged[i,]$FTeken_cf_2010_nna<-
    rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],
                         select=c(FTeken1_cf_2010_zscore,FTeken2_cf_2010_zscore,FTeken3_cf_2010_zscore,FTeken4_cf_2010_zscore,FTeken5_cf_2010_zscore,
                                  FTeken6_cf_2010_zscore,FTeken7_cf_2010_zscore))))
  JOMAG_predictores_criteria_merged[i,]$FTeken_cf_2010_pctna<-round(JOMAG_predictores_criteria_merged[i,]$FTeken_cf_2010_nna/7*100,2)
}

JOMAG_predictores_criteria_merged$RAvg1_am_2012_zscore<-as.numeric(JOMAG_predictores_criteria_merged$RAvg1_am_2012_zscore)
JOMAG_predictores_criteria_merged$RAvg2_am_2012_zscore<-as.numeric(JOMAG_predictores_criteria_merged$RAvg2_am_2012_zscore)
JOMAG_predictores_criteria_merged$RAvg3_am_2012_zscore<-as.numeric(JOMAG_predictores_criteria_merged$RAvg3_am_2012_zscore)
JOMAG_predictores_criteria_merged$RAvg4_am_2012_zscore<-as.numeric(JOMAG_predictores_criteria_merged$RAvg4_am_2012_zscore)
JOMAG_predictores_criteria_merged$RAvg5_am_2012_zscore<-as.numeric(JOMAG_predictores_criteria_merged$RAvg5_am_2012_zscore)
JOMAG_predictores_criteria_merged$RAvg_am_2012<-NA
JOMAG_predictores_criteria_merged$RAvg_am_2012_nna<-NA
JOMAG_predictores_criteria_merged$RAvg_am_2012_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
  JOMAG_predictores_criteria_merged[i,]$RAvg_am_2012<-
    rowMeans(subset(JOMAG_predictores_criteria_merged[i,],
                    select=c(RAvg1_am_2012_zscore,RAvg2_am_2012_zscore,RAvg3_am_2012_zscore,RAvg4_am_2012_zscore,RAvg5_am_2012_zscore)),na.rm=T)
  JOMAG_predictores_criteria_merged[i,]$RAvg_am_2012_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],
                         select=c(RAvg1_am_2012_zscore,RAvg2_am_2012_zscore,RAvg3_am_2012_zscore,RAvg4_am_2012_zscore,RAvg5_am_2012_zscore))))
  JOMAG_predictores_criteria_merged[i,]$RAvg_am_2012_pctna<-round(JOMAG_predictores_criteria_merged[i,]$RAvg_am_2012_nna/5*100,2)
}

JOMAG_predictores_criteria_merged$RTeken1_am_2012_zscore<-as.numeric(JOMAG_predictores_criteria_merged$RTeken1_am_2012_zscore)
JOMAG_predictores_criteria_merged$RTeken2_am_2012_zscore<-as.numeric(JOMAG_predictores_criteria_merged$RTeken2_am_2012_zscore)
JOMAG_predictores_criteria_merged$RTeken3_am_2012_zscore<-as.numeric(JOMAG_predictores_criteria_merged$RTeken3_am_2012_zscore)
JOMAG_predictores_criteria_merged$RTeken4_am_2012_zscore<-as.numeric(JOMAG_predictores_criteria_merged$RTeken4_am_2012_zscore)
JOMAG_predictores_criteria_merged$RTeken5_am_2012_zscore<-as.numeric(JOMAG_predictores_criteria_merged$RTeken5_am_2012_zscore)
JOMAG_predictores_criteria_merged$RTeken_am_2012<-NA
JOMAG_predictores_criteria_merged$RTeken_am_2012_nna<-NA
JOMAG_predictores_criteria_merged$RTeken_am_2012_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
  JOMAG_predictores_criteria_merged[i,]$RTeken_am_2012<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],
                    select=c(RTeken1_am_2012_zscore,RTeken2_am_2012_zscore,RTeken3_am_2012_zscore,RTeken4_am_2012_zscore,RTeken5_am_2012_zscore)),na.rm=T)
  JOMAG_predictores_criteria_merged[i,]$RTeken_am_2012_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],
                         select=c(RTeken1_am_2012_zscore,RTeken2_am_2012_zscore,RTeken3_am_2012_zscore,RTeken4_am_2012_zscore,RTeken5_am_2012_zscore))))
  JOMAG_predictores_criteria_merged[i,]$RTeken_am_2012_pctna<-round(JOMAG_predictores_criteria_merged[i,]$RTeken_am_2012_nna/5*100,2)
}

JOMAG_predictores_criteria_merged$NPct1_am_2012_zscore<-as.numeric(JOMAG_predictores_criteria_merged$NPct1_am_2012_zscore)
JOMAG_predictores_criteria_merged$NPct2_am_2012_zscore<-as.numeric(JOMAG_predictores_criteria_merged$NPct2_am_2012_zscore)
JOMAG_predictores_criteria_merged$NPct3_am_2012_zscore<-as.numeric(JOMAG_predictores_criteria_merged$NPct3_am_2012_zscore)
JOMAG_predictores_criteria_merged$NPct_am_2012<-NA
JOMAG_predictores_criteria_merged$NPct_am_2012_nna<-NA
JOMAG_predictores_criteria_merged$NPct_am_2012_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
  JOMAG_predictores_criteria_merged[i,]$NPct_am_2012<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],
                                                           select=c(NPct1_am_2012_zscore,NPct2_am_2012_zscore,NPct3_am_2012_zscore)),na.rm=T)
  JOMAG_predictores_criteria_merged[i,]$NPct_am_2012_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],
                                                                    select=c(NPct1_am_2012_zscore,NPct2_am_2012_zscore,NPct3_am_2012_zscore))))
  JOMAG_predictores_criteria_merged[i,]$NPct_am_2012_pctna<-round(JOMAG_predictores_criteria_merged[i,]$NPct_am_2012_nna/3*100,2)
}

JOMAG_predictores_criteria_merged$PAvg1_cf_2012_zscore<-as.numeric(JOMAG_predictores_criteria_merged$PAvg1_cf_2012_zscore)
JOMAG_predictores_criteria_merged$PAvg2_cf_2012_zscore<-as.numeric(JOMAG_predictores_criteria_merged$PAvg2_cf_2012_zscore)
JOMAG_predictores_criteria_merged$PAvg3_cf_2012_zscore<-as.numeric(JOMAG_predictores_criteria_merged$PAvg3_cf_2012_zscore)
JOMAG_predictores_criteria_merged$PAvg4_cf_2012_zscore<-as.numeric(JOMAG_predictores_criteria_merged$PAvg4_cf_2012_zscore)
JOMAG_predictores_criteria_merged$PAvg5_cf_2012_zscore<-as.numeric(JOMAG_predictores_criteria_merged$PAvg5_cf_2012_zscore)
JOMAG_predictores_criteria_merged$PAvg6_cf_2012_zscore<-as.numeric(JOMAG_predictores_criteria_merged$PAvg6_cf_2012_zscore)
JOMAG_predictores_criteria_merged$PAvg7_cf_2012_zscore<-as.numeric(JOMAG_predictores_criteria_merged$PAvg7_cf_2012_zscore)
JOMAG_predictores_criteria_merged$PAvg8_cf_2012_zscore<-as.numeric(JOMAG_predictores_criteria_merged$PAvg8_cf_2012_zscore)
JOMAG_predictores_criteria_merged$PAvg9_cf_2012_zscore<-as.numeric(JOMAG_predictores_criteria_merged$PAvg9_cf_2012_zscore)
JOMAG_predictores_criteria_merged$PAvg10_cf_2012_zscore<-as.numeric(JOMAG_predictores_criteria_merged$PAvg10_cf_2012_zscore)
JOMAG_predictores_criteria_merged$PAvg11_cf_2012_zscore<-as.numeric(JOMAG_predictores_criteria_merged$PAvg11_cf_2012_zscore)
JOMAG_predictores_criteria_merged$PAvg12_cf_2012_zscore<-as.numeric(JOMAG_predictores_criteria_merged$PAvg12_cf_2012_zscore)
JOMAG_predictores_criteria_merged$PAvg13_cf_2012_zscore<-as.numeric(JOMAG_predictores_criteria_merged$PAvg13_cf_2012_zscore)
JOMAG_predictores_criteria_merged$PAvg14_cf_2012_zscore<-as.numeric(JOMAG_predictores_criteria_merged$PAvg14_cf_2012_zscore)
JOMAG_predictores_criteria_merged$PAvg15_cf_2012_zscore<-as.numeric(JOMAG_predictores_criteria_merged$PAvg15_cf_2012_zscore)
JOMAG_predictores_criteria_merged$PAvg16_cf_2012_zscore<-as.numeric(JOMAG_predictores_criteria_merged$PAvg16_cf_2012_zscore)
JOMAG_predictores_criteria_merged$PAvg17_cf_2012_zscore<-as.numeric(JOMAG_predictores_criteria_merged$PAvg17_cf_2012_zscore)
JOMAG_predictores_criteria_merged$PAvg18_cf_2012_zscore<-as.numeric(JOMAG_predictores_criteria_merged$PAvg18_cf_2012_zscore)
JOMAG_predictores_criteria_merged$PAvg19_cf_2012_zscore<-as.numeric(JOMAG_predictores_criteria_merged$PAvg19_cf_2012_zscore)
JOMAG_predictores_criteria_merged$PAvg20_cf_2012_zscore<-as.numeric(JOMAG_predictores_criteria_merged$PAvg20_cf_2012_zscore)
JOMAG_predictores_criteria_merged$PAvg21_cf_2012_zscore<-as.numeric(JOMAG_predictores_criteria_merged$PAvg21_cf_2012_zscore)
JOMAG_predictores_criteria_merged$PAvg22_cf_2012_zscore<-as.numeric(JOMAG_predictores_criteria_merged$PAvg22_cf_2012_zscore)
JOMAG_predictores_criteria_merged$PAvg23_cf_2012_zscore<-as.numeric(JOMAG_predictores_criteria_merged$PAvg23_cf_2012_zscore)
JOMAG_predictores_criteria_merged$PAvg_cf_2012<-NA
JOMAG_predictores_criteria_merged$PAvg_cf_2012_nna<-NA
JOMAG_predictores_criteria_merged$PAvg_cf_2012_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
  JOMAG_predictores_criteria_merged[i,]$PAvg_cf_2012<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],
                    select=c(PAvg1_cf_2012_zscore,PAvg2_cf_2012_zscore,PAvg3_cf_2012_zscore,PAvg4_cf_2012_zscore,PAvg5_cf_2012_zscore,
                             PAvg6_cf_2012_zscore,PAvg7_cf_2012_zscore,PAvg8_cf_2012_zscore,PAvg9_cf_2012_zscore,PAvg10_cf_2012_zscore,
                             PAvg11_cf_2012_zscore,PAvg12_cf_2012_zscore,PAvg13_cf_2012_zscore,PAvg14_cf_2012_zscore,PAvg15_cf_2012_zscore,
                             PAvg16_cf_2012_zscore,PAvg17_cf_2012_zscore,PAvg18_cf_2012_zscore,PAvg19_cf_2012_zscore,PAvg20_cf_2012_zscore,
                             PAvg21_cf_2012_zscore,PAvg22_cf_2012_zscore,PAvg23_cf_2012_zscore)),na.rm=T)
  JOMAG_predictores_criteria_merged[i,]$PAvg_cf_2012_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],
                         select=c(PAvg1_cf_2012_zscore,PAvg2_cf_2012_zscore,PAvg3_cf_2012_zscore,PAvg4_cf_2012_zscore,PAvg5_cf_2012_zscore,
                                  PAvg6_cf_2012_zscore,PAvg7_cf_2012_zscore,PAvg8_cf_2012_zscore,PAvg9_cf_2012_zscore,PAvg10_cf_2012_zscore,
                                  PAvg11_cf_2012_zscore,PAvg12_cf_2012_zscore,PAvg13_cf_2012_zscore,PAvg14_cf_2012_zscore,PAvg15_cf_2012_zscore,
                                  PAvg16_cf_2012_zscore,PAvg17_cf_2012_zscore,PAvg18_cf_2012_zscore,PAvg19_cf_2012_zscore,PAvg20_cf_2012_zscore,
                                  PAvg21_cf_2012_zscore,PAvg22_cf_2012_zscore,PAvg23_cf_2012_zscore))))
  JOMAG_predictores_criteria_merged[i,]$PAvg_cf_2012_pctna<-round(JOMAG_predictores_criteria_merged[i,]$PAvg_cf_2012_nna/23*100,2)
}

JOMAG_predictores_criteria_merged$FAvg1_cf_2012_zscore<-as.numeric(JOMAG_predictores_criteria_merged$FAvg1_cf_2012_zscore)
JOMAG_predictores_criteria_merged$FAvg2_cf_2012_zscore<-as.numeric(JOMAG_predictores_criteria_merged$FAvg2_cf_2012_zscore)
JOMAG_predictores_criteria_merged$FAvg3_cf_2012_zscore<-as.numeric(JOMAG_predictores_criteria_merged$FAvg3_cf_2012_zscore)
JOMAG_predictores_criteria_merged$FAvg4_cf_2012_zscore<-as.numeric(JOMAG_predictores_criteria_merged$FAvg4_cf_2012_zscore)
JOMAG_predictores_criteria_merged$FAvg5_cf_2012_zscore<-as.numeric(JOMAG_predictores_criteria_merged$FAvg5_cf_2012_zscore)
JOMAG_predictores_criteria_merged$FAvg6_cf_2012_zscore<-as.numeric(JOMAG_predictores_criteria_merged$FAvg6_cf_2012_zscore)
JOMAG_predictores_criteria_merged$FAvg7_cf_2012_zscore<-as.numeric(JOMAG_predictores_criteria_merged$FAvg7_cf_2012_zscore)
JOMAG_predictores_criteria_merged$FAvg_cf_2012<-NA
JOMAG_predictores_criteria_merged$FAvg_cf_2012_nna<-NA
JOMAG_predictores_criteria_merged$FAvg_cf_2012_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
  JOMAG_predictores_criteria_merged[i,]$FAvg_cf_2012<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],
                    select=c(FAvg1_cf_2012_zscore,FAvg2_cf_2012_zscore,FAvg3_cf_2012_zscore,FAvg4_cf_2012_zscore,FAvg5_cf_2012_zscore,
                             FAvg6_cf_2012_zscore,FAvg7_cf_2012_zscore)),na.rm=T)
  JOMAG_predictores_criteria_merged[i,]$FAvg_cf_2012_nna<-
    rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],
                         select=c(FAvg1_cf_2012_zscore,FAvg2_cf_2012_zscore,FAvg3_cf_2012_zscore,FAvg4_cf_2012_zscore,FAvg5_cf_2012_zscore,
                                  FAvg6_cf_2012_zscore,FAvg7_cf_2012_zscore))))
  JOMAG_predictores_criteria_merged[i,]$FAvg_cf_2012_pctna<-round(JOMAG_predictores_criteria_merged[i,]$FAvg_cf_2012_nna/7*100,2)
}

JOMAG_predictores_criteria_merged$FTeken1_cf_2012_zscore<-as.numeric(JOMAG_predictores_criteria_merged$FTeken1_cf_2012_zscore)
JOMAG_predictores_criteria_merged$FTeken2_cf_2012_zscore<-as.numeric(JOMAG_predictores_criteria_merged$FTeken2_cf_2012_zscore)
JOMAG_predictores_criteria_merged$FTeken3_cf_2012_zscore<-as.numeric(JOMAG_predictores_criteria_merged$FTeken3_cf_2012_zscore)
JOMAG_predictores_criteria_merged$FTeken4_cf_2012_zscore<-as.numeric(JOMAG_predictores_criteria_merged$FTeken4_cf_2012_zscore)
JOMAG_predictores_criteria_merged$FTeken5_cf_2012_zscore<-as.numeric(JOMAG_predictores_criteria_merged$FTeken5_cf_2012_zscore)
JOMAG_predictores_criteria_merged$FTeken6_cf_2012_zscore<-as.numeric(JOMAG_predictores_criteria_merged$FTeken6_cf_2012_zscore)
JOMAG_predictores_criteria_merged$FTeken7_cf_2012_zscore<-as.numeric(JOMAG_predictores_criteria_merged$FTeken7_cf_2012_zscore)
JOMAG_predictores_criteria_merged$FTeken_cf_2012<-NA
JOMAG_predictores_criteria_merged$FTeken_cf_2012_nna<-NA
JOMAG_predictores_criteria_merged$FTeken_cf_2012_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
  JOMAG_predictores_criteria_merged[i,]$FTeken_cf_2012<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],
                    select=c(FTeken1_cf_2012_zscore,FTeken2_cf_2012_zscore,FTeken3_cf_2012_zscore,FTeken4_cf_2012_zscore,FTeken5_cf_2012_zscore,
                             FTeken6_cf_2012_zscore,FTeken7_cf_2012_zscore)),na.rm=T)
  JOMAG_predictores_criteria_merged[i,]$FTeken_cf_2012_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],
                         select=c(FTeken1_cf_2012_zscore,FTeken2_cf_2012_zscore,FTeken3_cf_2012_zscore,FTeken4_cf_2012_zscore,FTeken5_cf_2012_zscore,
                                  FTeken6_cf_2012_zscore,FTeken7_cf_2012_zscore))))
  JOMAG_predictores_criteria_merged[i,]$FTeken_cf_2012_pctna<-round(JOMAG_predictores_criteria_merged[i,]$FTeken_cf_2012_nna/7*100,2)
}

JOMAG_predictores_criteria_merged$RAvg1_am_2015_zscore<-as.numeric(JOMAG_predictores_criteria_merged$RAvg1_am_2015_zscore)
JOMAG_predictores_criteria_merged$RAvg2_am_2015_zscore<-as.numeric(JOMAG_predictores_criteria_merged$RAvg2_am_2015_zscore)
JOMAG_predictores_criteria_merged$RAvg3_am_2015_zscore<-as.numeric(JOMAG_predictores_criteria_merged$RAvg3_am_2015_zscore)
JOMAG_predictores_criteria_merged$RAvg4_am_2015_zscore<-as.numeric(JOMAG_predictores_criteria_merged$RAvg4_am_2015_zscore)
JOMAG_predictores_criteria_merged$RAvg5_am_2015_zscore<-as.numeric(JOMAG_predictores_criteria_merged$RAvg5_am_2015_zscore)
JOMAG_predictores_criteria_merged$RAvg_am_2015<-NA
JOMAG_predictores_criteria_merged$RAvg_am_2015_nna<-NA
JOMAG_predictores_criteria_merged$RAvg_am_2015_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
  JOMAG_predictores_criteria_merged[i,]$RAvg_am_2015<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],
                    select=c(RAvg1_am_2015_zscore,RAvg2_am_2015_zscore,RAvg3_am_2015_zscore,RAvg4_am_2015_zscore,RAvg5_am_2015_zscore)),na.rm=T)
  JOMAG_predictores_criteria_merged[i,]$RAvg_am_2015_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],
                         select=c(RAvg1_am_2015_zscore,RAvg2_am_2015_zscore,RAvg3_am_2015_zscore,RAvg4_am_2015_zscore,RAvg5_am_2015_zscore))))
  JOMAG_predictores_criteria_merged[i,]$RAvg_am_2015_pctna<-round(JOMAG_predictores_criteria_merged[i,]$RAvg_am_2015_nna/5*100,2)
}

JOMAG_predictores_criteria_merged$RTeken1_am_2015_zscore<-as.numeric(JOMAG_predictores_criteria_merged$RTeken1_am_2015_zscore)
JOMAG_predictores_criteria_merged$RTeken2_am_2015_zscore<-as.numeric(JOMAG_predictores_criteria_merged$RTeken2_am_2015_zscore)
JOMAG_predictores_criteria_merged$RTeken3_am_2015_zscore<-as.numeric(JOMAG_predictores_criteria_merged$RTeken3_am_2015_zscore)
JOMAG_predictores_criteria_merged$RTeken4_am_2015_zscore<-as.numeric(JOMAG_predictores_criteria_merged$RTeken4_am_2015_zscore)
JOMAG_predictores_criteria_merged$RTeken5_am_2015_zscore<-as.numeric(JOMAG_predictores_criteria_merged$RTeken5_am_2015_zscore)
JOMAG_predictores_criteria_merged$RTeken_am_2015<-NA
JOMAG_predictores_criteria_merged$RTeken_am_2015_nna<-NA
JOMAG_predictores_criteria_merged$RTeken_am_2015_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
  JOMAG_predictores_criteria_merged[i,]$RTeken_am_2015<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],
                    select=c(RTeken1_am_2015_zscore,RTeken2_am_2015_zscore,RTeken3_am_2015_zscore,RTeken4_am_2015_zscore,RTeken5_am_2015_zscore)),na.rm=T)
  JOMAG_predictores_criteria_merged[i,]$RTeken_am_2015_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],
                         select=c(RTeken1_am_2015_zscore,RTeken2_am_2015_zscore,RTeken3_am_2015_zscore,RTeken4_am_2015_zscore,RTeken5_am_2015_zscore))))
  JOMAG_predictores_criteria_merged[i,]$RTeken_am_2015_pctna<-round(JOMAG_predictores_criteria_merged[i,]$RTeken_am_2015_nna/5*100,2)
}

# am_2015/am_2015_special (it appears in the am_regular file, but has only NPct vars. Ronen: am special were only since am_2015)

zscore<-function(x,mean,sd){
  (x-mean)/sd
}
JOMAG_predictores_criteria_merged$NPct1_am_2015_special_zscore<-NA
JOMAG_predictores_criteria_merged$NPct2_am_2015_special_zscore<-NA
JOMAG_predictores_criteria_merged$NPct3_am_2015_special_zscore<-NA
JOMAG_predictores_criteria_merged$NPct1_am_2015_zscore<-NA
JOMAG_predictores_criteria_merged$NPct2_am_2015_zscore<-NA
JOMAG_predictores_criteria_merged$NPct3_am_2015_zscore<-NA

for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
    if(is.na(JOMAG_predictores_criteria_merged[i,]$RAvg_am_2015) & (!is.na(JOMAG_predictores_criteria_merged[i,]$NPct1_am_2015) | !is.na(JOMAG_predictores_criteria_merged[i,]$NPct2_am_2015) | !is.na(JOMAG_predictores_criteria_merged[i,]$NPct3_am_2015))){
    JOMAG_predictores_criteria_merged[i,]$NPct1_am_2015_special_zscore<-zscore(as.numeric(unlist(JOMAG_predictores_criteria_merged[i,]$NPct1_am_2015)),mean(as.numeric(unlist(JOMAG_predictores_criteria_merged$NPct1_am_2015)),na.rm=T),sd(as.numeric(unlist(JOMAG_predictores_criteria_merged$NPct1_am_2015)),na.rm=T))
    JOMAG_predictores_criteria_merged[i,]$NPct2_am_2015_special_zscore<-zscore(as.numeric(unlist(JOMAG_predictores_criteria_merged[i,]$NPct2_am_2015)),mean(as.numeric(unlist(JOMAG_predictores_criteria_merged$NPct2_am_2015)),na.rm=T),sd(as.numeric(unlist(JOMAG_predictores_criteria_merged$NPct2_am_2015)),na.rm=T))
    JOMAG_predictores_criteria_merged[i,]$NPct3_am_2015_special_zscore<-zscore(as.numeric(unlist(JOMAG_predictores_criteria_merged[i,]$NPct3_am_2015)),mean(as.numeric(unlist(JOMAG_predictores_criteria_merged$NPct3_am_2015)),na.rm=T),sd(as.numeric(unlist(JOMAG_predictores_criteria_merged$NPct3_am_2015)),na.rm=T))
  }
  
  else {
    
  JOMAG_predictores_criteria_merged[i,]$NPct1_am_2015_zscore<-zscore(as.numeric(unlist(JOMAG_predictores_criteria_merged[i,]$NPct1_am_2015)),mean(as.numeric(unlist(JOMAG_predictores_criteria_merged$NPct1_am_2015)),na.rm=T),sd(as.numeric(unlist(JOMAG_predictores_criteria_merged$NPct1_am_2015)),na.rm=T))
  JOMAG_predictores_criteria_merged[i,]$NPct2_am_2015_zscore<-zscore(as.numeric(unlist(JOMAG_predictores_criteria_merged[i,]$NPct2_am_2015)),mean(as.numeric(unlist(JOMAG_predictores_criteria_merged$NPct2_am_2015)),na.rm=T),sd(as.numeric(unlist(JOMAG_predictores_criteria_merged$NPct2_am_2015)),na.rm=T))
  JOMAG_predictores_criteria_merged[i,]$NPct3_am_2015_zscore<-zscore(as.numeric(unlist(JOMAG_predictores_criteria_merged[i,]$NPct3_am_2015)),mean(as.numeric(unlist(JOMAG_predictores_criteria_merged$NPct3_am_2015)),na.rm=T),sd(as.numeric(unlist(JOMAG_predictores_criteria_merged$NPct3_am_2015)),na.rm=T))
  }
}

nacol_before_NPct_am_2015_outliers<-ncol(JOMAG_predictores_criteria_merged)

# Locating and handling NPct_am_2015 outliers.
JOMAG_predictores_criteria_merged_outliers_relevant_columns <- colnames(JOMAG_predictores_criteria_merged[c((ncol(JOMAG_predictores_criteria_merged)-5)):ncol(JOMAG_predictores_criteria_merged)]) 

for(i in JOMAG_predictores_criteria_merged_outliers_relevant_columns) {
  JOMAG_predictores_criteria_merged[ncol(JOMAG_predictores_criteria_merged)+1]<-NA
  names(JOMAG_predictores_criteria_merged)[ncol(JOMAG_predictores_criteria_merged)]<-paste(i,"outlier",sep = "_")
  JOMAG_predictores_criteria_merged[ncol(JOMAG_predictores_criteria_merged)]<-as.numeric(unlist(JOMAG_predictores_criteria_merged[ncol(JOMAG_predictores_criteria_merged)]))
  for(j in 1:nrow(JOMAG_predictores_criteria_merged)){
    if (!is.na(JOMAG_predictores_criteria_merged[j,][i])) {
      JOMAG_predictores_criteria_merged[j,][ncol(JOMAG_predictores_criteria_merged)] <-
        ifelse(abs(JOMAG_predictores_criteria_merged[j,][i])>3.29,JOMAG_predictores_criteria_merged[j,][i],NA)
    }
  }
  if (all(is.na(JOMAG_predictores_criteria_merged[ncol(JOMAG_predictores_criteria_merged)]))) {
    JOMAG_predictores_criteria_merged<-JOMAG_predictores_criteria_merged[,-ncol(JOMAG_predictores_criteria_merged)]
  }
}

nacol_after_NPct_am_2015_outliers<-ncol(JOMAG_predictores_criteria_merged)

while (nacol_after_NPct_am_2015_outliers!=nacol_before_NPct_am_2015_outliers) {
# freq of NPct_am_2015 outliers.
library(descr)
library(psych)

mode<-function(X)
{
 temp<-table (as.vector(X))
 names (temp)[temp==max(temp)]
}
 options(width = 71,max.print=30000)
# # The 2 commands after the first command, are for cleaning the output file.
JOMAG_predictores_criteria_merged_freq_relevant_columns<-
colnames(JOMAG_predictores_criteria_merged[c((nacol_before_NPct_am_2015_outliers+1):nacol_after_NPct_am_2015_outliers)])
out<-""
cat("", out, file="Q:/04_Mehkar/18_asher/Junior officers MAGAV validation/NPct_am_2015 outliers.txt", sep="", append=F,fill = T)
suppressWarnings(for(i in JOMAG_predictores_criteria_merged_freq_relevant_columns) {
 newresult1<-round(freq(ordered(as.numeric(unlist(JOMAG_predictores_criteria_merged[[i]]))), plot = F,main=colnames(JOMAG_predictores_criteria_merged[i]),font=2),2)
 newresult2<-round(describe(as.numeric(unlist(JOMAG_predictores_criteria_merged[[i]]))),2)
 newresult3<-"mode="
 newresult4<-mode(JOMAG_predictores_criteria_merged[[i]])
 newresult5<- "                                                                                               "
 newresult6<- "----------------------------------------------------------------------------"
 out <- capture.output(newresult1,newresult5,newresult2,newresult3,newresult4,newresult5,newresult6)
 out[1]<-""
 cat(colnames(JOMAG_predictores_criteria_merged[i]),out, file="Q:/04_Mehkar/18_asher/Junior officers MAGAV validation/NPct_am_2015_outliers.txt", append=T,fill = T)
})
JOMAG_predictores_criteria_merged[JOMAG_predictores_criteria_merged_outliers_relevant_columns][abs(JOMAG_predictores_criteria_merged[JOMAG_predictores_criteria_merged_outliers_relevant_columns])>4]<-NA
break()
}

JOMAG_predictores_criteria_merged$NPct1_am_2015_zscore<-as.numeric(JOMAG_predictores_criteria_merged$NPct1_am_2015_zscore)
JOMAG_predictores_criteria_merged$NPct2_am_2015_zscore<-as.numeric(JOMAG_predictores_criteria_merged$NPct2_am_2015_zscore)
JOMAG_predictores_criteria_merged$NPct3_am_2015_zscore<-as.numeric(JOMAG_predictores_criteria_merged$NPct3_am_2015_zscore)
JOMAG_predictores_criteria_merged$NPct_am_2015<-NA
JOMAG_predictores_criteria_merged$NPct_am_2015_nna<-NA
JOMAG_predictores_criteria_merged$NPct_am_2015_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
  JOMAG_predictores_criteria_merged[i,]$NPct_am_2015<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],
                                                           select=c(NPct1_am_2015_zscore,NPct2_am_2015_zscore,NPct3_am_2015_zscore)),na.rm=T)
  JOMAG_predictores_criteria_merged[i,]$NPct_am_2015_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],
                                                                    select=c(NPct1_am_2015_zscore,NPct2_am_2015_zscore,NPct3_am_2015_zscore))))
  JOMAG_predictores_criteria_merged[i,]$NPct_am_2015_pctna<-round(JOMAG_predictores_criteria_merged[i,]$NPct_am_2015_nna/3*100,2)
}

JOMAG_predictores_criteria_merged$NPct1_am_2015_special_zscore<-as.numeric(JOMAG_predictores_criteria_merged$NPct1_am_2015_special_zscore)
JOMAG_predictores_criteria_merged$NPct2_am_2015_special_zscore<-as.numeric(JOMAG_predictores_criteria_merged$NPct2_am_2015_special_zscore)
JOMAG_predictores_criteria_merged$NPct3_am_2015_special_zscore<-as.numeric(JOMAG_predictores_criteria_merged$NPct3_am_2015_special_zscore)
JOMAG_predictores_criteria_merged$NPct_am_2015_special<-NA
JOMAG_predictores_criteria_merged$NPct_am_2015_special_nna<-NA
JOMAG_predictores_criteria_merged$NPct_am_2015_special_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
  JOMAG_predictores_criteria_merged[i,]$NPct_am_2015_special<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],
                                                                   select=c(NPct1_am_2015_special_zscore,NPct2_am_2015_special_zscore,NPct3_am_2015_special_zscore)),na.rm=T)
  JOMAG_predictores_criteria_merged[i,]$NPct_am_2015_special_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],
                                                                            select=c(NPct1_am_2015_special_zscore,NPct2_am_2015_special_zscore,NPct3_am_2015_special_zscore))))
  JOMAG_predictores_criteria_merged[i,]$NPct_am_2015_special_pctna<-round(JOMAG_predictores_criteria_merged[i,]$NPct_am_2015_special_nna/3*100,2)
}

JOMAG_predictores_criteria_merged$FAvg1_cf_2015_zscore<-as.numeric(JOMAG_predictores_criteria_merged$FAvg1_cf_2015_zscore)
JOMAG_predictores_criteria_merged$FAvg2_cf_2015_zscore<-as.numeric(JOMAG_predictores_criteria_merged$FAvg2_cf_2015_zscore)
JOMAG_predictores_criteria_merged$FAvg3_cf_2015_zscore<-as.numeric(JOMAG_predictores_criteria_merged$FAvg3_cf_2015_zscore)
JOMAG_predictores_criteria_merged$FAvg4_cf_2015_zscore<-as.numeric(JOMAG_predictores_criteria_merged$FAvg4_cf_2015_zscore)
JOMAG_predictores_criteria_merged$FAvg5_cf_2015_zscore<-as.numeric(JOMAG_predictores_criteria_merged$FAvg5_cf_2015_zscore)
JOMAG_predictores_criteria_merged$FAvg6_cf_2015_zscore<-as.numeric(JOMAG_predictores_criteria_merged$FAvg6_cf_2015_zscore)
JOMAG_predictores_criteria_merged$FAvg7_cf_2015_zscore<-as.numeric(JOMAG_predictores_criteria_merged$FAvg7_cf_2015_zscore)
JOMAG_predictores_criteria_merged$FAvg_cf_2015<-NA
JOMAG_predictores_criteria_merged$FAvg_cf_2015_nna<-NA
JOMAG_predictores_criteria_merged$FAvg_cf_2015_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
  JOMAG_predictores_criteria_merged[i,]$FAvg_cf_2015<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],
                    select=c(FAvg1_cf_2015_zscore,FAvg2_cf_2015_zscore,FAvg3_cf_2015_zscore,FAvg4_cf_2015_zscore,FAvg5_cf_2015_zscore,
                             FAvg6_cf_2015_zscore,FAvg7_cf_2015_zscore)),na.rm=T)
  JOMAG_predictores_criteria_merged[i,]$FAvg_cf_2015_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],
                         select=c(FAvg1_cf_2015_zscore,FAvg2_cf_2015_zscore,FAvg3_cf_2015_zscore,FAvg4_cf_2015_zscore,FAvg5_cf_2015_zscore,
                                  FAvg6_cf_2015_zscore,FAvg7_cf_2015_zscore))))
  JOMAG_predictores_criteria_merged[i,]$FAvg_cf_2015_pctna<-round(JOMAG_predictores_criteria_merged[i,]$FAvg_cf_2015_nna/7*100,2)
}

JOMAG_predictores_criteria_merged$FTeken1_cf_2015_zscore<-as.numeric(JOMAG_predictores_criteria_merged$FTeken1_cf_2015_zscore)
JOMAG_predictores_criteria_merged$FTeken2_cf_2015_zscore<-as.numeric(JOMAG_predictores_criteria_merged$FTeken2_cf_2015_zscore)
JOMAG_predictores_criteria_merged$FTeken3_cf_2015_zscore<-as.numeric(JOMAG_predictores_criteria_merged$FTeken3_cf_2015_zscore)
JOMAG_predictores_criteria_merged$FTeken4_cf_2015_zscore<-as.numeric(JOMAG_predictores_criteria_merged$FTeken4_cf_2015_zscore)
JOMAG_predictores_criteria_merged$FTeken5_cf_2015_zscore<-as.numeric(JOMAG_predictores_criteria_merged$FTeken5_cf_2015_zscore)
JOMAG_predictores_criteria_merged$FTeken6_cf_2015_zscore<-as.numeric(JOMAG_predictores_criteria_merged$FTeken6_cf_2015_zscore)
JOMAG_predictores_criteria_merged$FTeken7_cf_2015_zscore<-as.numeric(JOMAG_predictores_criteria_merged$FTeken7_cf_2015_zscore)
JOMAG_predictores_criteria_merged$FTeken_cf_2015<-NA
JOMAG_predictores_criteria_merged$FTeken_cf_2015_nna<-NA
JOMAG_predictores_criteria_merged$FTeken_cf_2015_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
  JOMAG_predictores_criteria_merged[i,]$FTeken_cf_2015<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],
                    select=c(FTeken1_cf_2015_zscore,FTeken2_cf_2015_zscore,FTeken3_cf_2015_zscore,FTeken4_cf_2015_zscore,FTeken5_cf_2015_zscore,
                             FTeken6_cf_2015_zscore,FTeken7_cf_2015_zscore)),na.rm=T)
  JOMAG_predictores_criteria_merged[i,]$FTeken_cf_2015_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],
                         select=c(FTeken1_cf_2015_zscore,FTeken2_cf_2015_zscore,FTeken3_cf_2015_zscore,FTeken4_cf_2015_zscore,FTeken5_cf_2015_zscore,
                                  FTeken6_cf_2015_zscore,FTeken7_cf_2015_zscore))))
  JOMAG_predictores_criteria_merged[i,]$FTeken_cf_2015_pctna<-round(JOMAG_predictores_criteria_merged[i,]$FTeken_cf_2015_nna/7*100,2)
}

JOMAG_predictores_criteria_merged$RAvg1_am_2018_zscore<-as.numeric(JOMAG_predictores_criteria_merged$RAvg1_am_2018_zscore)
JOMAG_predictores_criteria_merged$RAvg2_am_2018_zscore<-as.numeric(JOMAG_predictores_criteria_merged$RAvg2_am_2018_zscore)
JOMAG_predictores_criteria_merged$RAvg3_am_2018_zscore<-as.numeric(JOMAG_predictores_criteria_merged$RAvg3_am_2018_zscore)
JOMAG_predictores_criteria_merged$RAvg4_am_2018_zscore<-as.numeric(JOMAG_predictores_criteria_merged$RAvg4_am_2018_zscore)
JOMAG_predictores_criteria_merged$RAvg5_am_2018_zscore<-as.numeric(JOMAG_predictores_criteria_merged$RAvg5_am_2018_zscore)
JOMAG_predictores_criteria_merged$RAvg_am_2018<-NA
JOMAG_predictores_criteria_merged$RAvg_am_2018_nna<-NA
JOMAG_predictores_criteria_merged$RAvg_am_2018_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
  JOMAG_predictores_criteria_merged[i,]$RAvg_am_2018<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],
                                                           select=c(RAvg1_am_2018_zscore,RAvg2_am_2018_zscore,RAvg3_am_2018_zscore,RAvg4_am_2018_zscore,RAvg5_am_2018_zscore)),na.rm=T)
  JOMAG_predictores_criteria_merged[i,]$RAvg_am_2018_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],
                                                                    select=c(RAvg1_am_2018_zscore,RAvg2_am_2018_zscore,RAvg3_am_2018_zscore,RAvg4_am_2018_zscore,RAvg5_am_2018_zscore))))
  JOMAG_predictores_criteria_merged[i,]$RAvg_am_2018_pctna<-round(JOMAG_predictores_criteria_merged[i,]$RAvg_am_2018_nna/5*100,2)
}

JOMAG_predictores_criteria_merged$RTeken1_am_2018_zscore<-as.numeric(JOMAG_predictores_criteria_merged$RTeken1_am_2018_zscore)
JOMAG_predictores_criteria_merged$RTeken2_am_2018_zscore<-as.numeric(JOMAG_predictores_criteria_merged$RTeken2_am_2018_zscore)
JOMAG_predictores_criteria_merged$RTeken3_am_2018_zscore<-as.numeric(JOMAG_predictores_criteria_merged$RTeken3_am_2018_zscore)
JOMAG_predictores_criteria_merged$RTeken4_am_2018_zscore<-as.numeric(JOMAG_predictores_criteria_merged$RTeken4_am_2018_zscore)
JOMAG_predictores_criteria_merged$RTeken5_am_2018_zscore<-as.numeric(JOMAG_predictores_criteria_merged$RTeken5_am_2018_zscore)
JOMAG_predictores_criteria_merged$RTeken_am_2018<-NA
JOMAG_predictores_criteria_merged$RTeken_am_2018_nna<-NA
JOMAG_predictores_criteria_merged$RTeken_am_2018_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
  JOMAG_predictores_criteria_merged[i,]$RTeken_am_2018<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],
                                                             select=c(RTeken1_am_2018_zscore,RTeken2_am_2018_zscore,RTeken3_am_2018_zscore,RTeken4_am_2018_zscore,RTeken5_am_2018_zscore)),na.rm=T)
  JOMAG_predictores_criteria_merged[i,]$RTeken_am_2018_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],
                                                                      select=c(RTeken1_am_2018_zscore,RTeken2_am_2018_zscore,RTeken3_am_2018_zscore,RTeken4_am_2018_zscore,RTeken5_am_2018_zscore))))
  JOMAG_predictores_criteria_merged[i,]$RTeken_am_2018_pctna<-round(JOMAG_predictores_criteria_merged[i,]$RTeken_am_2018_nna/5*100,2)
}

# am_2018/am_2018_special (it appears in the am_regular file, but has only NPct vars. Ronen: am special were only since am_2015)
zscore<-function(x,mean,sd){
  (x-mean)/sd
}
JOMAG_predictores_criteria_merged$NPct1_am_2018_special_zscore<-NA
JOMAG_predictores_criteria_merged$NPct2_am_2018_special_zscore<-NA
JOMAG_predictores_criteria_merged$NPct3_am_2018_special_zscore<-NA
JOMAG_predictores_criteria_merged$NPct4_am_2018_special_zscore<-NA
JOMAG_predictores_criteria_merged$NPct1_am_2018_zscore<-NA
JOMAG_predictores_criteria_merged$NPct2_am_2018_zscore<-NA
JOMAG_predictores_criteria_merged$NPct3_am_2018_zscore<-NA
JOMAG_predictores_criteria_merged$NPct4_am_2018_zscore<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
    if(is.na(JOMAG_predictores_criteria_merged[i,]$RAvg_am_2018) & (!is.na(JOMAG_predictores_criteria_merged[i,]$NPct1_am_2018) | !is.na(JOMAG_predictores_criteria_merged[i,]$NPct2_am_2018) | !is.na(JOMAG_predictores_criteria_merged[i,]$NPct3_am_2018) | !is.na(JOMAG_predictores_criteria_merged[i,]$NPct4_am_2018))){
    JOMAG_predictores_criteria_merged[i,]$NPct1_am_2018_special_zscore<-zscore(as.numeric(unlist(JOMAG_predictores_criteria_merged[i,]$NPct1_am_2018)),mean(as.numeric(unlist(JOMAG_predictores_criteria_merged$NPct1_am_2018)),na.rm=T),sd(as.numeric(unlist(JOMAG_predictores_criteria_merged$NPct1_am_2018)),na.rm=T))
    JOMAG_predictores_criteria_merged[i,]$NPct2_am_2018_special_zscore<-zscore(as.numeric(unlist(JOMAG_predictores_criteria_merged[i,]$NPct2_am_2018)),mean(as.numeric(unlist(JOMAG_predictores_criteria_merged$NPct2_am_2018)),na.rm=T),sd(as.numeric(unlist(JOMAG_predictores_criteria_merged$NPct2_am_2018)),na.rm=T))
    JOMAG_predictores_criteria_merged[i,]$NPct3_am_2018_special_zscore<-zscore(as.numeric(unlist(JOMAG_predictores_criteria_merged[i,]$NPct3_am_2018)),mean(as.numeric(unlist(JOMAG_predictores_criteria_merged$NPct3_am_2018)),na.rm=T),sd(as.numeric(unlist(JOMAG_predictores_criteria_merged$NPct3_am_2018)),na.rm=T))
    JOMAG_predictores_criteria_merged[i,]$NPct4_am_2018_special_zscore<-zscore(as.numeric(unlist(JOMAG_predictores_criteria_merged[i,]$NPct4_am_2018)),mean(as.numeric(unlist(JOMAG_predictores_criteria_merged$NPct4_am_2018)),na.rm=T),sd(as.numeric(unlist(JOMAG_predictores_criteria_merged$NPct4_am_2018)),na.rm=T))
      }
  
else {   
  
  JOMAG_predictores_criteria_merged[i,]$NPct1_am_2018_zscore<-zscore(as.numeric(unlist(JOMAG_predictores_criteria_merged[i,]$NPct1_am_2018)),mean(as.numeric(unlist(JOMAG_predictores_criteria_merged$NPct1_am_2018)),na.rm=T),sd(as.numeric(unlist(JOMAG_predictores_criteria_merged$NPct1_am_2018)),na.rm=T))
  JOMAG_predictores_criteria_merged[i,]$NPct2_am_2018_zscore<-zscore(as.numeric(unlist(JOMAG_predictores_criteria_merged[i,]$NPct2_am_2018)),mean(as.numeric(unlist(JOMAG_predictores_criteria_merged$NPct2_am_2018)),na.rm=T),sd(as.numeric(unlist(JOMAG_predictores_criteria_merged$NPct2_am_2018)),na.rm=T))
  JOMAG_predictores_criteria_merged[i,]$NPct3_am_2018_zscore<-zscore(as.numeric(unlist(JOMAG_predictores_criteria_merged[i,]$NPct3_am_2018)),mean(as.numeric(unlist(JOMAG_predictores_criteria_merged$NPct3_am_2018)),na.rm=T),sd(as.numeric(unlist(JOMAG_predictores_criteria_merged$NPct3_am_2018)),na.rm=T))
  JOMAG_predictores_criteria_merged[i,]$NPct4_am_2018_zscore<-zscore(as.numeric(unlist(JOMAG_predictores_criteria_merged[i,]$NPct4_am_2018)),mean(as.numeric(unlist(JOMAG_predictores_criteria_merged$NPct4_am_2018)),na.rm=T),sd(as.numeric(unlist(JOMAG_predictores_criteria_merged$NPct4_am_2018)),na.rm=T))
  }
}

nacol_before_NPct_am_2018_outliers<-ncol(JOMAG_predictores_criteria_merged)


# Locating and handling NPct_am_2018 outliers.
JOMAG_predictores_criteria_merged_outliers_relevant_columns <- colnames(JOMAG_predictores_criteria_merged[c((ncol(JOMAG_predictores_criteria_merged)-7)):ncol(JOMAG_predictores_criteria_merged)]) 

for(i in JOMAG_predictores_criteria_merged_outliers_relevant_columns) {
  JOMAG_predictores_criteria_merged[ncol(JOMAG_predictores_criteria_merged)+1]<-NA
  names(JOMAG_predictores_criteria_merged)[ncol(JOMAG_predictores_criteria_merged)]<-paste(i,"outlier",sep = "_")
  JOMAG_predictores_criteria_merged[ncol(JOMAG_predictores_criteria_merged)]<-as.numeric(unlist(JOMAG_predictores_criteria_merged[ncol(JOMAG_predictores_criteria_merged)]))
  for(j in 1:nrow(JOMAG_predictores_criteria_merged)){
    if (!is.na(JOMAG_predictores_criteria_merged[j,][i])) {
      JOMAG_predictores_criteria_merged[j,][ncol(JOMAG_predictores_criteria_merged)] <-
        ifelse(abs(JOMAG_predictores_criteria_merged[j,][i])>3.29,JOMAG_predictores_criteria_merged[j,][i],NA)
    }
  }
  if (all(is.na(JOMAG_predictores_criteria_merged[ncol(JOMAG_predictores_criteria_merged)]))) {
    JOMAG_predictores_criteria_merged<-JOMAG_predictores_criteria_merged[,-ncol(JOMAG_predictores_criteria_merged)]
  }
}

nacol_after_NPct_am_2018_outliers<-ncol(JOMAG_predictores_criteria_merged)

while (nacol_after_NPct_am_2018_outliers!=nacol_before_NPct_am_2018_outliers) {

# freq of NPct_am_2018 outliers.
library(descr)
library(psych)
  
  mode<-function(X)
  {
    temp<-table (as.vector(X))
    names (temp)[temp==max(temp)]
  }
  options(width = 71,max.print=30000)
  # # The 2 commands after the first command, are for cleaning the output file.
  JOMAG_predictores_criteria_merged_freq_relevant_columns<-
    colnames(JOMAG_predictores_criteria_merged[c((nacol_before_NPct_am_2018_outliers+1):nacol_after_NPct_am_2018_outliers)])
  out<-""
  cat("", out, file="Q:/04_Mehkar/18_asher/Junior officers MAGAV validation/NPct_am_2018 outliers.txt", sep="", append=F,fill = T)
  suppressWarnings(for(i in JOMAG_predictores_criteria_merged_freq_relevant_columns) {
    newresult1<-round(freq(ordered(as.numeric(unlist(JOMAG_predictores_criteria_merged[[i]]))), plot = F,main=colnames(JOMAG_predictores_criteria_merged[i]),font=2),2)
    newresult2<-round(describe(as.numeric(unlist(JOMAG_predictores_criteria_merged[[i]]))),2)
    newresult3<-"mode="
    newresult4<-mode(JOMAG_predictores_criteria_merged[[i]])
    newresult5<- "                                                                                               "
    newresult6<- "----------------------------------------------------------------------------"
    out <- capture.output(newresult1,newresult5,newresult2,newresult3,newresult4,newresult5,newresult6)
    out[1]<-""
    cat(colnames(JOMAG_predictores_criteria_merged[i]),out, file="Q:/04_Mehkar/18_asher/Junior officers MAGAV validation/NPct_am_2018_outliers.txt", append=T,fill = T)
  })
  JOMAG_predictores_criteria_merged[JOMAG_predictores_criteria_merged_outliers_relevant_columns][abs(JOMAG_predictores_criteria_merged[JOMAG_predictores_criteria_merged_outliers_relevant_columns])>4]<-NA
  break()
}

JOMAG_predictores_criteria_merged$NPct1_am_2018_zscore<-as.numeric(JOMAG_predictores_criteria_merged$NPct1_am_2018_zscore)
JOMAG_predictores_criteria_merged$NPct2_am_2018_zscore<-as.numeric(JOMAG_predictores_criteria_merged$NPct2_am_2018_zscore)
JOMAG_predictores_criteria_merged$NPct3_am_2018_zscore<-as.numeric(JOMAG_predictores_criteria_merged$NPct3_am_2018_zscore)
JOMAG_predictores_criteria_merged$NPct4_am_2018_zscore<-as.numeric(JOMAG_predictores_criteria_merged$NPct4_am_2018_zscore)
JOMAG_predictores_criteria_merged$NPct_am_2018<-NA
JOMAG_predictores_criteria_merged$NPct_am_2018_nna<-NA
JOMAG_predictores_criteria_merged$NPct_am_2018_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
  JOMAG_predictores_criteria_merged[i,]$NPct_am_2018<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],
                                                           select=c(NPct1_am_2018_zscore,NPct2_am_2018_zscore,NPct3_am_2018_zscore,NPct4_am_2018_zscore)),na.rm=T)
  JOMAG_predictores_criteria_merged[i,]$NPct_am_2018_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],
                                                                    select=c(NPct1_am_2018_zscore,NPct2_am_2018_zscore,NPct3_am_2018_zscore,NPct4_am_2018_zscore))))
  JOMAG_predictores_criteria_merged[i,]$NPct_am_2018_pctna<-round(JOMAG_predictores_criteria_merged[i,]$NPct_am_2018_nna/4*100,2)
}

JOMAG_predictores_criteria_merged$NPct1_am_2018_special_zscore<-as.numeric(JOMAG_predictores_criteria_merged$NPct1_am_2018_special_zscore)
JOMAG_predictores_criteria_merged$NPct2_am_2018_special_zscore<-as.numeric(JOMAG_predictores_criteria_merged$NPct2_am_2018_special_zscore)
JOMAG_predictores_criteria_merged$NPct3_am_2018_special_zscore<-as.numeric(JOMAG_predictores_criteria_merged$NPct3_am_2018_special_zscore)
JOMAG_predictores_criteria_merged$NPct4_am_2018_special_zscore<-as.numeric(JOMAG_predictores_criteria_merged$NPct4_am_2018_special_zscore)
JOMAG_predictores_criteria_merged$NPct_am_2018_special<-NA
JOMAG_predictores_criteria_merged$NPct_am_2018_special_nna<-NA
JOMAG_predictores_criteria_merged$NPct_am_2018_special_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
  JOMAG_predictores_criteria_merged[i,]$NPct_am_2018_special<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],
                                                                   select=c(NPct1_am_2018_special_zscore,NPct2_am_2018_special_zscore,NPct3_am_2018_special_zscore,NPct4_am_2018_special_zscore)),na.rm=T)
  JOMAG_predictores_criteria_merged[i,]$NPct_am_2018_special_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],
                                                                            select=c(NPct1_am_2018_special_zscore,NPct2_am_2018_special_zscore,NPct3_am_2018_special_zscore,NPct4_am_2018_special_zscore))))
  JOMAG_predictores_criteria_merged[i,]$NPct_am_2018_special_pctna<-round(JOMAG_predictores_criteria_merged[i,]$NPct_am_2018_special_nna/4*100,2)
}

JOMAG_predictores_criteria_merged$FAvg1_cf_2018_zscore<-as.numeric(JOMAG_predictores_criteria_merged$memusa_hanhaga_cf_2018_zscore)
JOMAG_predictores_criteria_merged$FAvg2_cf_2018_zscore<-as.numeric(JOMAG_predictores_criteria_merged$memusa_irgun_cf_2018_zscore)
JOMAG_predictores_criteria_merged$FAvg3_cf_2018_zscore<-as.numeric(JOMAG_predictores_criteria_merged$memusa_mikz_cf_2018_zscore)
JOMAG_predictores_criteria_merged$FAvg4_cf_2018_zscore<-as.numeric(JOMAG_predictores_criteria_merged$memusa_enosh_cf_2018_zscore)
JOMAG_predictores_criteria_merged$FAvg5_cf_2018_zscore<-as.numeric(JOMAG_predictores_criteria_merged$memusa_aminut_cf_2018_zscore)
JOMAG_predictores_criteria_merged$FAvg6_cf_2018_zscore<-as.numeric(JOMAG_predictores_criteria_merged$memusa_sheruti_cf_2018_zscore)
JOMAG_predictores_criteria_merged$FAvg7_cf_2018_zscore<-as.numeric(JOMAG_predictores_criteria_merged$memusa_clali_cf_2018_zscore)
JOMAG_predictores_criteria_merged$FAvg_cf_2018<-NA
JOMAG_predictores_criteria_merged$FAvg_cf_2018_nna<-NA
JOMAG_predictores_criteria_merged$FAvg_cf_2018_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
  JOMAG_predictores_criteria_merged[i,]$FAvg_cf_2018<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],
                    select=c(FAvg1_cf_2018_zscore,FAvg2_cf_2018_zscore,FAvg3_cf_2018_zscore,FAvg4_cf_2018_zscore,FAvg5_cf_2018_zscore,
                             FAvg6_cf_2018_zscore,FAvg7_cf_2018_zscore)),na.rm=T)
  JOMAG_predictores_criteria_merged[i,]$FAvg_cf_2018_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],
                         select=c(FAvg1_cf_2018_zscore,FAvg2_cf_2018_zscore,FAvg3_cf_2018_zscore,FAvg4_cf_2018_zscore,FAvg5_cf_2018_zscore,
                                  FAvg6_cf_2018_zscore,FAvg7_cf_2018_zscore))))
  JOMAG_predictores_criteria_merged[i,]$FAvg_cf_2018_pctna<-round(JOMAG_predictores_criteria_merged[i,]$FAvg_cf_2018_nna/7*100,2)
}

JOMAG_predictores_criteria_merged$FTeken1_cf_2018_zscore<-as.numeric(JOMAG_predictores_criteria_merged$metuknan_hanhaga_cf_2018_zscore)
JOMAG_predictores_criteria_merged$FTeken2_cf_2018_zscore<-as.numeric(JOMAG_predictores_criteria_merged$metuknan_irgun_cf_2018_zscore)
JOMAG_predictores_criteria_merged$FTeken3_cf_2018_zscore<-as.numeric(JOMAG_predictores_criteria_merged$metuknan_mikz_cf_2018_zscore)
JOMAG_predictores_criteria_merged$FTeken4_cf_2018_zscore<-as.numeric(JOMAG_predictores_criteria_merged$metuknan_enosh_cf_2018_zscore)
JOMAG_predictores_criteria_merged$FTeken5_cf_2018_zscore<-as.numeric(JOMAG_predictores_criteria_merged$metuknan_aminut_cf_2018_zscore)
JOMAG_predictores_criteria_merged$FTeken6_cf_2018_zscore<-as.numeric(JOMAG_predictores_criteria_merged$metuknan_sheruti_cf_2018_zscore)
JOMAG_predictores_criteria_merged$FTeken7_cf_2018_zscore<-as.numeric(JOMAG_predictores_criteria_merged$metuknan_clali_cf_2018_zscore)
JOMAG_predictores_criteria_merged$FTeken_cf_2018<-NA
JOMAG_predictores_criteria_merged$FTeken_cf_2018_nna<-NA
JOMAG_predictores_criteria_merged$FTeken_cf_2018_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
  JOMAG_predictores_criteria_merged[i,]$FTeken_cf_2018<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],
                    select=c(FTeken1_cf_2018_zscore,FTeken2_cf_2018_zscore,FTeken3_cf_2018_zscore,FTeken4_cf_2018_zscore,FTeken5_cf_2018_zscore,
                             FTeken6_cf_2018_zscore,FTeken7_cf_2018_zscore)),na.rm=T)
  JOMAG_predictores_criteria_merged[i,]$FTeken_cf_2018_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],
                         select=c(FTeken1_cf_2018_zscore,FTeken2_cf_2018_zscore,FTeken3_cf_2018_zscore,FTeken4_cf_2018_zscore,FTeken5_cf_2018_zscore,
                                  FTeken6_cf_2018_zscore,FTeken7_cf_2018_zscore))))
  JOMAG_predictores_criteria_merged[i,]$FTeken_cf_2018_pctna<-round(JOMAG_predictores_criteria_merged[i,]$FTeken_cf_2018_nna/7*100,2)
}

JOMAG_predictores_criteria_merged$RAvg_am_2010<-as.numeric(JOMAG_predictores_criteria_merged$RAvg_am_2010)
JOMAG_predictores_criteria_merged$RTeken_am_2010<-as.numeric(JOMAG_predictores_criteria_merged$RTeken_am_2010)
JOMAG_predictores_criteria_merged$NPct_am_2010<-as.numeric(JOMAG_predictores_criteria_merged$NPct_am_2010)
JOMAG_predictores_criteria_merged$am_2010<-NA
JOMAG_predictores_criteria_merged$am_2010_nna<-NA
JOMAG_predictores_criteria_merged$am_2010_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
  JOMAG_predictores_criteria_merged[i,]$am_2010<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],
                    select=c(RAvg_am_2010,RTeken_am_2010,NPct_am_2010)),na.rm=T)
  JOMAG_predictores_criteria_merged[i,]$am_2010_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],
                         select=c(RAvg_am_2010,RTeken_am_2010,NPct_am_2010))))
  JOMAG_predictores_criteria_merged[i,]$am_2010_pctna<-round(JOMAG_predictores_criteria_merged[i,]$am_2010_nna/3*100,2)
}

JOMAG_predictores_criteria_merged$RAvg_am_2012<-as.numeric(JOMAG_predictores_criteria_merged$RAvg_am_2012)
JOMAG_predictores_criteria_merged$RTeken_am_2012<-as.numeric(JOMAG_predictores_criteria_merged$RTeken_am_2012)
JOMAG_predictores_criteria_merged$NPct_am_2012<-as.numeric(JOMAG_predictores_criteria_merged$NPct_am_2012)
JOMAG_predictores_criteria_merged$am_2012<-NA
JOMAG_predictores_criteria_merged$am_2012_nna<-NA
JOMAG_predictores_criteria_merged$am_2012_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
  JOMAG_predictores_criteria_merged[i,]$am_2012<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],
                                                      select=c(RAvg_am_2012,RTeken_am_2012,NPct_am_2012)),na.rm=T)
  JOMAG_predictores_criteria_merged[i,]$am_2012_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],
                                                               select=c(RAvg_am_2012,RTeken_am_2012,NPct_am_2012))))
  JOMAG_predictores_criteria_merged[i,]$am_2012_pctna<-round(JOMAG_predictores_criteria_merged[i,]$am_2012_nna/3*100,2)
}

JOMAG_predictores_criteria_merged$RAvg_am_2015<-as.numeric(JOMAG_predictores_criteria_merged$RAvg_am_2015)
JOMAG_predictores_criteria_merged$RTeken_am_2015<-as.numeric(JOMAG_predictores_criteria_merged$RTeken_am_2015)
JOMAG_predictores_criteria_merged$NPct_am_2015<-as.numeric(JOMAG_predictores_criteria_merged$NPct_am_2015)
JOMAG_predictores_criteria_merged$am_2015<-NA
JOMAG_predictores_criteria_merged$am_2015_nna<-NA
JOMAG_predictores_criteria_merged$am_2015_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
  JOMAG_predictores_criteria_merged[i,]$am_2015<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],
                    select=c(RAvg_am_2015,RTeken_am_2015,NPct_am_2015)),na.rm=T)
  JOMAG_predictores_criteria_merged[i,]$am_2015_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],
                         select=c(RAvg_am_2015,RTeken_am_2015,NPct_am_2015))))
  JOMAG_predictores_criteria_merged[i,]$am_2015_pctna<-round(JOMAG_predictores_criteria_merged[i,]$am_2015_nna/3*100,2)
}

JOMAG_predictores_criteria_merged$NPct_am_2015_special<-as.numeric(JOMAG_predictores_criteria_merged$NPct_am_2015_special)
JOMAG_predictores_criteria_merged$am_2015_special<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
  JOMAG_predictores_criteria_merged[i,]$am_2015_special<-JOMAG_predictores_criteria_merged[i,]$NPct_am_2015_special
}

JOMAG_predictores_criteria_merged$RAvg_am_2018<-as.numeric(JOMAG_predictores_criteria_merged$RAvg_am_2018)
JOMAG_predictores_criteria_merged$RTeken_am_2018<-as.numeric(JOMAG_predictores_criteria_merged$RTeken_am_2018)
JOMAG_predictores_criteria_merged$NPct_am_2018<-as.numeric(JOMAG_predictores_criteria_merged$NPct_am_2018)
JOMAG_predictores_criteria_merged$am_2018<-NA
JOMAG_predictores_criteria_merged$am_2018_nna<-NA
JOMAG_predictores_criteria_merged$am_2018_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
  JOMAG_predictores_criteria_merged[i,]$am_2018<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],
                    select=c(RAvg_am_2018,RTeken_am_2018,NPct_am_2018)),na.rm=T)
  rowMeans(subset(JOMAG_predictores_criteria_merged[i,],
                  select=c(RAvg_am_2018,RTeken_am_2018,NPct_am_2018)),na.rm=T)
  JOMAG_predictores_criteria_merged[i,]$am_2018_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],
                         select=c(RAvg_am_2018,RTeken_am_2018,NPct_am_2018))))
  JOMAG_predictores_criteria_merged[i,]$am_2018_pctna<-round(JOMAG_predictores_criteria_merged[i,]$am_2018_nna/3*100,2)
}

JOMAG_predictores_criteria_merged$NPct_am_2018_special<-as.numeric(JOMAG_predictores_criteria_merged$NPct_am_2018_special)
JOMAG_predictores_criteria_merged$am_2018_special<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
  JOMAG_predictores_criteria_merged[i,]$am_2018_special<-JOMAG_predictores_criteria_merged[i,]$NPct_am_2018_special
}

# am_2015_regular_special
JOMAG_predictores_criteria_merged$am_2015<-as.numeric(JOMAG_predictores_criteria_merged$am_2015)
JOMAG_predictores_criteria_merged$am_2015_special<-as.numeric(JOMAG_predictores_criteria_merged$am_2015_special)
JOMAG_predictores_criteria_merged$am_2015_regular_special<-NA
JOMAG_predictores_criteria_merged$am_2015_regular_special_nna<-NA
JOMAG_predictores_criteria_merged$am_2015_regular_special_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
  JOMAG_predictores_criteria_merged[i,]$am_2015_regular_special<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],select=c(am_2015,am_2015_special)),na.rm=T)
  JOMAG_predictores_criteria_merged[i,]$am_2015_regular_special_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],select=c(am_2015,am_2015_special))))
  JOMAG_predictores_criteria_merged[i,]$am_2015_regular_special_pctna<-round(JOMAG_predictores_criteria_merged[i,]$am_2015_regular_special_nna/2*100,2)
}

# am_2018_regular_special
JOMAG_predictores_criteria_merged$am_2018<-as.numeric(JOMAG_predictores_criteria_merged$am_2018)
JOMAG_predictores_criteria_merged$am_2018_special<-as.numeric(JOMAG_predictores_criteria_merged$am_2018_special)
JOMAG_predictores_criteria_merged$am_2018_regular_special<-NA
JOMAG_predictores_criteria_merged$am_2018_regular_special_nna<-NA
JOMAG_predictores_criteria_merged$am_2018_regular_special_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
  JOMAG_predictores_criteria_merged[i,]$am_2018_regular_special<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],select=c(am_2018,am_2018_special)),na.rm=T)
  JOMAG_predictores_criteria_merged[i,]$am_2018_regular_special_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],select=c(am_2018,am_2018_special))))
  JOMAG_predictores_criteria_merged[i,]$am_2018_regular_special_pctna<-round(JOMAG_predictores_criteria_merged[i,]$am_2018_regular_special_nna/2*100,2)
}

JOMAG_predictores_criteria_merged$PAvg_cf_2010<-as.numeric(JOMAG_predictores_criteria_merged$PAvg_cf_2010)
JOMAG_predictores_criteria_merged$FAvg_cf_2010<-as.numeric(JOMAG_predictores_criteria_merged$FAvg_cf_2010)
JOMAG_predictores_criteria_merged$FTeken_cf_2010<-as.numeric(JOMAG_predictores_criteria_merged$FTeken_cf_2010)
JOMAG_predictores_criteria_merged$cf_2010<-NA
JOMAG_predictores_criteria_merged$cf_2010_nna<-NA
JOMAG_predictores_criteria_merged$cf_2010_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
  JOMAG_predictores_criteria_merged[i,]$cf_2010<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],
                    select=c(PAvg_cf_2010,FAvg_cf_2010,FTeken_cf_2010)),na.rm=T)
  JOMAG_predictores_criteria_merged[i,]$cf_2010_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],
                         select=c(PAvg_cf_2010,FAvg_cf_2010,FTeken_cf_2010))))
  JOMAG_predictores_criteria_merged[i,]$cf_2010_pctna<-round(JOMAG_predictores_criteria_merged[i,]$cf_2010_nna/3*100,2)
}

JOMAG_predictores_criteria_merged$PAvg_cf_2012<-as.numeric(JOMAG_predictores_criteria_merged$PAvg_cf_2012)
JOMAG_predictores_criteria_merged$FAvg_cf_2012<-as.numeric(JOMAG_predictores_criteria_merged$FAvg_cf_2012)
JOMAG_predictores_criteria_merged$FTeken_cf_2012<-as.numeric(JOMAG_predictores_criteria_merged$FTeken_cf_2012)
JOMAG_predictores_criteria_merged$cf_2012<-NA
JOMAG_predictores_criteria_merged$cf_2012_nna<-NA
JOMAG_predictores_criteria_merged$cf_2012_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
  JOMAG_predictores_criteria_merged[i,]$cf_2012<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],
                    select=c(PAvg_cf_2012,FAvg_cf_2012,FTeken_cf_2012)),na.rm=T)
  JOMAG_predictores_criteria_merged[i,]$cf_2012_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],
                         select=c(PAvg_cf_2012,FAvg_cf_2012,FTeken_cf_2012))))
  JOMAG_predictores_criteria_merged[i,]$cf_2012_pctna<-round(JOMAG_predictores_criteria_merged[i,]$cf_2012_nna/3*100,2)
}

JOMAG_predictores_criteria_merged$FAvg_cf_2015<-as.numeric(JOMAG_predictores_criteria_merged$FAvg_cf_2015)
JOMAG_predictores_criteria_merged$FTeken_cf_2015<-as.numeric(JOMAG_predictores_criteria_merged$FTeken_cf_2015)
JOMAG_predictores_criteria_merged$cf_2015<-NA
JOMAG_predictores_criteria_merged$cf_2015_nna<-NA
JOMAG_predictores_criteria_merged$cf_2015_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
  JOMAG_predictores_criteria_merged[i,]$cf_2015<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],
                    select=c(FAvg_cf_2015,FTeken_cf_2015)),na.rm=T)
  JOMAG_predictores_criteria_merged[i,]$cf_2015_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],
                         select=c(FAvg_cf_2015,FTeken_cf_2015))))
  JOMAG_predictores_criteria_merged[i,]$cf_2015_pctna<-round(JOMAG_predictores_criteria_merged[i,]$cf_2015_nna/2*100,2)
}

JOMAG_predictores_criteria_merged$FAvg_cf_2018<-as.numeric(JOMAG_predictores_criteria_merged$FAvg_cf_2018)
JOMAG_predictores_criteria_merged$FTeken_cf_2018<-as.numeric(JOMAG_predictores_criteria_merged$FTeken_cf_2018)
JOMAG_predictores_criteria_merged$cf_2018<-NA
JOMAG_predictores_criteria_merged$cf_2018_nna<-NA
JOMAG_predictores_criteria_merged$cf_2018_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
  JOMAG_predictores_criteria_merged[i,]$cf_2018<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],
                    select=c(FAvg_cf_2018,FTeken_cf_2018)),na.rm=T)
  JOMAG_predictores_criteria_merged[i,]$cf_2018_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],
                         select=c(FAvg_cf_2018,FTeken_cf_2018))))
  JOMAG_predictores_criteria_merged[i,]$cf_2018_pctna<-round(JOMAG_predictores_criteria_merged[i,]$cf_2018_nna/2*100,2)
}

End_time<-Sys.time()
Total_time<- round(End_time-Start_time, digits = 2)
Total_time
# Total_time=39.77 mins

library(sendmailR)
from <- sprintf("<sendmailR@%s>", Sys.info()[4])
to <- "<xxxxx>"
subject <- "JOMAG session has been completed"
body <- list(paste("\nLength of time to run the code: ", Total_time,"."))
sendmail(from, to, subject, body,control=list(smtpServer="xxxxx"))

# Compute VaadaGrade for some of the officers that missed that score.
JOMAG_predictores_criteria_merged$officer<-ifelse(is.na(JOMAG_predictores_criteria_merged$darga_date),0,1)

library(descr)
library(psych)
freq(ordered(JOMAG_predictores_criteria_merged$officer), plot = F,main=colnames(JOMAG_predictores_criteria_merged$officer),font=2)

table(JOMAG_predictores_criteria_merged$FileGrade,JOMAG_predictores_criteria_merged$VaadaGrade,JOMAG_predictores_criteria_merged$officer,
      dnn=c("FileGrade","VaadaGrade","officer"))

JOMAG_predictores_criteria_merged$VaadaGrade<-ifelse(is.na(JOMAG_predictores_criteria_merged$VaadaGrade) & 
                                                     JOMAG_predictores_criteria_merged$officer==1 & 
                                                     JOMAG_predictores_criteria_merged$FileGrade==3.5,4,JOMAG_predictores_criteria_merged$VaadaGrade)

table(JOMAG_predictores_criteria_merged$FileGrade,JOMAG_predictores_criteria_merged$VaadaGrade,JOMAG_predictores_criteria_merged$officer,
      dnn=c("FileGrade","VaadaGrade","officer"))


# Add soc_officers_magav_hachana
soc_officers_magav_hachana<-read.csv("Q:/04_Mehkar/18_asher/Junior officers MAGAV validation/Files from Ronen/soc_officers_magav_hachana.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
colnames(soc_officers_magav_hachana)[12]<-"personal_number"
colnames(soc_officers_magav_hachana)<-paste(colnames(soc_officers_magav_hachana),"officerssochac",sep="_")
colnames(soc_officers_magav_hachana)[13]<-"id"

soc_officers_magav_hachana$RTeken1_officerssochac<-as.numeric(soc_officers_magav_hachana$RTeken1_officerssochac)
soc_officers_magav_hachana$RTeken2_officerssochac<-as.numeric(soc_officers_magav_hachana$RTeken2_officerssochac)
soc_officers_magav_hachana$RTeken3_officerssochac<-as.numeric(soc_officers_magav_hachana$RTeken3_officerssochac)
soc_officers_magav_hachana$RTeken4_officerssochac<-as.numeric(soc_officers_magav_hachana$RTeken4_officerssochac)
soc_officers_magav_hachana$RTeken5_officerssochac<-as.numeric(soc_officers_magav_hachana$RTeken5_officerssochac)

# The transformation was done in order to return the RTeken vars to their original state as standard scores.
soc_officers_magav_hachana$RTeken1_officerssochac<-(soc_officers_magav_hachana$RTeken1_officerssochac-70)/10
soc_officers_magav_hachana$RTeken2_officerssochac<-(soc_officers_magav_hachana$RTeken2_officerssochac-70)/10
soc_officers_magav_hachana$RTeken3_officerssochac<-(soc_officers_magav_hachana$RTeken3_officerssochac-70)/10
soc_officers_magav_hachana$RTeken4_officerssochac<-(soc_officers_magav_hachana$RTeken4_officerssochac-70)/10
soc_officers_magav_hachana$RTeken5_officerssochac<-(soc_officers_magav_hachana$RTeken5_officerssochac-70)/10

soc_officers_magav_hachana$RTeken_officerssochac<-NA
soc_officers_magav_hachana$RTeken_officerssochac_nna<-NA
soc_officers_magav_hachana$RTeken_officerssochac_pctna<-NA
for(i in 1:nrow(soc_officers_magav_hachana)){
  soc_officers_magav_hachana[i,]$RTeken_officerssochac<-rowMeans(subset(soc_officers_magav_hachana[i,],
                                                                        select=c(RTeken1_officerssochac,RTeken2_officerssochac,RTeken3_officerssochac,RTeken4_officerssochac,RTeken5_officerssochac)),na.rm=T)
  soc_officers_magav_hachana[i,]$RTeken_officerssochac_nna<-rowSums(is.na(subset(soc_officers_magav_hachana[i,],
                                                                                 select=c(RTeken1_officerssochac,RTeken2_officerssochac,RTeken3_officerssochac,RTeken4_officerssochac,RTeken5_officerssochac))))
  soc_officers_magav_hachana[i,]$RTeken_officerssochac_pctna<-round(soc_officers_magav_hachana[i,]$RTeken_officerssochac_nna/5*100,2)
}

# Next command is because NPct2 is for aggression (NPct1 is for success on job)
soc_officers_magav_hachana$NPct2_officerssochac<-100-soc_officers_magav_hachana$NPct2_officerssochac

soc_officers_magav_hachana$NPct1_officerssochac_zscore<-as.data.frame(scale(as.numeric(unlist(soc_officers_magav_hachana$NPct1))))
soc_officers_magav_hachana$NPct2_officerssochac_zscore<-as.data.frame(scale(as.numeric(unlist(soc_officers_magav_hachana$NPct2))))

soc_officers_magav_hachana$NPct1_officerssochac_zscore<-as.numeric(unlist(soc_officers_magav_hachana$NPct1_officerssochac_zscore))
soc_officers_magav_hachana$NPct2_officerssochac_zscore<-as.numeric(unlist(soc_officers_magav_hachana$NPct2_officerssochac_zscore))
soc_officers_magav_hachana$NPct_officerssochac<-NA
soc_officers_magav_hachana$NPct_officerssochac_nna<-NA
soc_officers_magav_hachana$NPct_officerssochac_pctna<-NA
for(i in 1:nrow(soc_officers_magav_hachana)){
  soc_officers_magav_hachana[i,]$NPct_officerssochac<-rowMeans(subset(soc_officers_magav_hachana[i,],
                                                                      select=c(NPct1_officerssochac_zscore,NPct2_officerssochac_zscore)),na.rm=T)
  soc_officers_magav_hachana[i,]$NPct_officerssochac_nna<-rowSums(is.na(subset(soc_officers_magav_hachana[i,],
                                                                               select=c(NPct1_officerssochac_zscore,NPct2_officerssochac_zscore))))
  soc_officers_magav_hachana[i,]$NPct_officerssochac_pctna<-round(soc_officers_magav_hachana[i,]$NPct_officerssochac_nna/2*100,2)
}

soc_officers_magav_hachana$RTeken_officerssochac<-as.numeric(soc_officers_magav_hachana$RTeken_officerssochac)
soc_officers_magav_hachana$NPct_officerssochac<-as.numeric(soc_officers_magav_hachana$NPct_officerssochac)
soc_officers_magav_hachana$officerssochach<-NA
soc_officers_magav_hachana$officerssochac_nna<-NA
soc_officers_magav_hachana$officerssochac_pctna<-NA
for(i in 1:nrow(soc_officers_magav_hachana)){
  soc_officers_magav_hachana[i,]$officerssochach<-rowMeans(subset(soc_officers_magav_hachana[i,],
                                                                  select=c(RTeken_officerssochac,NPct_officerssochac)),na.rm=T)
  soc_officers_magav_hachana[i,]$officerssochac_nna<-rowSums(is.na(subset(soc_officers_magav_hachana[i,],
                                                                          select=c(RTeken_officerssochac,NPct_officerssochac))))
  soc_officers_magav_hachana[i,]$officerssochac_pctna<-round(soc_officers_magav_hachana[i,]$officerssochac_nna/2*100,2)
}

n_occur<-data.frame(table(soc_officers_magav_hachana$id))
n_occur[n_occur$Freq>1,]

# Ronen: this time we treat the duplicates problem as following.However on the next time compute average of evaluations,
# because in officers course maybe there are realy multiple sociometric evaluations.
soc_officers_magav_hachana<-setDT(soc_officers_magav_hachana)[,.SD[which.max(GroupId_officerssochac)],keyby=id]
n_occur<-data.frame(table(soc_officers_magav_hachana$id))
n_occur[n_occur$Freq>1,]

JOMAG_predictores_criteria_merged <- merge(JOMAG_predictores_criteria_merged,soc_officers_magav_hachana,by=c("id"), all.x=T, all.y=F,sort = FALSE)

# Add soc_officers_magav_hashlama
soc_officers_magav_hashlama<-read.csv("Q:/04_Mehkar/18_asher/Junior officers MAGAV validation/Files from Ronen/soc_officers_magav_hashlama.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
colnames(soc_officers_magav_hashlama)[12]<-"personal_number"
colnames(soc_officers_magav_hashlama)<-paste(colnames(soc_officers_magav_hashlama),"officerssochas",sep="_")
colnames(soc_officers_magav_hashlama)[13]<-"id"

soc_officers_magav_hashlama$RTeken1_officerssochas<-as.numeric(soc_officers_magav_hashlama$RTeken1_officerssochas)
soc_officers_magav_hashlama$RTeken2_officerssochas<-as.numeric(soc_officers_magav_hashlama$RTeken2_officerssochas)
soc_officers_magav_hashlama$RTeken3_officerssochas<-as.numeric(soc_officers_magav_hashlama$RTeken3_officerssochas)
soc_officers_magav_hashlama$RTeken4_officerssochas<-as.numeric(soc_officers_magav_hashlama$RTeken4_officerssochas)
soc_officers_magav_hashlama$RTeken5_officerssochas<-as.numeric(soc_officers_magav_hashlama$RTeken5_officerssochas)

# The transformation was done in order to return the RTeken vars to their original state as standard scores.
soc_officers_magav_hashlama$RTeken1_officerssochas<-(soc_officers_magav_hashlama$RTeken1_officerssochas-70)/10
soc_officers_magav_hashlama$RTeken2_officerssochas<-(soc_officers_magav_hashlama$RTeken2_officerssochas-70)/10
soc_officers_magav_hashlama$RTeken3_officerssochas<-(soc_officers_magav_hashlama$RTeken3_officerssochas-70)/10
soc_officers_magav_hashlama$RTeken4_officerssochas<-(soc_officers_magav_hashlama$RTeken4_officerssochas-70)/10
soc_officers_magav_hashlama$RTeken5_officerssochas<-(soc_officers_magav_hashlama$RTeken5_officerssochas-70)/10

soc_officers_magav_hashlama$RTeken_officerssochas<-NA
soc_officers_magav_hashlama$RTeken_officerssochas_nna<-NA
soc_officers_magav_hashlama$RTeken_officerssochas_pctna<-NA
for(i in 1:nrow(soc_officers_magav_hashlama)){
  soc_officers_magav_hashlama[i,]$RTeken_officerssochas<-rowMeans(subset(soc_officers_magav_hashlama[i,],
                                                                         select=c(RTeken1_officerssochas,RTeken2_officerssochas,RTeken3_officerssochas,RTeken4_officerssochas,RTeken5_officerssochas)),na.rm=T)
  soc_officers_magav_hashlama[i,]$RTeken_officerssochas_nna<-rowSums(is.na(subset(soc_officers_magav_hashlama[i,],
                                                                                  select=c(RTeken1_officerssochas,RTeken2_officerssochas,RTeken3_officerssochas,RTeken4_officerssochas,RTeken5_officerssochas))))
  soc_officers_magav_hashlama[i,]$RTeken_officerssochas_pctna<-round(soc_officers_magav_hashlama[i,]$RTeken_officerssochas_nna/5*100,2)
}

# Next command is because NPct2 is for aggression (NPct1 is for success on job)
soc_officers_magav_hashlama$NPct2_officerssochas<-100-soc_officers_magav_hashlama$NPct2_officerssochas

soc_officers_magav_hashlama$NPct1_officerssochas_zscore<-as.data.frame(scale(as.numeric(unlist(soc_officers_magav_hashlama$NPct1))))
soc_officers_magav_hashlama$NPct2_officerssochas_zscore<-as.data.frame(scale(as.numeric(unlist(soc_officers_magav_hashlama$NPct2))))

soc_officers_magav_hashlama$NPct1_officerssochas_zscore<-as.numeric(unlist(soc_officers_magav_hashlama$NPct1_officerssochas_zscore))
soc_officers_magav_hashlama$NPct2_officerssochas_zscore<-as.numeric(unlist(soc_officers_magav_hashlama$NPct2_officerssochas_zscore))
soc_officers_magav_hashlama$NPct_officerssochas<-NA
soc_officers_magav_hashlama$NPct_officerssochas_nna<-NA
soc_officers_magav_hashlama$NPct_officerssochas_pctna<-NA
for(i in 1:nrow(soc_officers_magav_hashlama)){
  soc_officers_magav_hashlama[i,]$NPct_officerssochas<-rowMeans(subset(soc_officers_magav_hashlama[i,],
                                                                       select=c(NPct1_officerssochas_zscore,NPct2_officerssochas_zscore)),na.rm=T)
  soc_officers_magav_hashlama[i,]$NPct_officerssochas_nna<-rowSums(is.na(subset(soc_officers_magav_hashlama[i,],
                                                                                select=c(NPct1_officerssochas_zscore,NPct2_officerssochas_zscore))))
  soc_officers_magav_hashlama[i,]$NPct_officerssochas_pctna<-round(soc_officers_magav_hashlama[i,]$NPct_officerssochas_nna/2*100,2)
}

soc_officers_magav_hashlama$RTeken_officerssochas<-as.numeric(soc_officers_magav_hashlama$RTeken_officerssochas)
soc_officers_magav_hashlama$NPct_officerssochas<-as.numeric(soc_officers_magav_hashlama$NPct_officerssochas)
soc_officers_magav_hashlama$officerssochas<-NA
soc_officers_magav_hashlama$officerssochas_nna<-NA
soc_officers_magav_hashlama$officerssochas_pctna<-NA
for(i in 1:nrow(soc_officers_magav_hashlama)){
  soc_officers_magav_hashlama[i,]$officerssochas<-rowMeans(subset(soc_officers_magav_hashlama[i,],
                                                                  select=c(RTeken_officerssochas,NPct_officerssochas)),na.rm=T)
  soc_officers_magav_hashlama[i,]$officerssochas_nna<-rowSums(is.na(subset(soc_officers_magav_hashlama[i,],
                                                                           select=c(RTeken_officerssochas,NPct_officerssochas))))
  soc_officers_magav_hashlama[i,]$officerssochas_pctna<-round(soc_officers_magav_hashlama[i,]$officerssochas_nna/2*100,2)
}

n_occur<-data.frame(table(soc_officers_magav_hashlama$id))
n_occur[n_occur$Freq>1,]

# Ronen: this time we treat the duplicates problem as following.However on the next time compute average of evaluations,
# because in officers course maybe there are realy multiple sociometric evaluations.
soc_officers_magav_hashlama<-setDT(soc_officers_magav_hashlama)[,.SD[which.max(GroupId_officerssochas)],keyby=id]
n_occur<-data.frame(table(soc_officers_magav_hashlama$id))
n_occur[n_occur$Freq>1,]

JOMAG_predictores_criteria_merged <- merge(JOMAG_predictores_criteria_merged,soc_officers_magav_hashlama,by=c("id"), all.x=T, all.y=F,sort = FALSE)

# Add missing demographics.

demographics<-read.csv("Q:/04_Mehkar/18_asher/Junior officers MAGAV validation/Files from Shimi/demographics.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")

demographics$gender<-gsub('זכר','1',demographics$gender)

demographics$gender<-gsub('נקבה','2',demographics$gender)

demographics<-demographics[-2]

demographics$unit_date<-as.Date(as.character(demographics$unit_date),format="%d/%m/%Y") 
demographics$final_unit_date<-as.Date(as.character(demographics$final_unit_date),format="%d/%m/%Y")
# in final_unit_date if the year is 9999 it means that the policeman still serves in that unit
demographics$final_unit_date[demographics$final_unit_date=="9999-12-31"]<-"2018-12-31"
demographics$tafkid_date<-as.Date(as.character(demographics$tafkid_date),format="%d/%m/%Y")
demographics$giyus_date<-as.Date(as.character(demographics$giyus_date),format="%d/%m/%Y")
demographics$birth_date<-as.Date(as.character(demographics$birth_date),format="%d/%m/%Y")

# I didn't mrgege directly JOMAG_predictores_criteria_merged with demographics in order not to loose 3 candidates
newvars<-c("id","GibDate")
JOMAG_predictores_criteria_merged_subset<-JOMAG_predictores_criteria_merged[newvars]

demographics_combined <- merge(demographics,JOMAG_predictores_criteria_merged_subset,by=c("id"), all.x=F, all.y=T,sort = FALSE)
demographics_combined$GibDate<-as.Date(demographics_combined$GibDate)
demographics_combined$GibDate_tafkid_date_gap<-as.numeric(demographics_combined$GibDate-demographics_combined$tafkid_date)
demographics_combined$GibDate_unit_date_gap<-as.numeric(demographics_combined$GibDate-demographics_combined$unit_date)
demographics_combined$hirarchic2<-as.character(demographics_combined$hirarchic2)
demographics_combined$hirarchic3<-as.character(demographics_combined$hirarchic3)
demographics_combined$hirarchic4<-as.character(demographics_combined$hirarchic4)
demographics_combined$tafkid<-as.character(demographics_combined$tafkid)
#
demographics_combined$hirarchic3<-gsub('["]','',demographics_combined$hirarchic3)
demographics_combined$tafkid<-gsub('["]','',demographics_combined$tafkid)
demographics_combined$tafkid<-gsub("'","",demographics_combined$tafkid)
#
demographics_combined$bahad_mak<-ifelse(demographics_combined$GibDate_tafkid_date_gap>0 &
                                        demographics_combined$hirarchic3=="בסיס הדרכה מגב בהד" &
                                       (demographics_combined$tafkid=="מפקד כיתה" |
                                        demographics_combined$tafkid=="סמל מחלקה" |
                                        demographics_combined$tafkid=="מדריך מגב"|
                                        demographics_combined$tafkid=="רב סמל פלוגתי"),
                                        1,0)

# if the next command create a warning messege, resturt R and it will run correctly
demographics_combined_list<-split(demographics_combined,f=demographics_combined$id)

suppressWarnings(for (i in 1:length(demographics_combined_list)) {
  demographics_combined_list[[i]]$bahad_mak_sum<-
  sum(demographics_combined_list[[i]]$bahad_mak)
  demographics_combined_list[[i]]<-
  subset(demographics_combined_list[[i]],GibDate_tafkid_date_gap==min(GibDate_tafkid_date_gap[which(GibDate_tafkid_date_gap>0)],na.rm = F) &
  GibDate_unit_date_gap==min(GibDate_unit_date_gap[which(GibDate_unit_date_gap>0)],na.rm = F))
  })

demographics_combined_df<-do.call(rbind,demographics_combined_list)

demographics_combined_df<-demographics_combined_df[-15]

JOMAG_predictores_criteria_merged <- merge(JOMAG_predictores_criteria_merged,demographics_combined_df,by=c("id"), all.x=T, all.y=T,sort = FALSE)

# tafkid=officer!? 301134722, 301075636, 302874763, 201118247 O.K. only officer teken.

# tafkid=mefaked basis!? 34874461, 32041311: It's O.K. (volunteers base).


# add missing gender
JOMAG_predictores_criteria_merged$gender_new<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged)) {
  JOMAG_predictores_criteria_merged[i,]$gender_new<-ifelse(!is.na(JOMAG_predictores_criteria_merged[i,]$gender),JOMAG_predictores_criteria_merged[i,]$gender,
                                                    ifelse(!is.na(JOMAG_predictores_criteria_merged[i,]$Sex),JOMAG_predictores_criteria_merged[i,]$Sex,NA))
}

JOMAG_predictores_criteria_merged$gender<-JOMAG_predictores_criteria_merged$gender_new

JOMAG_predictores_criteria_merged<-JOMAG_predictores_criteria_merged[-1353]

library(sendmailR)
from <- sprintf("<sendmailR@%s>", Sys.info()[4])
to <-"<xxxxx>"
subject <- "JOMAG session has been started"
body <- list(paste(" "))
sendmail(from, to, subject, body,control=list(smtpServer="xxxxx"))

Start_time<-Sys.time()

# Optimal gap between apointment date and criteria.
# Check it by QV system and update accordingly the followig commands before running them.

# All TaarichHavara_am_2010_diff values are negative.
# All TaarichHavara_am_2012_diff values are less then 1 year.

JOMAG_predictores_criteria_merged$am_2015 <- 
ifelse(as.numeric(JOMAG_predictores_criteria_merged$TaarichHavara_am_2015_diff)< 251,NA,JOMAG_predictores_criteria_merged$am_2015)

# JOMAG_predictores_criteria_merged$am_2015_regular_special <- 
# ifelse(JOMAG_predictores_criteria_merged$TaarichHavara_am_2015_diff< 196,NA,JOMAG_predictores_criteria_merged$am_2015_regular_special)

# JOMAG_predictores_criteria_merged$NPct_am_2015 <- 
# ifelse(JOMAG_predictores_criteria_merged$TaarichHavara_am_2015_diff< 196,NA,JOMAG_predictores_criteria_merged$NPct_am_2015)

JOMAG_predictores_criteria_merged$am_2018 <- 
ifelse(as.numeric(JOMAG_predictores_criteria_merged$TaarichHavara_am_2018_diff)< 1220,NA,JOMAG_predictores_criteria_merged$am_2018)

JOMAG_predictores_criteria_merged$am_2018_regular_special <- 
ifelse(as.numeric(JOMAG_predictores_criteria_merged$TaarichHavara_am_2018_diff)< 1220,NA,JOMAG_predictores_criteria_merged$am_2018_regular_special)

JOMAG_predictores_criteria_merged$NPct_am_2018 <- 
ifelse(as.numeric(JOMAG_predictores_criteria_merged$TaarichHavara_am_2018_diff)< 1220,NA,JOMAG_predictores_criteria_merged$NPct_am_2018)

# All TaarichHavara_cf_2010_diff values are negative.

#   JOMAG_predictores_criteria_merged$cf_2012 <- 
#   ifelse(as.numeric(JOMAG_predictores_criteria_merged$TaarichHavara_cf_2012_diff)< 358,NA,JOMAG_predictores_criteria_merged$cf_2012)

#   JOMAG_predictores_criteria_merged$cf_2015 <- 
#   ifelse(as.numeric(JOMAG_predictores_criteria_merged$TaarichHavara_cf_2015_diff)< 306,NA,JOMAG_predictores_criteria_merged$cf_2015)

#   JOMAG_predictores_criteria_merged$cf_2018 <- 
#   ifelse(as.numeric(JOMAG_predictores_criteria_merged$TaarichHavara_cf_2018_diff)< 302,NA,JOMAG_predictores_criteria_merged$cf_2018)

JOMAG_predictores_criteria_merged$tkufatit_14_zscore <- 
ifelse(as.numeric(JOMAG_predictores_criteria_merged$date.tkufatit_14_diff< 362),NA,JOMAG_predictores_criteria_merged$tkufatit_14_zscore)

# Bcause tkufatit_15==period.eval.2015 and the second one has more subjects, it was decided with Ronen to remove the first one from analysis.

JOMAG_predictores_criteria_merged$final.score.2015_zscore <- 
ifelse(as.numeric(JOMAG_predictores_criteria_merged$date.period.eval.2015_diff)< 305,NA,JOMAG_predictores_criteria_merged$final.score.2015_zscore)

JOMAG_predictores_criteria_merged$final.score.2017_zscore <- 
ifelse(as.numeric(JOMAG_predictores_criteria_merged$date.period.eval.2017_diff)< 325,NA,JOMAG_predictores_criteria_merged$final.score.2017_zscore)

JOMAG_predictores_criteria_merged$final.score.2018_zscore <- 
ifelse(as.numeric(JOMAG_predictores_criteria_merged$date.period.eval.2018_diff)< 1662,NA,JOMAG_predictores_criteria_merged$final.score.2018_zscore)


#High order criteria

# NPct_am
JOMAG_predictores_criteria_merged$NPct_am_2015<-as.numeric(JOMAG_predictores_criteria_merged$NPct_am_2015)
JOMAG_predictores_criteria_merged$NPct_am_2018<-as.numeric(JOMAG_predictores_criteria_merged$NPct_am_2018)
JOMAG_predictores_criteria_merged$NPct_am<-NA
JOMAG_predictores_criteria_merged$NPct_am_nna<-NA
JOMAG_predictores_criteria_merged$NPct_am_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
  JOMAG_predictores_criteria_merged[i,]$NPct_am<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],select=c(NPct_am_2015,NPct_am_2018)),na.rm=T)
  JOMAG_predictores_criteria_merged[i,]$NPct_am_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],select=c(NPct_am_2015,NPct_am_2018))))
  JOMAG_predictores_criteria_merged[i,]$NPct_am_pctna<-round(JOMAG_predictores_criteria_merged[i,]$NPct_am_nna/2*100,2)
}

# NPct_am_special
JOMAG_predictores_criteria_merged$NPct_am_2015_special<-as.numeric(JOMAG_predictores_criteria_merged$NPct_am_2015_special)
JOMAG_predictores_criteria_merged$NPct_am_2018_special<-as.numeric(JOMAG_predictores_criteria_merged$NPct_am_2018_special)
JOMAG_predictores_criteria_merged$NPct_am_special<-NA
JOMAG_predictores_criteria_merged$NPct_am_special_nna<-NA
JOMAG_predictores_criteria_merged$NPct_am_special_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
  JOMAG_predictores_criteria_merged[i,]$NPct_am_special<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],select=c(NPct_am_2015_special,NPct_am_2018_special)),na.rm=T)
  JOMAG_predictores_criteria_merged[i,]$NPct_am_special_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],select=c(NPct_am_2015_special,NPct_am_2018_special))))
  JOMAG_predictores_criteria_merged[i,]$NPct_am_special_pctna<-round(JOMAG_predictores_criteria_merged[i,]$NPct_am_special_nna/2*100,2)
}

# tkufatit
JOMAG_predictores_criteria_merged$final.score.2015_zscore<-as.numeric(JOMAG_predictores_criteria_merged$final.score.2015_zscore)
JOMAG_predictores_criteria_merged$final.score.2017_zscore<-as.numeric(JOMAG_predictores_criteria_merged$final.score.2017_zscore)
JOMAG_predictores_criteria_merged$tkufatit_14_zscore<-as.numeric(JOMAG_predictores_criteria_merged$tkufatit_14_zscore)
JOMAG_predictores_criteria_merged$final.score.2018_zscore<-as.numeric(JOMAG_predictores_criteria_merged$final.score.2018_zscore)
JOMAG_predictores_criteria_merged$tkufatit<-NA
JOMAG_predictores_criteria_merged$tkufatit_nna<-NA
JOMAG_predictores_criteria_merged$tkufatit_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
  JOMAG_predictores_criteria_merged[i,]$tkufatit<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],
                    select=c(final.score.2015_zscore,final.score.2017_zscore,tkufatit_14_zscore,final.score.2018_zscore)),na.rm=T)
  JOMAG_predictores_criteria_merged[i,]$tkufatit_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],
                         select=c(final.score.2015_zscore,final.score.2017_zscore,tkufatit_14_zscore,final.score.2018_zscore))))
  JOMAG_predictores_criteria_merged[i,]$tkufatit_pctna<-round(JOMAG_predictores_criteria_merged[i,]$tkufatit_nna/4*100,2)
}

#am
JOMAG_predictores_criteria_merged$am_2015<-as.numeric(JOMAG_predictores_criteria_merged$am_2015)
JOMAG_predictores_criteria_merged$am_2018<-as.numeric(JOMAG_predictores_criteria_merged$am_2018)
JOMAG_predictores_criteria_merged$am<-NA
JOMAG_predictores_criteria_merged$am_nna<-NA
JOMAG_predictores_criteria_merged$am_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
  JOMAG_predictores_criteria_merged[i,]$am<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],select=c(am_2015,am_2018)),na.rm=T)
  JOMAG_predictores_criteria_merged[i,]$am_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],select=c(am_2015,am_2018))))
  JOMAG_predictores_criteria_merged[i,]$am_pctna<-round(JOMAG_predictores_criteria_merged[i,]$am_nna/2*100,2)
}

# am_regular_special
JOMAG_predictores_criteria_merged$am_2015_regular_special<-as.numeric(JOMAG_predictores_criteria_merged$am_2015_regular_special)
JOMAG_predictores_criteria_merged$am_2018_regular_special<-as.numeric(JOMAG_predictores_criteria_merged$am_2018_regular_special)
JOMAG_predictores_criteria_merged$am_regular_special<-NA
JOMAG_predictores_criteria_merged$am_regular_special_nna<-NA
JOMAG_predictores_criteria_merged$am_regular_special_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
  JOMAG_predictores_criteria_merged[i,]$am_regular_special<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],select=c(am_2015_regular_special,am_2018_regular_special)),na.rm=T)
  JOMAG_predictores_criteria_merged[i,]$am_regular_special_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],select=c(am_2015_regular_special,am_2018_regular_special))))
  JOMAG_predictores_criteria_merged[i,]$am_regular_special_pctna<-round(JOMAG_predictores_criteria_merged[i,]$am_regular_special_nna/2*100,2)
}

# #cf
# JOMAG_predictores_criteria_merged$cf_2015<-as.numeric(JOMAG_predictores_criteria_merged$cf_2015)
# JOMAG_predictores_criteria_merged$cf_2018<-as.numeric(JOMAG_predictores_criteria_merged$cf_2018)
# JOMAG_predictores_criteria_merged$cf<-NA
# JOMAG_predictores_criteria_merged$cf_nna<-NA
# JOMAG_predictores_criteria_merged$cf_pctna<-NA
# for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
#   JOMAG_predictores_criteria_merged[i,]$cf<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],select=c(cf_2015,cf_2018)),na.rm=T)
#   JOMAG_predictores_criteria_merged[i,]$cf_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],select=c(cf_2015,cf_2018))))
#   JOMAG_predictores_criteria_merged[i,]$cf_pctna<-round(JOMAG_predictores_criteria_merged[i,]$cf_nna/2*100,2)
# }

# # amcf
# JOMAG_predictores_criteria_merged$am<-as.numeric(JOMAG_predictores_criteria_merged$am)
# JOMAG_predictores_criteria_merged$cf<-as.numeric(JOMAG_predictores_criteria_merged$cf)
# JOMAG_predictores_criteria_merged$amcf<-NA
# JOMAG_predictores_criteria_merged$amcf_nna<-NA
# JOMAG_predictores_criteria_merged$amcf_pctna<-NA
# for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
#   JOMAG_predictores_criteria_merged[i,]$amcf<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],
#                     select=c(am,cf)),na.rm=F)
#   JOMAG_predictores_criteria_merged[i,]$amcf_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],
#                          select=c(am,cf))))
#   JOMAG_predictores_criteria_merged[i,]$amcf_pctna<-round(JOMAG_predictores_criteria_merged[i,]$amcf_nna/2*100,2)
# }

# # am_regular_special_cf
# JOMAG_predictores_criteria_merged$am_regular_special<-as.numeric(JOMAG_predictores_criteria_merged$am_regular_special)
# JOMAG_predictores_criteria_merged$cf<-as.numeric(JOMAG_predictores_criteria_merged$cf)
# JOMAG_predictores_criteria_merged$am_regular_special_cf<-NA
# JOMAG_predictores_criteria_merged$am_regular_special_cf_nna<-NA
# JOMAG_predictores_criteria_merged$am_regular_special_cf_pctna<-NA
# for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
#   JOMAG_predictores_criteria_merged[i,]$am_regular_special_cf<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],
#                                                    select=c(am_regular_special,cf)),na.rm=F)
#   JOMAG_predictores_criteria_merged[i,]$am_regular_special_cf_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],
#                                                             select=c(am_regular_special,cf))))
#   JOMAG_predictores_criteria_merged[i,]$am_regular_special_cf_pctna<-round(JOMAG_predictores_criteria_merged[i,]$am_regular_special_cf_nna/2*100,2)
# }

# # tkufatitamcf
# JOMAG_predictores_criteria_merged$amcf<-as.numeric(JOMAG_predictores_criteria_merged$amcf)
# JOMAG_predictores_criteria_merged$tkufatit<-as.numeric(JOMAG_predictores_criteria_merged$tkufatit)
# JOMAG_predictores_criteria_merged$tkufatitamcf<-NA
# JOMAG_predictores_criteria_merged$tkufatitamcf_nna<-NA
# JOMAG_predictores_criteria_merged$tkufatitamcf_pctna<-NA
# for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
#   JOMAG_predictores_criteria_merged[i,]$tkufatitamcf<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],
#                     select=c(amcf,tkufatit)),na.rm=F)
#   JOMAG_predictores_criteria_merged[i,]$tkufatitamcf_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],
#                          select=c(amcf,tkufatit))))
#   JOMAG_predictores_criteria_merged[i,]$tkufatitamcf_pctna<-round(JOMAG_predictores_criteria_merged[i,]$tkufatitamcf_nna/2*100,2)
# }

# # tkufatit_am_regular_special_cf
# JOMAG_predictores_criteria_merged$am_regular_special_cf<-as.numeric(JOMAG_predictores_criteria_merged$am_regular_special_cf)
# JOMAG_predictores_criteria_merged$tkufatit<-as.numeric(JOMAG_predictores_criteria_merged$tkufatit)
# JOMAG_predictores_criteria_merged$tkufatit_am_regular_special_cf<-NA
# JOMAG_predictores_criteria_merged$tkufatit_am_regular_special_cf_nna<-NA
# JOMAG_predictores_criteria_merged$tkufatit_am_regular_special_cf_pctna<-NA
# for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
#   JOMAG_predictores_criteria_merged[i,]$tkufatit_am_regular_special_cf<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],
#                                                            select=c(am_regular_special_cf,tkufatit)),na.rm=F)
#   JOMAG_predictores_criteria_merged[i,]$tkufatit_am_regular_special_cf_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],
#                                                                     select=c(am_regular_special_cf,tkufatit))))
#   JOMAG_predictores_criteria_merged[i,]$tkufatit_am_regular_special_cf_pctna<-round(JOMAG_predictores_criteria_merged[i,]$tkufatit_am_regular_special_cf_nna/2*100,2)
# }

# tkufatitam
JOMAG_predictores_criteria_merged$am<-as.numeric(JOMAG_predictores_criteria_merged$am)
JOMAG_predictores_criteria_merged$tkufatit<-as.numeric(JOMAG_predictores_criteria_merged$tkufatit)
JOMAG_predictores_criteria_merged$tkufatitam<-NA
JOMAG_predictores_criteria_merged$tkufatitam_nna<-NA
JOMAG_predictores_criteria_merged$tkufatitam_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
  JOMAG_predictores_criteria_merged[i,]$tkufatitam<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],
                    select=c(am,tkufatit)),na.rm=F)
  JOMAG_predictores_criteria_merged[i,]$tkufatitam_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],
                         select=c(am,tkufatit))))
  JOMAG_predictores_criteria_merged[i,]$tkufatitam_pctna<-round(JOMAG_predictores_criteria_merged[i,]$tkufatitam_nna/2*100,2)
}

# tkufatit_am_regular_special
JOMAG_predictores_criteria_merged$am<-as.numeric(JOMAG_predictores_criteria_merged$am)
JOMAG_predictores_criteria_merged$tkufatit<-as.numeric(JOMAG_predictores_criteria_merged$tkufatit)
JOMAG_predictores_criteria_merged$tkufatit_am_regular_special<-NA
JOMAG_predictores_criteria_merged$tkufatit_am_regular_special_nna<-NA
JOMAG_predictores_criteria_merged$tkufatit_am_regular_special_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
  JOMAG_predictores_criteria_merged[i,]$tkufatit_am_regular_special<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],
                                                         select=c(am_regular_special,tkufatit)),na.rm=F)
  JOMAG_predictores_criteria_merged[i,]$tkufatit_am_regular_special_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],
                                                                  select=c(am_regular_special,tkufatit))))
  JOMAG_predictores_criteria_merged[i,]$tkufatit_am_regular_special_pctna<-round(JOMAG_predictores_criteria_merged[i,]$tkufatit_am_regular_special_nna/2*100,2)
}

# tkufatitNPct_am
JOMAG_predictores_criteria_merged$NPct_am<-as.numeric(JOMAG_predictores_criteria_merged$NPct_am)
JOMAG_predictores_criteria_merged$tkufatit<-as.numeric(JOMAG_predictores_criteria_merged$tkufatit)
JOMAG_predictores_criteria_merged$tkufatitNPct_am<-NA
JOMAG_predictores_criteria_merged$tkufatitNPct_am_nna<-NA
JOMAG_predictores_criteria_merged$tkufatitNPct_am_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
  JOMAG_predictores_criteria_merged[i,]$tkufatitNPct_am<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],select=c(NPct_am,tkufatit)),na.rm=F)
  JOMAG_predictores_criteria_merged[i,]$tkufatitNPct_am_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],select=c(NPct_am,tkufatit))))
  JOMAG_predictores_criteria_merged[i,]$tkufatitNPct_am_pctna<-round(JOMAG_predictores_criteria_merged[i,]$tkufatitNPct_am_nna/2*100,2)
}

# tkufatitamcf_basic_pairwise
JOMAG_predictores_criteria_merged$final.score.2015_zscore<-as.numeric(JOMAG_predictores_criteria_merged$final.score.2015_zscore)
JOMAG_predictores_criteria_merged$final.score.2017_zscore<-as.numeric(JOMAG_predictores_criteria_merged$final.score.2017_zscore)
JOMAG_predictores_criteria_merged$tkufatit_14_zscore<-as.numeric(JOMAG_predictores_criteria_merged$tkufatit_14_zscore)
JOMAG_predictores_criteria_merged$am_2015<-as.numeric(JOMAG_predictores_criteria_merged$am_2015)
JOMAG_predictores_criteria_merged$am_2018<-as.numeric(JOMAG_predictores_criteria_merged$am_2018)
JOMAG_predictores_criteria_merged$cf_2012<-as.numeric(JOMAG_predictores_criteria_merged$cf_2012)
JOMAG_predictores_criteria_merged$cf_2015<-as.numeric(JOMAG_predictores_criteria_merged$cf_2015)
JOMAG_predictores_criteria_merged$cf_2018<-as.numeric(JOMAG_predictores_criteria_merged$cf_2018)
JOMAG_predictores_criteria_merged$tkufatitamcf_basic_pairwise<-NA
JOMAG_predictores_criteria_merged$tkufatitamcf_basic_pairwise_nna<-NA
JOMAG_predictores_criteria_merged$tkufatitamcf_basic_pairwise_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged)){JOMAG_predictores_criteria_merged[i,]$tkufatitamcf_basic_pairwise<-
    rowMeans(subset(JOMAG_predictores_criteria_merged[i,],select=c(NPct_am,tkufatit)),na.rm=T)
    JOMAG_predictores_criteria_merged[i,]$tkufatitamcf_basic_pairwise_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],
    select=c(final.score.2015_zscore,final.score.2017_zscore,tkufatit_14_zscore,final.score.2018_zscore,am_2015,am_2018))))
    JOMAG_predictores_criteria_merged[i,]$tkufatitamcf_basic_pairwise_pctna<-round(JOMAG_predictores_criteria_merged[i,]$tkufatitamcf_basic_pairwise_nna/6*100,2)
}

# # tkufatitcf
# JOMAG_predictores_criteria_merged$cf<-as.numeric(JOMAG_predictores_criteria_merged$cf)
# JOMAG_predictores_criteria_merged$tkufatit<-as.numeric(JOMAG_predictores_criteria_merged$tkufatit)
# JOMAG_predictores_criteria_merged$tkufatitcf<-NA
# JOMAG_predictores_criteria_merged$tkufatitcf_nna<-NA
# JOMAG_predictores_criteria_merged$tkufatitcf_pctna<-NA
# for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
#   JOMAG_predictores_criteria_merged[i,]$tkufatitcf<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],
#                     select=c(cf,tkufatit)),na.rm=F)
#   JOMAG_predictores_criteria_merged[i,]$tkufatitcf_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],
#                          select=c(cf,tkufatit))))
#   JOMAG_predictores_criteria_merged[i,]$tkufatitcf_pctna<-round(JOMAG_predictores_criteria_merged[i,]$tkufatitcf_nna/2*100,2)
# }

# # tkufatitam_or_cf
# JOMAG_predictores_criteria_merged$am<-as.numeric(JOMAG_predictores_criteria_merged$am)
# JOMAG_predictores_criteria_merged$cf<-as.numeric(JOMAG_predictores_criteria_merged$cf)
# JOMAG_predictores_criteria_merged$amcf<-as.numeric(JOMAG_predictores_criteria_merged$amcf)
# JOMAG_predictores_criteria_merged$tkufatit<-as.numeric(JOMAG_predictores_criteria_merged$tkufatit)
# JOMAG_predictores_criteria_merged$tkufatitam_or_cf<-NA
# JOMAG_predictores_criteria_merged$tkufatitam_or_cf_nna<-NA
# JOMAG_predictores_criteria_merged$tkufatitam_or_cf_pctna<-NA
# for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
#   if(is.na(JOMAG_predictores_criteria_merged[i,]$cf) & !is.na(JOMAG_predictores_criteria_merged[i,]$am)){
#   JOMAG_predictores_criteria_merged[i,]$tkufatitam_or_cf<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],select=c(am,tkufatit)),na.rm=F)
#   JOMAG_predictores_criteria_merged[i,]$tkufatitam_or_cf_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],select=c(am,tkufatit))))
#   JOMAG_predictores_criteria_merged[i,]$tkufatitam_or_cf_pctna<round(JOMAG_predictores_criteria_merged[i,]$tkufatitam_or_cf_nna/2*100,2)
#   }
#   
#     else {
#       
#   if(is.na(JOMAG_predictores_criteria_merged[i,]$am) & !is.na(JOMAG_predictores_criteria_merged[i,]$cf)){
#   JOMAG_predictores_criteria_merged[i,]$tkufatitam_or_cf<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],select=c(cf,tkufatit)),na.rm=F)
#   JOMAG_predictores_criteria_merged[i,]$tkufatitam_or_cf_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],select=c(cf,tkufatit))))
#   JOMAG_predictores_criteria_merged[i,]$tkufatitam_or_cf_pctna<-round(JOMAG_predictores_criteria_merged[i,]$tkufatitam_or_cf_nna/2*100,2)
#   }
#       
#    else {
#      
#   JOMAG_predictores_criteria_merged[i,]$tkufatitam_or_cf<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],select=c(amcf,tkufatit)),na.rm=F)
#   JOMAG_predictores_criteria_merged[i,]$tkufatitam_or_cf_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],select=c(amcf,tkufatit))))
#   JOMAG_predictores_criteria_merged[i,]$tkufatitam_or_cf_pctna<-round(JOMAG_predictores_criteria_merged[i,]$tkufatitamcf_nna/2*100,2)
#    }
#     }
# }
# 
# # tkufatitam_regular_special_or_cf
# JOMAG_predictores_criteria_merged$am_regular_special<-as.numeric(JOMAG_predictores_criteria_merged$am_regular_special)
# JOMAG_predictores_criteria_merged$cf<-as.numeric(JOMAG_predictores_criteria_merged$cf)
# JOMAG_predictores_criteria_merged$am_regular_special_cf<-as.numeric(JOMAG_predictores_criteria_merged$am_regular_special_cf)
# JOMAG_predictores_criteria_merged$tkufatit<-as.numeric(JOMAG_predictores_criteria_merged$tkufatit)
# JOMAG_predictores_criteria_merged$tkufatitam_regular_special_or_cf<-NA
# JOMAG_predictores_criteria_merged$tkufatitam_regular_special_or_cf_nna<-NA
# JOMAG_predictores_criteria_merged$tkufatitam_regular_special_or_cf_pctna<-NA
# for(i in 1:nrow(JOMAG_predictores_criteria_merged)){
#   if(is.na(JOMAG_predictores_criteria_merged[i,]$cf) & !is.na(JOMAG_predictores_criteria_merged[i,]$am_regular_special)){
#     JOMAG_predictores_criteria_merged[i,]$tkufatitam_regular_special_or_cf<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],select=c(am_regular_special,tkufatit)),na.rm=F)
#     JOMAG_predictores_criteria_merged[i,]$tkufatitam_regular_special_or_cf_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],select=c(am_regular_special,tkufatit))))
#     JOMAG_predictores_criteria_merged[i,]$tkufatitam_regular_special_or_cf_pctna<round(JOMAG_predictores_criteria_merged[i,]$tkufatitam_regular_special_or_cf_nna/2*100,2)
#   }
#   else {
#     
#     if(is.na(JOMAG_predictores_criteria_merged[i,]$am_regular_special) & !is.na(JOMAG_predictores_criteria_merged[i,]$cf)){
#       JOMAG_predictores_criteria_merged[i,]$tkufatitam_regular_special_or_cf<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],select=c(cf,tkufatit)),na.rm=F)
#       JOMAG_predictores_criteria_merged[i,]$tkufatitam_regular_special_or_cf_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],select=c(cf,tkufatit))))
#       JOMAG_predictores_criteria_merged[i,]$tkufatitam_regular_special_or_cf_pctna<-round(JOMAG_predictores_criteria_merged[i,]$tkufatitam_regular_special_or_cf_nna/2*100,2)
#     }
# 
#   else {
#     
#   JOMAG_predictores_criteria_merged[i,]$tkufatitam_regular_special_or_cf<-rowMeans(subset(JOMAG_predictores_criteria_merged[i,],select=c(am_regular_special_cf,tkufatit)),na.rm=F)
#   JOMAG_predictores_criteria_merged[i,]$tkufatitam_regular_special_or_cf_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged[i,],select=c(am_regular_special_cf,tkufatit))))
#   JOMAG_predictores_criteria_merged[i,]$tkufatitam_regular_special_or_cf_pctna<-round(JOMAG_predictores_criteria_merged[i,]$tkufatitam_regular_special_or_cf_nna/2*100,2)
#   }
# }
# }

End_time<-Sys.time()
Total_time<- round(End_time-Start_time, digits = 2)
Total_time
# Total_time=14.9 mins

library(sendmailR)
from <- sprintf("<sendmailR@%s>", Sys.info()[4])
to <- "<xxxxx>"
subject <- "JOMAG session has been completed"
body <- list(paste("\nLength of time to run the code: ", Total_time,"."))
sendmail(from, to, subject, body,control=list(smtpServer="xxxxx"))

# Add critria_count to data.
JOMAG_predictores_criteria_merged$critria_count<-
rowSums(!is.na(subset(JOMAG_predictores_criteria_merged,select=c(final.score.2015_zscore,final.score.2017_zscore,final.score.2018_zscore,tkufatit_14_zscore,am_2015,am_2018))))

# seniority_days
JOMAG_predictores_criteria_merged$seniority_days<-
    ifelse(!is.na(JOMAG_predictores_criteria_merged$date.period.eval.2018_diff) & JOMAG_predictores_criteria_merged$date.period.eval.2018_diff>=0,
           JOMAG_predictores_criteria_merged$date.period.eval.2018_diff,NA)

# seniority_years
JOMAG_predictores_criteria_merged$seniority_years<-round(as.numeric(JOMAG_predictores_criteria_merged$seniority_days)/365,2)

# seniority_high_low (1=low, 2=high)
JOMAG_predictores_criteria_merged$seniority_days<-as.numeric(JOMAG_predictores_criteria_merged$seniority_days)
median_seniority_days<-median(JOMAG_predictores_criteria_merged_qv$seniority_days[which(!is.na(JOMAG_predictores_criteria_merged_qv$seniority_days))],na.rm = F)
JOMAG_predictores_criteria_merged$seniority_high_low<-ifelse(JOMAG_predictores_criteria_merged$seniority_days<=median_seniority_days 
                                                          & JOMAG_predictores_criteria_merged$critria_count>1,1,
                                                     ifelse(JOMAG_predictores_criteria_merged$critria_count>1,2,NA))

# seniority_low_third_high (1=low third, 2=2 third high thirds)
JOMAG_predictores_criteria_merged$seniority_days<-as.numeric(JOMAG_predictores_criteria_merged$seniority_days)
seniority_low_third<-unname(quantile(JOMAG_predictores_criteria_merged_qv$seniority_days,c(.33),na.rm = T))
JOMAG_predictores_criteria_merged$seniority_low_third_high<-ifelse(JOMAG_predictores_criteria_merged$seniority_days<=seniority_low_third 
                                                                 & JOMAG_predictores_criteria_merged$critria_count>1,1,
                                                                  ifelse(JOMAG_predictores_criteria_merged$critria_count>1,2,NA))

# seniority_low_fifth_high (1=low fifthe, 2=4 high fifths)
JOMAG_predictores_criteria_merged$seniority_days<-as.numeric(JOMAG_predictores_criteria_merged$seniority_days)
seniority_low_fifth<-unname(quantile(JOMAG_predictores_criteria_merged_qv$seniority_days,c(.20),na.rm = T))
JOMAG_predictores_criteria_merged$seniority_low_fifth_high<-ifelse(JOMAG_predictores_criteria_merged$seniority_days<=seniority_low_fifth 
                                                                         & JOMAG_predictores_criteria_merged$critria_count>1,1,
                                                                    ifelse(JOMAG_predictores_criteria_merged$critria_count>1,2,NA))

# formula_score.
JOMAG_predictores_criteria_merged_civil_qv$formula_score<-NA
for (i in 1:nrow(JOMAG_predictores_criteria_merged_civil_qv)) {
  if(!is.na(JOMAG_predictores_criteria_merged_civil_qv[i,]$final_apptitudes_new) | 
     !is.na(JOMAG_predictores_criteria_merged_civil_qv[i,]$TsiunSofiAfter_zscore) |
     !is.na(JOMAG_predictores_criteria_merged_civil_qv[i,]$SocioGrade_zscore) |
     !is.na(JOMAG_predictores_criteria_merged_civil_qv[i,]$PersonalityGrade_zscore)) {
    JOMAG_predictores_criteria_merged_civil_qv[i,]$formula_score <-
      sum(.13*JOMAG_predictores_criteria_merged_civil_qv[i,]$final_apptitudes_new,
          .29*JOMAG_predictores_criteria_merged_civil_qv[i,]$TsiunSofiAfter_zscore,
          .29*JOMAG_predictores_criteria_merged_civil_qv[i,]$SocioGrade_zscore,
          .29*JOMAG_predictores_criteria_merged_civil_qv[i,]$PersonalityGrade_zscore,na.rm = T)}
  else {
    JOMAG_predictores_criteria_merged_civil_qv[i,]$formula_score <-NA
  }
}

# Handle nan values
is.nan.data.frame<-function(x)
  do.call(cbind,lapply(x,is.nan))  
JOMAG_predictores_criteria_merged[is.nan(JOMAG_predictores_criteria_merged)]<-NA

# Data base parallel to QV dashboard.
# JOMAG_predictores_criteria_merged_qv<-JOMAG_predictores_criteria_merged[which(!is.na(JOMAG_predictores_criteria_merged$FileGrade)
#                                                                              & JOMAG_predictores_criteria_merged$critria_count>1),]

# ***************** From here the processing was done on the civil pc. **********************

# Importing the data
JOMAG_predictores_criteria_merged_civil<-read.csv("C:/Users/Administrator.MAMADA-777/Documents/Junior officers MAGAV validation/JOMAG_predictores_criteria_merged_civil.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
JOMAG_predictores_criteria_merged_civil<-JOMAG_predictores_criteria_merged_civil[,-1]

# Data base parallel to QV dashboard.
JOMAG_predictores_criteria_merged_civil_qv<-JOMAG_predictores_criteria_merged_civil[which(!is.na(JOMAG_predictores_criteria_merged_civil$FileGrade)                                                                           & JOMAG_predictores_criteria_merged_civil$critria_count>1),]

# Cf
JOMAG_predictores_criteria_merged_civil$cf_2012 <-
  ifelse(as.numeric(JOMAG_predictores_criteria_merged_civil$TaarichHavara_cf_2012_diff)< 352,NA,JOMAG_predictores_criteria_merged_civil$cf_2012)

JOMAG_predictores_criteria_merged_civil$cf_2015 <-
  ifelse(as.numeric(JOMAG_predictores_criteria_merged_civil$TaarichHavara_cf_2015_diff)< 306,NA,JOMAG_predictores_criteria_merged_civil$cf_2015)

JOMAG_predictores_criteria_merged_civil$cf_2018 <-
  ifelse(as.numeric(JOMAG_predictores_criteria_merged_civil$TaarichHavara_cf_2018_diff)< 302,NA,JOMAG_predictores_criteria_merged_civil$cf_2018)

JOMAG_predictores_criteria_merged_civil$cf_2012<-as.numeric(JOMAG_predictores_criteria_merged_civil$cf_2012)
JOMAG_predictores_criteria_merged_civil$cf_2015<-as.numeric(JOMAG_predictores_criteria_merged_civil$cf_2015)
JOMAG_predictores_criteria_merged_civil$cf_2018<-as.numeric(JOMAG_predictores_criteria_merged_civil$cf_2018)
JOMAG_predictores_criteria_merged_civil$cf<-NA
JOMAG_predictores_criteria_merged_civil$cf_nna<-NA
JOMAG_predictores_criteria_merged_civil$cf_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged_civil)){
  JOMAG_predictores_criteria_merged_civil[i,]$cf<-rowMeans(subset(JOMAG_predictores_criteria_merged_civil[i,],select=c(cf_2012,cf_2015,cf_2018)),na.rm=T)
  JOMAG_predictores_criteria_merged_civil[i,]$cf_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged_civil[i,],select=c(cf_2012,cf_2015,cf_2018))))
  JOMAG_predictores_criteria_merged_civil[i,]$cf_pctna<-round(JOMAG_predictores_criteria_merged_civil[i,]$cf_nna/3*100,2)
}

JOMAG_predictores_criteria_merged_civil$cf_nna2<-ifelse(JOMAG_predictores_criteria_merged_civil$cf_nna<2,JOMAG_predictores_criteria_merged_civil$cf,NA)
JOMAG_predictores_criteria_merged_civil$cf_nna2_last<-ifelse(!is.na(JOMAG_predictores_criteria_merged_civil$cf_nna2) & 
                                                               !is.na(JOMAG_predictores_criteria_merged_civil$cf_2018),
                                                             JOMAG_predictores_criteria_merged_civil$cf_2018,
                                                             JOMAG_predictores_criteria_merged_civil$cf_2015)

JOMAG_predictores_criteria_merged_civil_qv<-JOMAG_predictores_criteria_merged_civil[which(!is.na(JOMAG_predictores_criteria_merged_civil$FileGrade)
                                                                                          & JOMAG_predictores_criteria_merged_civil$critria_count>1),]

# blue_officers
library(readr)
locale("he")
JOMAG_predictores_criteria_merged_civil<-read_csv("C:/Users/Asher/Documents/MAMDA/JOMAGAV/JOMAG_predictores_criteria_merged_civil_old.csv",locale = locale(date_names = "he", encoding = "ISO-8859-8"))
JOMAG_predictores_criteria_merged_civil<-JOMAG_predictores_criteria_merged_civil[-1]

# blue_officers <- read.csv("C:/Users/Asher/Documents/MAMDA/JOMAGAV/blue_officers_civil.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
blue_officers <- read.csv("C:/Users/Administrator.MAMADA-777/Documents/Junior officers MAGAV validation/blue_officers_civil.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")

blue_officers<-blue_officers[-1]

blue_officers$blue_date<-as.Date(unlist(blue_officers$blue_date),format="%d/%m/%Y")


JOMAG_predictores_criteria_merged_civil <- merge(JOMAG_predictores_criteria_merged_civil, blue_officers,by=c("personal_number"), all.x=T, all.y=F,sort = FALSE)

# JOMAG_predictores_criteria_merged_civil_qv<-JOMAG_predictores_criteria_merged_civil[which(!is.na(JOMAG_predictores_criteria_merged_civil$FileGrade)
#                                                                                          & JOMAG_predictores_criteria_merged_civil$critria_count>1),]

library(descr)
library(psych)
options(width = 71,max.print=30000)
freq(ordered(JOMAG_predictores_criteria_merged_civil$blue), plot = F,main=colnames(JOMAG_predictores_criteria_merged_civil$blue),font=2)

library(dplyr)

filtered_blue=JOMAG_predictores_criteria_merged_civil %>%
  filter(blue==1 & officer==1 & !is.na(FileGrade) & critria_count>1)

nrow(filtered_blue)
nrow(JOMAG_predictores_criteria_merged_civil)
freq(ordered(JOMAG_predictores_criteria_merged_civil$officer), plot = F,main=colnames(JOMAG_predictores_criteria_merged_civil$officer),font=2)


library(dplyr)
JOMAG_predictores_criteria_merged_civil_qv$blue_date<-as.Date(unlist(JOMAG_predictores_criteria_merged_civil_qv$blue_date),format="%d/%m/%Y")
JOMAG_predictores_criteria_merged_civil_qv$TaarichHavara_am_2015<-as.Date(unlist(JOMAG_predictores_criteria_merged_civil_qv$TaarichHavara_am_2015),format="%d/%m/%Y")
JOMAG_predictores_criteria_merged_civil_qv$TaarichHavara_am_2018<-as.Date(unlist(JOMAG_predictores_criteria_merged_civil_qv$TaarichHavara_am_2018),format="%d/%m/%Y")
JOMAG_predictores_criteria_merged_civil_qv$TaarichHavara_cf_2012<-as.Date(unlist(JOMAG_predictores_criteria_merged_civil_qv$TaarichHavara_cf_2012),format="%d/%m/%Y")
JOMAG_predictores_criteria_merged_civil_qv$TaarichHavara_cf_2015<-as.Date(unlist(JOMAG_predictores_criteria_merged_civil_qv$TaarichHavara_cf_2015),format="%d/%m/%Y")
JOMAG_predictores_criteria_merged_civil_qv$TaarichHavara_cf_2018<-as.Date(unlist(JOMAG_predictores_criteria_merged_civil_qv$TaarichHavara_cf_2018),format="%d/%m/%Y")
JOMAG_predictores_criteria_merged_civil_qv$date.tkufatit_14<-as.Date(unlist(JOMAG_predictores_criteria_merged_civil_qv$date.tkufatit_14),format="%d/%m/%Y")
JOMAG_predictores_criteria_merged_civil_qv$date.period.eval.2015<-as.Date(unlist(JOMAG_predictores_criteria_merged_civil_qv$date.period.eval.2015),format="%d/%m/%Y")
JOMAG_predictores_criteria_merged_civil_qv$date.period.eval.2017<-as.Date(unlist(JOMAG_predictores_criteria_merged_civil_qv$date.period.eval.2017),format="%d/%m/%Y")
JOMAG_predictores_criteria_merged_civil_qv$date.period.eval.2018<-as.Date(unlist(JOMAG_predictores_criteria_merged_civil_qv$date.period.eval.2018),format="%d/%m/%Y")

JOMAG_predictores_criteria_merged_civil_qv = JOMAG_predictores_criteria_merged_civil_qv %>% 
  mutate(am_2015 = ifelse(!is.na(blue_date) & blue_date<TaarichHavara_am_2015,NA,am_2015),
         am_2018 = ifelse(!is.na(blue_date) & blue_date<TaarichHavara_am_2018,NA,am_2018),
         cf_2012 = ifelse(!is.na(blue_date) & blue_date<TaarichHavara_cf_2012,NA,cf_2012),
         cf_2015 = ifelse(!is.na(blue_date) & blue_date<TaarichHavara_cf_2015,NA,cf_2015),
         cf_2018 = ifelse(!is.na(blue_date) & blue_date<TaarichHavara_cf_2018,NA,cf_2018),
         tkufatit_14 = ifelse(!is.na(blue_date) & blue_date<date.tkufatit_14,NA,tkufatit_14),
         final.score.2015 = ifelse(!is.na(blue_date) & blue_date<date.period.eval.2015,NA,final.score.2015),
         final.score.2017 = ifelse(!is.na(blue_date) & blue_date<date.period.eval.2017,NA,final.score.2017),
         final.score.2018 = ifelse(!is.na(blue_date) & blue_date<date.period.eval.2018,NA,final.score.2018))

#High order criteria blue

# NPct_am
JOMAG_predictores_criteria_merged_civil_qv$NPct_am_2015<-as.numeric(JOMAG_predictores_criteria_merged_civil_qv$NPct_am_2015)
JOMAG_predictores_criteria_merged_civil_qv$NPct_am_2018<-as.numeric(JOMAG_predictores_criteria_merged_civil_qv$NPct_am_2018)
JOMAG_predictores_criteria_merged_civil_qv$NPct_am<-NA
JOMAG_predictores_criteria_merged_civil_qv$NPct_am_nna<-NA
JOMAG_predictores_criteria_merged_civil_qv$NPct_am_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged_civil_qv)){
  JOMAG_predictores_criteria_merged_civil_qv[i,]$NPct_am<-rowMeans(subset(JOMAG_predictores_criteria_merged_civil_qv[i,],select=c(NPct_am_2015,NPct_am_2018)),na.rm=T)
  JOMAG_predictores_criteria_merged_civil_qv[i,]$NPct_am_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged_civil_qv[i,],select=c(NPct_am_2015,NPct_am_2018))))
  JOMAG_predictores_criteria_merged_civil_qv[i,]$NPct_am_pctna<-round(JOMAG_predictores_criteria_merged_civil_qv[i,]$NPct_am_nna/2*100,2)
}

# NPct_am_special
JOMAG_predictores_criteria_merged_civil_qv$NPct_am_2015_special<-as.numeric(JOMAG_predictores_criteria_merged_civil_qv$NPct_am_2015_special)
JOMAG_predictores_criteria_merged_civil_qv$NPct_am_2018_special<-as.numeric(JOMAG_predictores_criteria_merged_civil_qv$NPct_am_2018_special)
JOMAG_predictores_criteria_merged_civil_qv$NPct_am_special<-NA
JOMAG_predictores_criteria_merged_civil_qv$NPct_am_special_nna<-NA
JOMAG_predictores_criteria_merged_civil_qv$NPct_am_special_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged_civil_qv)){
  JOMAG_predictores_criteria_merged_civil_qv[i,]$NPct_am_special<-rowMeans(subset(JOMAG_predictores_criteria_merged_civil_qv[i,],select=c(NPct_am_2015_special,NPct_am_2018_special)),na.rm=T)
  JOMAG_predictores_criteria_merged_civil_qv[i,]$NPct_am_special_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged_civil_qv[i,],select=c(NPct_am_2015_special,NPct_am_2018_special))))
  JOMAG_predictores_criteria_merged_civil_qv[i,]$NPct_am_special_pctna<-round(JOMAG_predictores_criteria_merged_civil_qv[i,]$NPct_am_special_nna/2*100,2)
}

# tkufatit
JOMAG_predictores_criteria_merged_civil_qv$final.score.2015_zscore<-as.numeric(JOMAG_predictores_criteria_merged_civil_qv$final.score.2015_zscore)
JOMAG_predictores_criteria_merged_civil_qv$final.score.2017_zscore<-as.numeric(JOMAG_predictores_criteria_merged_civil_qv$final.score.2017_zscore)
JOMAG_predictores_criteria_merged_civil_qv$tkufatit_14_zscore<-as.numeric(JOMAG_predictores_criteria_merged_civil_qv$tkufatit_14_zscore)
JOMAG_predictores_criteria_merged_civil_qv$final.score.2018_zscore<-as.numeric(JOMAG_predictores_criteria_merged_civil_qv$final.score.2018_zscore)
JOMAG_predictores_criteria_merged_civil_qv$tkufatit<-NA
JOMAG_predictores_criteria_merged_civil_qv$tkufatit_nna<-NA
JOMAG_predictores_criteria_merged_civil_qv$tkufatit_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged_civil_qv)){
  JOMAG_predictores_criteria_merged_civil_qv[i,]$tkufatit<-rowMeans(subset(JOMAG_predictores_criteria_merged_civil_qv[i,],
                                                                  select=c(final.score.2015_zscore,final.score.2017_zscore,tkufatit_14_zscore,final.score.2018_zscore)),na.rm=T)
  JOMAG_predictores_criteria_merged_civil_qv[i,]$tkufatit_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged_civil_qv[i,],
                                                                           select=c(final.score.2015_zscore,final.score.2017_zscore,tkufatit_14_zscore,final.score.2018_zscore))))
  JOMAG_predictores_criteria_merged_civil_qv[i,]$tkufatit_pctna<-round(JOMAG_predictores_criteria_merged_civil_qv[i,]$tkufatit_nna/4*100,2)
}

#am
JOMAG_predictores_criteria_merged_civil_qv$am_2015<-as.numeric(JOMAG_predictores_criteria_merged_civil_qv$am_2015)
JOMAG_predictores_criteria_merged_civil_qv$am_2018<-as.numeric(JOMAG_predictores_criteria_merged_civil_qv$am_2018)
JOMAG_predictores_criteria_merged_civil_qv$am<-NA
JOMAG_predictores_criteria_merged_civil_qv$am_nna<-NA
JOMAG_predictores_criteria_merged_civil_qv$am_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged_civil_qv)){
  JOMAG_predictores_criteria_merged_civil_qv[i,]$am<-rowMeans(subset(JOMAG_predictores_criteria_merged_civil_qv[i,],select=c(am_2015,am_2018)),na.rm=T)
  JOMAG_predictores_criteria_merged_civil_qv[i,]$am_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged_civil_qv[i,],select=c(am_2015,am_2018))))
  JOMAG_predictores_criteria_merged_civil_qv[i,]$am_pctna<-round(JOMAG_predictores_criteria_merged_civil_qv[i,]$am_nna/2*100,2)
}

# am_regular_special
JOMAG_predictores_criteria_merged_civil_qv$am_2015_regular_special<-as.numeric(JOMAG_predictores_criteria_merged_civil_qv$am_2015_regular_special)
JOMAG_predictores_criteria_merged_civil_qv$am_2018_regular_special<-as.numeric(JOMAG_predictores_criteria_merged_civil_qv$am_2018_regular_special)
JOMAG_predictores_criteria_merged_civil_qv$am_regular_special<-NA
JOMAG_predictores_criteria_merged_civil_qv$am_regular_special_nna<-NA
JOMAG_predictores_criteria_merged_civil_qv$am_regular_special_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged_civil_qv)){
  JOMAG_predictores_criteria_merged_civil_qv[i,]$am_regular_special<-rowMeans(subset(JOMAG_predictores_criteria_merged_civil_qv[i,],select=c(am_2015_regular_special,am_2018_regular_special)),na.rm=T)
  JOMAG_predictores_criteria_merged_civil_qv[i,]$am_regular_special_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged_civil_qv[i,],select=c(am_2015_regular_special,am_2018_regular_special))))
  JOMAG_predictores_criteria_merged_civil_qv[i,]$am_regular_special_pctna<-round(JOMAG_predictores_criteria_merged_civil_qv[i,]$am_regular_special_nna/2*100,2)
}

# tkufatitam
JOMAG_predictores_criteria_merged_civil_qv$am<-as.numeric(JOMAG_predictores_criteria_merged_civil_qv$am)
JOMAG_predictores_criteria_merged_civil_qv$tkufatit<-as.numeric(JOMAG_predictores_criteria_merged_civil_qv$tkufatit)
JOMAG_predictores_criteria_merged_civil_qv$tkufatitam<-NA
JOMAG_predictores_criteria_merged_civil_qv$tkufatitam_nna<-NA
JOMAG_predictores_criteria_merged_civil_qv$tkufatitam_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged_civil_qv)){
  JOMAG_predictores_criteria_merged_civil_qv[i,]$tkufatitam<-rowMeans(subset(JOMAG_predictores_criteria_merged_civil_qv[i,],
                                                                    select=c(am,tkufatit)),na.rm=F)
  JOMAG_predictores_criteria_merged_civil_qv[i,]$tkufatitam_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged_civil_qv[i,],
                                                                             select=c(am,tkufatit))))
  JOMAG_predictores_criteria_merged_civil_qv[i,]$tkufatitam_pctna<-round(JOMAG_predictores_criteria_merged_civil_qv[i,]$tkufatitam_nna/2*100,2)
}

# tkufatit_am_regular_special
JOMAG_predictores_criteria_merged_civil_qv$am<-as.numeric(JOMAG_predictores_criteria_merged_civil_qv$am)
JOMAG_predictores_criteria_merged_civil_qv$tkufatit<-as.numeric(JOMAG_predictores_criteria_merged_civil_qv$tkufatit)
JOMAG_predictores_criteria_merged_civil_qv$tkufatit_am_regular_special<-NA
JOMAG_predictores_criteria_merged_civil_qv$tkufatit_am_regular_special_nna<-NA
JOMAG_predictores_criteria_merged_civil_qv$tkufatit_am_regular_special_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged_civil_qv)){
  JOMAG_predictores_criteria_merged_civil_qv[i,]$tkufatit_am_regular_special<-rowMeans(subset(JOMAG_predictores_criteria_merged_civil_qv[i,],
                                                                                     select=c(am_regular_special,tkufatit)),na.rm=F)
  JOMAG_predictores_criteria_merged_civil_qv[i,]$tkufatit_am_regular_special_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged_civil_qv[i,],
                                                                                              select=c(am_regular_special,tkufatit))))
  JOMAG_predictores_criteria_merged_civil_qv[i,]$tkufatit_am_regular_special_pctna<-round(JOMAG_predictores_criteria_merged_civil_qv[i,]$tkufatit_am_regular_special_nna/2*100,2)
}

# tkufatitNPct_am
JOMAG_predictores_criteria_merged_civil_qv$NPct_am<-as.numeric(JOMAG_predictores_criteria_merged_civil_qv$NPct_am)
JOMAG_predictores_criteria_merged_civil_qv$tkufatit<-as.numeric(JOMAG_predictores_criteria_merged_civil_qv$tkufatit)
JOMAG_predictores_criteria_merged_civil_qv$tkufatitNPct_am<-NA
JOMAG_predictores_criteria_merged_civil_qv$tkufatitNPct_am_nna<-NA
JOMAG_predictores_criteria_merged_civil_qv$tkufatitNPct_am_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged_civil_qv)){
  JOMAG_predictores_criteria_merged_civil_qv[i,]$tkufatitNPct_am<-rowMeans(subset(JOMAG_predictores_criteria_merged_civil_qv[i,],select=c(NPct_am,tkufatit)),na.rm=F)
  JOMAG_predictores_criteria_merged_civil_qv[i,]$tkufatitNPct_am_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged_civil_qv[i,],select=c(NPct_am,tkufatit))))
  JOMAG_predictores_criteria_merged_civil_qv[i,]$tkufatitNPct_am_pctna<-round(JOMAG_predictores_criteria_merged_civil_qv[i,]$tkufatitNPct_am_nna/2*100,2)
}

# tkufatitamcf_basic_pairwise
JOMAG_predictores_criteria_merged_civil_qv$final.score.2015_zscore<-as.numeric(JOMAG_predictores_criteria_merged_civil_qv$final.score.2015_zscore)
JOMAG_predictores_criteria_merged_civil_qv$final.score.2017_zscore<-as.numeric(JOMAG_predictores_criteria_merged_civil_qv$final.score.2017_zscore)
JOMAG_predictores_criteria_merged_civil_qv$tkufatit_14_zscore<-as.numeric(JOMAG_predictores_criteria_merged_civil_qv$tkufatit_14_zscore)
JOMAG_predictores_criteria_merged_civil_qv$am_2015<-as.numeric(JOMAG_predictores_criteria_merged_civil_qv$am_2015)
JOMAG_predictores_criteria_merged_civil_qv$am_2018<-as.numeric(JOMAG_predictores_criteria_merged_civil_qv$am_2018)
JOMAG_predictores_criteria_merged_civil_qv$cf_2012<-as.numeric(JOMAG_predictores_criteria_merged_civil_qv$cf_2012)
JOMAG_predictores_criteria_merged_civil_qv$cf_2015<-as.numeric(JOMAG_predictores_criteria_merged_civil_qv$cf_2015)
JOMAG_predictores_criteria_merged_civil_qv$cf_2018<-as.numeric(JOMAG_predictores_criteria_merged_civil_qv$cf_2018)
JOMAG_predictores_criteria_merged_civil_qv$tkufatitamcf_basic_pairwise<-NA
JOMAG_predictores_criteria_merged_civil_qv$tkufatitamcf_basic_pairwise_nna<-NA
JOMAG_predictores_criteria_merged_civil_qv$tkufatitamcf_basic_pairwise_pctna<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged_civil_qv)){JOMAG_predictores_criteria_merged_civil_qv[i,]$tkufatitamcf_basic_pairwise<-
  rowMeans(subset(JOMAG_predictores_criteria_merged_civil_qv[i,],select=c(NPct_am,tkufatit)),na.rm=T)
JOMAG_predictores_criteria_merged_civil_qv[i,]$tkufatitamcf_basic_pairwise_nna<-rowSums(is.na(subset(JOMAG_predictores_criteria_merged_civil_qv[i,],
                                                                                            select=c(final.score.2015_zscore,final.score.2017_zscore,tkufatit_14_zscore,final.score.2018_zscore,am_2015,am_2018))))
JOMAG_predictores_criteria_merged_civil_qv[i,]$tkufatitamcf_basic_pairwise_pctna<-round(JOMAG_predictores_criteria_merged_civil_qv[i,]$tkufatitamcf_basic_pairwise_nna/6*100,2)
}

#VaadaGrade_completed
JOMAG_predictores_criteria_merged_civil$VaadaGrade_completed <-
  ifelse(is.na(JOMAG_predictores_criteria_merged_civil$VaadaGrade),JOMAG_predictores_criteria_merged_civil$FileGrade,JOMAG_predictores_criteria_merged_civil$VaadaGrade)
library(descr)
library(psych)
options(width = 71,max.print=30000)
round(freq(ordered(as.numeric(unlist(JOMAG_predictores_criteria_merged_civil$VaadaGrade_completed))), plot = T,main=colnames(JOMAG_predictores_criteria_merged_civil$VaadaGrade_completed),font=2),2)

JOMAG_predictores_criteria_merged_civil_qv<-JOMAG_predictores_criteria_merged_civil[which(!is.na(JOMAG_predictores_criteria_merged_civil$FileGrade)                                                                                        & JOMAG_predictores_criteria_merged_civil$critria_count>1),]

#TsiunSofiBefore
JOMAG_predictores_criteria_merged_civil$TsiunSofiBefore<-NA
JOMAG_predictores_criteria_merged_civil$mSofiBefore1<-as.numeric(JOMAG_predictores_criteria_merged_civil$mSofiBefore1)
JOMAG_predictores_criteria_merged_civil$mSofiBefore2<-as.numeric(JOMAG_predictores_criteria_merged_civil$mSofiBefore2)
for(i in 1:nrow(JOMAG_predictores_criteria_merged_civil)){
  JOMAG_predictores_criteria_merged_civil[i,]$TsiunSofiBefore<-rowMeans(subset(JOMAG_predictores_criteria_merged_civil[i,],select=c(mSofiBefore1,mSofiBefore2)),na.rm=T)
}

is.nan.data.frame<-function(x)
  do.call(cbind,lapply(x,is.nan))  
JOMAG_predictores_criteria_merged_civil[is.nan(JOMAG_predictores_criteria_merged_civil)]<-NA


JOMAG_predictores_criteria_merged_civil_qv<-JOMAG_predictores_criteria_merged_civil[which(!is.na(JOMAG_predictores_criteria_merged_civil$FileGrade)
                                                                                          & JOMAG_predictores_criteria_merged_civil$critria_count>1),]


# Correlations predictors-criteria
JOMAG_predictores_criteria_merged_civil_qv_relevant_predictors_columns_for_correlations <- JOMAG_predictores_criteria_merged_civil_qv[c(32:61,63:64,66:68,849,1039:1043,1045,1129,1455,1457,1460,1462,1464,1466,1472,1450)]
JOMAG_predictores_criteria_merged_civil_qv_relevant_predictors_columns_names_for_correlations <- c(colnames(JOMAG_predictores_criteria_merged_civil_qv[c(32:61,63:64,66:68,849,1039:1043,1045,1129,1455,1457,1460,1462,1464,1466,1472,1450)]))
JOMAG_predictores_criteria_merged_civil_qv_relevant_criteria_columns_for_correlations <- JOMAG_predictores_criteria_merged_civil_qv[c(887:889,1038,1236,1240,1333,1406,1431,1434,1440,1467)]
JOMAG_predictores_criteria_merged_civil_qv_relevant_criteria_columns_names_for_correlations <- c(colnames(JOMAG_predictores_criteria_merged_civil_qv[c(887:889,1038,1236,1240,1333,1406,1431,1434,1440,1467)]))
JOMAG_predictores_criteria_merged_civil_qv_corr_output<-data.frame()[1:12,]

for(j in 1:length(JOMAG_predictores_criteria_merged_civil_qv_relevant_predictors_columns_names_for_correlations)){
  corr_output_temp<-c()
  for(i in 1:length(JOMAG_predictores_criteria_merged_civil_qv_relevant_predictores_columns_names_for_correlations)){
    corr_temp<-c()
    corr_try <- try(cor.test(as.numeric(JOMAG_predictores_criteria_merged_civil_qv_relevant_predictors_columns_for_correlations[[j]]),as.numeric(JOMAG_predictores_criteria_merged_civil_qv_relevant_criteria_columns_for_correlations[[i]]),use="pairwise.complete.obs"), silent=T)
    corr_temp$"predictor" <-ifelse(class(corr_try)=="try-error", NA, corr_try$estimate)
    corr_temp$p.value <-ifelse(class(corr_try)=="try-error", NA, corr_try$p.value)
    corr_temp$n <-(ifelse(class(corr_try)=="try-error", NA, corr_try$parameter+2))
    corr_temp<-data.frame(corr_temp)
    colnames(corr_temp)[1]<-JOMAG_predictores_criteria_merged_civil_qv_relevant_predictors_columns_names_for_correlations[[j]]
    row.names(corr_temp)<-JOMAG_predictores_criteria_merged_civil_qv_relevant_criteria_columns_names_for_correlations[i]
    corr_output_temp<-rbind (corr_output_temp,corr_temp)
    corr_output_temp <-round(corr_output_temp,2)
  }
  corr_output_temp$""<-"|"
  JOMAG_predictores_criteria_merged_civil_qv_corr_output<-cbind(JOMAG_predictores_criteria_merged_civil_qv_corr_output,corr_output_temp)
}
row.names(JOMAG_predictores_criteria_merged_civil_qv_corr_output)<-JOMAG_predictores_criteria_merged_civil_qv_relevant_criteria_columns_names_for_correlations

for(i in 1:(ncol(JOMAG_predictores_criteria_merged_civil_qv_corr_output)/4)){
  colnames(JOMAG_predictores_criteria_merged_civil_qv_corr_output)[i*4] <- ""
}
write.csv(JOMAG_predictores_criteria_merged_civil_qv_corr_output,file = "C:/Users/Administrator.MAMADA-777/Documents/Junior officers MAGAV validation/JOMAG_predictores_criteria_merged_civil_qv_corr_output_p-c_blue.csv")

# Correlations within predictors.
JOMAG_predictores_criteria_merged_civil_qv_relevant_predictors_columns_for_correlations <- JOMAG_predictores_criteria_merged_civil_qv[c(32:61,63:64,66:68,849,1039:1043,1045,1129,1455,1457,1460,1462,1464,1466,1472,1450)]
JOMAG_predictores_criteria_merged_civil_qv_relevant_predictors_columns_names_for_correlations <- c(colnames(JOMAG_predictores_criteria_merged_civil_qv[c(32:61,63:64,66:68,849,1039:1043,1045,1129,1455,1457,1460,1462,1464,1466,1472,1450)]))
JOMAG_predictores_criteria_merged_civil_qv_corr_output<-data.frame()[1:51,]

for(j in 1:length(JOMAG_predictores_criteria_merged_civil_qv_relevant_predictors_columns_names_for_correlations)){
  corr_output_temp<-c()
  for(i in 1:length(JOMAG_predictores_criteria_merged_civil_qv_relevant_predictors_columns_names_for_correlations)){
    corr_temp<-c()
    corr_try <- try(cor.test(as.numeric(JOMAG_predictores_criteria_merged_civil_qv_relevant_predictors_columns_for_correlations[[j]]),as.numeric(JOMAG_predictores_criteria_merged_civil_qv_relevant_predictors_columns_for_correlations[[i]]),use="pairwise.complete.obs"), silent=T)
    corr_temp$"predictor" <-ifelse(class(corr_try)=="try-error", NA, corr_try$estimate)
    corr_temp$p.value <-ifelse(class(corr_try)=="try-error", NA, corr_try$p.value)
    corr_temp$n <-(ifelse(class(corr_try)=="try-error", NA, corr_try$parameter+2))
    corr_temp<-data.frame(corr_temp)
    colnames(corr_temp)[1]<-JOMAG_predictores_criteria_merged_civil_qv_relevant_predictors_columns_names_for_correlations[[j]]
    row.names(corr_temp)<-JOMAG_predictores_criteria_merged_civil_qv_relevant_predictors_columns_names_for_correlations[i]
    corr_output_temp<-rbind (corr_output_temp,corr_temp)
    corr_output_temp <-round(corr_output_temp,2)
  }
  corr_output_temp$""<-"|"
  JOMAG_predictores_criteria_merged_civil_qv_corr_output<-cbind(JOMAG_predictores_criteria_merged_civil_qv_corr_output,corr_output_temp)
}
row.names(JOMAG_predictores_criteria_merged_civil_qv_corr_output)<-JOMAG_predictores_criteria_merged_civil_qv_relevant_predictors_columns_names_for_correlations

for(i in 1:(ncol(JOMAG_predictores_criteria_merged_civil_qv_corr_output)/4)){
  colnames(JOMAG_predictores_criteria_merged_civil_qv_corr_output)[i*4] <- ""
}
write.csv(JOMAG_predictores_criteria_merged_civil_qv_corr_output,file = "C:/Users/Administrator.MAMADA-777/Documents/Junior officers MAGAV validation/JOMAG_predictores_criteria_merged_civil_qv_corr_output_p-p_blue.csv")

# Correlations within criteria.
JOMAG_predictores_criteria_merged_civil_qv_relevant_criteria_columns_for_correlations <- JOMAG_predictores_criteria_merged_civil_qv[c(887:889,1038,1236,1240,1333,1406,1431,1434,1440,1467)]
JOMAG_predictores_criteria_merged_civil_qv_relevant_criteria_columns_names_for_correlations <- c(colnames(JOMAG_predictores_criteria_merged_civil_qv[c(887:889,1038,1236,1240,1333,1406,1431,1434,1440,1467)]))
JOMAG_predictores_criteria_merged_civil_qv_corr_output<-data.frame()[1:12,]

for(j in 1:length(JOMAG_predictores_criteria_merged_civil_qv_relevant_criteria_columns_names_for_correlations)){
  corr_output_temp<-c()
  for(i in 1:length(JOMAG_predictores_criteria_merged_civil_qv_relevant_criteria_columns_names_for_correlations)){
    corr_temp<-c()
    corr_try <- try(cor.test(as.numeric(JOMAG_predictores_criteria_merged_civil_qv_relevant_criteria_columns_for_correlations[[j]]),as.numeric(JOMAG_predictores_criteria_merged_civil_qv_relevant_criteria_columns_for_correlations[[i]]),use="pairwise.complete.obs"), silent=T)
    corr_temp$"predictor" <-ifelse(class(corr_try)=="try-error", NA, corr_try$estimate)
    corr_temp$p.value <-ifelse(class(corr_try)=="try-error", NA, corr_try$p.value)
    corr_temp$n <-(ifelse(class(corr_try)=="try-error", NA, corr_try$parameter+2))
    corr_temp<-data.frame(corr_temp)
    colnames(corr_temp)[1]<-JOMAG_predictores_criteria_merged_civil_qv_relevant_criteria_columns_names_for_correlations[[j]]
    row.names(corr_temp)<-JOMAG_predictores_criteria_merged_civil_qv_relevant_criteria_columns_names_for_correlations[i]
    corr_output_temp<-rbind (corr_output_temp,corr_temp)
    corr_output_temp <-round(corr_output_temp,2)
  }
  corr_output_temp$""<-"|"
  JOMAG_predictores_criteria_merged_civil_qv_corr_output<-cbind(JOMAG_predictores_criteria_merged_civil_qv_corr_output,corr_output_temp)
}
row.names(JOMAG_predictores_criteria_merged_civil_qv_corr_output)<-JOMAG_predictores_criteria_merged_civil_qv_relevant_criteria_columns_names_for_correlations

for(i in 1:(ncol(JOMAG_predictores_criteria_merged_civil_qv_corr_output)/4)){
  colnames(JOMAG_predictores_criteria_merged_civil_qv_corr_output)[i*4] <- ""
}
write.csv(JOMAG_predictores_criteria_merged_civil_qv_corr_output,file = "C:/Users/Administrator.MAMADA-777/Documents/Junior officers MAGAV validation/JOMAG_predictores_criteria_merged_civil_qv_corr_output_c-c_blue.csv")

# Semi-partial correlations predictors-criteria.
# Run without seniority_days at the predictores list.
library(ppcor)
JOMAG_predictores_criteria_merged_civil_qv_relevant_predictors_columns_for_spcorrelations <- JOMAG_predictores_criteria_merged_civil_qv[c(32:61,63:64,66:68,849,1039:1043,1045,1129,1455,1457,1460,1462,1464,1466,1472)]
JOMAG_predictores_criteria_merged_civil_qv_relevant_predictors_columns_names_for_spcorrelations <- c(colnames(JOMAG_predictores_criteria_merged_civil_qv[c(32:61,63:64,66:68,849,1039:1043,1045,1129,1455,1457,1460,1462,1464,1466,1472)]))
JOMAG_predictores_criteria_merged_civil_qv_relevant_criteria_columns_for_spcorrelations <- JOMAG_predictores_criteria_merged_civil_qv[c(887:889,1038,1236,1240,1333,1406,1431,1434,1440,1467)]
JOMAG_predictores_criteria_merged_civil_qv_relevant_criteria_columns_names_for_spcorrelations <- c(colnames(JOMAG_predictores_criteria_merged_civil_qv[c(887:889,1038,1236,1240,1333,1406,1431,1434,1440,1467)]))
JOMAG_predictores_criteria_merged_civil_qv_spcorr_output<-data.frame()[1:12,]
for(j in 1:length(JOMAG_predictores_criteria_merged_civil_qv_relevant_predictors_columns_names_for_spcorrelations)){
  spcorr_output_temp<-c()
  for(i in 1:length(JOMAG_predictores_criteria_merged_civil_qv_relevant_criteria_columns_names_for_spcorrelations)){
    JOMAG_predictores_criteria_merged_civil_qv_complete <- c()
    JOMAG_predictores_criteria_merged_civil_qv_complete$firstvar <- JOMAG_predictores_criteria_merged_civil_qv_relevant_predictors_columns_for_spcorrelations[[j]]
    JOMAG_predictores_criteria_merged_civil_qv_complete$secondvar <- JOMAG_predictores_criteria_merged_civil_qv_relevant_criteria_columns_for_spcorrelations[[i]]
    JOMAG_predictores_criteria_merged_civil_qv_complete$seniority_days <- JOMAG_predictores_criteria_merged_civil_qv$seniority_days
    JOMAG_predictores_criteria_merged_civil_qv_complete<-as.data.frame(JOMAG_predictores_criteria_merged_civil_qv_complete)
    JOMAG_predictores_criteria_merged_civil_qv_complete <- JOMAG_predictores_criteria_merged_civil_qv_complete[complete.cases(JOMAG_predictores_criteria_merged_civil_qv_complete),]
    spcorr_temp<-c()
    spcorr_try <- try(spcor.test(as.numeric(JOMAG_predictores_criteria_merged_civil_qv_complete[,1]),as.numeric(JOMAG_predictores_criteria_merged_civil_qv_complete[,2]),as.numeric(JOMAG_predictores_criteria_merged_civil_qv_complete[,3])), silent=T)
    spcorr_temp$"predictor" <-ifelse(class(spcorr_try)=="try-error", NA, spcorr_try$estimate)
    spcorr_temp$p.value <-ifelse(class(spcorr_try)=="try-error", NA, spcorr_try$p.value)
    spcorr_temp$n <-(ifelse(class(spcorr_try)=="try-error", NA, spcorr_try$n))
    spcorr_temp<-data.frame(spcorr_temp)
    colnames(spcorr_temp)[1]<-JOMAG_predictores_criteria_merged_civil_qv_relevant_predictors_columns_names_for_spcorrelations[[j]]
    row.names(spcorr_temp)<-JOMAG_predictores_criteria_merged_civil_qv_relevant_criteria_columns_names_for_spcorrelations[i]
    spcorr_output_temp<-rbind (spcorr_output_temp,spcorr_temp)
    spcorr_output_temp <-round(spcorr_output_temp,2)
  }
  spcorr_output_temp$""<-"|"
  JOMAG_predictores_criteria_merged_civil_qv_spcorr_output<-cbind(JOMAG_predictores_criteria_merged_civil_qv_spcorr_output,spcorr_output_temp)
}
row.names(JOMAG_predictores_criteria_merged_civil_qv_spcorr_output)<-JOMAG_predictores_criteria_merged_civil_qv_relevant_criteria_columns_names_for_spcorrelations

for(i in 1:(ncol(JOMAG_predictores_criteria_merged_civil_qv_spcorr_output)/4)){
  colnames(JOMAG_predictores_criteria_merged_civil_qv_spcorr_output)[i*4] <- ""
}
write.csv(JOMAG_predictores_criteria_merged_civil_qv_spcorr_output,file = "C:/Users/Administrator.MAMADA-777/Documents/Junior officers MAGAV validation/JOMAG_predictores_criteria_merged_civil_qv_spcorr_output_p-c_blue.csv")

# t-test of bahad_mak on FileGrade
t_test_temp<-c()
t_test_try <- try(t.test(as.numeric(JOMAG_predictores_criteria_merged_civil$FileGrade)~as.numeric(JOMAG_predictores_criteria_merged_civil$bahad_mak),use="pairwise.complete.obs"), silent=T)
t_test_temp$statistic <-ifelse(class(t_test_try)=="try-error", NA, t_test_try$statistic)
t_test_temp$parameter <-ifelse(class(t_test_try)=="try-error", NA, t_test_try$parameter)
t_test_temp$p.value <-ifelse(class(t_test_try)=="try-error", NA, t_test_try$p.value)
t_test_temp$"mean in group 1" <-ifelse(class(t_test_try)=="try-error", NA, t_test_try$estimate[[1]])
t_test_temp$"mean in group 2" <-ifelse(class(t_test_try)=="try-error", NA, t_test_try$estimate[[2]])
t_test_temp<-data.frame(t_test_temp)
row.names(t_test_temp)<-"FileGrade"
t_test_temp
CrossTable(JOMAG_predictores_criteria_merged_civil$FileGrade,JOMAG_predictores_criteria_merged_civil$bahad_mak,simulate.p.value=TRUE)

# t-test of bahad_mak on SocioGrade
t_test_temp<-c()
t_test_try <- try(t.test(as.numeric(JOMAG_predictores_criteria_merged_civil$SocioGrade)~as.numeric(JOMAG_predictores_criteria_merged_civil$bahad_mak),use="pairwise.complete.obs"), silent=T)
t_test_temp$statistic <-ifelse(class(t_test_try)=="try-error", NA, t_test_try$statistic)
t_test_temp$parameter <-ifelse(class(t_test_try)=="try-error", NA, t_test_try$parameter)
t_test_temp$p.value <-ifelse(class(t_test_try)=="try-error", NA, t_test_try$p.value)
t_test_temp$"mean in group 1" <-ifelse(class(t_test_try)=="try-error", NA, t_test_try$estimate[[1]])
t_test_temp$"mean in group 2" <-ifelse(class(t_test_try)=="try-error", NA, t_test_try$estimate[[2]])
t_test_temp<-data.frame(t_test_temp)
row.names(t_test_temp)<-"SocioGrade"
t_test_temp
CrossTable(JOMAG_predictores_criteria_merged_civil$SocioGrade,JOMAG_predictores_criteria_merged_civil$bahad_mak,simulate.p.value=TRUE)


#-----------------------------------------------------------------------------------------------------------------------------------------------------
# final_apptitudes_new_dic was built for all the data base becouse of predicted_overall_score (bellow)
JOMAG_predictores_criteria_merged_civil$final_apptitudes_new_dic<-ifelse(JOMAG_predictores_criteria_merged_civil$final_apptitudes_new< -0.028327486,0,1)
JOMAG_predictores_criteria_merged_civil_qv<-JOMAG_predictores_criteria_merged_civil[which(!is.na(JOMAG_predictores_criteria_merged_civil$FileGrade)
                                                                                          & JOMAG_predictores_criteria_merged_civil$critria_count>1),]
round(freq(ordered(as.numeric(unlist(JOMAG_predictores_criteria_merged_civil_qv$final_apptitudes_new_dic))), plot = F,main=colnames(JOMAG_predictores_criteria_merged_civil_qv$final_apptitudes_new_dic),font=2),2)


JOMAG_predictores_criteria_merged_civil_qv$final_apptitudes_new_combined_mean<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged_civil_qv)){
  JOMAG_predictores_criteria_merged_civil_qv[i,]$final_apptitudes_new_combined_mean<-rowMeans(subset(JOMAG_predictores_criteria_merged_civil_qv[i,],
  select=c(mKsherimBefore1,mKsherimAfter1,mKsherimBefore2,mKsherimAfter2,final_apptitudes_new)),na.rm=T)
}

# Regression analysis

library(QuantPsyc)  # lm.beta
library(car)  # vif, durbinWatsonTest
library(MASS)  # studres
library(lmSupport)  #lm.sumSquares
library(perturb)  # colldiag
library(regtools)  # pairwise

reg_tkufatitam <- lm(tkufatitam ~ final_apptitudes_new
                                + TsiunSofiAfter
                                + SocioGrade
                                + PersonalityGrade,
                                data=JOMAG_predictores_criteria_merged_civil_qv)
summary(reg_tkufatitam)
# standardised coefficients
round(lm.beta(reg_tkufatitam),2)

# R
R<-round(sqrt(0.1892),2)
R
#---------------------------------------------
reg_tkufatitam <- lm(tkufatitam ~ TsiunSofiAfter
                     + SocioGrade
                     + PersonalityGrade,
                     data=JOMAG_predictores_criteria_merged_civil_qv)
summary(reg_tkufatitam)
# standardised coefficients
round(lm.beta(reg_tkufatitam),2)

# R
R<-round(sqrt(0.1694),2)
R
#---------------------------------------------
# Good***********
reg_tkufatitam <- lm(tkufatitam ~ final_apptitudes_new_dic
                     + TsiunSofiAfter
                     + SocioGrade
                     + PersonalityGrade,
                     data=JOMAG_predictores_criteria_merged_civil_qv)
summary(reg_tkufatitam)
# standardised coefficients
# round(lm.beta(reg_tkufatitam),2)
round(lm.beta(reg_tkufatitam),2)

# R
R<-round(sqrt(0.1649),2)
R

#B
round(0.01790,2)
round(0.02880,2)
round(0.16181,2)
round(0.16900,2)

# round(0.01790,4)
# round(0.02880,4)
# round(0.16181,4)
# round(0.16900,4)


Weights
weights<-c()
Overall_weights<-sum(abs(lm.beta(reg_tkufatitam)))
weights$final_apptitudes_new_dic_weight<-as.numeric(unlist(round(lm.beta(reg_tkufatitam)[1]/Overall_weights,2)*100))
weights$TsiunSofiAfter_weight<-as.numeric(unlist(round(lm.beta(reg_tkufatitam)[2]/Overall_weights,2)*100))
weights$SocioGrade_weight<-as.numeric(unlist(round(lm.beta(reg_tkufatitam)[3]/Overall_weights,2)*100))
weights$PersonalityGrade_weight<-as.numeric(unlist(round(lm.beta(reg_tkufatitam)[4]/Overall_weights,2)*100))
weights<-as.data.frame(weights)
rownames(weights)<-"weights"
weights$sum_abs<-sum(as.numeric(abs(weights[1:4])))
weights[] <- lapply(weights, function(x) paste(x,"%"))
weights


weights<-c()
Overall_weights<-sum(abs(lm.beta(reg_tkufatitam)))
weights$final_apptitudes_new_dic_weight<-as.numeric(unlist(round(lm.beta(reg_tkufatitam)[1]/Overall_weights,4)*100))
weights$TsiunSofiAfter_weight<-as.numeric(unlist(round(lm.beta(reg_tkufatitam)[2]/Overall_weights,4)*100))
weights$SocioGrade_weight<-as.numeric(unlist(round(lm.beta(reg_tkufatitam)[3]/Overall_weights,4)*100))
weights$PersonalityGrade_weight<-as.numeric(unlist(round(lm.beta(reg_tkufatitam)[4]/Overall_weights,4)*100))
weights<-as.data.frame(weights)
rownames(weights)<-"weights"
weights$sum_abs<-sum(as.numeric(abs(weights[1:4])))
weights[] <- lapply(weights, function(x) paste(x,"%"))
weights

#---------------------------------------------
reg_tkufatitam <- lm(tkufatitam ~ final_apptitudes_new_combined_mean
                     + TsiunSofiAfter
                     + SocioGrade
                     + PersonalityGrade,
                     data=JOMAG_predictores_criteria_merged_civil_qv)
summary(reg_tkufatitam)
# standardised coefficients
round(lm.beta(reg_tkufatitam),2)

# R
R<-sqrt(0.1772)
R

#---------------------------------------------
# predicted_overall_score
JOMAG_predictores_criteria_merged_civil_qv$predicted_overall_score<-NA
for (i in 1:nrow(JOMAG_predictores_criteria_merged_civil_qv)) {
  if(!is.na(JOMAG_predictores_criteria_merged_civil_qv[i,]$final_apptitudes_new_dic) | 
     !is.na(JOMAG_predictores_criteria_merged_civil_qv[i,]$TsiunSofiAfter_zscore) |
     !is.na(JOMAG_predictores_criteria_merged_civil_qv[i,]$SocioGrade_zscore)|
     !is.na(JOMAG_predictores_criteria_merged_civil_qv[i,]$PersonalityGrade_zscore)){
    JOMAG_predictores_criteria_merged_civil_qv[i,]$predicted_overall_score <-
      sum(.03*JOMAG_predictores_criteria_merged_civil_qv[i,]$final_apptitudes_new_dic,
          .07*JOMAG_predictores_criteria_merged_civil_qv[i,]$TsiunSofiAfter_zscore,
          .53*JOMAG_predictores_criteria_merged_civil_qv[i,]$SocioGrade_zscore,
          .37*JOMAG_predictores_criteria_merged_civil_qv[i,]$PersonalityGrade_zscore,na.rm = T)}
  else {
    JOMAG_predictores_criteria_merged_civil_qv[i,]$predicted_overall_score <-NA
  }
}

# abs_residual_predicted_overall_score
JOMAG_predictores_criteria_merged_civil$abs_residual_predicted_overall_score <-
    abs(JOMAG_predictores_criteria_merged_civil$tkufatitam-JOMAG_predictores_criteria_merged_civil$predicted_overall_score)
JOMAG_predictores_criteria_merged_civil_qv<-JOMAG_predictores_criteria_merged_civil[which(!is.na(JOMAG_predictores_criteria_merged_civil$FileGrade)
                                                                                          & JOMAG_predictores_criteria_merged_civil$critria_count>1),]

mean_abs_residual_predicted_overall_score<-mean(JOMAG_predictores_criteria_merged_civil$abs_residual_predicted_overall_score,na.rm = T)
mean_abs_residual_predicted_overall_score

# abs_residual_formula_score
JOMAG_predictores_criteria_merged_civil$abs_residual_formula_score <-
    abs(JOMAG_predictores_criteria_merged_civil$tkufatitam-JOMAG_predictores_criteria_merged_civil$formula_score)

mean_abs_residual_formula_score<-mean(JOMAG_predictores_criteria_merged_civil$abs_residual_formula_score,na.rm = T)
mean_abs_residual_formula_score

# diff_res_formula_predicted
diff_res_formula_predicted<-mean_abs_residual_formula_score-mean_abs_residual_predicted_overall_score
diff_res_formula_predicted

JOMAG_predictores_criteria_merged_civil_qv<-JOMAG_predictores_criteria_merged_civil[which(!is.na(JOMAG_predictores_criteria_merged_civil$FileGrade)
                                                                                          & JOMAG_predictores_criteria_merged_civil$critria_count>1),]
# Correlation between predicted_overall_score and criterion 
cor.test(as.numeric(JOMAG_predictores_criteria_merged_civil_qv$predicted_overall_score),as.numeric(JOMAG_predictores_criteria_merged_civil_qv$tkufatitam),use="pairwise.complete.obs")
#---------------------------------------------

# predicted_overall_score_organizational1
JOMAG_predictores_criteria_merged_civil_qv$predicted_overall_score_organizational1<-NA
for (i in 1:nrow(JOMAG_predictores_criteria_merged_civil_qv)) {
  if(!is.na(JOMAG_predictores_criteria_merged_civil_qv[i,]$final_apptitudes_new_dic) | 
     !is.na(JOMAG_predictores_criteria_merged_civil_qv[i,]$TsiunSofiAfter_zscore) |
     !is.na(JOMAG_predictores_criteria_merged_civil_qv[i,]$SocioGrade_zscore)|
     !is.na(JOMAG_predictores_criteria_merged_civil_qv[i,]$PersonalityGrade_zscore)){
    JOMAG_predictores_criteria_merged_civil_qv[i,]$predicted_overall_score_organizational1 <-
      sum(.05*JOMAG_predictores_criteria_merged_civil_qv[i,]$final_apptitudes_new_dic,
          .31*JOMAG_predictores_criteria_merged_civil_qv[i,]$TsiunSofiAfter_zscore,
          .32*JOMAG_predictores_criteria_merged_civil_qv[i,]$SocioGrade_zscore,
          .32*JOMAG_predictores_criteria_merged_civil_qv[i,]$PersonalityGrade_zscore,na.rm = T)}
  else {
    JOMAG_predictores_criteria_merged_civil_qv[i,]$predicted_overall_score_organizational1 <-NA
  }
}

# abs_residual_predicted_overall_score_organizational1
JOMAG_predictores_criteria_merged_civil$abs_residual_predicted_overall_score_organizational1 <-
  abs(JOMAG_predictores_criteria_merged_civil$tkufatitam-JOMAG_predictores_criteria_merged_civil$predicted_overall_score_organizational1)
JOMAG_predictores_criteria_merged_civil_qv<-JOMAG_predictores_criteria_merged_civil[which(!is.na(JOMAG_predictores_criteria_merged_civil$FileGrade)
                                                                                          & JOMAG_predictores_criteria_merged_civil$critria_count>1),]

mean_abs_residual_predicted_overall_score_organizational1<-mean(JOMAG_predictores_criteria_merged_civil$abs_residual_predicted_overall_score_organizational1,na.rm = T)
mean_abs_residual_predicted_overall_score_organizational1

  # abs_residual_formula_score
JOMAG_predictores_criteria_merged_civil$abs_residual_formula_score <-
  abs(JOMAG_predictores_criteria_merged_civil$tkufatitam-JOMAG_predictores_criteria_merged_civil$formula_score)

mean_abs_residual_formula_score<-mean(JOMAG_predictores_criteria_merged_civil$abs_residual_formula_score,na.rm = T)
mean_abs_residual_formula_score

# diff_res_formula_organizational1
diff_res_formula_organizational1<-mean_abs_residual_formula_score-mean_abs_residual_predicted_overall_score_organizational1
diff_res_formula_organizational1

JOMAG_predictores_criteria_merged_civil_qv<-JOMAG_predictores_criteria_merged_civil[which(!is.na(JOMAG_predictores_criteria_merged_civil$FileGrade)
                                                                                          & JOMAG_predictores_criteria_merged_civil$critria_count>1),]

# Correlation between predicted_overall_score_organizational1 and criterion 
cor.test(as.numeric(JOMAG_predictores_criteria_merged_civil_qv$predicted_overall_score_organizational1),as.numeric(JOMAG_predictores_criteria_merged_civil_qv$tkufatitam),use="pairwise.complete.obs")
#---------------------------------------------

# predicted_overall_score_organizational2
JOMAG_predictores_criteria_merged_civil_qv$predicted_overall_score_organizational2<-NA
for (i in 1:nrow(JOMAG_predictores_criteria_merged_civil_qv)) {
  if(!is.na(JOMAG_predictores_criteria_merged_civil_qv[i,]$final_apptitudes_new_dic) | 
     !is.na(JOMAG_predictores_criteria_merged_civil_qv[i,]$TsiunSofiAfter_zscore) |
     !is.na(JOMAG_predictores_criteria_merged_civil_qv[i,]$SocioGrade_zscore)|
     !is.na(JOMAG_predictores_criteria_merged_civil_qv[i,]$PersonalityGrade_zscore)){
    JOMAG_predictores_criteria_merged_civil_qv[i,]$predicted_overall_score_organizational2 <-
      sum(.05*JOMAG_predictores_criteria_merged_civil_qv[i,]$final_apptitudes_new_dic,
          .30*JOMAG_predictores_criteria_merged_civil_qv[i,]$TsiunSofiAfter_zscore,
          .38*JOMAG_predictores_criteria_merged_civil_qv[i,]$SocioGrade_zscore,
          .27*JOMAG_predictores_criteria_merged_civil_qv[i,]$PersonalityGrade_zscore,na.rm = T)}
  else {
    JOMAG_predictores_criteria_merged_civil_qv[i,]$predicted_overall_score_organizational2 <-NA
  }
}

# abs_residual_predicted_overall_score_organizational2
JOMAG_predictores_criteria_merged_civil$abs_residual_predicted_overall_score_organizational2 <-
  abs(JOMAG_predictores_criteria_merged_civil$tkufatitam-JOMAG_predictores_criteria_merged_civil$predicted_overall_score_organizational2)
JOMAG_predictores_criteria_merged_civil_qv<-JOMAG_predictores_criteria_merged_civil[which(!is.na(JOMAG_predictores_criteria_merged_civil$FileGrade)
                                                                                          & JOMAG_predictores_criteria_merged_civil$critria_count>1),]

mean_abs_residual_predicted_overall_score_organizational2<-mean(JOMAG_predictores_criteria_merged_civil$abs_residual_predicted_overall_score_organizational2,na.rm = T)
mean_abs_residual_predicted_overall_score_organizational2

# abs_residual_formula_score
JOMAG_predictores_criteria_merged_civil$abs_residual_formula_score <-
  abs(JOMAG_predictores_criteria_merged_civil$tkufatitam-JOMAG_predictores_criteria_merged_civil$formula_score)

mean_abs_residual_formula_score<-mean(JOMAG_predictores_criteria_merged_civil$abs_residual_formula_score,na.rm = T)
mean_abs_residual_formula_score

# diff_res_formula_organizational2
diff_res_formula_organizational2<-mean_abs_residual_formula_score-mean_abs_residual_predicted_overall_score_organizational2
diff_res_formula_organizational2

JOMAG_predictores_criteria_merged_civil_qv<-JOMAG_predictores_criteria_merged_civil[which(!is.na(JOMAG_predictores_criteria_merged_civil$FileGrade)
                                                                                          & JOMAG_predictores_criteria_merged_civil$critria_count>1),]

# Correlation between predicted_overall_score_organizational2 and criterion 
cor.test(as.numeric(JOMAG_predictores_criteria_merged_civil_qv$predicted_overall_score_organizational2),as.numeric(JOMAG_predictores_criteria_merged_civil_qv$tkufatitam),use="pairwise.complete.obs")
#---------------------------------------------

# predicted_overall_score_organizational3
JOMAG_predictores_criteria_merged_civil_qv$predicted_overall_score_organizational3<-NA
for (i in 1:nrow(JOMAG_predictores_criteria_merged_civil_qv)) {
  if(!is.na(JOMAG_predictores_criteria_merged_civil_qv[i,]$final_apptitudes_new_dic) | 
     !is.na(JOMAG_predictores_criteria_merged_civil_qv[i,]$TsiunSofiAfter_zscore) |
     !is.na(JOMAG_predictores_criteria_merged_civil_qv[i,]$SocioGrade_zscore)|
     !is.na(JOMAG_predictores_criteria_merged_civil_qv[i,]$PersonalityGrade_zscore)){
    JOMAG_predictores_criteria_merged_civil_qv[i,]$predicted_overall_score_organizational3 <-
      sum(.05*JOMAG_predictores_criteria_merged_civil_qv[i,]$final_apptitudes_new_dic,
          .25*JOMAG_predictores_criteria_merged_civil_qv[i,]$TsiunSofiAfter_zscore,
          .40*JOMAG_predictores_criteria_merged_civil_qv[i,]$SocioGrade_zscore,
          .30*JOMAG_predictores_criteria_merged_civil_qv[i,]$PersonalityGrade_zscore,na.rm = T)}
  else {
    JOMAG_predictores_criteria_merged_civil_qv[i,]$predicted_overall_score_organizational3 <-NA
  }
}

# abs_residual_predicted_overall_score_organizational3
JOMAG_predictores_criteria_merged_civil$abs_residual_predicted_overall_score_organizational3 <-
  abs(JOMAG_predictores_criteria_merged_civil$tkufatitam-JOMAG_predictores_criteria_merged_civil$predicted_overall_score_organizational3)
JOMAG_predictores_criteria_merged_civil_qv<-JOMAG_predictores_criteria_merged_civil[which(!is.na(JOMAG_predictores_criteria_merged_civil$FileGrade)
                                                                                          & JOMAG_predictores_criteria_merged_civil$critria_count>1),]

mean_abs_residual_predicted_overall_score_organizational3<-mean(JOMAG_predictores_criteria_merged_civil$abs_residual_predicted_overall_score_organizational3,na.rm = T)
mean_abs_residual_predicted_overall_score_organizational3

# abs_residual_formula_score
JOMAG_predictores_criteria_merged_civil$abs_residual_formula_score <-
  abs(JOMAG_predictores_criteria_merged_civil$tkufatitam-JOMAG_predictores_criteria_merged_civil$formula_score)

mean_abs_residual_formula_score<-mean(JOMAG_predictores_criteria_merged_civil$abs_residual_formula_score,na.rm = T)
mean_abs_residual_formula_score

# diff_res_formula_organizational3
diff_res_formula_organizational3<-mean_abs_residual_formula_score-mean_abs_residual_predicted_overall_score_organizational3
diff_res_formula_organizational3

JOMAG_predictores_criteria_merged_civil_qv<-JOMAG_predictores_criteria_merged_civil[which(!is.na(JOMAG_predictores_criteria_merged_civil$FileGrade)
                                                                                          & JOMAG_predictores_criteria_merged_civil$critria_count>1),]
# Correlation between predicted_overall_score_organizational3 and criterion 
cor.test(as.numeric(JOMAG_predictores_criteria_merged_civil_qv$predicted_overall_score_organizational3),as.numeric(JOMAG_predictores_criteria_merged_civil_qv$tkufatitam),use="pairwise.complete.obs")

JOMAG_predictores_criteria_merged_civil_qv<-JOMAG_predictores_criteria_merged_civil[which(!is.na(JOMAG_predictores_criteria_merged_civil$FileGrade)
                                                                                          & JOMAG_predictores_criteria_merged_civil$critria_count>1),]

#---------------------------------------------
# Range restriction.
# The variance of all the sample of candidates in the A.C. should be higher then the variance of the sample that I performed on it
# the validation study (after the various filterings)

library (descr)
library (psych)

try(cor.test(as.numeric(JOMAG_predictores_criteria_merged_civil_qv$FileGrade),as.numeric(JOMAG_predictores_criteria_merged_civil_qv$officerssochach),use="pairwise.complete.obs"), silent=T)
round(describe (as.numeric(JOMAG_predictores_criteria_merged_civil$FileGrade)),2)

JOMAG_predictores_criteria_merged_civil_qv$FileGrade_restricted<-JOMAG_predictores_criteria_merged_civil_qv$FileGrade

round(describe (as.numeric(JOMAG_predictores_criteria_merged_civil_qv$FileGrade_restricted)),2)

#-----------------------------------------------------------------------------------------------------------------------------------------------------
library (descr)
library (psych)

try(cor.test(as.numeric(JOMAG_predictores_criteria_merged_civil_qv$VaadaGrade_completed),as.numeric(JOMAG_predictores_criteria_merged_civil_qv$officerssochach),use="pairwise.complete.obs"), silent=T)
round(describe (as.numeric(JOMAG_predictores_criteria_merged_civil$VaadaGrade_completed)),2)

JOMAG_predictores_criteria_merged_civil_qv$VaadaGrade_completed_restricted<-JOMAG_predictores_criteria_merged_civil_qv$VaadaGrade_completed

round(describe (as.numeric(JOMAG_predictores_criteria_merged_civil_qv$VaadaGrade_completed_restricted)),2)

#-----------------------------------------------------------------------------------------------------------------------------------------------------
try(cor.test(as.numeric(JOMAG_predictores_criteria_merged_civil_qv$formula_score),as.numeric(JOMAG_predictores_criteria_merged_civil_qv$tkufatitam),use="pairwise.complete.obs"), silent=T)
round(describe (as.numeric(JOMAG_predictores_criteria_merged_civil$formula_score)),2)

JOMAG_predictores_criteria_merged_civil_qv$formula_score_restricted<-JOMAG_predictores_criteria_merged_civil_qv$formula_score

round(describe (as.numeric(JOMAG_predictores_criteria_merged_civil_qv$formula_score)),2)

#-----------------------------------------------------------------------------------------------------------------------------------------------------
try(cor.test(as.numeric(JOMAG_predictores_criteria_merged_civil_qv$predicted_overall_score),as.numeric(JOMAG_predictores_criteria_merged_civil_qv$tkufatitam),use="pairwise.complete.obs"), silent=T)
round(describe (as.numeric(JOMAG_predictores_criteria_merged_civil$predicted_overall_score)),2)

JOMAG_predictores_criteria_merged_civil_qv$predicted_overall_score_restricted<-JOMAG_predictores_criteria_merged_civil_qv$predicted_overall_score

round(describe (as.numeric(JOMAG_predictores_criteria_merged_civil_qv$predicted_overall_score_restricted)),2)

#-----------------------------------------------------------------------------------------------------------------------------------------------------
try(cor.test(as.numeric(JOMAG_predictores_criteria_merged_civil_qv$predicted_overall_score_organizational1),as.numeric(JOMAG_predictores_criteria_merged_civil_qv$tkufatitam),use="pairwise.complete.obs"), silent=T)

round(describe (as.numeric(JOMAG_predictores_criteria_merged_civil$predicted_overall_score_organizational1)),2)

JOMAG_predictores_criteria_merged_civil_qv$predicted_overall_score_organizational1_restricted<-JOMAG_predictores_criteria_merged_civil_qv$predicted_overall_score_organizational1

round(describe (as.numeric(JOMAG_predictores_criteria_merged_civil_qv$predicted_overall_score_organizational1_restricted)),2)

#-----------------------------------------------------------------------------------------------------------------------------------------------------
try(cor.test(as.numeric(JOMAG_predictores_criteria_merged_civil_qv$predicted_overall_score_organizational2),as.numeric(JOMAG_predictores_criteria_merged_civil_qv$tkufatitam),use="pairwise.complete.obs"), silent=T)

round(describe (as.numeric(JOMAG_predictores_criteria_merged_civil$predicted_overall_score_organizational2)),2)

JOMAG_predictores_criteria_merged_civil_qv$predicted_overall_score_organizational2_restricted<-JOMAG_predictores_criteria_merged_civil_qv$predicted_overall_score_organizational2

round(describe (as.numeric(JOMAG_predictores_criteria_merged_civil_qv$predicted_overall_score_organizational2_restricted)),2)

#-----------------------------------------------------------------------------------------------------------------------------------------------------
try(cor.test(as.numeric(JOMAG_predictores_criteria_merged_civil_qv$predicted_overall_score_organizational3),as.numeric(JOMAG_predictores_criteria_merged_civil_qv$tkufatitam),use="pairwise.complete.obs"), silent=T)

round(describe (as.numeric(JOMAG_predictores_criteria_merged_civil$predicted_overall_score_organizational3)),2)

JOMAG_predictores_criteria_merged_civil_qv$predicted_overall_score_organizational3_restricted<-JOMAG_predictores_criteria_merged_civil_qv$predicted_overall_score_organizational3

round(describe (as.numeric(JOMAG_predictores_criteria_merged_civil_qv$predicted_overall_score_organizational3_restricted)),2)

#-----------------------------------------------------------------------------------------------------------------------------------------------------
try(cor.test(as.numeric(JOMAG_predictores_criteria_merged_civil_qv$PersonalityGrade),as.numeric(JOMAG_predictores_criteria_merged_civil_qv$tkufatitam),use="pairwise.complete.obs"), silent=T)

round(describe (as.numeric(JOMAG_predictores_criteria_merged_civil$PersonalityGrade)),2)

JOMAG_predictores_criteria_merged_civil_qv$PersonalityGrade_restricted<-JOMAG_predictores_criteria_merged_civil_qv$PersonalityGrade

round(describe (as.numeric(JOMAG_predictores_criteria_merged_civil_qv$PersonalityGrade_restricted)),2)

#-----------------------------------------------------------------------------------------------------------------------------------------------------
try(cor.test(as.numeric(JOMAG_predictores_criteria_merged_civil_qv$SocioGrade),as.numeric(JOMAG_predictores_criteria_merged_civil_qv$tkufatitam),use="pairwise.complete.obs"), silent=T)

round(describe (as.numeric(JOMAG_predictores_criteria_merged_civil$SocioGrade)),2)

JOMAG_predictores_criteria_merged_civil_qv$SocioGrade_restricted<-JOMAG_predictores_criteria_merged_civil_qv$SocioGrade

round(describe (as.numeric(JOMAG_predictores_criteria_merged_civil_qv$SocioGrade_restricted)),2)

#-----------------------------------------------------------------------------------------------------------------------------------------------------
try(cor.test(as.numeric(JOMAG_predictores_criteria_merged_civil_qv$TsiunSofiAfter),as.numeric(JOMAG_predictores_criteria_merged_civil_qv$tkufatitam),use="pairwise.complete.obs"), silent=T)

round(describe (as.numeric(JOMAG_predictores_criteria_merged_civil$TsiunSofiAfter)),2)

JOMAG_predictores_criteria_merged_civil_qv$TsiunSofiAfter_restricted<-JOMAG_predictores_criteria_merged_civil_qv$TsiunSofiAfter

round(describe (as.numeric(JOMAG_predictores_criteria_merged_civil_qv$TsiunSofiAfter_restricted)),2)

#-----------------------------------------------------------------------------------------------------------------------------------------------------
try(cor.test(as.numeric(JOMAG_predictores_criteria_merged_civil_qv$TsiunSofiBefore),as.numeric(JOMAG_predictores_criteria_merged_civil_qv$tkufatitam),use="pairwise.complete.obs"), silent=T)

round(describe (as.numeric(JOMAG_predictores_criteria_merged_civil$TsiunSofiBefore)),2)

JOMAG_predictores_criteria_merged_civil_qv$TsiunSofiBefore_restricted<-JOMAG_predictores_criteria_merged_civil_qv$TsiunSofiBefore

round(describe (as.numeric(JOMAG_predictores_criteria_merged_civil_qv$TsiunSofiBefore_restricted)),2)

#-----------------------------------------------------------------------------------------------------------------------------------------------------
try(cor.test(as.numeric(JOMAG_predictores_criteria_merged_civil_qv$mSofiBefore1),as.numeric(JOMAG_predictores_criteria_merged_civil_qv$tkufatitam),use="pairwise.complete.obs"), silent=T)

round(describe (as.numeric(JOMAG_predictores_criteria_merged_civil$mSofiBefore1)),2)

JOMAG_predictores_criteria_merged_civil_qv$mSofiBefore1_restricted<-JOMAG_predictores_criteria_merged_civil_qv$mSofiBefore1

round(describe (as.numeric(JOMAG_predictores_criteria_merged_civil_qv$mSofiBefore1_restricted)),2)

#-----------------------------------------------------------------------------------------------------------------------------------------------------
try(cor.test(as.numeric(JOMAG_predictores_criteria_merged_civil_qv$mSofiBefore2),as.numeric(JOMAG_predictores_criteria_merged_civil_qv$tkufatitam),use="pairwise.complete.obs"), silent=T)

round(describe (as.numeric(JOMAG_predictores_criteria_merged_civil$mSofiBefore2)),2)

JOMAG_predictores_criteria_merged_civil_qv$mSofiBefore2_restricted<-JOMAG_predictores_criteria_merged_civil_qv$mSofiBefore2

round(describe (as.numeric(JOMAG_predictores_criteria_merged_civil_qv$mSofiBefore2_restricted)),2)

#-----------------------------------------------------------------------------------------------------------------------------------------------------
try(cor.test(as.numeric(JOMAG_predictores_criteria_merged_civil_qv$mSofiAfter1),as.numeric(JOMAG_predictores_criteria_merged_civil_qv$tkufatitam),use="pairwise.complete.obs"), silent=T)

round(describe (as.numeric(JOMAG_predictores_criteria_merged_civil$mSofiAfter1)),2)

JOMAG_predictores_criteria_merged_civil_qv$mSofiAfter1_restricted<-JOMAG_predictores_criteria_merged_civil_qv$mSofiAfter1

round(describe (as.numeric(JOMAG_predictores_criteria_merged_civil_qv$mSofiAfter1_restricted)),2)

#-----------------------------------------------------------------------------------------------------------------------------------------------------
try(cor.test(as.numeric(JOMAG_predictores_criteria_merged_civil_qv$mSofiAfter2),as.numeric(JOMAG_predictores_criteria_merged_civil_qv$tkufatitam),use="pairwise.complete.obs"), silent=T)

round(describe (as.numeric(JOMAG_predictores_criteria_merged_civil$mSofiAfter2)),2)

JOMAG_predictores_criteria_merged_civil_qv$mSofiAfter2_restricted<-JOMAG_predictores_criteria_merged_civil_qv$mSofiAfter2

round(describe (as.numeric(JOMAG_predictores_criteria_merged_civil_qv$mSofiAfter2_restricted)),2)

#-----------------------------------------------------------------------------------------------------------------------------------------------------
# Average number of repeated criteria (for n in range restriction Excel file).

round(freq(ordered(JOMAG_predictores_criteria_merged_civil_qv$tkufatit_nna), plot = F,main=colnames(JOMAG_predictores_criteria_merged_civil_qv$tkufatit_nna),font=2),2)
notna_tkufatit_average<-(6*1+35*2+42*3+74*4)/(6+35+42+74)
notna_tkufatit_average

round(freq(ordered(JOMAG_predictores_criteria_merged_civil_qv$am_nna), plot = F,main=colnames(JOMAG_predictores_criteria_merged_civil_qv$am_nna),font=2),2)
notna_am_average<-(77*1+54*2)/(77+54)
notna_am_average


#DAPAR

DAPAR_GIBUSH_MAGAV_civil<-read.csv("C:/Users/Administrator.MAMADA-777/Documents/Junior officers MAGAV validation/DAPAR_GIBUSH_MAGAV_civil.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
DAPAR_GIBUSH_MAGAV_civil<-DAPAR_GIBUSH_MAGAV_civil[-1]

JOMAG_predictores_criteria_merged_civil_qv <- merge(JOMAG_predictores_criteria_merged_civil_qv, DAPAR_GIBUSH_MAGAV_civil,by=c("id"), all.x=T, all.y=F,sort = FALSE)

# Correlations between DAPAR and criteria.
JOMAG_predictores_criteria_merged_civil_qv_relevant_predictors_columns_for_correlations <- JOMAG_predictores_criteria_merged_civil_qv[c(1481)]
JOMAG_predictores_criteria_merged_civil_qv_relevant_predictors_columns_names_for_correlations <- c(colnames(JOMAG_predictores_criteria_merged_civil_qv[c(1481)]))
JOMAG_predictores_criteria_merged_civil_qv_relevant_criteria_columns_for_correlations <- JOMAG_predictores_criteria_merged_civil_qv[c(887:889,1038,1236,1240,1333,1406,1431,1434,1440,1467)]
JOMAG_predictores_criteria_merged_civil_qv_relevant_criteria_columns_names_for_correlations <- c(colnames(JOMAG_predictores_criteria_merged_civil_qv[c(887:889,1038,1236,1240,1333,1406,1431,1434,1440,1467)]))
JOMAG_predictores_criteria_merged_civil_qv_corr_output<-data.frame()[1:12,]

for(j in 1:length(JOMAG_predictores_criteria_merged_civil_qv_relevant_predictors_columns_names_for_correlations)){
  corr_output_temp<-c()
  for(i in 1:length(JOMAG_predictores_criteria_merged_civil_qv_relevant_criteria_columns_names_for_correlations)){
    corr_temp<-c()
    corr_try <- try(cor.test(as.numeric(JOMAG_predictores_criteria_merged_civil_qv_relevant_predictors_columns_for_correlations[[j]]),as.numeric(JOMAG_predictores_criteria_merged_civil_qv_relevant_criteria_columns_for_correlations[[i]]),use="pairwise.complete.obs"), silent=T)
    corr_temp$"predictor" <-ifelse(class(corr_try)=="try-error", NA, corr_try$estimate)
    corr_temp$p.value <-ifelse(class(corr_try)=="try-error", NA, corr_try$p.value)
    corr_temp$n <-(ifelse(class(corr_try)=="try-error", NA, corr_try$parameter+2))
    corr_temp<-data.frame(corr_temp)
    colnames(corr_temp)[1]<-JOMAG_predictores_criteria_merged_civil_qv_relevant_predictors_columns_names_for_correlations[[j]]
    row.names(corr_temp)<-JOMAG_predictores_criteria_merged_civil_qv_relevant_criteria_columns_names_for_correlations[i]
    corr_output_temp<-rbind (corr_output_temp,corr_temp)
    corr_output_temp <-round(corr_output_temp,2)
  }
  corr_output_temp$""<-"|"
  JOMAG_predictores_criteria_merged_civil_qv_corr_output<-cbind(JOMAG_predictores_criteria_merged_civil_qv_corr_output,corr_output_temp)
}
row.names(JOMAG_predictores_criteria_merged_civil_qv_corr_output)<-JOMAG_predictores_criteria_merged_civil_qv_relevant_criteria_columns_names_for_correlations

for(i in 1:(ncol(JOMAG_predictores_criteria_merged_civil_qv_corr_output)/4)){
  colnames(JOMAG_predictores_criteria_merged_civil_qv_corr_output)[i*4] <- ""
}
write.csv(JOMAG_predictores_criteria_merged_civil_qv_corr_output,file = "C:/Users/Administrator.MAMADA-777/Documents/Junior officers MAGAV validation/JOMAG_predictores_criteria_merged_civil_qv_corr_output_blue_DAPAR.csv")

# Various statistics

library(dplyr)

JOMAG_predictores_criteria_merged_civil_qv = JOMAG_predictores_criteria_merged_civil_qv %>% 
  rowwise() %>%
  mutate(mPikudBefore = mean(mPikudBefore1_zscore,mPikudBefore2_zscore,na.rm = T),
         mNihulBefore = mean(mNihulBefore1_zscore,mNihulBefore2_zscore,na.rm = T),
         mKsherimBefore = mean(mKsherimBefore1_zscore,mKsherimBefore2_zscore,na.rm = T),
         mYachasimBefore = mean(mYachasimBefore1_zscore,mYachasimBefore2_zscore,na.rm = T),
         mYitzugBefore = mean(mYitzugBefore1_zscore,mYitzugBefore2_zscore,na.rm = T),
         mPikudAfter = mean(mPikudAfter1_zscore,mPikudAfter2_zscore,na.rm = T),
         mNihulAfter = mean(mNihulAfter1_zscore,mNihulAfter2_zscore,na.rm = T),
         mKsherimAfter = mean(mKsherimAfter1_zscore,mKsherimAfter2_zscore,na.rm = T),
         mYachasimAfter = mean(mYachasimAfter1_zscore,mYachasimAfter2_zscore,na.rm = T),
         mYitzugAfter = mean(mYitzugAfter1_zscore,mYitzugAfter2_zscore,na.rm = T)
         )

# Correlations between GIBUSH dimentions and criteria.
JOMAG_predictores_criteria_merged_civil_qv_relevant_predictors_columns_for_correlations <- JOMAG_predictores_criteria_merged_civil_qv[c(1487:1496)]
JOMAG_predictores_criteria_merged_civil_qv_relevant_predictors_columns_names_for_correlations <- c(colnames(JOMAG_predictores_criteria_merged_civil_qv[c(1487:1496)]))
JOMAG_predictores_criteria_merged_civil_qv_relevant_criteria_columns_for_correlations <- JOMAG_predictores_criteria_merged_civil_qv[c(887:889,1038,1236,1240,1333,1406,1431,1434,1440,1467)]
JOMAG_predictores_criteria_merged_civil_qv_relevant_criteria_columns_names_for_correlations <- c(colnames(JOMAG_predictores_criteria_merged_civil_qv[c(887:889,1038,1236,1240,1333,1406,1431,1434,1440,1467)]))
JOMAG_predictores_criteria_merged_civil_qv_corr_output<-data.frame()[1:12,]

for(j in 1:length(JOMAG_predictores_criteria_merged_civil_qv_relevant_predictors_columns_names_for_correlations)){
  corr_output_temp<-c()
  for(i in 1:length(JOMAG_predictores_criteria_merged_civil_qv_relevant_criteria_columns_names_for_correlations)){
    corr_temp<-c()
    corr_try <- try(cor.test(as.numeric(JOMAG_predictores_criteria_merged_civil_qv_relevant_predictors_columns_for_correlations[[j]]),as.numeric(JOMAG_predictores_criteria_merged_civil_qv_relevant_criteria_columns_for_correlations[[i]]),use="pairwise.complete.obs"), silent=T)
    corr_temp$"predictor" <-ifelse(class(corr_try)=="try-error", NA, corr_try$estimate)
    corr_temp$p.value <-ifelse(class(corr_try)=="try-error", NA, corr_try$p.value)
    corr_temp$n <-(ifelse(class(corr_try)=="try-error", NA, corr_try$parameter+2))
    corr_temp<-data.frame(corr_temp)
    colnames(corr_temp)[1]<-JOMAG_predictores_criteria_merged_civil_qv_relevant_predictors_columns_names_for_correlations[[j]]
    row.names(corr_temp)<-JOMAG_predictores_criteria_merged_civil_qv_relevant_criteria_columns_names_for_correlations[i]
    corr_output_temp<-rbind (corr_output_temp,corr_temp)
    corr_output_temp <-round(corr_output_temp,2)
  }
  corr_output_temp$""<-"|"
  JOMAG_predictores_criteria_merged_civil_qv_corr_output<-cbind(JOMAG_predictores_criteria_merged_civil_qv_corr_output,corr_output_temp)
}
row.names(JOMAG_predictores_criteria_merged_civil_qv_corr_output)<-JOMAG_predictores_criteria_merged_civil_qv_relevant_criteria_columns_names_for_correlations

for(i in 1:(ncol(JOMAG_predictores_criteria_merged_civil_qv_corr_output)/4)){
  colnames(JOMAG_predictores_criteria_merged_civil_qv_corr_output)[i*4] <- ""
}
write.csv(JOMAG_predictores_criteria_merged_civil_qv_corr_output,file = "C:/Users/Administrator.MAMADA-777/Documents/Junior officers MAGAV validation/JOMAG_predictores_criteria_merged_civil_qv_corr_output_GIBUSH_dimentions.csv")

# Paired Samples T-test between mSofiBefore1 and mSofiBefore2, on the validation sample.
t_test_temp<-c()
t_test_try <- try(t.test(as.numeric(JOMAG_predictores_criteria_merged_civil_qv$mSofiBefore1),as.numeric(JOMAG_predictores_criteria_merged_civil_qv$mSofiBefore2),paired = T, alternative = "two.sided"), silent=T)
t_test_temp$t <-ifelse(class(t_test_try)=="try-error", NA, round(t_test_try$statistic,2))
t_test_temp$df <-ifelse(class(t_test_try)=="try-error", NA, t_test_try$parameter)
t_test_temp$p.value <-ifelse(class(t_test_try)=="try-error", NA, t_test_try$p.value)
t_test_temp$"mean of mSofiBefore1" <-ifelse(class(t_test_try)=="try-error", NA, round(mean(JOMAG_predictores_criteria_merged_civil_qv$mSofiBefore1,na.rm = T),2))
t_test_temp$"SD of mSofiBefore1" <-ifelse(class(t_test_try)=="try-error", NA, round(sd(JOMAG_predictores_criteria_merged_civil_qv$mSofiBefore1,na.rm = T),2))
t_test_temp$"mean of mSofiBefore2" <-ifelse(class(t_test_try)=="try-error", NA, round(mean(JOMAG_predictores_criteria_merged_civil_qv$mSofiBefore2,na.rm = T),2))
t_test_temp$"SD of mSofiBefore2" <-ifelse(class(t_test_try)=="try-error", NA, round(sd(JOMAG_predictores_criteria_merged_civil_qv$mSofiBefore2,na.rm = T),2))
t_test_temp<-data.frame(t_test_temp)
row.names(t_test_temp)<-"mSofiBefore"
t_test_temp

# Paired Samples T-test between mSofiAfter1 and mSofiAfter2, on the validation sample.
t_test_temp<-c()
t_test_try <- try(t.test(as.numeric(JOMAG_predictores_criteria_merged_civil_qv$mSofiAfter1),as.numeric(JOMAG_predictores_criteria_merged_civil_qv$mSofiAfter2),paired = T, alternative = "two.sided"), silent=T)
t_test_temp$t <-ifelse(class(t_test_try)=="try-error", NA, round(t_test_try$statistic,2))
t_test_temp$df <-ifelse(class(t_test_try)=="try-error", NA, t_test_try$parameter)
t_test_temp$p.value <-ifelse(class(t_test_try)=="try-error", NA, t_test_try$p.value)
t_test_temp$"mean of mSofiAfter1" <-ifelse(class(t_test_try)=="try-error", NA, round(mean(JOMAG_predictores_criteria_merged_civil_qv$mSofiAfter1,na.rm = T),2))
t_test_temp$"SD of mSofiAfter1" <-ifelse(class(t_test_try)=="try-error", NA, round(sd(JOMAG_predictores_criteria_merged_civil_qv$mSofiAfter1,na.rm = T),2))
t_test_temp$"mean of mSofiAfter2" <-ifelse(class(t_test_try)=="try-error", NA, round(mean(JOMAG_predictores_criteria_merged_civil_qv$mSofiAfter2,na.rm = T),2))
t_test_temp$"SD of mSofiAfter2" <-ifelse(class(t_test_try)=="try-error", NA, round(sd(JOMAG_predictores_criteria_merged_civil_qv$mSofiAfter2,na.rm = T),2))
t_test_temp<-data.frame(t_test_temp)
row.names(t_test_temp)<-"mSofiAfter"
t_test_temp

JOMAG_predictores_criteria_merged_civil_old<-read.csv("C:/Users/Administrator.MAMADA-777/Documents/Junior officers MAGAV validation/JOMAG_predictores_criteria_merged_civil_old.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
JOMAG_predictores_criteria_merged_civil_old<-JOMAG_predictores_criteria_merged_civil_old[-1]

# Paired Samples T-test between mSofiBefore1 and mSofiBefore2, on all the sample.
t_test_temp<-c()
t_test_try <- try(t.test(as.numeric(JOMAG_predictores_criteria_merged_civil_old$mSofiBefore1),as.numeric(JOMAG_predictores_criteria_merged_civil_old$mSofiBefore2),paired = T, alternative = "two.sided"), silent=T)
t_test_temp$t <-ifelse(class(t_test_try)=="try-error", NA, round(t_test_try$statistic,2))
t_test_temp$df <-ifelse(class(t_test_try)=="try-error", NA, t_test_try$parameter)
t_test_temp$p.value <-ifelse(class(t_test_try)=="try-error", NA, t_test_try$p.value)
t_test_temp$"mean of mSofiBefore1" <-ifelse(class(t_test_try)=="try-error", NA, round(mean(JOMAG_predictores_criteria_merged_civil_old$mSofiBefore1,na.rm = T),2))
t_test_temp$"SD of mSofiBefore1" <-ifelse(class(t_test_try)=="try-error", NA, round(sd(JOMAG_predictores_criteria_merged_civil_old$mSofiBefore1,na.rm = T),2))
t_test_temp$"mean of mSofiBefore2" <-ifelse(class(t_test_try)=="try-error", NA, round(mean(JOMAG_predictores_criteria_merged_civil_old$mSofiBefore2,na.rm = T),2))
t_test_temp$"SD of mSofiBefore2" <-ifelse(class(t_test_try)=="try-error", NA, round(sd(JOMAG_predictores_criteria_merged_civil_old$mSofiBefore2,na.rm = T),2))
t_test_temp<-data.frame(t_test_temp)
row.names(t_test_temp)<-"mSofiBefore"
t_test_temp

# Paired Samples T-test between mSofiAfter1 and mSofiAfter2, on all the sample.
t_test_temp<-c()
t_test_try <- try(t.test(as.numeric(JOMAG_predictores_criteria_merged_civil_old$mSofiAfter1),as.numeric(JOMAG_predictores_criteria_merged_civil_old$mSofiAfter2),paired = T, alternative = "two.sided"), silent=T)
t_test_temp$t <-ifelse(class(t_test_try)=="try-error", NA, round(t_test_try$statistic,2))
t_test_temp$df <-ifelse(class(t_test_try)=="try-error", NA, t_test_try$parameter)
t_test_temp$p.value <-ifelse(class(t_test_try)=="try-error", NA, t_test_try$p.value)
t_test_temp$"mean of mSofiAfter1" <-ifelse(class(t_test_try)=="try-error", NA, round(mean(JOMAG_predictores_criteria_merged_civil_old$mSofiAfter1,na.rm = T),2))
t_test_temp$"SD of mSofiAfter1" <-ifelse(class(t_test_try)=="try-error", NA, round(sd(JOMAG_predictores_criteria_merged_civil_old$mSofiAfter1,na.rm = T),2))
t_test_temp$"mean of mSofiAfter2" <-ifelse(class(t_test_try)=="try-error", NA, round(mean(JOMAG_predictores_criteria_merged_civil_old$mSofiAfter2,na.rm = T),2))
t_test_temp$"SD of mSofiAfter2" <-ifelse(class(t_test_try)=="try-error", NA, round(sd(JOMAG_predictores_criteria_merged_civil_old$mSofiAfter2,na.rm = T),2))
t_test_temp<-data.frame(t_test_temp)
row.names(t_test_temp)<-"mSofiAfter"
t_test_temp

# Correlations between the 2 assessors, on all sample
JOMAG_predictores_criteria_merged_civil_old_relevant_predictores_columns_for_correlations <- JOMAG_predictores_criteria_merged_civil_old[c(37,43,49,55)]
JOMAG_predictores_criteria_merged_civil_old_relevant_predictores_columns_names_for_correlations <- c(colnames(JOMAG_predictores_criteria_merged_civil_old[c(37,43,49,55)]))
JOMAG_predictores_criteria_merged_civil_old_corr_output<-data.frame()[1:4,]

for(j in 1:length(JOMAG_predictores_criteria_merged_civil_old_relevant_predictores_columns_names_for_correlations)){
  corr_output_temp<-c()
  for(i in 1:length(JOMAG_predictores_criteria_merged_civil_old_relevant_predictores_columns_names_for_correlations)){
    corr_temp<-c()
    corr_try <- try(cor.test(as.numeric(JOMAG_predictores_criteria_merged_civil_old_relevant_predictores_columns_for_correlations[[j]]),as.numeric(JOMAG_predictores_criteria_merged_civil_old_relevant_predictores_columns_for_correlations[[i]]),use="pairwise.complete.obs"), silent=T)
    corr_temp$"predictor" <-ifelse(class(corr_try)=="try-error", NA, corr_try$estimate)
    corr_temp$p.value <-ifelse(class(corr_try)=="try-error", NA, corr_try$p.value)
    corr_temp$n <-(ifelse(class(corr_try)=="try-error", NA, corr_try$parameter+2))
    corr_temp<-data.frame(corr_temp)
    colnames(corr_temp)[1]<-JOMAG_predictores_criteria_merged_civil_old_relevant_predictores_columns_names_for_correlations[[j]]
    row.names(corr_temp)<-JOMAG_predictores_criteria_merged_civil_old_relevant_predictores_columns_names_for_correlations[i]
    corr_output_temp<-rbind (corr_output_temp,corr_temp)
    corr_output_temp <-round(corr_output_temp,2)
  }
  corr_output_temp$""<-"|"
  JOMAG_predictores_criteria_merged_civil_old_corr_output<-cbind(JOMAG_predictores_criteria_merged_civil_old_corr_output,corr_output_temp)
}
row.names(JOMAG_predictores_criteria_merged_civil_old_corr_output)<-JOMAG_predictores_criteria_merged_civil_old_relevant_predictores_columns_names_for_correlations

for(i in 1:(ncol(JOMAG_predictores_criteria_merged_civil_old_corr_output)/4)){
  colnames(JOMAG_predictores_criteria_merged_civil_old_corr_output)[i*4] <- ""
}

write.csv(JOMAG_predictores_criteria_merged_civil_old_corr_output,file = "C:/Users/Asher/Documents/MAMDA/JOMAGAV/JOMAG_predictores_criteria_merged_civil_old_corr_output_assessors_all_sample.csv")


# flexible cutoff point
# JOMAG_predictores_criteria_merged_civil<-read.csv("C:/Users/Administrator.MAMADA-777/Documents/Junior officers MAGAV validation/JOMAG_predictores_criteria_merged_civil_old.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
JOMAG_predictores_criteria_merged_civil<-read.csv("C:/Users/Asher/Documents/MAMDA/JOMAGAV/JOMAG_predictores_criteria_merged_civil_old.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
JOMAG_predictores_criteria_merged_civil<-JOMAG_predictores_criteria_merged_civil[-1]

# JOMAG_predictores_criteria_merged_civil_qv<-read.csv("C:/Users/Asher/Documents/MAMDA/JOMAGAV/JOMAG_predictores_criteria_merged_civil_qv.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
# JOMAG_predictores_criteria_merged_civil_qv<-JOMAG_predictores_criteria_merged_civil_qv[-1]

# candidates_2019_civil<-read.csv("C:/Users/Administrator.MAMADA-777/Documents/Junior officers MAGAV validation/candidates_2019_civil.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
candidates_2019_civil<-read.csv("C:/Users/Asher/Documents/MAMDA/JOMAGAV/candidates_2019_civil.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
candidates_2019_civil<-candidates_2019_civil[-1]
candidates_2019_civil$GibDate<-as.Date(as.character(candidates_2019_civil$GibDate))
JOMAG_predictores_criteria_merged_civil$GibDate<-as.Date(as.character(JOMAG_predictores_criteria_merged_civil$GibDate),format="%d/%m/%Y")

JOMAG_predictores_criteria_merged_civil$officer<-ifelse(is.na(JOMAG_predictores_criteria_merged_civil$darga_date),0,1)

class(JOMAG_predictores_criteria_merged_civil$GibDate)
head(JOMAG_predictores_criteria_merged_civil$GibDate)
class(candidates_2019_civil$GibDate)
head(candidates_2019_civil$GibDate)

detach("package:dplyr", unload = TRUE)
library(plyr)
JOMAG_predictores_criteria_merged_civil<-rbind.fill(JOMAG_predictores_criteria_merged_civil,candidates_2019_civil)
critical_date<-"20/12/2017"
critical_date<-as.Date(as.character(critical_date),format="%d/%m/%Y")
JOMAG_predictores_criteria_merged_civil$candidates_2019<-ifelse(JOMAG_predictores_criteria_merged_civil$GibDate<critical_date,0,1)

library(dplyr)

JOMAG_predictores_criteria_merged_civil$VaadaGrade_completed <-
  ifelse(is.na(JOMAG_predictores_criteria_merged_civil$VaadaGrade),JOMAG_predictores_criteria_merged_civil$FileGrade,JOMAG_predictores_criteria_merged_civil$VaadaGrade)

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


# Ronen (on 28/06/2020: Amir asked to remove Ethiopians from the flexible cutoff point computations 
# and from his presentation about it).

ethiopian_civil<-read.csv("C:/Users/Asher/Documents/MAMDA/JOMAGAV/ethiopian_civil.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
ethiopian_civil<-ethiopian_civil[-1]

JOMAG_predictores_criteria_merged_civil <- merge(JOMAG_predictores_criteria_merged_civil,ethiopian_civil,by=c("personal_number"), all.x=T, all.y=F,sort = FALSE)

filtered_ethiopian=JOMAG_predictores_criteria_merged_civil %>%
  filter(!is.na(VaadaGrade_completed))
nrow(filtered_ethiopian)

filtered_ethiopian$ethiopian[is.na(filtered_ethiopian$ethiopian)] <- 0

library(descr)
library(psych)

round(freq(ordered(as.numeric(unlist(filtered_ethiopian$ethiopian))), plot = F,main=colnames(filtered_ethiopian$ethiopian),font=2),2)

filtered_nonethiopian_VaadaGrade_completed=filtered_ethiopian %>%
  filter(!is.na(ethiopian) & ethiopian != 1 & !is.na(VaadaGrade_completed))
nrow(filtered_nonethiopian_VaadaGrade_completed)

# library(tidyverse)
# library(tidyr)
library(gmodels)

CrossTable(filtered_nonethiopian_VaadaGrade_completed$FileGrade,filtered_nonethiopian_VaadaGrade_completed$candidates_2019,simulate.p.value=TRUE)

# T-test between before/after flexible cutoff point candidates on FileGrade

filtered_FileGrade_candidates_2019=filtered_nonethiopian_VaadaGrade_completed %>%
  filter(!is.na(FileGrade) & !is.na(candidates_2019))
filtered_FileGrade_candidates_2019 %>% 
  group_by(candidates_2019) %>%  
  summarise_at(vars(FileGrade),funs(mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),n())) 
t.test(as.numeric(filtered_FileGrade_candidates_2019$FileGrade)~as.numeric(filtered_FileGrade_candidates_2019$candidates_2019),use="pairwise.complete.obs")

head(filtered_FileGrade_candidates_2019$VaadaGrade_completed)

class(filtered_FileGrade_candidates_2019$VaadaGrade_completed)

filtered_FileGrade_candidates_2019$passed<-
  ifelse(filtered_FileGrade_candidates_2019$VaadaGrade_completed<4,0,1)

filtered_30=filtered_FileGrade_candidates_2019 %>%
  filter(FileGrade<=3 & candidates_2019==0)

library(descr)
library(psych)
round(freq(ordered(as.numeric(unlist(filtered_30$VaadaGrade_completed))), plot = F,main=colnames(filtered_30$VaadaGrade_completed),font=2),2)

filtered_31=filtered_FileGrade_candidates_2019 %>%
  filter(FileGrade<=3 & candidates_2019==1)

library(descr)
library(psych)
round(freq(ordered(as.numeric(unlist(filtered_31$VaadaGrade_completed))), plot = F,main=colnames(filtered_31$VaadaGrade_completed),font=2),2)

filtered_1=filtered_FileGrade_candidates_2019 %>%
  filter(candidates_2019==1)

library(descr)
library(psych)
round(freq(ordered(as.numeric(unlist(filtered_1$FileGrade))), plot = F,main=colnames(filtered_1$FileGrade),font=2),2)
round(freq(ordered(as.numeric(unlist(filtered_1$VaadaGrade_completed))), plot = F,main=colnames(filtered_1$VaadaGrade_completed),font=2),2)

filtered_0=filtered_FileGrade_candidates_2019 %>%
  filter(candidates_2019==0)

CrossTable(filtered_0$FileGrade,filtered_0$officer,simulate.p.value=TRUE)
CrossTable(filtered_1$FileGrade,filtered_1$officer,simulate.p.value=TRUE)

library(descr)
library(psych)
round(freq(ordered(as.numeric(unlist(filtered_0$ZALLS))), plot = F,main=colnames(filtered_0$ZALLS),font=2),2)
round(freq(ordered(as.numeric(unlist(filtered_1$ZALLS))), plot = F,main=colnames(filtered_1$ZALLS),font=2),2)

filtered_FileGrade_candidates_2019$passed<-
  ifelse(filtered_FileGrade_candidates_2019$VaadaGrade_completed<4,0,1)

filtered_02=filtered_FileGrade_candidates_2019 %>%
  filter(candidates_2019==0 & ZALLS==2)

library(descr)
library(psych)
round(describe(as.numeric(unlist(filtered_02$VaadaGrade_completed))),2)
round(freq(ordered(as.numeric(unlist(filtered_02$passed))), plot = F,main=colnames(filtered_02$passed),font=2),2)

filtered_025=filtered_FileGrade_candidates_2019 %>%
  filter(candidates_2019==0 & ZALLS==2.5)

library(descr)
library(psych)
round(describe(as.numeric(unlist(filtered_025$VaadaGrade_completed))),2)
round(freq(ordered(as.numeric(unlist(filtered_025$passed))), plot = F,main=colnames(filtered_025$passed),font=2),2)

filtered_12=filtered_FileGrade_candidates_2019 %>%
  filter(candidates_2019==1 & ZALLS==2)

library(descr)
library(psych)
round(describe(as.numeric(unlist(filtered_12$VaadaGrade_completed))),2)
round(freq(ordered(as.numeric(unlist(filtered_12$passed))), plot = F,main=colnames(filtered_12$passed),font=2),2)

filtered_125=filtered_FileGrade_candidates_2019 %>%
  filter(candidates_2019==1 & ZALLS==2.5)

library(descr)
library(psych)
round(describe(as.numeric(unlist(filtered_125$VaadaGrade_completed))),2)
round(freq(ordered(as.numeric(unlist(filtered_125$passed))), plot = F,main=colnames(filtered_125$passed),font=2),2)

# T-test between before/after flexible cutoff point candidates on VaadaGrade_completed

filtered_VaadaGrade_completed_candidates_2019=JOMAG_predictores_criteria_merged_civil %>%
  filter(!is.na(VaadaGrade_completed) & !is.na(candidates_2019))
filtered_VaadaGrade_completed_candidates_2019 %>% 
  group_by(candidates_2019) %>%  
  summarise_at(vars(VaadaGrade_completed),funs(mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),n())) 
t.test(as.numeric(filtered_VaadaGrade_completed_candidates_2019$VaadaGrade_completed)~as.numeric(filtered_VaadaGrade_completed_candidates_2019$candidates_2019),use="pairwise.complete.obs")

# Passed - failed the Gibush (VaadaGrade_completed>=4)
filtered_FileGrade_candidates_2019$passed<-
  ifelse(filtered_FileGrade_candidates_2019$VaadaGrade_completed<4,0,1)

library(descr)
CrossTable(filtered_FileGrade_candidates_2019$VaadaGrade_completed,filtered_FileGrade_candidates_2019$passed,simulate.p.value=TRUE)
CrossTable(filtered_FileGrade_candidates_2019$candidates_2019,filtered_FileGrade_candidates_2019$passed,simulate.p.value=TRUE)

t.test(as.numeric(filtered_FileGrade_candidates_2019$final_apptitudes_new)~as.numeric(filtered_FileGrade_candidates_2019$passed),use="pairwise.complete.obs")
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$final_apptitudes_new),filtered_FileGrade_candidates_2019$passed,mean,na.rm=T),2)
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$final_apptitudes_new),filtered_FileGrade_candidates_2019$passed,sd,na.rm=T),2)

t.test(as.numeric(filtered_FileGrade_candidates_2019$TsiunSofiAfter)~as.numeric(filtered_FileGrade_candidates_2019$passed),use="pairwise.complete.obs")
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$TsiunSofiAfter),filtered_FileGrade_candidates_2019$passed,mean,na.rm=T),2)
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$TsiunSofiAfter),filtered_FileGrade_candidates_2019$passed,sd,na.rm=T),2)

t.test(as.numeric(filtered_FileGrade_candidates_2019$SocioGrade)~as.numeric(filtered_FileGrade_candidates_2019$passed),use="pairwise.complete.obs")
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$SocioGrade),filtered_FileGrade_candidates_2019$passed,mean,na.rm=T),2)
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$SocioGrade),filtered_FileGrade_candidates_2019$passed,sd,na.rm=T),2)

t.test(as.numeric(filtered_FileGrade_candidates_2019$PersonalityGrade)~as.numeric(filtered_FileGrade_candidates_2019$passed),use="pairwise.complete.obs")
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$PersonalityGrade),filtered_FileGrade_candidates_2019$passed,mean,na.rm=T),2)
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$PersonalityGrade),filtered_FileGrade_candidates_2019$passed,sd,na.rm=T),2)

library(dplyr)
filtered_FileGrade_candidates_2019=filtered_FileGrade_candidates_2019 %>%
  filter(!is.na(mYachasimBefore1_zscore) & !is.na(mYachasimBefore2_zscore))

filtered_FileGrade_candidates_2019 = filtered_FileGrade_candidates_2019 %>% 
  rowwise() %>%
  mutate(mPikudBefore = mean(mPikudBefore1_zscore,mPikudBefore2_zscore,na.rm = T),
         mNihulBefore = mean(mNihulBefore1_zscore,mNihulBefore2_zscore,na.rm = T),
         mKsherimBefore = mean(mKsherimBefore1_zscore,mKsherimBefore2_zscore,na.rm = T),
         mYachasimBefore = mean(mYachasimBefore1_zscore,mYachasimBefore2_zscore,na.rm = T),
         mYitzugBefore = mean(mYitzugBefore1_zscore,mYitzugBefore2_zscore,na.rm = T),
         mPikudAfter = mean(mPikudAfter1_zscore,mPikudAfter2_zscore,na.rm = T),
         mNihulAfter = mean(mNihulAfter1_zscore,mNihulAfter2_zscore,na.rm = T),
         mKsherimAfter = mean(mKsherimAfter1_zscore,mKsherimAfter2_zscore,na.rm = T),
         mYachasimAfter = mean(mYachasimAfter1_zscore,mYachasimAfter2_zscore,na.rm = T),
         mYitzugAfter = mean(mYitzugAfter1_zscore,mYitzugAfter2_zscore,na.rm = T)
  )


# filtered_FileGrade_candidates_2019$mYachasimBefore<-NA
# for(i in 1:nrow(filtered_FileGrade_candidates_2019)){
#   filtered_FileGrade_candidates_2019[i,]$mYachasimBefore<-rowMeans(subset(filtered_FileGrade_candidates_2019[i,],
#                                                                               select=c(mYachasimBefore1_zscore,mYachasimBefore2_zscore)),na.rm=T)
# }

t.test(as.numeric(filtered_FileGrade_candidates_2019$mPikudBefore)~as.numeric(filtered_FileGrade_candidates_2019$passed),use="pairwise.complete.obs")
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$mPikudBefore),filtered_FileGrade_candidates_2019$passed,mean,na.rm=T),2)
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$mPikudBefore),filtered_FileGrade_candidates_2019$passed,sd,na.rm=T),2)

t.test(as.numeric(filtered_FileGrade_candidates_2019$mPikudAfter)~as.numeric(filtered_FileGrade_candidates_2019$passed),use="pairwise.complete.obs")
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$mPikudAfter),filtered_FileGrade_candidates_2019$passed,mean,na.rm=T),2)
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$mPikudAfter),filtered_FileGrade_candidates_2019$passed,sd,na.rm=T),2)

t.test(as.numeric(filtered_FileGrade_candidates_2019$mNihulBefore)~as.numeric(filtered_FileGrade_candidates_2019$passed),use="pairwise.complete.obs")
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$mNihulBefore),filtered_FileGrade_candidates_2019$passed,mean,na.rm=T),2)
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$mNihulBefore),filtered_FileGrade_candidates_2019$passed,sd,na.rm=T),2)

t.test(as.numeric(filtered_FileGrade_candidates_2019$mNihulAfter)~as.numeric(filtered_FileGrade_candidates_2019$passed),use="pairwise.complete.obs")
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$mNihulAfter),filtered_FileGrade_candidates_2019$passed,mean,na.rm=T),2)
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$mNihulAfter),filtered_FileGrade_candidates_2019$passed,sd,na.rm=T),2)

t.test(as.numeric(filtered_FileGrade_candidates_2019$mKsherimBefore)~as.numeric(filtered_FileGrade_candidates_2019$passed),use="pairwise.complete.obs")
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$mKsherimBefore),filtered_FileGrade_candidates_2019$passed,mean,na.rm=T),2)
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$mKsherimBefore),filtered_FileGrade_candidates_2019$passed,sd,na.rm=T),2)

t.test(as.numeric(filtered_FileGrade_candidates_2019$mKsherimAfter)~as.numeric(filtered_FileGrade_candidates_2019$passed),use="pairwise.complete.obs")
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$mKsherimAfter),filtered_FileGrade_candidates_2019$passed,mean,na.rm=T),2)
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$mKsherimAfter),filtered_FileGrade_candidates_2019$passed,sd,na.rm=T),2)

t.test(as.numeric(filtered_FileGrade_candidates_2019$mYachasimBefore)~as.numeric(filtered_FileGrade_candidates_2019$passed),use="pairwise.complete.obs")
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$mYachasimBefore),filtered_FileGrade_candidates_2019$passed,mean,na.rm=T),2)
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$mYachasimBefore),filtered_FileGrade_candidates_2019$passed,sd,na.rm=T),2)

t.test(as.numeric(filtered_FileGrade_candidates_2019$mYachasimAfter)~as.numeric(filtered_FileGrade_candidates_2019$passed),use="pairwise.complete.obs")
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$mYachasimAfter),filtered_FileGrade_candidates_2019$passed,mean,na.rm=T),2)
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$mYachasimAfter),filtered_FileGrade_candidates_2019$passed,sd,na.rm=T),2)

t.test(as.numeric(filtered_FileGrade_candidates_2019$mYitzugBefore)~as.numeric(filtered_FileGrade_candidates_2019$passed),use="pairwise.complete.obs")
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$mYitzugBefore),filtered_FileGrade_candidates_2019$passed,mean,na.rm=T),2)
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$mYitzugBefore),filtered_FileGrade_candidates_2019$passed,sd,na.rm=T),2)

t.test(as.numeric(filtered_FileGrade_candidates_2019$mYitzugAfter)~as.numeric(filtered_FileGrade_candidates_2019$passed),use="pairwise.complete.obs")
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$mYitzugAfter),filtered_FileGrade_candidates_2019$passed,mean,na.rm=T),2)
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$mYitzugAfter),filtered_FileGrade_candidates_2019$passed,sd,na.rm=T),2)


# --------------------------------------untill here only on non Ethiopians-------------------------
# library(tidyverse)
# library(tidyr)
library(gmodels)

CrossTable(JOMAG_predictores_criteria_merged_civil$FileGrade,JOMAG_predictores_criteria_merged_civil$candidates_2019,simulate.p.value=TRUE)

# T-test between before/after flexible cutoff point candidates on FileGrade

filtered_FileGrade_candidates_2019=JOMAG_predictores_criteria_merged_civil %>%
  filter(!is.na(FileGrade) & !is.na(candidates_2019))
filtered_FileGrade_candidates_2019 %>% 
  group_by(candidates_2019) %>%  
  summarise_at(vars(FileGrade),funs(mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),n())) 
t.test(as.numeric(filtered_FileGrade_candidates_2019$FileGrade)~as.numeric(filtered_FileGrade_candidates_2019$candidates_2019),use="pairwise.complete.obs")

head(filtered_FileGrade_candidates_2019$VaadaGrade_completed)

class(filtered_FileGrade_candidates_2019$VaadaGrade_completed)

filtered_FileGrade_candidates_2019$passed<-
  ifelse(filtered_FileGrade_candidates_2019$VaadaGrade_completed<4,0,1)


library(dplyr)

filtered_30=filtered_FileGrade_candidates_2019 %>%
  filter(FileGrade<=3 & candidates_2019==0)

library(descr)
library(psych)
round(freq(ordered(as.numeric(unlist(filtered_30$VaadaGrade_completed))), plot = F,main=colnames(filtered_30$VaadaGrade_completed),font=2),2)

filtered_31=filtered_FileGrade_candidates_2019 %>%
  filter(FileGrade<=3 & candidates_2019==1)

library(descr)
library(psych)
round(freq(ordered(as.numeric(unlist(filtered_31$VaadaGrade_completed))), plot = F,main=colnames(filtered_31$VaadaGrade_completed),font=2),2)

filtered_1=filtered_FileGrade_candidates_2019 %>%
  filter(candidates_2019==1)

library(descr)
library(psych)
round(freq(ordered(as.numeric(unlist(filtered_1$FileGrade))), plot = F,main=colnames(filtered_1$FileGrade),font=2),2)
round(freq(ordered(as.numeric(unlist(filtered_1$VaadaGrade_completed))), plot = F,main=colnames(filtered_1$VaadaGrade_completed),font=2),2)

filtered_0=filtered_FileGrade_candidates_2019 %>%
  filter(candidates_2019==0)

CrossTable(filtered_0$FileGrade,filtered_0$officer,simulate.p.value=TRUE)
CrossTable(filtered_1$FileGrade,filtered_1$officer,simulate.p.value=TRUE)

library(descr)
library(psych)
round(freq(ordered(as.numeric(unlist(filtered_0$ZALLS))), plot = F,main=colnames(filtered_0$ZALLS),font=2),2)
round(freq(ordered(as.numeric(unlist(filtered_1$ZALLS))), plot = F,main=colnames(filtered_1$ZALLS),font=2),2)

filtered_FileGrade_candidates_2019$passed<-
  ifelse(filtered_FileGrade_candidates_2019$VaadaGrade_completed<4,0,1)

  filtered_02=filtered_FileGrade_candidates_2019 %>%
    filter(candidates_2019==0 & ZALLS==2)
  
  library(descr)
  library(psych)
  round(describe(as.numeric(unlist(filtered_02$VaadaGrade_completed))),2)
  round(freq(ordered(as.numeric(unlist(filtered_02$passed))), plot = F,main=colnames(filtered_02$passed),font=2),2)

filtered_025=filtered_FileGrade_candidates_2019 %>%
  filter(candidates_2019==0 & ZALLS==2.5)

library(descr)
library(psych)
round(describe(as.numeric(unlist(filtered_025$VaadaGrade_completed))),2)
round(freq(ordered(as.numeric(unlist(filtered_025$passed))), plot = F,main=colnames(filtered_025$passed),font=2),2)

filtered_12=filtered_FileGrade_candidates_2019 %>%
  filter(candidates_2019==1 & ZALLS==2)

library(descr)
library(psych)
round(describe(as.numeric(unlist(filtered_12$VaadaGrade_completed))),2)
round(freq(ordered(as.numeric(unlist(filtered_12$passed))), plot = F,main=colnames(filtered_12$passed),font=2),2)

filtered_125=filtered_FileGrade_candidates_2019 %>%
  filter(candidates_2019==1 & ZALLS==2.5)

library(descr)
library(psych)
round(describe(as.numeric(unlist(filtered_125$VaadaGrade_completed))),2)
round(freq(ordered(as.numeric(unlist(filtered_125$passed))), plot = F,main=colnames(filtered_125$passed),font=2),2)

# T-test between before/after flexible cutoff point candidates on VaadaGrade_completed

filtered_VaadaGrade_completed_candidates_2019=JOMAG_predictores_criteria_merged_civil %>%
  filter(!is.na(VaadaGrade_completed) & !is.na(candidates_2019))
filtered_VaadaGrade_completed_candidates_2019 %>% 
  group_by(candidates_2019) %>%  
  summarise_at(vars(VaadaGrade_completed),funs(mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),n())) 
t.test(as.numeric(filtered_VaadaGrade_completed_candidates_2019$VaadaGrade_completed)~as.numeric(filtered_VaadaGrade_completed_candidates_2019$candidates_2019),use="pairwise.complete.obs")

# Passed - failed the Gibush (VaadaGrade_completed>=4)
filtered_FileGrade_candidates_2019$passed<-
  ifelse(filtered_FileGrade_candidates_2019$VaadaGrade_completed<4,0,1)

library(descr)
CrossTable(filtered_FileGrade_candidates_2019$VaadaGrade_completed,filtered_FileGrade_candidates_2019$passed,simulate.p.value=TRUE)

t.test(as.numeric(filtered_FileGrade_candidates_2019$final_apptitudes_new)~as.numeric(filtered_FileGrade_candidates_2019$passed),use="pairwise.complete.obs")
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$final_apptitudes_new),filtered_FileGrade_candidates_2019$passed,mean,na.rm=T),2)
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$final_apptitudes_new),filtered_FileGrade_candidates_2019$passed,sd,na.rm=T),2)

t.test(as.numeric(filtered_FileGrade_candidates_2019$TsiunSofiAfter)~as.numeric(filtered_FileGrade_candidates_2019$passed),use="pairwise.complete.obs")
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$TsiunSofiAfter),filtered_FileGrade_candidates_2019$passed,mean,na.rm=T),2)
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$TsiunSofiAfter),filtered_FileGrade_candidates_2019$passed,sd,na.rm=T),2)

t.test(as.numeric(filtered_FileGrade_candidates_2019$SocioGrade)~as.numeric(filtered_FileGrade_candidates_2019$passed),use="pairwise.complete.obs")
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$SocioGrade),filtered_FileGrade_candidates_2019$passed,mean,na.rm=T),2)
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$SocioGrade),filtered_FileGrade_candidates_2019$passed,sd,na.rm=T),2)

t.test(as.numeric(filtered_FileGrade_candidates_2019$PersonalityGrade)~as.numeric(filtered_FileGrade_candidates_2019$passed),use="pairwise.complete.obs")
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$PersonalityGrade),filtered_FileGrade_candidates_2019$passed,mean,na.rm=T),2)
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$PersonalityGrade),filtered_FileGrade_candidates_2019$passed,sd,na.rm=T),2)

library(dplyr)
filtered_FileGrade_candidates_2019=filtered_FileGrade_candidates_2019 %>%
  filter(!is.na(mYachasimBefore1_zscore) & !is.na(mYachasimBefore2_zscore))

filtered_FileGrade_candidates_2019 = filtered_FileGrade_candidates_2019 %>% 
  rowwise() %>%
  mutate(mPikudBefore = mean(mPikudBefore1_zscore,mPikudBefore2_zscore,na.rm = T),
         mNihulBefore = mean(mNihulBefore1_zscore,mNihulBefore2_zscore,na.rm = T),
         mKsherimBefore = mean(mKsherimBefore1_zscore,mKsherimBefore2_zscore,na.rm = T),
         mYachasimBefore = mean(mYachasimBefore1_zscore,mYachasimBefore2_zscore,na.rm = T),
         mYitzugBefore = mean(mYitzugBefore1_zscore,mYitzugBefore2_zscore,na.rm = T),
         mPikudAfter = mean(mPikudAfter1_zscore,mPikudAfter2_zscore,na.rm = T),
         mNihulAfter = mean(mNihulAfter1_zscore,mNihulAfter2_zscore,na.rm = T),
         mKsherimAfter = mean(mKsherimAfter1_zscore,mKsherimAfter2_zscore,na.rm = T),
         mYachasimAfter = mean(mYachasimAfter1_zscore,mYachasimAfter2_zscore,na.rm = T),
         mYitzugAfter = mean(mYitzugAfter1_zscore,mYitzugAfter2_zscore,na.rm = T)
  )


# filtered_FileGrade_candidates_2019$mYachasimBefore<-NA
# for(i in 1:nrow(filtered_FileGrade_candidates_2019)){
#   filtered_FileGrade_candidates_2019[i,]$mYachasimBefore<-rowMeans(subset(filtered_FileGrade_candidates_2019[i,],
#                                                                               select=c(mYachasimBefore1_zscore,mYachasimBefore2_zscore)),na.rm=T)
# }

t.test(as.numeric(filtered_FileGrade_candidates_2019$mPikudBefore)~as.numeric(filtered_FileGrade_candidates_2019$passed),use="pairwise.complete.obs")
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$mPikudBefore),filtered_FileGrade_candidates_2019$passed,mean,na.rm=T),2)
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$mPikudBefore),filtered_FileGrade_candidates_2019$passed,sd,na.rm=T),2)

t.test(as.numeric(filtered_FileGrade_candidates_2019$mPikudAfter)~as.numeric(filtered_FileGrade_candidates_2019$passed),use="pairwise.complete.obs")
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$mPikudAfter),filtered_FileGrade_candidates_2019$passed,mean,na.rm=T),2)
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$mPikudAfter),filtered_FileGrade_candidates_2019$passed,sd,na.rm=T),2)

t.test(as.numeric(filtered_FileGrade_candidates_2019$mNihulBefore)~as.numeric(filtered_FileGrade_candidates_2019$passed),use="pairwise.complete.obs")
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$mNihulBefore),filtered_FileGrade_candidates_2019$passed,mean,na.rm=T),2)
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$mNihulBefore),filtered_FileGrade_candidates_2019$passed,sd,na.rm=T),2)

t.test(as.numeric(filtered_FileGrade_candidates_2019$mNihulAfter)~as.numeric(filtered_FileGrade_candidates_2019$passed),use="pairwise.complete.obs")
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$mNihulAfter),filtered_FileGrade_candidates_2019$passed,mean,na.rm=T),2)
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$mNihulAfter),filtered_FileGrade_candidates_2019$passed,sd,na.rm=T),2)

t.test(as.numeric(filtered_FileGrade_candidates_2019$mKsherimBefore)~as.numeric(filtered_FileGrade_candidates_2019$passed),use="pairwise.complete.obs")
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$mKsherimBefore),filtered_FileGrade_candidates_2019$passed,mean,na.rm=T),2)
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$mKsherimBefore),filtered_FileGrade_candidates_2019$passed,sd,na.rm=T),2)

t.test(as.numeric(filtered_FileGrade_candidates_2019$mKsherimAfter)~as.numeric(filtered_FileGrade_candidates_2019$passed),use="pairwise.complete.obs")
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$mKsherimAfter),filtered_FileGrade_candidates_2019$passed,mean,na.rm=T),2)
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$mKsherimAfter),filtered_FileGrade_candidates_2019$passed,sd,na.rm=T),2)

t.test(as.numeric(filtered_FileGrade_candidates_2019$mYachasimBefore)~as.numeric(filtered_FileGrade_candidates_2019$passed),use="pairwise.complete.obs")
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$mYachasimBefore),filtered_FileGrade_candidates_2019$passed,mean,na.rm=T),2)
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$mYachasimBefore),filtered_FileGrade_candidates_2019$passed,sd,na.rm=T),2)

t.test(as.numeric(filtered_FileGrade_candidates_2019$mYachasimAfter)~as.numeric(filtered_FileGrade_candidates_2019$passed),use="pairwise.complete.obs")
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$mYachasimAfter),filtered_FileGrade_candidates_2019$passed,mean,na.rm=T),2)
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$mYachasimAfter),filtered_FileGrade_candidates_2019$passed,sd,na.rm=T),2)

t.test(as.numeric(filtered_FileGrade_candidates_2019$mYitzugBefore)~as.numeric(filtered_FileGrade_candidates_2019$passed),use="pairwise.complete.obs")
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$mYitzugBefore),filtered_FileGrade_candidates_2019$passed,mean,na.rm=T),2)
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$mYitzugBefore),filtered_FileGrade_candidates_2019$passed,sd,na.rm=T),2)

t.test(as.numeric(filtered_FileGrade_candidates_2019$mYitzugAfter)~as.numeric(filtered_FileGrade_candidates_2019$passed),use="pairwise.complete.obs")
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$mYitzugAfter),filtered_FileGrade_candidates_2019$passed,mean,na.rm=T),2)
round(tapply(as.numeric(filtered_FileGrade_candidates_2019$mYitzugAfter),filtered_FileGrade_candidates_2019$passed,sd,na.rm=T),2)


write.csv(JOMAG_predictores_criteria_merged_civil, file="C:/Users/Asher/Documents/MAMDA/JOMAGAV/JOMAG_predictores_criteria_merged_civil.csv")

# Correlation between FileGrade and SocioGrade on all sample.
cor.test(as.numeric(JOMAG_predictores_criteria_merged_civil$SocioGrade),as.numeric(JOMAG_predictores_criteria_merged_civil$FileGrade),use="pairwise.complete.obs")

# Seniority from giyus
install.packages(tidyverse)
library(readr)
locale("he")
  
JOMAG_predictores_criteria_merged_civil<-read.csv("C:/Users/Asher/Documents/MAMDA/JOMAGAV/JOMAG_predictores_criteria_merged_civil.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="",encoding = "UTF-8")
JOMAG_predictores_criteria_merged_civil<-JOMAG_predictores_criteria_merged_civil[-1]

JOMAG_predictores_criteria_merged_civil$giyus_date<-as.Date(as.character(JOMAG_predictores_criteria_merged_civil$giyus_date),format="%d/%m/%Y")
JOMAG_predictores_criteria_merged_civil$GibDate<-as.Date(as.character(JOMAG_predictores_criteria_merged_civil$GibDate))

JOMAG_predictores_criteria_merged_civil$GibDate_giyus_date<-
as.numeric(JOMAG_predictores_criteria_merged_civil$GibDate-JOMAG_predictores_criteria_merged_civil$giyus_date)
JOMAG_predictores_criteria_merged_civil$GibDate_giyus_date<-
ifelse(JOMAG_predictores_criteria_merged_civil$GibDate_giyus_date<0,NA,
       JOMAG_predictores_criteria_merged_civil$GibDate_giyus_date)

cor.test(as.numeric(JOMAG_predictores_criteria_merged_civil$giyus_date_GibDate),as.numeric(JOMAG_predictores_criteria_merged_civil$FileGrade),use="pairwise.complete.obs")

sum(!is.na(JOMAG_predictores_criteria_merged_civil$giyus_date))
sum(!is.na(JOMAG_predictores_criteria_merged_civil$GibDate))
sum(!is.na(JOMAG_predictores_criteria_merged_civil$GibDate_giyus_date))

head(JOMAG_predictores_criteria_merged_civil$GibDate_giyus_date)

#Variance of cf
sd(JOMAG_predictores_criteria_merged_civil$cf,na.rm = T)

#Variance of PersonalityGrade
sd(JOMAG_predictores_criteria_merged_civil$PersonalityGrade,na.rm = T)

# High/low apptitudes.
t.test(as.numeric(JOMAG_predictores_criteria_merged_civil$FileGrade)~as.numeric(JOMAG_predictores_criteria_merged_civil$final_apptitudes_new_dic),use="pairwise.complete.obs")
round(tapply(as.numeric(JOMAG_predictores_criteria_merged_civil$FileGrade),JOMAG_predictores_criteria_merged_civil$final_apptitudes_new_dic,mean,na.rm=T),2)
round(tapply(as.numeric(JOMAG_predictores_criteria_merged_civil$FileGrade),JOMAG_predictores_criteria_merged_civil$final_apptitudes_new_dic,sd,na.rm=T),2)
library(descr)
library(psych)
options(width = 71,max.print=30000)
round(freq(ordered(as.numeric(unlist(JOMAG_predictores_criteria_merged_civil$candidates_2019))), plot = F,main=colnames(JOMAG_predictores_criteria_merged_civil$candidates_2019),font=2),2)
CrossTable(JOMAG_predictores_criteria_merged_civil$final_apptitudes_new_dic,JOMAG_predictores_criteria_merged_civil$FileGrade,simulate.p.value=TRUE)
CrossTable(JOMAG_predictores_criteria_merged_civil$final_apptitudes_new_dic,JOMAG_predictores_criteria_merged_civil$VaadaGrade_completed,simulate.p.value=TRUE)
CrossTable(JOMAG_predictores_criteria_merged_civil$final_apptitudes_new_dic,JOMAG_predictores_criteria_merged_civil$TsiunSofiAfter,simulate.p.value=TRUE)
CrossTable(JOMAG_predictores_criteria_merged_civil$final_apptitudes_new_dic,JOMAG_predictores_criteria_merged_civil$PersonalityGrade,simulate.p.value=TRUE)

t.test(as.numeric(JOMAG_predictores_criteria_merged_civil$VaadaGrade_completed)~as.numeric(JOMAG_predictores_criteria_merged_civil$final_apptitudes_new_dic),use="pairwise.complete.obs")
round(tapply(as.numeric(JOMAG_predictores_criteria_merged_civil$VaadaGrade_completed),JOMAG_predictores_criteria_merged_civil$final_apptitudes_new_dic,mean,na.rm=T),2)
round(tapply(as.numeric(JOMAG_predictores_criteria_merged_civil$VaadaGrade_completed),JOMAG_predictores_criteria_merged_civil$final_apptitudes_new_dic,sd,na.rm=T),2)

t.test(as.numeric(JOMAG_predictores_criteria_merged_civil$TsiunSofiAfter)~as.numeric(JOMAG_predictores_criteria_merged_civil$final_apptitudes_new_dic),use="pairwise.complete.obs")
round(tapply(as.numeric(JOMAG_predictores_criteria_merged_civil$TsiunSofiAfter),JOMAG_predictores_criteria_merged_civil$final_apptitudes_new_dic,mean,na.rm=T),2)
round(tapply(as.numeric(JOMAG_predictores_criteria_merged_civil$TsiunSofiAfter),JOMAG_predictores_criteria_merged_civil$final_apptitudes_new_dic,sd,na.rm=T),2)

t.test(as.numeric(JOMAG_predictores_criteria_merged_civil$SocioGrade)~as.numeric(JOMAG_predictores_criteria_merged_civil$final_apptitudes_new_dic),use="pairwise.complete.obs")
round(tapply(as.numeric(JOMAG_predictores_criteria_merged_civil$SocioGrade),JOMAG_predictores_criteria_merged_civil$final_apptitudes_new_dic,mean,na.rm=T),2)
round(tapply(as.numeric(JOMAG_predictores_criteria_merged_civil$SocioGrade),JOMAG_predictores_criteria_merged_civil$final_apptitudes_new_dic,sd,na.rm=T),2)

t.test(as.numeric(JOMAG_predictores_criteria_merged_civil$PersonalityGrade)~as.numeric(JOMAG_predictores_criteria_merged_civil$final_apptitudes_new_dic),use="pairwise.complete.obs")
round(tapply(as.numeric(JOMAG_predictores_criteria_merged_civil$PersonalityGrade),JOMAG_predictores_criteria_merged_civil$final_apptitudes_new_dic,mean,na.rm=T),2)
round(tapply(as.numeric(JOMAG_predictores_criteria_merged_civil$PersonalityGrade),JOMAG_predictores_criteria_merged_civil$final_apptitudes_new_dic,sd,na.rm=T),2)

filtered_JOMAG_predictores_criteria_merged_civil_mKsherimAfter=JOMAG_predictores_criteria_merged_civil %>%
  filter(!is.na(final_apptitudes_new_dic) & !is.na(mKsherimAfter))
filtered_JOMAG_predictores_criteria_merged_civil_mKsherimAfter %>% 
  group_by(final_apptitudes_new_dic) %>%  
  summarise_at(vars(mKsherimAfter),funs(mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),n())) 
t.test(as.numeric(filtered_JOMAG_predictores_criteria_merged_civil_mKsherimAfter$mKsherimAfter)~as.numeric(filtered_JOMAG_predictores_criteria_merged_civil_mKsherimAfter$final_apptitudes_new_dic),use="pairwise.complete.obs")

filtered_JOMAG_predictores_criteria_merged_civil_mYachasimAfter=JOMAG_predictores_criteria_merged_civil %>%
  filter(!is.na(final_apptitudes_new_dic) & !is.na(mYachasimAfter))
filtered_JOMAG_predictores_criteria_merged_civil_mYachasimAfter %>% 
  group_by(final_apptitudes_new_dic) %>%  
  summarise_at(vars(mYachasimAfter),funs(mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),n())) 
t.test(as.numeric(filtered_JOMAG_predictores_criteria_merged_civil_mKsherimAfter$mYachasimAfter)~as.numeric(filtered_JOMAG_predictores_criteria_merged_civil_mKsherimAfter$final_apptitudes_new_dic),use="pairwise.complete.obs")


# predicted_overall_score_organizational1_final_apptitudes_new
JOMAG_predictores_criteria_merged_civil$predicted_overall_score_organizational1_final_apptitudes_new<-NA
for (i in 1:nrow(JOMAG_predictores_criteria_merged_civil)) {
  if(!is.na(JOMAG_predictores_criteria_merged_civil[i,]$final_apptitudes_new) | 
     !is.na(JOMAG_predictores_criteria_merged_civil[i,]$TsiunSofiAfter_zscore) |
     !is.na(JOMAG_predictores_criteria_merged_civil[i,]$SocioGrade_zscore)|
     !is.na(JOMAG_predictores_criteria_merged_civil[i,]$PersonalityGrade_zscore)){
    JOMAG_predictores_criteria_merged_civil[i,]$predicted_overall_score_organizational1_final_apptitudes_new <-
      sum(.05*JOMAG_predictores_criteria_merged_civil[i,]$final_apptitudes_new,
          .31*JOMAG_predictores_criteria_merged_civil[i,]$TsiunSofiAfter_zscore,
          .32*JOMAG_predictores_criteria_merged_civil[i,]$SocioGrade_zscore,
          .32*JOMAG_predictores_criteria_merged_civil[i,]$PersonalityGrade_zscore,na.rm = T)}
  else {
    JOMAG_predictores_criteria_merged_civil[i,]$predicted_overall_score_organizational1_final_apptitudes_new <-NA
  }
}

JOMAG_predictores_criteria_merged_civil_qv$predicted_overall_score_organizational1_final_apptitudes_new<-NA
for (i in 1:nrow(JOMAG_predictores_criteria_merged_civil_qv)) {
  if(!is.na(JOMAG_predictores_criteria_merged_civil_qv[i,]$final_apptitudes_new) | 
     !is.na(JOMAG_predictores_criteria_merged_civil_qv[i,]$TsiunSofiAfter_zscore) |
     !is.na(JOMAG_predictores_criteria_merged_civil_qv[i,]$SocioGrade_zscore)|
     !is.na(JOMAG_predictores_criteria_merged_civil_qv[i,]$PersonalityGrade_zscore)){
    JOMAG_predictores_criteria_merged_civil_qv[i,]$predicted_overall_score_organizational1_final_apptitudes_new <-
      sum(.05*JOMAG_predictores_criteria_merged_civil_qv[i,]$final_apptitudes_new,
          .31*JOMAG_predictores_criteria_merged_civil_qv[i,]$TsiunSofiAfter_zscore,
          .32*JOMAG_predictores_criteria_merged_civil_qv[i,]$SocioGrade_zscore,
          .32*JOMAG_predictores_criteria_merged_civil_qv[i,]$PersonalityGrade_zscore,na.rm = T)}
  else {
    JOMAG_predictores_criteria_merged_civil_qv[i,]$predicted_overall_score_organizational1_final_apptitudes_new <-NA
  }
}

# abs_residual_predicted_overall_score_organizational1_final_apptitudes_new
JOMAG_predictores_criteria_merged_civil_qv$abs_residual_predicted_overall_score_organizational1_final_apptitudes_new <-
  abs(JOMAG_predictores_criteria_merged_civil_qv$tkufatitam-JOMAG_predictores_criteria_merged_civil_qv$predicted_overall_score_organizational1_final_apptitudes_new)

mean_abs_residual_predicted_overall_score_organizational1_final_apptitudes_new<-mean(JOMAG_predictores_criteria_merged_civil_qv$abs_residual_predicted_overall_score_organizational1_final_apptitudes_new,na.rm = T)
mean_abs_residual_predicted_overall_score_organizational1_final_apptitudes_new

cor.test(as.numeric(JOMAG_predictores_criteria_merged_civil_qv$predicted_overall_score_organizational1_final_apptitudes_new),as.numeric(JOMAG_predictores_criteria_merged_civil_qv$officerssochach),use="pairwise.complete.obs")
cor.test(as.numeric(JOMAG_predictores_criteria_merged_civil_qv$predicted_overall_score_organizational1_final_apptitudes_new),as.numeric(JOMAG_predictores_criteria_merged_civil_qv$officerssochas),use="pairwise.complete.obs")
cor.test(as.numeric(JOMAG_predictores_criteria_merged_civil_qv$predicted_overall_score_organizational1_final_apptitudes_new),as.numeric(JOMAG_predictores_criteria_merged_civil_qv$tkufatit),use="pairwise.complete.obs")
cor.test(as.numeric(JOMAG_predictores_criteria_merged_civil_qv$predicted_overall_score_organizational1_final_apptitudes_new),as.numeric(JOMAG_predictores_criteria_merged_civil_qv$am),use="pairwise.complete.obs")
cor.test(as.numeric(JOMAG_predictores_criteria_merged_civil_qv$predicted_overall_score_organizational1_final_apptitudes_new),as.numeric(JOMAG_predictores_criteria_merged_civil_qv$cf),use="pairwise.complete.obs")
cor.test(as.numeric(JOMAG_predictores_criteria_merged_civil_qv$predicted_overall_score_organizational1_final_apptitudes_new),as.numeric(JOMAG_predictores_criteria_merged_civil_qv$tkufatitam),use="pairwise.complete.obs")

try(cor.test(as.numeric(JOMAG_predictores_criteria_merged_civil_qv$predicted_overall_score_organizational1_final_apptitudes_new),as.numeric(JOMAG_predictores_criteria_merged_civil_qv$tkufatitam),use="pairwise.complete.obs"), silent=T)

round(describe (as.numeric(JOMAG_predictores_criteria_merged_civil$predicted_overall_score_organizational1_final_apptitudes_new)),2)

JOMAG_predictores_criteria_merged_civil_qv$predicted_overall_score_organizational1_final_apptitudes_new_restricted<-JOMAG_predictores_criteria_merged_civil_qv$predicted_overall_score_organizational1_final_apptitudes_new

round(describe (as.numeric(JOMAG_predictores_criteria_merged_civil_qv$predicted_overall_score_organizational1_final_apptitudes_new_restricted)),2)

#Correlations between prdictes scores.

cor_matrix <- cbind(JOMAG_predictores_criteria_merged_civil_qv$formula_score,
                    JOMAG_predictores_criteria_merged_civil_qv$predicted_overall_score,
                    JOMAG_predictores_criteria_merged_civil_qv$predicted_overall_score_organizational1,
                    JOMAG_predictores_criteria_merged_civil_qv$predicted_overall_score_organizational2,
                    JOMAG_predictores_criteria_merged_civil_qv$predicted_overall_score_organizational3)
colnames(cor_matrix) <- c("formula_score","predicted_overall_score","predicted_overall_score_organizational1",
                          "predicted_overall_score_organizational2","predicted_overall_score_organizational3")

library(readr)
locale("he")

freq(ordered(JOMAG_predictores_criteria_merged_civil$hirarchic3), plot = F,main=colnames(JOMAG_predictores_criteria_merged_civil$hirarchic3),font=2)

# Mahoz

# The labeles spoiled the results!
# JOMAG_predictores_criteria_merged_civil$mahoz<-
#     factor(ifelse(JOMAG_predictores_criteria_merged_civil$hirarchic3=="אגמ מגב",
#          1,ifelse(JOMAG_predictores_criteria_merged_civil$hirarchic3=="בסיס הדרכה מגב בהד",
#          2,ifelse(JOMAG_predictores_criteria_merged_civil$hirarchic3=="חטיבה טקטית מגב",
#          3,ifelse(JOMAG_predictores_criteria_merged_civil$hirarchic3=="מחלקת מדעי התנהגות אמש",
#          4,ifelse(JOMAG_predictores_criteria_merged_civil$hirarchic3=="מפקדת חטמר דרום- מגב דרום",
#          5,ifelse(JOMAG_predictores_criteria_merged_civil$hirarchic3=="מפקדת חטמר חוף- מגב חוף",
#          6,ifelse(JOMAG_predictores_criteria_merged_civil$hirarchic3=="מפקדת חטמר מרכז- מגב מרכז",
#          7,ifelse(JOMAG_predictores_criteria_merged_civil$hirarchic3=="מפקדת חטמר צפון- מגב צפון",
#          8,ifelse(JOMAG_predictores_criteria_merged_civil$hirarchic3=="מפקדת מגב איוש",
#          9,ifelse(JOMAG_predictores_criteria_merged_civil$hirarchic3=="מפקדת מגב יממ",
#          10,ifelse(JOMAG_predictores_criteria_merged_civil$hirarchic3=="מפקדת מגב ירושלים",
#          11,ifelse(JOMAG_predictores_criteria_merged_civil$hirarchic3=="מפקדת מגב ערבה",
#          12,ifelse(JOMAG_predictores_criteria_merged_civil$hirarchic3=="מפקדת עוטף י-ם",
#          13,
#          NA))))))))))))),labels = c("אגם", 
#                                     "בהד", 
#                                     "טקטית",
#                                     "ממדה",
#                                     "דרום",
#                                     "חוף",
#                                     "מרכז",
#                                     "צפון",
#                                     "איוש",
#                                     "יממ",
#                                     "י-ם",
#                                     "ערבה",
#                                     "עוטף י-ם"))

# The labeles spoiled the results!
# JOMAG_predictores_criteria_merged_civil$mahoz<-
# as.factor(revalue(JOMAG_predictores_criteria_merged_civil$mahoz, c("אגמ מגב"=
#                                                          "אגם", 
#                                                          "בסיס הדרכה מגב בהד"=
#                                                          "בהד",
#                                                          "חטיבה טקטית מגב"=
#                                                            "טקטית", 
#                                                          "מחלקת מדעי התנהגות אמש"=
#                                                            "ממדה", 
#                                                          "מפקדת חטמר דרום- מגב דרום"=
#                                                            "דרום", 
#                                                          "מפקדת חטמר חוף- מגב חוף"=
#                                                            "חוף", 
#                                                          "מפקדת חטמר מרכז- מגב מרכז"=
#                                                            "מרכז", 
#                                                          "מפקדת חטמר צפון- מגב צפון"=
#                                                            "צפון", 
#                                                          "מפקדת מגב איוש"=
#                                                            "איוש", 
#                                                          "מפקדת מגב יממ"=
#                                                            "יממ", 
#                                                          "מפקדת מגב ירושלים"=
#                                                            "ירושלים", 
#                                                          "מפקדת מגב ערבה"=
#                                                            "ערבה", 
#                                                          "מפקדת עוטף י-ם"=
#                                                            "עוטף י-ם")))


JOMAG_predictores_criteria_merged_civil$mahoz<-
  factor(ifelse(JOMAG_predictores_criteria_merged_civil$hirarchic3=="אגמ מגב",
       1,ifelse(JOMAG_predictores_criteria_merged_civil$hirarchic3=="בסיס הדרכה מגב בהד",
       2,ifelse(JOMAG_predictores_criteria_merged_civil$hirarchic3=="חטיבה טקטית מגב",
       3,ifelse(JOMAG_predictores_criteria_merged_civil$hirarchic3=="מחלקת מדעי התנהגות אמש",
       4,ifelse(JOMAG_predictores_criteria_merged_civil$hirarchic3=="מפקדת חטמר דרום- מגב דרום",
       5,ifelse(JOMAG_predictores_criteria_merged_civil$hirarchic3=="מפקדת חטמר חוף- מגב חוף",
       6,ifelse(JOMAG_predictores_criteria_merged_civil$hirarchic3=="מפקדת חטמר מרכז- מגב מרכז",
       7,ifelse(JOMAG_predictores_criteria_merged_civil$hirarchic3=="מפקדת חטמר צפון- מגב צפון",
       8,ifelse(JOMAG_predictores_criteria_merged_civil$hirarchic3=="מפקדת מגב איוש",
       9,ifelse(JOMAG_predictores_criteria_merged_civil$hirarchic3=="מפקדת מגב יממ",
       10,ifelse(JOMAG_predictores_criteria_merged_civil$hirarchic3=="מפקדת מגב ירושלים",
       11,ifelse(JOMAG_predictores_criteria_merged_civil$hirarchic3=="מפקדת מגב ערבה",
       12,ifelse(JOMAG_predictores_criteria_merged_civil$hirarchic3=="מפקדת עוטף י-ם",
       13,
       NA))))))))))))))


# JOMAG_predictores_criteria_merged_civil$mahoz<-JOMAG_predictores_criteria_merged_civil$hirarchic3


# Handle nan values
is.nan.data.frame<-function(x)
do.call(cbind,lapply(x,is.nan))  
JOMAG_predictores_criteria_merged_civil[is.nan(JOMAG_predictores_criteria_merged_civil)]<-NA
  
library(descr)
library(psych)
freq(ordered(JOMAG_predictores_criteria_merged_civil$mahoz), plot = F,main=colnames(JOMAG_predictores_criteria_merged_civil$mahoz),font=2)

CrossTable(JOMAG_predictores_criteria_merged_civil$FileGrade,JOMAG_predictores_criteria_merged_civil$mahoz,simulate.p.value=TRUE)

tapply(JOMAG_predictores_criteria_merged_civil$FileGrade,as.factor(JOMAG_predictores_criteria_merged_civil$mahoz),mean,na.rm=T)
tapply(JOMAG_predictores_criteria_merged_civil$FileGrade,as.factor(JOMAG_predictores_criteria_merged_civil$mahoz),median,na.rm=T)
tapply(JOMAG_predictores_criteria_merged_civil$FileGrade,JOMAG_predictores_criteria_merged_civil$mahoz,sd,na.rm=T)

  
library(dplyr)
library("ggplot2")

theme_update(plot.title = element_text(hjust = 0.5))
  
JOMAG_predictores_criteria_merged_civil %>%
filter(!is.na(mahoz) & mahoz!=4)%>%
    ggplot(aes(x=reorder(mahoz, FileGrade), y=FileGrade, fill=mahoz)) + 
    geom_boxplot() +
    xlab("mahoz") +
    theme(legend.position="none") +
    xlab("מקום שירות") +
    ylab("ציון   גיבוש")+
    ggtitle("ציון גיבוש לפי מקום שירות") +
    stat_summary()+
    theme(plot.title = element_text(size = 16,color = "blue", face = "bold"))+
    theme(axis.title.x = element_text(size = 12,color = "#993333", face = "bold"))+
    theme(axis.title.y = element_text(size = 12,color = "#993333", face = "bold"))
#    theme(panel.border = element_rect(colour = "black", fill=NA, size=1))


library(dplyr)

filtered_mahoz=JOMAG_predictores_criteria_merged_civil %>%
  filter(!is.na(mahoz) & mahoz!=4)

# http://www.sthda.com/english/wiki/one-way-anova-test-in-r
# aov(FileGrade~as.factor(mahoz),data=filtered_mahoz)

# https://stats.stackexchange.com/questions/258341/unbalanced-two-way-anova-in-r-studio
anova(lm(FileGrade ~ mahoz, filtered_mahoz,contrasts = "treatment"))


#dimentions_average

library(dplyr)

JOMAG_predictores_criteria_merged_civil = JOMAG_predictores_criteria_merged_civil %>% 
rowwise() %>%
  mutate(dimentions_average=mean(mPikudBefore,mNihulBefore,mKsherimBefore,
                                 mYachasimBefore,mYitzugBefore,
                                 mPikudAfter,mNihulAfter,mKsherimAfter,
                                 mYachasimAfter,mYitzugAfter,na.rm = T))

JOMAG_predictores_criteria_merged_civil_qv = JOMAG_predictores_criteria_merged_civil_qv %>% 
  rowwise() %>%
  mutate(mPikudBefore = mean(mPikudBefore1_zscore,mPikudBefore2_zscore,na.rm = T),
         mNihulBefore = mean(mNihulBefore1_zscore,mNihulBefore2_zscore,na.rm = T),
         mKsherimBefore = mean(mKsherimBefore1_zscore,mKsherimBefore2_zscore,na.rm = T),
         #         mYachasimBefore = mean(mYachasimBefore1_zscore,mYachasimBefore2_zscore,na.rm = T),
         mYitzugBefore = mean(mYitzugBefore1_zscore,mYitzugBefore2_zscore,na.rm = T),
         mPikudAfter = mean(mPikudAfter1_zscore,mPikudAfter2_zscore,na.rm = T),
         mNihulAfter = mean(mNihulAfter1_zscore,mNihulAfter2_zscore,na.rm = T),
         mKsherimAfter = mean(mKsherimAfter1_zscore,mKsherimAfter2_zscore,na.rm = T),
         mYachasimAfter = mean(mYachasimAfter1_zscore,mYachasimAfter2_zscore,na.rm = T),
         mYitzugAfter = mean(mYitzugAfter1_zscore,mYitzugAfter2_zscore,na.rm = T)
  )

JOMAG_predictores_criteria_merged_civil_qv$mYachasimBefore<-NA
for(i in 1:nrow(JOMAG_predictores_criteria_merged_civil_qv)){
  JOMAG_predictores_criteria_merged_civil_qv[i,]$mYachasimBefore<-rowMeans(subset(JOMAG_predictores_criteria_merged_civil_qv[i,],
                                                                               select=c(mYachasimBefore1_zscore,mYachasimBefore2_zscore)),na.rm=T)
}

JOMAG_predictores_criteria_merged_civil_qv = JOMAG_predictores_criteria_merged_civil_qv %>% 
rowwise() %>%
  mutate(dimentions_average=mean(mPikudBefore,mNihulBefore,mKsherimBefore,
                                 mYachasimBefore,mYitzugBefore,
                                 mPikudAfter,mNihulAfter,mKsherimAfter,
                                 mYachasimAfter,mYitzugAfter,na.rm = T))

cor.test(as.numeric(JOMAG_predictores_criteria_merged_civil_qv$dimentions_average),as.numeric(JOMAG_predictores_criteria_merged_civil_qv$tkufatitam),use="pairwise.complete.obs")

# 3 exercises

JOMAG_predictores_criteria_merged_civil_qv$exercises<-
  ifelse(!is.na(JOMAG_predictores_criteria_merged_civil_qv$Targil3),1,0)

library(descr)
library(psych)
round(freq(ordered(as.numeric(unlist(JOMAG_predictores_criteria_merged_civil_qv$exercises))), plot = F,main=colnames(JOMAG_predictores_criteria_merged_civil_qv$exercises),font=2),2)

library(dplyr)
filtered_group=JOMAG_predictores_criteria_merged_civil_qv%>%
  filter(exercises==0)
cor.test(as.numeric(filtered_group$FileGrade),as.numeric(filtered_group$tkufatitam),use="pairwise.complete.obs")
cor.test(as.numeric(filtered_group$VaadaGrade),as.numeric(filtered_group$tkufatitam),use="pairwise.complete.obs")

#sadir/keva

library(readr)
locale("he")
# ranks_new_civil<-read_csv("C:/Users/Administrator.MAMADA-777/Documents/Junior officers MAGAV validation/ranks_new_civil.csv",locale = locale(date_names = "he", encoding = "ISO-8859-8"))
ranks_new_civil<-read_csv("C:/Users/Asher/Documents/MAMDA/JOMAGAV/ranks_new_civil.csv",locale = locale(date_names = "he", encoding = "ISO-8859-8"))
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

library(readr)
locale("he")
JOMAG_predictores_criteria_merged_civil<-read_csv("C:/Users/Asher/Documents/MAMDA/JOMAGAV/JOMAG_predictores_criteria_merged_civil_old.csv",locale = locale(date_names = "he", encoding = "ISO-8859-8"))
JOMAG_predictores_criteria_merged_civil<-JOMAG_predictores_criteria_merged_civil[-1]
JOMAG_predictores_criteria_merged_civil$VaadaGrade_completed <-
  ifelse(is.na(JOMAG_predictores_criteria_merged_civil$VaadaGrade),JOMAG_predictores_criteria_merged_civil$FileGrade,JOMAG_predictores_criteria_merged_civil$VaadaGrade)

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

library(dplyr)
filtered_GibDate<-JOMAG_predictores_criteria_merged_civil %>%
  select(personal_number,GibDate)
ranks_new_civil <-
  merge(ranks_new_civil,filtered_GibDate,by=c("personal_number"), all.x=T, all.y=F,sort = FALSE)

class(ranks_new_civil$rank_new_date)
head(ranks_new_civil$rank_new_date)
class(ranks_new_civil$GibDate)
head(ranks_new_civil$GibDate)
ranks_new_civil$GibDate<-as.Date(as.character(ranks_new_civil$GibDate),format="%d/%m/%Y")
class(ranks_new_civil$GibDate)
head(ranks_new_civil$GibDate)

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

ranks_new_civil=ranks_new_civil %>%
  filter(!is.na(GibDate))
  
n_occur<-data.frame(table(ranks_new_civil$id))
n_occur[n_occur$Freq>1,]
library (data.table)
ranks_new_civil<-setDT(ranks_new_civil)[,.SD[which.max(rank_new_date)],keyby=id]
n_occur<-data.frame(table(ranks_new_civil$id))
n_occur[n_occur$Freq>1,]
ranks_new_civil$GibDate<-NULL

JOMAG_predictores_criteria_merged_civil_ranks_new_civil <-
  merge(JOMAG_predictores_criteria_merged_civil,ranks_new_civil,by=c("personal_number"), all.x=T, all.y=F,sort = FALSE)

class(JOMAG_predictores_criteria_merged_civil_ranks_new_civil$GibDate)
head(JOMAG_predictores_criteria_merged_civil_ranks_new_civil$GibDate)
JOMAG_predictores_criteria_merged_civil_ranks_new_civil$GibDate<-as.Date(as.character(JOMAG_predictores_criteria_merged_civil_ranks_new_civil$GibDate),format="%d/%m/%Y")
class(JOMAG_predictores_criteria_merged_civil_ranks_new_civil$GibDate)
head(JOMAG_predictores_criteria_merged_civil_ranks_new_civil$GibDate)
class(JOMAG_predictores_criteria_merged_civil_ranks_new_civil$rank_new_date)
head(JOMAG_predictores_criteria_merged_civil_ranks_new_civil$rank_new_date)

JOMAG_predictores_criteria_merged_civil_ranks_new_civil$GibDate_rank_new_date_gap<-
  as.numeric(JOMAG_predictores_criteria_merged_civil_ranks_new_civil$GibDate-JOMAG_predictores_criteria_merged_civil_ranks_new_civil$rank_new_date)
JOMAG_predictores_criteria_merged_civil_ranks_new_civil$keva<-ifelse((JOMAG_predictores_criteria_merged_civil_ranks_new_civil$rank_new=="סמר" & JOMAG_predictores_criteria_merged_civil_ranks_new_civil$GibDate_rank_new_date_gap>540) |
                                                                       JOMAG_predictores_criteria_merged_civil_ranks_new_civil$rank_new=="סמש" |
                                                                       JOMAG_predictores_criteria_merged_civil_ranks_new_civil$rank_new=="רסב" |
                                                                       JOMAG_predictores_criteria_merged_civil_ranks_new_civil$rank_new=="רסל"|
                                                                       JOMAG_predictores_criteria_merged_civil_ranks_new_civil$rank_new=="רסמ"|
                                                                       JOMAG_predictores_criteria_merged_civil_ranks_new_civil$rank_new=="רסר",
                                                                     2,ifelse(JOMAG_predictores_criteria_merged_civil_ranks_new_civil$rank_new=="סמר" & JOMAG_predictores_criteria_merged_civil_ranks_new_civil$GibDate_rank_new_date_gap<=540,
                                                                              1,0))

library(descr)
library(psych)
freq(ranks_new_civil$rank_new, plot = F,main=colnames(ranks_new_civil$rank_new),font=2)
round(freq(ordered(as.numeric(unlist(JOMAG_predictores_criteria_merged_civil_ranks_new_civil$keva))), plot = F,main=colnames(JOMAG_predictores_criteria_merged_civil_ranks_new_civil$keva),font=2),2)
JOMAG_predictores_criteria_merged_civil_ranks_new_civil$keva[JOMAG_predictores_criteria_merged_civil_ranks_new_civil$keva==1] <- 0
round(freq(ordered(as.numeric(unlist(JOMAG_predictores_criteria_merged_civil_ranks_new_civil$keva))), plot = F,main=colnames(JOMAG_predictores_criteria_merged_civil_ranks_new_civil$keva),font=2),2)
freq(JOMAG_predictores_criteria_merged_civil_ranks_new_civil$rank_new, plot = F,main=colnames(JOMAG_predictores_criteria_merged_civil_ranks_new_civil$rank_new),font=2)

library(descr)
library(psych)
CrossTable(JOMAG_predictores_criteria_merged_civil_ranks_new_civil$VaadaGrade_completed,JOMAG_predictores_criteria_merged_civil_ranks_new_civil$keva,simulate.p.value=TRUE)
CrossTable(JOMAG_predictores_criteria_merged_civil_ranks_new_civil$SocioGrade,JOMAG_predictores_criteria_merged_civil_ranks_new_civil$keva,simulate.p.value=TRUE)

t.test(as.numeric(JOMAG_predictores_criteria_merged_civil_ranks_new_civil$VaadaGrade_completed)~as.numeric(JOMAG_predictores_criteria_merged_civil_ranks_new_civil$keva),use="pairwise.complete.obs")
library(dplyr)
filtered_quit_VaadaGrade_completed=JOMAG_predictores_criteria_merged_civil_ranks_new_civil %>%
  filter(!is.na(keva) & !is.na(VaadaGrade_completed))
filtered_quit_VaadaGrade_completed %>% 
  group_by(keva) %>%  
  summarise_at(vars(VaadaGrade_completed),funs(mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),n())) 

t.test(as.numeric(JOMAG_predictores_criteria_merged_civil_ranks_new_civil$SocioGrade)~as.numeric(JOMAG_predictores_criteria_merged_civil_ranks_new_civil$keva),use="pairwise.complete.obs")
library(dplyr)
filtered_quit_VaadaGrade_completed=JOMAG_predictores_criteria_merged_civil_ranks_new_civil %>%
  filter(!is.na(keva) & !is.na(SocioGrade))
filtered_quit_VaadaGrade_completed %>% 
  group_by(keva) %>%  
  summarise_at(vars(SocioGrade),funs(mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),n())) 


#Gaps between assessors
JOMAG_predictores_criteria_merged_civil_qv$before_gap<-abs(JOMAG_predictores_criteria_merged_civil_qv$mSofiBefore1-
JOMAG_predictores_criteria_merged_civil_qv$mSofiBefore2)
JOMAG_predictores_criteria_merged_civil_qv$after_gap<-abs(JOMAG_predictores_criteria_merged_civil_qv$mSofiAfter1-
                                                          JOMAG_predictores_criteria_merged_civil_qv$mSofiAfter2)


JOMAG_predictores_criteria_merged_civil$before_gap<-abs(JOMAG_predictores_criteria_merged_civil$mSofiBefore1-
                                                             JOMAG_predictores_criteria_merged_civil$mSofiBefore2)
JOMAG_predictores_criteria_merged_civil$after_gap<-abs(JOMAG_predictores_criteria_merged_civil$mSofiAfter1-
                                                            JOMAG_predictores_criteria_merged_civil$mSofiAfter2)

library(descr)
library(psych)
round(freq(ordered(as.numeric(unlist(JOMAG_predictores_criteria_merged_civil$before_gap))), plot = F,main=colnames(JOMAG_predictores_criteria_merged_civil$before_gap),font=2),2)
round(describe(as.numeric(unlist(JOMAG_predictores_criteria_merged_civil$before_gap))),2)
round(freq(ordered(as.numeric(unlist(JOMAG_predictores_criteria_merged_civil$after_gap))), plot = F,main=colnames(JOMAG_predictores_criteria_merged_civil$after_gap),font=2),2)
round(describe(as.numeric(unlist(JOMAG_predictores_criteria_merged_civil$after_gap))),2)

library(descr)
library(psych)

mode<-function(X)
{
  temp<-table (as.vector(X))
  names (temp)[temp==max(temp)]
}
options(width = 71,max.print=30000)
# The 2 commands after the first command, are for cleaning the output file.
JOMAG_predictores_criteria_merged_civil_freq_relevant_columns<-
  colnames(JOMAG_predictores_criteria_merged_civil[c(1456:1457)])
out<-""
cat("", out, file="C:/Users/Administrator.MAMADA-777/Documents/Junior officers MAGAV validation/assessments_gap.txt", sep="", append=F,fill = T)
suppressWarnings(for(i in JOMAG_predictores_criteria_merged_civil_freq_relevant_columns) {
  newresult1<-round(freq(ordered(as.numeric(unlist(JOMAG_predictores_criteria_merged_civil[[i]]))), plot = F,main=colnames(JOMAG_predictores_criteria_merged_civil[i]),font=2),2)
  newresult2<-round(describe(as.numeric(unlist(JOMAG_predictores_criteria_merged_civil[[i]]))),2)
  newresult3<-"mode="
  newresult4<-mode(JOMAG_predictores_criteria_merged_civil[[i]])
  newresult5<- "                                                                                               "
  newresult6<- "----------------------------------------------------------------------------"
  out <- capture.output(newresult1,newresult5,newresult2,newresult3,newresult4,newresult5,newresult6)
  out[1]<-""
  cat(colnames(JOMAG_predictores_criteria_merged_civil[i]),out, file="C:/Users/Administrator.MAMADA-777/Documents/Junior officers MAGAV validation/assessments_gap.txt", append=T,fill = T)
})

t.test(as.numeric(JOMAG_predictores_criteria_merged_civil$before_gap),as.numeric(JOMAG_predictores_criteria_merged_civil$after_gap),paired = T, alternative = "two.sided")

# Semi-partial correlations predictors-criteria.
library(ppcor)
completedata<-c()
completedata$FileGrade<-JOMAG_predictores_criteria_merged_civil_qv$FileGrade
completedata$tkufatitam<-JOMAG_predictores_criteria_merged_civil_qv$tkufatitam
completedata$before_gap<-JOMAG_predictores_criteria_merged_civil_qv$before_gap
completedata$after_gap<-JOMAG_predictores_criteria_merged_civil_qv$after_gap
completedata<-as.data.frame(completedata)
completedata <- completedata[complete.cases(completedata),]
library(ppcor)
spcor.test(as.numeric(completedata$tkufatitam),as.numeric(completedata$FileGrade),as.numeric(completedata$before_gap))
spcor.test(as.numeric(completedata$tkufatitam),as.numeric(completedata$FileGrade),as.numeric(completedata$after_gap))

# Histograms

# dev.off() #Reset pars
# par(
#    mfrow=c(1,2),
#    mar=c(4,4,1,0)
#  )
# 
# hist(JOMAG_predictores_criteria_merged_civil$FileGrade,
#            breaks=9,
#            col=rgb(1,0,0,0.5),
#            xlab="ציון גיבוש",
#            ylab="מספר   מועמדים/ות",
#            labels = T,
#            main="",
#            ylim = c(0,315),
#            xaxt='n',
#            cex.axis = 0.7,
#            xpd = F)
# #           mids=c(2.0,2.25,2.75,3.25,3.75,4.25,4.75,5.25))
# axis(1, at = seq(2, 6, 0.5),labels=T,cex.axis = 0.5)
# 
# JOMAG_predictores_criteria_merged_civil$VaadaGrade_completed <-
# ifelse(is.na(JOMAG_predictores_criteria_merged_civil$VaadaGrade),JOMAG_predictores_criteria_merged_civil$FileGrade,JOMAG_predictores_criteria_merged_civil$VaadaGrade)
# 
# hist(JOMAG_predictores_criteria_merged_civil$VaadaGrade_completed,
#      breaks=9,
#      col=rgb(0,0,1,0.5),
#      xlab="ציון  ועדה",
#      ylab = "",
#      labels = T,
#      main = "",
#      ylim = c(0,320),
#      xaxt='n',
#      cex.axis = 0.7,
#      xpd = FALSE)
# #     mids=c(2.0,2.25,2.75,3.25,3.75,4.25,4.75,5.25))
# axis(1, at = seq(2, 6, 0.5),labels=T,cex.axis = 0.5)


library(ggplot2)
library(scales)

ggplot(JOMAG_predictores_criteria_merged_civil, aes(x=FileGrade)) + 
  geom_bar(na.rm = T,fill = "#FF6666") +
  xlab("ציון    גיבוש")+
  ylab("מס'     מועמדים")+
#  ggtitle("תרשים 1: התפלגות ציוני גיבוש")+
  stat_bin(binwidth=0.5, geom="text", aes(label=..count..), vjust=-0.5, hjust=0.2) +
  scale_x_continuous(breaks = seq(2, 6, 0.5))+
  scale_y_continuous(breaks = seq(0, 315, 50),limits = c(0,315))+
#  theme(plot.title = element_text(hjust = 0.5, size = 16,color = "blue",face = "bold"))+
  theme(axis.title.x = element_text(size = 12,color = "#993333", face = "bold"))+
  theme(axis.title.y = element_text(size = 12,color = "#993333", face = "bold"))

JOMAG_predictores_criteria_merged_civil$VaadaGrade_completed <-
  ifelse(is.na(JOMAG_predictores_criteria_merged_civil$VaadaGrade),JOMAG_predictores_criteria_merged_civil$FileGrade,JOMAG_predictores_criteria_merged_civil$VaadaGrade)

ggplot(JOMAG_predictores_criteria_merged_civil, aes(x=VaadaGrade_completed)) + 
  geom_bar(na.rm = T,fill = "green") +
  xlab("ציון     ועדה")+
  ylab("מס' מועמדים")+
#  ggtitle("תרשים 2: התפלגות ציוני ועדה")+
  stat_bin(binwidth=0.5, geom="text", aes(label=..count..), vjust=-0.5, hjust=0.2) +
  scale_x_continuous(breaks = seq(2, 6, 0.5))+
  scale_y_continuous(breaks = seq(0, 315, 50),limits = c(0,315))+
#  theme(plot.title = element_text(hjust = 0.5, size = 16,color = "blue",face = "bold"))+
  theme(axis.title.x = element_text(size = 12,color = "#993333", face = "bold"))+
  theme(axis.title.y = element_text(size = 12,color = "#993333", face = "bold"))

# For AMASH presentation

area.color <- c("red", "red", "red", "red","green","green","green","green","green")
ggplot(JOMAG_predictores_criteria_merged_civil, aes(x=VaadaGrade_completed)) + 
  geom_bar(na.rm = T,fill = area.color) +
  annotate("text", x=2.5, y=200, label= "לא עברו:   26.3%") + 
  annotate("text", x = 5.7, y=200, label = "עברו:   73.7%")+
  xlab("ציון    סופי")+
  ylab("מס'    מועמדים")+
  stat_bin(binwidth=0.5, geom="text", aes(label=..count..), vjust=-1, hjust=0.5) +
  stat_bin(binwidth=0.5, geom="text", aes(label=scales::percent((..count..)/sum(..count..))), vjust=1, hjust=0.5) +
  scale_x_continuous(breaks = seq(2, 6, 0.5))+
  scale_y_continuous(breaks = seq(0, 315, 50),limits = c(0,315))+
  theme(axis.title.x = element_text(size = 12,color = "#993333", face = "bold"))+
  theme(axis.title.y = element_text(size = 12,color = "#993333", face = "bold"))

# 2015

library(dplyr)

JOMAG_predictores_criteria_merged_civil$GibDate<-as.Date(as.character(JOMAG_predictores_criteria_merged_civil$GibDate),format="%d/%m/%Y")

class(JOMAG_predictores_criteria_merged_civil$GibDate)
head(JOMAG_predictores_criteria_merged_civil$GibDate)

critical_date_2015<-"01/01/2015"
critical_date_2015<-as.Date(as.character(critical_date_2015),format="%d/%m/%Y")

class(critical_date_2015)
critical_date_2015

JOMAG_predictores_criteria_merged_civil$candidates_2015<-
  ifelse(JOMAG_predictores_criteria_merged_civil$GibDate<critical_date_2015,0,1)

library(descr)
library(psych)
round(freq(ordered(as.numeric(unlist(JOMAG_predictores_criteria_merged_civil$candidates_2015))), plot = F,main=colnames(JOMAG_predictores_criteria_merged_civil$candidates_2015),font=2),2)

t.test(as.numeric(JOMAG_predictores_criteria_merged_civil$mSofiBefore1)~as.numeric(JOMAG_predictores_criteria_merged_civil$candidates_2015),use="pairwise.complete.obs")
round(tapply(as.numeric(JOMAG_predictores_criteria_merged_civil$mSofiBefore1),JOMAG_predictores_criteria_merged_civil$candidates_2015,mean,na.rm=T),2)
round(tapply(as.numeric(JOMAG_predictores_criteria_merged_civil$mSofiBefore1),JOMAG_predictores_criteria_merged_civil$candidates_2015,sd,na.rm=T),2)
CrossTable(JOMAG_predictores_criteria_merged_civil$mSofiBefore1,JOMAG_predictores_criteria_merged_civil$candidates_2015,simulate.p.value=TRUE)

t.test(as.numeric(JOMAG_predictores_criteria_merged_civil$mSofiBefore2)~as.numeric(JOMAG_predictores_criteria_merged_civil$candidates_2015),use="pairwise.complete.obs")
round(tapply(as.numeric(JOMAG_predictores_criteria_merged_civil$mSofiBefore2),JOMAG_predictores_criteria_merged_civil$candidates_2015,mean,na.rm=T),2)
round(tapply(as.numeric(JOMAG_predictores_criteria_merged_civil$mSofiBefore2),JOMAG_predictores_criteria_merged_civil$candidates_2015,sd,na.rm=T),2)
CrossTable(JOMAG_predictores_criteria_merged_civil$mSofiBefore2,JOMAG_predictores_criteria_merged_civil$candidates_2015,simulate.p.value=TRUE)

t.test(as.numeric(JOMAG_predictores_criteria_merged_civil$mSofiAfter1)~as.numeric(JOMAG_predictores_criteria_merged_civil$candidates_2015),use="pairwise.complete.obs")
round(tapply(as.numeric(JOMAG_predictores_criteria_merged_civil$mSofiAfter1),JOMAG_predictores_criteria_merged_civil$candidates_2015,mean,na.rm=T),2)
round(tapply(as.numeric(JOMAG_predictores_criteria_merged_civil$mSofiAfter1),JOMAG_predictores_criteria_merged_civil$candidates_2015,sd,na.rm=T),2)
CrossTable(JOMAG_predictores_criteria_merged_civil$mSofiAfter1,JOMAG_predictores_criteria_merged_civil$candidates_2015,simulate.p.value=TRUE)

t.test(as.numeric(JOMAG_predictores_criteria_merged_civil$mSofiAfter2)~as.numeric(JOMAG_predictores_criteria_merged_civil$candidates_2015),use="pairwise.complete.obs")
round(tapply(as.numeric(JOMAG_predictores_criteria_merged_civil$mSofiAfter2),JOMAG_predictores_criteria_merged_civil$candidates_2015,mean,na.rm=T),2)
round(tapply(as.numeric(JOMAG_predictores_criteria_merged_civil$mSofiAfter2),JOMAG_predictores_criteria_merged_civil$candidates_2015,sd,na.rm=T),2)
CrossTable(JOMAG_predictores_criteria_merged_civil$mSofiAfter2,JOMAG_predictores_criteria_merged_civil$candidates_2015,simulate.p.value=TRUE)

t.test(as.numeric(JOMAG_predictores_criteria_merged_civil$TsiunSofiAfter)~as.numeric(JOMAG_predictores_criteria_merged_civil$candidates_2015),use="pairwise.complete.obs")
round(tapply(as.numeric(JOMAG_predictores_criteria_merged_civil$TsiunSofiAfter),JOMAG_predictores_criteria_merged_civil$candidates_2015,mean,na.rm=T),2)
round(tapply(as.numeric(JOMAG_predictores_criteria_merged_civil$TsiunSofiAfter),JOMAG_predictores_criteria_merged_civil$candidates_2015,sd,na.rm=T),2)
CrossTable(JOMAG_predictores_criteria_merged_civil$TsiunSofiAfter,JOMAG_predictores_criteria_merged_civil$candidates_2015,simulate.p.value=TRUE)

t.test(as.numeric(JOMAG_predictores_criteria_merged_civil$TsiunSofiBefore)~as.numeric(JOMAG_predictores_criteria_merged_civil$candidates_2015),use="pairwise.complete.obs")
round(tapply(as.numeric(JOMAG_predictores_criteria_merged_civil$TsiunSofiBefore),JOMAG_predictores_criteria_merged_civil$candidates_2015,mean,na.rm=T),2)
round(tapply(as.numeric(JOMAG_predictores_criteria_merged_civil$TsiunSofiBefore),JOMAG_predictores_criteria_merged_civil$candidates_2015,sd,na.rm=T),2)
CrossTable(JOMAG_predictores_criteria_merged_civil$TsiunSofiAfter,JOMAG_predictores_criteria_merged_civil$candidates_2015,simulate.p.value=TRUE)

# FileGrade=3.5 and VaadaGrade=4
library(dplyr)

filtered_FileGrade=JOMAG_predictores_criteria_merged_civil %>%
  filter(FileGrade==3.5 & VaadaGrade==4)
library(descr)
library(psych)
round(freq(ordered(as.numeric(unlist(filtered_FileGrade$am))), plot = F,main=colnames(filtered_FileGrade$am),font=2),2)
round(describe(as.numeric(unlist(filtered_FileGrade$am))),2)
                                                          
round(freq(ordered(as.numeric(unlist(filtered_FileGrade$tkufatit))), plot = F,main=colnames(filtered_FileGrade$tkufatit),font=2),2)
round(describe(as.numeric(unlist(filtered_FileGrade$tkufatit))),2)

round(freq(ordered(as.numeric(unlist(filtered_FileGrade$tkufatitam))), plot = F,main=colnames(filtered_FileGrade$tkufatitam),font=2),2)
round(describe(as.numeric(unlist(filtered_FileGrade$tkufatitam))),2)

# 3=<formula_score<=3.75 and 2010-2017 vs. 2018-2019 (distribution and t-test on FileGrade) 
JOMAG_predictores_criteria_merged_civil<-read.csv("C:/Users/Administrator.MAMADA-777/Documents/Junior officers MAGAV validation/JOMAG_predictores_criteria_merged_civil_old.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
JOMAG_predictores_criteria_merged_civil<-JOMAG_predictores_criteria_merged_civil[-1]

library(dplyr)

filtered_formula_score=JOMAG_predictores_criteria_merged_civil %>%
  filter(formula_score>=3 & formula_score<=3.75)

candidates_2019_civil<-read.csv("C:/Users/Administrator.MAMADA-777/Documents/Junior officers MAGAV validation/candidates_2019_civil.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
candidates_2019_civil<-candidates_2019_civil[-1]
candidates_2019_civil$GibDate<-as.Date(as.character(candidates_2019_civil$GibDate))
filtered_formula_score$GibDate<-as.Date(as.character(filtered_formula_score$GibDate),format="%d/%m/%Y")

class(filtered_formula_score$GibDate)
head(filtered_formula_score$GibDate)
class(candidates_2019_civil$GibDate)
head(candidates_2019_civil$GibDate)

library(plyr)
filtered_formula_score<-rbind.fill(filtered_formula_score,candidates_2019_civil)
critical_date<-"01/01/2018"
critical_date<-as.Date(as.character(critical_date),format="%d/%m/%Y")
filtered_formula_score$candidates_2018<-ifelse(filtered_formula_score$GibDate<critical_date,0,1)
t.test(as.numeric(filtered_formula_score$FileGrade)~as.numeric(filtered_formula_score$candidates_2018),use="pairwise.complete.obs")
round(tapply(as.numeric(filtered_formula_score$FileGrade),filtered_formula_score$candidates_2018,mean,na.rm=T),2)
round(tapply(as.numeric(filtered_formula_score$FileGrade),filtered_formula_score$candidates_2018,sd,na.rm=T),2)
CrossTable(filtered_formula_score$FileGrade,filtered_formula_score$candidates_2018,simulate.p.value=TRUE)

# On JOMAG_predictores_criteria_merged_civil
library(readr)
locale("he")
JOMAG_predictores_criteria_merged_civil<-read_csv("C:/Users/Asher/Documents/MAMDA/JOMAGAV/JOMAG_predictores_criteria_merged_civil_old.csv",locale = locale(date_names = "he", encoding = "ISO-8859-8"))
JOMAG_predictores_criteria_merged_civil<-JOMAG_predictores_criteria_merged_civil[-1]
#JOMAG_predictores_criteria_merged_civil_qv<-read_csv("C:/Users/Asher/Documents/MAMDA/JOMAGAV/JOMAG_predictores_criteria_merged_civil_qv_old.csv",locale = locale(date_names = "he", encoding = "ISO-8859-8"))
#JOMAG_predictores_criteria_merged_civil_qv<-JOMAG_predictores_criteria_merged_civil_qv[-1]

JOMAG_predictores_criteria_merged_civil$VaadaGrade_completed <-
  ifelse(is.na(JOMAG_predictores_criteria_merged_civil$VaadaGrade),JOMAG_predictores_criteria_merged_civil$FileGrade,JOMAG_predictores_criteria_merged_civil$VaadaGrade)

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

# On JOMAG_predictores_criteria_merged_civil_qv
# There are no such candidates because all their criteria_count < 2*******

JOMAG_predictores_criteria_merged_civil_qv$VaadaGrade_completed <-
  ifelse(is.na(JOMAG_predictores_criteria_merged_civil_qv$VaadaGrade),JOMAG_predictores_criteria_merged_civil_qv$FileGrade,JOMAG_predictores_criteria_merged_civil_qv$VaadaGrade)

filtered_FileGrade3=JOMAG_predictores_criteria_merged_civil_qv %>%
  filter(FileGrade==3 & VaadaGrade_completed==3.5)
nrow(filtered_FileGrade3)

filtered_FileGrade35=JOMAG_predictores_criteria_merged_civil_qv %>%
  filter(FileGrade==3.5 & VaadaGrade_completed==4.5)
nrow(filtered_FileGrade35)

JOMAG_predictores_criteria_merged_civil_qv$VaadaGrade_completed[JOMAG_predictores_criteria_merged_civil_qv$FileGrade==3.5 & JOMAG_predictores_criteria_merged_civil_qv$VaadaGrade_completed==4.5] <- 4

filtered_FileGrade5=JOMAG_predictores_criteria_merged_civil_qv %>%
  filter(FileGrade==5 & VaadaGrade_completed==4.5)
nrow(filtered_FileGrade5)

JOMAG_predictores_criteria_merged_civil_qv$VaadaGrade_completed[JOMAG_predictores_criteria_merged_civil_qv$FileGrade==5 & JOMAG_predictores_criteria_merged_civil_qv$VaadaGrade_completed==4.5] <- 5

filtered_FileGrade55=JOMAG_predictores_criteria_merged_civil_qv %>%
  filter(FileGrade==5.5 & VaadaGrade_completed==4.5)
nrow(filtered_FileGrade55)

JOMAG_predictores_criteria_merged_civil_qv$VaadaGrade_completed[JOMAG_predictores_criteria_merged_civil_qv$FileGrade==5.5 & JOMAG_predictores_criteria_merged_civil_qv$VaadaGrade_completed==4.5] <- 5.5

JOMAG_predictores_criteria_merged_civil$final_apptitudes_new_dic<-ifelse(JOMAG_predictores_criteria_merged_civil$final_apptitudes_new< -0.028327486,0,1)
t.test(as.numeric(JOMAG_predictores_criteria_merged_civil$VaadaGrade_completed)~as.numeric(JOMAG_predictores_criteria_merged_civil$final_apptitudes_new_dic),use="pairwise.complete.obs")
round(tapply(as.numeric(JOMAG_predictores_criteria_merged_civil$VaadaGrade_completed),JOMAG_predictores_criteria_merged_civil$final_apptitudes_new_dic,mean,na.rm=T),2)
round(tapply(as.numeric(JOMAG_predictores_criteria_merged_civil$VaadaGrade_completed),JOMAG_predictores_criteria_merged_civil$final_apptitudes_new_dic,sd,na.rm=T),2)

# Ethiopians
# ethiopian_civil<-read.csv("C:/Users/Administrator.MAMADA-777/Documents/Junior officers MAGAV validation/ethiopian_civil.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
ethiopian_civil<-read.csv("C:/Users/Asher/Documents/MAMDA/JOMAGAV/ethiopian_civil.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
ethiopian_civil<-ethiopian_civil[-1]

JOMAG_predictores_criteria_merged_civil <- merge(JOMAG_predictores_criteria_merged_civil,ethiopian_civil,by=c("personal_number"), all.x=T, all.y=F,sort = FALSE)


filtered_ethiopian=JOMAG_predictores_criteria_merged_civil %>%
  filter(!is.na(VaadaGrade_completed))
nrow(filtered_ethiopian)

filtered_ethiopian$ethiopian[is.na(filtered_ethiopian$ethiopian)] <- 0

round(freq(ordered(as.numeric(unlist(filtered_ethiopian$ethiopian))), plot = F,main=colnames(filtered_ethiopian$ethiopian),font=2),2)

filtered_ethiopian_VaadaGrade_completed=filtered_ethiopian %>%
  filter(!is.na(ethiopian) & !is.na(VaadaGrade_completed))

filtered_ethiopian_VaadaGrade_completed %>% 
  group_by(ethiopian) %>%  
  summarise_at(vars(VaadaGrade_completed),funs(mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),n())) 
t.test(as.numeric(filtered_ethiopian$VaadaGrade_completed)~as.numeric(filtered_ethiopian$ethiopian),use="pairwise.complete.obs")

filtered_ethiopian_VaadaGrade_completed %>% 
  group_by(ethiopian) %>%  
  summarise_at(vars(final_apptitudes_new),funs(mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),n())) 
t.test(as.numeric(filtered_ethiopian$final_apptitudes_new)~as.numeric(filtered_ethiopian$ethiopian),use="pairwise.complete.obs")

filtered_ethiopian_qv<-filtered_ethiopian[which(!is.na(filtered_ethiopian$VaadaGrade_completed)
                                                 & filtered_ethiopian$critria_count>1 & filtered_ethiopian$VaadaGrade_completed>=4),]
nrow(filtered_ethiopian_qv)

filtered_ethiopian_qv1<-filtered_ethiopian[which(!is.na(filtered_ethiopian$VaadaGrade_completed)
& filtered_ethiopian$critria_count>1 & filtered_ethiopian$ethiopian==1 & filtered_ethiopian$VaadaGrade_completed>=4),]
nrow(filtered_ethiopian_qv1)

cor.test(as.numeric(filtered_ethiopian_qv1$VaadaGrade_completed),as.numeric(filtered_ethiopian_qv1$tkufatitam),use="pairwise.complete.obs")
round(tapply(as.numeric(filtered_ethiopian$VaadaGrade_completed),filtered_ethiopian$ethiopian,sd,na.rm=T),2)
sd(filtered_ethiopian_qv1$VaadaGrade_completed,na.rm=T)

filtered_ethiopian_qv2<-filtered_ethiopian[which(!is.na(filtered_ethiopian$VaadaGrade_completed)
                                                 & filtered_ethiopian$critria_count>1 & filtered_ethiopian$ethiopian==0),]
nrow(filtered_ethiopian_qv2)

cor.test(as.numeric(filtered_ethiopian_qv2$VaadaGrade_completed),as.numeric(filtered_ethiopian_qv2$tkufatitam),use="pairwise.complete.obs")
round(tapply(as.numeric(filtered_ethiopian$VaadaGrade_completed),filtered_ethiopian$ethiopian,sd,na.rm=T),2)
sd(filtered_ethiopian_qv2$VaadaGrade_completed,na.rm=T)

library(ggplot2)
ggplot(filtered_ethiopian_qv,aes(x=VaadaGrade_completed,y=tkufatitam))+
  geom_point()+
  aes(color=factor(ethiopian))+geom_smooth(method="lm",se=FALSE)+
  xlab ("ציון  גיבוש סופי")+
  ylab ("הערכה  תקופתית - עמיתים - ציון תקן")+
  scale_color_manual(labels = c("אחר", "אתיופי"), values = c("blue", "green"),name="מוצא") +
  theme(axis.title.x = element_text(size = 12,color = "#993333", face = "bold"))+
  theme(axis.title.y = element_text(size = 12,color = "#993333", face = "bold"))+
  theme(legend.title = element_text(size = 11,color = "#993333", face = "bold"))

library(ggplot2)
ggplot(filtered_ethiopian_qv,aes(x=VaadaGrade_completed,y=tkufatitam))+
  geom_point()+
  aes(color=factor(ethiopian))+geom_smooth(method="lm",se=FALSE)+
  xlab ("ציון  גיבוש סופי")+
  ylab ("הערכה  תקופתית - עמיתים")+
  scale_color_manual(labels = c("אחר", "אתיופי"), values = c("blue", "green"),name="מוצא") +
  scale_y_continuous(breaks=c(-1,0,1),labels= c("נמוכה",
                                                "בינונית",
                                                "גבוהה"))+
  theme(axis.title.x = element_text(size = 12,color = "#993333", face = "bold"))+
  theme(axis.title.y = element_text(size = 12,color = "#993333", face = "bold"))+
  theme(legend.title = element_text(size = 11,color = "#993333", face = "bold"))

#religion

# religion_civil<-read.csv("C:/Users/Administrator.MAMADA-777/Documents/Junior officers MAGAV validation/religion_civil.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
religion_civil<-read.csv("C:/Users/Asher/Documents/MAMDA/JOMAGAV/religion_civil.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
religion_civil<-religion_civil[-1]

# library(readr)
# locale("he")
# JOMAG_predictores_criteria_merged_civil<-read_csv("C:/Users/Administrator.MAMADA-777/Documents/Junior officers MAGAV validation/JOMAG_predictores_criteria_merged_civil_old.csv",locale = locale(date_names = "he", encoding = "ISO-8859-8"))
# JOMAG_predictores_criteria_merged_civil<-JOMAG_predictores_criteria_merged_civil[-1]

JOMAG_predictores_criteria_merged_civil <- merge(JOMAG_predictores_criteria_merged_civil,religion_civil,by=c("personal_number"), all.x=T, all.y=F,sort = FALSE)

filtered_religion=JOMAG_predictores_criteria_merged_civil %>%
  filter(!is.na(VaadaGrade_completed))
nrow(filtered_religion)

round(freq(ordered(filtered_religion$religion), plot = F,main=colnames(filtered_religion$religion),font=2),2)

filtered_religion$jewish <- ifelse(!is.na(filtered_religion$religion) &
                                                                filtered_religion$religion=="jewish",0,
                                                  ifelse(!is.na(filtered_religion$religion),1,NA))

round(freq(ordered(filtered_religion$jewish), plot = F,main=colnames(filtered_religion$jewish),font=2),2)

filtered_jewish_VaadaGrade_completed=filtered_religion %>%
  filter(!is.na(jewish) & !is.na(VaadaGrade_completed))
filtered_jewish_VaadaGrade_completed %>% 
  group_by(jewish) %>%  
  summarise_at(vars(VaadaGrade_completed),funs(mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),n())) 
t.test(as.numeric(filtered_religion$VaadaGrade_completed)~as.numeric(filtered_religion$jewish),use="pairwise.complete.obs")

filtered_religion_qv<-filtered_religion[which(!is.na(filtered_religion$FileGrade)
                                                & filtered_religion$critria_count>1 & filtered_religion$jewish==1),]
nrow(filtered_religion_qv)

cor.test(as.numeric(filtered_religion_qv$VaadaGrade_completed),as.numeric(filtered_religion_qv$tkufatitam),use="pairwise.complete.obs")

#gender

filtered_gender=JOMAG_predictores_criteria_merged_civil %>%
  filter(!is.na(VaadaGrade_completed))
nrow(filtered_gender)

round(freq(ordered(filtered_gender$gender), plot = F,main=colnames(filtered_gender$round(freq(ordered(filtered_gender$gender),font=2),2)),font=2),2)

filtered_gender_VaadaGrade_completed=filtered_religion %>%
  filter(!is.na(gender) & !is.na(VaadaGrade_completed))
filtered_gender_VaadaGrade_completed %>% 
  group_by(gender) %>%  
  summarise_at(vars(VaadaGrade_completed),funs(mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),n())) 
t.test(as.numeric(filtered_gender$VaadaGrade_completed)~as.numeric(filtered_gender$gender),use="pairwise.complete.obs")

#quit

library(readr)
locale("he")
# JOMAG_predictores_criteria_merged_civil<-read_csv("C:/Users/Administrator.MAMADA-777/Documents/Junior officers MAGAV validation/JOMAG_predictores_criteria_merged_civil_old.csv",locale = locale(date_names = "he", encoding = "ISO-8859-8"))
JOMAG_predictores_criteria_merged_civil<-read_csv("C:/Users/Asher/Documents/MAMDA/JOMAGAV/JOMAG_predictores_criteria_merged_civil_old.csv",locale = locale(date_names = "he", encoding = "ISO-8859-8"))
JOMAG_predictores_criteria_merged_civil<-JOMAG_predictores_criteria_merged_civil[-1]
n_occur<-data.frame(table(JOMAG_predictores_criteria_merged_civil$personal_number))
n_occur[n_occur$Freq>1,]
nrow(JOMAG_predictores_criteria_merged_civil)

ranks_civil<-read.csv("C:/Users/Asher/Documents/MAMDA/JOMAGAV/ranks_civil.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
ranks_civil$darga_date<-as.Date(ranks_civil$darga_date,format="%d/%m/%Y")
n_occur<-data.frame(table(ranks_civil$personal_number))
n_occur[n_occur$Freq>1,]
nrow(ranks_civil)

ranks_civil$matching_ranks1<-
  ifelse(!is.na(match(ranks_civil$personal_number,JOMAG_predictores_criteria_merged_civil$personal_number)),1,NA)

library(readr)
locale("he")
#read_csv***************************************************
# quit_civil<-read.csv("C:/Users/Administrator.MAMADA-777/Documents/Junior officers MAGAV validation/quit_civil.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
quit_civil<-read_csv("C:/Users/Asher/Documents/MAMDA/JOMAGAV/quit_civil.csv",locale = locale(date_names = "he", encoding = "ISO-8859-8"))
quit_civil<-quit_civil[-c(1,3,4)]
quit_civil$quit_date<-as.Date(quit_civil$quit_date,format="%d/%m/%Y")
quit_civil$darga_date<-as.Date(quit_civil$darga_date)

n_occur<-data.frame(table(quit_civil$personal_number))
n_occur[n_occur$Freq>1,]
library (data.table)
quit_civil <- quit_civil[!duplicated(quit_civil[,c("personal_number")]),]
n_occur<-data.frame(table(quit_civil$personal_number))
n_occur[n_occur$Freq>1,]

library(dplyr)

quit_civil_old=quit_civil %>%
  filter(!is.na(matching))%>%
  filter(sector=="יעודי")
quit_civil_old<-quit_civil_old[-c(8,10)]
nrow(quit_civil_old)

quit_civil_new=quit_civil %>%
  filter(is.na(matching) & sector=="יעודי")
nrow(quit_civil_new)

ranks_civil_new=ranks_civil %>%
  filter(is.na(matching_ranks1))
ranks_civil_new<-ranks_civil_new[-1]
nrow(ranks_civil_new)

start_date = quit_civil$darga_date[quit_civil$quit_date==min(quit_civil$quit_date)] 
class(start_date)

ranks_civil$matching_ranks2<-
  ifelse(!is.na(match(ranks_civil$personal_number,quit_civil_new$personal_number)),1,NA)
ranks_civil_new2=ranks_civil %>%
  filter(is.na(matching_ranks1) & is.na(matching_ranks2) & darga_date>=start_date)
ranks_civil_new2<-ranks_civil_new2[-1]
nrow(ranks_civil_new2)

library(plyr)
quit_civil_new<-rbind.fill(quit_civil_new,ranks_civil_new2)
nrow(quit_civil_new)

# ksharim_civil<-read.csv("C:/Users/Administrator.MAMADA-777/Documents/Junior officers MAGAV validation/ksharim_civil.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
ksharim_civil<-read.csv("C:/Users/Asher/Documents/MAMDA/JOMAGAV/ksharim_civil.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
ksharim_civil<-ksharim_civil[-c(1,3)]
colnames(ksharim_civil)[2]<-paste(colnames(ksharim_civil)[2],"ksharim",sep="_")
n_occur<-data.frame(table(ksharim_civil$id))
n_occur[n_occur$Freq>1,]
quit_civil_new <- merge(quit_civil_new ,ksharim_civil,by=c("id"), all.x=T, all.y=F,sort = FALSE)

# birth_date_civil<-read.csv("C:/Users/Administrator.MAMADA-777/Documents/Junior officers MAGAV validation/birth_date_civil.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
birth_date_civil<-read.csv("C:/Users/Asher/Documents/MAMDA/JOMAGAV/birth_date_civil.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
birth_date_civil<-birth_date_civil[-c(1,4)]
birth_date_civil$birth_date<-as.Date(birth_date_civil$birth_date,format="%d/%m/%Y")
class(birth_date_civil$birth_date)
head(birth_date_civil$birth_date)

n_occur<-data.frame(table(birth_date_civil$personal_number))
n_occur[n_occur$Freq>1,]
class(birth_date_civil$birth_date)
head(birth_date_civil$birth_date)
quit_civil_new <- merge(quit_civil_new ,birth_date_civil,by=c("personal_number"), all.x=T, all.y=F,sort = FALSE)
class(quit_civil_new$birth_date)
head(quit_civil_new$birth_date)

library(plyr)
JOMAG_predictores_criteria_merged_civil_officers=JOMAG_predictores_criteria_merged_civil %>%
  filter(officer==1)
nrow(JOMAG_predictores_criteria_merged_civil_officers)

JOMAG_predictores_criteria_merged_civil_officers <- merge(JOMAG_predictores_criteria_merged_civil_officers,quit_civil_old,by=c("personal_number"), all.x=T, all.y=F,sort = FALSE)
nrow(JOMAG_predictores_criteria_merged_civil_officers)
class(JOMAG_predictores_criteria_merged_civil_officers$birth_date)
head(JOMAG_predictores_criteria_merged_civil_officers$birth_date)
JOMAG_predictores_criteria_merged_civil_officers$birth_date<-as.Date(JOMAG_predictores_criteria_merged_civil_officers$birth_date,format="%d/%m/%Y")
class(JOMAG_predictores_criteria_merged_civil_officers$darga_date)
head(JOMAG_predictores_criteria_merged_civil_officers$darga_date)
JOMAG_predictores_criteria_merged_civil_officers$darga_date<-as.Date(JOMAG_predictores_criteria_merged_civil_officers$darga_date,format="%d/%m/%Y")
class(JOMAG_predictores_criteria_merged_civil_officers$quit_date)
head(JOMAG_predictores_criteria_merged_civil_officers$quit_date)
#JOMAG_predictores_criteria_merged_civil_officers$quit_date<-as.Date(JOMAG_predictores_criteria_merged_civil_officers$quit_date,format="%d/%m/%Y")
JOMAG_predictores_criteria_merged_civil_officers<-rbind.fill(JOMAG_predictores_criteria_merged_civil_officers,quit_civil_new)
nrow(JOMAG_predictores_criteria_merged_civil_officers)

# DAPAR_GIBUSH_MAGAV_civil<-read.csv("C:/Users/Administrator.MAMADA-777/Documents/Junior officers MAGAV validation/DAPAR_GIBUSH_MAGAV_civil.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
DAPAR_GIBUSH_MAGAV_civil<-read.csv("C:/Users/Asher/Documents/MAMDA/JOMAGAV/DAPAR_GIBUSH_MAGAV_civil.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
DAPAR_GIBUSH_MAGAV_civil<-DAPAR_GIBUSH_MAGAV_civil[-c(1,4,5)]
colnames(DAPAR_GIBUSH_MAGAV_civil)[2]<-paste(colnames(DAPAR_GIBUSH_MAGAV_civil)[2],"DAPAR",sep="_")
n_occur<-data.frame(table(DAPAR_GIBUSH_MAGAV_civil$id))
n_occur[n_occur$Freq>1,]
JOMAG_predictores_criteria_merged_civil_officers<- merge(JOMAG_predictores_criteria_merged_civil_officers ,DAPAR_GIBUSH_MAGAV_civil,by=c("id"), all.x=T, all.y=F,sort = FALSE)

# HEBREW_GIBUSH_MAGAV_civil<-read.csv("C:/Users/Administrator.MAMADA-777/Documents/Junior officers MAGAV validation/HEBREW_GIBUSH_MAGAV_civil.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
HEBREW_GIBUSH_MAGAV_civil<-read.csv("C:/Users/Asher/Documents/MAMDA/JOMAGAV/HEBREW_GIBUSH_MAGAV_civil.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
HEBREW_GIBUSH_MAGAV_civil<-HEBREW_GIBUSH_MAGAV_civil[-c(1,4,5)]
colnames(HEBREW_GIBUSH_MAGAV_civil)[2]<-paste(colnames(HEBREW_GIBUSH_MAGAV_civil)[2],"HEBREW",sep="_")
n_occur<-data.frame(table(HEBREW_GIBUSH_MAGAV_civil$id))
n_occur[n_occur$Freq>1,]
JOMAG_predictores_criteria_merged_civil_officers<- merge(JOMAG_predictores_criteria_merged_civil_officers ,HEBREW_GIBUSH_MAGAV_civil,by=c("id"), all.x=T, all.y=F,sort = FALSE)

JOMAG_predictores_criteria_merged_civil_officers$quit <- ifelse(is.na(JOMAG_predictores_criteria_merged_civil_officers$commants),0,
                                                ifelse(JOMAG_predictores_criteria_merged_civil_officers$commants=="התפטרות", 
                                                2,1))
class(JOMAG_predictores_criteria_merged_civil_officers$GibDate)
head(JOMAG_predictores_criteria_merged_civil_officers$GibDate)
JOMAG_predictores_criteria_merged_civil_officers$GibDate<-as.Date(as.character(JOMAG_predictores_criteria_merged_civil_officers$GibDate),format="%d/%m/%Y")
class(JOMAG_predictores_criteria_merged_civil_officers$darga_date)
head(JOMAG_predictores_criteria_merged_civil_officers$darga_date)
# JOMAG_predictores_criteria_merged_civil_officers$darga_date<-as.Date(as.character(JOMAG_predictores_criteria_merged_civil_officers$darga_date),format="%d/%m/%Y")

quit_date = "2020-03-08" 
quit_date<-as.Date(quit_date)
class(quit_date)
head(quit_date)
class(quit_civil_new$quit_date)
head(quit_civil_new$quit_date)

JOMAG_predictores_criteria_merged_civil_officers$tenure <- 
  ifelse(JOMAG_predictores_criteria_merged_civil_officers$quit==1|
         JOMAG_predictores_criteria_merged_civil_officers$quit==2,
         JOMAG_predictores_criteria_merged_civil_officers$quit_date-JOMAG_predictores_criteria_merged_civil_officers$darga_date,
         quit_date-JOMAG_predictores_criteria_merged_civil_officers$darga_date)
JOMAG_predictores_criteria_merged_civil_officers$tenure_years<-
  round(JOMAG_predictores_criteria_merged_civil_officers$tenure/360,2)

JOMAG_predictores_criteria_merged_civil_officers$VaadaGrade_completed <-NA
JOMAG_predictores_criteria_merged_civil_officers$VaadaGrade_completed <-
  as.numeric(ifelse(is.na(JOMAG_predictores_criteria_merged_civil_officers$VaadaGrade),JOMAG_predictores_criteria_merged_civil_officers$FileGrade,JOMAG_predictores_criteria_merged_civil_officers$VaadaGrade))

filtered_FileGrade3=JOMAG_predictores_criteria_merged_civil_officers %>%
  filter(FileGrade==3 & VaadaGrade_completed==3.5)
nrow(filtered_FileGrade3)

filtered_FileGrade35=JOMAG_predictores_criteria_merged_civil_officers %>%
  filter(FileGrade==3.5 & VaadaGrade_completed==4.5)
nrow(filtered_FileGrade35)

JOMAG_predictores_criteria_merged_civil_officers$VaadaGrade_completed[JOMAG_predictores_criteria_merged_civil_officers$FileGrade==3.5 & JOMAG_predictores_criteria_merged_civil_officers$VaadaGrade_completed==4.5] <- 4

filtered_FileGrade5=JOMAG_predictores_criteria_merged_civil_officers %>%
  filter(FileGrade==5 & VaadaGrade_completed==4.5)
nrow(filtered_FileGrade5)

JOMAG_predictores_criteria_merged_civil_officers$VaadaGrade_completed[JOMAG_predictores_criteria_merged_civil_officers$FileGrade==5 & JOMAG_predictores_criteria_merged_civil_officers$VaadaGrade_completed==4.5] <- 5

filtered_FileGrade55=JOMAG_predictores_criteria_merged_civil_officers %>%
  filter(FileGrade==5.5 & VaadaGrade_completed==4.5)
nrow(filtered_FileGrade55)

JOMAG_predictores_criteria_merged_civil_officers$VaadaGrade_completed[JOMAG_predictores_criteria_merged_civil_officers$FileGrade==5.5 & JOMAG_predictores_criteria_merged_civil_officers$VaadaGrade_completed==4.5] <- 5.5

head(JOMAG_predictores_criteria_merged_civil_officers$VaadaGrade_completed)

class(JOMAG_predictores_criteria_merged_civil_officers$VaadaGrade_completed)

library(descr)
library(psych)

mode<-function(X)
{
  temp<-table (as.vector(X))
  names (temp)[temp==max(temp)]
}
options(width = 71,max.print=30000)
# # The 2 commands after the first command, are for cleaning the output file.
JOMAG_predictores_criteria_merged_civil_freq_relevant_columns<-colnames(JOMAG_predictores_criteria_merged_civil_officers[c(843:849,1129,1457:1460,1462,1468,1470:ncol(JOMAG_predictores_criteria_merged_civil_officers))])
out<-""
cat("", out, file="C:/Users/Administrator.MAMADA-777/Documents/Junior officers MAGAV validation/quit.txt", sep="", append=F,fill = T)
# cat("", out, file="C:/Users/Asher/Documents/MAMDA/JOMAGAV/quit.txt", sep="", append=F,fill = T)
suppressWarnings(for(i in JOMAG_predictores_criteria_merged_civil_freq_relevant_columns) {
  newresult1<-round(freq(ordered(JOMAG_predictores_criteria_merged_civil_officers[[i]]), plot = F,main=colnames(JOMAG_predictores_criteria_merged_civil_officers[i]),font=2),2)
  newresult2<-round(describe(as.numeric(unlist(JOMAG_predictores_criteria_merged_civil_officers[[i]]))),2)
  newresult3<-"mode="
  newresult4<-mode(JOMAG_predictores_criteria_merged_civil_officers[[i]])
  newresult5<- "                                                                                               "
  newresult6<- "----------------------------------------------------------------------------"
  out <- capture.output(newresult1,newresult5,newresult2,newresult3,newresult4,newresult5,newresult6)
  out[1]<-""
# cat(colnames(JOMAG_predictores_criteria_merged_civil_officers[i]),out, file="C:/Users/Administrator.MAMADA-777/Documents/Junior officers MAGAV validation/quit.txt", append=T,fill = T)
  cat(colnames(JOMAG_predictores_criteria_merged_civil_officers[i]),out, file="C:/Users/Asher/Documents/MAMDA/JOMAGAV/quit.txt", append=T,fill = T)
  })

# Correlations with tenure
JOMAG_predictores_criteria_merged_civil_officers$VaadaGrade_completed<-
  as.numeric(JOMAG_predictores_criteria_merged_civil_officers$VaadaGrade_completed)

JOMAG_predictores_criteria_merged_civil_officers_relevant_predictors_columns_for_correlations <- JOMAG_predictores_criteria_merged_civil_officers[c(843:849,1129,1431,1434,1440,1468,1470:1472,1476)]
JOMAG_predictores_criteria_merged_civil_officers_relevant_predictors_columns_names_for_correlations <- c(colnames(JOMAG_predictores_criteria_merged_civil_officers[c(843:849,1129,1431,1434,1440,1468,1470:1472,1476)]))
JOMAG_predictores_criteria_merged_civil_officers_relevant_criteria_columns_for_correlations <- JOMAG_predictores_criteria_merged_civil_officers[c(1474)]
JOMAG_predictores_criteria_merged_civil_officers_relevant_criteria_columns_names_for_correlations <- c(colnames(JOMAG_predictores_criteria_merged_civil_officers[c(1474)]))
JOMAG_predictores_criteria_merged_civil_officers_corr_output<-data.frame()[1,]

for(j in 1:length(JOMAG_predictores_criteria_merged_civil_officers_relevant_predictors_columns_names_for_correlations)){
  corr_output_temp<-c()
  for(i in 1:length(JOMAG_predictores_criteria_merged_civil_officers_relevant_criteria_columns_names_for_correlations)){
    corr_temp<-c()
    corr_try <- try(cor.test(as.numeric(JOMAG_predictores_criteria_merged_civil_officers_relevant_predictors_columns_for_correlations[[j]]),as.numeric(JOMAG_predictores_criteria_merged_civil_officers_relevant_criteria_columns_for_correlations[[i]]),use="pairwise.complete.obs"), silent=T)
    corr_temp$"predictor" <-ifelse(class(corr_try)=="try-error", NA, corr_try$estimate)
    corr_temp$p.value <-ifelse(class(corr_try)=="try-error", NA, corr_try$p.value)
    corr_temp$n <-(ifelse(class(corr_try)=="try-error", NA, corr_try$parameter+2))
    corr_temp<-data.frame(corr_temp)
    colnames(corr_temp)[1]<-JOMAG_predictores_criteria_merged_civil_officers_relevant_predictors_columns_names_for_correlations[[j]]
    row.names(corr_temp)<-JOMAG_predictores_criteria_merged_civil_officers_relevant_criteria_columns_names_for_correlations[i]
    corr_output_temp<-rbind (corr_output_temp,corr_temp)
    corr_output_temp <-round(corr_output_temp,2)
  }
  corr_output_temp$""<-"|"
  JOMAG_predictores_criteria_merged_civil_officers_corr_output<-cbind(JOMAG_predictores_criteria_merged_civil_officers_corr_output,corr_output_temp)
}
row.names(JOMAG_predictores_criteria_merged_civil_officers_corr_output)<-JOMAG_predictores_criteria_merged_civil_officers_relevant_criteria_columns_names_for_correlations

for(i in 1:(ncol(JOMAG_predictores_criteria_merged_civil_officers_corr_output)/4)){
  colnames(JOMAG_predictores_criteria_merged_civil_officers_corr_output)[i*4] <- ""
}
# write.csv(JOMAG_predictores_criteria_merged_civil_officers_corr_output,file = "C:/Users/Administrator.MAMADA-777/Documents/Junior officers MAGAV validation/JOMAG_predictores_criteria_merged_civil_officers_corr_output_p-tenure.csv")
write.csv(JOMAG_predictores_criteria_merged_civil_officers_corr_output,file = "C:/Users/Asher/Documents/MAMDA/JOMAGAV/JOMAG_predictores_criteria_merged_civil_officers_corr_output_p-tenure.csv")

library(dplyr)
filtered_quit_FileGrade=JOMAG_predictores_criteria_merged_civil_officers %>%
  filter(!is.na(quit) & !is.na(FileGrade))
filtered_quit_FileGrade %>% 
  group_by(quit) %>%  
  summarise_at(vars(FileGrade),funs(mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),n())) 
# http://www.sthda.com/english/wiki/one-way-anova-test-in-r
aov_Filegrade<-aov(FileGrade~as.factor(quit),data=JOMAG_predictores_criteria_merged_civil_officers)
summary(aov_Filegrade)
TukeyHSD(aov_Filegrade)

filtered_quit_VaadaGrade_completed=JOMAG_predictores_criteria_merged_civil_officers %>%
  filter(!is.na(quit) & !is.na(VaadaGrade_completed))
filtered_quit_VaadaGrade_completed %>% 
  group_by(quit) %>%  
  summarise_at(vars(VaadaGrade_completed),funs(mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),n())) 
aov_VaadaGrade_completed<-aov(VaadaGrade_completed~as.factor(quit),data=JOMAG_predictores_criteria_merged_civil_officers)
summary(aov_VaadaGrade_completed)
TukeyHSD(aov_VaadaGrade_completed)

filtered_quit_final_apptitudes_new=JOMAG_predictores_criteria_merged_civil_officers %>%
  filter(!is.na(quit) & !is.na(final_apptitudes_new))
filtered_quit_final_apptitudes_new %>% 
  group_by(quit) %>%  
  summarise_at(vars(final_apptitudes_new),funs(mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),n())) 
aov_final_apptitudes_new<-aov(final_apptitudes_new~as.factor(quit),data=JOMAG_predictores_criteria_merged_civil_officers)
summary(aov_final_apptitudes_new)
TukeyHSD(aov_final_apptitudes_new)

filtered_quit_dapar_copmlete=JOMAG_predictores_criteria_merged_civil_officers %>%
  filter(!is.na(quit) & !is.na(dapar_copmlete))
filtered_quit_dapar_copmlete %>% 
  group_by(quit) %>%  
  summarise_at(vars(dapar_copmlete),funs(mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),n())) 
aov_dapar_copmlete<-aov(dapar_copmlete~as.factor(quit),data=JOMAG_predictores_criteria_merged_civil_officers)
summary(aov_dapar_copmlete)
TukeyHSD(aov_dapar_copmlete)

filtered_quit_HEBREW_copmlete=JOMAG_predictores_criteria_merged_civil_officers %>%
  filter(!is.na(quit) & !is.na(HEBREW_copmlete))
filtered_quit_HEBREW_copmlete %>% 
  group_by(quit) %>%  
  summarise_at(vars(HEBREW_copmlete),funs(mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),n())) 
aov_HEBREW_copmlete<-aov(HEBREW_copmlete~as.factor(quit),data=JOMAG_predictores_criteria_merged_civil_officers)
summary(aov_HEBREW_copmlete)
TukeyHSD(aov_HEBREW_copmlete)

filtered_quit_HEBREW_copmlete_zscore_internal=JOMAG_predictores_criteria_merged_civil_officers %>%
  filter(!is.na(quit) & !is.na(HEBREW_copmlete_zscore_internal))
filtered_quit_HEBREW_copmlete_zscore_internal %>% 
  group_by(quit) %>%  
  summarise_at(vars(HEBREW_copmlete_zscore_internal),funs(mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),n())) 
aov_HEBREW_copmlete_zscore_internal<-aov(HEBREW_copmlete_zscore_internal~as.factor(quit),data=JOMAG_predictores_criteria_merged_civil_officers)
summary(aov_HEBREW_copmlete_zscore_internal)
TukeyHSD(aov_HEBREW_copmlete_zscore_internal)

filtered_quit_HEBREW_copmlete_zscore_external=JOMAG_predictores_criteria_merged_civil_officers %>%
  filter(!is.na(quit) & !is.na(HEBREW_copmlete_zscore_external))
filtered_quit_HEBREW_copmlete_zscore_external %>% 
  group_by(quit) %>%  
  summarise_at(vars(HEBREW_copmlete_zscore_external),funs(mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),n())) 
aov_HEBREW_copmlete_zscore_external<-aov(HEBREW_copmlete_zscore_external~as.factor(quit),data=JOMAG_predictores_criteria_merged_civil_officers)
summary(aov_HEBREW_copmlete_zscore_external)
TukeyHSD(aov_HEBREW_copmlete_zscore_external)

filtered_quit_SocioGrade_zscore=JOMAG_predictores_criteria_merged_civil_officers %>%
  filter(!is.na(quit) & !is.na(SocioGrade_zscore))
filtered_quit_SocioGrade_zscore %>% 
  group_by(quit) %>%  
  summarise_at(vars(SocioGrade_zscore),funs(mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),n())) 
aov_SocioGrade_zscore<-aov(SocioGrade_zscore~as.factor(quit),data=JOMAG_predictores_criteria_merged_civil_officers)
summary(aov_SocioGrade_zscore)
TukeyHSD(aov_SocioGrade_zscore)

filtered_quit_InterviewGrade=JOMAG_predictores_criteria_merged_civil_officers %>%
  filter(!is.na(quit) & !is.na(InterviewGrade))
filtered_quit_InterviewGrade %>% 
  group_by(quit) %>%  
  summarise_at(vars(InterviewGrade),funs(mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),n())) 
aov_InterviewGrade<-aov(InterviewGrade~as.factor(quit),data=JOMAG_predictores_criteria_merged_civil_officers)
summary(aov_InterviewGrade)
TukeyHSD(aov_InterviewGrade)

#TsiunSofiBefore
JOMAG_predictores_criteria_merged_civil_officers$TsiunSofiBefore<-NA
JOMAG_predictores_criteria_merged_civil_officers$mSofiBefore1<-as.numeric(JOMAG_predictores_criteria_merged_civil_officers$mSofiBefore1)
JOMAG_predictores_criteria_merged_civil_officers$mSofiBefore2<-as.numeric(JOMAG_predictores_criteria_merged_civil_officers$mSofiBefore2)
for(i in 1:nrow(JOMAG_predictores_criteria_merged_civil_officers)){
  JOMAG_predictores_criteria_merged_civil_officers[i,]$TsiunSofiBefore<-rowMeans(subset(JOMAG_predictores_criteria_merged_civil_officers[i,],select=c(mSofiBefore1,mSofiBefore2)),na.rm=T)
}

filtered_quit_TsiunSofiBefore=JOMAG_predictores_criteria_merged_civil_officers %>%
  filter(!is.na(quit) & !is.na(TsiunSofiBefore))
filtered_quit_TsiunSofiBefore %>% 
  group_by(quit) %>%  
  summarise_at(vars(TsiunSofiBefore),funs(mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),n())) 
aov_TsiunSofiBefore<-aov(TsiunSofiBefore~as.factor(quit),data=JOMAG_predictores_criteria_merged_civil_officers)
summary(aov_TsiunSofiBefore)
TukeyHSD(aov_TsiunSofiBefore)

filtered_quit_TsiunSofiAfter=JOMAG_predictores_criteria_merged_civil_officers %>%
  filter(!is.na(quit) & !is.na(TsiunSofiAfter))
filtered_quit_TsiunSofiAfter %>% 
  group_by(quit) %>%  
  summarise_at(vars(TsiunSofiAfter),funs(mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),n())) 
aov_TsiunSofiAfter_zscore<-aov(TsiunSofiAfter_zscore~as.factor(quit),data=JOMAG_predictores_criteria_merged_civil_officers)
summary(aov_TsiunSofiAfter_zscore)
TukeyHSD(aov_TsiunSofiAfter_zscore)

filtered_quit_PersonalityGrade_zscore=JOMAG_predictores_criteria_merged_civil_officers %>%
  filter(!is.na(quit) & !is.na(PersonalityGrade_zscore))
filtered_quit_PersonalityGrade_zscore %>% 
  group_by(quit) %>%  
  summarise_at(vars(PersonalityGrade_zscore),funs(mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),n())) 
aov_PersonalityGrade_zscore<-aov(PersonalityGrade_zscore~as.factor(quit),data=JOMAG_predictores_criteria_merged_civil_officers)
summary(aov_PersonalityGrade_zscore)
TukeyHSD(aov_PersonalityGrade_zscore)

JOMAG_predictores_criteria_merged_civil_officers$before_gap<-abs(JOMAG_predictores_criteria_merged_civil_officers$mSofiBefore1-
                                                          JOMAG_predictores_criteria_merged_civil_officers$mSofiBefore2)

filtered_quit_before_gap=JOMAG_predictores_criteria_merged_civil_officers %>%
  filter(!is.na(quit) & !is.na(before_gap))
filtered_quit_before_gap %>% 
  group_by(quit) %>%  
  summarise_at(vars(before_gap),funs(mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),n())) 
aov_before_gap<-aov(before_gap~as.factor(quit),data=JOMAG_predictores_criteria_merged_civil_officers)
summary(aov_before_gap)
TukeyHSD(aov_before_gap)

JOMAG_predictores_criteria_merged_civil_officers$after_gap<-abs(JOMAG_predictores_criteria_merged_civil_officers$mSofiAfter1-
                                                         JOMAG_predictores_criteria_merged_civil_officers$mSofiAfter2)

filtered_quit_after_gap=JOMAG_predictores_criteria_merged_civil_officers %>%
  filter(!is.na(quit) & !is.na(after_gap))
filtered_quit_after_gap %>% 
  group_by(quit) %>%  
  summarise_at(vars(after_gap),funs(mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),n())) 
aov_after_gap<-aov(after_gap~as.factor(quit),data=JOMAG_predictores_criteria_merged_civil_officers)
summary(aov_after_gap)
TukeyHSD(aov_after_gap)

filtered_quit_tenure_years=JOMAG_predictores_criteria_merged_civil_officers %>%
filter(!is.na(quit) & !is.na(tenure_years))
filtered_quit_tenure_years %>% 
group_by(quit) %>%  
summarise_at(vars(tenure_years),funs(mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),n()))
aov_tenure_years<-aov(tenure_years~as.factor(quit),data=JOMAG_predictores_criteria_merged_civil_officers)
summary(aov_tenure_years)
TukeyHSD(aov_tenure_years)

#sadir/ keva

library(dplyr)
filtered_GibDate=JOMAG_predictores_criteria_merged_civil_officers %>%
  filter(!is.na(GibDate))
filtered_GibDate$gap_darga_date_GibDate<-
  as.numeric(filtered_GibDate$darga_date-filtered_GibDate$GibDate)
library(descr)
library(psych)
options(width = 71,max.print=30000)
round(freq(ordered(filtered_GibDate$gap_darga_date_GibDate), plot = F,main=colnames(filtered_GibDate$gap_darga_date_GibDate),font=2),2)

filtered_GibDate$gap_darga_date_GibDate_zscore <-
  as.numeric(scale(as.numeric(unlist(filtered_GibDate$gap_darga_date_GibDate))))
filtered_GibDate$gap_darga_date_GibDate[abs(filtered_GibDate$gap_darga_date_GibDate_zscore)>4]<-NA
library(descr)
library(psych)
options(width = 71,max.print=30000)
round(freq(ordered(filtered_GibDate$gap_darga_date_GibDate), plot = F,main=colnames(filtered_GibDate$gap_darga_date_GibDate),font=2),2)

mean_gap_darga_date_GibDate<-round(as.numeric(mean(filtered_GibDate$gap_darga_date_GibDate,na.rm = T)),0)
mean_gap_darga_date_GibDate

min_gap_darga_date_GibDate<-round(as.numeric(min(filtered_GibDate$gap_darga_date_GibDate,na.rm = T)),0)
min_gap_darga_date_GibDate

JOMAG_predictores_criteria_merged_civil_officers$GibDate_computed<-
as.Date(JOMAG_predictores_criteria_merged_civil_officers$darga_date)-mean_gap_darga_date_GibDate
class(JOMAG_predictores_criteria_merged_civil_officers$GibDate_computed)
head(JOMAG_predictores_criteria_merged_civil_officers$GibDate_computed)

for (i in nrow(JOMAG_predictores_criteria_merged_civil_officers)) {
  if (!is.na(JOMAG_predictores_criteria_merged_civil_officers[i,]$GibDate)){
    JOMAG_predictores_criteria_merged_civil_officers[i,]$GibDate_computed<-
      JOMAG_predictores_criteria_merged_civil_officers[,i]$GibDate
}
}

class(JOMAG_predictores_criteria_merged_civil_officers$GibDate)
head(JOMAG_predictores_criteria_merged_civil_officers$GibDate,300)
class(JOMAG_predictores_criteria_merged_civil_officers$GibDate_computed)
head(JOMAG_predictores_criteria_merged_civil_officers$GibDate_computed,300)

library(readr)
locale("he")
# ranks_new_civil<-read_csv("C:/Users/Administrator.MAMADA-777/Documents/Junior officers MAGAV validation/ranks_new_civil.csv",locale = locale(date_names = "he", encoding = "ISO-8859-8"))
ranks_new_civil<-read_csv("C:/Users/Asher/Documents/MAMDA/JOMAGAV/ranks_new_civil.csv",locale = locale(date_names = "he", encoding = "ISO-8859-8"))
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

library(dplyr)
filtered_GibDate_computed<-JOMAG_predictores_criteria_merged_civil_officers %>%
  select(personal_number,GibDate_computed)
ranks_new_civil <-
  merge(ranks_new_civil,filtered_GibDate_computed,by=c("personal_number"), all.x=T, all.y=F,sort = FALSE)
ranks_new_civil=ranks_new_civil %>%
  filter(!is.na(GibDate_computed)) %>%
  filter(rank_new_date<GibDate_computed) %>%
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
ranks_new_civil$GibDate_computed<-NULL

JOMAG_predictores_criteria_merged_civil_officers_ranks_new_civil <-
  merge(JOMAG_predictores_criteria_merged_civil_officers,ranks_new_civil,by=c("personal_number"), all.x=T, all.y=F,sort = FALSE)

class(JOMAG_predictores_criteria_merged_civil_officers_ranks_new_civil$GibDate_computed)
head(JOMAG_predictores_criteria_merged_civil_officers_ranks_new_civil$GibDate_computed)
class(JOMAG_predictores_criteria_merged_civil_officers_ranks_new_civil$rank_new_date)
head(JOMAG_predictores_criteria_merged_civil_officers_ranks_new_civil$rank_new_date)

JOMAG_predictores_criteria_merged_civil_officers_ranks_new_civil$GibDate_computed_rank_new_date_gap<-
  as.numeric(JOMAG_predictores_criteria_merged_civil_officers_ranks_new_civil$GibDate_computed-JOMAG_predictores_criteria_merged_civil_officers_ranks_new_civil$rank_new_date)
JOMAG_predictores_criteria_merged_civil_officers_ranks_new_civil$keva<-ifelse((JOMAG_predictores_criteria_merged_civil_officers_ranks_new_civil$rank_new=="סמר" & JOMAG_predictores_criteria_merged_civil_officers_ranks_new_civil$GibDate_computed_rank_new_date_gap>540) |
                                                     JOMAG_predictores_criteria_merged_civil_officers_ranks_new_civil$rank_new=="סמש" |
                                                     JOMAG_predictores_criteria_merged_civil_officers_ranks_new_civil$rank_new=="רסב" |
                                                     JOMAG_predictores_criteria_merged_civil_officers_ranks_new_civil$rank_new=="רסל"|
                                                     JOMAG_predictores_criteria_merged_civil_officers_ranks_new_civil$rank_new=="רסמ"|
                                                     JOMAG_predictores_criteria_merged_civil_officers_ranks_new_civil$rank_new=="רסר",
                                                   2,ifelse(JOMAG_predictores_criteria_merged_civil_officers_ranks_new_civil$rank_new=="סמר" & JOMAG_predictores_criteria_merged_civil_officers_ranks_new_civil$GibDate_computed_rank_new_date_gap<=540,
                                                            1,0))

library(descr)
library(psych)
freq(ranks_new_civil$rank_new, plot = F,main=colnames(ranks_new_civil$rank_new),font=2)
round(freq(ordered(as.numeric(unlist(JOMAG_predictores_criteria_merged_civil_officers_ranks_new_civil$keva))), plot = F,main=colnames(JOMAG_predictores_criteria_merged_civil_officers_ranks_new_civil$keva),font=2),2)
JOMAG_predictores_criteria_merged_civil_officers_ranks_new_civil$keva[JOMAG_predictores_criteria_merged_civil_officers_ranks_new_civil$keva==1] <- 0
round(freq(ordered(as.numeric(unlist(JOMAG_predictores_criteria_merged_civil_officers_ranks_new_civil$keva))), plot = F,main=colnames(JOMAG_predictores_criteria_merged_civil_officers_ranks_new_civil$keva),font=2),2)
freq(JOMAG_predictores_criteria_merged_civil_officers_ranks_new_civil$rank_new, plot = F,main=colnames(JOMAG_predictores_criteria_merged_civil_officers_ranks_new_civil$rank_new),font=2)

library(dplyr)
filtered_keva_tenure=JOMAG_predictores_criteria_merged_civil_officers_ranks_new_civil %>%
  filter(!is.na(keva) & !is.na(tenure))
filtered_keva_tenure %>% 
  group_by(keva) %>%  
  summarise_at(vars(tenure),funs(mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),n())) 
t.test(as.numeric(JOMAG_predictores_criteria_merged_civil_officers_ranks_new_civil$tenure)~as.numeric(JOMAG_predictores_criteria_merged_civil_officers_ranks_new_civil$keva),use="pairwise.complete.obs")

quit_date = "2020-03-08" 
quit_date<-as.Date(quit_date)
class(quit_date)

class(JOMAG_predictores_criteria_merged_civil_officers$quit_date)

#JOMAG_predictores_criteria_merged_civil_officers$birth_date<-as.Date(JOMAG_predictores_criteria_merged_civil_officers$birth_date,format="%d/%m/%Y")
class(JOMAG_predictores_criteria_merged_civil_officers$birth_date)
head(JOMAG_predictores_criteria_merged_civil_officers$birth_date)

#tenure_age in days
JOMAG_predictores_criteria_merged_civil_officers$tenure_age <- 
  ifelse(JOMAG_predictores_criteria_merged_civil_officers$quit==1|
         JOMAG_predictores_criteria_merged_civil_officers$quit==2,
         JOMAG_predictores_criteria_merged_civil_officers$quit_date-JOMAG_predictores_criteria_merged_civil_officers$birth_date,
         quit_date-JOMAG_predictores_criteria_merged_civil_officers$birth_date)

head(JOMAG_predictores_criteria_merged_civil_officers$tenure_age)

cor.test(as.numeric(JOMAG_predictores_criteria_merged_civil_officers$tenure),as.numeric(JOMAG_predictores_criteria_merged_civil_officers$tenure_age),use="pairwise.complete.obs")

JOMAG_predictores_criteria_merged_civil_officers$tenure_age_years<-round(JOMAG_predictores_criteria_merged_civil_officers$tenure_age/360,2)


filtered_quit_tenure_age_years=JOMAG_predictores_criteria_merged_civil_officers %>%
  filter(!is.na(quit) & !is.na(tenure_age_years))
filtered_quit_tenure_age_years %>% 
  group_by(quit) %>%  
  summarise_at(vars(tenure_age_years),funs(mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),n())) 
aov_tenure_age_years<-aov(tenure_age_years~as.factor(quit),data=JOMAG_predictores_criteria_merged_civil_officers)
summary(aov_tenure_age_years)
TukeyHSD(aov_tenure_age_years)

# tifkud
tifkud_civil<-read.csv("C:/Users/Asher/Documents/MAMDA/JOMAGAV/tifkud_civil.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
tifkud_civil<-tifkud_civil[-1]

tifkud_civil[2][tifkud_civil[2]==0] <- NA

JOMAG_predictores_criteria_merged_civil_officers<- merge(JOMAG_predictores_criteria_merged_civil_officers ,tifkud_civil,by=c("personal_number"), all.x=T, all.y=F,sort = FALSE)

library (dplyr)
filtered_quit_tifkud=JOMAG_predictores_criteria_merged_civil_officers %>%
  filter(!is.na(quit) & !is.na(tifkud))
filtered_quit_tifkud %>% 
  group_by(quit) %>%  
  summarise_at(vars(tifkud),funs(mean(.,na.rm=TRUE),sd(.,na.rm=TRUE),n()))
aov_tifkud<-aov(tifkud~as.factor(quit),data=JOMAG_predictores_criteria_merged_civil_officers)
summary(aov_tifkud)
TukeyHSD(aov_tifkud)

mean(tifkud_civil$tifkud)
sd(tifkud_civil$tifkud)




# blue_officers
# library(readr)
# locale("he")
# JOMAG_predictores_criteria_merged_civil<-read_csv("C:/Users/Asher/Documents/MAMDA/JOMAGAV/JOMAG_predictores_criteria_merged_civil_old.csv",locale = locale(date_names = "he", encoding = "ISO-8859-8"))
# JOMAG_predictores_criteria_merged_civil<-JOMAG_predictores_criteria_merged_civil[-1]
# 
# blue_officers <- read.csv("C:/Users/Asher/Documents/MAMDA/JOMAGAV/blue_officers_civil.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
# # blue_officers <- read.csv("C:/Users/Administrator.MAMADA-777/Documents/Junior officers MAGAV validation/blue_officers_civil.csv",header=T, sep=",", quote="\"", dec=".", fill=T, comment.char="")
# 
# blue_officers<-blue_officers[-1]
# 
# blue_officers$blue_date<-as.Date(unlist(blue_officers$blue_date),format="%d/%m/%Y")
# 
# JOMAG_predictors_criteria_merged_civil_officers <- merge(JOMAG_predictores_criteria_merged_civil_officers, blue_officers,by=c("personal_number"), all.x=T, all.y=F,sort = FALSE)

# Logistic regression analysis

JOMAG_predictores_criteria_merged_civil_officers$quit_dich12_0<-
  ifelse(JOMAG_predictores_criteria_merged_civil_officers$quit==1|
         JOMAG_predictores_criteria_merged_civil_officers$quit==2,1,0)

JOMAG_predictores_criteria_merged_civil_officers$quit_dich0_2<-
  as.factor(ifelse(JOMAG_predictores_criteria_merged_civil_officers$quit==0,0,
  ifelse(JOMAG_predictores_criteria_merged_civil_officers$quit==2,2,NA)))


logitquit <- glm(quit_dich12_0 ~ final_apptitudes_new 
               + tenure_age_years, 
               data=JOMAG_predictores_criteria_merged_civil_officers, 
               family=binomial(link="logit"))

summary(logitquit)


logitquit <- glm(quit_dich0_2 ~ final_apptitudes_new 
                 + tenure_age_years, 
                 data=JOMAG_predictores_criteria_merged_civil_officers, 
                 family=binomial(link="logit"))

summary(logitquit)


# Alternative to the Gibush.    $$$$$$$$$$$$$$$$$$$$$$$$$$

#Tali code

# End of Tali code.
# Beginning of civil code.****************************
options(digits = 2)
# soc_mac
library(readr)
locale("he")
soc_mac_civil<-read_csv("C:/Users/Asher/Documents/MAMDA/JOMAGAV/alternative_gibush/courses_soc_mac_civil.csv",locale = locale(date_names = "he", encoding = "ISO-8859-8"))
soc_mac_civil<-soc_mac_civil[-1]
colnames(soc_mac_civil)
nrow(soc_mac_civil)
head(soc_mac_civil$RAvg5_courses_soc_mac)

library(dplyr)
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
           GroupId_courses_soc_mac==5074)

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

library(dplyr)
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
# soc_mac_civil_am[c(4:45)][abs(soc_mac_civil_am[c(4:45)])>4]<-NA

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
head(filtered_soc_mac_civil_am)
filtered_soc_mac_civil_am$am_courses_soc_mac<-format(round(filtered_soc_mac_civil_am$am_courses_soc_mac,2), nsmall = 2)


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
gibush_candidates_kakatz_03.2021_civil$apptitudes<-format(round(gibush_candidates_kakatz_03.2021_civil$apptitudes,2), nsmall = 2)

# soc_mac_civil
class(gibush_candidates_kakatz_03.2021_civil)
gibush_candidates_kakatz_03.2021_civil<-as.data.frame(gibush_candidates_kakatz_03.2021_civil)
class(gibush_candidates_kakatz_03.2021_civil$final_mac_course_score)

class(filtered_soc_mac_civil_am)
class(gibush_candidates_kakatz_03.2021_civil$personal_number)
class(filtered_soc_mac_civil_am$personal_number)

gibush_candidates_kakatz_03.2021_civil <- 
merge(gibush_candidates_kakatz_03.2021_civil,filtered_soc_mac_civil_am,by=c("personal_number"), all.x=T, all.y=F,sort = FALSE)

gibush_candidates_kakatz_03.2021_civil$am_courses_soc_mac<-as.numeric(gibush_candidates_kakatz_03.2021_civil$am_courses_soc_mac)

# courses_soc file on TALI includes candidates until 01.2020. 
# Hence I've got the data for the next 4 candidates manually from MEGAMA. 

# gibush_candidates_kakatz_03.2021_civil[27][gibush_candidates_kakatz_03.2021_civil[2]==16] <- 0.0504
# gibush_candidates_kakatz_03.2021_civil[27][gibush_candidates_kakatz_03.2021_civil[2]==20] <- -0.2446
# gibush_candidates_kakatz_03.2021_civil[27][gibush_candidates_kakatz_03.2021_civil[2]==27] <- 0.1511
# gibush_candidates_kakatz_03.2021_civil[27][gibush_candidates_kakatz_03.2021_civil[2]==40] <- -0.9361
# 
# 
# gibush_candidates_kakatz_03.2021_civil[28][gibush_candidates_kakatz_03.2021_civil[2]==16] <- "מכים מחלקה 4"
# gibush_candidates_kakatz_03.2021_civil[28][gibush_candidates_kakatz_03.2021_civil[2]==20] <- "מכים מחלקה 4"
# gibush_candidates_kakatz_03.2021_civil[28][gibush_candidates_kakatz_03.2021_civil[2]==27] <- "מכים מחלקה 4"
# gibush_candidates_kakatz_03.2021_civil[28][gibush_candidates_kakatz_03.2021_civil[2]==40] <- "מכים מחלקה 1"

library(descr)
library(psych)
describe(as.numeric(unlist(gibush_candidates_kakatz_03.2021_civil$am_courses_soc_mac)))
freq(ordered(round(gibush_candidates_kakatz_03.2021_civil$am_courses_soc_mac,2)), plot = F,main=colnames(gibush_candidates_kakatz_03.2021_civil$am_courses_soc_mac),font=2)
freq(ordered(gibush_candidates_kakatz_03.2021_civil$GroupName_courses_soc_mac), plot = F,main=colnames(gibush_candidates_kakatz_03.2021_civil$GroupName_courses_soc_mac),font=2)

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

#**********************get and share scores***************************


# https://www.youtube.com/watch?v=yjRrsha0TdQ
# https://googledrive.tidyverse.org/reference/drive_auth.html
# https://cran.r-project.org/web/packages/googledrive/googledrive.pdf
#  The access can be removed, through my google account. preffered by drive_deauth() bellow.
library(googledrive)
drive_auth(email = "asherr1211@gmail.com")
# drive_find(n_max = 10,type = "csv")
library(readr)
locale("he")

# use this*****************
# https://stackoverflow.com/questions/30993185/set-a-timer-in-r-to-execute-a-program


#**********************get and share scores - loop starts***************************

#run the following rows only once-when creating the Google sheet.

# drive_download("gibush_candidates_kakatz_03.2021_civil_final_scores****",path = "C:/Users/Asher/Documents/MAMDA/JOMAGAV/alternative_gibush/gibush_candidates_kakatz_03.2021_civil_final_scores_gd",type = "csv",overwrite=T)
# gibush_candidates_kakatz_03.2021_civil_final_scores_gd<-read_csv("C:/Users/Asher/Documents/MAMDA/JOMAGAV/alternative_gibush/gibush_candidates_kakatz_03.2021_civil_final_scores_gd.csv")
# gibush_candidates_kakatz_03.2021_civil_final_scores_gd<-gibush_candidates_kakatz_03.2021_civil_final_scores_gd[-1]
# gibush_candidates_kakatz_03.2021_civil_final_scores_gd[2]<- gibush_candidates_kakatz_03.2021_civil$personal_number
# write.csv(gibush_candidates_kakatz_03.2021_civil_final_scores_gd, file="C:/Users/Asher/Documents/MAMDA/JOMAGAV/alternative_gibush/gibush_candidates_kakatz_03.2021_civil_final_scores_gd.csv")
# drive_update("https://docs.google.com/spreadsheets/d/1n0pmKCYDixljzyBJuh_YuyshwCZVlY-7v7Uq7HgptHc/edit#gid=0", "C:/Users/Asher/Documents/MAMDA/JOMAGAV/alternative_gibush/gibush_candidates_kakatz_03.2021_civil_final_scores_gd.csv")

# in the beginning, once, share the Google sheet with Talia and Miki, by running the next code rows.

# drive_share("https://docs.google.com/spreadsheets/d/1n0pmKCYDixljzyBJuh_YuyshwCZVlY-7v7Uq7HgptHc/edit#gid=0",
#             role = "writer",
#             type = "user",
#             emailAddress = "asherr1211@gmail.com",
#             emailMessage = "Hi Asher. This file was sent to Talia and Miki"
# )
# 
# drive_share("https://docs.google.com/spreadsheets/d/1n0pmKCYDixljzyBJuh_YuyshwCZVlY-7v7Uq7HgptHc/edit#gid=0",
#             role = "writer",
#             type = "user",
#             emailAddress = "talyashmueli@gmail.com",
#             emailMessage = "Hi Talia. Please add data."
# )
# 
# 
# drive_share("https://docs.google.com/spreadsheets/d/1n0pmKCYDixljzyBJuh_YuyshwCZVlY-7v7Uq7HgptHc/edit#gid=0",
#             role = "writer",
#             type = "user",
#             emailAddress = "Ramikey1@gmail.com",
#             emailMessage = "Hi Miki. This file was sent to Talia. You also can add data."
# )

# drive_share("https://docs.google.com/spreadsheets/d/1n0pmKCYDixljzyBJuh_YuyshwCZVlY-7v7Uq7HgptHc/edit#gid=0",
#             role = "writer",
#             type = "user",
#             emailAddress = "Lizhazan10@gmail.com",
#             emailMessage = "Hi Liz. This file was sent to Talia. You also can add data."
# )

# drive_share("https://docs.google.com/spreadsheets/d/1n0pmKCYDixljzyBJuh_YuyshwCZVlY-7v7Uq7HgptHc/edit#gid=0",
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
drive_update("https://docs.google.com/spreadsheets/d/1n0pmKCYDixljzyBJuh_YuyshwCZVlY-7v7Uq7HgptHc/edit#gid=0", "C:/Users/Asher/Documents/MAMDA/JOMAGAV/alternative_gibush/gibush_candidates_kakatz_03.2021_civil_final_scores_gd.csv")

#   timestamp()
#   
#   n <- n+1
#   if (n == 3){ # set the n(n times the code should run)
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


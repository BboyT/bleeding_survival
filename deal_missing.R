rm(list = ls())

library(sqldf)
library(DataExplorer)
library(mice)
library(CBCgrps)
library(openxlsx)

gh_all<-read.csv("./mimic_data/gh.csv")
gh_big_blooding<-read.xlsx("./mimic_data/gh_big_blooding.xlsx",sheet = 1)

height<-read.csv("./mimic_data/gh_before_height.csv")
weight<-read.csv("./mimic_data/gh_weight.csv")
sofa<-read.csv("./mimic_data/gh_firstday_sofa.csv")
gcs<-read.csv("./mimic_data/gh_firstday_gcs.csv")
lab<-read.csv("./mimic_data/gh_firstday_lab.csv")
vital<-read.csv("./mimic_data//gh_firstday_vital.csv")
bg_art<-read.csv("./mimic_data/gh_firstday_bg_art.csv")
uo<-read.csv("./mimic_data/gh_firstday_uo.csv")

df<-sqldf("select distinct * from gh_all
          left join height on height.stay_id = gh_all.stay_id
          left join weight on weight.stay_id = gh_all.stay_id
          left join bg_art on bg_art.stay_id = gh_all.stay_id
          left join sofa on sofa.stay_id = gh_all.stay_id
          left join gcs on gcs.stay_id = gh_all.stay_id
          left join lab on lab.stay_id = gh_all.stay_id
          left join vital on vital.stay_id = gh_all.stay_id
          left join uo on uo.stay_id = gh_all.stay_id
          left join gh_big_blooding on gh_big_blooding.stay_id = gh_all.stay_id")


#where gh_all.los_survival is not null")

df$big_blooding[is.na(df$big_blooding)]<-0
#table(df$hospital_expire_flag)

dup_cols <- duplicated(names(df))
names(df)[dup_cols] <- sub("\\.(.*)", "", names(df)[dup_cols], perl = TRUE)
dup_cols <- duplicated(names(df))
df1 <- df[, !dup_cols]


missing_prop <- colMeans(is.na(df1))
cols_to_remove <- names(df1)[missing_prop > 0.40]
df2 <- df1[, !(names(df1) %in% cols_to_remove)]
plot_missing(df2)


#gender
df2$sex[df2$gender == "F" ]<-0
df2$sex[df2$gender == "M" ]<-1

#race
df2$ethnicity[df2$race == 'WHITE'] <- 1
df2$ethnicity[df2$race == 'WHITE - BRAZILIAN'] <- 1
df2$ethnicity[df2$race == 'WHITE - EASTERN EUROPEAN'] <- 1
df2$ethnicity[df2$race == 'WHITE - OTHER EUROPEAN'] <- 1
df2$ethnicity[df2$race == 'WHITE - RUSSIAN'] <- 1


df2$ethnicity[df2$race == 'BLACK/AFRICAN'] <- 2
df2$ethnicity[df2$race == 'BLACK/AFRICAN AMERICAN'] <- 2
df2$ethnicity[df2$race == 'BLACK/CAPE VERDEAN'] <- 2
df2$ethnicity[df2$race == 'BLACK/CARIBBEAN ISLAND'] <- 2

df2$ethnicity[df2$race == 'ASIAN'] <- 3
df2$ethnicity[df2$race == 'ASIAN - ASIAN INDIAN'] <- 3
df2$ethnicity[df2$race == 'ASIAN - CHINESE'] <- 3
df2$ethnicity[df2$race == 'ASIAN - SOUTH EAST ASIAN'] <- 3
df2$ethnicity[df2$race == 'HISPANIC/LATINO - DOMINICAN'] <- 3
df2$ethnicity[df2$race == 'HISPANIC/LATINO - GUATEMALAN'] <- 3
df2$ethnicity[df2$race == 'HISPANIC/LATINO - HONDURAN'] <- 3
df2$ethnicity[df2$race == 'ISPANIC/LATINO - PUERTO RICAN'] <- 3

df2$ethnicity[df2$race == 'UNKNOWN' ] <- 4
df2$ethnicity[df2$race == 'PATIENT DECLINED TO ANSWER' ] <- 4
df2$ethnicity[df2$race == 'PORTUGUESE'] <- 4
df2$ethnicity[df2$race == 'OTHER' ] <- 4

df2$ethnicity[is.na(df2$ethnicity)]<-4

df3 <- df2[, !(names(df2) %in% c("race", "gender",'dod','subject_id','stay_id','hadm_id','weight_admit'))]

time_cols <- grep("time$", names(df3), value = TRUE)
df4 <- df3[, !(names(df3) %in% time_cols)]


jpeg(file='test.jpeg', height=5690, width=1980,bg = "white",res = 300)
plot_missing(df4)
dev.off()
write.csv(na.omit(df4),"hos_data_no_imputation.csv")

#进行缺失值填补
imputed_data <- mice(df4, m = 5, maxit = 50, meth = "pmm", seed = 123)
completed_data <- complete(imputed_data, 1)
df5 <- as.data.frame(completed_data)
plot_missing(df5)
write.csv(df5,'data_30.csv')







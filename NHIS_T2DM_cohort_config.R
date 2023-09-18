# Flow 설명

# 1. 청구 기록에서 진단 + 약물 처방 데이터의 기록 가져옴. (진단일 확인용)
# 2. 약물기록은 진단 후 365일 이내에 있어야 함.
# 3. 검진 기록과 청구 데이터 합치기
# 4. 질병 최초 판정일 찾기 (검진 or (진단 + 약물))
# 5. 건강 검진 관련 변수 생성(e.g., 흡연, 음주, 운동)
# 6. 전체 데이터에서 필요한 시기의 데이터를 선택(e.g., 40대 마지막 검진, 50대 첫번째 검진)

# 진단 & 약물 -------------------------------------------------------------
require(data.table)
require(haven)
require(dplyr)
require(tidyr)
t2dm_id <- setDT(read_sas('data_source/t2dm_id.sas7bdat'))

t2dm_id[,`:=`(
  T2DM_DIAG_DATE = as.Date(dates_first_diag,'%Y%m%d'),
  T2DM_DRUG_DATE = as.Date(dates_first_drug,'%Y%m%d'),
  dates_first_diag = NULL,
  dates_first_drug = NULL
)]

# 진단, 약물 365일 이내 

t2dm_id <- t2dm_id[difftime(T2DM_DRUG_DATE, T2DM_DIAG_DATE, units='days')>=0 &
                     difftime(T2DM_DRUG_DATE, T2DM_DIAG_DATE, units='days')<365]

# 검진과 진단 기록 합치기 

t2dm_cohort <- left_join(checkup, t2dm_id, by='PERSON_ID') %>% setDT()
setnames(t2dm_cohort, 'STND_Y','CHECKUP_YEAR')
setnames(t2dm_cohort, 'HME_DT','CHECKUP_DATE')

# 고혈압 진단 또는 검진 판정일 찾기 -----------------------------------------------------

# age when diagnosed
t2dm_diag_age <- t2dm_cohort[!is.na(T2DM_DIAG_DATE)][substr(T2DM_DIAG_DATE,1,4)==
                                                      CHECKUP_YEAR,.(PERSON_ID, T2DM_DIAG_AGE = AGE)]
t2dm_cohort <- left_join(t2dm_cohort,t2dm_diag_age, by='PERSON_ID') %>% setDT()

# first T2DM date when checkup
t2dm_checkup <- t2dm_cohort[t2dm_cohort[BP_HIGH>=140|BP_LWST>=90,.I[1L], by=PERSON_ID]$V1,.(PERSON_ID, T2DM_CHECKUP_DATE=CHECKUP_DATE, T2DM_CHECKUP_AGE = AGE)]
t2dm_cohort <- left_join(t2dm_cohort, t2dm_checkup, by='PERSON_ID') %>% setDT()
names(t2dm_cohort)

# having checkup records of 40's 50's 
t2dm_4050_id <- t2dm_cohort[,.(min_age=min(AGE), max_age=max(AGE)),by='PERSON_ID'][
  as.numeric(substr(min_age,1,1)==4) & as.numeric(substr(max_age,1,1)>=5)
  ,PERSON_ID]

t2dm_ages <- dcast(t2dm_cohort[PERSON_ID %in% t2dm_4050_id], PERSON_ID ~ AGE, value.var='AGE')
t2dm_ages[,before_50:= rowSums(.SD), .SDcols=2:11]
t2dm_ages[,after_50 := rowSums(.SD), .SDcols=12:21]
t2dm_target_id <- t2dm_ages[before_50>=1 & after_50>=1,PERSON_ID]

# having records at least once each
t2dm_gj_eachone <- t2dm_cohort[PERSON_ID %in% t2dm_target_id]
t2dm_gj_eachone <- t2dm_gj_eachone[,.(PERSON_ID, CHECKUP_DATE, CHECKUP_YEAR, T2DM_DIAG_DATE, T2DM_DIAG_AGE,
                                      T2DM_CHECKUP_DATE, T2DM_CHECKUP_AGE,
                                      SEX,AGE,BMI,FMLY_HPRTS_PATIEN_YN, 
                                      BP_HIGH,BP_LWST, BLDS,OLIG_PROTE_CD,
                                      WLK30_WEK_FREQ_ID, MOV20_WEK_FREQ_ID,MOV30_WEK_FREQ_ID, #exercise
                                      SMK_STAT_TYPE_RSPS_CD, DSQTY_RSPS_CD,CUR_DSQTY_RSPS_CD, # smoking
                                      DRNK_HABIT_RSPS_CD,TM1_DRKQTY_RSPS_CD,DRNK_HABIT_RSPS_CD #drinking
)]

# 음주, 흡연, 운동 --------------------------------------------------------------

t2dm_gj_eachone[,`:=`(
  WLK30_WEK_FREQ_ID = ifelse(WLK30_WEK_FREQ_ID=="",NA,as.numeric(WLK30_WEK_FREQ_ID)-1),
  MOV20_WEK_FREQ_ID = ifelse(MOV20_WEK_FREQ_ID=="",NA,as.numeric(MOV20_WEK_FREQ_ID)-1),
  MOV30_WEK_FREQ_ID = ifelse(MOV30_WEK_FREQ_ID=="",NA,as.numeric(MOV30_WEK_FREQ_ID)-1)
)]

t2dm_gj_eachone[,`:=`(
  MET_WALK = (WLK30_WEK_FREQ_ID * 20 * 3.3),
  MET_MED = (MOV20_WEK_FREQ_ID * 30 * 4.0),
  MET_HIGH = (MOV30_WEK_FREQ_ID * 30 * 8.0)
)]
t2dm_gj_eachone[,MET_CON := MET_WALK + MET_MED + MET_HIGH]
t2dm_gj_eachone[,MET_CAT := as.factor(ifelse(MET_HIGH>=1500 | MET_CON>=3000,'High',
                                             ifelse(MOV30_WEK_FREQ_ID>=4 | MOV20_WEK_FREQ_ID>=6 | MET_CON>=600,'Middle',
                                                    ifelse(MET_CON==0,'None','Low'))))]

t2dm_gj_eachone[,SMOKING:= ifelse(SMK_STAT_TYPE_RSPS_CD == 1,'None', 
                                  ifelse(CHECKUP_YEAR <2009 & DSQTY_RSPS_CD==1,'Light',
                                         ifelse(CHECKUP_YEAR < 2009 & DSQTY_RSPS_CD==2,'Moderate',
                                                ifelse(CHECKUP_YEAR < 2009 & DSQTY_RSPS_CD>=3,'Heavy',
                                                       ifelse(CHECKUP_YEAR<2009 & DSQTY_RSPS_CD=="",NA,
                                                              ifelse(CHECKUP_YEAR>=2009 & CUR_DSQTY_RSPS_CD <10,'Light',
                                                                     ifelse(CHECKUP_YEAR>=2009 & CUR_DSQTY_RSPS_CD <20,'Moderate',
                                                                            ifelse(CHECKUP_YEAR>=2009 & CUR_DSQTY_RSPS_CD>=20,'Heavy',NA)))))))
)]

t2dm_gj_eachone[,DRINK_HABIT_RSPS_0208 := 
                  ifelse(CHECKUP_YEAR<2009 & DRNK_HABIT_RSPS_CD == 1,0, ifelse(
                    CHECKUP_YEAR<2009 & DRNK_HABIT_RSPS_CD ==2,1, ifelse(
                      CHECKUP_YEAR<2009 & DRNK_HABIT_RSPS_CD==3,2, ifelse(
                        CHECKUP_YEAR<2009 & DRNK_HABIT_RSPS_CD==4,4, ifelse(
                          CHECKUP_YEAR<2009 & DRNK_HABIT_RSPS_CD==5,7, NA)))))]

t2dm_gj_eachone[,TM1_DRKQTY_RSPS_0208 := 
                  ifelse(CHECKUP_YEAR<2009 & TM1_DRKQTY_RSPS_CD == 1,3, ifelse(
                    CHECKUP_YEAR<2009 & TM1_DRKQTY_RSPS_CD ==2,7, ifelse(
                      CHECKUP_YEAR<2009 & TM1_DRKQTY_RSPS_CD==3,11, ifelse(
                        CHECKUP_YEAR<2009 & TM1_DRKQTY_RSPS_CD==4,14, NA))))]


t2dm_gj_eachone[,DRINK_HABIT_RSPS_0915 := as.numeric(ifelse(CHECKUP_YEAR>=2009, 
                                                            as.numeric(DRNK_HABIT_RSPS_CD)-1,
                                                            NA))]

length(unique(t2dm_gj_eachone$PERSON_ID))


t2dm_gj_eachone[,DRINKING_CON :=ifelse(CHECKUP_YEAR<2009, (DRINK_HABIT_RSPS_0208 * TM1_DRKQTY_RSPS_0208),
                                       ifelse(CHECKUP_YEAR>=2009, as.numeric(DRINK_HABIT_RSPS_0915) * as.numeric(TM1_DRKQTY_RSPS_CD),NA))]

t2dm_gj_eachone[,DRINKING := ifelse(DRINKING_CON==0,'none',
                                    ifelse(DRINKING_CON<15,'light',
                                           ifelse(DRINKING_CON<30,'moderate','heavy')))]


# DF 만들기 ------------------------------------------------------------------

# last checkup before 50
t2dm_last_gj_in_40 <- t2dm_gj_eachone[t2dm_gj_eachone[AGE<50,.I[.N],by=PERSON_ID]$V1,.(PERSON_ID,SEX,AGE,
                                                                                       BMI,FMLY_HPRTS_PATIEN_YN,
                                                                                       CHECKUP_DATE,CHECKUP_YEAR,
                                                                                       BP_HIGH,BP_LWST, BLDS,OLIG_PROTE_CD, 
                                                                                       SMOKING, DRINKING,MET_CAT,
                                                                                       T2DM_DIAG_DATE, T2DM_DIAG_AGE,
                                                                                       T2DM_CHECKUP_AGE, T2DM_CHECKUP_DATE)]

# first checkup after 52
t2dm_first_gj_after_52 <- t2dm_gj_eachone[t2dm_gj_eachone[AGE>=52,.I[1L],by=PERSON_ID]$V1,.(PERSON_ID,SEX,AGE,
                                                                                            BMI,FMLY_HPRTS_PATIEN_YN,
                                                                                            CHECKUP_DATE,CHECKUP_YEAR,
                                                                                            BP_HIGH,BP_LWST, BLDS,OLIG_PROTE_CD, 
                                                                                            SMOKING, DRINKING,MET_CAT)]

colnames(t2dm_first_gj_after_52) <- paste0(colnames(t2dm_first_gj_after_52),'_2nd')
setnames(t2dm_first_gj_after_52,'PERSON_ID_2nd','PERSON_ID')

# last gj date for calculating observation duration
t2dm_last_gj <- t2dm_gj_eachone[t2dm_gj_eachone[,.I[.N],by=PERSON_ID]$V1,.(PERSON_ID=PERSON_ID,
                                                                           LAST_CHECKUP_DATE = CHECKUP_DATE)]

t2dm_df <- inner_join(t2dm_first_gj_after_52,t2dm_last_gj_in_40,by='PERSON_ID') %>% setDT()
t2dm_df <- inner_join(t2dm_df, t2dm_last_gj, by='PERSON_ID') %>% setDT()


# transform to date forms
dates_vars <- grep('DATE',names(t2dm_df), value=T)
t2dm_df[,(dates_vars):=lapply(.SD, function(x) as.Date(x, '%Y%m%d')), .SDcols=dates_vars]

# Exclude T2DM patients before second checkup date
t2dm_exclusion_id <- t2dm_df[T2DM_DIAG_AGE < 52 | # diagnosed before 52
                              T2DM_CHECKUP_AGE < 52 | #T2DM in checkup before 52
                              T2DM_DIAG_DATE < CHECKUP_DATE_2nd | # T2DM diagnosis before 2nd checkup date
                              T2DM_CHECKUP_DATE < CHECKUP_DATE_2nd # T2DM checkup before 2nd checkup date
                            ,PERSON_ID]
t2dm_df <- t2dm_df[!PERSON_ID %in% t2dm_exclusion_id]


# Determining T2DM occurence date: DIAG or check-up, which is faster?
t2dm_df[,T2DM_OCCUR_DATE := pmin(T2DM_DIAG_DATE, T2DM_CHECKUP_DATE, na.rm=T)]
t2dm_df[,T2DM_OCCUR_AGE := pmin(T2DM_DIAG_AGE, T2DM_CHECKUP_AGE, na.rm=T)]



### obsevartion duration ###
# if not t2dm => last checkup date - first checkup date since 52
# if t2dm => t2dm_diag_date - first checkup date since 52
t2dm_df[,OBS_DURATION := ifelse(is.na(T2DM_OCCUR_DATE),
                               as.numeric(difftime('2015-12-31', CHECKUP_DATE_2nd,units='days')),
                               as.numeric(difftime(T2DM_OCCUR_DATE,CHECKUP_DATE_2nd, units='days')))]

t2dm_df[,OBS_DURATION_Y := round(OBS_DURATION/365,1)]

# Variables
t2dm_df[,SEX:= as.factor(ifelse(SEX==2,'F','M'))]
t2dm_df[,SEX_2nd:= as.factor(ifelse(SEX_2nd==2,'F','M'))]
t2dm_df[,T2DM_YN := ifelse(is.na(T2DM_OCCUR_DATE),0,1)]
t2dm_df[,PROTE_URIA_2nd:=ifelse(is.na(OLIG_PROTE_CD_2nd), NA,
                               ifelse(OLIG_PROTE_CD_2nd>= '3','1','0'))]
t2dm_df[,PROTE_URIA:=ifelse(is.na(OLIG_PROTE_CD), NA,
                           ifelse(OLIG_PROTE_CD>= '3','1','0'))]

# ordering factors
t2dm_df$DRINKING_2nd <- factor(t2dm_df$DRINKING_2nd, level=c('none','light','moderate','heavy'))
t2dm_df$SMOKING_2nd <- factor(t2dm_df$SMOKING_2nd, level=c('None','Light','Moderate','Heavy'))
t2dm_df$MET_CAT_2nd <- factor(t2dm_df$MET_CAT_2nd, levels=c('None','Low','Middle','High'))

t2dm_df$DRINKING <- factor(t2dm_df$DRINKING, level=c('none','light','moderate','heavy'))
t2dm_df$SMOKING <- factor(t2dm_df$SMOKING, level=c('None','Light','Moderate','Heavy'))
t2dm_df$MET_CAT <- factor(t2dm_df$MET_CAT, levels=c('None','Low','Middle','High'))


# change var names
setnames(t2dm_df,'BP_HIGH_2nd','SBP_2nd')
setnames(t2dm_df,'BP_LWST_2nd','DBP_2nd')
setnames(t2dm_df,'BP_HIGH','SBP')
setnames(t2dm_df,'BP_LWST','DBP')


t2dm_df[,FMLY_HPRTS_PATIEN_YN:=as.numeric(ifelse(FMLY_HPRTS_PATIEN_YN=='2','1',
                                                ifelse(FMLY_HPRTS_PATIEN_YN=='1','0',
                                                       ifelse(FMLY_HPRTS_PATIEN_YN=="",NA,FMLY_HPRTS_PATIEN_YN))))]

t2dm_df[,FMLY_HPRTS_PATIEN_YN_2nd:=as.numeric(ifelse(FMLY_HPRTS_PATIEN_YN_2nd=='2','1',
                                                    ifelse(FMLY_HPRTS_PATIEN_YN_2nd=='1','0',
                                                           ifelse(FMLY_HPRTS_PATIEN_YN_2nd=="",NA,FMLY_HPRTS_PATIEN_YN_2nd))))]
t2dm_df[,FHx_T2DM:=pmax(FMLY_HPRTS_PATIEN_YN, FMLY_HPRTS_PATIEN_YN_2nd, na.rm=T)]

t2dm_df[,BMI_GROUP := as.factor(ifelse(BMI < 25 & BMI_2nd <25, 'A',
                                      ifelse(BMI < 25 & BMI_2nd >=25, 'B',
                                             ifelse(BMI >= 25 & BMI_2nd < 25,'C',
                                                    ifelse(BMI >= 25 & BMI_2nd >=25, 'D', NA)))))]


t2dm_df[,`:=`(
  SMK = as.factor(ifelse(SMOKING_2nd =='None',0,1)),
  EXC = as.factor(ifelse(MET_CAT_2nd == 'None',0,1))
)]

t2dm_df[,T2E:= ifelse(T2DM_YN==1,as.numeric(difftime(T2DM_OCCUR_DATE, CHECKUP_DATE_2nd , units='days')),NA)]

# 3번째 검진 ------------------------------------------------------------------

# second checkup after 52
second_gj_after_52 <- t2dm_gj_eachone[t2dm_gj_eachone[AGE >= 52, .I[2L],
                                                      by = PERSON_ID]$V1, .(
                                                        PERSON_ID,SEX,AGE,BMI,
                                                        FMLY_HPRTS_PATIEN_YN,CHECKUP_DATE,
                                                        CHECKUP_YEAR,BP_HIGH,BP_LWST,
                                                        BLDS,OLIG_PROTE_CD,
                                                        SMOKING,DRINKING,MET_CAT)]

colnames(second_gj_after_52) <- paste0(colnames(second_gj_after_52),'_3rd')
setnames(second_gj_after_52,'PERSON_ID_3rd','PERSON_ID')


t2dm_df2 <- inner_join(t2dm_df,second_gj_after_52,by='PERSON_ID')
cols <- grep('DATE',colnames(t2dm_df2))
t2dm_df2[,(cols):=lapply(.SD, function(x) as.Date(x,'%Y%m%d')),.SDcols=cols]
t2dm_df2[,OBS_DURATION2 := ifelse(is.na(T2DM_OCCUR_DATE),
                                 as.numeric(difftime('2015-12-31', CHECKUP_DATE_3rd,units='days')),
                                 as.numeric(difftime(T2DM_OCCUR_DATE, CHECKUP_DATE_3rd, units='days')))]
t2dm_df2[,OBS_DURATION2_Y:= OBS_DURATION2/365]
setnames(t2dm_df2,'BP_HIGH_3rd','SBP_3rd')
setnames(t2dm_df2,'BP_LWST_3rd','DBP_3rd')

t2dm_df2$SMOKING_3rd <- factor(t2dm_df2$SMOKING_3rd, levels=c('None','Light','Moderate','Heavy'))
t2dm_df2$DRINKING_3rd <- factor(t2dm_df2$DRINKING_3rd, levels=c('none','light','moderate','heavy'))
t2dm_df2$MET_CAT_3rd <- factor(t2dm_df2$MET_CAT_3rd, levels=c('None','Low','Middle','High'))

t2dm_df2[,`:=`(
  obese_1 =ifelse(BMI>=25,'Obese','Normal'),
  obese_2 = ifelse(BMI_2nd>=25,'Obese','Normal'),
  obese_3 = ifelse(BMI_3rd>=25,'Obese','Normal')
)][,BMI_GROUP2 := ifelse(obese_1=='Normal' & obese_2=='Normal' & obese_3=='Normal','MNMN',
                         ifelse(obese_1 =='Normal'& obese_2 =='Normal' & obese_3 =='Obese','MNBO',
                                ifelse(obese_1 == 'Normal' & obese_2=='Obese' & obese_3 =='Normal','BOBN',
                                       ifelse(obese_1=='Obese' & obese_2=='Normal' & obese_3 =='Obese','BNBO',
                                              ifelse(obese_1=='Obese' & obese_2=='Obese'& obese_3=='Normal','MOBN','etc'))))
)]

t2dm_df2[,BMI_GROUP2:=factor(BMI_GROUP2, levels=c('MNMN','MOBN','BNBO','BOBN','MNBO','etc'))]
t2dm_df2[,FHx_T2DM2 := pmax(FMLY_HPRTS_PATIEN_YN,FMLY_HPRTS_PATIEN_YN_2nd, FMLY_HPRTS_PATIEN_YN_3rd,na.rm=T)]
t2dm_df2[,PROTE_URIA2 := ifelse(is.na(OLIG_PROTE_CD_3rd), NA,
                               ifelse(OLIG_PROTE_CD_3rd>= '3','1','0'))] 


t2dm_df2[,`:=`(
  SMK2 = as.factor(ifelse(SMOKING_3rd =='None',0,1)),
  EXC2 = as.factor(ifelse(MET_CAT_3rd == 'None',0,1))
)]


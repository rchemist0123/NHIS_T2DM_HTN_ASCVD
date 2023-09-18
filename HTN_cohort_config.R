# Flow 설명

# 1. 청구 기록에서 진단 + 약물 처방 데이터의 기록 가져옴. (진단일 확인용)
# 2. 약물기록은 진단 후 365일 이내에 있어야 함.
# 3. 검진 기록과 청구 데이터 합치기
# 4. 질병 최초 판정일 찾기 (검진 or (진단 + 약물))
# 5. 건강 검진 관련 변수 생성(e.g., 흡연, 음주, 운동)
# 6. 전체 데이터에서 필요한 시기의 데이터를 선택(e.g., 40대 마지막 검진, 50대 첫번째 검진)

# 고혈압 진단 & 약물 -------------------------------------------------------------
require(data.table)
require(haven)
require(dplyr)
require(tidyr)
hptn_id <- setDT(read_sas('data_source/hptn_id.sas7bdat'))

hptn_id[,`:=`(
  HTN_DIAG_DATE = as.Date(dates_first_diag,'%Y%m%d'),
  HTN_DRUG_DATE = as.Date(dates_first_drug,'%Y%m%d'),
  dates_first_diag = NULL,
  dates_first_drug = NULL
)]

# 진단, 약물 365일 이내 

hptn_id <- hptn_id[difftime(HTN_DRUG_DATE, HTN_DIAG_DATE, units='days')>=0 &
                     difftime(HTN_DRUG_DATE, HTN_DIAG_DATE, units='days')<365]

# 검진과 진단 기록 합치기 

hptn_cohort <- left_join(checkup, hptn_id, by='PERSON_ID') %>% setDT()
setnames(hptn_cohort, 'STND_Y','CHECKUP_YEAR')
setnames(hptn_cohort, 'HME_DT','CHECKUP_DATE')

# 고혈압 진단 또는 검진 판정일 찾기 -----------------------------------------------------

# age when diagnosed
hptn_diag_age <- hptn_cohort[!is.na(HTN_DIAG_DATE)][substr(HTN_DIAG_DATE,1,4)==
                                                      CHECKUP_YEAR,.(PERSON_ID, HTN_DIAG_AGE = AGE)]
hptn_cohort <- left_join(hptn_cohort,hptn_diag_age, by='PERSON_ID') %>% setDT()

# first HTN date when checkup
htn_checkup <- hptn_cohort[hptn_cohort[BP_HIGH>=140|BP_LWST>=90,.I[1L], by=PERSON_ID]$V1,.(PERSON_ID, HTN_CHECKUP_DATE=CHECKUP_DATE, HTN_CHECKUP_AGE = AGE)]
hptn_cohort <- left_join(hptn_cohort, htn_checkup, by='PERSON_ID') %>% setDT()
names(hptn_cohort)

# having checkup records of 40's 50's 
hptn_4050_id <- hptn_cohort[,.(min_age=min(AGE), max_age=max(AGE)),by='PERSON_ID'][
  as.numeric(substr(min_age,1,1)==4) & as.numeric(substr(max_age,1,1)>=5)
  ,PERSON_ID]

hptn_ages <- dcast(hptn_cohort[PERSON_ID %in% hptn_4050_id], PERSON_ID ~ AGE, value.var='AGE')
hptn_ages[,before_50:= rowSums(.SD), .SDcols=2:11]
hptn_ages[,after_50 := rowSums(.SD), .SDcols=12:21]
hptn_target_id <- hptn_ages[before_50>=1 & after_50>=1,PERSON_ID]

# having records at least once each
hptn_gj_eachone <- hptn_cohort[PERSON_ID %in% hptn_target_id]
hptn_gj_eachone <- hptn_gj_eachone[,.(PERSON_ID, CHECKUP_DATE, CHECKUP_YEAR, HTN_DIAG_DATE, HTN_DIAG_AGE,
                                      HTN_CHECKUP_DATE, HTN_CHECKUP_AGE,
                                      SEX,AGE,BMI,FMLY_HPRTS_PATIEN_YN, 
                                      BP_HIGH,BP_LWST, BLDS,OLIG_PROTE_CD,
                                      WLK30_WEK_FREQ_ID, MOV20_WEK_FREQ_ID,MOV30_WEK_FREQ_ID, #exercise
                                      SMK_STAT_TYPE_RSPS_CD, DSQTY_RSPS_CD,CUR_DSQTY_RSPS_CD, # smoking
                                      DRNK_HABIT_RSPS_CD,TM1_DRKQTY_RSPS_CD,DRNK_HABIT_RSPS_CD #drinking
)]

# 음주, 흡연, 운동 --------------------------------------------------------------

hptn_gj_eachone[,`:=`(
  WLK30_WEK_FREQ_ID = ifelse(WLK30_WEK_FREQ_ID=="",NA,as.numeric(WLK30_WEK_FREQ_ID)-1),
  MOV20_WEK_FREQ_ID = ifelse(MOV20_WEK_FREQ_ID=="",NA,as.numeric(MOV20_WEK_FREQ_ID)-1),
  MOV30_WEK_FREQ_ID = ifelse(MOV30_WEK_FREQ_ID=="",NA,as.numeric(MOV30_WEK_FREQ_ID)-1)
)]

hptn_gj_eachone[,`:=`(
  MET_WALK = (WLK30_WEK_FREQ_ID * 20 * 3.3),
  MET_MED = (MOV20_WEK_FREQ_ID * 30 * 4.0),
  MET_HIGH = (MOV30_WEK_FREQ_ID * 30 * 8.0)
)]
hptn_gj_eachone[,MET_CON := MET_WALK + MET_MED + MET_HIGH]
hptn_gj_eachone[,MET_CAT := as.factor(ifelse(MET_HIGH>=1500 | MET_CON>=3000,'High',
                                             ifelse(MOV30_WEK_FREQ_ID>=4 | MOV20_WEK_FREQ_ID>=6 | MET_CON>=600,'Middle',
                                                    ifelse(MET_CON==0,'None','Low'))))]

hptn_gj_eachone[,SMOKING:= ifelse(SMK_STAT_TYPE_RSPS_CD == 1,'None', 
                                  ifelse(CHECKUP_YEAR <2009 & DSQTY_RSPS_CD==1,'Light',
                                         ifelse(CHECKUP_YEAR < 2009 & DSQTY_RSPS_CD==2,'Moderate',
                                                ifelse(CHECKUP_YEAR < 2009 & DSQTY_RSPS_CD>=3,'Heavy',
                                                       ifelse(CHECKUP_YEAR<2009 & DSQTY_RSPS_CD=="",NA,
                                                              ifelse(CHECKUP_YEAR>=2009 & CUR_DSQTY_RSPS_CD <10,'Light',
                                                                     ifelse(CHECKUP_YEAR>=2009 & CUR_DSQTY_RSPS_CD <20,'Moderate',
                                                                            ifelse(CHECKUP_YEAR>=2009 & CUR_DSQTY_RSPS_CD>=20,'Heavy',NA)))))))
)]

hptn_gj_eachone[,DRINK_HABIT_RSPS_0208 := 
                  ifelse(CHECKUP_YEAR<2009 & DRNK_HABIT_RSPS_CD == 1,0, ifelse(
                    CHECKUP_YEAR<2009 & DRNK_HABIT_RSPS_CD ==2,1, ifelse(
                      CHECKUP_YEAR<2009 & DRNK_HABIT_RSPS_CD==3,2, ifelse(
                        CHECKUP_YEAR<2009 & DRNK_HABIT_RSPS_CD==4,4, ifelse(
                          CHECKUP_YEAR<2009 & DRNK_HABIT_RSPS_CD==5,7, NA)))))]

hptn_gj_eachone[,TM1_DRKQTY_RSPS_0208 := 
                  ifelse(CHECKUP_YEAR<2009 & TM1_DRKQTY_RSPS_CD == 1,3, ifelse(
                    CHECKUP_YEAR<2009 & TM1_DRKQTY_RSPS_CD ==2,7, ifelse(
                      CHECKUP_YEAR<2009 & TM1_DRKQTY_RSPS_CD==3,11, ifelse(
                        CHECKUP_YEAR<2009 & TM1_DRKQTY_RSPS_CD==4,14, NA))))]


hptn_gj_eachone[,DRINK_HABIT_RSPS_0915 := as.numeric(ifelse(CHECKUP_YEAR>=2009, 
                                                            as.numeric(DRNK_HABIT_RSPS_CD)-1,
                                                            NA))]

length(unique(hptn_gj_eachone$PERSON_ID))


hptn_gj_eachone[,DRINKING_CON :=ifelse(CHECKUP_YEAR<2009, (DRINK_HABIT_RSPS_0208 * TM1_DRKQTY_RSPS_0208),
                                       ifelse(CHECKUP_YEAR>=2009, as.numeric(DRINK_HABIT_RSPS_0915) * as.numeric(TM1_DRKQTY_RSPS_CD),NA))]

hptn_gj_eachone[,DRINKING := ifelse(DRINKING_CON==0,'none',
                                    ifelse(DRINKING_CON<15,'light',
                                           ifelse(DRINKING_CON<30,'moderate','heavy')))]


# DF 만들기 ------------------------------------------------------------------

# last checkup before 50
hptn_last_gj_in_40 <- hptn_gj_eachone[hptn_gj_eachone[AGE<50,.I[.N],by=PERSON_ID]$V1,.(PERSON_ID,SEX,AGE,
                                                                                       BMI,FMLY_HPRTS_PATIEN_YN,
                                                                                       CHECKUP_DATE,CHECKUP_YEAR,
                                                                                       BP_HIGH,BP_LWST, BLDS,OLIG_PROTE_CD, 
                                                                                       SMOKING, DRINKING,MET_CAT,
                                                                                       HTN_DIAG_DATE, HTN_DIAG_AGE,
                                                                                       HTN_CHECKUP_AGE, HTN_CHECKUP_DATE)]

# first checkup after 52
hptn_first_gj_after_52 <- hptn_gj_eachone[hptn_gj_eachone[AGE>=52,.I[1L],by=PERSON_ID]$V1,.(PERSON_ID,SEX,AGE,
                                                                                            BMI,FMLY_HPRTS_PATIEN_YN,
                                                                                            CHECKUP_DATE,CHECKUP_YEAR,
                                                                                            BP_HIGH,BP_LWST, BLDS,OLIG_PROTE_CD, 
                                                                                            SMOKING, DRINKING,MET_CAT)]

colnames(hptn_first_gj_after_52) <- paste0(colnames(hptn_first_gj_after_52),'_2nd')
setnames(hptn_first_gj_after_52,'PERSON_ID_2nd','PERSON_ID')

# last gj date for calculating observation duration
hptn_last_gj <- hptn_gj_eachone[hptn_gj_eachone[,.I[.N],by=PERSON_ID]$V1,.(PERSON_ID=PERSON_ID,
                                                                           LAST_CHECKUP_DATE = CHECKUP_DATE)]

htn_df <- inner_join(hptn_first_gj_after_52,hptn_last_gj_in_40,by='PERSON_ID') %>% setDT()
htn_df <- inner_join(htn_df, hptn_last_gj, by='PERSON_ID') %>% setDT()


# transform to date forms
dates_vars <- grep('DATE',names(htn_df), value=T)
htn_df[,(dates_vars):=lapply(.SD, function(x) as.Date(x, '%Y%m%d')), .SDcols=dates_vars]

# Exclude HTN patients before second checkup date
hptn_exclusion_id <- htn_df[HTN_DIAG_AGE < 52 | # diagnosed before 52
                              HTN_CHECKUP_AGE < 52 | #HTN in checkup before 52
                              HTN_DIAG_DATE < CHECKUP_DATE_2nd | # HTN diagnosis before 2nd checkup date
                              HTN_CHECKUP_DATE < CHECKUP_DATE_2nd # HTN checkup before 2nd checkup date
                            ,PERSON_ID]
htn_df <- htn_df[!PERSON_ID %in% hptn_exclusion_id]


# Determining HTN occurence date: DIAG or check-up, which is faster?
htn_df[,HTN_OCCUR_DATE := pmin(HTN_DIAG_DATE, HTN_CHECKUP_DATE, na.rm=T)]
htn_df[,HTN_OCCUR_AGE := pmin(HTN_DIAG_AGE, HTN_CHECKUP_AGE, na.rm=T)]



### obsevartion duration ###
# if not htn => last checkup date - first checkup date since 52
# if htn => htn_diag_date - first checkup date since 52
htn_df[,OBS_DURATION := ifelse(is.na(HTN_OCCUR_DATE),
                               as.numeric(difftime('2015-12-31', CHECKUP_DATE_2nd,units='days')),
                               as.numeric(difftime(HTN_OCCUR_DATE,CHECKUP_DATE_2nd, units='days')))]

htn_df[,OBS_DURATION_Y := round(OBS_DURATION/365,1)]

# Variables
htn_df[,SEX:= as.factor(ifelse(SEX==2,'F','M'))]
htn_df[,SEX_2nd:= as.factor(ifelse(SEX_2nd==2,'F','M'))]
htn_df[,HTN_YN := ifelse(is.na(HTN_OCCUR_DATE),0,1)]
htn_df[,PROTE_URIA_2nd:=ifelse(is.na(OLIG_PROTE_CD_2nd), NA,
                           ifelse(OLIG_PROTE_CD_2nd>= '3','1','0'))]
htn_df[,PROTE_URIA:=ifelse(is.na(OLIG_PROTE_CD), NA,
                           ifelse(OLIG_PROTE_CD>= '3','1','0'))]

# ordering factors
htn_df$DRINKING_2nd <- factor(htn_df$DRINKING_2nd, level=c('none','light','moderate','heavy'))
htn_df$SMOKING_2nd <- factor(htn_df$SMOKING_2nd, level=c('None','Light','Moderate','Heavy'))
htn_df$MET_CAT_2nd <- factor(htn_df$MET_CAT_2nd, levels=c('None','Low','Middle','High'))

htn_df$DRINKING <- factor(htn_df$DRINKING, level=c('none','light','moderate','heavy'))
htn_df$SMOKING <- factor(htn_df$SMOKING, level=c('None','Light','Moderate','Heavy'))
htn_df$MET_CAT <- factor(htn_df$MET_CAT, levels=c('None','Low','Middle','High'))


# change var names
setnames(htn_df,'BP_HIGH_2nd','SBP_2nd')
setnames(htn_df,'BP_LWST_2nd','DBP_2nd')
setnames(htn_df,'BP_HIGH','SBP')
setnames(htn_df,'BP_LWST','DBP')


htn_df[,FMLY_HPRTS_PATIEN_YN:=as.numeric(ifelse(FMLY_HPRTS_PATIEN_YN=='2','1',
                                               ifelse(FMLY_HPRTS_PATIEN_YN=='1','0',
                                                      ifelse(FMLY_HPRTS_PATIEN_YN=="",NA,FMLY_HPRTS_PATIEN_YN))))]

htn_df[,FMLY_HPRTS_PATIEN_YN_2nd:=as.numeric(ifelse(FMLY_HPRTS_PATIEN_YN_2nd=='2','1',
                                               ifelse(FMLY_HPRTS_PATIEN_YN_2nd=='1','0',
                                                      ifelse(FMLY_HPRTS_PATIEN_YN_2nd=="",NA,FMLY_HPRTS_PATIEN_YN_2nd))))]
htn_df[,FHx_HTN:=pmax(FMLY_HPRTS_PATIEN_YN, FMLY_HPRTS_PATIEN_YN_2nd, na.rm=T)]

htn_df[,BMI_GROUP := as.factor(ifelse(BMI < 25 & BMI_2nd <25, 'A',
                                      ifelse(BMI < 25 & BMI_2nd >=25, 'B',
                                             ifelse(BMI >= 25 & BMI_2nd < 25,'C',
                                                    ifelse(BMI >= 25 & BMI_2nd >=25, 'D', NA)))))]


htn_df[,`:=`(
  SMK = as.factor(ifelse(SMOKING_2nd =='None',0,1)),
  EXC = as.factor(ifelse(MET_CAT_2nd == 'None',0,1))
)]

htn_df[,T2E:= ifelse(HTN_YN==1,as.numeric(difftime(HTN_OCCUR_DATE, CHECKUP_DATE_2nd , units='days')),NA)]

# 3번째 검진 ------------------------------------------------------------------

# second checkup after 52
second_gj_after_52 <- hptn_gj_eachone[hptn_gj_eachone[AGE >= 52, .I[2L],
                                                      by = PERSON_ID]$V1, .(
                                                        PERSON_ID,SEX,AGE,BMI,
                                                        FMLY_HPRTS_PATIEN_YN,CHECKUP_DATE,
                                                        CHECKUP_YEAR,BP_HIGH,BP_LWST,
                                                        BLDS,OLIG_PROTE_CD,
                                                        SMOKING,DRINKING,MET_CAT)]

colnames(second_gj_after_52) <- paste0(colnames(second_gj_after_52),'_3rd')
setnames(second_gj_after_52,'PERSON_ID_3rd','PERSON_ID')


htn_df2 <- inner_join(htn_df,second_gj_after_52,by='PERSON_ID')
cols <- grep('DATE',colnames(htn_df2))
htn_df2[,(cols):=lapply(.SD, function(x) as.Date(x,'%Y%m%d')),.SDcols=cols]
htn_df2[,OBS_DURATION2 := ifelse(is.na(HTN_OCCUR_DATE),
                                 as.numeric(difftime('2015-12-31', CHECKUP_DATE_3rd,units='days')),
                                 as.numeric(difftime(HTN_OCCUR_DATE, CHECKUP_DATE_3rd, units='days')))]
htn_df2[,OBS_DURATION2_Y:= OBS_DURATION2/365]
setnames(htn_df2,'BP_HIGH_3rd','SBP_3rd')
setnames(htn_df2,'BP_LWST_3rd','DBP_3rd')

htn_df2$SMOKING_3rd <- factor(htn_df2$SMOKING_3rd, levels=c('None','Light','Moderate','Heavy'))
htn_df2$DRINKING_3rd <- factor(htn_df2$DRINKING_3rd, levels=c('none','light','moderate','heavy'))
htn_df2$MET_CAT_3rd <- factor(htn_df2$MET_CAT_3rd, levels=c('None','Low','Middle','High'))

htn_df2[,`:=`(
  obese_1 =ifelse(BMI>=25,'Obese','Normal'),
  obese_2 = ifelse(BMI_2nd>=25,'Obese','Normal'),
  obese_3 = ifelse(BMI_3rd>=25,'Obese','Normal')
)][,BMI_GROUP2 := ifelse(obese_1=='Normal' & obese_2=='Normal' & obese_3=='Normal','MNMN',
                         ifelse(obese_1 =='Normal'& obese_2 =='Normal' & obese_3 =='Obese','MNBO',
                                ifelse(obese_1 == 'Normal' & obese_2=='Obese' & obese_3 =='Normal','BOBN',
                                       ifelse(obese_1=='Obese' & obese_2=='Normal' & obese_3 =='Obese','BNBO',
                                              ifelse(obese_1=='Obese' & obese_2=='Obese'& obese_3=='Normal','MOBN','etc'))))
)]

htn_df2[,BMI_GROUP2:=factor(BMI_GROUP2, levels=c('MNMN','MOBN','BNBO','BOBN','MNBO','etc'))]
htn_df2[,FHx_HTN2 := pmax(FMLY_HPRTS_PATIEN_YN,FMLY_HPRTS_PATIEN_YN_2nd, FMLY_HPRTS_PATIEN_YN_3rd,na.rm=T)]
htn_df2[,PROTE_URIA2 := ifelse(is.na(OLIG_PROTE_CD_3rd), NA,
                               ifelse(OLIG_PROTE_CD_3rd>= '3','1','0'))] 


htn_df2[,`:=`(
  SMK2 = as.factor(ifelse(SMOKING_3rd =='None',0,1)),
  EXC2 = as.factor(ifelse(MET_CAT_3rd == 'None',0,1))
)]


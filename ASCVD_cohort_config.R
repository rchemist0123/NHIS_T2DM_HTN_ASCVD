
# 1. 52세 전후 ---------------------------------------------------------------

# ASCVD 진단  -------------------------------------------------------------
ascvd_id <- setDT(read_sas('data_source/ascvd_diags.sas7bdat'))
ascvd_id[,`:=`(
  ASCVD_DIAG_DATE = as.Date(dates_first_diag,'%Y%m%d'),
  dates_first_diag = NULL
)]

# 검진과 진단 기록 합치기 

ascvd_cohort <- left_join(checkup, ascvd_id, by='PERSON_ID') %>% as.data.table()
setnames(ascvd_cohort, 'STND_Y','CHECKUP_YEAR')
setnames(ascvd_cohort, 'HME_DT','CHECKUP_DATE')

ascvd_cohort[,INCOME_4 := as.factor(ntile(INCOME,4))]

ascvd_cohort %>% head()
# ASCVD 진단일 찾기 -----------------------------------------------------

# age when diagnosed
ascvd_diag_age <- ascvd_cohort[!is.na(ASCVD_DIAG_DATE)][substr(ASCVD_DIAG_DATE,1,4)==CHECKUP_YEAR,.(PERSON_ID, ASCVD_DIAG_AGE = AGE)]
ascvd_cohort <- left_join(ascvd_cohort,ascvd_diag_age, by='PERSON_ID') %>% as.data.table()

# having checkup records of 40's 50's 
ascvd_4050_id <- ascvd_cohort[,.(min_age=min(AGE), max_age=max(AGE)),by='PERSON_ID'][
  as.numeric(substr(min_age,1,1)==4) & as.numeric(substr(max_age,1,1)>=5)
  ,PERSON_ID]

ascvd_ages <- dcast(ascvd_cohort[PERSON_ID %in% ascvd_4050_id], PERSON_ID ~ AGE, value.var='AGE')
ascvd_ages[,before_50:= rowSums(.SD), .SDcols=2:11]
ascvd_ages[,after_50 := rowSums(.SD), .SDcols=12:21]
ascvd_target_id <- ascvd_ages[before_50>=1 & after_50>=1,PERSON_ID]

# having records at least once each
ascvd_gj_eachone <- ascvd_cohort[PERSON_ID %in% ascvd_target_id]
ascvd_gj_eachone <- ascvd_gj_eachone[,.(PERSON_ID, CHECKUP_DATE, CHECKUP_YEAR, ASCVD_DIAG_DATE, ASCVD_DIAG_AGE,
                                      SEX,AGE,BMI,FMLY_HDISE_PATIEN_YN, TOT_CHOLE, INCOME_4,
                                      BP_HIGH,BP_LWST, BLDS,OLIG_PROTE_CD,
                                      WLK30_WEK_FREQ_ID, MOV20_WEK_FREQ_ID,MOV30_WEK_FREQ_ID, #exercise
                                      SMK_STAT_TYPE_RSPS_CD, DSQTY_RSPS_CD,CUR_DSQTY_RSPS_CD, # smoking
                                      DRNK_HABIT_RSPS_CD,TM1_DRKQTY_RSPS_CD,DRNK_HABIT_RSPS_CD #drinking
)]

# 음주, 흡연, 운동 --------------------------------------------------------------

ascvd_gj_eachone[,`:=`(
  WLK30_WEK_FREQ_ID = ifelse(WLK30_WEK_FREQ_ID=="",NA,as.numeric(WLK30_WEK_FREQ_ID)-1),
  MOV20_WEK_FREQ_ID = ifelse(MOV20_WEK_FREQ_ID=="",NA,as.numeric(MOV20_WEK_FREQ_ID)-1),
  MOV30_WEK_FREQ_ID = ifelse(MOV30_WEK_FREQ_ID=="",NA,as.numeric(MOV30_WEK_FREQ_ID)-1)
)]

ascvd_gj_eachone[,`:=`(
  MET_WALK = (WLK30_WEK_FREQ_ID * 20 * 3.3),
  MET_MED = (MOV20_WEK_FREQ_ID * 30 * 4.0),
  MET_HIGH = (MOV30_WEK_FREQ_ID * 30 * 8.0)
)]
ascvd_gj_eachone[,MET_CON := MET_WALK + MET_MED + MET_HIGH]
ascvd_gj_eachone[,MET_CAT := as.factor(ifelse(MET_HIGH>=1500 | MET_CON>=3000,'High',
                                             ifelse(MOV30_WEK_FREQ_ID>=4 | MOV20_WEK_FREQ_ID>=6 | MET_CON>=600,'Middle',
                                                    ifelse(MET_CON==0,'None','Low'))))]

ascvd_gj_eachone[,SMOKING:= ifelse(SMK_STAT_TYPE_RSPS_CD == 1,'None', 
                                  ifelse(CHECKUP_YEAR <2009 & DSQTY_RSPS_CD==1,'Light',
                                         ifelse(CHECKUP_YEAR < 2009 & DSQTY_RSPS_CD==2,'Moderate',
                                                ifelse(CHECKUP_YEAR < 2009 & DSQTY_RSPS_CD>=3,'Heavy',
                                                       ifelse(CHECKUP_YEAR<2009 & DSQTY_RSPS_CD=="",NA,
                                                              ifelse(CHECKUP_YEAR>=2009 & CUR_DSQTY_RSPS_CD <10,'Light',
                                                                     ifelse(CHECKUP_YEAR>=2009 & CUR_DSQTY_RSPS_CD <20,'Moderate',
                                                                            ifelse(CHECKUP_YEAR>=2009 & CUR_DSQTY_RSPS_CD>=20,'Heavy',NA)))))))
)]

ascvd_gj_eachone[,DRINK_HABIT_RSPS_0208 := 
                  ifelse(CHECKUP_YEAR<2009 & DRNK_HABIT_RSPS_CD == 1,0, ifelse(
                    CHECKUP_YEAR<2009 & DRNK_HABIT_RSPS_CD ==2,1, ifelse(
                      CHECKUP_YEAR<2009 & DRNK_HABIT_RSPS_CD==3,2, ifelse(
                        CHECKUP_YEAR<2009 & DRNK_HABIT_RSPS_CD==4,4, ifelse(
                          CHECKUP_YEAR<2009 & DRNK_HABIT_RSPS_CD==5,7, NA)))))]

ascvd_gj_eachone[,TM1_DRKQTY_RSPS_0208 := 
                  ifelse(CHECKUP_YEAR<2009 & TM1_DRKQTY_RSPS_CD == 1,3, ifelse(
                    CHECKUP_YEAR<2009 & TM1_DRKQTY_RSPS_CD ==2,7, ifelse(
                      CHECKUP_YEAR<2009 & TM1_DRKQTY_RSPS_CD==3,11, ifelse(
                        CHECKUP_YEAR<2009 & TM1_DRKQTY_RSPS_CD==4,14, NA))))]


ascvd_gj_eachone[,DRINK_HABIT_RSPS_0915 := as.numeric(ifelse(CHECKUP_YEAR>=2009, 
                                                            as.numeric(DRNK_HABIT_RSPS_CD)-1,
                                                            NA))]

length(unique(ascvd_gj_eachone$PERSON_ID))


ascvd_gj_eachone[,DRINKING_CON :=ifelse(CHECKUP_YEAR<2009, (DRINK_HABIT_RSPS_0208 * TM1_DRKQTY_RSPS_0208),
                                       ifelse(CHECKUP_YEAR>=2009, as.numeric(DRINK_HABIT_RSPS_0915) * as.numeric(TM1_DRKQTY_RSPS_CD),NA))]

ascvd_gj_eachone[,DRINKING := ifelse(DRINKING_CON==0,'none',
                                    ifelse(DRINKING_CON<15,'light',
                                           ifelse(DRINKING_CON<30,'moderate','heavy')))]

# DF 만들기 ------------------------------------------------------------------

# last checkup before 50
ascvd_last_gj_in_40 <- ascvd_gj_eachone[ascvd_gj_eachone[AGE<50,.I[.N],by=PERSON_ID]$V1,.(PERSON_ID, SEX, BMI,AGE, INCOME_4,
                                                                                          FMLY_HDISE_PATIEN_YN,
                                                                                          TOT_CHOLE,
                                                                                          BP_HIGH,BP_LWST, BLDS,OLIG_PROTE_CD, 
                                                                                          SMOKING, DRINKING,MET_CAT,
                                                                                  CHECKUP_DATE,CHECKUP_YEAR,
                                                                                  ASCVD_DIAG_DATE, ASCVD_DIAG_AGE)]

# first checkup after 52
ascvd_first_gj_after_52 <- ascvd_gj_eachone[ascvd_gj_eachone[AGE>=52,.I[1L],by=PERSON_ID]$V1,.(PERSON_ID,SEX,AGE, INCOME_4,
                                                                                       BMI,FMLY_HDISE_PATIEN_YN,
                                                                                       CHECKUP_DATE,CHECKUP_YEAR, TOT_CHOLE,
                                                                                       BP_HIGH,BP_LWST, BLDS,OLIG_PROTE_CD, 
                                                                                       SMOKING, DRINKING,MET_CAT)]

colnames(ascvd_first_gj_after_52) <- paste0(colnames(ascvd_first_gj_after_52),'_2nd')
setnames(ascvd_first_gj_after_52,'PERSON_ID_2nd','PERSON_ID')

# last gj date for calculating observation duration
ascvd_last_gj <- ascvd_gj_eachone[ascvd_gj_eachone[,.I[.N],by=PERSON_ID]$V1,.(PERSON_ID=PERSON_ID,
                                                                      LAST_CHECKUP_DATE = CHECKUP_DATE)]


ascvd_df <- inner_join(ascvd_first_gj_after_52,ascvd_last_gj_in_40,by='PERSON_ID') %>% setDT()
ascvd_df <- inner_join(ascvd_df, ascvd_last_gj, by='PERSON_ID') %>% setDT()


# transform to date forms
dates_vars <- grep('DATE',names(ascvd_df), value=T)
ascvd_df[,(dates_vars):=lapply(.SD, function(x) as.Date(x, '%Y%m%d')), .SDcols=dates_vars]

# Exclude ASCVD patients before second checkup date
ascvd_exclusion_id <- ascvd_df[ASCVD_DIAG_AGE < 52 | # diagnosed before 52
                         ASCVD_DIAG_DATE < CHECKUP_DATE_2nd # ASCVD diagnosis before 2nd checkup date
                       ,PERSON_ID]
ascvd_df <- ascvd_df[!PERSON_ID %in% ascvd_exclusion_id]

# Determining ASCVD occurence date: DIAG or check-up, which is faster?
ascvd_df[,ASCVD_OCCUR_DATE := pmin(ASCVD_DIAG_DATE, na.rm=T)]
ascvd_df[,ASCVD_OCCUR_AGE := pmin(ASCVD_DIAG_AGE, na.rm=T)]



### obsevartion duration ###
# if not ascvd => last checkup date - first checkup date since 52
# if ascvd => ascvd_diag_date - first checkup date since 52
ascvd_df[,OBS_DURATION := ifelse(is.na(ASCVD_OCCUR_DATE),
                               as.numeric(difftime('2015-12-31', CHECKUP_DATE_2nd,units='days')),
                               as.numeric(difftime(ASCVD_OCCUR_DATE,CHECKUP_DATE_2nd, units='days')))]

ascvd_df[,OBS_DURATION_Y := round(OBS_DURATION/365,1)]
ascvd_df <- ascvd_df[!OBS_DURATION<0]

# Variables
ascvd_df[,SEX:= as.factor(ifelse(SEX=='2','F','M'))]
ascvd_df[,SEX_2nd:= as.factor(ifelse(SEX_2nd=='2','F','M'))]
ascvd_df[,ASCVD_YN := ifelse(is.na(ASCVD_OCCUR_DATE),0,1)]
ascvd_df[,PROTE_URIA1:=ifelse(is.na(OLIG_PROTE_CD), NA,
                           ifelse(OLIG_PROTE_CD>= '3','1','0'))]
ascvd_df[,PROTE_URIA:=ifelse(is.na(OLIG_PROTE_CD_2nd), NA,
                           ifelse(OLIG_PROTE_CD_2nd>= '3','1','0'))]

# ordering factors
ascvd_df$DRINKING <- factor(ascvd_df$DRINKING, level=c('none','light','moderate','heavy'))
ascvd_df$SMOKING <- factor(ascvd_df$SMOKING, level=c('None','Light','Moderate','Heavy'))
ascvd_df$MET_CAT <- factor(ascvd_df$MET_CAT, levels=c('None','Low','Middle','High'))

ascvd_df$DRINKING_2nd <- factor(ascvd_df$DRINKING_2nd, level=c('none','light','moderate','heavy'))
ascvd_df$SMOKING_2nd <- factor(ascvd_df$SMOKING_2nd, level=c('None','Light','Moderate','Heavy'))
ascvd_df$MET_CAT_2nd <- factor(ascvd_df$MET_CAT_2nd, levels=c('None','Low','Middle','High'))

# change var names
setnames(ascvd_df,'BP_HIGH','SBP')
setnames(ascvd_df,'BP_LWST','DBP')

setnames(ascvd_df,'BP_HIGH_2nd','SBP_2nd')
setnames(ascvd_df,'BP_LWST_2nd','DBP_2nd')

ascvd_df[,FMLY_HDISE_PATIEN_YN:=(ifelse(FMLY_HDISE_PATIEN_YN=='2','1',
                                               ifelse(FMLY_HDISE_PATIEN_YN=='1','0',
                                                      ifelse(FMLY_HDISE_PATIEN_YN=="",NA,FMLY_HDISE_PATIEN_YN))))]
ascvd_df[,FMLY_HDISE_PATIEN_YN_2nd:=(ifelse(FMLY_HDISE_PATIEN_YN_2nd=='2','1',
                                               ifelse(FMLY_HDISE_PATIEN_YN_2nd=='1','0',
                                                      ifelse(FMLY_HDISE_PATIEN_YN_2nd=="",NA,FMLY_HDISE_PATIEN_YN_2nd))))]
ascvd_df[,FHx_ASCVD:=pmax(FMLY_HDISE_PATIEN_YN, FMLY_HDISE_PATIEN_YN_2nd, na.rm=T)]

ascvd_df[,BMI_GROUP := as.factor(ifelse(BMI < 25 & BMI_2nd <25, 'A',
                                      ifelse(BMI < 25 & BMI_2nd >=25, 'B',
                                             ifelse(BMI >= 25 & BMI_2nd < 25,'C',
                                                    ifelse(BMI >= 25 & BMI_2nd >=25, 'D', NA)))))]

ascvd_df[,`:=`(
  SMK = as.factor(ifelse(SMOKING_2nd =='None',0,1)),
  EXC = as.factor(ifelse(MET_CAT_2nd == 'None',0,1))
)]


ascvd_df[,T2E:= ifelse(ASCVD_YN==1,as.numeric(difftime(ASCVD_OCCUR_DATE, CHECKUP_DATE_2nd , units='days')),NA)]
ascvd_df[,BLDS_CAT:= as.factor(ifelse(BLDS_2nd<100,0, ifelse(BLDS_2nd<126,1,2)))]
ascvd_df[,CHOLE_CAT:= as.factor(ifelse(TOT_CHOLE_2nd<200,0, ifelse(TOT_CHOLE_2nd<240,1,2)))]

ascvd_df[,SBP_CAT := as.factor(ifelse(SBP_2nd<120,0,ifelse(SBP_2nd<140,1,2)))]
ascvd_df[,DBP_CAT := as.factor(ifelse(DBP_2nd<80,0,ifelse(DBP_2nd<90,1,2)))]

ascvd_df[,BLDS_CAT1:= as.factor(ifelse(BLDS<100,0, ifelse(BLDS<126,1,2)))]
ascvd_df[,CHOLE_CAT1:= as.factor(ifelse(TOT_CHOLE<200,0, ifelse(TOT_CHOLE<240,1,2)))]

ascvd_df[,SBP_CAT1 := as.factor(ifelse(SBP<120,0,ifelse(SBP<140,1,2)))]
ascvd_df[,DBP_CAT1 := as.factor(ifelse(DBP<80,0,ifelse(DBP<90,1,2)))]

# CCI ---------------------------------------------------------------------

cci_list <- setDT(read_sas('data_source/cci_list.sas7bdat'))
cci_list
ascvd_df <- inner_join(ascvd_df, cci_list, by=c("PERSON_ID" = "PERSON_ID", 'CHECKUP_YEAR_2nd'='year')) %>% setDT()
ascvd_df <- left_join(ascvd_df, cci_list[,.(PERSON_ID,year,org_CCI2=org_CCI, weight_cci2=weight_cci)], by=c("PERSON_ID" = "PERSON_ID", 'CHECKUP_YEAR'='year')) %>% as.data.table()

names(ascvd_df)
ascvd_df[,`:=`(
  # CCI_ORG_CAT1 = as.factor(ifelse(org_CCI2==0,0, ifelse(org_CCI2==1,1,2))),
  # CCI_WT_CAT1 = as.factor(ifelse(weight_cci2==0,0,ifelse(weight_cci2==1,1,2))),
  CCI_ORG_CAT = as.factor(ifelse(org_CCI==0,0, ifelse(org_CCI==1,1,2))),
  CCI_WT_CAT = as.factor(ifelse(weight_cci==0,0,ifelse(weight_cci==1,1,2)))
)]
ascvd_df[,`:=`(
  CCI_ORG_CAT1 = as.factor(ifelse(org_CCI2==0,0, ifelse(org_CCI2==1,1,2))),
  CCI_WT_CAT1 = as.factor(ifelse(weight_cci2==0,0,ifelse(weight_cci2==1,1,2))))]
  



# new BMI group -----------------------------------------------------------

ascvd_df[,BMI_GROUP_NEW := as.factor(ifelse(BMI<23.0,0, ifelse(BMI<25.0,1,2)))]
ascvd_df[,BMI_GROUP_NEW_2nd := as.factor(ifelse(BMI_2nd<23.0,0, ifelse(BMI_2nd<25.0,1,2)))]
ascvd_df[,BMI_GROUP_NEW_CAT:= as.factor(ifelse(BMI_GROUP_NEW==0 & BMI_GROUP_NEW_2nd==0,'A0',
                                               ifelse(BMI_GROUP_NEW==0 & BMI_GROUP_NEW_2nd==1,'A1',
                                                      ifelse(BMI_GROUP_NEW==0 & BMI_GROUP_NEW_2nd==2,'A2',
                                                             ifelse(BMI_GROUP_NEW==1 & BMI_GROUP_NEW_2nd==0,'B0',
                                                                    ifelse(BMI_GROUP_NEW==1 & BMI_GROUP_NEW_2nd==1,'B1',
                                                                           ifelse(BMI_GROUP_NEW==1 & BMI_GROUP_NEW_2nd==2,'B2',
                                                                                  ifelse(BMI_GROUP_NEW==2 & BMI_GROUP_NEW_2nd==0,'C0',
                                                                                         ifelse(BMI_GROUP_NEW==2 & BMI_GROUP_NEW_2nd==1,'C1','C2')))))))))]


# exclusion: 당뇨나 고혈압 환자 제외 ------------------------------------------------
ascvd_df2 <- ascvd_df[!PERSON_ID %in% t2dm_df[T2DM_YN==1]$PERSON_ID &
           !PERSON_ID %in% htn_df[HTN_YN==1]$PERSON_ID]


# 3번째 검진 ------------------------------------------------------------------

# second checkup after 52
second_gj_after_52 <- ascvd_gj_eachone[ascvd_gj_eachone[AGE >= 52, .I[2L],
                                                      by = PERSON_ID]$V1, .(
                                                        PERSON_ID,SEX,AGE,BMI, INCOME_4,
                                                        CHECKUP_DATE, TOT_CHOLE,
                                                        CHECKUP_YEAR,BP_HIGH,BP_LWST,
                                                        BLDS,OLIG_PROTE_CD,
                                                        SMOKING)]

colnames(second_gj_after_52) <- paste0(colnames(second_gj_after_52),'_3rd')
setnames(second_gj_after_52,'PERSON_ID_3rd','PERSON_ID')


ascvd_df2 <- inner_join(ascvd_df,second_gj_after_52,by='PERSON_ID')
cols <- grep('DATE',colnames(ascvd_df2))
ascvd_df2[,(cols):=lapply(.SD, function(x) as.Date(x,'%Y%m%d')),.SDcols=cols]
ascvd_df2[,OBS_DURATION2 := ifelse(is.na(ASCVD_OCCUR_DATE),
                                 as.numeric(difftime(LAST_CHECKUP_DATE, CHECKUP_DATE_3rd,units='days')),
                                 as.numeric(difftime(ASCVD_OCCUR_DATE, CHECKUP_DATE_3rd, units='days')))]
ascvd_df2[,OBS_DURATION2_Y:= OBS_DURATION2/365]
setnames(ascvd_df2,'BP_HIGH_3rd','SBP_3rd')
setnames(ascvd_df2,'BP_LWST_3rd','DBP_3rd')

ascvd_df2$SMOKING_3rd <- factor(ascvd_df2$SMOKING_3rd, levels=c('None','Light','Moderate','Heavy'))
ascvd_df2$DRINKING_3rd <- factor(ascvd_df2$DRINKING_3rd, levels=c('none','light','moderate','heavy'))
ascvd_df2$MET_CAT_3rd <- factor(ascvd_df2$MET_CAT_3rd, levels=c('None','Low','Middle','High'))

ascvd_df2[,`:=`(
  obese_1 =ifelse(BMI>=25,'Obese','Normal'),
  obese_2 = ifelse(BMI_2nd>=25,'Obese','Normal'),
  obese_3 = ifelse(BMI_3rd>=25,'Obese','Normal')
)][,BMI_GROUP2 := ifelse(obese_1=='Normal' & obese_2=='Normal' & obese_3=='Normal','MNMN',
                         ifelse(obese_1 =='Normal'& obese_2 =='Normal' & obese_3 =='Obese','MNBO',
                                ifelse(obese_1 == 'Normal' & obese_2=='Obese' & obese_3 =='Normal','BOBN',
                                       ifelse(obese_1=='Obese' & obese_2=='Normal' & obese_3 =='Obese','BNBO',
                                              ifelse(obese_1=='Obese' & obese_2=='Obese'& obese_3=='Normal','MOBN','etc'))))
)]

ascvd_df2[,BMI_GROUP2:=factor(BMI_GROUP2, levels=c('MNMN','MOBN','BNBO','BOBN','MNBO','etc'))]
ascvd_df2[,PROTE_URIA2 := ifelse(is.na(OLIG_PROTE_CD_3rd), NA,
                               ifelse(OLIG_PROTE_CD_3rd>= '3','1','0'))] 


ascvd_df2[,`:=`(
  SMK2 = as.factor(ifelse(SMOKING_3rd =='None',0,1)),
  EXC2 = as.factor(ifelse(MET_CAT_3rd == 'None',0,1))
)]

ascvd_df2[,T2E2:= ifelse(ASCVD_YN==1,as.numeric(difftime(ASCVD_OCCUR_DATE, CHECKUP_DATE_3rd , units='days')),NA)]
ascvd_df2[,BLDS_CAT2:= as.factor(ifelse(BLDS_3rd<100,0, ifelse(BLDS_3rd<126,1,2)))]
ascvd_df2[,CHOLE_CAT2:= as.factor(ifelse(TOT_CHOLE_3rd<200,0, ifelse(TOT_CHOLE_3rd<240,1,2)))]
ascvd_df2
ascvd_df2[,SBP_CAT2 := as.factor(ifelse(SBP_3rd<120,0,ifelse(SBP_3rd<140,1,2)))]
ascvd_df2[,DBP_CAT2 := as.factor(ifelse(DBP_3rd<80,0,ifelse(DBP_3rd<90,1,2)))]
ascvd_df2[,BMI_GROUP_NEW_3rd := as.factor(ifelse(BMI_3rd<23.0,0, ifelse(BMI_3rd<25.0,1,2)))]


ascvd_df2 <- inner_join(ascvd_df2, cci_list, by=c("PERSON_ID" = "PERSON_ID", 'CHECKUP_YEAR_3rd'='year'))


ascvd_df2[,`:=`(
  CCI_ORG_CAT2 = as.factor(ifelse(org_CCI.y==0,0, ifelse(org_CCI.y==1,1,2))),
  CCI_WT_CAT2 = as.factor(ifelse(weight_cci.y==0,0,ifelse(weight_cci.y==1,1,2)))
)]

# groups
ascvd_df2[,BMI_GROUP_NEW_CAT2 := as.factor(
           ifelse(BMI_GROUP_NEW==0 & BMI_GROUP_NEW_2nd ==0 & BMI_GROUP_NEW_3rd == 0,'MNMN_1',
           ifelse(BMI_GROUP_NEW==0 & BMI_GROUP_NEW_2nd ==0 & BMI_GROUP_NEW_3rd == 1,'MNBW_1',
           ifelse(BMI_GROUP_NEW==0 & BMI_GROUP_NEW_2nd ==0 & BMI_GROUP_NEW_3rd == 2,'MNBO_1',
           ifelse(BMI_GROUP_NEW==0 & BMI_GROUP_NEW_2nd ==1 & BMI_GROUP_NEW_3rd == 0,'BWBN_1',
           ifelse(BMI_GROUP_NEW==0 & BMI_GROUP_NEW_2nd ==1 & BMI_GROUP_NEW_3rd == 1,'BWMW_1',
           ifelse(BMI_GROUP_NEW==0 & BMI_GROUP_NEW_2nd ==1 & BMI_GROUP_NEW_3rd == 2,'BWBO_1',
           ifelse(BMI_GROUP_NEW==0 & BMI_GROUP_NEW_2nd ==2 & BMI_GROUP_NEW_3rd == 0,'BOBN_1',
           ifelse(BMI_GROUP_NEW==0 & BMI_GROUP_NEW_2nd ==2 & BMI_GROUP_NEW_3rd == 1,'BOBW_1',
           ifelse(BMI_GROUP_NEW==0 & BMI_GROUP_NEW_2nd ==2 & BMI_GROUP_NEW_3rd == 2,'BOMO_1',
           
           ifelse(BMI_GROUP_NEW==1 & BMI_GROUP_NEW_2nd ==0 & BMI_GROUP_NEW_3rd == 0,'BNMN_2',
           ifelse(BMI_GROUP_NEW==1 & BMI_GROUP_NEW_2nd ==0 & BMI_GROUP_NEW_3rd == 1,'BNBW_2',
           ifelse(BMI_GROUP_NEW==1 & BMI_GROUP_NEW_2nd ==0 & BMI_GROUP_NEW_3rd == 2,'BNBO_2',
           ifelse(BMI_GROUP_NEW==1 & BMI_GROUP_NEW_2nd ==1 & BMI_GROUP_NEW_3rd == 0,'MWBN_2',
           ifelse(BMI_GROUP_NEW==1 & BMI_GROUP_NEW_2nd ==1 & BMI_GROUP_NEW_3rd == 1,'MWMW_2',
           ifelse(BMI_GROUP_NEW==1 & BMI_GROUP_NEW_2nd ==1 & BMI_GROUP_NEW_3rd == 2,'MWBO_2',
           ifelse(BMI_GROUP_NEW==1 & BMI_GROUP_NEW_2nd ==2 & BMI_GROUP_NEW_3rd == 0,'BOBN_2',
           ifelse(BMI_GROUP_NEW==1 & BMI_GROUP_NEW_2nd ==2 & BMI_GROUP_NEW_3rd == 1,'BOBW_2',
           ifelse(BMI_GROUP_NEW==1 & BMI_GROUP_NEW_2nd ==2 & BMI_GROUP_NEW_3rd == 2,'BOMO_2',
                  
           ifelse(BMI_GROUP_NEW==2 & BMI_GROUP_NEW_2nd ==0 & BMI_GROUP_NEW_3rd == 0,'BNMN_3',
           ifelse(BMI_GROUP_NEW==2 & BMI_GROUP_NEW_2nd ==0 & BMI_GROUP_NEW_3rd == 1,'BNBW_3',
           ifelse(BMI_GROUP_NEW==2 & BMI_GROUP_NEW_2nd ==0 & BMI_GROUP_NEW_3rd == 2,'BNBO_3',
           ifelse(BMI_GROUP_NEW==2 & BMI_GROUP_NEW_2nd ==1 & BMI_GROUP_NEW_3rd == 0,'BWBN_3',
           ifelse(BMI_GROUP_NEW==2 & BMI_GROUP_NEW_2nd ==1 & BMI_GROUP_NEW_3rd == 1,'BWMW_3',
           ifelse(BMI_GROUP_NEW==2 & BMI_GROUP_NEW_2nd ==1 & BMI_GROUP_NEW_3rd == 2,'BWBO_3',
           ifelse(BMI_GROUP_NEW==2 & BMI_GROUP_NEW_2nd ==2 & BMI_GROUP_NEW_3rd == 0,'MOBN_3',
           ifelse(BMI_GROUP_NEW==2 & BMI_GROUP_NEW_2nd ==2 & BMI_GROUP_NEW_3rd == 1,'MOBW_3','MOMO_3')))))))))))))))))))))))))))]



# 60세 전후 ------------------------------------------------------------------

# having checkup records of 40's 50's 
ascvd_4050_id <- ascvd_cohort[,.(min_age=min(AGE), max_age=max(AGE)),by='PERSON_ID'][
  as.numeric(substr(min_age,1,1)==5) & as.numeric(substr(max_age,1,1)>=6)
  ,PERSON_ID]

ascvd_ages <- dcast(ascvd_cohort[PERSON_ID %in% ascvd_4050_id], PERSON_ID ~ AGE, value.var='AGE')
ascvd_ages
ascvd_ages[,before_60:= rowSums(.SD), .SDcols=2:11]
ascvd_ages[,after_60 := rowSums(.SD), .SDcols=12:21]
target_id <- ascvd_ages[before_60>=1 & after_60>=1,PERSON_ID]

# having records at least once each
ascvd_gj_eachone <- ascvd_cohort[PERSON_ID %in% target_id]
ascvd_gj_eachone <- ascvd_gj_eachone[,.(PERSON_ID, CHECKUP_DATE, CHECKUP_YEAR, ASCVD_DIAG_DATE, ASCVD_DIAG_AGE,
                                        SEX,AGE,BMI,FMLY_HDISE_PATIEN_YN, TOT_CHOLE,
                                        BP_HIGH,BP_LWST, BLDS,OLIG_PROTE_CD,
                                        WLK30_WEK_FREQ_ID, MOV20_WEK_FREQ_ID,MOV30_WEK_FREQ_ID, #exercise
                                        SMK_STAT_TYPE_RSPS_CD, DSQTY_RSPS_CD,CUR_DSQTY_RSPS_CD, # smoking
                                        DRNK_HABIT_RSPS_CD,TM1_DRKQTY_RSPS_CD,DRNK_HABIT_RSPS_CD #drinking
)]

# 음주, 흡연, 운동 --------------------------------------------------------------

ascvd_gj_eachone[,`:=`(
  WLK30_WEK_FREQ_ID = ifelse(WLK30_WEK_FREQ_ID=="",NA,as.numeric(WLK30_WEK_FREQ_ID)-1),
  MOV20_WEK_FREQ_ID = ifelse(MOV20_WEK_FREQ_ID=="",NA,as.numeric(MOV20_WEK_FREQ_ID)-1),
  MOV30_WEK_FREQ_ID = ifelse(MOV30_WEK_FREQ_ID=="",NA,as.numeric(MOV30_WEK_FREQ_ID)-1)
)]

ascvd_gj_eachone[,`:=`(
  MET_WALK = (WLK30_WEK_FREQ_ID * 20 * 3.3),
  MET_MED = (MOV20_WEK_FREQ_ID * 30 * 4.0),
  MET_HIGH = (MOV30_WEK_FREQ_ID * 30 * 8.0)
)]
ascvd_gj_eachone[,MET_CON := MET_WALK + MET_MED + MET_HIGH]
ascvd_gj_eachone[,MET_CAT := as.factor(ifelse(MET_HIGH>=1500 | MET_CON>=3000,'High',
                                              ifelse(MOV30_WEK_FREQ_ID>=4 | MOV20_WEK_FREQ_ID>=6 | MET_CON>=600,'Middle',
                                                     ifelse(MET_CON==0,'None','Low'))))]

ascvd_gj_eachone[,SMOKING:= ifelse(SMK_STAT_TYPE_RSPS_CD == 1,'None', 
                                   ifelse(CHECKUP_YEAR <2009 & DSQTY_RSPS_CD==1,'Light',
                                          ifelse(CHECKUP_YEAR < 2009 & DSQTY_RSPS_CD==2,'Moderate',
                                                 ifelse(CHECKUP_YEAR < 2009 & DSQTY_RSPS_CD>=3,'Heavy',
                                                        ifelse(CHECKUP_YEAR<2009 & DSQTY_RSPS_CD=="",NA,
                                                               ifelse(CHECKUP_YEAR>=2009 & CUR_DSQTY_RSPS_CD <10,'Light',
                                                                      ifelse(CHECKUP_YEAR>=2009 & CUR_DSQTY_RSPS_CD <20,'Moderate',
                                                                             ifelse(CHECKUP_YEAR>=2009 & CUR_DSQTY_RSPS_CD>=20,'Heavy',NA)))))))
)]

ascvd_gj_eachone[,DRINK_HABIT_RSPS_0208 := 
                   ifelse(CHECKUP_YEAR<2009 & DRNK_HABIT_RSPS_CD == 1,0, ifelse(
                     CHECKUP_YEAR<2009 & DRNK_HABIT_RSPS_CD ==2,1, ifelse(
                       CHECKUP_YEAR<2009 & DRNK_HABIT_RSPS_CD==3,2, ifelse(
                         CHECKUP_YEAR<2009 & DRNK_HABIT_RSPS_CD==4,4, ifelse(
                           CHECKUP_YEAR<2009 & DRNK_HABIT_RSPS_CD==5,7, NA)))))]

ascvd_gj_eachone[,TM1_DRKQTY_RSPS_0208 := 
                   ifelse(CHECKUP_YEAR<2009 & TM1_DRKQTY_RSPS_CD == 1,3, ifelse(
                     CHECKUP_YEAR<2009 & TM1_DRKQTY_RSPS_CD ==2,7, ifelse(
                       CHECKUP_YEAR<2009 & TM1_DRKQTY_RSPS_CD==3,11, ifelse(
                         CHECKUP_YEAR<2009 & TM1_DRKQTY_RSPS_CD==4,14, NA))))]


ascvd_gj_eachone[,DRINK_HABIT_RSPS_0915 := as.numeric(ifelse(CHECKUP_YEAR>=2009, 
                                                             as.numeric(DRNK_HABIT_RSPS_CD)-1,
                                                             NA))]

length(unique(ascvd_gj_eachone$PERSON_ID))


ascvd_gj_eachone[,DRINKING_CON :=ifelse(CHECKUP_YEAR<2009, (DRINK_HABIT_RSPS_0208 * TM1_DRKQTY_RSPS_0208),
                                        ifelse(CHECKUP_YEAR>=2009, as.numeric(DRINK_HABIT_RSPS_0915) * as.numeric(TM1_DRKQTY_RSPS_CD),NA))]

ascvd_gj_eachone[,DRINKING := ifelse(DRINKING_CON==0,'none',
                                     ifelse(DRINKING_CON<15,'light',
                                            ifelse(DRINKING_CON<30,'moderate','heavy')))]


# DF 만들기 ------------------------------------------------------------------

# last checkup before 50
last_gj_in_40 <- ascvd_gj_eachone[ascvd_gj_eachone[AGE<60,.I[.N],by=PERSON_ID]$V1,.(PERSON_ID,BMI,AGE, FMLY_HDISE_PATIEN_YN,
                                                                                    CHECKUP_DATE,CHECKUP_YEAR,
                                                                                    ASCVD_DIAG_DATE, ASCVD_DIAG_AGE)]

# first checkup after 52
first_gj_after_52 <- ascvd_gj_eachone[ascvd_gj_eachone[AGE>=62,.I[1L],by=PERSON_ID]$V1,.(PERSON_ID,SEX,AGE,
                                                                                         BMI,FMLY_HDISE_PATIEN_YN,
                                                                                         CHECKUP_DATE,CHECKUP_YEAR, TOT_CHOLE,
                                                                                         BP_HIGH,BP_LWST, BLDS,OLIG_PROTE_CD, 
                                                                                         SMOKING, DRINKING,MET_CAT)]

colnames(first_gj_after_52) <- paste0(colnames(first_gj_after_52),'_2nd')
setnames(first_gj_after_52,'PERSON_ID_2nd','PERSON_ID')

# last gj date for calculating observation duration
last_gj <- ascvd_gj_eachone[ascvd_gj_eachone[,.I[.N],by=PERSON_ID]$V1,.(PERSON_ID=PERSON_ID,
                                                                        LAST_CHECKUP_DATE = CHECKUP_DATE)]


ascvd_df <- inner_join(first_gj_after_52,last_gj_in_40,by='PERSON_ID')
ascvd_df <- inner_join(ascvd_df, last_gj, by='PERSON_ID')


# transform to date forms
dates_vars <- grep('DATE',names(ascvd_df), value=T)
ascvd_df[,(dates_vars):=lapply(.SD, function(x) as.Date(x, '%Y%m%d')), .SDcols=dates_vars]

# Exclude ASCVD patients before second checkup date
exclusion_id <- ascvd_df[ASCVD_DIAG_AGE < 62 | # diagnosed before 52
                           ASCVD_DIAG_DATE < CHECKUP_DATE_2nd # ASCVD diagnosis before 2nd checkup date
                         ,PERSON_ID]
ascvd_df <- ascvd_df[!PERSON_ID %in% exclusion_id]

# Determining ASCVD occurence date: DIAG or check-up, which is faster?
ascvd_df[,ASCVD_OCCUR_DATE := pmin(ASCVD_DIAG_DATE, na.rm=T)]
ascvd_df[,ASCVD_OCCUR_AGE := pmin(ASCVD_DIAG_AGE, na.rm=T)]



### obsevartion duration ###
# if not ascvd => last checkup date - first checkup date since 52
# if ascvd => ascvd_diag_date - first checkup date since 52
ascvd_df[,OBS_DURATION := ifelse(is.na(ASCVD_OCCUR_DATE),
                                 as.numeric(difftime(LAST_CHECKUP_DATE, CHECKUP_DATE_2nd,units='days')),
                                 as.numeric(difftime(ASCVD_OCCUR_DATE,CHECKUP_DATE_2nd, units='days')))]

ascvd_df[,OBS_DURATION_Y := round(OBS_DURATION/365,1)]
ascvd_df <- ascvd_df[!OBS_DURATION<0]
# Variables
ascvd_df[,SEX_2nd:= as.factor(ifelse(SEX_2nd==2,'F','M'))]
ascvd_df[,ASCVD_YN := ifelse(is.na(ASCVD_OCCUR_DATE),0,1)]
ascvd_df[,PROTE_URIA:=ifelse(is.na(OLIG_PROTE_CD_2nd), NA,
                             ifelse(OLIG_PROTE_CD_2nd>= '3','1','0'))]

# ordering factors
ascvd_df$DRINKING_2nd <- factor(ascvd_df$DRINKING_2nd, level=c('none','light','moderate','heavy'))
ascvd_df$SMOKING_2nd <- factor(ascvd_df$SMOKING_2nd, level=c('None','Light','Moderate','Heavy'))
ascvd_df$MET_CAT_2nd <- factor(ascvd_df$MET_CAT_2nd, levels=c('None','Low','Middle','High'))

# change var names
setnames(ascvd_df,'BP_HIGH_2nd','SBP_2nd')
setnames(ascvd_df,'BP_LWST_2nd','DBP_2nd')

ascvd_df[,FMLY_HDISE_PATIEN_YN:=as.factor(ifelse(FMLY_HDISE_PATIEN_YN=='2','1',
                                                 ifelse(FMLY_HDISE_PATIEN_YN=='1','0',
                                                        ifelse(FMLY_HDISE_PATIEN_YN=="",NA,FMLY_HDISE_PATIEN_YN))))]
ascvd_df[,FHx_ASCVD:=pmax(FMLY_HDISE_PATIEN_YN, FMLY_HDISE_PATIEN_YN_2nd, na.rm=T)]

ascvd_df[,BMI_GROUP := as.factor(ifelse(BMI < 25 & BMI_2nd <25, 'A',
                                        ifelse(BMI < 25 & BMI_2nd >=25, 'B',
                                               ifelse(BMI >= 25 & BMI_2nd < 25,'C',
                                                      ifelse(BMI >= 25 & BMI_2nd >=25, 'D', NA)))))]

ascvd_df[,`:=`(
  SMK = as.factor(ifelse(SMOKING_2nd =='None',0,1)),
  EXC = as.factor(ifelse(MET_CAT_2nd == 'None',0,1))
)]

ascvd_df[,T2E:= ifelse(ASCVD_YN==1,as.numeric(difftime(ASCVD_OCCUR_DATE, CHECKUP_DATE_2nd , units='days')),NA)]


ascvd_df[,BLDS_CAT:= as.factor(ifelse(BLDS_2nd<100,0, ifelse(BLDS_2nd<126,1,2)))]
ascvd_df[,CHOLE_CAT:= as.factor(ifelse(TOT_CHOLE_2nd<200,0, ifelse(TOT_CHOLE_2nd<240,1,2)))]

ascvd_df[,SBP_CAT := as.factor(ifelse(SBP_2nd<120,0,ifelse(SBP_2nd<140,1,2)))]
ascvd_df[,DBP_CAT := as.factor(ifelse(DBP_2nd<80,0,ifelse(DBP_2nd<90,1,2)))]

# CCI ---------------------------------------------------------------------
cci_list <- setDT(read_sas('working/DM/cci_list.sas7bdat'))

ascvd_df <- inner_join(ascvd_df, cci_list, by=c("PERSON_ID" = "PERSON_ID", 'CHECKUP_YEAR_2nd'='year'))


ascvd_df[,`:=`(
  CCI_ORG_CAT = as.factor(ifelse(org_CCI==0,0, ifelse(org_CCI==1,1,2))),
  CCI_WT_CAT = as.factor(ifelse(weight_cci==0,0,ifelse(weight_cci==1,1,2)))
)]

# 2. 45세 전후 ------------------------------------------------------------------

# having checkup records of 40's 50's 
ascvd_4050_id <- ascvd_cohort[,.(min_age=min(AGE), max_age=max(AGE)),by='PERSON_ID'][
  as.numeric(substr(min_age,1,1)==4) & as.numeric(substr(max_age,1,1)>=5)
  ,PERSON_ID]

ascvd_ages <- dcast(ascvd_cohort[PERSON_ID %in% ascvd_4050_id], PERSON_ID ~ AGE, value.var='AGE')
ascvd_ages[,before_45:= rowSums(.SD), .SDcols=2:6]
ascvd_ages[,after_45 := rowSums(.SD), .SDcols=7:15]
target_id <- ascvd_ages[before_45>=1 & after_45>=1,PERSON_ID]

# having records at least once each
ascvd_gj_eachone <- ascvd_cohort[PERSON_ID %in% target_id]
ascvd_gj_eachone <- ascvd_gj_eachone[,.(PERSON_ID, CHECKUP_DATE, CHECKUP_YEAR, ASCVD_DIAG_DATE, ASCVD_DIAG_AGE,
                                        SEX,AGE,BMI,FMLY_HDISE_PATIEN_YN, TOT_CHOLE, INCOME_4,
                                        BP_HIGH,BP_LWST, BLDS,OLIG_PROTE_CD,
                                        WLK30_WEK_FREQ_ID, MOV20_WEK_FREQ_ID,MOV30_WEK_FREQ_ID, #exercise
                                        SMK_STAT_TYPE_RSPS_CD, DSQTY_RSPS_CD,CUR_DSQTY_RSPS_CD, # smoking
                                        DRNK_HABIT_RSPS_CD,TM1_DRKQTY_RSPS_CD,DRNK_HABIT_RSPS_CD #drinking
)]

# 음주, 흡연, 운동 --------------------------------------------------------------

ascvd_gj_eachone[,`:=`(
  WLK30_WEK_FREQ_ID = ifelse(WLK30_WEK_FREQ_ID=="",NA,as.numeric(WLK30_WEK_FREQ_ID)-1),
  MOV20_WEK_FREQ_ID = ifelse(MOV20_WEK_FREQ_ID=="",NA,as.numeric(MOV20_WEK_FREQ_ID)-1),
  MOV30_WEK_FREQ_ID = ifelse(MOV30_WEK_FREQ_ID=="",NA,as.numeric(MOV30_WEK_FREQ_ID)-1)
)]

ascvd_gj_eachone[,`:=`(
  MET_WALK = (WLK30_WEK_FREQ_ID * 20 * 3.3),
  MET_MED = (MOV20_WEK_FREQ_ID * 30 * 4.0),
  MET_HIGH = (MOV30_WEK_FREQ_ID * 30 * 8.0)
)]
ascvd_gj_eachone[,MET_CON := MET_WALK + MET_MED + MET_HIGH]
ascvd_gj_eachone[,MET_CAT := as.factor(ifelse(MET_HIGH>=1500 | MET_CON>=3000,'High',
                                              ifelse(MOV30_WEK_FREQ_ID>=4 | MOV20_WEK_FREQ_ID>=6 | MET_CON>=600,'Middle',
                                                     ifelse(MET_CON==0,'None','Low'))))]

ascvd_gj_eachone[,SMOKING:= ifelse(SMK_STAT_TYPE_RSPS_CD == 1,'None', 
                                   ifelse(CHECKUP_YEAR <2009 & DSQTY_RSPS_CD==1,'Light',
                                          ifelse(CHECKUP_YEAR < 2009 & DSQTY_RSPS_CD==2,'Moderate',
                                                 ifelse(CHECKUP_YEAR < 2009 & DSQTY_RSPS_CD>=3,'Heavy',
                                                        ifelse(CHECKUP_YEAR<2009 & DSQTY_RSPS_CD=="",NA,
                                                               ifelse(CHECKUP_YEAR>=2009 & CUR_DSQTY_RSPS_CD <10,'Light',
                                                                      ifelse(CHECKUP_YEAR>=2009 & CUR_DSQTY_RSPS_CD <20,'Moderate',
                                                                             ifelse(CHECKUP_YEAR>=2009 & CUR_DSQTY_RSPS_CD>=20,'Heavy',NA)))))))
)]

ascvd_gj_eachone[,DRINK_HABIT_RSPS_0208 := 
                   ifelse(CHECKUP_YEAR<2009 & DRNK_HABIT_RSPS_CD == 1,0, ifelse(
                     CHECKUP_YEAR<2009 & DRNK_HABIT_RSPS_CD ==2,1, ifelse(
                       CHECKUP_YEAR<2009 & DRNK_HABIT_RSPS_CD==3,2, ifelse(
                         CHECKUP_YEAR<2009 & DRNK_HABIT_RSPS_CD==4,4, ifelse(
                           CHECKUP_YEAR<2009 & DRNK_HABIT_RSPS_CD==5,7, NA)))))]

ascvd_gj_eachone[,TM1_DRKQTY_RSPS_0208 := 
                   ifelse(CHECKUP_YEAR<2009 & TM1_DRKQTY_RSPS_CD == 1,3, ifelse(
                     CHECKUP_YEAR<2009 & TM1_DRKQTY_RSPS_CD ==2,7, ifelse(
                       CHECKUP_YEAR<2009 & TM1_DRKQTY_RSPS_CD==3,11, ifelse(
                         CHECKUP_YEAR<2009 & TM1_DRKQTY_RSPS_CD==4,14, NA))))]


ascvd_gj_eachone[,DRINK_HABIT_RSPS_0915 := as.numeric(ifelse(CHECKUP_YEAR>=2009, 
                                                             as.numeric(DRNK_HABIT_RSPS_CD)-1,
                                                             NA))]

length(unique(ascvd_gj_eachone$PERSON_ID))


ascvd_gj_eachone[,DRINKING_CON :=ifelse(CHECKUP_YEAR<2009, (DRINK_HABIT_RSPS_0208 * TM1_DRKQTY_RSPS_0208),
                                        ifelse(CHECKUP_YEAR>=2009, as.numeric(DRINK_HABIT_RSPS_0915) * as.numeric(TM1_DRKQTY_RSPS_CD),NA))]

ascvd_gj_eachone[,DRINKING := ifelse(DRINKING_CON==0,'none',
                                     ifelse(DRINKING_CON<15,'light',
                                            ifelse(DRINKING_CON<30,'moderate','heavy')))]


# DF 만들기 ------------------------------------------------------------------

# last checkup before 50
last_gj_in_40 <- ascvd_gj_eachone[ascvd_gj_eachone[AGE<45,.I[.N],by=PERSON_ID]$V1,.(PERSON_ID,BMI,AGE, FMLY_HDISE_PATIEN_YN,
                                                                                    CHECKUP_DATE,CHECKUP_YEAR,
                                                                                    ASCVD_DIAG_DATE, ASCVD_DIAG_AGE)]

# first checkup after 52
first_gj_after_52 <- ascvd_gj_eachone[ascvd_gj_eachone[AGE>=45,.I[1L],by=PERSON_ID]$V1,.(PERSON_ID,SEX,AGE,
                                                                                         BMI,FMLY_HDISE_PATIEN_YN, INCOME_4,
                                                                                         CHECKUP_DATE,CHECKUP_YEAR, TOT_CHOLE,
                                                                                         BP_HIGH,BP_LWST, BLDS,OLIG_PROTE_CD, 
                                                                                         SMOKING, DRINKING,MET_CAT)]

colnames(first_gj_after_52) <- paste0(colnames(first_gj_after_52),'_2nd')
setnames(first_gj_after_52,'PERSON_ID_2nd','PERSON_ID')

# last gj date for calculating observation duration
last_gj <- ascvd_gj_eachone[ascvd_gj_eachone[,.I[.N],by=PERSON_ID]$V1,.(PERSON_ID=PERSON_ID,
                                                                        LAST_CHECKUP_DATE = CHECKUP_DATE)]


ascvd_df <- inner_join(first_gj_after_52,last_gj_in_40,by='PERSON_ID')
ascvd_df <- inner_join(ascvd_df, last_gj, by='PERSON_ID')


# transform to date forms
dates_vars <- grep('DATE',names(ascvd_df), value=T)
ascvd_df[,(dates_vars):=lapply(.SD, function(x) as.Date(x, '%Y%m%d')), .SDcols=dates_vars]

# Exclude ASCVD patients before second checkup date
exclusion_id <- ascvd_df[ASCVD_DIAG_AGE < 45 | # diagnosed before 52
                           ASCVD_DIAG_DATE < CHECKUP_DATE_2nd # ASCVD diagnosis before 2nd checkup date
                         ,PERSON_ID]
ascvd_df <- ascvd_df[!PERSON_ID %in% exclusion_id]

# Determining ASCVD occurence date: DIAG or check-up, which is faster?
ascvd_df[,ASCVD_OCCUR_DATE := pmin(ASCVD_DIAG_DATE, na.rm=T)]
ascvd_df[,ASCVD_OCCUR_AGE := pmin(ASCVD_DIAG_AGE, na.rm=T)]



### obsevartion duration ###
# if not ascvd => last checkup date - first checkup date since 52
# if ascvd => ascvd_diag_date - first checkup date since 52
ascvd_df[,OBS_DURATION := ifelse(is.na(ASCVD_OCCUR_DATE),
                                 as.numeric(difftime(LAST_CHECKUP_DATE, CHECKUP_DATE_2nd,units='days')),
                                 as.numeric(difftime(ASCVD_OCCUR_DATE,CHECKUP_DATE_2nd, units='days')))]

ascvd_df[,OBS_DURATION_Y := round(OBS_DURATION/365,1)]

ascvd_df <- ascvd_df[!OBS_DURATION<0]
# Variables
ascvd_df[,SEX_2nd:= as.factor(ifelse(SEX_2nd==2,'F','M'))]
ascvd_df[,ASCVD_YN := ifelse(is.na(ASCVD_OCCUR_DATE),0,1)]
ascvd_df[,PROTE_URIA:=ifelse(is.na(OLIG_PROTE_CD_2nd), NA,
                             ifelse(OLIG_PROTE_CD_2nd>= '3','1','0'))]

# ordering factors
ascvd_df$DRINKING_2nd <- factor(ascvd_df$DRINKING_2nd, level=c('none','light','moderate','heavy'))
ascvd_df$SMOKING_2nd <- factor(ascvd_df$SMOKING_2nd, level=c('None','Light','Moderate','Heavy'))
ascvd_df$MET_CAT_2nd <- factor(ascvd_df$MET_CAT_2nd, levels=c('None','Low','Middle','High'))

# change var names
setnames(ascvd_df,'BP_HIGH_2nd','SBP_2nd')
setnames(ascvd_df,'BP_LWST_2nd','DBP_2nd')

ascvd_df[,FMLY_HDISE_PATIEN_YN:=as.factor(ifelse(FMLY_HDISE_PATIEN_YN=='2','1',
                                                 ifelse(FMLY_HDISE_PATIEN_YN=='1','0',
                                                        ifelse(FMLY_HDISE_PATIEN_YN=="",NA,FMLY_HDISE_PATIEN_YN))))]
ascvd_df[,FHx_ASCVD:=pmax(FMLY_HDISE_PATIEN_YN, FMLY_HDISE_PATIEN_YN_2nd, na.rm=T)]

ascvd_df[,BMI_GROUP := as.factor(ifelse(BMI < 25 & BMI_2nd <25, 'A',
                                        ifelse(BMI < 25 & BMI_2nd >=25, 'B',
                                               ifelse(BMI >= 25 & BMI_2nd < 25,'C',
                                                      ifelse(BMI >= 25 & BMI_2nd >=25, 'D', NA)))))]

ascvd_df[,`:=`(
  SMK = as.factor(ifelse(SMOKING_2nd =='None',0,1)),
  EXC = as.factor(ifelse(MET_CAT_2nd == 'None',0,1))
)]

ascvd_df[,T2E:= ifelse(ASCVD_YN==1,as.numeric(difftime(ASCVD_OCCUR_DATE, CHECKUP_DATE_2nd , units='days')),NA)]
ascvd_df[,BLDS_CAT:= as.factor(ifelse(BLDS_2nd<100,0, ifelse(BLDS_2nd<126,1,2)))]
ascvd_df[,CHOLE_CAT:= as.factor(ifelse(TOT_CHOLE_2nd<200,0, ifelse(TOT_CHOLE_2nd<240,1,2)))]

ascvd_df[,SBP_CAT := as.factor(ifelse(SBP_2nd<120,0,ifelse(SBP_2nd<140,1,2)))]
ascvd_df[,DBP_CAT := as.factor(ifelse(DBP_2nd<80,0,ifelse(DBP_2nd<90,1,2)))]


ascvd_df <- inner_join(ascvd_df, cci_list, by=c("PERSON_ID" = "PERSON_ID", 'CHECKUP_YEAR_2nd'='year'))


ascvd_df[,`:=`(
  CCI_ORG_CAT = as.factor(ifelse(org_CCI==0,0, ifelse(org_CCI==1,1,2))),
  CCI_WT_CAT = as.factor(ifelse(weight_cci==0,0,ifelse(weight_cci==1,1,2)))
)]

ascvd_df[,BMI_GROUP_NEW := as.factor(ifelse(BMI<23.0,0, ifelse(BMI<25.0,1,2)))]
ascvd_df[,BMI_GROUP_NEW_2nd := as.factor(ifelse(BMI_2nd<23.0,0, ifelse(BMI_2nd<25.0,1,2)))]
ascvd_df[,BMI_GROUP_NEW_CAT:= as.factor(ifelse(BMI_GROUP_NEW==0 & BMI_GROUP_NEW_2nd==0,'A0',
                                               ifelse(BMI_GROUP_NEW==0 & BMI_GROUP_NEW_2nd==1,'A1',
                                                      ifelse(BMI_GROUP_NEW==0 & BMI_GROUP_NEW_2nd==2,'A2',
                                                             ifelse(BMI_GROUP_NEW==1 & BMI_GROUP_NEW_2nd==0,'B0',
                                                                    ifelse(BMI_GROUP_NEW==1 & BMI_GROUP_NEW_2nd==1,'B1',
                                                                           ifelse(BMI_GROUP_NEW==1 & BMI_GROUP_NEW_2nd==2,'B2',
                                                                                  ifelse(BMI_GROUP_NEW==2 & BMI_GROUP_NEW_2nd==0,'C0',
                                                                                         ifelse(BMI_GROUP_NEW==2 & BMI_GROUP_NEW_2nd==1,'C1','C2')))))))))]




# 3. 65세 전후 ------------------------------------------------------------------

# having checkup records of 40's 50's 
ascvd_4050_id <- ascvd_cohort[,.(min_age=min(AGE), max_age=max(AGE)),by='PERSON_ID'][
  as.numeric(substr(min_age,1,1)==5) & as.numeric(substr(max_age,1,1)>=6)
  ,PERSON_ID]

ascvd_ages <- dcast(ascvd_cohort[PERSON_ID %in% ascvd_4050_id], PERSON_ID ~ AGE, value.var='AGE')
ascvd_ages
ascvd_ages[,before_65:= rowSums(.SD), .SDcols=2:16]
ascvd_ages[,after_65 := rowSums(.SD), .SDcols=17:24]
target_id <- ascvd_ages[before_65>=1 & after_65>=1,PERSON_ID]

# having records at least once each
ascvd_gj_eachone <- ascvd_cohort[PERSON_ID %in% target_id]
ascvd_gj_eachone <- ascvd_gj_eachone[,.(PERSON_ID, CHECKUP_DATE, CHECKUP_YEAR, ASCVD_DIAG_DATE, ASCVD_DIAG_AGE,
                                        SEX,AGE,BMI,FMLY_HDISE_PATIEN_YN, TOT_CHOLE, INCOME_4,
                                        BP_HIGH,BP_LWST, BLDS,OLIG_PROTE_CD,
                                        WLK30_WEK_FREQ_ID, MOV20_WEK_FREQ_ID,MOV30_WEK_FREQ_ID, #exercise
                                        SMK_STAT_TYPE_RSPS_CD, DSQTY_RSPS_CD,CUR_DSQTY_RSPS_CD, # smoking
                                        DRNK_HABIT_RSPS_CD,TM1_DRKQTY_RSPS_CD,DRNK_HABIT_RSPS_CD #drinking
)]

# 음주, 흡연, 운동 --------------------------------------------------------------

ascvd_gj_eachone[,`:=`(
  WLK30_WEK_FREQ_ID = ifelse(WLK30_WEK_FREQ_ID=="",NA,as.numeric(WLK30_WEK_FREQ_ID)-1),
  MOV20_WEK_FREQ_ID = ifelse(MOV20_WEK_FREQ_ID=="",NA,as.numeric(MOV20_WEK_FREQ_ID)-1),
  MOV30_WEK_FREQ_ID = ifelse(MOV30_WEK_FREQ_ID=="",NA,as.numeric(MOV30_WEK_FREQ_ID)-1)
)]

ascvd_gj_eachone[,`:=`(
  MET_WALK = (WLK30_WEK_FREQ_ID * 20 * 3.3),
  MET_MED = (MOV20_WEK_FREQ_ID * 30 * 4.0),
  MET_HIGH = (MOV30_WEK_FREQ_ID * 30 * 8.0)
)]
ascvd_gj_eachone[,MET_CON := MET_WALK + MET_MED + MET_HIGH]
ascvd_gj_eachone[,MET_CAT := as.factor(ifelse(MET_HIGH>=1500 | MET_CON>=3000,'High',
                                              ifelse(MOV30_WEK_FREQ_ID>=4 | MOV20_WEK_FREQ_ID>=6 | MET_CON>=600,'Middle',
                                                     ifelse(MET_CON==0,'None','Low'))))]

ascvd_gj_eachone[,SMOKING:= ifelse(SMK_STAT_TYPE_RSPS_CD == 1,'None', 
                                   ifelse(CHECKUP_YEAR <2009 & DSQTY_RSPS_CD==1,'Light',
                                          ifelse(CHECKUP_YEAR < 2009 & DSQTY_RSPS_CD==2,'Moderate',
                                                 ifelse(CHECKUP_YEAR < 2009 & DSQTY_RSPS_CD>=3,'Heavy',
                                                        ifelse(CHECKUP_YEAR<2009 & DSQTY_RSPS_CD=="",NA,
                                                               ifelse(CHECKUP_YEAR>=2009 & CUR_DSQTY_RSPS_CD <10,'Light',
                                                                      ifelse(CHECKUP_YEAR>=2009 & CUR_DSQTY_RSPS_CD <20,'Moderate',
                                                                             ifelse(CHECKUP_YEAR>=2009 & CUR_DSQTY_RSPS_CD>=20,'Heavy',NA)))))))
)]

ascvd_gj_eachone[,DRINK_HABIT_RSPS_0208 := 
                   ifelse(CHECKUP_YEAR<2009 & DRNK_HABIT_RSPS_CD == 1,0, ifelse(
                     CHECKUP_YEAR<2009 & DRNK_HABIT_RSPS_CD ==2,1, ifelse(
                       CHECKUP_YEAR<2009 & DRNK_HABIT_RSPS_CD==3,2, ifelse(
                         CHECKUP_YEAR<2009 & DRNK_HABIT_RSPS_CD==4,4, ifelse(
                           CHECKUP_YEAR<2009 & DRNK_HABIT_RSPS_CD==5,7, NA)))))]

ascvd_gj_eachone[,TM1_DRKQTY_RSPS_0208 := 
                   ifelse(CHECKUP_YEAR<2009 & TM1_DRKQTY_RSPS_CD == 1,3, ifelse(
                     CHECKUP_YEAR<2009 & TM1_DRKQTY_RSPS_CD ==2,7, ifelse(
                       CHECKUP_YEAR<2009 & TM1_DRKQTY_RSPS_CD==3,11, ifelse(
                         CHECKUP_YEAR<2009 & TM1_DRKQTY_RSPS_CD==4,14, NA))))]


ascvd_gj_eachone[,DRINK_HABIT_RSPS_0915 := as.numeric(ifelse(CHECKUP_YEAR>=2009, 
                                                             as.numeric(DRNK_HABIT_RSPS_CD)-1,
                                                             NA))]

length(unique(ascvd_gj_eachone$PERSON_ID))


ascvd_gj_eachone[,DRINKING_CON :=ifelse(CHECKUP_YEAR<2009, (DRINK_HABIT_RSPS_0208 * TM1_DRKQTY_RSPS_0208),
                                        ifelse(CHECKUP_YEAR>=2009, as.numeric(DRINK_HABIT_RSPS_0915) * as.numeric(TM1_DRKQTY_RSPS_CD),NA))]

ascvd_gj_eachone[,DRINKING := ifelse(DRINKING_CON==0,'none',
                                     ifelse(DRINKING_CON<15,'light',
                                            ifelse(DRINKING_CON<30,'moderate','heavy')))]


# DF 만들기 ------------------------------------------------------------------

# last checkup before 50
last_gj_in_40 <- ascvd_gj_eachone[ascvd_gj_eachone[AGE<65,.I[.N],by=PERSON_ID]$V1,.(PERSON_ID,BMI,AGE, FMLY_HDISE_PATIEN_YN,
                                                                                    CHECKUP_DATE,CHECKUP_YEAR,
                                                                                    ASCVD_DIAG_DATE, ASCVD_DIAG_AGE)]

# first checkup after 52
first_gj_after_52 <- ascvd_gj_eachone[ascvd_gj_eachone[AGE>=65,.I[1L],by=PERSON_ID]$V1,.(PERSON_ID,SEX,AGE,
                                                                                         BMI,FMLY_HDISE_PATIEN_YN, INCOME_4,
                                                                                         CHECKUP_DATE,CHECKUP_YEAR, TOT_CHOLE,
                                                                                         BP_HIGH,BP_LWST, BLDS,OLIG_PROTE_CD, 
                                                                                         SMOKING, DRINKING,MET_CAT)]

colnames(first_gj_after_52) <- paste0(colnames(first_gj_after_52),'_2nd')
setnames(first_gj_after_52,'PERSON_ID_2nd','PERSON_ID')

# last gj date for calculating observation duration
last_gj <- ascvd_gj_eachone[ascvd_gj_eachone[,.I[.N],by=PERSON_ID]$V1,.(PERSON_ID=PERSON_ID,
                                                                        LAST_CHECKUP_DATE = CHECKUP_DATE)]


ascvd_df <- inner_join(first_gj_after_52,last_gj_in_40,by='PERSON_ID')
ascvd_df <- inner_join(ascvd_df, last_gj, by='PERSON_ID')


# transform to date forms
dates_vars <- grep('DATE',names(ascvd_df), value=T)
ascvd_df[,(dates_vars):=lapply(.SD, function(x) as.Date(x, '%Y%m%d')), .SDcols=dates_vars]

# Exclude ASCVD patients before second checkup date
exclusion_id <- ascvd_df[ASCVD_DIAG_AGE < 65 | # diagnosed before 52
                           ASCVD_DIAG_DATE < CHECKUP_DATE_2nd # ASCVD diagnosis before 2nd checkup date
                         ,PERSON_ID]
ascvd_df <- ascvd_df[!PERSON_ID %in% exclusion_id]

# Determining ASCVD occurence date: DIAG or check-up, which is faster?
ascvd_df[,ASCVD_OCCUR_DATE := pmin(ASCVD_DIAG_DATE, na.rm=T)]
ascvd_df[,ASCVD_OCCUR_AGE := pmin(ASCVD_DIAG_AGE, na.rm=T)]



### obsevartion duration ###
# if not ascvd => last checkup date - first checkup date since 52
# if ascvd => ascvd_diag_date - first checkup date since 52
ascvd_df[,OBS_DURATION := ifelse(is.na(ASCVD_OCCUR_DATE),
                                 as.numeric(difftime(LAST_CHECKUP_DATE, CHECKUP_DATE_2nd,units='days')),
                                 as.numeric(difftime(ASCVD_OCCUR_DATE,CHECKUP_DATE_2nd, units='days')))]

ascvd_df[,OBS_DURATION_Y := round(OBS_DURATION/365,1)]
ascvd_df <- ascvd_df[!OBS_DURATION<0]

# Variables
ascvd_df[,SEX_2nd:= as.factor(ifelse(SEX_2nd==2,'F','M'))]
ascvd_df[,ASCVD_YN := ifelse(is.na(ASCVD_OCCUR_DATE),0,1)]
ascvd_df[,PROTE_URIA:=ifelse(is.na(OLIG_PROTE_CD_2nd), NA,
                             ifelse(OLIG_PROTE_CD_2nd>= '3','1','0'))]

# ordering factors
ascvd_df$DRINKING_2nd <- factor(ascvd_df$DRINKING_2nd, level=c('none','light','moderate','heavy'))
ascvd_df$SMOKING_2nd <- factor(ascvd_df$SMOKING_2nd, level=c('None','Light','Moderate','Heavy'))
ascvd_df$MET_CAT_2nd <- factor(ascvd_df$MET_CAT_2nd, levels=c('None','Low','Middle','High'))

# change var names
setnames(ascvd_df,'BP_HIGH_2nd','SBP_2nd')
setnames(ascvd_df,'BP_LWST_2nd','DBP_2nd')

ascvd_df[,FMLY_HDISE_PATIEN_YN:=as.factor(ifelse(FMLY_HDISE_PATIEN_YN=='2','1',
                                                 ifelse(FMLY_HDISE_PATIEN_YN=='1','0',
                                                        ifelse(FMLY_HDISE_PATIEN_YN=="",NA,FMLY_HDISE_PATIEN_YN))))]
ascvd_df[,FHx_ASCVD:=pmax(FMLY_HDISE_PATIEN_YN, FMLY_HDISE_PATIEN_YN_2nd, na.rm=T)]

ascvd_df[,BMI_GROUP := as.factor(ifelse(BMI < 25 & BMI_2nd <25, 'A',
                                        ifelse(BMI < 25 & BMI_2nd >=25, 'B',
                                               ifelse(BMI >= 25 & BMI_2nd < 25,'C',
                                                      ifelse(BMI >= 25 & BMI_2nd >=25, 'D', NA)))))]

ascvd_df[,`:=`(
  SMK = as.factor(ifelse(SMOKING_2nd =='None',0,1)),
  EXC = as.factor(ifelse(MET_CAT_2nd == 'None',0,1))
)]

ascvd_df[,T2E:= ifelse(ASCVD_YN==1,as.numeric(difftime(ASCVD_OCCUR_DATE, CHECKUP_DATE_2nd , units='days')),NA)]
ascvd_df[,BLDS_CAT:= as.factor(ifelse(BLDS_2nd<100,0, ifelse(BLDS_2nd<126,1,2)))]
ascvd_df[,CHOLE_CAT:= as.factor(ifelse(TOT_CHOLE_2nd<200,0, ifelse(TOT_CHOLE_2nd<240,1,2)))]
ascvd_df[,SBP_CAT := as.factor(ifelse(SBP_2nd<120,0,ifelse(SBP_2nd<140,1,2)))]
ascvd_df[,DBP_CAT := as.factor(ifelse(DBP_2nd<80,0,ifelse(DBP_2nd<90,1,2)))]


ascvd_df <- inner_join(ascvd_df, cci_list, by=c("PERSON_ID" = "PERSON_ID", 'CHECKUP_YEAR_2nd'='year'))


ascvd_df[,`:=`(
  CCI_ORG_CAT = as.factor(ifelse(org_CCI==0,0, ifelse(org_CCI==1,1,2))),
  CCI_WT_CAT = as.factor(ifelse(weight_cci==0,0,ifelse(weight_cci==1,1,2)))
)]

ascvd_df[,BMI_GROUP_NEW := as.factor(ifelse(BMI<23.0,0, ifelse(BMI<25.0,1,2)))]
ascvd_df[,BMI_GROUP_NEW_2nd := as.factor(ifelse(BMI_2nd<23.0,0, ifelse(BMI_2nd<25.0,1,2)))]
ascvd_df[,BMI_GROUP_NEW_CAT:= as.factor(ifelse(BMI_GROUP_NEW==0 & BMI_GROUP_NEW_2nd==0,'A0',
                                               ifelse(BMI_GROUP_NEW==0 & BMI_GROUP_NEW_2nd==1,'A1',
                                                      ifelse(BMI_GROUP_NEW==0 & BMI_GROUP_NEW_2nd==2,'A2',
                                                             ifelse(BMI_GROUP_NEW==1 & BMI_GROUP_NEW_2nd==0,'B0',
                                                                    ifelse(BMI_GROUP_NEW==1 & BMI_GROUP_NEW_2nd==1,'B1',
                                                                           ifelse(BMI_GROUP_NEW==1 & BMI_GROUP_NEW_2nd==2,'B2',
                                                                                  ifelse(BMI_GROUP_NEW==2 & BMI_GROUP_NEW_2nd==0,'C0',
                                                                                         ifelse(BMI_GROUP_NEW==2 & BMI_GROUP_NEW_2nd==1,'C1','C2')))))))))]


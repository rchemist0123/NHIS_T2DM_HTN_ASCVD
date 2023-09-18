

# 관찰기간
htn_df[PERSON_ID %in% htn_id,.(mean(OBS_DURATION_Y), sd(OBS_DURATION_Y))]

# mean t2e
htn_df[!is.na(T2E) & PERSON_ID %in% htn_id,.(mean(T2E)/365, sd(T2E)/365)]

#target ID
htn_id <- htn_df[!is.na(SEX_2nd) &
                   !is.na(AGE_2nd) &
                   !is.na(BMI_GROUP) &
                   !is.na(FHx_HTN) &
                   !is.na(SBP_2nd) &
                   !is.na(DBP_2nd) &
                   !is.na(SMOKING_2nd)&
                   # !is.na(DRINKING_2nd) &
                   !is.na(MET_CAT_2nd) &
                   !is.na(PROTE_URIA) &
                   !is.na(BLDS_2nd)
                 ,PERSON_ID]


### table 1 ------------------------------------------------------
require(tableone)
vars = c('SEX_2nd','AGE_2nd','BMI_2nd', 'SBP_2nd','DBP_2nd','BLDS_2nd','FHx_HTN','PROTE_URIA',
         'SMK','EXC')
vars = c('SEX','AGE','BMI', 'SBP','DBP','BLDS','FMLY_HPRTS_PATIEN_YN','PROTE_URIA',
         'SMOKING','MET_CAT')
catvars = c('FMLY_HPRTS_PATIEN_YN','PROTE_URIA',
            'SMOKING','MET_CAT')
CreateTableOne(vars=vars, factorVars = catvars, 
               strata='HTN_YN',
               data=htn_df[PERSON_ID %in% htn_id])
# strata = 'HTN_YN')


table(htn_df[PERSON_ID %in% htn_id]$HTN_DIAG_AGE)

### table 2 --------------------------------------------------
CreateTableOne(vars=vars, factorVars = catvars, 
               strata = 'BMI_GROUP',
               data=htn_df[PERSON_ID %in% htn_id])


htn_df[PERSON_ID %in% htn_id,.(n=.N,
                               event = sum(HTN_YN),
                               duration = sum(OBS_DURATION_Y),
                               rate = sum(as.numeric(HTN_YN))/sum(OBS_DURATION_Y)*1000), by=BMI_GROUP][order(BMI_GROUP)]


# Table 3 -----------------------------------------------------------------

htn_df[PERSON_ID %in% htn_id,.(n=.N,
                                 event = sum(HTN_YN),
                                 duration = sum(OBS_DURATION_Y),
                                 rate = sum(as.numeric(HTN_YN))/sum(OBS_DURATION_Y)*1000), by=BMI_GROUP][order(BMI_GROUP)]
# cox regerssion
# 1. model 1: only BMI_GRORP
fit1 <- coxph(Surv(OBS_DURATION, HTN_YN==1) ~ BMI_GROUP,
              data=htn_df[PERSON_ID %in% htn_id])
# 2. model 2: + sex, age
fit2 <- coxph(Surv(OBS_DURATION, HTN_YN==1) ~ BMI_GROUP+ SEX_2nd + AGE_2nd,
              data=htn_df[PERSON_ID %in% htn_id])

#3. model 3: + all
fit3 <- coxph(Surv(OBS_DURATION, HTN_YN==1) ~ 
                BMI_GROUP+ 
                SEX_2nd + 
                AGE_2nd + 
                BMI_2nd+ 
                BLDS_2nd +
                SBP_2nd + DBP_2nd +
                FHx_HTN+
                PROTE_URIA +
                SMK+
                # DRINKING_2nd+
                EXC,
              data= htn_df[PERSON_ID %in% htn_id])

createTable3(mainVar = 'BMI_GROUP', fit1, fit2, fit3, data=htn_df[PERSON_ID %in% htn_id])
summary(fit3)

# subgroup ----------------------------------------------------------------

require(Publish)
subgroups <- htn_df[BMI_GROUP %in% c('A','D')]
subgroups[,group :=as.factor(ifelse(BMI_GROUP=='A',0,1))]
subfit <- coxph(Surv(OBS_DURATION, HTN_YN==1) ~ 
        group+ 
        SEX_2nd + 
        AGE_2nd + 
        BMI_2nd+ 
        PROTE_URIA+
        FHx_HTN+
        BLDS_2nd +
        SBP_2nd + 
          DBP_2nd+
        SMK+
        EXC,
      data= subgroups[PERSON_ID %in% htn_id])
subfit %>% summary()
tars <- c('BMI_GROUP','SEX_2nd','FHx_HTN','PROTE_URIA','SMK','EXC')
subgroups[,(tars):=lapply(.SD, as.factor),.SDcols=tars]

rst <- subgroupAnalysis(subfit, 
                 treatment = 'group',
                 subgroups = c('SEX_2nd','FHx_HTN','PROTE_URIA','SMK','EXC'),
                 data= subgroups %>% filter(PERSON_ID %in% htn_id))
setDT(rst)[,`:=`(ratio1= paste0(event_0,'/',sample_0),
          ratio2=paste0(event_1,'/',sample_1))][,.(subgroups,ratio1,ratio2,HazardRatio,
                                                   lower=format(round(Lower,2),nsmall=2),
                                                   upper=format(round(Upper,2),nsmall=2),
                                                   pinteraction=ifelse(pinteraction<0.001,'<0.001',round(pinteraction,4)))]

# Supple table 1 --------------------------------------------------------------------


htn_id2 <- htn_df2[!is.na(SEX_3rd) &
                     !is.na(AGE_3rd) &
                     !is.na(FHx_HTN2) &
                     !is.na(SBP_3rd) &
                     !is.na(DBP_3rd) &
                     !is.na(SMOKING_3rd)&
                     # !is.na(DRINKING_3rd)&
                     !is.na(MET_CAT_3rd) &
                     !is.na(PROTE_URIA2) &
                     !is.na(BLDS_3rd) &
                     BMI_GROUP2!='etc'&
                     OBS_DURATION2>=0
                   ,PERSON_ID]

htn_df2[PERSON_ID %in% htn_id2,.(mean(OBS_DURATION2_Y), sd(OBS_DURATION2_Y))]

# mean t2e
htn_df2[!is.na(T2E) & PERSON_ID %in% htn_id2,.(mean(T2E)/365, sd(T2E)/365)]

table(htn_df[PERSON_ID %in% htn_id]$HTN_DIAG_AGE)

htn_df2[PERSON_ID %in% htn_id2,.(n=.N,
                                 event = sum(HTN_YN),
                                 duration = sum(OBS_DURATION2_Y),
                                 rate = sum(as.numeric(HTN_YN))/sum(OBS_DURATION2_Y)*1000), by=BMI_GROUP2][order(BMI_GROUP2)]

# cox regerssion
# 1. model 1: only BMI_GRORP
fit1 <- coxph(Surv(OBS_DURATION2, HTN_YN==1) ~ BMI_GROUP2,
              data=htn_df2[PERSON_ID %in% htn_id2])
# 2. model 2: + sex, age
fit2 <- coxph(Surv(OBS_DURATION2, HTN_YN==1) ~ BMI_GROUP2+ SEX_3rd + AGE_3rd,
              data=htn_df2[PERSON_ID %in% htn_id2])

#3. model 3: + all
fit3 <- coxph(Surv(OBS_DURATION2, HTN_YN==1) ~ 
                BMI_GROUP2+ 
                SEX_3rd + 
                AGE_3rd + 
                BMI_3rd+ 
                PROTE_URIA2+
                FHx_HTN2+
                BLDS_3rd +
                SBP_3rd + DBP_3rd+
                SMK2+
                # DRINKING_3rd+
                EXC2,
              data= htn_df2[PERSON_ID %in% htn_id2])

createTable3(mainVar = 'BMI_GROUP2', fit1, fit2, fit3, data=htn_df2[PERSON_ID %in% htn_id2])

summary(fit3)

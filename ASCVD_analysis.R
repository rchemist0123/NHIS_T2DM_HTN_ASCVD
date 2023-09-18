

# table 1. baseline characteristics ---------------------------------------
ascvd_ids <- ascvd_df[!is.na(SEX_2nd) &
                       !is.na(AGE_2nd)&
                       !is.na(BMI_GROUP) &
                       !is.na(BLDS_2nd) &
                       !is.na(SBP_2nd) &
                       !is.na(INCOME_4_2nd) &
                       !is.na(DBP_2nd)&
                       !is.na(PROTE_URIA) &
                       !is.na(TOT_CHOLE_2nd) &
                       # !is.na(CCI_ORG_CAT) &
                       !is.na(CCI_WT_CAT)&
                        !is.na(SMOKING_2nd) &
                       OBS_DURATION>365,
                     PERSON_ID]
ascvd_ids
ascvd_df[OBS_DURATION<=365]
ascvd_ids2 <- ascvd_df2[!is.na(SEX_3rd) &
                        !is.na(AGE_3rd)&
                        !is.na(BMI_GROUP2) &
                        !is.na(BLDS_3rd) &
                        !is.na(SBP_3rd) &
                        !is.na(INCOME_4_3rd) &
                        !is.na(DBP_3rd)&
                        !is.na(PROTE_URIA2) &
                        !is.na(TOT_CHOLE_3rd) &
                        # !is.na(CCI_ORG_CAT) &
                        !is.na(CCI_WT_CAT2)&
                        !is.na(SMOKING_3rd) &
                        OBS_DURATION2>365,
                      PERSON_ID]


vars <- c('AGE_2nd', 'SEX_2nd','INCOME_4_2nd', 'BLDS_CAT','SBP_CAT','DBP_CAT','PROTE_URIA','CHOLE_CAT','SMOKING_2nd', 'CCI_WT_CAT')
catVars <- c('SEX_2nd','INCOME_4_2nd','BLDS_CAT','PROTE_URIA','SBP_CAT','DBP_CAT', 'CHOLE_CAT','SMOKING_2nd','CCI_WT_CAT')
vars <- c('AGE', 'SEX','BMI', 'INCOME_4', 'BLDS_CAT1','SBP_CAT1','DBP_CAT1','PROTE_URIA1',
          'CHOLE_CAT1','SMOKING', 'CCI_WT_CAT1')
catVars <- c('SEX','INCOME_4','BLDS_CAT1','PROTE_URIA1','SBP_CAT1','DBP_CAT1', 'CHOLE_CAT1','SMOKING','CCI_WT_CAT1')

tableone::CreateTableOne(vars=vars,
                         factorVars = catVars,
                         strata='ASCVD_YN',
                         data=ascvd_df[PERSON_ID %in% ascvd_ids])

ascvd_df[PERSON_ID %in% ascvd_ids]
# table 2. cox regression table ---------------------------------------------------


fit1<- coxph(Surv(OBS_DURATION, ASCVD_YN==1) ~ BMI_GROUP_NEW_2nd+ SEX_2nd + AGE_2nd+
               INCOME_4_2nd + BLDS_CAT + SBP_CAT + DBP_CAT+
               PROTE_URIA + CHOLE_CAT +
               SMOKING_2nd + CCI_WT_CAT,
             data=ascvd_df[PERSON_ID %in% ascvd_ids & BMI_GROUP_NEW==0])

table(ascvd_df$BMI_GROUP_NEW)
ascvd_df[,BMI_GROUP_NEW_2nd:= factor(BMI_GROUP_NEW_2nd, levels = c('0','1','2'))]
ascvd_df[PERSON_ID %in% ascvd_ids & BMI_GROUP_NEW==0,.(n=.N,
                                    event = sum(ASCVD_YN),
                                    duration = sum(OBS_DURATION_Y),
                                    rate = sum(as.numeric(ASCVD_YN)) /
                                      sum(OBS_DURATION_Y) * 1000), by = BMI_GROUP_NEW_2nd][order(BMI_GROUP_NEW_2nd)]

ascvd_df[,BMI_GROUP_NEW_2nd:= factor(BMI_GROUP_NEW_2nd, levels = c('1','0','2'))]
ascvd_df[PERSON_ID %in% ascvd_ids & BMI_GROUP_NEW==1,.(n=.N,
                                                       event = sum(ASCVD_YN),
                                                       duration = sum(OBS_DURATION_Y),
                                                       rate = sum(as.numeric(ASCVD_YN)) /
                                                         sum(OBS_DURATION_Y) * 1000), by = BMI_GROUP_NEW_2nd][order(BMI_GROUP_NEW_2nd)]

ascvd_df[,BMI_GROUP_NEW_2nd:= factor(BMI_GROUP_NEW_2nd, levels = c('2','0','1'))]
ascvd_df[PERSON_ID %in% ascvd_ids & BMI_GROUP_NEW==2,.(n=.N,
                                                       event = sum(ASCVD_YN),
                                                       duration = sum(OBS_DURATION_Y),
                                                       rate = sum(as.numeric(ASCVD_YN)) /
                                                         sum(OBS_DURATION_Y) * 1000), by = BMI_GROUP_NEW_2nd][order(BMI_GROUP_NEW_2nd)]

ascvd_df[,BMI_GROUP_NEW_2nd:= factor(BMI_GROUP_NEW_2nd, levels = c('2','0','1'))]
fit1 <- coxph(Surv(OBS_DURATION_Y, ASCVD_YN==1) ~ BMI_GROUP_NEW_2nd, data=ascvd_df[PERSON_ID %in% ascvd_ids & BMI_GROUP_NEW==0])
fit2 <- coxph(Surv(OBS_DURATION_Y, ASCVD_YN==1) ~ BMI_GROUP_NEW_2nd + SEX_2nd+ AGE_2nd, data=ascvd_df[PERSON_ID %in% ascvd_ids & BMI_GROUP_NEW==0])

fit3<- coxph(Surv(OBS_DURATION, ASCVD_YN==1) ~ BMI_GROUP_NEW_2nd+ SEX_2nd + AGE_2nd+
               INCOME_4_2nd + BLDS_CAT + SBP_CAT + DBP_CAT+
               PROTE_URIA + CHOLE_CAT +
               SMOKING_2nd + CCI_WT_CAT,
             data=ascvd_df[PERSON_ID %in% ascvd_ids & BMI_GROUP_NEW==0])

createTable3(mainVar = 'BMI_GROUP_NEW_2nd',fit1, fit2, fit3, data=ascvd_df[PERSON_ID %in% ascvd_ids & BMI_GROUP_NEW==0])
multiRegression(fit3)
# table 3. multivariate cox regression ---------------------------------

ascvd_df[PERSON_ID %in% ascvd_ids,.(n=.N,
                                    event = sum(ASCVD_YN),
                                    duration = sum(OBS_DURATION_Y),
                                    rate = sum(as.numeric(ASCVD_YN)) /
                                      sum(OBS_DURATION_Y) * 1000), by = BMI_GROUP][order(BMI_GROUP)]

fit1 <- coxph(Surv(OBS_DURATION, ASCVD_YN==1) ~ BMI_GROUP,
              data=ascvd_df[PERSON_ID %in% ascvd_ids])

fit2 <- coxph(Surv(OBS_DURATION, ASCVD_YN==1) ~ BMI_GROUP+ SEX_2nd + AGE_2nd,
              data=ascvd_df[PERSON_ID %in% ascvd_ids])

fit3 <- coxph(Surv(OBS_DURATION, ASCVD_YN==1) ~ BMI_GROUP+ SEX_2nd + AGE_2nd+
                INCOME_4_2nd + BLDS_CAT + SBP_CAT + DBP_CAT+
                PROTE_URIA + CHOLE_CAT +
                SMOKING_2nd + CCI_WT_CAT,
              data=ascvd_df[PERSON_ID %in% ascvd_ids]) 
createTable3(mainVar='BMI_GROUP',fit1, fit2, fit3, data=ascvd_df2[PERSON_ID %in% ascvd_ids])

summary(fit3)

ascvd_df2[PERSON_ID %in% ascvd_id,.(n=.N,
              event = sum(ASCVD_YN),
              duration = sum(OBS_DURATION_Y),
              rate = sum(as.numeric(ASCVD_YN)) /
                sum(OBS_DURATION_Y) * 1000), by = BMI_GROUP][order(BMI_GROUP)]

require(car)
car::vif(fit3)



# 추가 그룹 모델 ----------------------------------------------------------------
ascvd_df[,BMI_GROUP_NEW_2nd:= factor(BMI_GROUP_NEW_2nd, levels = c('0','1','2'))]

fit1<- coxph(Surv(OBS_DURATION, ASCVD_YN==1) ~ BMI_GROUP_NEW_2nd+ SEX_2nd + AGE_2nd+
                INCOME_4_2nd + BLDS_CAT + SBP_CAT + DBP_CAT+
                PROTE_URIA + CHOLE_CAT +
                SMOKING_2nd + CCI_WT_CAT,
              data=ascvd_df[PERSON_ID %in% ascvd_ids & BMI_GROUP_NEW==0])

fit1
ascvd_df[,BMI_GROUP_NEW_2nd:= factor(BMI_GROUP_NEW_2nd, levels = c('1','0','2'))]
fit2<- coxph(Surv(OBS_DURATION, ASCVD_YN==1) ~ BMI_GROUP_NEW_2nd+ SEX_2nd + AGE_2nd+
               INCOME_4_2nd + BLDS_CAT + SBP_CAT + DBP_CAT+
               PROTE_URIA + CHOLE_CAT +
               SMOKING_2nd + CCI_WT_CAT,
             data=ascvd_df[PERSON_ID %in% ascvd_ids & BMI_GROUP_NEW==1])

fit2
ascvd_df[,BMI_GROUP_NEW_2nd:= factor(BMI_GROUP_NEW_2nd, levels = c('2','0','1'))]
fit3<- coxph(Surv(OBS_DURATION, ASCVD_YN==1) ~ BMI_GROUP_NEW_2nd+ SEX_2nd + AGE_2nd+
               INCOME_4_2nd + BLDS_CAT + SBP_CAT + DBP_CAT+
               PROTE_URIA + CHOLE_CAT +
               SMOKING_2nd + CCI_WT_CAT,
             data=ascvd_df[PERSON_ID %in% ascvd_ids & BMI_GROUP_NEW==2])


createTable3(mainVar = 'BMI_GROUP_NEW_2nd', fit1,fit2,fit3, ascvd_df[PERSON_ID %in% ascvd_ids & BMI_GROUP_NEW==2])
temp1 <- multiRegression(fit1)
temp2 <- multiRegression(fit2)
temp3 <- multiRegression(fit3)
rbind(temp1[1:2],temp2[1:2], temp3[1:2])
setnames(temp,'rn','variables')

# 3rd checkup
ascvd_df2[,BMI_GROUP_NEW_CAT2:= relevel(BMI_GROUP_NEW_CAT2, ref='MOMO_3')]
coxph(Surv(OBS_DURATION2, ASCVD_YN==1) ~ BMI_GROUP_NEW_CAT2+ SEX_3rd + AGE_3rd+
               INCOME_4_3rd + BLDS_CAT2 + SBP_CAT2 + DBP_CAT2+
               PROTE_URIA2 + CHOLE_CAT2 +
               SMOKING_3rd + CCI_WT_CAT2,
             data=ascvd_df2[PERSON_ID %in% ascvd_ids2 & BMI_GROUP_NEW==2]) %>% multiRegression()


table(ascvd_df2$BMI_GROUP_NEW_CAT2)

ascvd_df2[,.N, by=.(BMI_GROUP_NEW_CAT2,ASCVD_YN)][order(BMI_GROUP_NEW_CAT2)]

# 기타 ----------------------------------------------------------------------

ascvd_df[,.(mean(TOT_CHOLE_2nd, na.rm=T), sd(TOT_CHOLE_2nd, na.rm=T)),by=ASCVD_YN]

with(ascvd_df,
     t.test(TOT_CHOLE_2nd ~ ASCVD_YN))
# histogram ---------------------------------------------------------------

ascvd_df[PERSON_ID %in% ascvd_id] %>% 
  ggplot(aes(x=TOT_CHOLE_2nd))+
  geom_histogram()+
  theme_bw()+
  labs(x='Cholesterol at second checkup')


# sankey ------------------------------------------------------------------
require(ggsankey)
require(alluvial)
ascvd_table_df2 <- bmi_after_52[ascvd_table_df, on='PERSON_ID']
ascvd_table_df2[,`:=`(
  OBESE_BEFORE_50 = ifelse(BMI_BEFORE_50<25,'Normal','Obese'),
  OBESE_52 = ifelse(BMI<25,'Normal','Obese'),
  OBESE_AFTER_50 = ifelse(BMI_AFTER_52<25,'Normal','Obese')
)]

ascvd_table_df2 %>% 
  filter(!is.na(OBESE_52) & !is.na(OBESE_AFTER_50) & !is.na(OBESE_BEFORE_50)) %>% 
  group_by(OBESE_BEFORE_50, OBESE_52, OBESE_AFTER_50) %>% 
  summarise(n=n())
ascvd_checkup_eachone[,.(PERSON_ID,AGE,BMI,OBESE)]
ascvd_checkup_eachone[,OBESE:=ifelse(BMI>25,'Obese','Normal')]
ascvd_obese <- ascvd_table_df2[!is.na(OBESE_52) & !is.na(OBESE_AFTER_50) & !is.na(OBESE_BEFORE_50),.N, by=c('OBESE_BEFORE_50','OBESE_52','OBESE_AFTER_50')]
ascvd_obese[order(OBESE_BEFORE_50, OBESE_52)]

prop.table(table(ascvd_table_df2[!is.na(OBESE_52) & !is.na(OBESE_AFTER_50) & !is.na(OBESE_BEFORE_50)]$OBESE_AFTER_50))

ascvd_allu <- ascvd_checkup_eachone[PERSON_ID %in% ascvd_table_df2$PERSON_ID,.(PERSON_ID,AGE,BMI, CHECKUP_DATE)]
a <- ascvd_allu[AGE<50,.(first_bmi=last(BMI), age = last(AGE), first_bmi_date = last(CHECKUP_DATE)), by=PERSON_ID]
a
a[,obese_before_50:=ifelse(first_bmi>=25,'Obese','Normal')]
b <- ascvd_allu[AGE>=52, .(bmi_52=first(BMI), first_bmi_date_52 = first(CHECKUP_DATE)),by=PERSON_ID]
b[,obese_52:=ifelse(bmi_52>=25,'Obese','Normal')]
c <- ascvd_allu[ascvd_allu[AGE>=52,.I[2L],by=PERSON_ID]$V1][,.(PERSON_ID,bmi_after_52=BMI, bmi_date_after_52=CHECKUP_DATE)]
c
c[,obese_after_52:=ifelse(bmi_after_52>=25,'Obese','Normal')]
ab <- merge(a,b, by = 'PERSON_ID')
abc <- merge(ab,c, by='PERSON_ID')
abc
smry <- abc[,.(gap1 = as.numeric(difftime(first_bmi_date_52, first_bmi_date, units='days')),
               gap2 = as.numeric(difftime(bmi_date_after_52, first_bmi_date_52, units='days')))]
smry[,.(mean(gap1)/365, sd(gap1)/365, mean(gap2)/365, sd(gap2)/365)]

abc[,flow:=paste0(substr(obese_before_50,1,1),substr(obese_52,1,1), substr(obese_after_52,1,1), collapse = '-'), by=PERSON_ID]
abc[!is.na(obese_before_50) & !is.na(obese_52) & !is.na(obese_after_52),.N, by=flow]
melt(abc, id.vars = 'PERSON_ID', m)

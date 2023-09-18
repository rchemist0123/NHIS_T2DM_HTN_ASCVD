
# sankey plot -------------------------------------------------------------

htn_df2[,.N, by=.(obese_1,obese_2,obese_3)][order(obese_1, obese_2, obese_3)]
htn_df2[,.(mean(OBS_DURATION_Y),sd(OBS_DURATION_Y),
           mean(OBS_DURATION2_Y), sd(OBS_DURATION2_Y))]

prop.table(table(htn_df2$obese_1))


# KM plot -----------------------------------------------------------------

fit1 <- survfit(Surv(OBS_DURATION_Y, HTN_YN==1) ~ BMI_GROUP, data=htn_df %>% filter(PERSON_ID %in% htn_id))

summary(fit1,times=c(4,5,6))

km1 <- ggsurvplot(fit1,
           censor=F,
           size=.7,
           fun='event',
           # pval=T,
           risk.table = T,
           legend=c(0.2,0.7))

# Survival probability: S(t) = S(t-1)*(1-(No. of Event)/(No. of risk))

pairwise_survdiff(Surv(OBS_DURATION_Y, HTN_YN==1) ~ BMI_GROUP, data=htn_df %>% filter(PERSON_ID %in% htn_id))


km1$plot+theme(axis.text=element_text(family='serif'),
               axis.title = element_text(family='serif'))

km1$table

pairwise_survdiff(Surv(OBS_DURATION_Y, HTN_YN==1) ~ BMI_GROUP,data=htn_df %>% filter(BMI_GROUP2 !='etc' & OBS_DURATION2>=0))


htn_id2

htn_df2[PERSON_ID %in% htn_id2,.N,by=.(BMI_GROUP2, HTN_YN)][order(BMI_GROUP2)]
htn_df2[PERSON_ID %in% htn_id2]
fit2 <- survfit(Surv(OBS_DURATION2_Y, HTN_YN==1) ~ BMI_GROUP2, data=htn_df2 %>% filter(PERSON_ID %in% htn_id2))

require(survminer)

htn_df2[PERSON_ID %in% htn_id2,.(mean(OBS_DURATION2_Y),
           min(OBS_DURATION2_Y),
           max(OBS_DURATION2_Y)),by=BMI_GROUP2]

km2 <- ggsurvplot(fit2,
           censor=F,
           size=.7,
           fun='event',
           pval=T,
           xlim=c(0,8),
           risk.table = T,
           legend=c(0.3,0.8))
km2$plot
Skm2$table$data
km2$plot+theme(axis.text=element_text(family='serif'),
               axis.title = element_text(family='serif'))
pairwise_survdiff(Surv(OBS_DURATION2_Y, HTN_YN==1) ~ BMI_GROUP2,data=htn_df2 %>% filter(BMI_GROUP2 !='etc' & OBS_DURATION2>=0))

summary(fit2)
summary(fit2,times=seq(0,7,0.1))

seq(0,7,0.1)

# forest plot -------------------------------------------------------------

coxph(Surv(OBS_DURATION, HTN_YN==1) ~  BMI_GROUP+ 
        SEX_2nd + 
        AGE_2nd + 
        BMI_2nd+ 
        BLDS_2nd +
        SBP_2nd + DBP_2nd +
        FHx_HTN+
        PROTE_URIA +
        SMOKING_2nd+
        MET_CAT_2nd,
      data=htn_df[PERSON_ID %in% htn_id ]) %>% summary()






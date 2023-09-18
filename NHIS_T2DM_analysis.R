
# baseline table ----------------------------------------------------------

t2dm_ids <- t2dm_df[!is.na(AGE) &
          !is.na(SEX_2nd) &
          !is.na(BMI_2nd) &
          !is.na(BMI_GROUP) &
          !is.na(BLDS_2nd) &
          !is.na(IFG) &
          !is.na(HPTN_YN) &
          !is.na(FHx_T2DM) &
          !is.na(SMOKING_2nd) &
          OBS_DURATION>0, PERSON_ID]


t2dm_ids2 <- t2dm_df2[!is.na(AGE_3rd) &
                      !is.na(SEX_3rd) &
                      !is.na(BMI_3rd) &
                      !is.na(BMI_GROUP2) &
                      !is.na(BLDS_3rd) &
                      !is.na(IFG2) &
                      !is.na(HPTN_YN2) &
                      !is.na(FHx_T2DM2) &
                      !is.na(SMOKING_3rd) &
                      OBS_DURATION2>0, PERSON_ID]

mytable(BMI_GROUP ~ MET_CAT, t2dm_df)
require(tableone)
allVars = c('SEX_2nd','AGE_2nd','BMI_2nd','FHx_T2DM','BLDS_2nd','IFG','TOT_CHOLE_2nd','HPTN_YN','PROTE_URIA','SMOKING_2nd')
catVars = c('SEX_2nd','IFG_2nd','PROTE_URIA','HPTN_YN','FHx_T2DM','SMOKING_2nd')
allVars = c('SEX','AGE','BMI','FMLY_DIABML_PATIEN_YN','BLDS','IFG1','TOT_CHOLE','HPTN_YN1',
            'PROTE_URIA1','SMOKING','MET_CAT')
catVars = c('SEX','IFG1','PROTE_URIA1','HPTN_YN1','FMLY_DIABML_PATIEN_YN','SMOKING')
t2dm_df[,(allVars)]
CreateTableOne(vars=allVars,
               factorVars=catVars, 
               strata = 'BMI_GROUP',
               data=t2dm_df[PERSON_ID %in% t2dm_ids])

t2dm_df[!is.na(SEX) & !is.na(AGE) &
                    !is.na(SEX) &
                    !is.na(BMI) &
                    !is.na(BMI_GROUP) &
                    !is.na(BLDS) &
                    !is.na(IFG) &
                    !is.na(HPTN_YN) &
                    !is.na(FMLY_DIABML_PATIEN_YN)] %>% dim()

t2dm_df[OBS_DURATION>0 & !is.na(AGE) &
          !is.na(SEX_2nd) &
          !is.na(BMI_2nd) &
          !is.na(BMI_GROUP)&
          !is.na(BLDS_2nd) &
          !is.na(IFG) &
          !is.na(HPTN_YN) &
          !is.na(FMLY_DIABML_PATIEN_YN)] %>% dim()

# PSM ----------------------------------------------------------------

require(MatchIt)
require(ggplot2)


# B, D matching
fit_uni <- glm(T2DM_YN ~ BMI, family = binomial, data=t2dm_df[BMI_GROUP=='B'|BMI_GROUP=='D'])
fit <- glm(T2DM_YN ~ BMI + SEX + MET_CAT + IFG + SMOKING, family = binomial, t2dm_df[BMI_GROUP=='B'|BMI_GROUP=='D'])


summary(fit)
score.table <- data.table(ps=predict(fit, type='response'), T2DM_YN=fit$model$T2DM_YN)
score.table %>% ggplot(aes(x=ps, fill=T2DM_YN))+
  geom_histogram(color='white',position='dodge', bins=30)+
  ylim(c(0,1000))

set.seed(7795)

require(data.table)
t2dm_df$OBS_DURATION2
bmi_bd <- t2dm_df[BMI_GROUP=='B'|BMI_GROUP=='D',.(SEX_2nd,AGE_2nd, BMI_2nd, BLDS_2nd,IFG, HPTN_YN,T2DM_YN, SMOKING_2nd, BMI_GROUP, PROTE_URIA,TOT_CHOLE_2nd,PROTE_URIA, OBS_DURATION, OBS_DURATION_Y, FHx_T2DM )][OBS_DURATION_Y>0]
bmi_bd <- na.omit(bmi_bd)
bmi_bd$BMI_GROUP <- as.character(bmi_bd$BMI_GROUP)
bmi_bd[,group:=ifelse(BMI_GROUP=='B',T,
                      ifelse(BMI_GROUP=='D',F,NA))]

table(bmi_bd$BMI_GROUP)

tab_bd <- CreateTableOne(vars=c('AGE_2nd','SEX_2nd','BMI_2nd','BLDS_2nd','IFG','TOT_CHOLE_2nd', 'PROTE_URIA', 'HPTN_YN','FHx_T2DM', 'SMOKING_2nd','OBS_DURATION2_Y'),
                       data=bmi_bd,
                       factorVars = c('SEX','IFG','HPTN_YN','FHx_T2DM', 'SMOKING_2nd'),
                       strata = 'group',
                       smd=TRUE)

x1 <- print(tab_bd, printToggle = F, noSpaces = F, smd=T, quote=T)
x1

bmi_ac <- t2dm_df[BMI_GROUP=='A'|BMI_GROUP=='C',.(SEX_2nd,AGE_2nd, BLDS_2nd,BMI_2nd, IFG, HPTN_YN,T2DM_YN, SMOKING_2nd, BMI_GROUP, PROTE_URIA,TOT_CHOLE_2nd,  PROTE_URIA,OBS_DURATION, OBS_DURATION_Y, FHx_T2DM )][OBS_DURATION_Y>0]
bmi_ac <- na.omit(bmi_ac)
bmi_ac[,group:=ifelse(BMI_GROUP=='A',F,
                      ifelse(BMI_GROUP=='C',T,NA))]

tab_ac <- CreateTableOne(vars=c('AGE_2nd','SEX_2nd','BMI_2nd','BLDS_2nd','IFG','TOT_CHOLE_2nd', 'PROTE_URIA', 'HPTN_YN','FHx_T2DM', 'SMOKING_2nd','OBS_DURATION2_Y'),
                         data=bmi_ac,
                         factorVars = c('SEX','IFG','HPTN_YN','FHx_T2DM', 'SMOKING_2nd'),
                         strata = 'group',
                         smd=TRUE)
x2 <- print(tab_ac, printToggle = F, noSpaces = F, smd=T, quote=T)
x2
table(bmi_ac$BMI_GROUP)
bmi_bd[,.(mean(BMI),median(BMI),sd(BMI)),by=group]
bmi_ac[,.(mean(BMI),median(BMI),sd(BMI)),by=group]

set.seed(7795)
options("optmatch_max_problem_size"=Inf)
m1 <- matchit(group ~ BMI_2nd,
              data=bmi_bd,
              method="optimal",
              ratio=1)
m2 <- matchit(group ~ BMI_2nd,
              data=bmi_ac,
              method="optimal",
              ratio=1)
match_data1 <- match.data(m1)
match_data2 <- match.data(m2)
match_bd <- CreateTableOne(vars=c('AGE_2nd','SEX_2nd','BMI_2nd','BLDS_2nd','IFG','TOT_CHOLE_2nd', 'PROTE_URIA', 'HPTN_YN','FHx_T2DM', 'SMOKING_2nd','OBS_DURATION2_Y'),
                         data=match_data1,
                         factorVars = c('SEX','IFG','HPTN_YN','FHx_T2DM', 'SMOKING_2nd'),
                         strata = 'group',
                         smd=TRUE)

match_ac <- CreateTableOne(vars=c('AGE_2nd','SEX_2nd','BMI_2nd','BLDS_2nd','IFG','TOT_CHOLE_2nd', 'PROTE_URIA', 'HPTN_YN','FHx_T2DM', 'SMOKING_2nd','OBS_DURATION2_Y'),
                           data=match_data2,
                           factorVars = c('SEX','IFG','HPTN_YN','FHx_T2DM', 'SMOKING_2nd'),
                           strata = 'group',
                           smd=TRUE)

print(match_ac,smd=T)
match_data2 %>% group_by(group,weights) %>%
  tally() %>% 
  group_by(group) %>% 
  mutate(weight.ratio = weights/min(weights))
# summarise(min.weight = min(weights),
#           max.weight = max(weights),
#           mean.weight = min(weights),
#           sum.weights = sum(weights),
#           max.match.ratio = max.weight / min.weight)

tab2 <- CreateTableOne(vars=c('AGE','SEX', 'BMI','FMLY_DIABML_PATIEN_YN','BLDS','IFG','TOT_CHOLE', 'HPTN_YN','PROTE_URIA', 'SMOKING','DRINKING','OBS_DURATION2_Y'),
                       data=match_data1, 
                       factorVars = c('SEX','IFG','HPTN_YN','SMOKING','DRINKING'),
                       strata = 'group',
                       smd=TRUE)
x2 <- print(tab2, printToggle = F, noSpaces = F, smd=T, quote=T)
bal.tab(m2, thresholds = 0.1)
love.plot(m2, thresholds = 0.1)
fit <- survfit(Surv(OBS_DURATION2_Y, T2DM_YN==1) ~ BMI_GROUP, data=match_data2)
plot <- ggsurvplot(fit, 
                   # pval=T,
                   fun='event',
                   # pval.size=4,
                   ylim=c(0,0.7),
                   size=0.5, # line size
                   legend = 'none',
                   # legend.title="BMI Group",
                   # legend.labs=c('BMI Increase', 'BMI High'),
                   # xlab="Years",
                   palette = c("black", "red"),
                   break.time.by=1,
                   censor=F,
                   # table
                   risk.table=T,
                   fontsize=3,
                   # font.family='serif',
                   tables.height = 0.3)

plot$plot <-  plot$plot + 
  scale_x_continuous(breaks=c(0,1,4,6,10))+
  scale_y_continuous(breaks=c(0.0,0.2,0.4,0.6),limits = c(0,0.6))+
  theme(text= element_text(family = 'serif', size=3),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.line.x = element_line(size=.3),
        axis.line.y = element_line(size=.3, lineend = 'butt'))
# geom_segment(x=0, xend=0, y=0.6, yend=0.6)
plot$plot
plot$table +scale_x_continuous(breaks=c(0,1,4,6,10))
plot
table(t2dm_df$T2DM_YN)


# 매칭 이전
mytable(BMI_GROUP ~ T2DM_YN+SEX+ AGE+ BMI+ BLDS + IFG + HPTN_YN+ SMOKING+ DRINKING + OBS_DURATION2_Y + TOT_CHOLE, bmi_bd)
bmi_bd$BL

histbackback(split(bmi_bd$psvalue, bmi_bd$group), xlab=c('D','B'))
set.seed(7795)

require(tableone)
tab1 <- CreateTableOne(vars=c('AGE','SEX','BMI','FMLY_DIABML_PATIEN_YN','BLDS','IFG','TOT_CHOLE', 'HPTN_YN','PROTE_URIA', 'SMOKING','DRINKING','OBS_DURATION2_Y'),
                       data=bmi_bd, 
                       factorVars = c('SEX','IFG','HPTN_YN','SMOKING','DRINKING'),
                       strata = 'BMI_GROUP',
                       smd=TRUE)
x1 <- print(tab1, printToggle = F, noSpaces = F, smd=T, quote=T)
x1
set.seed(7795)
#matching
m <- matchit(group ~ BMI,
             data=bmi_bd ,
             method="optimal", ratio=1)
match_data <- match.data(m)
tab2 <- CreateTableOne(vars=c('AGE','SEX','BMI','FMLY_DIABML_PATIEN_YN','BLDS','IFG','TOT_CHOLE', 'HPTN_YN','PROTE_URIA', 'SMOKING','DRINKING','OBS_DURATION2_Y'),
                       data=match_data, 
                       factorVars = c('SEX','IFG','HPTN_YN','SMOKING','DRINKING'),
                       strata = 'BMI_GROUP',
                       smd=TRUE)
x2 <- print(tab2, printToggle = F, noSpaces = F, smd=T, quote=T)
x2
# after matching
mytable(BMI_GROUP ~ T2DM_YN+SEX+ AGE+ BMI+ BLDS + IFG + HPTN_YN+ SMOKING+ DRINKING + OBS_DURATION2_Y + TOT_CHOLE, match_data)

histbackback(split(match_data$psvalue, match_data$group), xlab=c('Group D','Group B'))

fit <- glm(T2DM_YN ~ SEX+ AGE+ BMI+ BMI_GROUP+ IFG +  HPTN_YN+ SMOKING+  DRINKING,
           family=binomial, match_data)
match_data$BMI_GROUP <- as.factor(match_data$BMI_GROUP)
require(cobalt)
bal.tab(m, thresholds = 0.1)

# KM PLOT
require(survival)
require(survminer)
fit <- survfit(Surv(OBS_DURATION2_Y, T2DM_YN==1) ~ BMI_GROUP, data=match_data)

plot <- ggsurvplot(fit, 
                   # pval=T,
                   fun='event',
                   # pval.size=4,
                   ylim=c(0,0.7),
                   size=0.5, # line size
                   legend = 'none',
                   # legend.title="BMI Group",
                   # legend.labs=c('BMI Increase', 'BMI High'),
                   # xlab="Years",
                   palette = c("black", "red"),
                   break.time.by=1,
                   censor=F,
                   # table
                   risk.table=T,
                   fontsize=3,
                   # font.family='serif',
                   tables.height = 0.3)

plot$plot <-  plot$plot + 
  scale_x_continuous(breaks=c(0,1,4,6,10))+
  scale_y_continuous(breaks=c(0.0,0.2,0.4,0.6),limits = c(0,0.6))+
  theme(text= element_text(family = 'serif', size=3),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.line.x = element_line(size=.3),
        axis.line.y = element_line(size=.3, lineend = 'butt'))
# geom_segment(x=0, xend=0, y=0.6, yend=0.6)
plot$plot
plot$table +scale_x_continuous(breaks=c(0,1,4,6,10))
plot


#matching coxph
fit <- coxph(Surv(OBS_DURATION2_Y, T2DM_YN==1) ~ BMI_GROUP, data=match_data)

fit2 <- coxph(Surv(OBS_DURATION2_Y, T2DM_YN==1) ~ BMI_GROUP + SEX + AGE, data=match_data)

fit3 <- coxph(Surv(OBS_DURATION2_Y, T2DM_YN==1) ~ BMI_GROUP + SEX + AGE+
                IFG + HPTN_YN + FMLY_DIABML_PATIEN_YN + SMOKING, data=match_data)

fit4 <- coxph(Surv(OBS_DURATION2_Y, T2DM_YN==1) ~ BMI_GROUP + SEX + AGE+
                IFG + HPTN_YN + FMLY_DIABML_PATIEN_YN + SMOKING + BMI, data=match_data)

summary(fit4)
# matching 검증 -------------------------------------------------------------

require(tidyverse)
require(cobalt)
bal.plot(m, var.name='BMI', which='both')
bal.plot(m, var.name='distance', mirror=T, type='histogram')

love.plot(m, stat="mean.diffs", binary='std',
          var.order = 'unadjusted',
          thresholds=c(m=.1))

mytable(BMI_GROUP ~ BLDS, t2dm_df[SEX==1])

mytable(~T2DM_YN, t2dm_df)


# 관찰기간, 검사 사이 기간 ----------------------------------------------------------

t2dm_df[PERSON_ID %in% t2dm_ids,.(mean(OBS_DURATION_Y),sd(OBS_DURATION_Y))]
t2dm_df2[PERSON_ID %in% t2dm_ids2,.(mean(OBS_DURATION2_Y),sd(OBS_DURATION2_Y))]

t2dm_df[PERSON_ID %in% t2dm_ids, .(mean(difftime(CHECKUP_DATE_2nd,CHECKUP_DATE, units = 'days')/365),
                                   sd(difftime(CHECKUP_DATE_2nd,CHECKUP_DATE, units = 'days')/365))]
t2dm_df2[PERSON_ID %in% t2dm_ids2, .(mean(difftime(CHECKUP_DATE_3rd,CHECKUP_DATE_2nd, units = 'days')/365),
                                   sd(difftime(CHECKUP_DATE_3rd,CHECKUP_DATE_2nd, units = 'days')/365))]


# posthoc -----------------------------------------------------------------

with(t2dm_df,
  pairwise.t.test(AGE_2nd, BMI_GROUP),  p.adjust.method = "bonf")


# COX ----------------------------------------------------------------
table(t2dm_df$DRINKING)
t2dm_df[,DRINKING_BIN := ifelse(DRINKING=='none','No','Yes')]

t2dm_df[PERSON_ID %in% t2dm_ids][,.(n=.N,
                                             event = sum(T2DM_YN),
                                             duration = sum(OBS_DURATION_Y),
                                             rate = sum(as.numeric(T2DM_YN))/sum(OBS_DURATION_Y)*1000), by=BMI_GROUP][order(BMI_GROUP)]

#1. unadjusted
fit1 <- coxph(Surv(OBS_DURATION, T2DM_YN==1) ~ 
        BMI_GROUP,data=t2dm_df[PERSON_ID %in% t2dm_ids])


#2. sex, age
fit2 <- coxph(Surv(OBS_DURATION, T2DM_YN==1) ~ 
                BMI_GROUP + SEX_2nd + AGE_2nd,t2dm_df[PERSON_ID %in% t2dm_ids])

#3. FHx, smoking, drinking, IFG, BLDS
fit3 <- coxph(Surv(OBS_DURATION, T2DM_YN==1) ~ 
                BMI_GROUP + SEX_2nd + AGE_2nd + BMI_2nd+ IFG + HPTN_YN + FHx_T2DM+ SMOKING_2nd,
              data=t2dm_df[PERSON_ID %in% t2dm_ids])

#4. 3 + BMI
fit4 <- coxph(Surv(OBS_DURATION, T2DM_YN==1) ~ 
                BMI_GROUP + SEX + AGE + BMI+IFG + HPTN_YN + FMLY_DIABML_PATIEN_YN+ SMOKING,
              data=t2dm_df[PERSON_ID %in% t2dm_ids])


coxph(Surv(OBS_DURATION, T2DM_YN==1) ~ BMI_GROUP + SEX_2nd + AGE_2nd + BMI_2nd + IFG + HPTN_YN + FHx_T2DM + SMOKING_2nd, t2dm_df[PERSON_ID %in% t2dm_ids]) %>% summary()
coxph(Surv(OBS_DURATION2, T2DM_YN==1) ~ BMI_GROUP2 + SEX_3rd + AGE_3rd + BMI_3rd + IFG2 + HPTN_YN2 + FHx_T2DM2 + SMOKING_3rd, t2dm_df2[PERSON_ID %in% t2dm_ids2]) %>% summary()

summary(fit3)
createTable3(mainVar = 'BMI_GROUP', fit1,fit2,fit3, data=t2dm_df[PERSON_ID %in% t2dm_ids])

with(t2dm_df[PERSON_ID %in% t2dm_ids],
     prop.table(table(BMI_GROUP)*100))


# subgroup AC, BD cox ------------------------------------------------------------

fit1 <- coxph(Surv(OBS_DURATION_Y, T2DM_YN==1) ~ BMI_GROUP, data=match_data2)
fit2 <- coxph(Surv(OBS_DURATION_Y, T2DM_YN==1) ~ BMI_GROUP + SEX_2nd + AGE_2nd, data=match_data2)
fit3 <- coxph(Surv(OBS_DURATION_Y, T2DM_YN==1) ~ BMI_GROUP + SEX_2nd + AGE_2nd + BMI_2nd + IFG + HPTN_YN + FHx_T2DM + SMOKING_2nd, data=match_data2)

summary(fit3)
createTable3(mainVar = 'BMI_GROUP', fit1, fit2, fit3, data = bmi_bd)

a <- multiRegression(fit1)
b <- multiRegression(fit3)
b
left_join(b,a, by='rn')

fit2# subgroups AB,AC,AD ---------------------------------------------------------------

require(Publish)
match_data$BMI_GROUP <-  as.factor(match_data$BMI_GROUP)
t2dm_df$SMOKING <- factor(t2dm_df$SMOKING, level=c('None','Light','Moderate','Heavy'))
t2dm_df$DRINKING <- factor(t2dm_df$DRINKING, level=c('none','light','moderate','heavy'))
coxph(Surv(OBS_DURATION2, T2DM_YN==1) ~ BMI_GROUP, t2dm_df) %>% summary()


t2dm_df$T2DM_YN <- as.numeric(t2dm_df$T2DM_YN)


sub_df <- t2dm_df[BMI_GROUP=='A' | BMI_GROUP=='D'][OBS_DURATION_Y>0]
sub_df[,T2DM_YN:= as.numeric(ifelse(T2DM_YN==1,1,0))]
sub_df[,BMI_GROUP:=as.factor(ifelse(BMI_GROUP=='A',0,1))]
cols <- c('SMOKING_2nd')
sub_df[,(cols):=lapply(.SD, as.factor),.SDcols=cols]
fit.coxph <- coxph(Surv(OBS_DURATION_Y, T2DM_YN==1) ~ 
                     BMI_GROUP + SEX_2nd + IFG + HPTN_YN + BMI_2nd+
                     FHx_T2DM + SMOKING_2nd,
                   data=sub_df)
summary(fit.coxph)
rst <- subgroupAnalysis(fit.coxph, data=sub_df, 
                 treatment = "BMI_GROUP",
                 subgroups = c('SEX_2nd','IFG','HPTN_YN','FHx_T2DM', 'SMOKING_2nd'))
setDT(rst)[,`:=`(ratio1= paste0(event_0,'/',sample_0),
                 ratio2=paste0(event_1,'/',sample_1))][,.(subgroups,ratio1,ratio2,HazardRatio,
                                                          lower=format(round(Lower,2),nsmall=2),
                                                          upper=format(round(Upper,2),nsmall=2),
                                                          pinteraction=ifelse(pinteraction<0.001,'<0.001',round(pinteraction,4)))]



# subgroup by drink -------------------------------------------------------
t2dm_df$OBS_DURATION2_Y
t2dm_df$DRINKING <- factor(t2dm_df$DRINKING, level=c('None','Light','Moderate','Heavy'))
t2dm_df$SMOKING <- factor(t2dm_df$SMOKING, level=c('None','Light','Moderate','Heavy'))
t2dm_df$SMOKING_BEFORE_50 <- factor(t2dm_df$SMOKING_BEFORE_50, level=c('None','Light','Moderate','Heavy'))
t2dm_df$DRINKING_BEFORE_50 <- factor(t2dm_df$DRINKING_BEFORE_50, level=c('None','Light','Moderate','Heavy'))


t2dm_df$SMOKING <- relevel(t2dm_df$SMOKING, ref='None')
coxph(Surv(OBS_DURATION2_Y, T2DM_YN==1) ~ SMOKING, data=t2dm_df[SMOKING_BEFORE_50=='None'])


mytable(MET_CAT ~ MET_BEFORE_50 ,t2dm_df)

# subgroup by MET_CAT -----------------------------------------------------


require(twang)
require(survey)

met_df <- t2dm_df[,.(SEX,AGE,FMLY_DIABML_PATIEN_YN,T2DM_YN, OBS_DURATION2_Y, MET_CAT, MET_BEFORE_50)] %>% as.data.frame()
met_df <- met_df %>% na.omit() %>% as.data.frame()

met_df$MET_CAT <- factor(met_df$MET_CAT, level=c('None','Low','Middle','High'))
met_df_none <- met_df %>% filter(MET_BEFORE_50=='None')
met_df_low <- met_df %>% filter(MET_BEFORE_50=='Low')
met_df_middle <- met_df %>% filter(MET_BEFORE_50=='Middle')
met_df_high <- met_df %>% filter(MET_BEFORE_50=='High')

met_df_low$MET_CAT <- relevel(met_df_low$MET_CAT, ref='Low')
met_df_middle$MET_CAT <- relevel(met_df_middle$MET_CAT, ref='Middle')
met_df_high$MET_CAT <- relevel(met_df_high$MET_CAT, ref='High')


x <- twang::mnps(MET_CAT ~ SEX + AGE + FMLY_DIABML_PATIEN_YN,
                 data=met_df_high,
                 estimand = 'ATE',
                 n.trees=2000,
                 verbose = F,
                 stop.method = c('es.mean','ks.mean'))

x$data
met_df_none$weight <- get.weights(x, stop.method = 'es.mean')
met_df_low$weight <- get.weights(x, stop.method = 'es.mean')
met_df_middle$weight <- get.weights(x, stop.method = 'es.mean')
met_df_high$weight <- get.weights(x, stop.method = 'es.mean')


design_met <- svydesign(ids = ~ 1, weight = ~weight, data=met_df_high)

svycoxph(formula= Surv(OBS_DURATION2_Y, T2DM_YN==1) ~ MET_CAT,
         design=design_met) %>% summary()

coxph(formula= Surv(OBS_DURATION2_Y, T2DM_YN==1) ~ MET_CAT,
      data=met_df) %>% summary()

match_data <- match.data(m)
t2dm_df$MET_CAT <- relevel(t2dm_df$MET_CAT, ref='None')
coxph(Surv(OBS_DURATION2_Y, T2DM_YN==1) ~ 
        SEX+AGE + IFG + FMLY_DIABML_PATIEN_YN+MET_CAT, 
      data=t2dm_df[MET_BEFORE_50=='None']) %>% 
  summary()

coxph(Surv(OBS_DURATION2_Y, T2DM_YN==1) ~ SEX, data=met_df)
coxph(Surv(OBS_DURATION2_Y, T2DM_YN==1) ~ SEX, data=x)

t2dm_df$OB
# group by waist ----------------------------------------------------------
t2dm_df[!is.na(DRINKING) & !is.na(SMOKING) & !is.na(MET_CAT) &
                    !is.na(FMLY_DIABML_PATIEN_YN)][BMI_GROUP=='MAO',.(n=.N,
                                                                      event = sum(T2DM_YN),
                                                                      duration = sum(OBS_DURATION2),
                                                                      rate = sum(as.numeric(T2DM_YN))/sum(OBS_DURATION2)*1000), by=WAIST_GROUP][order(WAIST_GROUP)]
t2dm_df$OBS_DURATION2
# cox regerssion
# 1. model 1: only BMI_GRORP
coxph(Surv(OBS_DURATION2, T2DM_YN==1) ~ WAIST_GROUP,
      data=t2dm_df[BMI_GROUP=='MAO']) %>% summary()

# 2. model 2: + sex, age
coxph(Surv(OBS_DURATION2, T2DM_YN==1) ~ WAIST_GROUP + SEX + AGE,
      data=t2dm_df[BMI_GROUP=='MAO']) %>% summary()

#3. model 3: + all
t2dm_df$MET_CAT <- relevel(t2dm_df$MET_CAT, ref='None')
coxph(Surv(OBS_DURATION2, T2DM_YN==1) ~ 
        SEX + AGE + BMI + TOT_CHOLE+ WAIST_GROUP+
        FMLY_DIABML_PATIEN_YN + 
        DRINKING + SMOKING + MET_CAT, data=t2dm_df[BMI_GROUP=='MAO']) %>% summary()




# subgroup: B에서 정상체중 유지? --------------------------------------------------

b_id <- t2dm_df[BMI_GROUP=='B']$PERSON_ID
b_df <- t2dm_cohort_eachone[PERSON_ID %in% b_id & AGE>=49, .(PERSON_ID, CHECK_YEAR, AGE,BMI)]
b_df2 <- dcast(b_df[AGE>52], PERSON_ID ~ AGE, value.var='BMI', 
               fill = NA, 
               fun.aggregate = function(x) x[1])
b_df3 <- b_df2[,lapply(.SD, function(x) ifelse(x>=25,1,0)), .SDcols=-'PERSON_ID']

b_df3[,`:=`(
  total=Reduce(`+`, lapply(.SD, function(x) !is.na(x))),
  obese=rowSums(.SD, na.rm=T))][,normal := total - obese]

b_df3 <- bind_cols(b_df2[,.(PERSON_ID)], b_df3[,.(total,obese,normal)])
b_df3
b_df3[normal >= obese]
b_df3[normal < obese]
b_normal <- b_df3[normal>obese]$PERSON_ID
b_table_df <- t2dm_df[BMI_GROUP=='B']
b_normal
b_table_df[,change_yn:= ifelse(PERSON_ID %in% b_normal,1,0)]
fit <-  survfit(Surv(OBS_DURATION2_Y, T2DM_YN==1) ~ change_yn, b_table_df)

table(b_table_df$change_yn)
ggsurvplot(fit,
           pval = T,
           fun = 'event',
           ylim = c(0, 0.7),
           break.x.by = 1,
           size = 0.5, # line size
           legend = 'none',
           xlab = "",
           ylab = "",
           palette = c('black','red'),
           censor = F,
           font.family = 'serif',
           risk.table = T)

cox_fit <- coxph(Surv(OBS_DURATION2_Y, T2DM_YN==1) ~ change_yn, b_table_df)
ggsurvplot(survfit(cox_fit, data=b_table_df))


# subgroup: C에서 정상체중 유지 or 증가? --------------------------------------------

c_id <- t2dm_df[BMI_GROUP=='C']$PERSON_ID
c_df <- t2dm_cohort_eachone[PERSON_ID %in% c_id, .(PERSON_ID, CHECK_YEAR, AGE,BMI)]
c_df[,BMI:=round(BMI,1)]
c_df %>% pivot_wider(id_cols=PERSON_ID,
                     names_from='CHECK_YEAR',
                     values_from='BMI')
c_df[AGE>=52 & BMI>=25, .N , by=PERSON_ID]
c_df2 <- c_df[AGE>=52, .(n=.N,
                         BMIs=paste(sort(substr(BMI,1,2)),collapse = " ")), by=PERSON_ID]
c_df2
c_df

# pivot_wider(c_df[AGE>=52], id_cols = 'PERSON_ID', names_from = 'AGE', 
#             names_prefix = 'AGE_',
#             values_from = 'BMI') %>% 
#   unnest() %>%
#   summarise(across(c(2:12), ~ as.numeric(.x>=25)))

c_df[AGE>52,.N, by=.(PERSON_ID,CHECK_YEAR)]
dcast(c_df[AGE>52], PERSON_ID ~ AGE, value.var='BMI', 
      fill = NA, 
      fun.aggregate = function(x) x[1])
c_df2 <- dcast(c_df[AGE>52], PERSON_ID ~ AGE, value.var='BMI', 
               fill = NA, 
               fun.aggregate = function(x) x[1])
c_df2[,lapply(.SD, function(x) ifelse(x>=25,1,0)),.SDcols=-'PERSON_ID']
c_df3 <- c_df2[,lapply(.SD, function(x) ifelse(x>=25,1,0)), .SDcols=-'PERSON_ID']
c_df3[,`:=`(
  total = Reduce(`+`, lapply(.SD, function(x) !is.na(x))),
  obese = rowSums(.SD, na.rm=T )
)][,normal:= total-obese]

c_df3 <- bind_cols(c_df2[,.(PERSON_ID)], c_df3[,.(total,obese,normal)])
c_df3[normal<=obese]
c_sustain <- c_df3[obese>0]$PERSON_ID
c_df3[obese>0]
c_table_df <- t2dm_df[BMI_GROUP=='C']
c_table_df[,change_yn:= ifelse(PERSON_ID %in% c_sustain,1,0)]

fit <-  survfit(Surv(OBS_DURATION2_Y, T2DM_YN==1) ~ change_yn, c_table_df)
ggsurvplot(fit,
           pval = T,
           fun = 'event',
           ylim = c(0, 0.7),
           break.x.by = 1,
           size = 0.5, # line size
           legend = 'none',
           xlab = "",
           ylab = "",
           palette = c('black','red'),
           censor = F,
           font.family = 'serif',
           risk.table = T)


# subgroup 4 --------------------------------------------------------------

sub_table_df <- t2dm_df[PERSON_ID %in% subgroup4_id]
coxfit <- coxph(Surv(OBS_DURATION2_Y, T2DM_YN==1) ~ 
                  SEX+ BMI + BMI_GROUP + IFG + FMLY_DIABML_PATIEN_YN +
                  SMOKING + DRINKING  ,
                data=sub_table_df)
coxfit2 <- coxph(Surv(OBS_DURATION2_Y, T2DM_YN==1) ~ 
                   SEX+ BMI + IFG + FMLY_DIABML_PATIEN_YN +
                   SMOKING + DRINKING  + strata(BMI_GROUP),
                 data=sub_table_df)

summary(coxfit)
vif(coxfit)
surv_fit <- survfit(Surv(OBS_DURATION2_Y, T2DM_YN==1) ~ BMI_GROUP, data=sub_table_df)
surv_fit

require(car)

ggsurvplot(surv_fit,
           fun='event',
           pval=T,
           size=.5,
           palette = c('black','green3','blue','red'),
           censor=F)
pairwise_survdiff(Surv(OBS_DURATION2_Y, T2DM_YN==1) ~ 
                    BMI_GROUP, data=sub_table_df)


t2dm_df[,.(BMI_BEFORE_50, BMI, BMI_AFTER_52)]


# subgroup: 3개 검진 ---------------------------------------------------------

t2dm_ids2 <- t2dm_df2[!is.na(SMOKING_3rd) &
                        !is.na(SEX_3rd) &
                        !is.na(AGE_3rd) &
                        !is.na(BMI_3rd) &
                        !is.na(IFG2) &
                        !is.na(FHx_T2DM2)&
                        OBS_DURATION2>0,PERSON_ID]
t2dm_df2[PERSON_ID %in% t2dm_ids2][,.(n=.N,
                               event = sum(T2DM_YN),
                               duration = sum(OBS_DURATION2_Y),
                               rate = sum(as.numeric(T2DM_YN)) /
                                 sum(OBS_DURATION2_Y) * 1000), by = BMI_GROUP2][order(BMI_GROUP2)]
t2dm_df$OBS_DURATION2

# t2dm_sankey[,group := ifelse(OBESE_BEFORE_50 == 'Normal' & OBESE_52=='Normal' & OBESE_AFTER_52=='Normal', 'MNMN',
#                               ifelse(OBESE_BEFORE_50 == 'Normal' & OBESE_52=='Normal' & OBESE_AFTER_52=='Obese','MNBO',
#                                       ifelse(OBESE_BEFORE_50 == 'Obese' & OBESE_52=='Obese' & OBESE_AFTER_52=='Normal','MOBN',
#                                               ifelse(OBESE_BEFORE_50 == 'Obese' & OBESE_52=='Normal' & OBESE_AFTER_52=='Obese','BNBO',
#                                                       ifelse(OBESE_BEFORE_50 == 'Normal' & OBESE_52=='Obese' & OBESE_AFTER_52=='Normal','BOBN','etc')))))]

#m1
fit1 <- coxph(Surv(OBS_DURATION2_Y, T2DM_YN==1) ~ BMI_GROUP2, data=t2dm_df2[BMI_GROUP2!='etc'])

#m2
fit2 <- coxph(Surv(OBS_DURATION2_Y, T2DM_YN==1) ~ BMI_GROUP2 + SEX_3rd + AGE_3rd, data=t2dm_df2[BMI_GROUP2!='etc'])

#m3
fit3 <- coxph(Surv(OBS_DURATION2_Y, T2DM_YN==1) ~ BMI_GROUP2 + SEX_3rd + AGE_3rd +
                BMI_3rd + IFG2 + HPTN_YN2 + FHx_T2DM2 + SMOKING_3rd, data=t2dm_df2[BMI_GROUP2!='etc'])


fit2
createTable3(mainVar='BMI_GROUP2',fit1,fit2,fit3, data=t2dm_df2)


subgroup_table_df[,new_obs_duration := ifelse(
  T2DM_YN==1,
  difftime(T2DM_OCCUR_DATE,CHECK_DATE_AFTER_52, units='days'),
  difftime(LAST_CHECK_DATE,CHECK_DATE_AFTER_52, units='days')
)]

subgroup_table_df[,.(PERSON_ID, T2DM_YN,new_obs_duration)] %>%  View()
mean(t2dm_df2$OBS_DURATION2_2nd)/365


# n수 파악 -------------------------------------------------------------------



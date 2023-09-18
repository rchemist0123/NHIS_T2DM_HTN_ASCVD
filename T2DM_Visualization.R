

# Flow chart --------------------------------------------------------------

require(grid)
require(Gmisc)
require(magrittr)
require(glue)
grid.newpage()

leftx <- .25
midx <- .5
rightx <- .75
width <- .4
gp <- gpar(fill = "lightgrey")

# create boxes
total <- boxGrob(glue("All patients",
                      '(n = {pop})',
                      pop = txtInt(514866),
                      .sep= "\n"),
                  x=midx, y=.9, box_gp = gp, width = width)



# height, weight ----------------------------------------------------------
t2dm_over52[!is.na(HEIGHT),.(PERSON_ID)]
require(ggplot2)
require(dplyr)
t2dm_over52 %>% ggplot(aes(x=BMI_DELTA)) + geom_histogram(bins=200) +
  xlim(-10,10) + labs(title='BMI Delta Histogram') + theme_bw() +
  facet_wrap(~BMI_GROUP)

hist(t2dm_over52$BMI_DELTA)
summary(t2dm_over52$BMI_DELTA)
boxplot(t2dm_over52$BMI_DELTA)

t2dm_over52 %>% ggplot(aes(x=BMI_GROUP, y=BMI_DELTA)) +
  geom_violin(aes(fill=BMI_GROUP), alpha=.5)


# 그룹별 연령에 따른 BMI 변화 -------------------------------------------------------

t2dm_cohort_eachone[PERSON_ID %in% baseline_table_df$PERSON_ID] %>% group_by(CHECK_YEAR) %>% tally() 

baseline_table_df %>% colnames()
baseline_table_df[,.(PERSON_ID, BMI_GROUP)]


t2dm_cohort[PERSON_ID %in% baseline_table_df$PERSON_ID,.(PERSON_ID,STND_Y, AGE)] %>% View()

bmi_ts_cohort <- t2dm_cohort_eachone[,.(CHECK_YEAR, AGE,min_age = min(AGE), max_age=max(AGE), BMI), by=PERSON_ID]
bmi_ts_cohort <- bmi_ts_cohort[baseline_table_df[,.(PERSON_ID,BMI_GROUP)], on =c('PERSON_ID'), nomatch=0]
bmi_ts_cohort <- bmi_ts_cohort[!is.na(BMI_GROUP)]
require(ggplot2)
ggplot(bmi_ts_cohort, aes(x=AGE, y=BMI, color=BMI_GROUP)) + 
  stat_smooth(aes(fill=BMI_GROUP), method = 'loess') + 
  scale_x_continuous(name="AGE", breaks=c(40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60),
                     limits = c(40,60))+
  scale_fill_discrete(labels=c('A(BMI≤25)','B(BMI ↑)','C(BMI ↓)', 'D(BMI ≥25)'))+
  scale_color_discrete(labels=c('A(BMI≤25)','B(BMI ↑)','C(BMI ↓)', 'D(BMI ≥25)'))+
  labs(title = "BMI by ages")
s
require(Publish)
ci.mean()
bmi_ts_cohort %>% group_by(AGE, BMI_GROUP) %>% 
  summarise(BMI_mean = mean(BMI,na.rm=T),
            BMI_se = as.numeric(ci.mean(BMI)[2])) %>% 
  ggplot(aes(x=AGE, y=BMI_mean, color=BMI_GROUP))+
  geom_errorbar(aes(ymin=BMI_mean-BMI_se, ymax=BMI_mean + BMI_se), alpha=.3)+
  geom_line()+
  geom_point() +
  scale_x_continuous(name="AGE", breaks=c(40,41,42,43,44,45,46,47,48,49,50,60),
                     limits = c(40,50))+
  labs(title = "BMI by ages")

require(RColorBrewer)
bmi_ts_cohort %>% group_by(AGE, BMI_GROUP) %>% 
  summarise(BMI_mean = mean(BMI,na.rm=T),
            BMI_sd = sd(BMI,na.rm=T)) %>% 
  ggplot(aes(x=AGE, y=BMI_mean,color=BMI_GROUP))+
  geom_errorbar(aes(ymin=BMI_mean-BMI_sd, ymax=BMI_mean + BMI_sd))+
  geom_point() +
  scale_x_continuous(name="AGE", breaks=c(40,41,42,43,44,45,46,47,48,49,50),
                     limits = c(40,50))+
  geom_line()+
  facet_wrap(~BMI_GROUP)
scale_color_manual(values=c(brewer.pal(n=4, name='Set1')))


# t2dm by IFG -------------------------------------------------------------

mytable(IFG_before_50 ~ T2DM_YN, t2dm_over52[!is.na(IFG)])
mytable(T2DM_YN ~ IFG_before_50, t2dm_over52[!is.na(IFG)])

t2dm_over52[!is.na(IFG)][,.(count =.N),by=c('IFG','T2DM_YN')][]
x <- t2dm_over52[!is.na(IFG_before_50)][,.N, by =c('IFG_before_50','T2DM_YN')][,pct := N/sum(N)*100, by='T2DM_YN'][order(T2DM_YN,-IFG_before_50)]
remove(X)
x
x[,label_y :=cumsum(pct)-0.5*pct, by='T2DM_YN']
x
x %>% ggplot(aes(x=T2DM_YN, y= pct, fill=IFG_before_50)) +
  geom_col(width=0.5)+
  geom_text(aes(y=label_y, label=round(pct,1)))+
  labs(title="IFG(before 50) rate by T2DM", y="%")+
  theme(legend.position ='bottom')

# OR plot with references ----------------------------------------

result <- data.table(
  coef = names(coef(fit)),
  or = round(exp(coef(fit)),2),
  ci_low = round(exp(confint(fit, level=.95)),2)[,1],
  ci_high = round(exp(confint(fit, level=.95)),2)[,2],
  p = round(summary(fit)$coefficients[,4],4)
)
result[,sig:=fifelse(p<0.001,'***',fifelse(p<0.01,'**', fifelse(p<0.05,'*',' ')))]
terms.levels <- names(fit$xlevels)
result.levels <- lapply(terms.levels,
                        function(x) data.table(term=x,
                                               coef=paste0(x,fit$xlevels[[x]]),
                                               stringsAsFactors = F)) %>% rbindlist()
result
new_or_table <- merge(result, result.levels, all=T)
new_or_table <- new_or_table[-1] %>% 
  mutate(isRef = is.na(or)) %>% 
  mutate(term = ifelse(is.na(term), coef, term)) %>% 
  mutate_at(vars(or,ci_low,ci_high), funs(ifelse(isRef,0,.))) %>% 
  mutate(term = factor(term, levels=attr(fit$terms, 'term.labels'))) %>% 
  as.data.table()

new_or_table$isRef
new_or_table[isRef==T, or:=1]
new_or_table[,label:=fifelse(isRef==TRUE, 'reference', paste0(or, '(', ci_low, '-', ci_high, ')', sig))]
new_or_table[, pm := as.factor(fifelse(or>=1,1,0))]
new_or_table
require(ggplot2)
new_or_table %>%
  ggplot(aes(
    x = or,
    xmin = ci_low,
    xmax = ci_high,
    label = label,
    y = coef,
    alpha = !isRef
  )) +
  geom_vline(xintercept = 1, linetype = 'longdash') +
  geom_errorbarh(height=.3, aes(color=pm)) +
  geom_point(size = 3, shape = 18, aes(color=pm)) +
  geom_text(x =-1,
            hjust = 0,
            size = 3) +
  coord_cartesian(xlim = c(-1, 5)) +
  theme(legend.position = 'None')+
  scale_color_manual(values=c('blue','red'))+
  labs(x="Odds Ratio", y='Variable', title="OR plot by BMI Matching")+
  scale_y_discrete(limits=rev(unique(sort(new_or_table$coef))))

new_or_table


# km plot -----------------------------------------------------------------


# all
fit_all <- survfit(Surv(OBS_DURATION2_Y, T2DM_YN==1) ~ BMI_GROUP, baseline_table_df)

pairwise_survdiff(Surv(OBS_DURATION2_Y, T2DM_YN==1) ~ BMI_GROUP, baseline_table_df[IFG==0])
all <- ggsurvplot(fit_all, 
                  pval=T,
                  # pval.size=4,
                  fun='event',
                  ylim=c(0,0.7),
                  size=0.5, # line size
                  legend='none',
                  xlab="",
                  ylab="",
                  palette = c('black','green3','blue','red'),
                  break.x.by=1,
                  censor=F,
                  font.family='serif',
                  risk.table=T,
                  tables.col="strata",
                  tables.y.text=F
                  )

# years: 0,1,4,7,10
all
all$plot <-  all$plot + 
  scale_x_continuous(breaks=c(0,1,4,7,10))+
  scale_y_continuous(breaks=c(0.0,0.2,0.4,0.6),limits = c(0,0.6))+
  theme(text= element_text(family = 'serif', size=3),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.line.x = element_line(size=.3),
        axis.line.y = element_line(size=.3, lineend = 'butt'))
  # geom_segment(x=0, xend=0, y=0.6, yend=0.6)
all$plot
all$table +scale_x_continuous(breaks=c(0,1,4,7,10))
# all$table <- all$table + theme(panel.background = element_rect(fill = "white"),
#                                text= element_text(family = 'sans'),
#                                plot.title=element_text(family="sans",size=10))+
  # labs(x="")

fit_female <- survfit(Surv(OBS_DURATION2_Y, T2DM_YN==1) ~ BMI_GROUP, baseline_table_df %>% filter(SEX==0))
female <- ggsurvplot(fit_female, 
                     pval=T,
                     # pval.size=4,
                     fun='event',
                     ylim=c(0,0.7),
                     size=0.5, # line size
                     legend='none',
                     break.x.by=1,
                     xlab="",
                     ylab="",
                     palette = c('black','green3','blue','red'),
                     censor=F,
                     font.family='serif',
                     risk.table=T)
female$table +scale_x_continuous(breaks=c(0,1,4,7,10))
# years: 0,1,4,7,10
female
female$plot <-  female$plot + 
  scale_x_continuous(breaks=c(0,1,4,7,10))+
  scale_y_continuous(breaks=c(0.0,0.2,0.4,0.6),limits = c(0,0.6))+
  theme(text= element_text(family = 'serif', size=3),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.line.x = element_line(size=.3),
        axis.line.y = element_line(size=.3, lineend = 'butt'))

female$table

female$table <- female$table + 
  ggtitle('No. at Risk')+
  theme(panel.background = element_rect(fill = "white"),
       text= element_text(family = 'sans'),
       plot.title=element_text(family="sans",size=10))+labs(x="")
female_result <- ggpar(female, 
                    font.x=c(10),
                    font.y=c(10))

female_result
fit_male <- survfit(Surv(OBS_DURATION2_Y, T2DM_YN==1) ~ BMI_GROUP, baseline_table_df %>% filter(SEX==1))
male <- ggsurvplot(fit_male, 
                   pval=T,
                   # pval.size=4,
                   fun='event',
                   ylim=c(0,0.7),
                   size=0.5, # line size
                   legend='none',
                   xlab="",
                   ylab="",
                   palette = c('black','green3','blue','red'),
                   censor=F,
                   break.x.by=1,
                   font.family='serif',
                   risk.table=T)
male$table +scale_x_continuous(breaks=c(0,1,4,7,10))
# years: 0,1,4,7,10
all
male$plot <-  male$plot + 
  scale_x_continuous(breaks=c(0,1,4,7,10))+
  scale_y_continuous(breaks=c(0.0,0.2,0.4,0.6),limits = c(0,0.6))+
  theme(text= element_text(family = 'serif', size=3),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.line.x = element_line(size=.3),
        axis.line.y = element_line(size=.3, lineend = 'butt'))
male$plot

male$table <- male$table + 
  ggtitle('No. at Risk')+
  theme(panel.background = element_rect(fill = "white"),
        text= element_text(family = 'sans'),
        plot.title=element_text(family="sans",size=10))+labs(x="")
male_result <- ggpar(male, 
                       font.x=c(10),
                       font.y=c(10))
male_result
fit_ifg0 <- survfit(Surv(OBS_DURATION2_Y, T2DM_YN==1) ~ BMI_GROUP, baseline_table_df %>% filter(IFG==0))
ifg0 <- ggsurvplot(fit_ifg0, 
                   # pval=T,
                   # pval.size=4,
                   fun='event',
                   ylim=c(0,0.7),
                   break.x.by=1,
                   size=0.5, # line size
                   legend='none',
                   xlab="",
                   ylab="",
                   palette = c('black','green3','blue','red'),
                   censor=F,
                   font.family='serif',
                   risk.table=T)
ifg0$table +scale_x_continuous(breaks=c(0,1,4,7,10))
# years: 0,1,4,7,10
ifg0$plot <-  ifg0$plot + 
  scale_x_continuous(breaks=c(0,1,4,7,10))+
  scale_y_continuous(breaks=c(0.0,0.2,0.4,0.6),limits = c(0,0.6))+
  theme(text= element_text(family = 'serif', size=3),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.line.x = element_line(size=.3),
        axis.line.y = element_line(size=.3, lineend = 'butt'))


ifg0$plot
ifg0$table <- ifg0$table + 
  ggtitle('No. at Risk')+
  theme(panel.background = element_rect(fill = "white"),
        text= element_text(family = 'sans'),
        plot.title=element_text(family="sans",size=10))+labs(x="")
ifg0_result <- ggpar(ifg0, 
                       font.x=c(10),
                       font.y=c(10))


fit_ifg1 <- survfit(Surv(OBS_DURATION2_Y, T2DM_YN==1) ~ BMI_GROUP, baseline_table_df %>% filter(IFG==1))
coxph(Surv(OBS_DURATION2_Y, T2DM_YN==1) ~ BMI_GROUP, baseline_table_df %>% filter(IFG==1)) %>% pyears()

ifg1 <- ggsurvplot(fit_ifg1, 
                   # pval=T,
                   break.x.by=1,
                   # pval.size=4,
                   fun='event',
                   ylim=c(0,0.7),
                   size=0.5, # line size
                   legend='none',
                   xlab="",
                   ylab="",
                   palette = c('black','green3','blue','red'),
                   censor=F,
                   font.family='serif',
                   risk.table=T)
ifg1$table +scale_x_continuous(breaks=c(0,1,4,7,10))
# years: 0,1,4,7,10
ifg1
ifg1$plot <-  ifg1$plot + 
  scale_x_continuous(breaks=c(0,1,4,7,10))+
  scale_y_continuous(breaks=c(0.0,0.2,0.4,0.6),limits = c(0,0.6))+
  theme(text= element_text(family = 'serif', size=3),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.line.x = element_line(size=.3),
        axis.line.y = element_line(size=.3, lineend = 'butt'))

ifg1$plot


ifg1 <- ggsurvplot(fit_ifg1, 
                   pval=T,
                   fun='event',
                   pval.size=4,
                   ylim=c(0,0.7),
                   size=0.5, # line size
                   # legend=c(0.17,0.78),
                   legend='none',
                   legend.title='',
                   # legend.labs=c('Maintaining Normal','Becoming Obese','Becoming Normal','Maintaining Obese'),
                   xlab="Years",
                   ylab="Incidence Probability",
                   palette = c('black','#00BA38','#619CFF','#F8766D'),
                   ggtheme = theme_cleantable(),
                   break.time.by=1,
                   censor=F,
                   # table
                   risk.table=T,
                   risk.table.x.text=F,
                   risk.table.y.text=FALSE,
                   fontsize=3,
                   font.family='sans',
                   tables.height = 0.35)

ifg1$plot <-  ifg1$plot + 
  ggtitle("E. Baseline impaired fasting glucose")+
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        legend.key = element_rect(fill='white'),
        text= element_text(family = 'sans'),
        plot.title = element_text(size=15))

ifg1$table <- ifg1$table + 
  ggtitle('No. at Risk')+
  theme(panel.background = element_rect(fill = "white"),
        text= element_text(family = 'sans'),
        plot.title=element_text(family="sans",size=10))+labs(x="")
ifg1_result <- ggpar(ifg1, 
                     font.x=c(10),
                     font.y=c(10))

km_list <- list()
km_list[[1]] <- female_result
km_list[[2]] <- ifg0_result
km_list[[3]] <- male_result
km_list[[4]] <- ifg1_result
arrange_ggsurvplots(km_list, print=TRUE, 
                    nrow=2, ncol=2, 
                    risk.table.height = 0.37)



all
ifg1

# macthing kmplot ---------------------------------------------------------

fit <- survfit(Surv(OBS_DURATION2, T2DM_YN==1) ~ BMI_GROUP, match_data2)
match <- ggsurvplot(fit, 
                   pval=T,
                   fun='event',
                   pval.size=4,
                   ylim=c(0,0.7),
                   size=0.5, # line size
                   legend='none',
                   legend.title='',
                   legend.labs=c('Becoming Obese','Maintaining Obese'),
                   xlab="Years",
                   break.x.by=1,
                   ylab="Cumulative Incidence",
                   # palette = c('#00BA38','#619CFF'),
                   ggtheme = theme_cleantable(),
                   break.time.by=1,
                   censor=F,
                   
                   # table
                   risk.table=T,
                   risk.table.y.text=FALSE,
                   fontsize=3,
                   # font.family='Times New Roman',
                   tables.height = 0.35)
match$plot <-  match$plot + 
  theme(panel.background = element_rect(fill = "white", colour = "black"))
  # annotate("text", x=5, y=0.67, label=)
match$table <- match$table + theme(panel.background = element_rect(fill = "white"))
match_result <- ggpar(match, 
                     font.title = c(10), 
                     font.x=c(10),
                     font.y=c(10))

match_result


# coxplot -----------------------------------------------------------------

coxfit <- coxph(Surv(OBS_DURATION2_Y, T2DM_YN==1) ~ 
                  SEX+ BMI + BMI_GROUP + IFG + FMLY_DIABML_PATIEN_YN +
                  SMOKING + DRINKING  ,
                data=baseline_table_df)
summary(coxfit)

coxfit <- coxph(Surv(OBS_DURATION2_Y, T2DM_YN==1) ~ 
                  SEX+ BMI +
                  SMOKING + DRINKING + IFG + FMLY_DIABML_PATIEN_YN +
                  strata(BMI_GROUP), 
             data=baseline_table_df)
summary(coxfit)

survfit(coxfit)
ggsurvplot(survfit(coxfit),
           fun='event',
           censor=F,
           ylim=c(0,0.7),
           data=baseline_table_df)


# SANKEY plot -------------------------------------------------------------
setnames(bmi_after_52, 'CHECK_DATE','CHECK_DATE_AFTER_52')

t2dm_sankey <- t2dm_df2[PERSON_ID %in% t2dm_ids2,.(PERSON_ID,BMI,BMI_2nd,BMI_3rd)]
t2dm_sankey[,`:=`(
  OBESE_BEFORE_50 = ifelse(BMI>=25,'Obese','Normal'),
  OBESE_52 = ifelse(BMI_2nd>=25,'Obese','Normal'),
  OBESE_AFTER_52 = ifelse(BMI_3rd>=25,'Obese','Normal')
)]
t2dm_sankey[!is.na(OBESE_52) & !is.na(OBESE_AFTER_52) & !is.na(OBESE_BEFORE_50),.N, by=c('OBESE_BEFORE_50','OBESE_52','OBESE_AFTER_52')][order(OBESE_BEFORE_50,OBESE_52, OBESE_AFTER_52)]
prop.table(table(t2dm_sankey$OBESE_BEFORE_50))
baseline_table_df[,.(PERSON_ID,CHECK_DATE_BEFORE_50, CHECK_DATE, CHECK_DATE_AFTER_52)][,.(
  gap1 = as.numeric(difftime(CHECK_DATE, CHECK_DATE_BEFORE_50, units='days')),
  gap2 = as.numeric(difftime(CHECK_DATE_AFTER_52, CHECK_DATE, units='days'))
)][,.(mean(gap1, na.rm=T)/365, sd(gap1, na.rm=T)/365, mean(gap2, na.rm=T)/365, sd(gap2, na.rm=T)/365)]

subgroup4_id <- t2dm_sankey[
  (OBESE_BEFORE_50=='Obese' & OBESE_52=='Obese' & OBESE_AFTER_52=='Obese') |
    (OBESE_BEFORE_50=='Normal' & OBESE_52=='Normal' & OBESE_AFTER_52=='Normal') |
    (OBESE_BEFORE_50=='Normal' & OBESE_52=='Obese' & OBESE_AFTER_52=='Obese') |
    (OBESE_BEFORE_50=='Obese' & OBESE_52=='Normal' & OBESE_AFTER_52=='Normal')
]$PERSON_ID
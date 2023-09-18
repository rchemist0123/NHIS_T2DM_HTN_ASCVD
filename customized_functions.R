createTable3 <- function(mainVar, fit1, fit2, fit3, data){
  require(data.table)
  t1 <- summary(fit1)[[8]] %>% as.data.table()
  t2 <- summary(fit2)[[8]] %>% as.data.table()
  t3 <- summary(fit3)[[8]] %>% as.data.table()
  
  p1 <- summary(fit1)[[7]] %>% as.data.table()
  p2 <- summary(fit2)[[7]] %>% as.data.table()
  p3 <- summary(fit3)[[7]] %>% as.data.table()
  
  refs <- as.data.table("1 (Ref.)")
  varnames <- levels(data[[mainVar]]) %>% as.data.table()
  names(varnames) <- mainVar
  varLen <- length(levels(data[[mainVar]]))
  t1[,model1 := paste0(format(round(`exp(coef)`,2),nsmall=2),' (',
                       format(round(`lower .95`,2),nsmall=2),'-',
                       format(round(`upper .95`,2),nsmall=2),')')]
  p1[,p:= ifelse(`Pr(>|z|)`<0.001,'<0.001',format(round(`Pr(>|z|)`,3),nsmall=3))]
  t1_1 <- bind_rows(refs, t1)
  p1_1 <- bind_rows(refs, p1)
  t1_new = cbind(t1_1[,.(model1)], p1_1[,.(p)])
  t1_new2 = cbind(varnames, t1_new)
  
  t2[,model2 := paste0(format(round(`exp(coef)`,2),nsmall=2),' (',
                       format(round(`lower .95`,2),nsmall=2),'-',
                       format(round(`upper .95`,2),nsmall=2),')')]
  p2[,p:= ifelse(`Pr(>|z|)`<0.001,'<0.001',format(round(`Pr(>|z|)`,3),nsmall=3))]
  
  t2_1 <- bind_rows(refs, t2)
  p2_1 <- bind_rows(refs, p2)
  t2_new <-  bind_cols(t2_1[1:varLen,.(model2)], p2_1[1:varLen,.(p)])
  
  t3[,model3 := paste0(format(round(`exp(coef)`,2),nsmall=2),' (',
                       format(round(`lower .95`,2),nsmall=2),'-',
                       format(round(`upper .95`,2),nsmall=2),')')]
  p3[,p:= ifelse(`Pr(>|z|)`<0.001,'<0.001',format(round(`Pr(>|z|)`,3),nsmall=3))]
  t3_1 <- bind_rows(refs, t3)
  p3_1 <- bind_rows(refs, p3)
  t3_new = cbind(t3_1[1:varLen,.(model3)], p3_1[1:varLen,.(p)])
  
  result = cbind(t1_new2,t2_new, t3_new)
  message('** The result of the < Table 3 > is **\n')
  return(result)
}


multiRegression <- function(fit){
  p <- summary(fit)[[7]] %>% as.data.table(keep.rownames = T)
  setnames(p,"Pr(>|z|)",'p')
  p <- p[,.(rn,p)]
  p[,p:=ifelse(p<0.001,'<0.001',format(round(p,3),nsmall=3))]
  
  hr <- summary(fit)[[8]] %>% as.data.table(keep.rownames = T)
  names(hr) <- c('rn','HR','tete','lower','upper')
  hr[,`HR (95% CI)` := paste0(format(round(HR,2),nsmall=2),
                              ' (',
                              format(round(lower,2),nsmall=2), '-',
                              format(round(upper,2),nsmall=2),')')]
  hr2 <- hr[,.(rn,`HR (95% CI)`)]
  return(inner_join(hr2,p, by='rn'))
}


a <- multiRegression(fit4)
b <- multiRegression(fit5)
left_join(b,a, by='rn', suffix=c('fit5','fit4'))


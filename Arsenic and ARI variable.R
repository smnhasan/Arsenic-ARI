#ak<- c(2,4,6,7,8,9,10,11)
#ak<- ifelse(ak<=4, 0, ifelse(ak>4 & ak <=8, 1,ifelse(ak>8 & ak<11, 2,3)))
#factor(ak)
#ak<- factor(ak,levels=c(0,1,2,3),labels = c('Low','medium', 'Moderate','High'))
              
              
#factor(ak)


require(foreign)
require(MASS)
require(pROC)
require(survey)
require(ResourceSelection)
require(ROCR)
require(car)
require(ggplot2)
require(maptools)
require(dplyr)

#rm(list=ls(all=TRUE)[sapply(mget(ls(all=TRUE)), class) == "data.frame"]) # for clear r environment 


## importing dataset


 

setwd("E:/Study/Paper/MICS/Bangladesh MICS6 SPSS Datasets")

wm <- as.data.frame(read.spss('wm.sav',use.value.labels=F),stringsAsFactors = FALSE)

wm<- wm[with(wm, order(HH1, HH2, LN)),]

hh <- as.data.frame(read.spss('hh.sav',use.value.labels=F),stringsAsFactors = FALSE)

hh<- hh[with(hh, order(HH1, HH2)),]

ch <- as.data.frame(read.spss('ch.sav',use.value.labels=F),stringsAsFactors = FALSE)

ch<- ch[with(ch, order(HH1, HH2, LN)),]

merdat <- left_join(x = ch, y = wm, by=c("HH1", "HH2","LN"))

merdat2 <- left_join(x = merdat, y = hh, by=c("HH1", "HH2"))
str(merdat2)

merdat$stratum.x




## subsetting datasets
f <- c('CA1','WQ12A', 'CA16', 'CA18', 'WQ19C','WQ27','WQ12','WS1','CAGE_11','HL4','HH6.x','HH7.x',
       'melevel','windex5.x','HC1A','HHSEX','ethnicity','WAGE','WS11',
       'EC3A','SA1','MT1','MT2','MT3','WAZ2',
       'HAZ2','WHZ2','HC17','HH48','HC15','HC14','WS15',
       'WS3','WS9','chweight','HH1','stratum','melevel','UB2')


fd <- merdat2[, f]

fd <- filter(fd, UB2>2)  # filter age using age is gratter than 2 year

#design1 <- svydesign(id=fd$HH1, strata = fd$stratum,   weights=fd$chweight,data=fd)
design1 <- svydesign(id=fd$HH1,  weights=fd$chweight, data=fd)

#outcome and key predictor

## Ari variable 
fd$ari <- ifelse(fd$CA16==1 & (fd$CA18==1 |fd$CA18==3),1 ,0)

fd$ari <- ifelse(is.na(fd$ari)==1,0,fd$ari) # as dhs suggested

fd$ari <- factor(fd$ari,levels = c(0,1),labels = c('no','yes'))
summary(fd$ari)


#household water test (100ml).
factor(fd$WQ12A)

fd$hhwq <- ifelse(fd$WQ12A<= 10,0,ifelse(fd$WQ12A>10 & fd$WQ12A<=50, 1, ifelse(fd$WQ12A>50 & fd$WQ12A<= 200, 2, 3)))
factor(fd$hhwq)
fd$hhwq <- factor(fd$hhwq,levels=c(0,1,2,3),labels = c('Low','Moderate','High','Very high'))
factor(fd$hhwq)

summary(fd$hhwq)

#svychisq(~fd$hhwq+fd$ari,design=design1, data=fd)

#cross tab between ari & arsenic

round(svytable(~fd$hhwq+fd$ari,design=design1))
round(svytable(~fd$hhwq,design=design1))
prop.table(svytable(~fd$hhwq,design=design1))*100
prop.table(svytable(~fd$hhwq+fd$ari,design=design1), margin=1)*100
round(prop.table(svytable(~fd$hhwq+fd$ari,design=design1), margin=1)*100,3)
chisq.test((svytable(~fd$hhwq+fd$ari,design=design1)))





#Child Age
fd$cage <- factor(fd$CAGE_11)

round(svytable(~fd$cage+fd$ari,design=design1))
round(svytable(~fd$cage,design=design1))
prop.table(svytable(~fd$cage,design=design1))*100
prop.table(svytable(~fd$cage+fd$ari,design=design1), margin=1)*100
chisq.test((svytable(~fd$cage+fd$ari,design=design1)))

#Child sex
fd$csex <- fd$HL4
fd$csex <- factor(fd$csex,levels=c(1,2),labels = c('Male','Female'))
factor(fd$csex)

round(svytable(~fd$csex+fd$ari,design=design1))
round(svytable(~fd$csex,design=design1))
prop.table(svytable(~fd$csex,design=design1))*100
prop.table(svytable(~fd$csex+fd$ari,design=design1), margin=1)*100
chisq.test(svytable(~fd$csex+fd$ari,design=design1))

#Residence.

fd$residence <- fd$HH6.x
fd$residence <- factor(fd$HH6.x,levels=c(1,2,3,4,5),labels = c('Rural','Urban','Urban','Urban','Rural'))
factor(fd$residence)

round(svytable(~fd$residence+fd$ari,design=design1))
round(svytable(~fd$residence,design=design1))
prop.table(svytable(~fd$residence,design=design1))*100
prop.table(svytable(~fd$residence+fd$ari,design=design1), margin=1)*100
chisq.test(svytable(~fd$residence+fd$ari,design=design1))


#Division. ( problem level do not shown )

fd$division <- fd$HH7.x
fd$division <- factor(fd$HH7.x,levels=c(10,20,30,40,45,50,55,60),labels = c('Barisal','Chattogram','Dhaka','Khulna','Mymensingh','Rajshahi','Rangpur','Sylhet'))
factor(fd$HH7.x)

round(svytable(~fd$HH7.x+fd$ari,design=design1))
round(svytable(~fd$HH7.x,design=design1))
prop.table(svytable(~fd$HH7.x,design=design1))*100
prop.table(svytable(~fd$HH7.x+fd$ari,design=design1), margin=1)*100
chisq.test(svytable(~fd$HH7.x+fd$ari,design=design1))

#Mother Education.
factor(fd$melevel)
fd$melevel <- ifelse(fd$melevel==9,NA,fd$melevel)
fd$mel <- factor(fd$melevel,levels=c(0,1,2,3),labels = c('P','LS','SH','H'))
factor(fd$mel)

round(svytable(~fd$mel+fd$ari,design=design1))
round(svytable(~fd$mel,design=design1))
prop.table(svytable(~fd$mel,design=design1))*100
prop.table(svytable(~fd$mel+fd$ari,design=design1), margin=1)*100
chisq.test(svytable(~fd$mel+fd$ari,design=design1))

#Wealth index Status.
factor(fd$windex5.x)
fd$windex <- ifelse(fd$windex5.x==0,NA,fd$windex5.x)
factor(fd$windex)
fd$windex <- factor(fd$windex,levels=c(1,2,3,4,5),labels = c('Poor','Poor','Middle','Rich','Rich'))
factor(fd$windex)

round(svytable(~fd$windex+fd$ari,design=design1))
round(svytable(~fd$windex,design=design1))
prop.table(svytable(~fd$windex,design=design1))*100
prop.table(svytable(~fd$windex+fd$ari,design=design1), margin=1)*100
chisq.test(svytable(~fd$windex+fd$ari,design=design1))


#Type of toilet facility.
fd$tf <- ifelse(fd$WS11==99,NA,fd$WS11)
fd$tf <- ifelse(fd$WS11<24,1,2)
fd$tf <- factor(fd$tf,levels=c(1,2),labels = c('Improved','Unimproved'))

round(svytable(~fd$tf+fd$ari,design=design1))
round(svytable(~fd$tf,design=design1))
prop.table(svytable(~fd$tf,design=design1))*100
prop.table(svytable(~fd$tf+fd$ari,design=design1), margin=1)*100
chisq.test(svytable(~fd$tf+fd$ari,design=design1))

#Salt Iodization.
fd$si <- ifelse(fd$SA1==9,NA,fd$SA1)
fd$si <- ifelse(fd$si==2|fd$si==3,1,2)
fd$si <- factor(fd$si,levels=c(1,2),labels = c('Yes','No'))
factor(fd$si)
round(svytable(~fd$si+fd$ari,design=design1))
round(svytable(~fd$si,design=design1))
prop.table(svytable(~fd$si,design=design1))*100
prop.table(svytable(~fd$si+fd$ari,design=design1), margin=1)*100
chisq.test(svytable(~fd$si+fd$ari,design=design1))


#Household own any animals
fd$animal <- ifelse(fd$HC17==9,NA,fd$HC17)
fd$animal <- factor(fd$animal,levels=c(1,2),labels = c('Yes','No'))
factor(fd$animal)

round(svytable(~fd$animal+fd$ari,design=design1))
round(svytable(~fd$animal,design=design1))
prop.table(svytable(~fd$animal,design=design1))*100
prop.table(svytable(~fd$animal+fd$ari,design=design1), margin=1)*100
chisq.test(svytable(~fd$animal+fd$ari,design=design1))


#househol member
median(fd$HH48)
fd$hhmem <- ifelse(fd$HH48<5,1,2)
fd$hhmem <- factor(fd$hhmem,levels=c(1,2),labels = c('Small','Large'))
factor(fd$hhmem)

round(svytable(~fd$hhmem+fd$ari,design=design1))
round(svytable(~fd$hhmem,design=design1))
prop.table(svytable(~fd$hhmem,design=design1))*100
prop.table(svytable(~fd$hhmem+fd$ari,design=design1), margin=1)*100
chisq.test(svytable(~fd$hhmem+fd$ari,design=design1))

#Toilet facility shared
fd$tfshared <- ifelse(fd$WS15==9,NA,fd$WS15)
fd$tfshared <- factor(fd$tfshared,levels=c(1,2),labels = c('Yes','No'))
factor(fd$tfshared)

round(svytable(~fd$tfshared+fd$ari,design=design1))
round(svytable(~fd$tfshared,design=design1))
prop.table(svytable(~fd$tfshared,design=design1))*100
prop.table(svytable(~fd$tfshared+fd$ari,design=design1), margin=1)*100
chisq.test(svytable(~fd$tfshared+fd$ari,design=design1))


#Treat water to make safer for drinking

fd$watertreat <- ifelse(fd$WS9==8|fd$WS9==9,NA,fd$WS9)
fd$watertreat <- factor(fd$watertreat,levels=c(1,2),labels = c('Yes','No'))
factor(fd$watertreat)

round(svytable(~fd$tfshared+fd$ari,design=design1))
round(svytable(~fd$tfshared,design=design1))
prop.table(svytable(~fd$tfshared,design=design1))*100
prop.table(svytable(~fd$tfshared+fd$ari,design=design1), margin=1)*100
chisq.test(svytable(~fd$tfshared+fd$ari,design=design1))

#end cross tabulation



str(fd)
fd <- subset(fd, !is.na(ari))
fd <- subset(fd, !is.na(hhwq))
str(fd)

logit1 <- (svyglm(relevel(factor(ari), ref = "no") ~  csex, family=quasibinomial(link=logit), data=fd, design=design1 , na.action = na.omit, rescale=TRUE))
summary(logit1) #sig

model.or <- cbind(exp(coef(logit1)),exp(confint(logit1)))
colnames(model.or) <- c('odds ratio','ci(2.5%)','ci(97.5%)')
model.or #for odds ratio and ci

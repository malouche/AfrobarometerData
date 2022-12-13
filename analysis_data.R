### distribution of the duration

d0$durinterv[d0$durinterv>=120]=NA
d0$durinterv[d0$durinterv<9]=NA
library(ggplot2)

quantile(d0$durinterv,probs = seq(0,1,by=0.1),na.rm=T)
p<-ggplot(d0,aes(durinterv))+geom_density()+theme_bw()+xlab("deuration in minutes")
p
     

### By gender respondent

dtemp=d0[,c("durinterv","gender_respondent")]
dtemp=na.omit(dtemp)
p<-ggplot(dtemp,aes(durinterv, fill=gender_respondent))+geom_density(alpha=.4)+theme_bw()+xlab("duration in minutes")
p

dtemp=d0[,c("durinterv","interviewerGender")]
dtemp=na.omit(dtemp)
p<-ggplot(dtemp,aes(durinterv, fill=interviewerGender))+geom_density(alpha=.4)+theme_bw()+xlab("duration in minutes")
p


dtemp=d0[,c("durinterv","samegender")]
dtemp=na.omit(dtemp)
dtemp$samegender=as.factor(dtemp$samegender)
p<-ggplot(dtemp,aes(durinterv, fill=samegender))+geom_density(alpha=.4)+theme_bw()+xlab("duration in minutes")
p


### By countries

ct=sort(unique(d0$country))
d0$country[d0$country%in%ct[9:11]]=ct[9]

library(gmodels)
library(dplyr)
d_count= d0 %>% group_by(country,AfroWage) %>% summarise(mean = ci(na.omit(durinterv))[1], 
                                       lowCI = ci(na.omit(durinterv))[2],
                                       hiCI = ci(na.omit(durinterv))[3], 
                                       sd = ci (na.omit(durinterv))[4])
d_count

d_count2= d0 %>% group_by(country) %>% summarise(mean = ci(na.omit(durinterv))[1], 
                                                         lowCI = ci(na.omit(durinterv))[2],
                                                         hiCI = ci(na.omit(durinterv))[3], 
                                                         sd = ci (na.omit(durinterv))[4])
d_count$country=factor(d_count$country,levels=d_count2$country[order(d_count2$mean)])
p<- ggplot(d_count, aes(x=country, y=mean, group=country)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=lowCI, ymax=hiCI), width=.2)+facet_wrap(~AfroWage,ncol = 3)
p+coord_flip()+theme_bw()


d_count= d0 %>% group_by(country,AfroWage,samelanginterresp) %>% summarise(mean = ci(na.omit(durinterv))[1], 
                                                         lowCI = ci(na.omit(durinterv))[2],
                                                         hiCI = ci(na.omit(durinterv))[3], 
                                                         sd = ci (na.omit(durinterv))[4])



d_count$country=factor(d_count$country,levels=d_count2$country[order(d_count2$mean)])
p<- ggplot(d_count, aes(x=country, y=mean, group=as.factor(samelanginterresp),color=as.factor(samelanginterresp))) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=lowCI, ymax=hiCI), width=.2,position=position_dodge(.9))+facet_wrap(~AfroWage,ncol = 3)
p+coord_flip()+theme_bw()

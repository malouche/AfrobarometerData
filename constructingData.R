#### My data will continue date interview, start interview, end interview, 
#### number of don't know (999), number of refused to answer (998) number of missing values (-1)
#### I will work from r2 to r7

## r2 -> 16,17,266
## r3 -> 17,18,259
## r4 -> 30,31,265
## r5 -> 37,38,305
## r6 -> 42,43,319
## r7 -> 45,46,307


library(haven)
r1_data <- read_sav("r1_data.sav")
r2_data <- read_sav("r2_data.sav")
r3_data <- read_sav("r3_data.sav")
r4_data <- read_sav("r4_data.sav")
r5_data <- read_sav("r5_data.sav")
r6_data <- read_sav("r6_data.sav")
r7_data <- read_sav("r7_data.sav")
### I first keep the complete interviews

i=which(r2_data$hholdse1==997)
r2_data=r2_data[i,]

i=which(r3_data$hholdse1==997)
r3_data=r3_data[i,]

i=which(r4_data$NOCALL_1==997)
r4_data=r4_data[i,]

i=which(r5_data$NOCALL_1==997)
r5_data=r5_data[i,]

i=which(r6_data$NOCALL_1==9997)
r6_data=r6_data[i,]

i=which(r7_data$NOCALL_1==9997)
r7_data=r7_data[i,]


###
v2=colnames(r2_data)[c(16,17,266,267,289,247,288)]
v3=colnames(r3_data)[c(17,18,259,261,282,19,281)]
v4=colnames(r4_data)[c(30,31,265,267,288,32,287)]
v5=colnames(r5_data)[c(37,38,305,307,329,39,328)]
v6=colnames(r6_data)[c(42,43,319,321,343,44,342)]
v7=colnames(r7_data)[c(45,46,307,309,332,47,331)]

x=unlist(apply(r2_data,2,function(x)length(unique(x))))
r2_data=r2_data[,-which(x==1)]

x=unlist(apply(r3_data,2,function(x)length(unique(x))))
r3_data=r3_data[,-which(x==1)]

x=unlist(apply(r4_data,2,function(x)length(unique(x))))
r4_data=r4_data[,-which(x==1)]


x=unlist(apply(r5_data,2,function(x)length(unique(x))))
r5_data=r5_data[,-which(x==1)]

x=unlist(apply(r6_data,2,function(x)length(unique(x))))
r6_data=r6_data[,-which(x==1)]

x=unlist(apply(r7_data,2,function(x)length(unique(x))))
r7_data=r7_data[,-which(x==1)]


### age interview

grep("age",tolower(get_label(r2_data)),value=T)
grep("q80",tolower(colnames(r2_data)),value=F) ## 247 
grep("q107",tolower(colnames(r2_data)),value=F) ##288

grep("age",tolower(get_label(r3_data)),value=T)
grep("q1",tolower(colnames(r3_data)),value=F) ## 19 
grep("q111",tolower(colnames(r3_data)),value=F) ##281

grep("age",tolower(get_label(r4_data)),value=T)
grep("q1",tolower(colnames(r4_data)),value=F) ## 32 
grep("q111",tolower(colnames(r4_data)),value=F) ##287

grep("age",tolower(get_label(r5_data)),value=T)
grep("q1",tolower(colnames(r5_data)),value=F) ## 39 
grep("q113",tolower(colnames(r5_data)),value=F) ##328 

grep("age",tolower(get_label(r6_data)),value=T)
grep("q1",tolower(colnames(r6_data)),value=F) ## 44 
grep("q113",tolower(colnames(r6_data)),value=F) ## 342 


grep("age",tolower(get_label(r7_data)),value=T)
grep("q1",tolower(colnames(r7_data)),value=F) ## 47
grep("q113",tolower(colnames(r7_data)),value=F) ## 331 

## countries

x_countries=c(as.character(as_label(r2_data$country)),as.character(as_label(r3_data$country)),
              as.character(as_label(r4_data$COUNTRY)),as.character(as_label(r5_data$COUNTRY)),
              as.character(as_label(r6_data$COUNTRY)),as.character(as_label(r7_data$COUNTRY)))

## regions

x_reg=c(as.character(as_label(r2_data$region)),as.character(as_label(r3_data$region)),
        as.character(as_label(r4_data$REGION)),as.character(as_label(r5_data$REGION)),
        as.character(as_label(r6_data$REGION)),as.character(as_label(r7_data$REGION)))

x_urb=c(as.character(as_label(r2_data$urbrur)),as.character(as_label(r3_data$urbrur)),
        as.character(as_label(r4_data$URBRUR)),as.character(as_label(r5_data$URBRUR)),
          as.character(as_label(r6_data$URBRUR)),as.character(as_label(r7_data$URBRUR)))

### language resp, language interviewer, language interview
grep("language",tolower(get_label(r2_data)),value=T)
grep("language",tolower(get_label(r3_data)),value=T)
grep("language",tolower(get_label(r4_data)),value=T)
grep("language",tolower(get_label(r5_data)),value=T)
grep("language",tolower(get_label(r6_data)),value=T)
grep("language",tolower(get_label(r7_data)),value=T)

x_resp_lang=c(as.character(as_label(r2_data$q83)),as.character(as_label(r3_data$q3)),
              as.character(as_label(r4_data$Q3)),as.character(as_label(r5_data$Q2)),
              as.character(as_label(r6_data$Q2)),as.character(as_label(r7_data$Q2B)))

x_interviewer_lang=c(as.character(as_label(r2_data$q110)),as.character(as_label(r3_data$q114)),
              as.character(as_label(r4_data$Q114)),as.character(as_label(r5_data$Q116)),
              as.character(as_label(r6_data$Q116)),as.character(as_label(r7_data$Q116)))

x_interview_lang=c(as.character(as_label(r2_data$q97)),as.character(as_label(r3_data$q103)),
                     as.character(as_label(r4_data$Q103)),as.character(as_label(r5_data$Q103)),
                     as.character(as_label(r6_data$Q103)),as.character(as_label(r7_data$Q103)))


### date interv, start interv, end interv, gender respond, gender interviewer, age respondent, age interviewer 

library(gtools)

d0list=list(AfroW2=r2_data[,v2],
            AfroW3=r3_data[,v3],
            AfroW4=r4_data[,v4],
            AfroW5=r5_data[,v5],
            AfroW6=r6_data[,v6],
            AfroW7=r7_data[,v7])

d0list$AfroW2=unname(d0list$AfroW2)
d0list$AfroW3=unname(d0list$AfroW3)
d0list$AfroW4=unname(d0list$AfroW4)
d0list$AfroW5=unname(d0list$AfroW5)
d0list$AfroW6=unname(d0list$AfroW6)
d0list$AfroW7=unname(d0list$AfroW7)

colnames(d0list$AfroW2)=colnames(d0list$AfroW3)=colnames(d0list$AfroW4)=colnames(d0list$AfroW5)=colnames(d0list$AfroW6)=colnames(d0list$AfroW7)=c("date","strtime","endtime","gender_respondent","interviewerGender","age_respondent", "age_interviewer")

library(plyr)

d0=ldply(d0list)

colnames(d0)[1]="AfroWage"
d0$country=tolower(x_countries)
d0$region=tolower(x_reg)
d0$urb=tolower(x_urb)
d0$interview_lang=tolower(x_interview_lang)
d0$interviewer_lang=tolower(x_interviewer_lang)
d0$resp_lang=tolower(x_resp_lang)

### Compute durations, days,...

## durartions

durinterv=difftime(d0$endtime,d0$strtime,units = "secs")
summary(durinterv)
durinterv[1:100]
x=as.numeric(durinterv)
summary(x)

d0$durinterv=x

sum(d0$durinterv<0,na.rm = T)
d0$durinterv[d0$durinterv<0]=NA
summary(d0$durinterv)

library(lubridate)
d0$weekdaysinterv=weekdays(d0$date)
xtabs(~d0$weekdaysinterv)
d0$monthinterv=months(d0$date)
xtabs(~d0$monthinterv)
d0$yearinterv=year(d0$date)

### labeling

d0$gender_respondent=as_label(d0$gender_respondent)
d0$interviewerGender=as_label(d0$interviewerGender)


## counting don't know answer
library(sjlabelled)
xx=as.data.frame(r2_data)
for(j in 1:ncol(xx)){
  print(j)
  x=as.character(as_label(xx[,j]))
  i=grep("Don't know",x)
  if(length(i)>0){
    xx[i,j]=1
    xx[-i,j]=0
  }
  else xx[,j]=0
  xx[,j]=as.numeric(xx[,j])
}
x_dk_w2=rowSums(xx)


xx=as.data.frame(r3_data)
for(j in 1:ncol(xx)){
  print(j)
  x=as.character(as_label(xx[,j]))
  i=grep("Don't know",x)
  if(length(i)>0){
    xx[i,j]=1
    xx[-i,j]=0
  }
  else xx[,j]=0
  xx[,j]=as.numeric(xx[,j])
}
x_dk_w3=rowSums(xx)

xx=as.data.frame(r4_data)
for(j in 1:ncol(xx)){
  print(j)
  x=as.character(as_label(xx[,j]))
  i=grep("Don't know",x)
  if(length(i)>0){
    xx[i,j]=1
    xx[-i,j]=0
  }
  else xx[,j]=0
  xx[,j]=as.numeric(xx[,j])
}
x_dk_w4=rowSums(xx)



xx=as.data.frame(r5_data)
for(j in 1:ncol(xx)){
  print(j)
  x=as.character(as_label(xx[,j]))
  i=grep("Don't know",x)
  if(length(i)>0){
    xx[i,j]=1
    xx[-i,j]=0
  }
  else xx[,j]=0
  xx[,j]=as.numeric(xx[,j])
}
x_dk_w5=rowSums(xx)

xx=as.data.frame(r6_data)
for(j in 1:ncol(xx)){
  print(j)
  x=as.character(as_label(xx[,j]))
  i=grep("Don't know",x)
  if(length(i)>0){
    xx[i,j]=1
    xx[-i,j]=0
  }
  else xx[,j]=0
  xx[,j]=as.numeric(xx[,j])
}
x_dk_w6=rowSums(xx)


xx=as.data.frame(r7_data)
for(j in 1:ncol(xx)){
  print(j)
  x=as.character(as_label(xx[,j]))
  i=grep("Don't know",x)
  if(length(i)>0){
    xx[i,j]=1
    xx[-i,j]=0
  }
  else xx[,j]=0
  xx[,j]=as.numeric(xx[,j])
}
x_dk_w7=rowSums(xx)

x_dk=c(x_dk_w2,x_dk_w3,x_dk_w4,x_dk_w5,x_dk_w6,x_dk_w7)

### Refused


xx=as.data.frame(r2_data)
for(j in 1:ncol(xx)){
  print(j)
  x=as.character(as_label(xx[,j]))
  i=grep("Refused",x)
  if(length(i)>0){
    xx[i,j]=T
    xx[-i,j]=F
  }
  else xx[,j]=0
  xx[,j]=as.numeric(xx[,j])
}
x_rf_w2=rowSums(xx)


xx=as.data.frame(r3_data)
for(j in 1:ncol(xx)){
  print(j)
  x=as.character(as_label(xx[,j]))
  i=grep("Refused",x)
  if(length(i)>0){
    xx[i,j]=T
    xx[-i,j]=F
  }
  else xx[,j]=0
  xx[,j]=as.numeric(xx[,j])
}
x_rf_w3=rowSums(xx)

xx=as.data.frame(r4_data)
for(j in 1:ncol(xx)){
  print(j)
  x=as.character(as_label(xx[,j]))
  i=grep("Refused",x)
  if(length(i)>0){
    xx[i,j]=T
    xx[-i,j]=F
  }
  else xx[,j]=0
  xx[,j]=as.numeric(xx[,j])
}
x_rf_w4=rowSums(xx)



xx=as.data.frame(r5_data)
for(j in 1:ncol(xx)){
  print(j)
  x=as.character(as_label(xx[,j]))
  i=grep("Refused",x)
  if(length(i)>0){
    xx[i,j]=T
    xx[-i,j]=F
  }
  else xx[,j]=0
  xx[,j]=as.numeric(xx[,j])
}
x_rf_w5=rowSums(xx)

xx=as.data.frame(r6_data)
for(j in 1:ncol(xx)){
  print(j)
  x=as.character(as_label(xx[,j]))
  i=grep("Refused",x)
  if(length(i)>0){
    xx[i,j]=T
    xx[-i,j]=F
  }
  else xx[,j]=0
  xx[,j]=as.numeric(xx[,j])
}
x_rf_w6=rowSums(xx)


xx=as.data.frame(r7_data)
for(j in 1:ncol(xx)){
  print(j)
  x=as.character(as_label(xx[,j]))
  i=grep("Refused",x)
  if(length(i)>0){
    xx[i,j]=T
    xx[-i,j]=F
  }
  else xx[,j]=0
  xx[,j]=as.numeric(xx[,j])
}
x_rf_w7=rowSums(xx)

x_rf=c(x_rf_w2,x_rf_w3,x_rf_w4,x_rf_w5,x_rf_w6,x_rf_w7)


### missing values



xx=as.data.frame(is.na(r2_data))
xx=as.matrix(xx)

x_mv_w2=rowSums(xx)


xx=as.data.frame(is.na(r3_data))
xx=as.matrix(xx)

x_mv_w3=rowSums(xx)

xx=as.data.frame(is.na(r4_data))
xx=as.matrix(xx)

x_mv_w4=rowSums(xx)

xx=as.data.frame(is.na(r5_data))
xx=as.matrix(xx)

x_mv_w5=rowSums(xx)

xx=as.data.frame(is.na(r6_data))
xx=as.matrix(xx)

x_mv_w6=rowSums(xx)

xx=as.data.frame(is.na(r7_data))
xx=as.matrix(xx)

x_mv_w7=rowSums(xx)

x_mv=c(x_mv_w2,x_mv_w3,x_mv_w4,x_mv_w5,x_mv_w6,x_mv_w7)


d0$dontknow=x_dk
d0$refused=x_rf
  d0$missingvalues=x_mv


### number characters

library(sjlabelled)
x_nchar=c(rep(sum(sapply(X = get_label(r2_data),FUN = nchar)),nrow(r2_data)),
          rep(sum(sapply(X = get_label(r3_data),FUN = nchar)),nrow(r3_data)),
          rep(sum(sapply(X = get_label(r4_data),FUN = nchar)),nrow(r4_data)),
          rep(sum(sapply(X = get_label(r5_data),FUN = nchar)),nrow(r5_data)),
          rep(sum(sapply(X = get_label(r6_data),FUN = nchar)),nrow(r6_data)),
          rep(sum(sapply(X = get_label(r7_data),FUN = nchar)),nrow(r7_data)))

d0$ncharact=as.vector(x_nchar)


### same gender 
d0$interviewerGender[d0$interviewerGender=="Missing"]=NA
d0$interviewerGender=droplevels(d0$interviewerGender)
d0$samegender=1*(d0$gender_respondent==d0$interviewerGender)

d0$samelanginterresp=1*(d0$interviewer_lang==d0$resp_lang)
d0$samelanginterint=1*(d0$interviewer_lang==d0$interview_lang)
d0$samelangintresp=1*(d0$resp_lang==d0$interview_lang)


d0$language=d0$samelanginterresp+d0$samelanginterint+d0$samelangintresp
xtabs(~d0$language)


###


x_ncol=c(rep(ncol(r2_data),nrow(r2_data)),
          rep(ncol(r3_data),nrow(r3_data)),
          rep(ncol(r4_data),nrow(r4_data)),
          rep(ncol(r5_data),nrow(r5_data)),
          rep(ncol(r6_data),nrow(r6_data)),
          rep(ncol(r7_data),nrow(r7_data)))
d0$cquest=x_ncol


####

d0$durinterv=d0$durinterv/60


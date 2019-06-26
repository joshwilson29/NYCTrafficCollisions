library(tools)
for (year in c('2015')) {#,'2015','2016','2017')) {
  df=NULL
  setwd('~/Documents/Classes/W210 - Capstone/NYCTrafficCollisions/data')
  setwd(year)
  for (filename in list.files()) {
    print(paste('processing',filename))
    if (file_ext(filename) == "csv") {
      dftmp=read.csv(filename)
      l=NULL
      for (name in names(dftmp)) {
        if (name!='ST_CASE') {
          l=c(l,paste(file_path_sans_ext(filename),name,sep='_'))
        } else {
          l=c(l,name)
        }
      }
      names(dftmp)=l
    }
    if (is.null(df)) {
      df=dftmp
    } else {
      df=merge(df,dftmp,by='ST_CASE')
    }
  }
}


library(dplyr)
library(MASS)
library(fitdistrplus)
year='2014'
setwd('~/Documents/Classes/W210 - Capstone/NYCTrafficCollisions/data')
setwd(year)
df14=read.csv('accident.csv')
df14['md']=df14$accident_MONTH*100+df$accident_DAY
df14.agg=df14 %>% group_by(md) %>% 
  select(fatals,ppl,grep("factor",names(df14.agg)),accident_FATALS,accident_PERSONS) %>% 
    summarise(fatals=sum(accident_FATALS),ppl=mean(accident_PERSONS),
                .-accident_FATALS-accident_PERSONS=sum(.-accident_FATALS-accident_PERSONS))

  
%>% mutate(count=n(),
                                             fatals=sum(accident_FATALS),
                                             ppl=mean(accident_PERSONS)
                                             ) %>% 
m=glm.nb(fatals~.,df14.agg)
summary(m)                                             
m2=glm.nb(fatals~.-count,df14.agg)
anova(m,m2)
mod

df14=data.frame(model.matrix(~factor(accident_STATE)-1,df14),df14)
df14['md']=df14$accident_MONTH*100+df$accident_DAY
df14.agg=df14 %>% group_by(md) %>% 
    summarise(count=n(),fatals=sum(accident_FATALS),ppl=mean(accident_PERSONS),
                drunk=mean(accident_DRUNK_DR))

df17=data.frame(model.matrix(~factor(accident_STATE)-1,df17),df17)
df17['md']=df17$accident_MONTH*100+df$accident_DAY
df17.agg=df17 %>% group_by(md) %>% 
    summarise(count=n(),fatals=sum(accident_FATALS),ppl=mean(accident_PERSONS),
                drunk=mean(accident_DRUNK_DR))
m=glm.nb(fatals~.-md,df17.agg)
summary(m)                                             
m2=glm.nb(fatals~.-md-drunk,df17.agg)
anova(m,m2)

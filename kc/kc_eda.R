library(dplyr)
library(ggplot2)
library(lubridate)
eda = function(sep,sep2,long,lat,var,namesep,namesep2,namelong,namelat,namevar) {
  dfplot=data.frame(c(sep,var))
  names(dfplot) = c('group','target')
  p = ggplot(dfplot,aes(x=factor(group),fill=factor(target)))   + geom_histogram(stat='count')  +xlab(namesep)   + guides(fill=guide_legend(title=namevar)) + theme(axis.text.x = element_text(angle = 90))
  print(p)
  
  # dfplot=data.frame(c(sep,sep2,var))
  # names(dfplot) = c('sep1','sep2','var')
  
  # dfplot = dfplot %>% group_by(sep1,sep2) %>% mutate('var'=sum(var)) %>% unique %>% date.frame
  # p = ggplot(dfplot,aes(x=sep2,y=var,color=factor(sep1))) + geom_line()
  # print(p)
  
  # p = ggplot(dfplot %>% group_by(sep2),aes(x=factor(sep1),y=var)) + geom_boxplot() + xlab(namesep2) + ylab(varname)
  # print(p)
  
  # p = ggplot(dfplot %>% group_by(sep1),aes(x=factor(sep2),y=var)) + geom_boxplot() + xlab(namesep2) + ylab(varname)
  # print(p) 
  
  # dfplot = dfplot %>% group_by(sep,sep2) %>% mutate(namevar=sum(var)) %>% data.frame()
  # p=ggplot(dfplot,aes(x=sep2,y=var,color=factor(sep1))) + geom_line() + xlab(namesep2) + ylab(varname)
  # print(p)
  dfplot=data.frame(c(sep,var))
  names(dfplot) = c('group','target') 
  dfplot=dfplot %>% group_by(target) %>% count() %>% data.frame %>% mutate(pct=n/dim(dfplot)[1]) %>% data.frame
  dfplot=unique(dfplot)
  dfplot=dfplot[order(dfplot$pct,decreasing=TRUE),]
  names(dfplot)[1]=c(namevar)
  dfplot=head(dfplot)
  print(head(dfplot))
  print(sum(dfplot$pct))
  k=dfplot[1,1]
  dfplot=data.frame(c(sep,long,lat,var))
  names(dfplot) = c('sep','long','lat','var')
  p=ggplot(dfplot[dfplot$var!=k,],aes(x=long,y=lat,color=factor(var))) + geom_point()
  print(p) 

}

map_var = function(k,sep,sep2,long,lat,var,namesep,namesep2,namelong,namelat,namevar) {
 
  dfplot=data.frame(c(sep,long,lat,var))
  names(dfplot) = c('sep','long','lat','var')
  p=ggplot(dfplot[dfplot$var==k,],aes(x=long,y=lat,color=factor(var))) + geom_point()
  print(p) 

}

map_varne = function(k,sep,sep2,long,lat,var,namesep,namesep2,namelong,namelat,namevar) {
  dfplot=data.frame(c(sep,long,lat,var))
  names(dfplot) = c('sep','long','lat','var')
  p=ggplot(dfplot[dfplot$var!=k,],aes(x=long,y=lat,color=factor(var))) + geom_point()
  print(p) 

}

print_all_xtabs = function(dftmp){
  for (i in names(dftmp)){
    if ((i!='id') | (i!='veh_no')){
      dftmp2=data.frame(df$veh_no, df[i])
      names(dftmp2)=c('veh_no','var')
      x=xtabs(var~veh_no,dftmp2)
      #print('************************')
      # print(i)
      #print(x[1:7])
      #print((x/sum(x))[1:7])
      #print(cumsum((x/sum(x)))[1:7])
      print(paste(i,cumsum((x/sum(x)))[4]))
    }
  }
}

# for (i in names(df)) {
#   eda(df['state'],df[i],'state',i)
# }
setwd('~/Documents/Classes/W210 - Capstone/NYCTrafficCollisions/')
df=read.csv('kc_eda_vehicle.csv')
df$tstamp=strptime(as.factor(df$tstamp),'%Y-%m-%d %H:%M:%S')
df$year=year(df$tstamp)
names(df)

i='numoccs'  # number of occupants - cut off at 4
xt(df['veh_no'],df[i])
eda(df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_var(3,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)

i='unittype'  # Motor Vehicle Type - all 1
xt(df['veh_no'],df[i])
eda(df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_var(3,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)

i='hit_run'  # hit & run, 9 == unknown - seems like clustering in 9
eda(df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_var(9,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)

i='reg_stat'  # registration state -  nothing too intersting... maybe state 48
eda(df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_var(9,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)

i='owner'     #can combine 1 & 2 (private owner (driver, etc))
# 9 - unkown - what should we do with these?
eda(df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
# busi/company/govt vehicle
map_var(3,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
# rental
map_var(4,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
# stolen
map_var(5,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
# driverless
map_var(6,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
# unkonwn
map_var(9,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)

i='make'  # brand of car
# examine if any state is particulary different from the general
# distribution
eda(df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_var(9,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)

i='model'  # cluster 1-399 as 1 (all automobiles),401-499 (light trucks)
# 598-599 (Low speed electric vehicles - Golf Carts)
# 701-709 (Motorcycles/mopeds)
# 731-734 ATVs
# 801-809 (Medium/Heavy truck)
# 850 Motor Homes
# 870-898 Trucks
# 901-989 Buses
# 997 - not reported
# 998 - Other (Vehicle)?
# 999 - Unknown
eda(df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_var(9,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)

i='body_typ'  #interesting - look in data_dict 
eda(df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_var(9,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)

i='mod_year'  #interesting
eda(df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_var(9,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)

i='gvwr'  # top 3 should be neough - vehicle weight rating
eda(df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_var(9,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)

i='spec_use'  #  top 2 if that 2nd most = unkown - probably drop?
# Note that taxi is the lowest, very interesting
eda(df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_var(1,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)

i='trav_sp'  #  travel_sp - interesting 997 > 151mph,  998-999 unknown/not reported
# 998-999 are the   top 2
# 997 is not interesting
eda(df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_var(997,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)

i='underide'  #seems kind of random - prbably better to ignore all not 0 < 0.5% each
eda(df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_var(1,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_varne(0,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)

i='rollover'  # rollovers - 9 is unknown (2 vehicles max)
eda(df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_var(9,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_varne(0,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)

i='rolinloc'  # where id the rollover occur - 0, 5 and 1 should be enough
eda(df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_var(9,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_varne(0,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)

i='deformed'  # vehicle deformities - interesting - #6 disabling - #4 functional (like broken glass, tires, etc)
# #2 - minor damnage (vehicle was not towed) #8 - not reported - can eliminate #9 unknown can eliminate
eda(df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_var(2,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_varne(0,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
# interesting on minor damage
# state #36 & #53 - very large % of unreported

i='m_harm'  # top 5 = 88% - after < 1.6% - most harmful event
eda(df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_var(2,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_varne(0,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)

i='vtrafway'  # type of traffic way - median etc, 1-6, 8 & 9 are unreported
# may need prior
eda(df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_var(2,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_varne(0,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)

i='vnum_lan'  # # of lanes - Think we ignore - not very intersting - 8-9 remove; need prior
eda(df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_var(2,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_varne(0,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)

i='vspd_lim'  # speed limit on the road for the vehicle type just prior to accident
eda(df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_var(2,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_varne(0,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)

i='valign'  # roadway - alignment 8-9 unkonwn 0 = driveway, 8 - not reported
eda(df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_var(2,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_varne(0,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)

i='vprofile'  # grade of the roadway - 8 & 9 - unreported and unkown
eda(df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_var(2,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_varne(0,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)

i='vpavetyp'  # type of roadway surface - top 3 is enough 22% unreported @ #2 
# top (2 coding) is blacktop - may not be very informative, 3rd is concrete
# leave out - probably geographically located
eda(df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_var(4,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_varne(0,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)

i='vsurcond'  # vehicle surface condition, top 5 is enough
# need priors
eda(df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_var(4,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_varne(0,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)

i='vtrafcon'  # vehicle traffic control condition, lights (0 - no controls)
# need priors
eda(df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_var(4,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_varne(0,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)

i='vtcont_f'  # whther vtrafcond is working, trafflic lights (3-8)
# leave out - no enough info
eda(df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_var(4,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_varne(0,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)

i='p_crash1'  # Pre-crash event 1 (prior to crash) - can merge some of these together
eda(df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_var(4,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_varne(0,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)

i='p_crash2'  # Pre-crash event 2 (event that made the crash imminent)
# very scattered - top 5 = 52%; probably need to code them all
# probably can gather some together
eda(df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_var(4,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_varne(0,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)

i='p_crash3'  # (Attempted avoidenace) - 99 & 1 = ~81% no attempt or unreported
# can combine - most common = steer left or right (6-7)
# third = braking
eda(df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_var(4,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_varne(0,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)

i='pcrash4'  # pre-impact stability - if there's braking
# can combine into 1 (tracking), 9 (unknown) and everything else (skidding/loss of control)
eda(df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_var(4,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_varne(0,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)

i='pcrash5'  # impact location - can combine 1 4 ,2 ,3 needed - drop rest
eda(df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_var(4,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_varne(0,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)

i='acc_type'  # impact location - can combine 1 4 ,2 ,3 needed - drop rest
# can combine
eda(df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_var(4,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_varne(0,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)

df=read.csv('kc_eda_veh_aux.csv')
df$tstamp=strptime(as.factor(df$tstamp),'%Y-%m-%d %H:%M:%S')
df$year=year(df$tstamp)
names(df)

# No data dictionary on X

i='a_drdro'        # drowsy driver - 1 - Yes, 2 - no
eda(df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_var(4,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_varne(0,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)

i='a_imp1'        # initial impact point 1- 1st 5
eda(df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_var(4,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_varne(0,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)

i='a_imp2'        # principal impact point - remove aall NAs
eda(df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_var(4,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_varne(0,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)

i='a_lic_s'        # licenses status top 2 only - 1 = invalid 2= valid 3 & 4 = unknown/not applicable
eda(df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_var(4,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)
map_varne(0,df['state'],df['year'],df['longitud'],df['latitude'],df[i],'state','year', 'longitude','latitude',i)

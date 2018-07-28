
##################################################################################################################################
##################################################################################################################################
############################## code 'I missed my flight because of rain, so I made this instead' #################################
##################################################################################################################################
##################################################################################################################################


########################################
####### load libraries and data ########
########################################

library(tidyverse)
library(stringr)
library(lubridate)
setwd("C://users/nickt/desktop")
D.weath<-readLines("USW00093721.dly")

##########################
####### code base ########
##########################

numberOfDays <- function(date) {
  m <- format(date, format="%m")
  weat
  while (format(date, format="%m") == m) {
    date <- date + 1
  }
  
  return(as.integer(format(date - 1, format="%d")))
}

find_rainy_periods<-function(vec, sun_par){
  rain_counter=0
  sun_counter=0
  max_rain_counter=0
  max_rain_start=1
  rain_start=1
  
  for(i in 1:length(vec)){
    if(vec[i]>0){
      if(sun_counter<=sun_par){
        rain_counter=rain_counter+1
        
        if(rain_counter>max_rain_counter){
          max_rain_counter=rain_counter
          max_rain_start=rain_start
        }
      }else{
        rain_start=i
        rain_counter=1
      }
      sun_counter=0
    }else{
      sun_counter=sun_counter+1
      
      if(sun_counter<=sun_par){
        rain_counter=rain_counter+1
      }else{
        rain_counter=0
      }
    }
  }
  return(list("most_sequential_rain_days"=max_rain_counter, "rain_start"=max_rain_start))
}

create_rainy_plot<-function(D, sun_par){
  month_list = c("January","Feburary","March","April","May","June","July","August","September","October","November","December")
  rain_period<-find_rainy_periods(D$value, sun_par)
  rain_inds <-  rain_period$rain_start:(rain_period$rain_start+rain_period$most_sequential_rain_days)
  D.rain.seg<-D[rain_inds[-length(rain_inds)],]
  break_steps<-round((range(D.rain.seg$value)/5)[2])
  break_seqs<-seq(round(min(D.rain.seg$value)), max(D.rain.seg$value), break_steps)
  break_seqs[length(break_seqs)]<-max(D.rain.seg$value)
  break_labs <-str_c(break_seqs[1:(length(break_seqs)-1)]," to ",break_seqs[2:length(break_seqs)], " mm")
  colors<-brewer.pal(length(break_labs)+1, name="Greys")[-1]
  if(min(D.rain.seg$value)==0){
    break_seqs<-c(break_seqs[1], 1, break_seqs[2:length(break_seqs)])
    break_labs <- c("0 mm",str_c(break_seqs[2:(length(break_seqs)-1)]," to ",break_seqs[3:length(break_seqs)], " mm"))
    colors<-brewer.pal(length(break_labs)+1, name="Greys")
  }
  
  titles<-str_c("Longest period of rain without ", sun_par+1, " consecutive days of sun")
  if(sun_par==0){
    titles <- "Longest period of rain without any days of sun"
  }
  
  D.rain.seg%>%mutate(day=day(date), month= str_c(month_list[month(date)]," ", year(date)),
                         val_lev=cut(value,breaks=break_seqs, include.lowest = TRUE))%>%
    mutate(month=factor(month, levels=unique((month)[rev(order(date))])))%>%ggplot(aes(y = month, x = day)) +
    geom_tile(aes(fill = val_lev), color = "#808080") + 
    scale_fill_manual(values=colors, name="Amount of rain", labels=break_labs)+
  theme_scatter()+labs(title=titles, x="Day", y="Month")+coord_fixed(expand=TRUE)
  
}


single_row_extract<-function(vec){
  ID<-str_sub(vec,1,11)
  yr<-str_sub(vec,12,15)
  mo<-str_sub(vec,16,17)
  element<-str_sub(vec, 18,21)
  dates<-str_c(yr, "-",mo)%>%parse_date_time("y-m")%>%as.Date
  days_in_month<-numberOfDays(dates)
  start_inds<-seq(from=22, to=22+8*days_in_month, by=8)
  val<-str_sub(vec, start_inds, start_inds+4)%>%trimws
  val<-val[-length(val)]
  mflag<-str_sub(vec,start_inds+5,start_inds+5) 
  mflag<-mflag[-length(mflag)]
  qflag<-str_sub(vec,start_inds+6,start_inds+6) 
  qflag<-qflag[-length(qflag)]
  sflag<-str_sub(vec,start_inds+7,start_inds+7) 
  sflag<-sflag[-length(sflag)]
  row.dat <- data.frame(ID=ID, date=dates, value=val,element=element,mflag=mflag,qflag=qflag,sflag=sflag)
  return(row.dat)
}

theme_scatter <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Ubuntu Regular", color = "#22211d"),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank(),
      ...
    )
}

##########################
####### data prep ########
##########################
D.weath.final<-lapply(D.weath, single_row_extract)
D<-do.call(rbind, D.weath.final)

D<-read.csv("weather_baltimore.csv")
D.rain<-D%>%filter(element=="PRCP")

date_table<-table(D.rain$date)

entries <-c()
for(i in 1:length(date_table)){
  entries<-c(entries,as.character(seq(ymd(names(date_table[i])), length=date_table[i], by="day")))
}

D.rain$date<-ymd(entries)
D.rain.clean<-D.rain%>%filter(value!=-9999)

#########################
####### data vis ########
#########################

D.rain.clean%>%filter(date>ymd("1939-01-01"))%>%mutate(date_mo=ymd(str_c(str_sub(date, 1,7), "-01")))%>%group_by(date_mo)%>%
  summarise(m_rain=mean(value))%>%ggplot(aes(x=date_mo, y=m_rain))+geom_line(color="slategray4")+theme_scatter()+
  labs(x="Date", y="Cubic millimeters of rain per month", title="Average monthly rain in Baltimore", subtitle="Measured at BWI")+
  annotate("text",label="August, 1955",x=ymd("1949-08-01"), y = 150)+
  annotate("text",label="Now :(",x=ymd("2015-08-01"), y = 160)

D.rain.clean%>%filter(date>ymd("1939-01-01"))%>%mutate(date_mo=ymd(str_c(str_sub(date, 1,7), "-01")))%>%group_by(date_mo)%>%
  summarise(m_rain=median(value))%>%ggplot(aes(x=date_mo, y=m_rain))+geom_line(color="slategray4")+theme_scatter()+
  labs(x="Date", y="Cubic millimeters of rain per month", title="Median monthly rain in Baltimore", 
       subtitle="Doesn't look like much BUT A LINE IT MEANS IT RAINED MORE THAN HALF THE DAYS THAT MONTH :( :( :(")


create_rainy_plot(D.rain.clean, 1)



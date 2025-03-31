# Load required packages, fetch the latest data
source("R/Setup.R")

# Bitcoin Prices
bitcoin<-fredr("CBBTCUSD")

# Change in Bitcoin since PP-Shwarma
# bitcoin US prices since March 28
plotdata<-bitcoin %>%
  mutate(date=as.Date(date)) %>%
    filter(date>="2022-03-01")
ggplot(plotdata,aes(date,value))+
  geom_line(size=2,color=col[1])+
  geom_hline(yintercept=0,size=1)+
  geom_point(data=filter(plotdata,date=="2022-03-28"),
             color=col[1],size=2.5,stroke=2.5,fill='white',shape=21)+
  geom_point(data=filter(plotdata,date==max(date)),
             color=col[1],size=2.5,stroke=2.5,fill='white',shape=21)+
  annotate('text',x=as.Date("2022-05-01"),y=53000,hjust=0,color=col[1],size=2.5,
           label="Pierre Poilievre says: \"opt out\" of\ninflation by buying crypto")+
  annotate('text',x=max(plotdata$date)+5,
           y=filter(plotdata,date==max(date))$value+500,
           hjust=0,color=col[1],vjust=0,size=3,
           label=paste0("Down\n",percent(abs(filter(plotdata,date==max(date))$value/filter(plotdata,date==as.Date("2022-03-28"))$value-1))))+
  geom_segment(x=as.Date("2022-04-28"),y=53000,
               xend=as.Date("2022-04-01"),yend=48500,
               color=col[1],size=0.7,arrow=arrow(length=unit(1.5,'mm')))+
  scale_y_continuous(limit=c(0,55000),label=dollar,
                     breaks=pretty_breaks(5))+
  scale_x_date(limit=as.Date(c(min(plotdata$date),max(plotdata$date)+21)),
               date_labels = format("%b\n%Y"),
               date_breaks = '2 month')+
  labs(x="",y="US Dollars",
       caption='Graph by @trevortombe',
       title=paste("Bitcoin Prices (USD),",
                   gsub(" 0"," ",format(min(plotdata$date),"%B %d")),"to",
                   gsub(" 0"," ",format(max(plotdata$date),"%B %d"))),
       subtitle=paste0("Source: St. Louis FRED, Series CBBTCUSD. As of ",max(plotdata$date),"."))
ggsave("Plots/Bitcoin.png",width=8,height=4)

##############################################
# Provincial Government Responses: Gas Taxes #
##############################################

# Approximate city populations
city_pop<-read_excel('Data/city_population.xlsx')

# End of the consumer carbon tax in Canada: April 1, 2025
start="2025-02-01" # tretment start
url<-'https://charting.kalibrate.com/WPPS/Unleaded/Retail%20(Incl.%20Tax)/DAILY/2024/Unleaded_Retail%20(Incl.%20Tax)_DAILY_2024.xlsx'
GET(url, write_disk(gas_file <- tempfile(fileext = ".xlsx")))
data2024 <- read_excel(gas_file,skip=2)
colnames(data2024)[1]<-"city"
url<-'https://charting.kalibrate.com/WPPS/Unleaded/Retail%20(Incl.%20Tax)/DAILY/2025/Unleaded_Retail%20(Incl.%20Tax)_DAILY_2025.xlsx'
GET(url, write_disk(gas_file <- tempfile(fileext = ".xlsx")))
data2025 <- read_excel(gas_file,skip=2)
colnames(data2025)[1]<-"city"
clean_data<-data2024 %>%
  mutate(row=row_number()) %>%
  filter(row<=77) %>%
  gather(date,val,-city,-row) %>%
  mutate(date=paste0("2024/",date),
         date=as.Date(date,"%Y/%m/%d"),
         val=as.numeric(val)) %>%
  filter(date>as.Date("2024-01-01")) %>%
  rbind(
    data2025 %>%
      mutate(row=row_number()) %>%
      filter(row<=77) %>%
      gather(date,val,-city,-row) %>%
      mutate(date=paste0("2025/",date),
             date=as.Date(date,"%Y/%m/%d"),
             val=as.numeric(val))
  ) %>%
  mutate(province=case_when(
    row %in% seq(2,8) ~ "BC",
    row %in% seq(10,15) ~ "AB",
    row %in% seq(16,19) ~ "SK",
    row %in% seq(20,21) ~ "MB",
    row %in% seq(22,46) ~ "ON",
    row %in% seq(47,56) ~ "QC",
    row %in% seq(57,65) ~ "NB",
    row %in% seq(66,71) ~ "NS",
    row %in% seq(72,72) ~ "PE",
    row %in% seq(73,77) ~ "NL"
  )) %>%
  drop_na() %>%
  group_by(date,province) %>%
  summarise(val=mean(val)) %>% 
  ungroup() %>%
  spread(province,val)
pretreat<-clean_data %>%
  filter(date<start)
newdata<-clean_data %>%
  filter(date>=start)
model<-lm(QC~BC+AB+SK+MB+ON+NB+NS+PE+NL,data=pretreat) # pre-treatment best fit
plotdata<-pretreat %>% 
  cbind(synth=fitted(model)) %>%
  select(date,QC,synth) %>%
  rbind(
    data.frame(date=newdata$date,
               QC=newdata$QC,
               synth=predict(model,newdata %>% select(BC,AB,SK,MB,ON,NB,NS,PE,NL)))
  ) %>%
  gather(type,val,-date)
# regdata<-plotdata %>% 
#   group_by(type) %>%
#   mutate(change=val-weighted.mean(val,date=="2025-01-31")) %>%
#   select(date,type,change) %>%
#   spread(type,change) %>% 
#   mutate(gap=QC-synth) %>%
#   mutate(treated=ifelse(date>"2025-01-31",1,0))
# model2<-lm(gap~1,data=regdata %>% filter(treated==1))
# confint(model2,'(Intercept)',level=0.95)
# effect<-paste0("95% CI Est (Jan 1 to Latest):  ",percent(-confint(model2,'(Intercept)',level=0.95)/15.7,1)[2],
#                " to ",percent(-confint(model2,'(Intercept)',level=0.95)/15.7,1)[1]," passthrough")
ggplot(plotdata,aes(date,val,group=type,color=type))+
  geom_line(size=1.5)
  scale_color_manual(label=c("Manitoba","\"Synthetic Manitoba\" (Weighted Average of Other Provinces)"),
                     values=col[2:1])+
  scale_y_continuous(limit=c(110,170))+
  scale_x_date(labels=date_format("%b\n%Y"),
               date_breaks = '1 month',expand=c(0,0),
               limit=c(as.Date(start),max(plotdata$date)+30))+
  geom_vline(xintercept=as.Date("2024-01-01"),size=0.75,linetype='dashed')+
  geom_point(data=filter(plotdata,date==max(date)),size=2,stroke=2,shape=21,
             fill='white',show.legend = F)+
  annotate('text',x=as.Date("2023-12-28"),y=165,hjust=1,size=2,
           label="MB Prov Gas Tax\nFully Suspended (-14c/L)")+
  annotate('text',x=as.Date("2024-01-05"),y=110,hjust=0,
           label=effect,color=col[3],size=2)+
  geom_segment(x=max(plotdata$date)+5,xend=max(plotdata$date)+5,
               y=filter(plotdata,(type=="synth"&date==max(date)))$val,
               yend=filter(plotdata,(type=="MB"&date==max(date)))$val,
               arrow=arrow(length=unit(1,'mm')),
               size=0.75,color=col[3])+
  annotate('text',x=max(plotdata$date)+6,
           y=mean(filter(plotdata,date==max(date))$val),
           size=3,color=col[3],
           label=paste0("  Price Gap\n  Estimate for\n  ",
                        gsub(" 0"," ",format(max(plotdata$date),"%b %d, %Y")),
                        ":\n  ",change),hjust=0)+
  labs(x="",y="Cents per Litre",
       title="Effect of Suspending the Manitoba Gas Tax on Prices",
       caption='Source: own calculations from daily Kalibrate DPPS data\nGraph by @trevortombe',
       subtitle=paste0("Displays average prices in Manitoba compared to a \"synthetic Manitoba\" composed of a fixed-weighted average of other
provinces, excluding Alberta, with weights selected to best fit the period prior to the tax change. Data to ",
                       gsub(" 0"," ",format(max(newdata$date),"%B %d, %Y")),"."))
ggsave('Plots/gas_ctax_2025.png',width=8,height=4.5)


plotdata<-clean_data %>%
  filter(date>=as.Date("2025-01-01")) %>%
  mutate(RoC=ifelse(province=="QC","Quebec","Rest of Canada")) %>%
  group_by(date,RoC) %>%
  summarise(val=mean(val))
ggplot(plotdata,aes(date,val,group=RoC,color=RoC))+
  geom_line()

# Gas Tax Holiday in Manitoba, Jan 1, 2024
start="2023-10-01" # pre-tretment start
url<-'https://charting.kalibrate.com/WPPS/Unleaded/Retail%20(Incl.%20Tax)/DAILY/2023/Unleaded_Retail%20(Incl.%20Tax)_DAILY_2023.xlsx'
GET(url, write_disk(gas_file <- tempfile(fileext = ".xlsx")))
data2023 <- read_excel(gas_file,skip=2)
colnames(data2023)[1]<-"city"
pretreat<-data2023 %>%
  mutate(row=row_number()) %>%
  filter(row<=77) %>%
  gather(date,val,-city,-row) %>%
  mutate(date=paste0("2023/",date),
         date=as.Date(date,"%Y/%m/%d"),
         val=as.numeric(val)) %>%
  mutate(province=case_when(
    row %in% seq(2,8) ~ "BC",
    row %in% seq(10,15) ~ "AB",
    row %in% seq(16,19) ~ "SK",
    row %in% seq(20,21) ~ "MB",
    row %in% seq(22,46) ~ "ON",
    row %in% seq(47,56) ~ "QC",
    row %in% seq(57,65) ~ "NB",
    row %in% seq(66,71) ~ "NS",
    row %in% seq(72,72) ~ "PE",
    row %in% seq(73,77) ~ "NL"
  )) %>%
  drop_na() %>%
  group_by(date,province) %>%
  summarise(val=mean(val)) %>% 
  ungroup() %>%
  filter(date>=start) %>%
  spread(province,val)
url<-'https://charting.kalibrate.com/WPPS/Unleaded/Retail%20(Incl.%20Tax)/DAILY/2024/Unleaded_Retail%20(Incl.%20Tax)_DAILY_2024.xlsx'
GET(url, write_disk(gas_file <- tempfile(fileext = ".xlsx")))
new <- read_excel(gas_file,skip=2)
colnames(new)[1]<-"city"
newdata<-new %>%
  mutate(row=row_number()) %>%
  filter(row<=77) %>%
  gather(date,val,-city,-row) %>%
  mutate(date=paste0("2024/",date),
         date=as.Date(date,"%Y/%m/%d"),
         val=as.numeric(val)) %>%
  mutate(province=case_when(
    row %in% seq(2,8) ~ "BC",
    row %in% seq(10,15) ~ "AB",
    row %in% seq(16,19) ~ "SK",
    row %in% seq(20,21) ~ "MB",
    row %in% seq(22,46) ~ "ON",
    row %in% seq(47,56) ~ "QC",
    row %in% seq(57,65) ~ "NB",
    row %in% seq(66,71) ~ "NS",
    row %in% seq(72,72) ~ "PE",
    row %in% seq(73,77) ~ "NL"
  )) %>%
  drop_na() %>%
  group_by(date,province) %>%
  summarise(val=mean(val)) %>% 
  ungroup() %>%
  # filter(date<="2023-03-01") %>%
  spread(province,val)
model<-lm(MB~BC+SK+ON+QC+NB+NS+PE+NL,data=pretreat) # pre-treatment best fit
plotdata<-pretreat %>% 
  cbind(synth=fitted(model)) %>%
  select(date,MB,synth) %>%
  rbind(
    data.frame(date=newdata$date,
               MB=newdata$MB,
               synth=predict(model,newdata %>% select(BC,AB,SK,ON,QC,NB,NS,PE,NL)))
  ) %>%
  gather(type,val,-date)
# average passthrough estimate
regdata<-plotdata %>% 
  group_by(type) %>%
  mutate(change=val-weighted.mean(val,date=="2024-01-01")) %>%
  select(date,type,change) %>%
  spread(type,change) %>% 
  mutate(gap=MB-synth) %>%
  mutate(treated=ifelse(date>"2024-01-01",1,0))
model2<-lm(gap~1,data=regdata %>% filter(treated==1))
confint(model2,'(Intercept)',level=0.95)
effect<-paste0("95% CI Est (Jan 1 to Latest):  ",percent(-confint(model2,'(Intercept)',level=0.95)/15.7,1)[2],
               " to ",percent(-confint(model2,'(Intercept)',level=0.95)/15.7,1)[1]," passthrough")
# latest passthrough
change<-(plotdata %>% 
           group_by(type) %>%
           mutate(change=val-weighted.mean(val,date=="2024-01-01")) %>%
           filter(date==max(date)) %>%
           select(type,change) %>%
           spread(type,change) %>%
           mutate(drop=paste(round(synth-MB,1),"c/L"))) %>%
  select(drop) %>%
  as.character()
ggplot(plotdata,aes(date,val,group=type,color=type))+
  geom_line(size=1.5)+
  scale_color_manual(label=c("Manitoba","\"Synthetic Manitoba\" (Weighted Average of Other Provinces)"),
                     values=col[2:1])+
  scale_y_continuous(limit=c(110,170))+
  scale_x_date(labels=date_format("%b\n%Y"),
               date_breaks = '1 month',expand=c(0,0),
               limit=c(as.Date(start),max(plotdata$date)+30))+
  geom_vline(xintercept=as.Date("2024-01-01"),size=0.75,linetype='dashed')+
  geom_point(data=filter(plotdata,date==max(date)),size=2,stroke=2,shape=21,
             fill='white',show.legend = F)+
  annotate('text',x=as.Date("2023-12-28"),y=165,hjust=1,size=2,
           label="MB Prov Gas Tax\nFully Suspended (-14c/L)")+
  annotate('text',x=as.Date("2024-01-05"),y=110,hjust=0,
           label=effect,color=col[3],size=2)+
  geom_segment(x=max(plotdata$date)+5,xend=max(plotdata$date)+5,
               y=filter(plotdata,(type=="synth"&date==max(date)))$val,
               yend=filter(plotdata,(type=="MB"&date==max(date)))$val,
               arrow=arrow(length=unit(1,'mm')),
               size=0.75,color=col[3])+
  annotate('text',x=max(plotdata$date)+6,
           y=mean(filter(plotdata,date==max(date))$val),
           size=3,color=col[3],
           label=paste0("  Price Gap\n  Estimate for\n  ",
                        gsub(" 0"," ",format(max(plotdata$date),"%b %d, %Y")),
                        ":\n  ",change),hjust=0)+
  labs(x="",y="Cents per Litre",
       title="Effect of Suspending the Manitoba Gas Tax on Prices",
       caption='Source: own calculations from daily Kalibrate DPPS data\nGraph by @trevortombe',
       subtitle=paste0("Displays average prices in Manitoba compared to a \"synthetic Manitoba\" composed of a fixed-weighted average of other
provinces, excluding Alberta, with weights selected to best fit the period prior to the tax change. Data to ",
                       gsub(" 0"," ",format(max(newdata$date),"%B %d, %Y")),"."))
ggsave('Plots/gas_tax_mb.png',width=8,height=4.5)

# Gas Tax Holiday in Manitoba, Jan 1, 2024 - DIESEL
start="2023-10-01" # pre-tretment start
url<-'https://charting.kalibrate.com/WPPS/Diesel/Retail%20(Incl.%20Tax)/DAILY/2023/Diesel_Retail%20(Incl.%20Tax)_DAILY_2023.xlsx'
GET(url, write_disk(gas_file <- tempfile(fileext = ".xlsx")))
data2023 <- read_excel(gas_file,skip=2)
colnames(data2023)[1]<-"city"
pretreat<-data2023 %>%
  mutate(row=row_number()) %>%
  filter(row<=77) %>%
  gather(date,val,-city,-row) %>%
  mutate(date=paste0("2023/",date),
         date=as.Date(date,"%Y/%m/%d"),
         val=as.numeric(val)) %>%
  mutate(province=case_when(
    row %in% seq(2,8) ~ "BC",
    row %in% seq(10,15) ~ "AB",
    row %in% seq(16,19) ~ "SK",
    row %in% seq(20,21) ~ "MB",
    row %in% seq(22,46) ~ "ON",
    row %in% seq(47,56) ~ "QC",
    row %in% seq(57,65) ~ "NB",
    row %in% seq(66,71) ~ "NS",
    row %in% seq(72,72) ~ "PE",
    row %in% seq(73,77) ~ "NL"
  )) %>%
  drop_na() %>%
  group_by(date,province) %>%
  summarise(val=mean(val)) %>% 
  ungroup() %>%
  filter(date>=start) %>%
  spread(province,val)
url<-'https://charting.kalibrate.com/WPPS/Diesel/Retail%20(Incl.%20Tax)/DAILY/2024/Diesel_Retail%20(Incl.%20Tax)_DAILY_2024.xlsx'
GET(url, write_disk(gas_file <- tempfile(fileext = ".xlsx")))
new <- read_excel(gas_file,skip=2)
colnames(new)[1]<-"city"
newdata<-new %>%
  mutate(row=row_number()) %>%
  filter(row<=77) %>%
  gather(date,val,-city,-row) %>%
  mutate(date=paste0("2024/",date),
         date=as.Date(date,"%Y/%m/%d"),
         val=as.numeric(val)) %>%
  mutate(province=case_when(
    row %in% seq(2,8) ~ "BC",
    row %in% seq(10,15) ~ "AB",
    row %in% seq(16,19) ~ "SK",
    row %in% seq(20,21) ~ "MB",
    row %in% seq(22,46) ~ "ON",
    row %in% seq(47,56) ~ "QC",
    row %in% seq(57,65) ~ "NB",
    row %in% seq(66,71) ~ "NS",
    row %in% seq(72,72) ~ "PE",
    row %in% seq(73,77) ~ "NL"
  )) %>%
  drop_na() %>%
  group_by(date,province) %>%
  summarise(val=mean(val)) %>% 
  ungroup() %>%
  # filter(date<="2023-03-01") %>%
  spread(province,val)
model<-lm(MB~BC+SK+ON+QC+NB+NS+PE+NL,data=pretreat) # pre-treatment best fit
plotdata<-pretreat %>% 
  cbind(synth=fitted(model)) %>%
  select(date,MB,synth) %>%
  rbind(
    data.frame(date=newdata$date,
               MB=newdata$MB,
               synth=predict(model,newdata %>% select(BC,AB,SK,ON,QC,NB,NS,PE,NL)))
  ) %>%
  gather(type,val,-date)
# average passthrough estimate
regdata<-plotdata %>% 
  group_by(type) %>%
  mutate(change=val-weighted.mean(val,date=="2024-01-01")) %>%
  select(date,type,change) %>%
  spread(type,change) %>% 
  mutate(gap=MB-synth) %>%
  mutate(treated=ifelse(date>"2024-01-01",1,0))
model2<-lm(gap~1,data=regdata %>% filter(treated==1))
confint(model2,'(Intercept)',level=0.95)
effect<-paste0("95% CI Est (Jan 1 to Latest):  ",percent(-confint(model2,'(Intercept)',level=0.95)/15.7,1)[2],
               " to ",percent(-confint(model2,'(Intercept)',level=0.95)/15.7,1)[1]," passthrough")
# latest passthrough
change<-(plotdata %>% 
           group_by(type) %>%
           mutate(change=val-weighted.mean(val,date=="2024-01-01")) %>%
           filter(date==max(date)) %>%
           select(type,change) %>%
           spread(type,change) %>%
           mutate(drop=paste(round(synth-MB,1),"c/L"))) %>%
  select(drop) %>%
  as.character()
ggplot(plotdata,aes(date,val,group=type,color=type))+
  geom_line(size=1.5)+
  scale_color_manual(label=c("Manitoba","\"Synthetic Manitoba\" (Weighted Average of Other Provinces)"),
                     values=col[2:1])+
  scale_y_continuous(limit=c(155,190))+
  scale_x_date(labels=date_format("%b\n%Y"),
               date_breaks = '1 month',expand=c(0,0),
               limit=c(as.Date(start),max(plotdata$date)+30))+
  geom_vline(xintercept=as.Date("2024-01-01"),size=0.75,linetype='dashed')+
  geom_point(data=filter(plotdata,date==max(date)),size=2,stroke=2,shape=21,
             fill='white',show.legend = F)+
  annotate('text',x=as.Date("2023-12-28"),y=165,hjust=1,size=2,
           label="MB Prov Gas Tax\nFully Suspended (-14c/L)")+
  annotate('text',x=as.Date("2024-01-05"),y=155,hjust=0,
           label=effect,color=col[3],size=2)+
  geom_segment(x=max(plotdata$date)+5,xend=max(plotdata$date)+5,
               y=filter(plotdata,(type=="synth"&date==max(date)))$val,
               yend=filter(plotdata,(type=="MB"&date==max(date)))$val,
               arrow=arrow(length=unit(1,'mm')),
               size=0.75,color=col[3])+
  annotate('text',x=max(plotdata$date)+6,
           y=mean(filter(plotdata,date==max(date))$val),
           size=3,color=col[3],
           label=paste0("  Price Gap\n  Estimate for\n  ",
                        gsub(" 0"," ",format(max(plotdata$date),"%b %d, %Y")),
                        ":\n  ",change),hjust=0)+
  labs(x="",y="Cents per Litre",
       title="Effect of Suspending the Manitoba Gas Tax on Prices",
       caption='Source: own calculations from daily Kalibrate DPPS data\nGraph by @trevortombe',
       subtitle=paste0("Displays average prices in Manitoba compared to a \"synthetic Manitoba\" composed of a fixed-weighted average of other
provinces, excluding Alberta, with weights selected to best fit the period prior to the tax change. Data to ",
                       gsub(" 0"," ",format(max(newdata$date),"%B %d, %Y")),"."))
ggsave('Plots/diesel_tax_mb.png',width=8,height=4.5)

# Gas Taxes return in AB, Jan 1, 2024
start="2023-10-01" # pre-tretment start
url<-'https://charting.kalibrate.com/WPPS/Unleaded/Retail%20(Incl.%20Tax)/DAILY/2023/Unleaded_Retail%20(Incl.%20Tax)_DAILY_2023.xlsx'
GET(url, write_disk(gas_file <- tempfile(fileext = ".xlsx")))
data2023 <- read_excel(gas_file,skip=2)
colnames(data2023)[1]<-"city"
pretreat<-data2023 %>%
  mutate(row=row_number()) %>%
  filter(row<=77) %>%
  gather(date,val,-city,-row) %>%
  mutate(date=paste0("2023/",date),
         date=as.Date(date,"%Y/%m/%d"),
         val=as.numeric(val)) %>%
  mutate(province=case_when(
    row %in% seq(2,8) ~ "BC",
    row %in% seq(10,15) ~ "AB",
    row %in% seq(16,19) ~ "SK",
    row %in% seq(20,21) ~ "MB",
    row %in% seq(22,46) ~ "ON",
    row %in% seq(47,56) ~ "QC",
    row %in% seq(57,65) ~ "NB",
    row %in% seq(66,71) ~ "NS",
    row %in% seq(72,72) ~ "PE",
    row %in% seq(73,77) ~ "NL"
  )) %>%
  drop_na() %>%
  group_by(date,province) %>%
  summarise(val=mean(val)) %>% 
  ungroup() %>%
  filter(date>=start) %>%
  spread(province,val)
url<-'https://charting.kalibrate.com/WPPS/Unleaded/Retail%20(Incl.%20Tax)/DAILY/2024/Unleaded_Retail%20(Incl.%20Tax)_DAILY_2024.xlsx'
GET(url, write_disk(gas_file <- tempfile(fileext = ".xlsx")))
new <- read_excel(gas_file,skip=2)
colnames(new)[1]<-"city"
newdata<-new %>%
  mutate(row=row_number()) %>%
  filter(row<=77) %>%
  gather(date,val,-city,-row) %>%
  mutate(date=paste0("2024/",date),
         date=as.Date(date,"%Y/%m/%d"),
         val=as.numeric(val)) %>%
  mutate(province=case_when(
    row %in% seq(2,8) ~ "BC",
    row %in% seq(10,15) ~ "AB",
    row %in% seq(16,19) ~ "SK",
    row %in% seq(20,21) ~ "MB",
    row %in% seq(22,46) ~ "ON",
    row %in% seq(47,56) ~ "QC",
    row %in% seq(57,65) ~ "NB",
    row %in% seq(66,71) ~ "NS",
    row %in% seq(72,72) ~ "PE",
    row %in% seq(73,77) ~ "NL"
  )) %>%
  drop_na() %>%
  group_by(date,province) %>%
  summarise(val=mean(val)) %>% 
  ungroup() %>%
  # filter(date<="2023-03-01") %>%
  spread(province,val)
model<-lm(AB~BC+SK+ON+QC+NB+NS+PE+NL,data=pretreat) # pre-treatment best fit
plotdata<-pretreat %>% 
  cbind(synth=fitted(model)) %>%
  select(date,AB,synth) %>%
  rbind(
    data.frame(date=newdata$date,
               AB=newdata$AB,
               synth=predict(model,newdata %>% select(BC,SK,ON,QC,NB,NS,PE,NL)))
  ) %>%
  gather(type,val,-date)
# average passthrough estimate
regdata<-plotdata %>% 
  group_by(type) %>%
  mutate(change=val-weighted.mean(val,date=="2024-01-01")) %>%
  select(date,type,change) %>%
  spread(type,change) %>% 
  mutate(gap=AB-synth) %>%
  mutate(treated=ifelse(date>"2024-01-01",1,0))
model2<-lm(gap~1,data=regdata %>% filter(treated==1))
confint(model2,'(Intercept)',level=0.95)
effect<-paste0("95% CI Est (Jan 1 to Latest):  ",percent(confint(model2,'(Intercept)',level=0.95)/9.45,1)[1],
               " to ",percent(confint(model2,'(Intercept)',level=0.95)/9.45,1)[2]," passthrough")
# latest passthrough
change<-(plotdata %>% 
           group_by(type) %>%
           mutate(change=val-weighted.mean(val,date=="2024-01-01")) %>%
           filter(date==max(date)) %>%
           select(type,change) %>%
           spread(type,change) %>%
           mutate(drop=paste(round(AB-synth,1),"c/L"))) %>%
  select(drop) %>%
  as.character()
ggplot(plotdata,aes(date,val,group=type,color=type))+
  geom_line(size=1.5)+
  scale_color_manual(label=c("Alberta","\"Synthetic Alberta\" (Weighted Average of Other Provinces)"),
                     values=col[2:1])+
  scale_y_continuous(limit=c(110,150))+
  scale_x_date(labels=date_format("%b\n%Y"),
               date_breaks = '1 month',expand=c(0,0),
               limit=c(as.Date(start),max(plotdata$date)+30))+
  geom_vline(xintercept=as.Date("2024-01-01"),size=0.75,linetype='dashed')+
  geom_point(data=filter(plotdata,date==max(date)),size=2,stroke=2,shape=21,
             fill='white',show.legend = F)+
  annotate('text',x=as.Date("2023-12-28"),y=145,hjust=1,size=2,
           label="AB Prov Gas Tax\nPartially Reinstated (+9c/L)")+
  annotate('text',x=as.Date("2024-01-05"),y=110,hjust=0,
           label=effect,color=col[3],size=2)+
  geom_segment(x=max(plotdata$date)+5,xend=max(plotdata$date)+5,
               y=filter(plotdata,(type=="synth"&date==max(date)))$val,
               yend=filter(plotdata,(type=="AB"&date==max(date)))$val,
               arrow=arrow(length=unit(1,'mm')),
               size=0.75,color=col[3])+
  annotate('text',x=max(plotdata$date)+6,
           y=mean(filter(plotdata,date==max(date))$val),
           size=3,color=col[3],
           label=paste0("  Price Gap\n  Estimate for\n  ",
                        gsub(" 0"," ",format(max(plotdata$date),"%b %d, %Y")),
                        ":\n  ",change),hjust=0)+
  labs(x="",y="Cents per Litre",
       title="Effect of Partially Reinstating Alberta's Gas Tax",
       caption='Source: own calculations from daily Kalibrate DPPS data\nGraph by @trevortombe',
       subtitle=paste0("Displays average prices in Alberta compared to a \"synthetic Alberta\" composed of a fixed-weighted average of other
provinces, excluding Manitoba, with weights selected to best fit the period prior to the tax change. Data to ",
                       gsub(" 0"," ",format(max(newdata$date),"%B %d, %Y")),"."))
ggsave('Plots/gas_tax_ab.png',width=8,height=4.5)

# # Gas Tax Holiday in Alberta - in 2023
# start="2022-10-01" # pre-tretment start: period pf partial gas tax (4.5c/L) in AB
# url<-'https://charting.kalibrate.com/WPPS/Unleaded/Retail%20(Incl.%20Tax)/DAILY/2022/Unleaded_Retail%20(Incl.%20Tax)_DAILY_2022.xlsx'
# GET(url, write_disk(gas_file <- tempfile(fileext = ".xlsx")))
# data2022 <- read_excel(gas_file,skip=2)
# colnames(data2022)[1]<-"city"
# pretreat<-data2022 %>%
#   mutate(row=row_number()) %>%
#   filter(row<=77) %>%
#   gather(date,val,-city,-row) %>%
#   mutate(date=paste0("2022/",date),
#          date=as.Date(date,"%Y/%m/%d"),
#          val=as.numeric(val)) %>%
#   mutate(province=case_when(
#     row %in% seq(2,8) ~ "BC",
#     row %in% seq(10,15) ~ "AB",
#     row %in% seq(16,19) ~ "SK",
#     row %in% seq(20,21) ~ "MB",
#     row %in% seq(22,46) ~ "ON",
#     row %in% seq(47,56) ~ "QC",
#     row %in% seq(57,65) ~ "NB",
#     row %in% seq(66,71) ~ "NS",
#     row %in% seq(72,72) ~ "PE",
#     row %in% seq(73,77) ~ "NL"
#   )) %>%
#   drop_na() %>%
#   group_by(date,province) %>%
#   summarise(val=mean(val)) %>% 
#   ungroup() %>%
#   filter(date>=start) %>%
#   spread(province,val)
# model<-lm(AB~BC+SK+MB+ON+QC+NB+NS+PE+NL,data=pretreat) # pre-treatment best fit
# url<-'https://charting.kalibrate.com/WPPS/Unleaded/Retail%20(Incl.%20Tax)/DAILY/2023/Unleaded_Retail%20(Incl.%20Tax)_DAILY_2023.xlsx'
# GET(url, write_disk(gas_file <- tempfile(fileext = ".xlsx")))
# new <- read_excel(gas_file,skip=2)
# colnames(new)[1]<-"city"
# newdata<-new %>%
#   mutate(row=row_number()) %>%
#   filter(row<=77) %>%
#   gather(date,val,-city,-row) %>%
#   mutate(date=paste0("2023/",date),
#          date=as.Date(date,"%Y/%m/%d"),
#          val=as.numeric(val)) %>%
#   mutate(province=case_when(
#     row %in% seq(2,8) ~ "BC",
#     row %in% seq(10,15) ~ "AB",
#     row %in% seq(16,19) ~ "SK",
#     row %in% seq(20,21) ~ "MB",
#     row %in% seq(22,46) ~ "ON",
#     row %in% seq(47,56) ~ "QC",
#     row %in% seq(57,65) ~ "NB",
#     row %in% seq(66,71) ~ "NS",
#     row %in% seq(72,72) ~ "PE",
#     row %in% seq(73,77) ~ "NL"
#   )) %>%
#   drop_na() %>%
#   group_by(date,province) %>%
#   summarise(val=mean(val)) %>% 
#   ungroup() %>%
#   # filter(date<="2023-03-01") %>%
#   spread(province,val)
# plotdata<-pretreat %>% 
#   cbind(fitted=fitted(model)) %>%
#   select(date,AB,fitted) %>%
#   rbind(
#     data.frame(date=newdata$date,
#                AB=newdata$AB,
#                fitted=predict(model,newdata %>% select(BC,SK,MB,ON,QC,NB,NS,PE,NL)))
#   ) %>%
#   gather(type,val,-date)
# # average passthrough estimate
# regdata<-plotdata %>% 
#   group_by(type) %>%
#   mutate(change=val-weighted.mean(val,date=="2022-12-30")) %>%
#   select(date,type,change) %>%
#   spread(type,change) %>% 
#   mutate(gap=AB-fitted) %>%
#   mutate(treated=ifelse(date>="2023-01-01",1,0))
# model2<-lm(gap~1,data=regdata %>% filter(treated==1))
# confint(model2,'(Intercept)',level=0.95)
# effect<-paste0("95% CI Est (Jan 1 to Latest):  ",percent(-confint(model2,'(Intercept)',level=0.95)/4.5,1)[2],
#                " to ",percent(-confint(model2,'(Intercept)',level=0.95)/4.5,1)[1]," passthrough")
# # latest passthrough
# change<-(plotdata %>% 
#            group_by(type) %>%
#            mutate(change=val-weighted.mean(val,date=="2022-12-30")) %>%
#            filter(date==max(date)) %>%
#            select(type,change) %>%
#            spread(type,change) %>%
#            mutate(drop=paste(round(fitted-AB,1),"c/L"))) %>%
#   select(drop) %>%
#   as.character()
# ggplot(plotdata,aes(date,val,group=type,color=type))+
#   geom_line(size=1.5)+
#   scale_color_manual(label=c("Alberta","\"Synthetic Alberta\" (Weighted Average of Other Provinces)"),
#                      values=col[1:2])+
#   scale_y_continuous(limit=c(118,NA))+
#   scale_x_date(labels=date_format("%d\n%b"),
#                date_breaks = '1 month',expand=c(0,0),
#                limit=c(as.Date(start),max(plotdata$date)+60))+
#   geom_vline(xintercept=as.Date("2022-12-30"),size=0.75,linetype='dashed')+
#   geom_point(data=filter(plotdata,date==max(date)),size=2,stroke=2,shape=21,
#              fill='white',show.legend = F)+
#   annotate('text',x=as.Date("2022-12-28"),y=165,hjust=1,size=2,
#            label="AB Prov Gas Tax\nFully Suspended (-4.5c/L)")+
#   annotate('text',x=as.Date("2023-01-01"),y=119,hjust=0,
#            label=effect,color=col[3],size=2)+
#   geom_segment(x=max(plotdata$date)+5,xend=max(plotdata$date)+5,
#                y=filter(plotdata,(type=="fitted"&date==max(date)))$val,
#                yend=filter(plotdata,(type=="AB"&date==max(date)))$val,
#                arrow=arrow(length=unit(1,'mm')),
#                size=0.75,color=col[3])+
#   annotate('text',x=max(plotdata$date)+6,
#            y=mean(filter(plotdata,date==max(date))$val),
#            size=3,color=col[3],
#            label=paste0("  Price Gap\n  Estimate for\n  ",
#                         gsub(" 0"," ",format(max(plotdata$date),"%b %d, %Y")),
#                         ":\n  ",change),hjust=0)+
#   labs(x="",y="Cents per Litre",
#        title="Effect of Suspending the Alberta Gas Tax on Prices",
#        caption='Source: own calculations from daily Kalibrate DPPS data\nGraph by @trevortombe',
#        subtitle=paste0("Displays average prices in Alberta compared to a \"synthetic Alberta\" composed of a fixed-weighted average of
# other provinces, with weights selected to best fit the period prior to the tax change. Data to ",
#                        gsub(" 0"," ",format(max(newdata$date),"%B %d, %Y")),"."))
# ggsave('Plots/gas_tax_ab.png',width=8,height=4.5)
# 
# # Diesel in 2023
# start="2022-10-01" # pre-tretment start: period pf partial gas tax (4.5c/L) in AB
# url<-'https://charting.kalibrate.com/WPPS/Diesel/Retail%20(Incl.%20Tax)/DAILY/2022/Diesel_Retail%20(Incl.%20Tax)_DAILY_2022.xlsx'
# GET(url, write_disk(gas_file <- tempfile(fileext = ".xlsx")))
# data2022 <- read_excel(gas_file,skip=2)
# colnames(data2022)[1]<-"city"
# pretreat<-data2022 %>%
#   mutate(row=row_number()) %>%
#   filter(row<=77) %>%
#   gather(date,val,-city,-row) %>%
#   mutate(date=paste0("2022/",date),
#          date=as.Date(date,"%Y/%m/%d"),
#          val=as.numeric(val)) %>%
#   mutate(province=case_when(
#     row %in% seq(2,8) ~ "BC",
#     row %in% seq(10,15) ~ "AB",
#     row %in% seq(16,19) ~ "SK",
#     row %in% seq(20,21) ~ "MB",
#     row %in% seq(22,46) ~ "ON",
#     row %in% seq(47,56) ~ "QC",
#     row %in% seq(57,65) ~ "NB",
#     row %in% seq(66,71) ~ "NS",
#     row %in% seq(72,72) ~ "PE",
#     row %in% seq(73,77) ~ "NL"
#   )) %>%
#   drop_na() %>%
#   group_by(date,province) %>%
#   summarise(val=mean(val)) %>% 
#   ungroup() %>%
#   filter(date>=start) %>%
#   spread(province,val)
# model<-lm(AB~BC+SK+MB+ON+QC+NB+NS+PE+NL,data=pretreat) # pre-treatment best fit
# url<-'https://charting.kalibrate.com/WPPS/Diesel/Retail%20(Incl.%20Tax)/DAILY/2023/Diesel_Retail%20(Incl.%20Tax)_DAILY_2023.xlsx'
# GET(url, write_disk(gas_file <- tempfile(fileext = ".xlsx")))
# new <- read_excel(gas_file,skip=2)
# colnames(new)[1]<-"city"
# newdata<-new %>%
#   mutate(row=row_number()) %>%
#   filter(row<=77) %>%
#   gather(date,val,-city,-row) %>%
#   mutate(date=paste0("2023/",date),
#          date=as.Date(date,"%Y/%m/%d"),
#          val=as.numeric(val)) %>%
#   mutate(province=case_when(
#     row %in% seq(2,8) ~ "BC",
#     row %in% seq(10,15) ~ "AB",
#     row %in% seq(16,19) ~ "SK",
#     row %in% seq(20,21) ~ "MB",
#     row %in% seq(22,46) ~ "ON",
#     row %in% seq(47,56) ~ "QC",
#     row %in% seq(57,65) ~ "NB",
#     row %in% seq(66,71) ~ "NS",
#     row %in% seq(72,72) ~ "PE",
#     row %in% seq(73,77) ~ "NL"
#   )) %>%
#   drop_na() %>%
#   group_by(date,province) %>%
#   summarise(val=mean(val)) %>% 
#   ungroup() %>%
#   # filter(date<="2023-03-01") %>%
#   spread(province,val)
# plotdata<-pretreat %>% 
#   cbind(fitted=fitted(model)) %>%
#   select(date,AB,fitted) %>%
#   rbind(
#     data.frame(date=newdata$date,
#                AB=newdata$AB,
#                fitted=predict(model,newdata %>% select(BC,SK,MB,ON,QC,NB,NS,PE,NL)))
#   ) %>%
#   gather(type,val,-date)
# # average passthrough estimate
# regdata<-plotdata %>% 
#   group_by(type) %>%
#   mutate(change=val-weighted.mean(val,date=="2022-12-30")) %>%
#   select(date,type,change) %>%
#   spread(type,change) %>% 
#   mutate(gap=AB-fitted) %>%
#   mutate(treated=ifelse(date>="2023-01-01",1,0))
# model2<-lm(gap~1,data=regdata %>% filter(treated==1))
# confint(model2,'(Intercept)',level=0.95)
# effect<-paste0("95% CI Est (Jan 1 to Latest):  ",percent(-confint(model2,'(Intercept)',level=0.95)/4.5,1)[2],
#                " to ",percent(-confint(model2,'(Intercept)',level=0.95)/4.5,1)[1]," passthrough")
# # latest passthrough
# change<-(plotdata %>% 
#            group_by(type) %>%
#            mutate(change=val-weighted.mean(val,date=="2022-12-30")) %>%
#            filter(date==max(date)) %>%
#            select(type,change) %>%
#            spread(type,change) %>%
#            mutate(drop=paste(round(fitted-AB,1),"c/L"))) %>%
#   select(drop) %>%
#   as.character()
# ggplot(plotdata,aes(date,val,group=type,color=type))+
#   geom_line(size=1.5)+
#   scale_color_manual(label=c("Alberta","\"Synthetic Alberta\" (Weighted Average of Other Provinces)"),
#                      values=col[1:2])+
#   scale_y_continuous(limit=c(118,NA))+
#   scale_x_date(labels=date_format("%d\n%b"),
#                date_breaks = '1 month',expand=c(0,0),
#                limit=c(as.Date(start),max(plotdata$date)+60))+
#   geom_vline(xintercept=as.Date("2022-12-30"),size=0.75,linetype='dashed')+
#   geom_point(data=filter(plotdata,date==max(date)),size=2,stroke=2,shape=21,
#              fill='white',show.legend = F)+
#   annotate('text',x=as.Date("2022-12-28"),y=165,hjust=1,size=2,
#            label="AB Prov Diesel Tax\nFully Suspended (-4.5c/L)")+
#   annotate('text',x=as.Date("2023-01-01"),y=119,hjust=0,
#            label=effect,color=col[3],size=2)+
#   geom_segment(x=max(plotdata$date)+5,xend=max(plotdata$date)+5,
#                y=filter(plotdata,(type=="fitted"&date==max(date)))$val,
#                yend=filter(plotdata,(type=="AB"&date==max(date)))$val,
#                arrow=arrow(length=unit(1,'mm')),
#                size=0.75,color=col[3])+
#   annotate('text',x=max(plotdata$date)+6,
#            y=mean(filter(plotdata,date==max(date))$val),
#            size=3,color=col[3],
#            label=paste0("  Price Gap\n  Estimate for\n  ",
#                         gsub(" 0"," ",format(max(plotdata$date),"%b %d, %Y")),
#                         ":\n  ",change),hjust=0)+
#   labs(x="",y="Cents per Litre",
#        title="Effect of Suspending the Alberta Fuel Tax on Diesel Prices",
#        caption='Source: own calculations from daily Kalibrate DPPS data\nGraph by @trevortombe',
#        subtitle=paste0("Displays average prices in Alberta compared to a \"synthetic Alberta\" composed of a fixed-weighted average of
# other provinces, with weights selected to best fit the period prior to the tax change. Data to ",
#                        gsub(" 0"," ",format(max(newdata$date),"%B %d, %Y")),"."))
# ggsave('Plots/diesel_tax_ab.png',width=8,height=4.5)

# Gas Tax Holiday in Alberta - in 2022
# data<-read_excel("Data/gas_data_all.xls") %>%
#   mutate(date=as.Date(Dates,"%m/%d/%Y"))
# start="2022-01-10" # start date matters; pick one that helps graph look intuitive
# model<-lm(AB~BC+SK+MB+ON+QC+NB+NS+PE+NL,data=data %>% filter(date>=start)) # pre-treatment best fit
# url<-'https://charting.kalibrate.com/WPPS/Unleaded/Retail%20(Incl.%20Tax)/DAILY/2022/Unleaded_Retail%20(Incl.%20Tax)_DAILY_2022.xlsx'
# GET(url, write_disk(gas_file <- tempfile(fileext = ".xlsx")))
# new <- read_excel(gas_file,skip=2)
# colnames(new)[1]<-"city"
# newdata<-new %>%
#   mutate(row=row_number()) %>%
#   filter(row<=77) %>%
#   gather(date,val,-city,-row) %>%
#   mutate(date=paste0("2022/",date),
#          date=as.Date(date,"%Y/%m/%d"),
#          val=as.numeric(val)) %>%
#   mutate(province=case_when(
#     row %in% seq(2,8) ~ "BC",
#     row %in% seq(10,15) ~ "AB",
#     row %in% seq(16,19) ~ "SK",
#     row %in% seq(20,21) ~ "MB",
#     row %in% seq(22,46) ~ "ON",
#     row %in% seq(47,56) ~ "QC",
#     row %in% seq(57,65) ~ "NB",
#     row %in% seq(66,71) ~ "NS",
#     row %in% seq(72,72) ~ "PE",
#     row %in% seq(73,77) ~ "NL"
#   )) %>%
#   drop_na() %>%
#   group_by(date,province) %>%
#   summarise(val=mean(val)) %>% 
#   ungroup() %>%
#   filter(date>="2022-04-01") %>%
#   spread(province,val)
# plotdata<-data %>% 
#   filter(date>=start) %>%
#   cbind(fitted=fitted(model)) %>%
#   select(date,AB,fitted) %>%
#   rbind(
#     data.frame(date=newdata$date,
#                AB=newdata$AB,
#                fitted=predict(model,newdata %>% select(BC,SK,MB,ON,QC,NB,NS,PE,NL)))
#   ) %>%
#   gather(type,val,-date) %>%
#   filter(date<"2022-10-01") # tax partially reinstated Oct 1
# # average passthrough estimate
# regdata<-plotdata %>% 
#   group_by(type) %>%
#   mutate(change=val-weighted.mean(val,date=="2022-03-31")) %>%
#   select(date,type,change) %>%
#   spread(type,change) %>% 
#   mutate(gap=AB-fitted) %>%
#   mutate(treated=ifelse(date>="2022-04-01",1,0))
# model2<-lm(gap~1,data=regdata %>% filter(treated==1))
# confint(model2,'(Intercept)',level=0.95)
# effect<-paste0("95% CI Est (April 1 to Latest):  ",percent(-confint(model2,'(Intercept)',level=0.95)/13,1)[2],
#                " to ",percent(-confint(model2,'(Intercept)',level=0.95)/13,1)[1]," passthrough")
# # latest passthrough
# change<-(plotdata %>% 
#            group_by(type) %>%
#            mutate(change=val-weighted.mean(val,date=="2022-03-31")) %>%
#            filter(date==max(date)) %>%
#            select(type,change) %>%
#            spread(type,change) %>%
#            mutate(drop=paste(round(fitted-AB,1),"c/L"))) %>%
#   select(drop) %>%
#   as.character()
# ggplot(plotdata,aes(date,val,group=type,color=type))+
#   geom_line(size=1.5)+
#   scale_color_manual(label=c("Alberta","\"Synthetic Alberta\" (Weighted Average of Other Provinces)"),
#                      values=col[1:2])+
#   scale_y_continuous(limit=c(120,NA))+
#   scale_x_date(labels=date_format("%d\n%b"),
#                date_breaks = '1 month',expand=c(0,0),
#                limit=c(as.Date(start),max(plotdata$date)+60))+
#   geom_vline(xintercept=as.Date("2022-03-31"),size=0.75,linetype='dashed')+
#   geom_point(data=filter(plotdata,date==max(date)),size=2,stroke=2,shape=21,
#              fill='white',show.legend = F)+
#   annotate('text',x=as.Date("2022-03-31"),y=125,hjust=1,size=2,
#            label="AB Prov Gas Tax Suspended (-13c/L)  \nNational Carbon Price Increases (+2.2c/L)  ")+
#   annotate('text',x=as.Date("2022-04-05"),y=125,hjust=0,
#            label=effect,color=col[3],size=2)+
#   # annotate('text',x=as.Date("2022-07-01"),size=3,color=col[3],
#   #          y=160,label="Rising retail margins have\nnow absorbed the tax reduction")+
#   # geom_segment(x=as.Date("2022-07-01"),y=167,yend=177,xend=max(plotdata$date)-5,
#   #              arrow=arrow(length=unit(1,'mm')),color=col[3])+
#   geom_segment(x=max(plotdata$date)+5,xend=max(plotdata$date)+5,
#                y=filter(plotdata,(type=="fitted"&date==max(date)))$val,
#                yend=filter(plotdata,(type=="AB"&date==max(date)))$val,
#                arrow=arrow(length=unit(1,'mm')),
#                size=0.75,color=col[3])+
#   # annotate('text',x=max(plotdata$date)+6,
#   #          y=mean(filter(plotdata,date==max(date))$val),
#   #          size=3,color=col[3],
#   #          label=paste0("  Price Gap\n  Estimate for\n  ",
#   #                       gsub(" 0"," ",format(max(newdata$date),"%b %d, %Y")),":\n  ",change),hjust=0)+
#   annotate('text',x=max(plotdata$date)+6,
#            y=mean(filter(plotdata,date==max(date))$val),
#            size=3,color=col[3],
#            label=paste0("  Price Gap\n  Estimate for\n  ",
#                         "Sept 30, 2022",":\n  ",change),hjust=0)+
#   labs(x="",y="Cents per Litre",
#        title="Effect of Suspending the Alberta Gas Tax on Prices",
#        caption='Source: own calculations from daily Kalibrate DPPS data\nGraph by @trevortombe',
#        subtitle=paste0("Displays average prices in Alberta compared to a \"synthetic Alberta\" composed of a fixed-weighted average of
# other provinces, with weights selected to best fit the period prior to the tax change. Data to ",
#                        gsub(" 0"," ",format(max(newdata$date),"%B %d, %Y")),"."))
# ggsave('Plots/gas_tax_ab.png',width=8,height=4.5)


# Gas Tax Holiday in Alberta - in 2022
# data<-read_excel("Data/gas_data_all.xls") %>%
#   mutate(date=as.Date(Dates,"%m/%d/%Y"))
# start="2022-01-10" # start date matters; pick one that helps graph look intuitive
# model<-lm(AB~BC+SK+MB+ON+QC+NB+NS+PE+NL,data=data %>% filter(date>=start)) # pre-treatment best fit
# url<-'https://charting.kalibrate.com/WPPS/Unleaded/Retail%20(Incl.%20Tax)/DAILY/2022/Unleaded_Retail%20(Incl.%20Tax)_DAILY_2022.xlsx'
# GET(url, write_disk(gas_file <- tempfile(fileext = ".xlsx")))
# new <- read_excel(gas_file,skip=2)
# colnames(new)[1]<-"city"
# newdata<-new %>%
#   mutate(row=row_number()) %>%
#   filter(row<=77) %>%
#   gather(date,val,-city,-row) %>%
#   mutate(date=paste0("2022/",date),
#          date=as.Date(date,"%Y/%m/%d"),
#          val=as.numeric(val)) %>%
#   mutate(province=case_when(
#     row %in% seq(2,8) ~ "BC",
#     row %in% seq(10,15) ~ "AB",
#     row %in% seq(16,19) ~ "SK",
#     row %in% seq(20,21) ~ "MB",
#     row %in% seq(22,46) ~ "ON",
#     row %in% seq(47,56) ~ "QC",
#     row %in% seq(57,65) ~ "NB",
#     row %in% seq(66,71) ~ "NS",
#     row %in% seq(72,72) ~ "PE",
#     row %in% seq(73,77) ~ "NL"
#   )) %>%
#   drop_na() %>%
#   group_by(date,province) %>%
#   summarise(val=mean(val)) %>% 
#   ungroup() %>%
#   filter(date>="2022-04-01") %>%
#   spread(province,val)
# plotdata<-data %>% 
#   filter(date>=start) %>%
#   cbind(fitted=fitted(model)) %>%
#   select(date,AB,fitted) %>%
#   rbind(
#     data.frame(date=newdata$date,
#                AB=newdata$AB,
#                fitted=predict(model,newdata %>% select(BC,SK,MB,ON,QC,NB,NS,PE,NL)))
#   ) %>%
#   gather(type,val,-date) %>%
#   filter(date<"2022-10-01") # tax partially reinstated Oct 1
# # average passthrough estimate
# regdata<-plotdata %>% 
#   group_by(type) %>%
#   mutate(change=val-weighted.mean(val,date=="2022-03-31")) %>%
#   select(date,type,change) %>%
#   spread(type,change) %>% 
#   mutate(gap=AB-fitted) %>%
#   mutate(treated=ifelse(date>="2022-04-01",1,0))
# model2<-lm(gap~1,data=regdata %>% filter(treated==1))
# confint(model2,'(Intercept)',level=0.95)
# effect<-paste0("95% CI Est (April 1 to Latest):  ",percent(-confint(model2,'(Intercept)',level=0.95)/13,1)[2],
#                " to ",percent(-confint(model2,'(Intercept)',level=0.95)/13,1)[1]," passthrough")
# # latest passthrough
# change<-(plotdata %>% 
#            group_by(type) %>%
#            mutate(change=val-weighted.mean(val,date=="2022-03-31")) %>%
#            filter(date==max(date)) %>%
#            select(type,change) %>%
#            spread(type,change) %>%
#            mutate(drop=paste(round(fitted-AB,1),"c/L"))) %>%
#   select(drop) %>%
#   as.character()
# ggplot(plotdata,aes(date,val,group=type,color=type))+
#   geom_line(size=1.5)+
#   scale_color_manual(label=c("Alberta","\"Synthetic Alberta\" (Weighted Average of Other Provinces)"),
#                      values=col[1:2])+
#   scale_y_continuous(limit=c(120,NA))+
#   scale_x_date(labels=date_format("%d\n%b"),
#                date_breaks = '1 month',expand=c(0,0),
#                limit=c(as.Date(start),max(plotdata$date)+60))+
#   geom_vline(xintercept=as.Date("2022-03-31"),size=0.75,linetype='dashed')+
#   geom_point(data=filter(plotdata,date==max(date)),size=2,stroke=2,shape=21,
#              fill='white',show.legend = F)+
#   annotate('text',x=as.Date("2022-03-31"),y=125,hjust=1,size=2,
#            label="AB Prov Gas Tax Suspended (-13c/L)  \nNational Carbon Price Increases (+2.2c/L)  ")+
#   annotate('text',x=as.Date("2022-04-05"),y=125,hjust=0,
#            label=effect,color=col[3],size=2)+
#   # annotate('text',x=as.Date("2022-07-01"),size=3,color=col[3],
#   #          y=160,label="Rising retail margins have\nnow absorbed the tax reduction")+
#   # geom_segment(x=as.Date("2022-07-01"),y=167,yend=177,xend=max(plotdata$date)-5,
#   #              arrow=arrow(length=unit(1,'mm')),color=col[3])+
#   geom_segment(x=max(plotdata$date)+5,xend=max(plotdata$date)+5,
#                y=filter(plotdata,(type=="fitted"&date==max(date)))$val,
#                yend=filter(plotdata,(type=="AB"&date==max(date)))$val,
#                arrow=arrow(length=unit(1,'mm')),
#                size=0.75,color=col[3])+
#   # annotate('text',x=max(plotdata$date)+6,
#   #          y=mean(filter(plotdata,date==max(date))$val),
#   #          size=3,color=col[3],
#   #          label=paste0("  Price Gap\n  Estimate for\n  ",
#   #                       gsub(" 0"," ",format(max(newdata$date),"%b %d, %Y")),":\n  ",change),hjust=0)+
#   annotate('text',x=max(plotdata$date)+6,
#            y=mean(filter(plotdata,date==max(date))$val),
#            size=3,color=col[3],
#            label=paste0("  Price Gap\n  Estimate for\n  ",
#                         "Sept 30, 2022",":\n  ",change),hjust=0)+
#   labs(x="",y="Cents per Litre",
#        title="Effect of Suspending the Alberta Gas Tax on Prices",
#        caption='Source: own calculations from daily Kalibrate DPPS data\nGraph by @trevortombe',
#        subtitle=paste0("Displays average prices in Alberta compared to a \"synthetic Alberta\" composed of a fixed-weighted average of
# other provinces, with weights selected to best fit the period prior to the tax change. Data to ",
#                        gsub(" 0"," ",format(max(newdata$date),"%B %d, %Y")),"."))
# ggsave('Plots/gas_tax_ab_old.png',width=8,height=4.5)

# Gas Tax Holiday in Alberta - Diesel
# data<-read_excel("Data/diesel_data_all.xls") %>%
#   mutate(date=as.Date(Dates,"%m/%d/%Y"))
# start="2022-01-10"
# model<-lm(AB~BC+SK+MB+ON+QC+NB+NS+PE+NL,data=data %>% filter(date>=start))
# url<-'https://charting.kalibrate.com/WPPS/Diesel/Retail%20(Incl.%20Tax)/DAILY/2022/Diesel_Retail%20(Incl.%20Tax)_DAILY_2022.xlsx'
# GET(url, write_disk(diesel_file <- tempfile(fileext = ".xlsx")))
# new <- read_excel(diesel_file,skip=2)
# colnames(new)[1]<-"city"
# newdata<-new %>%
#   mutate(row=row_number()) %>%
#   filter(row<=77) %>%
#   gather(date,val,-city,-row) %>%
#   mutate(date=paste0("2022/",date),
#          date=as.Date(date,"%Y/%m/%d"),
#          val=as.numeric(val)) %>%
#   mutate(province=case_when(
#     row %in% seq(2,8) ~ "BC",
#     row %in% seq(10,15) ~ "AB",
#     row %in% seq(16,19) ~ "SK",
#     row %in% seq(20,21) ~ "MB",
#     row %in% seq(22,46) ~ "ON",
#     row %in% seq(47,56) ~ "QC",
#     row %in% seq(57,65) ~ "NB",
#     row %in% seq(66,71) ~ "NS",
#     row %in% seq(72,72) ~ "PE",
#     row %in% seq(73,77) ~ "NL"
#   )) %>%
#   drop_na() %>%
#   group_by(date,province) %>%
#   summarise(val=mean(val)) %>% 
#   ungroup() %>%
#   filter(date>="2022-04-01") %>%
#   spread(province,val)
# plotdata<-data %>% 
#   filter(date>=start) %>%
#   cbind(fitted=fitted(model)) %>%
#   select(date,AB,fitted) %>%
#   rbind(
#     data.frame(date=newdata$date,
#                AB=newdata$AB,
#                fitted=predict(model,newdata %>% select(BC,SK,MB,ON,QC,NB,NS,PE,NL)))
#   ) %>%
#   gather(type,val,-date) %>%
#   filter(date<"2022-10-01") # tax partially reinstated Oct 1
# # regression estimate
# regdata<-plotdata %>% 
#   group_by(type) %>%
#   mutate(change=val-weighted.mean(val,date=="2022-03-31")) %>%
#   select(date,type,change) %>%
#   spread(type,change) %>% 
#   mutate(gap=AB-fitted) %>%
#   mutate(treated=ifelse(date>="2022-04-01",1,0))
# model2<-lm(gap~1,data=regdata %>% filter(treated==1))
# confint(model2,'(Intercept)',level=0.95)
# effect<-paste0("95% CI Est (April 1 to Latest):  ",percent(-confint(model2,'(Intercept)',level=0.95)/13,1)[2],
#                " to ",percent(-confint(model2,'(Intercept)',level=0.95)/13,1)[1]," passthrough")
# change<-(plotdata %>% 
#            group_by(type) %>%
#            mutate(change=val-weighted.mean(val,date=="2022-03-31")) %>%
#            filter(date==max(date)) %>%
#            select(type,change) %>%
#            spread(type,change) %>%
#            mutate(drop=paste(round(fitted-AB,1),"c/L"))) %>%
#   select(drop) %>%
#   as.character()
# ggplot(plotdata,aes(date,val,group=type,color=type))+
#   geom_line(size=1.5)+
#   scale_color_manual(label=c("Alberta","\"Synthetic Alberta\" (Weighted Average of Other Provinces)"),
#                      values=col[1:2])+
#   scale_y_continuous(limit=c(120,NA))+
#   scale_x_date(labels=date_format("%d\n%b"),
#                date_breaks = '1 month',expand=c(0,0),
#                limit=c(as.Date(start),max(plotdata$date)+60))+
#   geom_vline(xintercept=as.Date("2022-03-31"),size=0.75,linetype='dashed')+
#   geom_point(data=filter(plotdata,date==max(date)),size=2,stroke=2,shape=21,
#              fill='white',show.legend = F)+
#   annotate('text',x=as.Date("2022-03-31"),y=125,hjust=1,size=2,
#            label="Alberta Provincial Diesel Tax Suspended (-13c/L)  \nNational Carbon Price Increases (+2.7c/L)  ")+
#   annotate('text',x=as.Date("2022-04-05"),y=125,hjust=0,
#            label=effect,color=col[3],size=2)+
#   # annotate('text',x=as.Date("2022-07-01"),size=3,color=col[3],
#   #          y=160,label="Rising retail margins have\nnow absorbed the tax reduction")+
#   # geom_segment(x=as.Date("2022-07-01"),y=167,yend=177,xend=max(plotdata$date)-5,
#   #              arrow=arrow(length=unit(1,'mm')),color=col[3])+
#   geom_segment(x=max(plotdata$date)+5,xend=max(plotdata$date)+5,
#                y=filter(plotdata,(type=="fitted"&date==max(date)))$val,
#                yend=filter(plotdata,(type=="AB"&date==max(date)))$val,
#                arrow=arrow(length=unit(1,'mm')),
#                size=0.75,color=col[3])+
#   annotate('text',x=max(plotdata$date)+6,
#            y=mean(filter(plotdata,date==max(date))$val),
#            size=3,color=col[3],
#            label=paste0("  Passthrough\n  Estimate for\n  ",
#                         gsub(" 0"," ",format(max(newdata$date),"%B %d, %Y")),":\n  ",change),hjust=0)+
#   labs(x="",y="Cents per Litre",
#        title="Effect of Suspending the Alberta Diesel Tax on Prices",
#        caption='Source: own calculations from daily Kalibrate DPPS data\nGraph by @trevortombe',
#        subtitle=paste0("Displays average prices in Alberta compared to a \"synthetic Alberta\" composed of a fixed-weighted average of
# other provinces, with weights selected to best fit the period prior to the tax change. Data to ",
#                        gsub(" 0"," ",format(max(newdata$date),"%B %d, %Y")),"."))
# ggsave('Plots/diesel_tax_ab_old.png',width=8,height=4.5)

# Gas Tax Reduction in Ontario
# start="2022-03-01"
# new <- read_excel(gas_file,skip=2)
# colnames(new)[1]<-"city"
# data<-new %>%
#   mutate(row=row_number()) %>%
#   filter(row<=77) %>%
#   gather(date,val,-city,-row) %>%
#   mutate(date=paste0("2022/",date),
#          date=as.Date(date,"%Y/%m/%d"),
#          val=as.numeric(val)) %>%
#   mutate(province=case_when(
#     row %in% seq(2,8) ~ "BC",
#     row %in% seq(10,15) ~ "AB",
#     row %in% seq(16,19) ~ "SK",
#     row %in% seq(20,21) ~ "MB",
#     row %in% seq(22,46) ~ "ON",
#     row %in% seq(47,56) ~ "QC",
#     row %in% seq(57,65) ~ "NB",
#     row %in% seq(66,71) ~ "NS",
#     row %in% seq(72,72) ~ "PE",
#     row %in% seq(73,77) ~ "NL"
#   )) %>%
#   drop_na() %>%
#   group_by(date,province) %>%
#   summarise(val=mean(val)) %>% 
#   ungroup() %>%
#   filter(date>=start,date<"2022-07-01") %>%
#   spread(province,val)
# model<-lm(ON~BC+SK+MB+AB+QC+NB+NS+PE+NL,data=data)
# newdata<-new %>%
#   mutate(row=row_number()) %>%
#   filter(row<=77) %>%
#   gather(date,val,-city,-row) %>%
#   mutate(date=paste0("2022/",date),
#          date=as.Date(date,"%Y/%m/%d"),
#          val=as.numeric(val)) %>%
#   mutate(province=case_when(
#     row %in% seq(2,8) ~ "BC",
#     row %in% seq(10,15) ~ "AB",
#     row %in% seq(16,19) ~ "SK",
#     row %in% seq(20,21) ~ "MB",
#     row %in% seq(22,46) ~ "ON",
#     row %in% seq(47,56) ~ "QC",
#     row %in% seq(57,65) ~ "NB",
#     row %in% seq(66,71) ~ "NS",
#     row %in% seq(72,72) ~ "PE",
#     row %in% seq(73,77) ~ "NL"
#   )) %>%
#   drop_na() %>%
#   group_by(date,province) %>%
#   summarise(val=mean(val)) %>% 
#   ungroup() %>%
#   filter(date>="2022-07-01") %>%
#   spread(province,val)
# plotdata<-data %>% 
#   filter(date>=start) %>%
#   cbind(fitted=fitted(model)) %>%
#   select(date,ON,fitted) %>%
#   rbind(
#     data.frame(date=newdata$date,
#                ON=newdata$ON,
#                fitted=predict(model,newdata %>% select(BC,SK,MB,AB,QC,NB,NS,PE,NL)))
#   ) %>%
#   gather(type,val,-date)
# # regression estimate
# regdata<-plotdata %>% 
#   group_by(type) %>%
#   mutate(change=val-weighted.mean(val,date=="2022-06-30")) %>%
#   select(date,type,change) %>%
#   spread(type,change) %>% 
#   mutate(gap=ON-fitted) %>%
#   mutate(treated=ifelse(date>="2022-07-01",1,0))
# model2<-lm(gap~1,data=regdata %>% filter(treated==1))
# confint(model2,'(Intercept)',level=0.95)
# effect<-paste0("95% CI Est (July 1 to Latest):\n",percent(-confint(model2,'(Intercept)',level=0.95)/13,1)[2],
#                " to ",percent(-confint(model2,'(Intercept)',level=0.95)/13,1)[1]," passthrough")
# # latest gap
# change<-(plotdata %>% 
#            group_by(type) %>%
#            mutate(change=val-weighted.mean(val,date=="2022-06-30")) %>%
#            filter(date==max(date)) %>%
#            select(type,change) %>%
#            spread(type,change) %>%
#            mutate(drop=paste(round(fitted-ON,1),"c/L"))) %>%
#   select(drop) %>%
#   as.character()
# ggplot(plotdata,aes(date,val,group=type,color=type))+
#   geom_line(size=1.5)+
#   scale_color_manual(label=c("\"Synthetic Ontario\" (Weighted Average of Other Provinces)","Ontario"),
#                      values=col[1:2])+
#   # scale_y_continuous(limit=c(160,NA))+
#   scale_x_date(labels=date_format("%d\n%b"),
#                date_breaks = '1 month',
#                limit=c(as.Date(start),max(plotdata$date)+40))+
#   geom_vline(xintercept=as.Date("2022-06-30"),size=0.75,linetype='dashed')+
#   geom_point(data=filter(plotdata,date==max(date)),size=2,stroke=2,shape=21,
#              fill='white',show.legend = F)+
#   annotate('text',x=as.Date("2022-06-29"),y=162,hjust=1,size=2,
#            label="Provincial Gasoline Tax Lowered 5.7c/L")+
#   geom_segment(x=max(plotdata$date)+5,xend=max(plotdata$date)+5,
#                y=filter(plotdata,(type=="fitted"&date==max(date)))$val,
#                yend=filter(plotdata,(type=="ON"&date==max(date)))$val,
#                arrow=arrow(length=unit(1,'mm')),
#                size=0.75,color=col[3])+
#   annotate('text',x=max(plotdata$date)+6,
#            y=mean(filter(plotdata,date==max(date))$val),
#            size=2,color=col[3],
#            label=paste0("  Passthrough\n  Estimate for\n  ",
#                         gsub(" 0"," ",format(max(newdata$date),"%B %d, %Y")),":\n  ",change),hjust=0)+
#   annotate('text',x=as.Date("2022-07-05"),y=162,hjust=0,
#            label=effect,color=col[3],size=2.5)+
#   labs(x="",y="Cents per Litre",
#        title="Effect of Lowering the Ontario Gas Tax on Prices",
#        caption='Source: own calculations from daily Kalibrate DPPS data\nGraph by @trevortombe',
#        subtitle=paste0("Displays average gas prices in Ontario compared to a \"synthetic Ontario\" composed of a fixed-weighted average of
# other provinces, with weights selected to best fit the period prior to the tax change. Data to ",
#                        gsub(" 0"," ",format(max(newdata$date),"%B %d, %Y")),"."))
# ggsave('Plots/gas_tax_on_old.png',width=8,height=4.5)


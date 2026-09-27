# Load required packages, fetch the latest data
source("R/Setup.R")

##############################################
# Provincial Government Responses: Gas Taxes #
##############################################

# Approximate city populations
city_pop<-read_excel('Data/city_population.xlsx')

# End of the consumer carbon tax in Canada: April 1, 2025
start=as.Date("2025-04-01") # treatment start
# url<-'https://charting.kalibrate.com/WPPS/Unleaded/Retail%20(Incl.%20Tax)/DAILY/2025/Unleaded_Retail%20(Incl.%20Tax)_DAILY_2025.xlsx'
# GET(url, write_disk(gas_file <- tempfile(fileext = ".xlsx")))
# data2025 <- read_excel(gas_file,skip=2)
data2025 <- read_excel('Data/Unleaded_Retail (Incl. Tax)_DAILY_2025.xlsx',skip=2)
colnames(data2025)[1]<-"city"
clean_data<-data2025 %>%
  mutate(row=row_number()) %>%
  filter(row<=77) %>%
  cbind(pop=city_pop$pop) %>%
  gather(date,val,-city,-row,-pop) %>%
  mutate(date=paste0("2025/",date),
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
  filter(date<=as.Date("2025-07-01")) %>%
  group_by(date,province) %>%
  summarise(val=weighted.mean(val,pop)) %>% 
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
ggplot(plotdata,aes(date,val,group=type,color=type))+
  geom_line(linewidth=1.5)+
  scale_color_manual(label=c("Quebec (No Change in Carbon Pricing)","Rest of Canada (Adjusted)"),values=col[2:1])+
  scale_y_continuous(limit=c(min(plotdata$val)-7,170))+
  scale_x_date(labels=date_format("%b\n%Y"),
               date_breaks = '1 month',expand=c(0,0),
               limit=c(as.Date("2025-01-01"),max(plotdata$date)))+
  geom_vline(xintercept=start,linewidth=0.75,linetype='dashed')+
  annotate('text',x=start-1,y=165,hjust=1,size=3,
           label="Retail carbon tax eliminated")+
  annotate('text',x=max(plotdata$date)-60,y=146,
           size=3,color=col[3],
           label=paste("Average price drop:",
                       round((filter(plotdata,date>start) %>% group_by(type) %>% 
                          summarise(val=mean(val)) %>% 
                          spread(type,val) %>% mutate(gap=QC-synth))$gap,2),"c/L"),hjust=0)+
  labs(x="",y="Cents per Litre",
       title="Effect of ending the federal carbon tax on gasoline prices",
       caption='Source: Own calculations from daily Kalibrate DPPS data\nGraph by @trevortombe',
       subtitle=paste0("Displays average prices in Quebec compared to an adjusted average for the rest of Canada, which reflects a fixed-weighted
average of other provinces, with weights selected to best fit the period prior to the tax change."))
ggsave('Plots/gas_ctax_2025.png',width=8,height=4.5)

# Gas Tax Holiday in Manitoba, Jan 1, 2024
start="2023-10-01" # pre-tretment start
# url<-'https://charting.kalibrate.com/WPPS/Unleaded/Retail%20(Incl.%20Tax)/DAILY/2023/Unleaded_Retail%20(Incl.%20Tax)_DAILY_2023.xlsx'
# GET(url, write_disk(gas_file <- tempfile(fileext = ".xlsx")))
# data2023 <- read_excel(gas_file,skip=2)
data2023 <- read_excel('Data/Unleaded_Retail (Incl. Tax)_DAILY_2023.xlsx',skip=2)
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
# url<-'https://charting.kalibrate.com/WPPS/Unleaded/Retail%20(Incl.%20Tax)/DAILY/2024/Unleaded_Retail%20(Incl.%20Tax)_DAILY_2024.xlsx'
# GET(url, write_disk(gas_file <- tempfile(fileext = ".xlsx")))
# new <- read_excel(gas_file,skip=2)
new <- read_excel('Data/Unleaded_Retail (Incl. Tax)_DAILY_2024.xlsx',skip=2)
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
  filter(date<=as.Date("2024-05-01")) %>%
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
               synth=predict(model,newdata %>% select(BC,SK,ON,QC,NB,NS,PE,NL)))
  ) %>%
  gather(type,val,-date)
ggplot(plotdata,aes(date,val,group=type,color=type))+
  geom_line(size=1.5)+
  scale_color_manual(label=c("Manitoba","\"Synthetic Manitoba\" (Weighted Average of Other Provinces)"),
                     values=col[2:1])+
  scale_x_date(labels=date_format("%b\n%Y"),
               date_breaks = '1 month')+
  geom_vline(xintercept=as.Date("2024-01-01"),size=0.75,linetype='dashed')+
  annotate('text',x=as.Date("2023-12-28"),y=165,hjust=1,size=3,
           label="Prov gas tax suspended")+
  annotate('text',x=max(plotdata$date)-30,y=150,
           size=3,color=col[3],
           label=paste("Average price\ndrop:",
                       round((filter(plotdata,date>start) %>% group_by(type) %>% 
                                summarise(val=mean(val)) %>% 
                                spread(type,val) %>% mutate(gap=synth-MB))$gap,1),"c/L"))+
  labs(x="",y="Cents per Litre",
       title="Effect of suspending the Manitoba fuel tax on gasoline prices",
       caption='Source: own calculations from daily Kalibrate DPPS data\nGraph by @trevortombe',
       subtitle=paste0("Displays average prices in Manitoba compared to a \"synthetic Manitoba\" composed of a fixed-weighted average of other
provinces (excluding Alberta), with weights selected to best fit the period prior to the tax change."))
ggsave('Plots/gas_tax_mb.png',width=8,height=4.5)

# Gas Tax Holiday in Alberta - Diesel
data<-read_excel("Data/diesel_data_all.xls") %>%
  mutate(date=as.Date(Dates,"%m/%d/%Y"))
model<-lm(AB~BC+SK+MB+ON+QC+NB+NS+PE+NL,data=data %>% filter(date>="2022-01-10"))
url<-'https://charting.kalibrate.com/WPPS/Diesel/Retail%20(Incl.%20Tax)/DAILY/2022/Diesel_Retail%20(Incl.%20Tax)_DAILY_2022.xlsx'
GET(url, write_disk(diesel_file <- tempfile(fileext = ".xlsx")))
new <- read_excel(diesel_file,skip=2)
colnames(new)[1]<-"city"
newdata<-new %>%
  mutate(row=row_number()) %>%
  filter(row<=77) %>%
  gather(date,val,-city,-row) %>%
  mutate(date=paste0("2022/",date),
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
  filter(date>="2022-04-01") %>%
  spread(province,val)
plotdata<-data %>%
  filter(date>="2022-01-10") %>%
  cbind(fitted=fitted(model)) %>%
  select(date,AB,fitted) %>%
  rbind(
    data.frame(date=newdata$date,
               AB=newdata$AB,
               fitted=predict(model,newdata %>% select(BC,SK,MB,ON,QC,NB,NS,PE,NL)))
  ) %>%
  gather(type,val,-date) %>%
  filter(date<"2022-07-01") # tax partially reinstated Oct 1
ggplot(plotdata,aes(date,val,group=type,color=type))+
  geom_line(size=1.5)+
  scale_color_manual(label=c("Alberta","\"Synthetic Alberta\" (Weighted Average of Other Provinces)"),
                     values=col[1:2])+
  scale_x_date(labels=date_format("%d\n%b"),date_breaks = '1 month',)+
  geom_vline(xintercept=as.Date("2022-03-31"),size=0.75,linetype='dashed')+
  annotate('text',x=as.Date("2022-03-28"),y=205,hjust=1,size=3,
           label="Fuel tax suspended")+
  annotate('text',x=max(plotdata$date)-30,y=175,
           size=3,color=col[3],
           label=paste("Average price\ndrop:",
                       round((filter(plotdata,date>"2022-04-01") %>% group_by(type) %>% 
                                summarise(val=mean(val)) %>% 
                                spread(type,val) %>% mutate(gap=fitted-AB))$gap,1),"c/L"))+
  labs(x="",y="Cents per litre",
       title="Effect of suspending the Alberta fuel tax on diesel prices",
       caption='Source: Own calculations from daily Kalibrate DPPS data\nGraph by @trevortombe',
       subtitle=paste0("Displays average prices in Alberta compared to a \"synthetic Alberta\" composed of a fixed-weighted average of
other provinces, with weights selected to best fit the period prior to the tax change."))
ggsave('Plots/diesel_ab.png',width=8,height=4.5)

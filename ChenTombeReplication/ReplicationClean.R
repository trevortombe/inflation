source('Setup.R')

##########################
# Load the Required Data #
##########################

# The old vintage of data used in the paper: load('Files/DataForPaper.RData')
# This may differ from later updates due to StatCan data revisions.
# Either load the old data or run the 'get_cansim' functions below

# Monthly CPI
cpi_data<-get_cansim("18100004") %>%
  mutate(Ref_Date=as.yearmon(REF_DATE,"%Y-%m")) %>%
  rename(Value=VALUE)

# CPI basket weights
weights<-get_cansim("18100007") %>%
  mutate(Ref_Date=as.numeric(REF_DATE)) %>%
  rename(Value=VALUE)

# Long series household final consumption data
pce_data<-get_cansim("36100107") %>%
  mutate(Ref_Date=as.yearmon(REF_DATE,"%Y-%m")) %>%
  rename(Value=VALUE)

# Detailed household final consumption data
pce_data_new<-get_cansim("36100124") %>%
  mutate(Ref_Date=as.yearmon(REF_DATE,"%Y-%m")) %>%
  rename(Value=VALUE)

# CPI product list
product_list<-read.csv("Files/cpi_products.csv")

# Oil prices and exchange rates (FRED API key required)
wti<-fredr("MCOILWTICO")
cdnusd<-fredr("DEXCAUS")

#############
# Section 2 #
#############

# Construct useful objects to decompose the CPI
inf_rates<-cpi_data %>%
  filter(`Products and product groups`=="All-items",
         GEO=="Canada") %>%
  mutate(YoY=Value/lag(Value,12)-1,
         change=YoY-lag(YoY,12)) %>%
  dplyr::select(Ref_Date,YoY,change) %>%
  drop_na()
weights_monthly<-weights %>%
  filter(GEO=="Canada",
         `Geographic distribution of weight`=="Distribution to selected geographies",
         `Price period of weight`=="Weight at basket link month prices") %>%
  dplyr::select(basket=Ref_Date,product=`Products and product groups`,w=Value)
link_months<-data.frame(
  Ref_Date=seq(as.yearmon("1978-01"),max(cpi_data$Ref_Date),1/12)
) %>%
  mutate(basket=case_when(
    Ref_Date>="Oct 1978" & Ref_Date<"Apr 1982" ~ 1974,
    Ref_Date>="Apr 1982" & Ref_Date<"Jan 1985" ~ 1978,
    Ref_Date>="Jan 1985" & Ref_Date<"Jan 1989" ~ 1982,
    Ref_Date>="Jan 1989" & Ref_Date<"Jan 1995" ~ 1986,
    Ref_Date>="Jan 1995" & Ref_Date<"Jan 1998" ~ 1992,
    Ref_Date>="Jan 1998" & Ref_Date<"Jan 2003" ~ 1996,
    Ref_Date>="Jan 2003" & Ref_Date<"Apr 2007" ~ 2001,
    Ref_Date>="Apr 2007" & Ref_Date<"Apr 2011" ~ 2005,
    Ref_Date>="Apr 2011" & Ref_Date<"Jan 2013" ~ 2009,
    Ref_Date>="Jan 2013" & Ref_Date<"Dec 2014" ~ 2011,
    Ref_Date>="Dec 2014" & Ref_Date<"Dec 2016" ~ 2013,
    Ref_Date>="Dec 2016" & Ref_Date<"Dec 2018" ~ 2015,
    Ref_Date>="Dec 2018" & Ref_Date<"Jun 2021" ~ 2017,
    Ref_Date>="Jun 2021" & Ref_Date<"May 2022" ~ 2020,
    Ref_Date>="May 2022" & Ref_Date<"May 2023" ~ 2021,
    Ref_Date>="May 2023" ~ 2022
  )) %>%
  group_by(basket) %>%
  mutate(link_month=min(Ref_Date)) %>% ungroup()

# Figure 1: Key Drives of CPI
decomp_cpi<-cpi_data %>%
  filter(GEO=="Canada") %>%
 dplyr::select(Ref_Date,product=`Products and product groups`,Value) %>%
  left_join(link_months,by="Ref_Date") %>%
  filter(!is.na(basket)) %>%
  left_join(weights_monthly,by=c("product","basket")) %>%
  group_by(Ref_Date) %>%
  mutate(all=weighted.mean(Value,product=="All-items")) %>%
  group_by(product) %>%
  mutate(period=cumsum(ifelse(Ref_Date==link_month,1,0))) %>%
  group_by(product,period) %>%
  mutate(I_atlink=Value[1],
         all_atlink=all[1]) %>%
  group_by(product) %>%
  mutate(cpi=all/lag(all,12)-1,
         relimp=(w/100)*(Value/I_atlink)/(all/all_atlink), # relative importance
         relimp_old=lag(relimp,12), # relative importance using weights and prices from t-12
         relimp_new=(w/100)*(lag(Value,12)/I_atlink)/(lag(all,12)/all_atlink)) %>% # relative importance using current weights but t-12 prices) %>%
  mutate(contrib_old=(I_atlink/lag(Value,12)-1)*relimp_old, #statcan, https://www150.statcan.gc.ca/n1/pub/62-553-x/2019001/chap-8-eng.htm
         contrib_new=(Value/I_atlink-1)*(w/100)*(all_atlink/lag(all,12)), #statcan, https://www150.statcan.gc.ca/n1/pub/62-553-x/2019001/chap-8-eng.htm
         contrib_cross=contrib_new+contrib_old, 
         contrib_nocross=(Value/lag(Value,12)-1)*relimp_old, 
         contrib_check=ifelse(basket!=lag(basket,12),contrib_cross,contrib_nocross), # verify statcan same as your main approach
         effective_weight=(relimp_old/(Value/I_atlink)+(1-1/(Value/I_atlink))*relimp_new), # an intuitive way? same as statcan approach
         change=Value/lag(Value,12)-1,
         contrib=(1+change)*effective_weight-relimp_old) # main estimate
plotdata<-decomp_cpi %>%
  filter(Ref_Date>="Jan 1989") %>% # when the 1986 basket begins
  filter(!is.na(w)) %>% # remove items without weight
  filter(product %in% c("Food purchased from stores",
                        "Energy",
                        "Household furnishings and equipment",
                        "Rented accommodation","Owned accommodation",
                        "Purchase of recreational vehicles and outboard motors",
                        "Purchase of passenger vehicles")) %>%
  group_by(Ref_Date) %>%
  mutate(total=sum(contrib),
         `All other items`=cpi-total) %>%
  dplyr::select(Ref_Date,product,contrib,`All other items`,cpi) %>%
  spread(product,contrib) %>%
  gather(product,contrib,-Ref_Date,-cpi) %>%
  mutate(product=ifelse(product %in% c("Purchase of recreational vehicles and outboard motors",
                                       "Purchase, leasing and rental of passenger vehicles",
                                       "Purchase of passenger vehicles",
                                       "Household furnishings and equipment",
                                       "Purchase and leasing of passenger vehicles"),
                        "Furniture/Equip./Vehicles",product)) %>%
  group_by(Ref_Date,product) %>%
  summarise(contrib=sum(contrib),
            cpi=mean(cpi)) %>%
  ungroup() %>%
  filter(Ref_Date>="Jan 2017") %>%
  mutate(product=case_when(
    product=="Food purchased from stores" ~ "Food (groceries)",
    product=="Household furnishings and equipment" ~ "Furniture and household equip.",
    product=="Homeowners' replacement cost" ~ "Homeowners' depreciation",
    TRUE ~ product
  )) %>%
  filter(!is.na(cpi))
dev.off()
p<-ggplot(plotdata,aes(Ref_Date,contrib,group=product,fill=product))+
  geom_col(position='stack',size=0.05,color='white')+
  geom_line(aes(y=cpi),size=2)+
  scale_y_continuous(label=percent,breaks=pretty_breaks(5))+
  scale_x_continuous(breaks=pretty_breaks(6))+
  theme(plot.margin = unit(c(0.25,10,0.25,0.25),"lines"),
        legend.position = 'none',
        legend.key.width = unit(2,"cm"),
        legend.title=element_blank(),
        panel.grid.major.y = element_line(color='gray'))+
  geom_label(data=plotdata %>% filter(Ref_Date==max(Ref_Date)) %>%
               arrange(desc(product)) %>%
               mutate(location=ifelse(row_number()==1,contrib/2,NA),
                      location=ifelse(row_number()>1,lag(cumsum(contrib),1)+contrib/2,location),
                      location=ifelse(product=="Energy",-0.005,location),
                      location=ifelse(product=="Food (groceries)",0.024,location),
                      location=ifelse(product=="All other items",0.03,location),
                      labelname=gsub(" ","\n  ",product)),
             aes(label=paste0("  ",product),y=location,color=product),
             hjust=0,nudge_x=1/12,fontface="bold",size=3,fill='white',label.size=0)+
  annotate('text',x=2021.5,hjust=1,y=0.05,label="All-item CPI",size=3)+
  labs(x="",
       title="Key Drives of Consumer Price Inflation in Canada",
       subtitle="Source: Authors' calculations from Statistics Canada data tables 18-10-0007 and 18-10-0004",
       y="Year-over-Year Change")
gt <- ggplotGrob(p)
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)
ggsave("Figures/Figure1.png",gt,width=8,height=4)

# Save Figure 1 data in a text file
write.csv(plotdata %>%
  spread(product,contrib),"Figures/Figure1_data.csv",row.names = F)

# Figure 2: Sensitivity of individual components to oil prices
oil_change<-wti %>%
  mutate(Ref_Date=as.yearmon(date)) %>%
  left_join(
    cdnusd %>% mutate(Ref_Date=as.yearmon(date)) %>%
      filter(!is.na(value)) %>%
      group_by(Ref_Date) %>%
      summarise(cdnusd=mean(value)),
    by="Ref_Date"
  ) %>%
  mutate(oil_change=value*cdnusd-lag(value*cdnusd,12)) %>%
  dplyr::select(Ref_Date,oil_change) %>%
  drop_na()
regdata<-decomp_cpi %>%
  filter(product %in% product_list$product) %>%
  filter(!product %in% c('Natural gas','Fuel oil and other fuels','Gasoline',
                         'Electricity','Fuel, parts and accessories for recreational vehicles')) %>%
  dplyr::select(Ref_Date,product,change,contrib,cpi,effective_weight) %>%
  left_join(oil_change,by="Ref_Date") %>%
  drop_na() %>%
  filter(Ref_Date>="Feb 1995")
results<-NULL
for (p in unique(regdata$product)){
  temp<-tidy(lm(change~oil_change,data=filter(regdata,product==p)))
  results<-results %>%
    rbind(
      data.frame(
        beta=temp[2,2],
        tstat=temp[2,4],
        product=p
      )
    )
}
plot<-regdata %>%
  left_join(results %>%
              mutate(type=ifelse((statistic)>1.96,"Items\nsensitive\nto oil\nprices",
                                 "Items not\nsensitive\nto oil\nprices")) %>% 
              dplyr::select(product,type,estimate,statistic),by=c("product")) %>%
  group_by(Ref_Date,type) %>%
  summarise(contrib=sum(contrib),
            effective_weight=sum(effective_weight),
            cpi=mean(cpi)) %>%
  group_by(Ref_Date,type) %>% ungroup() %>%
  filter(Ref_Date>="Jan 2017") %>%
  group_by(Ref_Date) %>%
  mutate(contrib=contrib/sum(effective_weight)) %>% ungroup()
dev.off()
p<-ggplot(plot,aes(Ref_Date,contrib,group=type,fill=type))+
  geom_col(position='stack',size=0.05,color='white')+
  geom_line(data=decomp_cpi %>%
              filter(product=="All-items excluding energy",
                     Ref_Date>="Jan 2017") %>%
              dplyr::select(Ref_Date,change) %>% drop_na(),aes(Ref_Date,change),inherit.aes = F,
            size=2)+
  scale_y_continuous(label=percent,breaks=pretty_breaks(5))+
  scale_x_continuous(breaks=pretty_breaks(6))+
  theme(plot.margin = unit(c(0.25,2,0.25,0.25),"lines"),
        legend.position = 'none',
        legend.key.width = unit(2,"cm"),
        legend.title=element_blank(),
        panel.grid.major.y = element_line(color='gray'))+
  geom_label(data=plot %>% filter(Ref_Date==max(Ref_Date)) %>%
               arrange(desc(type)) %>%
               mutate(location=ifelse(row_number()==1,contrib/2,NA),
                      location=ifelse(row_number()>1,lag(cumsum(contrib),1)+contrib/2,location),
                      labelname=gsub(" ","\n  ",type)),
             aes(label=paste0("",type),y=location,color=type),
             hjust=0,nudge_x=2/12,fontface="bold",size=3,fill='white',label.size=0)+
  annotate('text',x=2021.75,hjust=1,y=0.055,label="All items excluding energy",size=3)+
  labs(x="",
       title="Contribution of Items Sensitive to Oil Prices to Non-Energy CPI Inflation",
       subtitle="Source: Authors' calculations using Statistics Canada data tables 18-10-0004, 18-10-0007
and FRED MCOILWTICO and DEXCAUS",
       y="Year-over-Year Change")
gt <- ggplotGrob(p)
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)
ggsave("Figures/Figure2.png",gt,width=8,height=4)

#############
# Section 3 #
#############

# Figure 4: Two Measures of Inflation
real_change<-pce_data %>%
  filter(Estimates=="Household final consumption expenditure [C]",
         `Seasonal adjustment`=="Seasonally adjusted at annual rates",
         Prices=="Chained (2012) dollars") %>%
  group_by(Estimates) %>%
  mutate(qty_change=Value/lag(Value,4),
         qty_index=Value) %>%
  dplyr::select(Ref_Date,product=Estimates,qty_change,qty_index)
nom_change<-pce_data %>%
  filter(Estimates=="Household final consumption expenditure [C]",
         `Seasonal adjustment`=="Seasonally adjusted at annual rates",
         Prices=="Current prices") %>%
  group_by(Estimates) %>%
  mutate(nom_change=Value/lag(Value,4)) %>%
  dplyr::select(Ref_Date,product=Estimates,nom_change,spending=Value)
price_change<-nom_change %>%
  left_join(real_change,by=c("Ref_Date","product")) %>%
  mutate(price_change=nom_change/qty_change,
         price_index=spending/qty_index) %>%
  group_by(Ref_Date) %>%
  mutate(total=sum(spending),
         weight=spending/total) %>%
  group_by(product) %>%
  mutate(weight_lag=lag(weight,4)) %>%
  group_by(Ref_Date) %>%
  mutate(PCE=sum((price_change-1)*weight_lag)) %>%
  mutate(Ref_Date=as.yearqtr(Ref_Date)) %>%
  dplyr::select(Ref_Date,PCE)
plotdata<-cpi_data %>%
  filter(`Products and product groups`=="All-items",
         GEO=="Canada") %>%
  mutate(Ref_Date=as.yearqtr(Ref_Date)) %>%
  group_by(Ref_Date) %>%
  summarise(Value=mean(Value)) %>% ungroup() %>%
  mutate(CPI=Value/lag(Value,4)-1) %>%
  dplyr::select(Ref_Date,CPI) %>%
  left_join(price_change,by="Ref_Date") %>%
  filter(Ref_Date>="1962 Q1",Ref_Date<=max(price_change$Ref_Date)) %>%
  gather(type,val,-Ref_Date)
ggplot(plotdata,aes(Ref_Date,val,group=type,color=type))+
  geom_line(size=2,show.legend = F)+
  geom_hline(yintercept=0,size=1)+
  scale_x_continuous(breaks=pretty_breaks(6))+
  annotate('text',x=2020,hjust=1,y=0.07,label="CPI Inflation",color=col[1],fontface='bold')+
  annotate('text',x=1985,hjust=0,y=0.09,label="PCE Inflation",color=col[2],fontface='bold')+
  scale_y_continuous(label=percent,expand=c(0,0),limit=c(NA,0.145),breaks=pretty_breaks(6))+
  labs(x="",
       title="Two Measures of Inflation in Canada",
       subtitle="Source: Authors' calculations using Statistics Canada data tables 18-10-0004 and 36-10-0107",
       y="Per Cent")
ggsave("Figures/Figure3.png",width=8,height=4)

# Construct the supply/demand classifications using VAR estimates
products_aggregate<-c("Food and non-alcoholic beverages",
                      "Alcoholic beverages, tobacco and cannabis",
                      "Clothing and footwear",
                      "Housing, water, electricity, gas and other fuels",
                      "Furnishings, household equipment and other goods and services related to the dwelling and property",
                      "Health",
                      "Transport",
                      "Communications",
                      "Recreation and culture",
                      "Education",
                      "Food, beverage and accommodation services",
                      "Insurance and financial services",
                      "Miscellaneous goods and services",
                      "Net expenditure abroad")
products_exclude<-c(products_aggregate,
                    "Expenditure by non-residents in Canada", 
                    "Expenditure by Canadians abroad", 
                    "Household final consumption expenditure",
                    "Net expenditure abroad, adjusting entry",
                    "Household final consumption expenditure, adjusting entry")
real_change<-pce_data_new %>%
  filter(!Estimates %in% products_exclude,
         !grepl("adjusting entry",Estimates),
         !grepl("Cannabis",Estimates),
         `Seasonal adjustment`=="Seasonally adjusted at quarterly rates",
         Ref_Date>="Jan 1981", # when the detailed data starts
         Prices=="2017 constant prices") %>%
  group_by(Estimates) %>%
  mutate(qty_change=Value/lag(Value,1),
         qty_change_yoy=Value/lag(Value,4),
         qty_index=Value) %>%
  dplyr::select(Ref_Date,product=Estimates,qty_change,qty_index,qty_change_yoy)
nom_change<-pce_data_new %>%
  filter(!Estimates %in% products_exclude,
         !grepl("adjusting entry",Estimates),
         !grepl("Cannabis",Estimates),
         Ref_Date>="Jan 1981", # when the detailed data starts
         `Seasonal adjustment`=="Seasonally adjusted at quarterly rates",
         Prices=="Current prices") %>%
  group_by(Estimates) %>%
  mutate(nom_change=Value/lag(Value,1),
         nom_change_yoy=Value/lag(Value,4)) %>%
  dplyr::select(Ref_Date,product=Estimates,nom_change,spending=Value,nom_change_yoy)
price_change<-nom_change %>%
  left_join(real_change,by=c("Ref_Date","product")) %>%
  mutate(price_change=nom_change/qty_change,
         price_change_yoy=nom_change_yoy/qty_change_yoy,
         price_index=spending/qty_index) %>%
  group_by(Ref_Date) %>%
  mutate(total=sum(spending),
         weight=spending/total) %>%
  group_by(product) %>%
  mutate(weight_lag=lag(weight,1),
         weight_lag_yoy=lag(weight,4)) %>% 
  group_by(Ref_Date) %>%
  mutate(w=weight_lag) %>% 
  filter(!is.na(w)) %>% # drops 1981
  mutate(Pl=sum(price_change*weight_lag)-1,
         Pp=1/sum(weight/price_change)-1,
         Pf=sqrt((1+Pl)*(1+Pp))-1,
         contrib=(price_change-1)*w,
         PCE=sum(contrib))
dates<-unique((price_change %>% filter(Ref_Date>="Jan 2009"))$Ref_Date)
prods<-unique((price_change)$product)
unexp_shocks<-NULL
for (p in prods){ # VAR models; rolling windows; product-specific
  for (d in sort(dates,decreasing=T)){
    regdata<-price_change %>% ungroup() %>%
      filter(Ref_Date>=as.yearmon(d)-10, # 10-year rolling window
             Ref_Date<=as.yearmon(d),
             product==p)
    yr_val=year(min(regdata$Ref_Date))
    qtr_val=quarter(min(regdata$Ref_Date))
    P<-ts(log(regdata$price_index),start=c(yr_val,qtr_val),frequency = 4)
    Q<-ts(log(regdata$qty_index),start=c(yr_val,qtr_val),frequency = 4)
    VAR_data <- window(ts.union(P, Q), start = c(yr_val,qtr_val))
    model<-VAR(VAR_data,p=4)
    residP<-data.frame(residuals(model))$P
    residQ<-data.frame(residuals(model))$Q
    upperP=0.1*sd(residP); upperQ=0.1*sd(residQ); lowerP=(-upperP); lowerQ=(-upperQ)
    unexp_shocks<-unexp_shocks %>%
      rbind(
        data.frame(
          Ref_Date=as.yearmon(d),
          product=p,
          qty_unexp=tail(residQ,1),
          price_unexp=tail(residP,1)
        ) %>%
          mutate(type=ifelse((qty_unexp>upperQ & price_unexp>upperP) |
                               (qty_unexp<lowerQ & price_unexp<lowerP),"Demand",NA),
                 type=ifelse((qty_unexp>upperQ & price_unexp<lowerP) |
                               (qty_unexp<lowerQ & price_unexp>upperP),"Supply",type),
                 type=ifelse(is.na(type),"Ambiguous",type))
      )
  }
}
# confirm that roughly 20% of observations are ambiguous
unexp_shocks %>% summarise(share=mean(1*(type=="Ambiguous")))

# Gather the results to generate the main figures / results
classify_types<-read.csv("Files/product_list.csv")
results<-price_change %>%
  left_join(unexp_shocks %>% dplyr::select(Ref_Date,product,type),by=c("Ref_Date","product")) %>%
  filter(!is.na(type)) %>% # drop cannabis
  left_join(classify_types,by="product") %>% 
  group_by(Ref_Date,type) %>%
  summarise(contrib=sum(contrib),
            PCE=mean(PCE))
results<-CJ(Ref_Date=unique(results$Ref_Date), # this ensures all type categories are present
            type=unique(results$type)) %>%
  left_join(results,by=c("Ref_Date","type")) %>%
  mutate(contrib=ifelse(is.na(contrib),0,contrib)) %>%
  group_by(type) %>%
  mutate(contrib_annual=rollsum(contrib,4,fill=NA,na.pad=F,align='right'),
         PCE_annual=rollsum(PCE,4,fill=NA,na.pad=F,align='right')) %>%
  group_by(Ref_Date,type) %>%
  mutate(share=contrib/PCE) %>% ungroup() %>%
  filter(Ref_Date>="Jan 2010") %>%
  mutate(type=factor(type,level=c("Supply","Ambiguous","Demand")))

# Figure 5a: Annual PCE Inflation
dev.off()
p<-ggplot(results,aes(Ref_Date,contrib_annual,group=type,fill=type))+
  geom_col(position='stack',linewidth=0.05,color='white')+
  geom_line(aes(Ref_Date,PCE_annual),size=2)+
  geom_hline(yintercept=0,size=1)+
  theme(plot.margin = unit(c(0.25,3,0.25,0.25),"lines"),
        legend.position = 'none',
        legend.key.width = unit(2,"cm"),
        legend.title=element_blank(),
        panel.grid.major.y = element_line(color='gray'))+
  geom_label(data=results %>% filter(Ref_Date==max(Ref_Date)) %>%
               arrange(desc(type)) %>%
               mutate(location=ifelse(row_number()==1,contrib_annual/2,NA),
                      location=ifelse(row_number()>1,lag(cumsum(contrib_annual),1)+contrib_annual/2,location),
                      location=ifelse(contrib_annual<0,contrib_annual-.0015,location),
                      labelname=gsub(" ","\n  ",type)),
             aes(label=paste0("  ",type),y=location,color=type),
             hjust=0,nudge_x=1/6,fontface="bold",size=3,fill='white',label.size = 0)+
  scale_x_yearmon(format='%Y')+
  scale_y_continuous(label=percent,breaks=pretty_breaks(6))+
  annotate('text',x=2021.65,hjust=1,label="Annual PCE Inflation",size=3,y=.055)+
  labs(x="",
       title="Contribution of Supply and Demand Shocks to Annual PCE Inflation",
       subtitle="Source: Authors' calculations using Statistics Canada data table 36-10-0124",
       y="Per Cent")
gt <- ggplotGrob(p)
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)
ggsave("Figures/Figure5a.png",gt,width=8,height=4)

# Figure 5b: Quarterly PCE Inflation
dev.off()
p<-ggplot(results,aes(Ref_Date,contrib,group=type,fill=type,color=type))+
  geom_col(position='stack',size=0.05,color='white')+
  geom_line(aes(Ref_Date,PCE),size=2,color='black',inherit.aes = F)+
  geom_hline(yintercept=0,size=1)+
  theme(plot.margin = unit(c(0.25,3,0.25,0.25),"lines"),
        legend.position = 'none',
        legend.key.width = unit(2,"cm"),
        legend.title=element_blank(),
        panel.grid.major.y = element_line(color='gray'))+
  geom_label(data=results %>% filter(Ref_Date==max(Ref_Date)) %>%
               arrange(desc(type)) %>%
               mutate(location=ifelse(row_number()==1,contrib/2,NA),
                      location=ifelse(row_number()>1,lag(cumsum(contrib),1)+contrib/2,location),
                      location=ifelse(contrib_annual<0,contrib-.0015,location),
                      labelname=gsub(" ","\n  ",type)),
             aes(label=paste0("  ",type),y=location,color=type),
             hjust=0,nudge_x=1/6,fontface="bold",size=3,fill='white',label.size = 0)+
  scale_x_yearmon(format='%Y')+
  scale_y_continuous(label=percent,breaks=pretty_breaks(6))+
  annotate('text',x=2021.65,hjust=1,label="Quarterly PCE Inflation",size=3,y=.0175)+
  labs(x="",
       title="Contribution of Supply and Demand Shocks to Quarterly PCE Inflation",
       subtitle="Source: Authors' calculations using Statistics Canada data table 36-10-0124",
       y="Per Cent")
gt <- ggplotGrob(p)
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)
ggsave("Figures/Figure5b.png",gt,width=8,height=4)

# Save csv of the results - annual
write.csv(results %>% select(Date=Ref_Date,contrib_annual,
                             type,PCE_annual) %>%
            spread(type,contrib_annual) %>%
            mutate(Date=as.yearqtr(Date)),"Files/Results_2010_Latest_Annual.csv",row.names = F)
# Save csv of the results - quarterly
write.csv(results %>% select(Date=Ref_Date,contrib,
                             type,PCE) %>%
            spread(type,contrib) %>%
            mutate(Date=as.yearqtr(Date)),"Files/Results_2010_Latest_Quarterly.csv",row.names = F)

# Table 1: Top Contributors to Annual PCE Inflation (Latest Quarter)
top_contributors<-price_change %>%
  left_join(unexp_shocks %>% dplyr::select(Ref_Date,product,type),by=c("Ref_Date","product")) %>%
  filter(Ref_Date>=max(Ref_Date)-1) %>%
  dplyr::select(Ref_Date,product,price_change,contrib,type) %>%
  drop_na() %>%
  group_by(product) %>%
  mutate(contrib_sup=rollsum(contrib*(type=="Supply"),4,fill=NA,na.pad=F,align='right'),
         contrib_dem=rollsum(contrib*(type=="Demand"),4,fill=NA,na.pad=F,align='right'),
         contrib_amb=rollsum(contrib*(type=="Ambiguous"),4,fill=NA,na.pad=F,align='right')) %>%
  filter(Ref_Date==max(Ref_Date))
table1<-top_contributors %>%
  arrange(-contrib_dem) %>% ungroup() %>%
  filter(rank(-contrib_dem)<=10) %>%
  mutate(contrib=round(100*contrib_dem,2)) %>%
  dplyr::select(Demand=product,contrib)
table2<-top_contributors %>%
  arrange(-contrib_sup) %>% ungroup() %>%
  filter(rank(-contrib_sup)<=10) %>%
  mutate(contrib=round(100*contrib_sup,2)) %>%
  dplyr::select(Supply=product,contrib)
table<-data.frame(table1,table2) %>% # Shorten selected product names
  mutate(Demand=case_when(
    Demand=="Cable, satellite and other program distribution services" ~ "Cable/satellite service",
    Demand=="Gas" ~ "Natural gas for homes",
    Demand=="Food" ~ "Food (groceries)",
    Demand=="Other fuels" ~ "Other fuels for homes)",
    Demand=="Fuels and lubricants" ~ "Vehicle fuel/lubricants",
    Demand=="Spare parts and accessories for vehicles" ~ "Vehicle parts/accessories",
    Demand=="Major durables for outdoor recreation" ~ "Durables for outdoor rec.",
    Demand=="Other appliances, articles and products for personal care" ~ "Personal elec. appliances",
    Demand=="New trucks, vans and sport utility vehicles" ~ "New trucks/vans/SUVs",
    Demand=="Food and non-alcoholic beverage services" ~ "Food (restaurants)",
    Demand=="Carpets and other floor coverings" ~ "Carpets/floor coverings",
    Demand=="Cable, satellite and other program distribution services" ~ "Cable/satellite services",
    TRUE ~ Demand
  ),
  Supply=case_when(
    Supply=="Veterinary and other services for pets" ~ "Vet. services for pets",
    Supply=="Fuels and lubricants" ~ "Vehicle fuel/lubricants",
    Supply=="Gas" ~ "Natural gas for homes",
    Supply=="New trucks, vans and sport utility vehicles" ~ "New trucks/vans/SUVs",
    Supply=="Major household appliances" ~ "Household appliances",
    Supply=="Food" ~ "Food (groceries)",
    Supply=="Spare parts and accessories for vehicles" ~ "Vehicle parts/accessories",
    Supply=="Food and non-alcoholic beverage services" ~ "Food (restaurants)",
    TRUE ~ Supply
  )
  )
table
write.table(table,'Figures/Table1.txt',row.names = F)

###############
# Section 3.5 #
###############

# Figure 6: Goods and Services
plotdata<-unexp_shocks %>%
  left_join(classify_types %>% dplyr::select(product,category=good_service),by=c("product")) %>%
  left_join(price_change,by=c("Ref_Date","product")) %>%
  group_by(Ref_Date,type,category) %>%
  summarise(contrib=sum(contrib))
plotdata<-CJ(Ref_Date=unique(plotdata$Ref_Date),
             type=unique(plotdata$type),
             category=unique(plotdata$category)) %>%
  left_join(plotdata,by=c("Ref_Date","type","category")) %>%
  mutate(contrib=ifelse(is.na(contrib),0,contrib)) %>%
  group_by(type,category) %>%
  mutate(contrib_annual=rollsum(contrib,4,fill=NA,na.pad=F,align='right')) %>%
  filter(Ref_Date>="Jan 2017") %>%
  mutate(category=case_when(
    category=="good" ~ "Goods",
    category=="service" ~ "Services",
  )) %>%
  mutate(type=factor(type,level=c("Supply","Ambiguous","Demand")))
ggplot(plotdata,aes(Ref_Date,contrib_annual,group=type,fill=type))+
  geom_col(position='stack',size=0.05,color='white')+
  facet_wrap(~category)+
  theme(legend.position = 'bottom')+
  scale_x_continuous("",breaks=seq(2017,2024,2))+
  scale_y_continuous("Per Cent",label=percent)+
  labs(title="Goods and Services Inflation",
       subtitle="Source: Authors' calculations using Statistics Canada data table 36-10-0124")
ggsave("Figures/Figure6.png",width=8,height=4)

# Figure 7: Energy Intensive or Not
plotdata<-unexp_shocks %>%
  left_join(classify_types %>% dplyr::select(product,energy_share),by=c("product")) %>%
  left_join(price_change,by=c("Ref_Date","product")) %>%
  mutate(category=ifelse(energy_share<=0.005,"Not Energy Intensive",NA), # 25th percentile (weighted)
         category=ifelse(energy_share>=0.02,"Energy Intensive",category), # 75th percentile (weighted)
         category=ifelse(energy_share<0.02 & energy_share>0.005,"Moderately Energy Intensive",category)) %>%
  group_by(Ref_Date,type,category) %>%
  summarise(contrib=sum(contrib))
plotdata<-CJ(Ref_Date=unique(plotdata$Ref_Date),
             type=unique(plotdata$type),
             category=unique(plotdata$category)) %>%
  left_join(plotdata,by=c("Ref_Date","type","category")) %>%
  mutate(contrib=ifelse(is.na(contrib),0,contrib)) %>%
  group_by(type,category) %>%
  mutate(contrib_annual=rollsum(contrib,4,fill=NA,na.pad=F,align='right')) %>%
  filter(Ref_Date>="Jan 2017") %>%
  mutate(type=factor(type,level=c("Supply","Ambiguous","Demand")))
ggplot(plotdata,aes(Ref_Date,contrib_annual,group=type,fill=type))+
  geom_col(position='stack',size=0.05,color='white')+
  facet_wrap(~category)+
  theme(legend.position = 'bottom')+
  scale_x_continuous("",breaks=seq(2017,2024,2))+
  scale_y_continuous("Per Cent",label=percent)+
  labs(title="Inflation Contributions by Energy Intensity",
       subtitle="Source: Authors' calculations using Statistics Canada data table 36-10-0124")
ggsave("Figures/Figure7.png",width=8,height=4)

# Figure 8: Traded vs Non-Traded
plotdata<-unexp_shocks %>%
  left_join(classify_types %>% dplyr::select(product,import_share),by=c("product")) %>%
  left_join(price_change,by=c("Ref_Date","product")) %>%
  mutate(category=ifelse(import_share==0,"Not Traded",NA), # zero is the 25th percentile
         category=ifelse(import_share>=21.4,"Highly Traded",category), # 15 is the weighted median among traded items; 21.4 is the 75th percentile
         category=ifelse(import_share<21.4 & import_share>0,"Moderately Traded",category)) %>%
  group_by(Ref_Date,type,category) %>%
  summarise(contrib=sum(contrib))
plotdata<-CJ(Ref_Date=unique(plotdata$Ref_Date),
             type=unique(plotdata$type),
             category=unique(plotdata$category)) %>%
  left_join(plotdata,by=c("Ref_Date","type","category")) %>%
  mutate(contrib=ifelse(is.na(contrib),0,contrib)) %>%
  group_by(type,category) %>%
  mutate(contrib_annual=rollsum(contrib,4,fill=NA,na.pad=F,align='right')) %>%
  filter(Ref_Date>="Jan 2017") %>%
  mutate(type=factor(type,level=c("Supply","Ambiguous","Demand")))
ggplot(plotdata,aes(Ref_Date,contrib_annual,group=type,fill=type))+
  geom_col(position='stack',size=0.05,color='white')+
  facet_wrap(~category)+
  theme(legend.position = 'bottom')+
  scale_x_continuous("",breaks=seq(2017,2024,2))+
  scale_y_continuous("Per Cent",label=percent)+
  labs(title="Inflation Contributions by Trade Intensity",
       subtitle="Source: Authors' calculations using Statistics Canada data table 36-10-0124")
ggsave("Figures/Figure8.png",width=8,height=4)

#############
# Section 4 #
#############

# Figure 10: Persistence
plotdata<-price_change %>%
  dplyr::select(Ref_Date,product,price_index) %>%
  group_by(product) %>%
  mutate(Ref_Date=as.yearqtr(Ref_Date),
         price_change=price_index/lag(price_index,4)-1) %>%
  filter(Ref_Date>="1995 Q1",Ref_Date<"2020 Q1")
results<-NULL
for (p in unique(plotdata$product)){
  temp<-tidy(lm(price_change~lag(price_change,4),
                data=filter(plotdata,product==p)))
  results<-results %>%
    rbind(
      data.frame(
        beta=as.numeric(temp[2,2]),
        tstat=as.numeric(temp[2,4]),
        product=p
      )
    )
}
plotdata<-unexp_shocks %>%
  left_join(results %>%
              mutate(type=ifelse(tstat>1.96,"Persistent","Not Persistent")) %>%
              dplyr::select(product,category=type),by=c("product")) %>%
  left_join(price_change,by=c("Ref_Date","product")) %>%
  left_join(classify_types %>% dplyr::select(product,category2=bank_influence),by=c("product")) %>% 
  group_by(Ref_Date,type,category,category2) %>%
  summarise(contrib=sum(contrib))
plotdata<-CJ(Ref_Date=unique(plotdata$Ref_Date),
             type=unique(plotdata$type),
             category=unique(plotdata$category),
             category2=unique(plotdata$category2)) %>%
  left_join(plotdata,by=c("Ref_Date","type","category","category2")) %>%
  mutate(contrib=ifelse(is.na(contrib),0,contrib)) %>%
  group_by(type,category,category2) %>%
  mutate(contrib_annual=rollsum(contrib,4,fill=NA,na.pad=F,align='right')) %>%
  filter(Ref_Date>="Jan 2017") %>%
  mutate(category2=case_when(
    category2=="moderate" ~ "Moderately Sensitive",
    category2=="yes" ~ "Sensitive to Monetary Policy",
    category2=="no" ~ "Not Sensitive",
  )) %>%
  mutate(category2=factor(category2,level=c("Sensitive to Monetary Policy",
                                            "Moderately Sensitive",
                                            "Not Sensitive")),
         category=factor(category,level=c("Persistent","Not Persistent"))) %>%
  mutate(type=factor(type,level=c("Supply","Ambiguous","Demand")))
ggplot(plotdata,aes(Ref_Date,contrib_annual,group=type,fill=type))+
  geom_col(position='stack',size=0.05,color='white')+
  facet_grid(rows=vars(category2),cols=vars(category))+
  theme(legend.position = 'bottom')+
  scale_x_continuous("",breaks=seq(2017,2024,2))+
  scale_y_continuous("Per Cent",label=percent)+
  labs(title="Inflation Persistence and Sensitivity to Monetary Policy",
       subtitle='Source: Authors calculations using Statistics Canada data table 36-10-0124 and Chernis and Luu (2018)')
ggsave("Figures/Figure10.png",width=8,height=8)

#################
# Section 3.5.4 #
#################

# Figure 9: Contributions to PCE Inflation, 1972 to 2019
products_aggregate<-c("Food and non-alcoholic beverages [C11]",
                      "Alcoholic beverages, tobacco and cannabis [C12]",
                      "Clothing and footwear [C13]",
                      "Housing, water, electricity, gas and other fuels [C14]",
                      "Furnishings, household equipment and other goods and services related to the dwelling and property [C15]",
                      "Health [C16]",
                      "Transport [C17]",
                      "Communications [C18]",
                      "Recreation and culture [C19]",
                      "Education [C21]",
                      "Food, beverage and accommodation services [C22]",
                      "Insurance and financial services [C23]",
                      "Miscellaneous goods and services [C24]",
                      "Expenditure by non-residents in Canada [C252]", # negative, so affects the log()
                      "Net expenditure abroad [C25]")
products_exclude<-c(products_aggregate,
                    "Goods [CG]","Durable goods [CD]",
                    "Semi-durable goods [CSD]","Non-durable goods [CND]",
                    "Services [CS]",
                    "Expenditure by Canadians abroad [C251]",
                    "Household final consumption expenditure [C]")
real_change<-pce_data %>%
  filter(!Estimates %in% products_exclude,
         !grepl("Cannabis",Estimates),
         `Seasonal adjustment`=="Seasonally adjusted at annual rates",
         Prices=="Chained (2012) dollars") %>%
  group_by(Estimates) %>%
  mutate(qty_change=Value/lag(Value,1),
         qty_index=Value) %>%
  dplyr::select(Ref_Date,product=Estimates,qty_change,qty_index)
nom_change<-pce_data %>%
  filter(!Estimates %in% products_exclude,
         !grepl("Cannabis",Estimates),
         `Seasonal adjustment`=="Seasonally adjusted at annual rates",
         Prices=="Current prices") %>%
  group_by(Estimates) %>%
  mutate(nom_change=Value/lag(Value,1)) %>%
  dplyr::select(Ref_Date,product=Estimates,nom_change,spending=Value)
price_change<-nom_change %>%
  left_join(real_change,by=c("Ref_Date","product")) %>%
  mutate(price_change=nom_change/qty_change,
         price_index=spending/qty_index) %>%
  group_by(Ref_Date) %>%
  mutate(total=sum(spending),
         weight=spending/total) %>%
  group_by(product) %>%
  mutate(weight_lag=lag(weight,1)) %>%
  group_by(Ref_Date) %>%
  mutate(PCE=sum((price_change-1)*weight_lag),
         contrib=(price_change-1)*weight_lag)
dates<-unique((price_change %>% filter(Ref_Date>="Jan 1971",Ref_Date<"Jan 2020"))$Ref_Date)
prods<-unique((price_change)$product)
unexp_shocks<-NULL
for (p in prods){
  for (d in dates){
    regdata<-price_change %>% ungroup() %>%
      filter(Ref_Date>=as.yearmon(d)-10, # ten year rolling window
             Ref_Date<=as.yearmon(d),
             product==p)
    yr_val=year(min(regdata$Ref_Date))
    qtr_val=quarter(min(regdata$Ref_Date))
    P<-ts(log(regdata$price_index),start=c(yr_val,qtr_val),frequency = 4)
    Q<-ts(log(regdata$qty_index),start=c(yr_val,qtr_val),frequency = 4)
    VAR_data <- window(ts.union(P, Q), start = c(yr_val,qtr_val))
    model<-VAR(VAR_data,p=4)
    residP<-data.frame(residuals(model))$P
    residQ<-data.frame(residuals(model))$Q
    upperP=0.1*sd(residP); upperQ=0.1*sd(residQ); lowerP=(-upperP); lowerQ=(-upperQ)
    unexp_shocks<-unexp_shocks %>%
      rbind(
        data.frame(
          Ref_Date=as.yearmon(d),
          product=p,
          qty_unexp=tail(residQ,1),
          price_unexp=tail(residP,1)
        ) %>%
          mutate(type=ifelse((qty_unexp>upperQ & price_unexp>upperP) |
                               (qty_unexp<lowerQ & price_unexp<lowerP),"Demand",NA),
                 type=ifelse((qty_unexp>upperQ & price_unexp<lowerP) |
                               (qty_unexp<lowerQ & price_unexp>upperP),"Supply",type),
                 type=ifelse(is.na(type),"Ambiguous",type))
      )
  }
}
results<-price_change %>%
  left_join(unexp_shocks %>% dplyr::select(Ref_Date,product,type),by=c("Ref_Date","product")) %>%
  group_by(Ref_Date,type) %>%
  summarise(contrib=sum(contrib),
            PCE=mean(PCE)) %>%
  group_by(type) %>%
  mutate(contrib_annual=rollsum(contrib,4,fill=NA,na.pad=F,align='right'),
         PCE_annual=rollsum(PCE,4,fill=NA,na.pad=F,align='right')) %>%
  group_by(Ref_Date,type) %>%
  mutate(share=contrib/PCE) %>% ungroup() %>% drop_na() %>% 
  filter(Ref_Date>="Jan 1972" & Ref_Date<"Jan 2020") %>%
  mutate(type=factor(type,level=c("Supply",'Ambiguous',"Demand")))
dev.off()
p<-ggplot(results,aes(Ref_Date,contrib_annual,group=type,fill=type))+
  geom_area(position='stack',size=0.05,color='white')+
  geom_line(aes(Ref_Date,PCE_annual),linewidth=2,inherit.aes = F)+
  geom_hline(yintercept=0,size=1)+
  theme(plot.margin = unit(c(0.25,3,0.25,0.25),"lines"),
        legend.position = 'none',
        legend.key.width = unit(2,"cm"),
        legend.title=element_blank(),
        panel.grid.major.y = element_line(color='gray'))+
  geom_label(data=results %>% filter(Ref_Date==max(Ref_Date)) %>%
               arrange(desc(type)) %>%
               mutate(location=ifelse(row_number()==1,contrib_annual/2,NA),
                      location=ifelse(row_number()>1,lag(cumsum(contrib_annual),1)+contrib_annual/2,location),
                      location=ifelse(contrib_annual<0,contrib_annual-.0015,location),
                      location=ifelse(type=="Demand",location-.0025,location),
                      labelname=gsub(" ","\n  ",type)),
             aes(label=paste0("  ",type),y=location,color=type),
             hjust=0,nudge_x=1/6,fontface="bold",size=3,fill='white',label.size = 0)+
  scale_x_yearmon(breaks=pretty_breaks(6),
                  format='%Y')+
  scale_y_continuous(label=percent,expand=c(0,0))+
  annotate('text',x=1985,hjust=0,label="Annual PCE Inflation",size=3,y=.11)+
  labs(x="",
       title="Contribution of Supply and Demand Shocks to PCE Inflation",
       subtitle="Source: Authors' calculations using Statistics Canada data table 36-10-0107",
       y="Per Cent")
gt <- ggplotGrob(p)
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)
ggsave("Figures/Figure9.png",gt,width=8,height=4)

# Save csv of the results - annual
write.csv(results %>% select(Date=Ref_Date,contrib_annual,
                             type,PCE=PCE_annual) %>%
            spread(type,contrib_annual) %>%
            mutate(Date=as.yearqtr(Date)),"Files/Results_1972_2019_Annual.csv",row.names = F)
# Save csv of the results - quarterly
write.csv(results %>% select(Date=Ref_Date,contrib,
                             type,PCE) %>%
            spread(type,contrib) %>%
            mutate(Date=as.yearqtr(Date)),"Files/Results_1972_2019_Quarterly.csv",row.names = F)

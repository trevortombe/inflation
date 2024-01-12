# Load required packages, define variables
source("R/Setup.R")

# Fetch the latest data
source("R/GetData.R")

#############################
# Generate the Plot Updates #
#############################

# Headline and Core Inflation Measures
plotdata<-data %>%
  filter(Products.and.product.groups %in% c("All-items","All-items excluding food and energy"),
         GEO=="Canada") %>%
  group_by(Products.and.product.groups,GEO) %>%
  mutate(YoY=Value/lag(Value,12)-1) %>%
  select(Ref_Date,YoY,Products.and.product.groups) %>%
  left_join(
    BoCdata %>% filter(UOM=="Percent") %>%
      mutate(alt=case_when(
        grepl("trim",Alternative.measures) ~ "Trimmed",
        grepl("median",Alternative.measures) ~ "Median",
        grepl("common",Alternative.measures) ~ "Common"
      )) %>%
      select(Ref_Date,alt,Value) %>%
      group_by(Ref_Date) %>%
      summarise(min=min(Value),
                max=max(Value)),by="Ref_Date"
  )
ggplot(plotdata %>%
         filter(Ref_Date>="Jan 2015"),aes(Ref_Date,(1+YoY)-1,group=Products.and.product.groups,
                                          color=Products.and.product.groups))+
  geom_hline(yintercept=0,size=1)+
  geom_ribbon(aes(ymin=min/100,ymax=max/100,x=Ref_Date),alpha=0.5,fill=col[3],
              inherit.aes = F)+
  geom_line(linewidth=2)+
  annotate('text',x=2020.5,y=.04,label="Range of Core\nBoC Measures",
           color=col[3],fontface='bold',alpha=0.6)+
  scale_color_brewer(name="",palette="Set1")+
  labs(x="",y="Per Cent Change, YoY")+
  scale_y_continuous(label=percent)+
  scale_x_continuous(breaks=pretty_breaks(6))+
  labs(x="",y="Per Cent",
       title="Year-over-Year Change in Consumer Prices in Canada",
       caption="Graph by @trevortombe",
       subtitle="Source: Own calculations from Statistics Canada data table 18-10-0004 and 18-10-0256")
ggsave('Plots/CoreInflation.png',width=8,height=4)

# Inflation Expectations
plotdata<-yields %>%
  filter(Financial.market.statistics %in% c("Government of Canada benchmark bond yields, long term",
                                              "Real return benchmark bond yield, long term"),
         Ref_Date>=2000,Value!=0) %>%
  mutate(date=as.Date(Ref_Date,"%Y-%m-%d")) %>%
  select(date,Value,type=Financial.market.statistics) %>%
  mutate(type=ifelse(type=="Real return benchmark bond yield, long term","real","nom")) %>%
  spread(type,Value) %>%
  mutate(break_even=(1+nom/100)/(1+real/100)-1) %>%
  drop_na()
ggplot(plotdata,aes(date,break_even))+
  geom_line(size=1,color=col[1])+
  geom_hline(yintercept=0,size=1)+
  geom_text(data=filter(plotdata,date==max(date)),color=col[1],nudge_x=180,
            hjust=0,aes(label=paste0("Latest:",
                                     "\n",percent(break_even,.1))))+
  scale_y_continuous(label=percent)+
  scale_x_date(date_breaks="4 years",date_labels="%Y",
               limits=as.Date(c(min(plotdata$date),max(plotdata$date)+500)))+
  labs(y="Percent",x="",
       title="Long-Run Breakeven Inflation Expectations in Canada",
       subtitle=paste("Note: based on the difference between nominal and real return Government of Canada long-run bonds.
Source: Own calculations from Statistics Canada data table 10-10-0139-01, with data to",max(plotdata$date)),
       caption="Graph by @trevortombe")
ggsave("Plots/Expectations.png",width=7,height=4)

# Share of Products with >3% Inflation
temp<-data %>% 
  filter(GEO=="Canada",Ref_Date>="Jan 1980") %>%
  select(Ref_Date,product=Products.and.product.groups,Value) %>%
  filter(product %in% product_list$product) %>%
  group_by(product) %>%
  mutate(change=Value/lag(Value,12)-1,
         above=change>0.03) %>%
  drop_na() %>% # this drops a lot, be careful
  group_by(Ref_Date) %>%
  summarise(above=mean(above),
            median=median(change))
ggplot(temp,aes(Ref_Date,above))+
  geom_line(size=2,color=col[1])+
  geom_hline(yintercept=0,size=1)+
  scale_y_continuous(label=percent)+
  scale_x_continuous(breaks=pretty_breaks(6))+
  labs(x="",y="Per Cent",
       title="Share of Consumer Price Items With >3% YoY Price Changes",
       caption="Graph by @trevortombe",
       subtitle="Source: Own calculations from Statistics Canada data table 18-10-0004")
ggsave('Plots/ProductShare3Plus.png',width=8,height=4)

# Services excluding shelter services
plotdata<-data %>%
  filter(GEO=="Canada",Ref_Date>="Jan 2003") %>%
  select(Ref_Date,product=Products.and.product.groups,Value) %>%
  left_join(link_months,by="Ref_Date") %>%
  left_join(weights_monthly,by=c("product","basket")) %>%
  group_by(Ref_Date) %>%
  mutate(all=weighted.mean(Value,product=="Services"),
         sub_w=weighted.mean(w,product=="Services")) %>%
  group_by(product) %>%
  mutate(period=cumsum(ifelse(Ref_Date==link_month,1,0))) %>%
  group_by(product,period) %>%
  mutate(I_atlink=Value[1],
         all_atlink=all[1]) %>%
  group_by(product) %>%
  mutate(cpi=all/lag(all,12)-1,
         relimp=(w/sub_w)*(Value/I_atlink)/(all/all_atlink), # relative importance
         relimp_old=lag(relimp,12), # relative importance using weights and prices from t-12
         relimp_new=(w/sub_w)*(lag(Value,12)/I_atlink)/(lag(all,12)/all_atlink)) %>% # relative importance using current weights but t-12 prices) %>%
  mutate(effective_weight=(relimp_old/(Value/I_atlink)+(1-1/(Value/I_atlink))*relimp_new), # an intuitive way? same as statcan approach
         contrib=(Value/lag(Value,12))*effective_weight-relimp_old, # main estimate
         excluding_item=(cpi-contrib)/(1-effective_weight)) %>%
  filter(product %in% c("Rented accommodation",
                        "Owned accommodation")) %>%
  select(Ref_Date,cpi,effective_weight,contrib,product) %>%
  group_by(Ref_Date) %>%
  summarise(cpi=mean(cpi),
            weight=sum(effective_weight),
            contrib=sum(contrib)) %>%
  mutate(excluding=(cpi-contrib)/(1-weight)) %>%
  left_join(
    data %>%
      filter(Products.and.product.groups=="Services excluding shelter services",
             GEO=="Canada") %>%
      mutate(actual=Value/lag(Value,12)-1) %>%
      select(Ref_Date,actual),
    by="Ref_Date"
  ) %>%
  filter(Ref_Date>="Jan 2010")
ggplot(plotdata,aes(Ref_Date))+
  geom_hline(yintercept=0,size=1,color='gray')+
  geom_line(aes(y=excluding,color="Trevor's Not Terrible Calculation"),size=1.5)+
  geom_line(aes(y=actual,color="Actual Series, Terminated :("),size=1.5)+
  labs(title="CPI Change for Services Excluding Shelter Services",
       caption="Graph by @trevortombe",
       x="",y="Percent Change (YoY)",
       subtitle="Source: own calculations from Statistics Canada data tables 18-10-0007 and 18-10-0004")+
  geom_vline(xintercept=as.yearmon("2021-05"),linetype='dashed')+
  scale_y_continuous(label=percent)+
  scale_x_continuous(breaks=pretty_breaks(6))
ggsave("Plots/ServicesExShelter.png",width=7.5,height=4.5)

# Save a CSV file
write.csv(plotdata %>% select(Date=Ref_Date,
                              TrevorSeries=excluding,
                              StatCanSeries=actual),
          "Data/ServicesExShelter.csv",row.names = F)

# Decomposition of Specific Contributors
# https://www.oecd.org/sdd/prices-ppp/OECD-calculation-contributions-annual-inflation.pdf
plotdata<-decomp_cpi %>%
  filter(product %in% c("Food purchased from stores",
                        "Energy",
                        "Rented accommodation",
                        "Owned accommodation","Water",
                        "Goods excluding food purchased from stores and energy")) %>%
  group_by(Ref_Date) %>%
  mutate(total=sum(contrib),
         `Services ex shelter`=cpi-total) %>%
  select(Ref_Date,product,contrib,`Services ex shelter`,cpi) %>%
  spread(product,contrib) %>%
  gather(product,contrib,-Ref_Date,-cpi) %>%
  mutate(product=ifelse(product %in% c("Rented accommodation",
                                       "Owned accommodation","Water"),
                        "Shelter ex energy",product)) %>%
  group_by(Ref_Date,product) %>%
  summarise(contrib=sum(contrib),
            cpi=mean(cpi)) %>%
  ungroup() %>%
  filter(Ref_Date>="Jan 2015") %>%
  mutate(product=case_when(
    product=="Food purchased from stores" ~ "Groceries",
    product=="Goods excluding food purchased from stores and energy" ~ "Goods ex groceries/energy",
    TRUE ~ product
  )) %>%
  filter(!is.na(cpi)) %>%
  mutate(product=factor(product,levels=c("Services ex shelter",
                                         "Shelter ex energy",
                                         "Groceries",
                                         "Goods ex groceries/energy",
                                         "Energy"))) # ensure negatives are at bottom
dev.off()
p<-ggplot(plotdata,aes(Ref_Date,contrib,group=product,fill=product))+
  geom_col(position='stack')+
  geom_hline(yintercept=0,size=1)+
  geom_line(aes(y=cpi),size=1.5)+
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
                      shift=-contrib[1],
                      location=ifelse(row_number()>1,lag(cumsum(contrib),1)+contrib/2+shift,location),
                      location=ifelse(product=="Energy",-0.005,location),
                      labelname=gsub(" ","\n  ",product)),
             aes(label=paste0("  ",product),y=location,color=product),
             hjust=0,nudge_x=1/12,fontface="bold",size=3,fill='white',label.size=0)+
  annotate('text',x=2021.5,hjust=1,y=0.05,label="All-Items CPI",size=3)+
  labs(x="",
       title="Contribution of Selected Products to Canada's Inflation Rate",
       subtitle="Source: own calculations from Statistics Canada data tables 18-10-0007 and 18-10-0004",
       caption="Graph by @trevortombe",
       y="Year-over-Year Change")
gt <- ggplotGrob(p)
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)
ggsave("Plots/MainDecomposition.png",gt,width=10,height=4.5)

# Change Since Feb 2020
time_age=max(data$Ref_Date)-as.yearmon("Feb 2020")
temp<-decomp_cpi %>%
  filter(Ref_Date>=max(Ref_Date)-time_age) %>%
  group_by(product) %>%
  mutate(change=contrib-contrib[1],
         total=cpi-cpi[1],
         share=change/total) %>%
  ungroup() %>%
  filter(Ref_Date==max(Ref_Date)) %>%
  select(Ref_Date,product,change,total,share)
plotdata<-temp %>%
  filter(product %in% c("Energy",
                        "Purchase and leasing of passenger vehicles",
                        "Purchase of recreational vehicles and outboard motors",
                        "Owned accommodation","Rented accommodation",
                        "Household furnishings and equipment",
                        "Food purchased from stores")) %>%
  group_by(Ref_Date) %>%
  mutate(included=sum(change),
         `All Other Items`=total-included) %>%
  select(Ref_Date,product,change,`All Other Items`,total) %>%
  spread(product,change) %>%
  gather(product,change,-Ref_Date,-total) %>%
  mutate(product=ifelse(product %in% c("Purchase of recreational vehicles and outboard motors",
                                       "Purchase and leasing of passenger vehicles"),
                        "New/Used\nVehicles",product)) %>%
  group_by(Ref_Date,product) %>%
  summarise(change=sum(change)) %>%
  ungroup() %>%
  filter(Ref_Date>="Jan 2018") %>%
  mutate(product=case_when(
    product=="Food purchased from stores" ~ "Food\n(Groceries)",
    product=="All Other Items" ~ "Everything\nElse",
    product=="Water, fuel and electricity" ~ "Water\nFuel\nElectricity",
    product=="Mortgage interest cost" ~ "Mortgage\nInterest",
    product=="Household furnishings and equipment" ~ "Household\nFurnishings\nand Equip.",
    product=="Purchase and leasing of passenger vehicles" ~ "New\nVehicles",
    product=="Owned accommodation" ~ "Owned\nAccommodation",
    product=="Rented accommodation" ~ "Rented\nAccommodation",
    TRUE ~ product
  )) %>%
  select(type=product,value=change) %>%
  rbind(
    data.frame(
      type=as.character(c(max(decomp_cpi$Ref_Date)-time_age,max(decomp_cpi$Ref_Date))),
      value=c(filter(inf_rates,Ref_Date==max(inf_rates$Ref_Date)-time_age)$YoY,
              filter(inf_rates,Ref_Date==max(inf_rates$Ref_Date))$YoY)
    )
  ) %>%
  mutate(id=ifelse(type==as.character(max(inf_rates$Ref_Date)-time_age),0.5,row_number()),
         id=ifelse(type=="Everything\nElse",98,id),
         id=ifelse(type=="Energy",1,id),
         id=ifelse(type=="Homeowners'\ndepreciation",2,id),
         id=ifelse(type==as.character(max(inf_rates$Ref_Date)),99,id)) %>%
  arrange(id) %>%
  mutate(type=factor(type,levels=type),
         id=seq_along(value),
         sign=ifelse(value>0,"in","out"),
         sign=ifelse(id==n(),"net",sign),
         sign=ifelse(id==1,"first",sign),
         end=cumsum(value),
         end=ifelse(id==n(),0,end),
         start=ifelse(id>1,lag(end,1),0))
ggplot(plotdata %>% filter(sign!="first" & sign!="net") %>%
         mutate(id=id-1))+
  geom_segment(x=-0.5,xend=0.5,
               y=plotdata[1,]$value,yend=plotdata[1,]$value,size=2,
               color=col[2])+
  geom_segment(x=dim(plotdata)[1]-1-0.5,xend=dim(plotdata)[1]-1+0.5,
               y=plotdata[dim(plotdata)[1],]$value,
               yend=plotdata[dim(plotdata)[1],]$value,size=2,color=col[2])+
  geom_rect(aes(x=type,xmin = id - 0.4, xmax = id + 0.4,
                ymin=end,ymax=start,fill=sign),show.legend = F)+
  annotate('text',x=0,y=plotdata[1,]$value,size=3.5,
           vjust=-0.5,color=col[2],fontface='bold',
           label=paste0(plotdata[1,]$type,": ",
                        percent(plotdata[1,]$value,0.1)))+
  annotate('text',x=dim(plotdata)[1]-1,
           y=plotdata[dim(plotdata)[1],]$value,size=3.5,
           vjust=1.5,color=col[2],fontface='bold',
           label=paste0(plotdata[dim(plotdata)[1],]$type,": ",
                        percent(plotdata[dim(plotdata)[1],]$value,0.1)))+
  coord_cartesian(xlim=c(0,dim(plotdata)[1]-1))+
  geom_segment(data=plotdata %>% filter(sign!="net",sign!='first') %>%
                 mutate(id=id-1),
               aes(x=id+0.45,xend=id+0.45,y=start,yend=end),
               size=0.5,arrow=arrow(type="closed",length=unit(0.1,"cm")))+
  # scale_fill_manual(name="",values=c(col[3],col[2],col[2],col[1]))+
  scale_y_continuous(breaks=pretty_breaks(n=6),label=percent)+
  labs(x="",y="Percentage Points",
       title="Contributions to Changes in Canada's Inflation Rate",
       subtitle="Source: own calculations from Statistics Canada data tables 18-10-0007 and 18-10-0004",
       caption="Graph by @trevortombe")
ggsave("Plots/ChangeFeb2020.png",width=10,height=4)

# Top Contributors to Changes since Previous Month
contributor_list<-product_list %>%
  mutate(product=ifelse(category=="",product,category)) %>%
  select(product) %>% distinct()
top_contrib<-decomp_cpi %>% 
  filter(product %in% contributor_list$product) %>%
  group_by(product) %>%
  mutate(change_contrib=contrib-lag(contrib,1),
         change_cpi=cpi-lag(cpi,1)) %>%
  ungroup() %>%
  filter(Ref_Date==max(Ref_Date)) %>%
  select(Ref_Date,product,contrib,cpi,change_contrib,change_cpi) %>%
  arrange(-abs(change_contrib)) %>%
  mutate(cum=cumsum(change_contrib))
time_age=max(data$Ref_Date)-(max(data$Ref_Date)-1/12)
temp<-decomp_cpi %>%
  filter(Ref_Date>=max(Ref_Date)-time_age) %>%
  group_by(product) %>%
  mutate(change=contrib-contrib[1],
         total=cpi-cpi[1],
         share=change/total) %>%
  ungroup() %>%
  filter(Ref_Date==max(Ref_Date)) %>%
  select(Ref_Date,product,change,total,share) %>%
  left_join(product_list)
plotdata<-temp %>%
  filter(product %in% filter(top_contrib,row_number()<=5)$product) %>%
  group_by(Ref_Date) %>%
  mutate(included=sum(change),
         `All Other Items`=total-included) %>%
  select(Ref_Date,product,change,`All Other Items`,total) %>%
  spread(product,change) %>%
  gather(product,change,-Ref_Date,-total) %>%
  mutate(product=ifelse(product %in% c("Purchase of recreational vehicles and outboard motors",
                                       "Purchase of passenger vehicles",
                                       "Purchase and leasing of passenger vehicles"),
                        "New\nvehicles",product)) %>%
  group_by(Ref_Date,product) %>%
  summarise(change=sum(change)) %>%
  ungroup() %>%
  filter(Ref_Date>="Jan 2018") %>%
  mutate(product=case_when(
    product=="Food purchased from stores" ~ "Food\n(Groceries)",
    product=="All Other Items" ~ "Everything\nElse",
    product=="Traveller accommodation" ~ "Hotels",
    product=="Air transportation" ~ "Flights",
    product=="Water, fuel and electricity" ~ "Water\nFuel\nElectricity",
    product=="Mortgage interest cost" ~ "Mortgage\nInterest",
    product=="Household furnishings and equipment" ~ "Household\nFurnishings\nand Equip.",
    product=="Owned accommodation" ~ "Owned\nAccommodation",
    product=="Homeowners' replacement cost" ~ "Homeowners'\ndepreciation",
    TRUE ~ product
  )) %>%
  select(type=product,value=change) %>%
  rbind(
    data.frame(
      type=as.character(c(max(decomp_cpi$Ref_Date)-time_age,max(decomp_cpi$Ref_Date))),
      value=c(filter(inf_rates,Ref_Date==max(inf_rates$Ref_Date)-time_age)$YoY,
              filter(inf_rates,Ref_Date==max(inf_rates$Ref_Date))$YoY)
    )
  ) %>%
  arrange(-abs(value)) %>%
  mutate(id=row_number(),
         id=ifelse(type==as.character(max(inf_rates$Ref_Date)-time_age),0.5,row_number()),
         id=ifelse(type=="Everything\nElse",98,id),
         id=ifelse(type==as.character(max(inf_rates$Ref_Date)),99,id)) %>%
  arrange(id) %>%
  mutate(type=factor(type,levels=type),
         id=seq_along(value),
         sign=ifelse(value>0,"in","out"),
         sign=ifelse(id==n(),"net",sign),
         sign=ifelse(id==1,"first",sign),
         end=cumsum(value),
         end=ifelse(id==n(),0,end),
         start=ifelse(id>1,lag(end,1),0))
ggplot(plotdata %>% filter(sign!="first" & sign!="net") %>%
         mutate(id=id-1))+
  geom_segment(x=-0.5,xend=0.5,
               y=plotdata[1,]$value,yend=plotdata[1,]$value,size=2,
               color=col[2])+
  geom_segment(x=dim(plotdata)[1]-1-0.5,xend=dim(plotdata)[1]-1+0.5,
               y=plotdata[dim(plotdata)[1],]$value,
               yend=plotdata[dim(plotdata)[1],]$value,size=2,color=col[2])+
  geom_rect(aes(x=type,xmin = id - 0.4, xmax = id + 0.4,
                ymin=end,ymax=start,fill=sign),show.legend = F)+
  annotate('text',x=0,y=plotdata[1,]$value,
           vjust=-0.5,color=col[2],fontface='bold',
           label=paste0(plotdata[1,]$type,": ",
                        percent(plotdata[1,]$value,0.1)))+
  annotate('text',x=dim(plotdata)[1]-1,
           y=plotdata[dim(plotdata)[1],]$value,
           vjust=1.5,color=col[2],fontface='bold',
           label=paste0(plotdata[dim(plotdata)[1],]$type,": ",
                        percent(plotdata[dim(plotdata)[1],]$value,0.1)))+
  coord_cartesian(xlim=c(0,dim(plotdata)[1]-1))+
  geom_segment(data=plotdata %>% filter(sign!="net",sign!='first') %>%
                 mutate(id=id-1),
               aes(x=id+0.45,xend=id+0.45,y=start,yend=end),
               size=0.75,arrow=arrow(type="closed",length=unit(0.15,"cm")))+
  scale_y_continuous(breaks=pretty_breaks(n=6),label=percent_format(accuracy=0.1),
                     limit=c(min(plotdata[1:dim(plotdata)[1]-1,]$end)-0.001,
                             max(plotdata[1:dim(plotdata)[1]-1,]$end)+0.001))+
  labs(x="",y="Percentage Points",
       title="Contributions to Changes in Canada's Inflation Rate",
       subtitle="Source: own calculations from Statistics Canada data tables 18-10-0007 and 18-10-0004",
       caption="Graph by @trevortombe")
ggsave("Plots/ChangePriorMonth.png",width=10,height=4)

# Personalized Inflation Rates (commented out since doesn't run well on server)
# spend_list<-c("Food purchased from stores",
#               "Foor purchased from restaurants",
#               "Rented living quarters",
#               "Owned living quarters",
#               "Water, fuel and electricity for principal accommodation",
#               "Household operations",
#               "Household furnishings and equipment",
#               "Clothing and accessories",
#               "Private transportation",
#               "Public transportation",
#               "Health care",
#               "Personal care",
#               "Recreation",
#               "Education",
#               "Reading materials and other printed matter",
#               "Tobacco products, alcoholic beverages and cannabis for non-medical use")
# spending_income<-get_cansim("11100223") %>%
#   rename(Value=VALUE) %>%
#   mutate(Ref_Date=as.numeric(REF_DATE)) %>%
#   rename_all(list(~make.names(.)))
# spending_HHtype<-get_cansim("11100224") %>%
#   rename(Value=VALUE) %>%
#   mutate(Ref_Date=as.numeric(REF_DATE)) %>%
#   rename_all(list(~make.names(.)))
# spending_tenure<-get_cansim("11100225") %>%
#   rename(Value=VALUE) %>%
#   mutate(Ref_Date=as.numeric(REF_DATE)) %>%
#   rename_all(list(~make.names(.)))
# spending_rural<-get_cansim("11100226") %>%
#   rename(Value=VALUE) %>%
#   mutate(Ref_Date=as.numeric(REF_DATE)) %>%
#   rename_all(list(~make.names(.)))
# spending_age<-get_cansim("11100227") %>%
#   rename(Value=VALUE) %>%
#   mutate(Ref_Date=as.numeric(REF_DATE)) %>%
#   rename_all(list(~make.names(.)))
# personal_inf <- function(df){
#   df %>%
#     rename(product=Household.expenditures..summary.level.categories) %>%
#     rename(type=as.character(colnames(df)[26])) %>%
#     filter(Statistic=="Average expenditure per household",
#            product %in% spend_list,GEO=="Canada",
#            Ref_Date==max(Ref_Date)) %>%
#     mutate(product=case_when(
#       product=="Clothing and accessories" ~ "Clothing and footwear",
#       product=="Rented living quarters" ~ "Rented accommodation",
#       product=="Owned living quarters" ~ "Owned accommodation",
#       product=="Water, fuel and electricity for principal accommodation" ~ "Water, fuel and electricity",
#       product=="Reading materials and other printed matter" ~ "Reading material (excluding textbooks)",
#       product=="Tobacco products, alcoholic beverages and cannabis for non-medical use" ~ "Alcoholic beverages, tobacco products and recreational cannabis",
#       TRUE ~ as.character(product))) %>%
#     select(product,type,Value) %>%
#     left_join(
#       decomp_cpi %>%
#         filter(Ref_Date>=max(Ref_Date)-1/12 | Ref_Date==max(Ref_Date)-1) %>%
#         select(Ref_Date,product,change),
#       by='product'
#     ) %>%
#     group_by(Ref_Date,type) %>%
#     mutate(share=Value/sum(Value)) %>%
#     summarise(personal_inflation=sum(change*share)) %>%
#     mutate(personal_inflation=percent(personal_inflation,.1)) %>%
#     spread(Ref_Date,personal_inflation) %>% 
#     select(type,last_col(),contains(as.character(max(decomp_cpi$Ref_Date)-1/12)),everything())
# }
# table_data<-inf_rates %>%
#   filter(Ref_Date>=max(Ref_Date)-1/12 | Ref_Date==max(Ref_Date)-1) %>%
#   mutate(YoY=percent(YoY,.1),row=1) %>%
#   spread(Ref_Date,YoY) %>% 
#   select(last_col(),contains(as.character(max(decomp_cpi$Ref_Date)-1/12)),everything(),-row) %>%
#   rbind(personal_inf(spending_income)[c(3,4),2:4]) %>%
#   rbind(personal_inf(spending_rural)[c(3,8),2:4]) %>%
#   rbind(personal_inf(spending_HHtype)[c(2,5,6),2:4]) %>%
#   rbind(personal_inf(spending_tenure)[c(2,5),2:4]) %>%
#   rbind(personal_inf(spending_age)[c(6,4),2:4])
# table<-data.frame(Group=c("All Households","Highest Income Quintile","Lowest Income Quintile",
#                           "Large Urban Area (>1M)","Rural Area","Couple with Children",
#                           "Long-Parent Household","One-Person Household",
#                           "Homeowner","Renter","Head Age 30 and Under","Head Age 65 and Over")) %>%
#   cbind(table_data) %>% 
#   gt() %>%
#   tab_spanner(
#     label="Year-over-year change as of...",
#     columns=c(colnames(table_data))
#   ) %>%
#   cols_align(align = "center") %>%
#   tab_header(
#     title=md("**Household Type-Specific Inflation Rates in Canada**")
#   ) %>%
#   tab_source_note(gt::html(paste("<p style='font-size:12px'>Source: Own calculations from Statistics Canada data tables 18-10-0004 and 11-10-0223 through 11-10-0227. Based on 2019 SHS expenditure shares. Experimental estimates.",
#                                  "Table and calculations by @trevortombe. </p>"))) %>%
#   cols_width(starts_with('Group')~px(350),
#              everything() ~px(85)) %>%
#   cols_label(Group="Household Type") %>%
#   tab_options(data_row.padding = px(1))
# gtsave(table,'Plots/PersonalInflation.png')

# All items excluding whatever you want
plotdata<-decomp_cpi %>%
  filter(product %in% c("Energy","Rented accommodation","Owned accommodation")) %>%
  select(Ref_Date,cpi,effective_weight,contrib,product) %>%
  group_by(Ref_Date) %>%
  summarise(cpi=mean(cpi),
            weight=sum(effective_weight),
            contrib=sum(contrib),
            if_zero=cpi-contrib) %>%
  mutate(excluding=(cpi-contrib)/(1-weight)) %>%
  filter(Ref_Date>="Jan 1990")
ggplot(plotdata,aes(Ref_Date))+
  geom_line(aes(y=cpi,color="All-Items"),size=2)+
  geom_line(aes(y=cpi-contrib,color="Counterfactual with No Change in Energy / Shelter Prices"),size=2)+
  geom_text(data=filter(plotdata,Ref_Date==max(Ref_Date)),fontface='bold',
            aes(label=percent(cpi,0.1),y=cpi),hjust=0,nudge_x=0.5,color=col[1])+
  geom_text(data=filter(plotdata,Ref_Date==max(Ref_Date)),fontface='bold',
            aes(label=percent(cpi-contrib,0.1),y=cpi-contrib),hjust=0,
            nudge_x=0.5,color=col[2])+
  scale_y_continuous(label=percent)+
  scale_x_continuous(breaks=pretty_breaks(6),limit=c(NA,max(plotdata$Ref_Date)+1))+
  geom_hline(yintercept=0,size=1)+
  labs(title="Energy and Shelter's Effect on Inflation in Canada",
       x="",y="Percent",
       subtitle="Source: own calculations from Statistics Canada data tables 18-10-0007 and 18-10-0004",
       caption="Graph by @trevortombe")
ggsave("Plots/EnergyShelterEffect.png",width=8,height=4)

# Seasonally Adjusted CPI Levels vs Bank of Canada Target
plotdata<-data2 %>%
  group_by(Products.and.product.groups,GEO) %>%
  mutate(YoY=Value/lag(Value,12)-1) %>%
  filter(Products.and.product.groups=="All-items",
         Ref_Date>="Jan 2000",GEO=="Canada") %>% 
  mutate(month=month(Ref_Date)) %>%
  filter(Ref_Date>="Jan 2019") %>%
  mutate(row=1:n()) %>%
  select(Ref_Date,Value,row) %>%
  mutate(avg=(Value/Value[1])^(12/row)-1,
         months_since_covid=12*(Ref_Date-as.yearmon("Feb 2020")),
         target=1.02^(months_since_covid/12)*weighted.mean(Value,Ref_Date=="Feb 2020"),
         upper=1.03^(months_since_covid/12)*weighted.mean(Value,Ref_Date=="Feb 2020"),
         lower=1.01^(months_since_covid/12)*weighted.mean(Value,Ref_Date=="Feb 2020"))
ggplot(plotdata,aes(Ref_Date,Value))+
  geom_ribbon(data=filter(plotdata,Ref_Date>="Feb 2020"),
              aes(ymin=lower,ymax=upper),fill='gray',alpha=0.5)+
  geom_line(size=2,color=col[2])+
  geom_line(data=filter(plotdata,Ref_Date>="Feb 2020"),aes(y=target),size=2,color=col[3],
            linetype='dotted')+
  annotate('text',x=2023,y=138,
           label="Bank of Canada's\nControl Range",color='gray80',fontface='bold')+
  annotate('text',x=2020.5,y=142,hjust=1,label="2% Inflation Target",color=col[3],fontface='bold')+
  annotate('text',x=2021,y=137,hjust=0,label="Price Level Data",color=col[2],fontface='bold')+
  annotate('text',x=2022,y=157.5,hjust=1,color=col[1],
           label=paste("Price levels are now",
                       percent(filter(plotdata,Ref_Date==max(Ref_Date))$Value/
                                 filter(plotdata,Ref_Date==max(Ref_Date))$target-1,.1),
                       "\nhigher than if BoC target met"))+
  geom_segment(x=2022.1,xend=2023.2,y=157.5,yend=157.5,color=col[1],
               arrow=arrow(length=unit(1.5,'mm')))+
  labs(title="Path of the Consumer Price Index in Canada",x="",y="CPI Index (2002 = 100)",
       subtitle="Source: own calculations from Statistics Canada data table 18-10-0006",
       caption='Graph by @trevortombe')
ggsave('Plots/BoCPath.png',width=8,height=4)

# Change in Price Levels by Major Product Category
plotdata<-data2 %>%
  filter(!(Products.and.product.groups %in% c("All-items","All-items excluding food and energy","All-items excluding food")) & 
           Ref_Date>="Feb 2020") %>%
  mutate(Products.and.product.groups=as.character(Products.and.product.groups)) %>%
  group_by(Products.and.product.groups) %>%
  mutate(change=Value/Value[1]-1) %>%
  ungroup() %>%
  mutate(Products.and.product.groups=ifelse(Products.and.product.groups=="Alcoholic beverages, tobacco products and recreational cannabis",
                                            "Alcohol and Tobacco",Products.and.product.groups),
         Products.and.product.groups=ifelse(Products.and.product.groups=="Household operations, furnishings and equipment",
                                            "Household, furniture/equip.",Products.and.product.groups),
         Products.and.product.groups=ifelse(Products.and.product.groups=="Recreation, education and reading",
                                            "Recreation, education",Products.and.product.groups))
ggplot(plotdata,aes(Ref_Date,change,group=Products.and.product.groups,
                    color=Products.and.product.groups))+
  geom_hline(yintercept=0,size=1)+
  geom_text_repel(data=filter(plotdata,Ref_Date==max(Ref_Date)),
                  aes(label=Products.and.product.groups),hjust=0,
                  direction='y',nudge_x=0.05,size=3,
                  show.legend = F,segment.alpha=0)+
  geom_line(size=2,show.legend=F)+
  scale_y_continuous(label=percent)+
  scale_x_yearmon(limit=c(NA,year(max(plotdata$Ref_Date))+1.5),
                  breaks=pretty_breaks(6),format="%b\n%Y")+
  labs(x="",y="Per Cent Change",title="Price Changes in Canada, by Broad Product Category",
       subtitle="Displays the change in prices since February 2020, by major CPI item,
Source: Own calculations from Statistics Canada data table 18-10-0006.",
       caption="Graph by @trevortombe")
ggsave("Plots/ByProduct.png",width=8,height=4)

# Bank of Canada Preferred Measures (Median and Trim), Monthly
CPImedian<-indexes_sa %>%
  left_join(weights,by=c("date","product")) %>%
  filter(product!="Consumer Price Index (CPI), all-items excluding the effect of indirect taxes") %>%
  group_by(product) %>%
  mutate(change=index/lag(index,1)) %>%
  arrange(date,change) %>%
  group_by(date) %>%
  mutate(cum_w=cumsum(w)) %>%
  filter(cum_w>=50 & lag(cum_w,1)<50) # this is the StatCan approach, not exactly median
CPItrim<-indexes_sa %>%
  left_join(weights,by=c("date","product")) %>%
  filter(product!="Consumer Price Index (CPI), all-items excluding the effect of indirect taxes") %>%
  group_by(product) %>%
  mutate(change=index/lag(index,1)) %>%
  arrange(date,change) %>%
  group_by(date) %>%
  mutate(cum_w=cumsum(w),
         trim_w=ifelse(cum_w<20 | cum_w>80,0,w),
         trim_w=ifelse(cum_w>20 & lag(cum_w<20),cum_w-20,trim_w),
         trim_w=ifelse(cum_w>80 & lag(cum_w<80),80-lag(cum_w,1),trim_w)) %>% # the StatCan approach
  filter(trim_w!=0) %>%
  summarise(change=weighted.mean(change,trim_w))

# Construct CPI trim core services ex shelter
CPItrim_CoreServeExShelter<-indexes_sa %>%
  left_join(weights,by=c("date","product")) %>%
  filter(product!="Consumer Price Index (CPI), all-items excluding the effect of indirect taxes") %>%
  group_by(date) %>%
  mutate(core_serve_ex_shelter=ifelse(
    row_number() %in% c(8,20,21,24,28,32,37,38,39,40,
                        42,44,45,48,49,50,51,53),1,0)) %>%
  group_by(product) %>%
  mutate(change=index/lag(index,1)) %>%
  group_by(date) %>%
  arrange(date,change) %>%
  group_by(date) %>%
  mutate(cum_w=cumsum(w),
         trim_w=ifelse(cum_w<20 | cum_w>80,0,w),
         trim_w=ifelse(cum_w>20 & lag(cum_w<20),cum_w-20,trim_w),
         trim_w=ifelse(cum_w>80 & lag(cum_w<80),80-lag(cum_w,1),trim_w)) %>% # the StatCan approach
  filter(core_serve_ex_shelter==1) %>%
  filter(trim_w!=0,core_serve_ex_shelter==1) %>%
  summarise(change=weighted.mean(change,trim_w),
            w_tot=sum(w)) %>%
  filter(date>="Jan 1990") %>%
  mutate(index=cumprod(change))
plotdata<-CPImedian %>%
  select(date,CPImedian=change) %>%
  left_join(CPItrim %>% select(date,CPItrim=change),by='date') %>%
  filter(date>="Jan 2009") %>%
  mutate(CPImedian=CPImedian^12-1,
         CPItrim=CPItrim^12-1) %>%
  select(date,CPImedian,CPItrim) %>%
  gather(type,rate,-date)
ggplot(plotdata,aes(date,rate,group=type,color=type))+
  annotate('rect',xmin=-Inf,xmax=Inf,ymin=0.01,ymax=0.03,alpha=0.25,fill='dodgerblue')+
  annotate('text',x=min(plotdata$date),y=0.035,label="Target Range",color='dodgerblue',alpha=0.6,size=2.5)+
  geom_line(size=1.5)+
  scale_y_continuous(label=percent)+
  scale_color_manual(label=c("CPI-Median","CPI-Trim"),values=col[1:2])+
  scale_x_continuous(breaks=pretty_breaks(5))+
  labs(title="Monthly Change in the Bank of Canada's Core Inflation Measures",
       subtitle="Reflects the month-over-month annualized change in CPI-median and CPI-trim",
       caption='Source: own calculations from Statistics Canada data for 55 products. Graph by @trevortombe',
       x="",y="Percent")
ggsave("Plots/MedianTrim.png",width=8,height=4)

# Add own calculation of CPI Common
# three-month average
allitems<-indexes_raw %>%
  filter(grepl("Consumer Price Index",product)) %>%
  mutate(index=as.numeric(index),
         allitems=index/lag(index,12)-1) %>%
  select(date,allitems) %>%
  filter(!is.na(allitems)) %>%
  select(-date) %>%
  ts(frequency=12,start=c(1990,1))
change<-indexes_raw %>%
  filter(!grepl("Consumer Price Index",product)) %>%
  group_by(product) %>%
  mutate(index=as.numeric(index),
         YoY=index/lag(index,12)-1) %>%
  filter(!is.na(YoY)) %>%
  select(date,product,YoY) %>%
  spread(product,YoY) %>%
  select(-date) %>%
  ts(frequency=12,start=c(1990,1))
CPIcommon_index<-indexes_raw %>%
  filter(grepl("Consumer Price Index",product)) %>%
  select(-product) %>%
  mutate(index=as.numeric(index)) %>%
  filter(year(date)==1989)
temp<-data.frame(
  date=as.yearmon("Jan 1990")+seq(0,length(allitems)-1)/12,
  CPIcommon=predict(lm(allitems~predict(prcomp(change,scale=T,center=T),newdata=change)[,1]))
)
results<-CPIcommon_index
for (y in seq(1990,max(year(temp$date)))){
  temp2<-temp %>%
    filter(year(date)==y) %>%
    left_join(filter(results,year(date)==y-1) %>% mutate(date=date+1),by='date') %>%
    mutate(index=(1+CPIcommon)*index) %>%
    select(date,index)
  results<-results %>%
    bind_rows(temp2)
}
CPIcommon_index_sa<-results %>%
  mutate(group='group') %>%
  rename(Value=index,
         Ref_Date=date) %>%
  getseas('group') %>%
  mutate(change=Value/lag(Value,1))
plotdata<-CPImedian %>%
  select(date,CPImedian=change) %>%
  left_join(CPItrim %>% select(date,CPItrim=change),by='date') %>%
  left_join(CPIcommon_index_sa %>% select(date=Ref_Date,CPIcommon=change),by='date') %>%
  filter(date>="Jan 2009") %>%
  select(date,CPImedian,CPItrim,CPIcommon) %>%
  gather(type,rate,-date) %>%
  group_by(type) %>%
  mutate(rate6=(rate*lag(rate,1)*lag(rate,2)*lag(rate,3)*lag(rate,1)*lag(rate,5))^2-1,
         rate=(rate*lag(rate,1)*lag(rate,2))^4-1)
ggplot(plotdata %>% filter(date>="Jan 2018"),aes(date,rate,group=type,color=type))+
  annotate('rect',xmin=-Inf,xmax=Inf,ymin=0.01,ymax=0.03,alpha=0.25,fill='dodgerblue')+
  annotate('text',x=-Inf,y=0.035,hjust=0,
           label="Target Range",color='dodgerblue',alpha=0.6,size=2.5)+
  geom_line(size=2)+
  scale_y_continuous(label=percent)+
  scale_color_manual(label=c("CPI-Common *","CPI-Median","CPI-Trim"),values=col[1:3])+
  scale_x_continuous(breaks=pretty_breaks(3))+
  labs(title="Bank of Canada's Core Inflation Measures, 3-Month (Annualized) Change",
       subtitle="Reflects the 3-month average annualized change in CPI-median, CPI-trim, and CPI-common",
       caption='* Based on the implied seasonally adjusted price index that corresponds to the year-over-year CPI-common series.
Source: Own calculations from Statistics Canada data for 55 products. Graph by @trevortombe',
       x="",y="Percent")
ggsave("Plots/MedianTrimCommon_3mo.png",width=8,height=4)

# Bank of Canada Preferred Measures (Median and Trim + supercore), 3-month moving average
plotdata<-CPImedian %>%
  select(date,CPImedian=change) %>%
  left_join(CPItrim %>% select(date,CPItrim=change),by='date') %>%
  left_join(CPItrim_CoreServeExShelter %>% 
              select(date,CPItrim_CoreServeExShelter=change),by='date') %>%
  filter(date>="Jan 2009") %>%
  select(date,CPImedian,CPItrim,CPItrim_CoreServeExShelter) %>%
  gather(type,rate,-date) %>%
  group_by(type) %>%
  mutate(rate=(rate*lag(rate,1)*lag(rate,2))^4-1)
ggplot(plotdata %>% filter(date>="Jan 2017"),aes(date,rate,group=type,color=type))+
  annotate('rect',xmin=-Inf,xmax=Inf,ymin=0.01,ymax=0.03,alpha=0.25,fill='dodgerblue')+
  annotate('text',x=-Inf,y=0.035,hjust=0,
           label="Target Range",color='dodgerblue',alpha=0.6,size=2.5)+
  geom_line(size=2)+
  geom_hline(yintercept=0,size=1)+
  scale_y_continuous(label=percent)+
  scale_color_manual(label=c("CPI-Median","CPI-Trim",
                             "CPI-Trim Services, Ex Shelter"),
                     values=col[1:3])+
  scale_x_continuous(breaks=pretty_breaks(6))+
  labs(title="3-Month Average Annual Change in the Bank of Canada's Core Inflation Measures",
       subtitle="Reflects the 3-month average annualized change in selected Bank of Canada core measures of inflation",
       caption='Source: own calculations from Statistics Canada data for 55 products. Graph by @trevortombe',
       x="",y="Percent")
ggsave("Plots/MedianTrim_3mo.png",width=8,height=4)

# Treemap of annual price changes
plotdata<-decomp_cpi %>% 
  filter(product %in% BoC_list$product,
         Ref_Date==max(Ref_Date)) %>%
  left_join(BoC_list %>% select(product,subgroup),by='product') %>%
  ungroup() %>%
  filter(!is.na(contrib)) %>%
  arrange(change) %>%
  mutate(label=paste0(product," (",percent(change,0.1),")"),
         shading=ifelse(change>0.1,0.1,change),
         shading=ifelse(change<0,0,shading)) %>%
  select(product,label,effective_weight,shading,change,subgroup)
ggplot(plotdata,aes(fill=shading,area=effective_weight,subgroup=subgroup)) + 
  geom_treemap(color="white",size=2)+
  geom_treemap_text(aes(label=label),place="center",
                    color=ifelse(plotdata$change<0.04,'gray30','white'),
                    min.size = 0,reflow=T,
                    padding.x = unit(1.5, "mm"),
                    padding.y = unit(1.5, "mm"))+
  scale_fill_gradient2(low='#abdda4',high='#d7191c',limit=c(0,NA),
                       mid='#abdda4',midpoint=0.02,
                       breaks=seq(0,0.1,0.02),
                       labels=c("<0%","2%","4%","6%","8%",">10%"))+
  geom_treemap_subgroup_text(place = "centre", grow = TRUE,
                             alpha = 0.2, colour = "black",
                             fontface = "italic",
                             padding.x = unit(5, "mm"),
                             padding.y = unit(5, "mm")) +
  geom_treemap_subgroup_border(size=8,color="white")+
  theme(legend.position = 'none')+
  labs(title = paste0("Annual Price Changes in Canada (",max(data$Ref_Date),")"),
       caption='Source: own calculations from Statistics Canada data table 18-10-0004-01. Graph by @trevortombe',
       subtitle="Size of each square reflects the amount of average consumer spending.")
ggsave('Plots/TreeMap_Annual.png',width=6,height=5,dpi=300)

# Treemap of three-month average price changes
plotdata<-indexes_sa %>%
  left_join(weights,by=c("date","product")) %>% 
  filter(!grepl("Consumer Price Index",product)) %>%
  group_by(product) %>%
  mutate(change=(index/lag(index,3))^4-1) %>%
  filter(date==max(date)) %>%
  left_join(BoC_list %>% select(product,subgroup),by='product') %>%
  ungroup() %>%
  mutate(label=paste0(product," (",percent(change,0.1),")"),
         shading=ifelse(change>0.1,0.1,change),
         shading=ifelse(change<0,0,shading)) %>%
  select(product,label,w,shading,change,subgroup)
ggplot(plotdata,aes(fill=shading,area=w,subgroup=subgroup)) + 
  geom_treemap(color="white",size=2)+
  geom_treemap_text(aes(label=label),place="center",
                    color=ifelse(plotdata$change<0.04,'gray30','white'),
                    min.size = 0,reflow=T,
                    padding.x = unit(1.5, "mm"),
                    padding.y = unit(1.5, "mm"))+
  scale_fill_gradient2(low='#abdda4',high='#d7191c',limit=c(0,NA),
                       mid='#abdda4',midpoint=0.02,
                       breaks=seq(0,0.1,0.02),
                       labels=c("<0%","2%","4%","6%","8%",">10%"))+
  geom_treemap_subgroup_text(place = "centre", grow = TRUE,
                             alpha = 0.2, colour = "black",
                             fontface = "italic",
                             padding.x = unit(5, "mm"),
                             padding.y = unit(5, "mm")) +
  geom_treemap_subgroup_border(size=8,color="white")+
  theme(legend.position = 'none')+
  labs(title = paste0("Annualized 3-Month Average Price Changes in Canada (",max(data$Ref_Date),")"),
       caption='Source: Own calculations from the Bank of Canada\'s Core Inflation Measures. Seasonally adjusted. Graph by @trevortombe',
       subtitle="Size of each square reflects the amount of average consumer spending.")
ggsave('Plots/TreeMap_3moMA.png',width=6.5,height=5,dpi=300)

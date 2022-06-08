# Load required packages, fetch the latest data
source("R/Setup.R")

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
  geom_line(size=2)+
  annotate('text',x=2020.5,y=0.0325,label="Range of Core\nBoC Measures",
           color=col[3],fontface='bold',alpha=0.6)+
  mytheme+
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
  mytheme+
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
  left_join(
    product_list,by="product"
  ) %>%
  filter(keep_product==1) %>%
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
  mytheme+
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
  mytheme+
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
decomp_cpi<-data %>%
  filter(GEO=="Canada") %>%
  select(Ref_Date,product=Products.and.product.groups,Value) %>%
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
  mutate(effective_weight=(relimp_old/(Value/I_atlink)+(1-1/(Value/I_atlink))*relimp_new), # an intuitive way? same as statcan approach
         change=Value/lag(Value,12)-1,
         contrib=(1+change)*effective_weight-relimp_old, # main estimate
         value_if_2prc=lag(Value,12)*1.02,
         weight_if_2prc=(relimp_old/(value_if_2prc/I_atlink)+(1-1/(value_if_2prc/I_atlink))*relimp_new),
         contrib_if_2prc=1.02*weight_if_2prc-relimp_old,
         excluding_item=(cpi-contrib)/(1-effective_weight))
plotdata<-decomp_cpi %>%
  filter(product %in% c("Food purchased from stores",
                        "Energy",
                        "Household furnishings and equipment",
                        "Rent",
                        "Purchase and leasing of passenger vehicles",
                        "Purchase of recreational vehicles and outboard motors",
                        "Homeowners' replacement cost")) %>%
  group_by(Ref_Date) %>%
  mutate(total=sum(contrib),
         `All Other Items`=cpi-total) %>%
  select(Ref_Date,product,contrib,`All Other Items`,cpi) %>%
  spread(product,contrib) %>%
  gather(product,contrib,-Ref_Date,-cpi) %>%
  mutate(product=ifelse(product %in% c("Purchase of recreational vehicles and outboard motors",
                                       "Purchase and leasing of passenger vehicles"),
                        "New Vehicles",product)) %>%
  group_by(Ref_Date,product) %>%
  summarise(contrib=sum(contrib),
            cpi=mean(cpi)) %>%
  ungroup() %>%
  filter(Ref_Date>="Jan 2018") %>%
  mutate(product=case_when(
    product=="Food purchased from stores" ~ "Food (Groceries)",
    product=="Homeowners' replacement cost" ~ "Homeowners' depreciation",
    TRUE ~ product
  )) %>%
  filter(!is.na(cpi))
dev.off()
p<-ggplot(plotdata,aes(Ref_Date,contrib,group=product,fill=product))+
  geom_col(position='stack')+
  geom_line(aes(y=cpi),size=2)+
  scale_y_continuous(label=percent,breaks=pretty_breaks(5))+
  scale_x_continuous(breaks=pretty_breaks(6))+
  mytheme+
  theme(plot.margin = unit(c(0.25,10,0.25,0.25),"lines"),
        legend.position = 'none',
        legend.key.width = unit(2,"cm"),
        legend.title=element_blank(),
        panel.grid.major.y = element_line(color='gray'))+
  geom_label(data=plotdata %>% filter(Ref_Date==max(Ref_Date)) %>%
               arrange(desc(product)) %>%
               mutate(location=ifelse(row_number()==1,contrib/2,NA),
                      location=ifelse(row_number()>1,lag(cumsum(contrib),1)+contrib/2,location),
                      labelname=gsub(" ","\n  ",product)),
             aes(label=paste0("  ",product),y=location,color=product),
             hjust=0,nudge_x=1/12,fontface="bold",size=3,fill='white',label.size=0)+
  annotate('text',x=2021.5,hjust=1,y=0.045,label="All-Items CPI",size=3)+
  labs(x="",y="Year-over-Year Change",
       title="Contribution of Selected Products to Canada's Inflation Rate",
       subtitle="Source: own calculations from Statistics Canada data tables 18-10-0007 and 18-10-0004",
       caption="Graph by @trevortombe")
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
  filter(product %in% c("Gasoline",
                        "Purchase and leasing of passenger vehicles",
                        "Purchase of recreational vehicles and outboard motors",
                        "Homeowners' replacement cost",
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
                        "New\nVehicles",product)) %>%
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
  mytheme+
  # scale_fill_manual(name="",values=c(col[3],col[2],col[2],col[1]))+
  scale_y_continuous(breaks=pretty_breaks(n=6),label=percent)+
  labs(x="",y="Percentage Points",
       title="Contributions to Changes in Canada's Inflation Rate",
       subtitle="Source: own calculations from Statistics Canada data tables 18-10-0007 and 18-10-0004",
       caption="Graph by @trevortombe")
ggsave("Plots/ChangeFeb2020.png",width=10,height=4)


# Load required packages, fetch the latest data
source("R/Setup.R")

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

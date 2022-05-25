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

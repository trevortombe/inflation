###########################
# Load the Main Data Sets #
###########################

# Load and Format Main CPI Data
data<-get_cansim('18100004') %>%
  rename(Value=VALUE) %>%
  mutate(Ref_Date=as.yearmon(REF_DATE,"%Y-%m")) %>%
  rename_all(list(~make.names(.)))

# Seasonally Adjusted CPI Data
data2<-get_cansim('18100006') %>%
  rename(Value=VALUE) %>%
  mutate(Ref_Date=as.yearmon(REF_DATE,"%Y-%m")) %>%
  rename_all(list(~make.names(.)))

# Load and Format Main Bank of Canada Data
BoCdata<-get_cansim('18100256') %>%
  rename(Value=VALUE) %>%
  mutate(Ref_Date=as.yearmon(REF_DATE,"%Y-%m")) %>%
  rename_all(list(~make.names(.)))

# Load and Format Bond Yield Data
yields<-get_cansim('10100139') %>%
  rename(Value=VALUE) %>%
  mutate(Ref_Date=as.Date(REF_DATE)) %>%
  rename_all(list(~make.names(.)))

# Product List (Manually Created)
product_list<-read.csv("Data/cpi_products.csv",stringsAsFactors = F)
BoC_list<-read.csv("Data/bank_cpi_products.csv",stringsAsFactors = F)

# Headline Inflation Rate (Used in the UpdatePlots.R)
inf_rates<-data %>%
  filter(Products.and.product.groups=="All-items",
         GEO=="Canada") %>%
  mutate(YoY=Value/lag(Value,12)-1) %>%
  select(Ref_Date,YoY) %>%
  drop_na()

# Basket Weights
weights<-get_cansim('18100007') %>%
  rename(Value=VALUE) %>%
  mutate(Ref_Date=as.numeric(REF_DATE)) %>%
  rename_all(list(~make.names(.)))

# Convert to monthly basket weights
weights_monthly<-weights %>%
  filter(GEO=="Canada",
         Geographic.distribution.of.weight=="Distribution to selected geographies",
         Price.period.of.weight=="Weight at basket link month prices") %>%
  select(basket=Ref_Date,product=Products.and.product.groups,w=Value)
link_months<-data.frame(
  Ref_Date=seq(as.yearmon("1978-01"),max(data$Ref_Date),1/12)
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

# Decompose using appropriate basket weights
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
  mutate(contrib_old=(I_atlink/lag(Value,12)-1)*relimp_old, #statcan, https://www150.statcan.gc.ca/n1/pub/62-553-x/2019001/chap-8-eng.htm
         contrib_new=(Value/I_atlink-1)*(w/100)*(all_atlink/lag(all,12)), #statcan, https://www150.statcan.gc.ca/n1/pub/62-553-x/2019001/chap-8-eng.htm
         contrib_cross=contrib_new+contrib_old, 
         contrib_nocross=(Value/lag(Value,12)-1)*relimp_old, 
         contrib_check=ifelse(basket!=lag(basket,12),contrib_cross,contrib_nocross), # verify statcan same as your main approach
         effective_weight=(relimp_old/(Value/I_atlink)+(1-1/(Value/I_atlink))*relimp_new), # an intuitive way? same as statcan approach
         change=Value/lag(Value,12)-1,
         MoM=Value/lag(Value,1)-1,
         contrib=(1+change)*effective_weight-relimp_old, # main estimate
         value_if_2prc=lag(Value,12)*1.02,
         weight_if_2prc=(relimp_old/(value_if_2prc/I_atlink)+(1-1/(value_if_2prc/I_atlink))*relimp_new),
         contrib_if_2prc=1.02*weight_if_2prc-relimp_old,
         excluding_item=(cpi-contrib)/(1-effective_weight))

# The Bank of Canada Preferred Core Measures Product-Level Data
data_sa<-read_excel("Data/Core_Measures_Inputs_External_E&F.xlsx",
                    sheet="Indexes_SA")
colnames(data_sa)[1]<-"product"
colnames(data_sa)[2]<-"product_fr"
indexes_sa<-data_sa %>%
  select(-product_fr) %>%
  filter(!is.na(product),!grepl("Source: Statistics Canada",product)) %>%
  gather(date,index,-product) %>%
  mutate(date=gsub("I_SA_","",date),
         date=as.yearmon(date,"%Y%m"))
data_w<-read_excel("Data/Core_Measures_Inputs_External_E&F.xlsx",
                   sheet="Weights")
colnames(data_w)[1]<-"product"
colnames(data_w)[2]<-"product_fr"
weights<-data_w %>%
  select(-product_fr) %>%
  filter(!is.na(product),!grepl("Source: Statistics Canada",product)) %>%
  gather(date,w,-product) %>%
  mutate(date=gsub("wght_","",date),
         date=as.yearmon(date,"%Y%m"))
data_raw<-read_excel("Data/Core_Measures_Inputs_External_E&F.xlsx",
                     sheet="Indexes_Raw")
colnames(data_raw)[1]<-"product"
colnames(data_raw)[2]<-"product_fr"
indexes_raw<-data_raw %>%
  select(-product_fr) %>%
  filter(!is.na(product),!grepl("Source: Statistics Canada",product)) %>%
  gather(date,index,-product) %>%
  mutate(date=gsub("I_","",date),
         date=as.yearmon(date,"%Y%m"))

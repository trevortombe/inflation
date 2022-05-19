# wipes previous workspace
rm(list=ls(all=TRUE)) 

# Install Packages and Load
packages<-c("scales","zoo","dplyr","gt","testit","data.table",
            "ggplot2","ggthemes","tidyr")
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,type="binary") # installing from source takes significantly longer on Github Actions
  sapply(pkg, require, character.only = TRUE)
}
check.packages(packages)

# Your preferred color scheme (https://www.color-hex.com/color-palette/33490)
col<-c("#CC2529","#396ab1","#3E9651","#DA7C30","#535154","#6B4C9A","#922428","#948B3D")
scale_colour_discrete <- function(...) {
  scale_colour_manual(..., values = col)
}
scale_fill_discrete <- function(...) {
  scale_fill_manual(..., values = col)
}

# List of Provinces
provsort<-c("BC","AB","SK","MB","ON","QC","NB","NS","PE","NL")
provinces<-c("Canada","Newfoundland and Labrador","Prince Edward Island","Nova Scotia",
             "New Brunswick","Quebec","Ontario","Manitoba","Saskatchewan",
             "Alberta","British Columbia","Yukon","Northwest Territories","Nunavut")
provinces2<-c("CAN","NL","PE","NS",
              "NB","QC","ON","MB","SK",
              "AB","BC","YT","NT","NU")
provnames<-data.frame(GEO=provinces,short=provinces2)
provnames$short <- factor(provnames$short, levels = c("CAN","BC","AB","SK","MB","ON","QC","NB","NS","PE","NL","YT","NT","NU")) # Lock in factor level order

# Define your plot theme
mytheme<-theme_minimal()+theme(
  axis.title.y = element_text(size=9),
  axis.title.x = element_text(size=9),
  legend.position = "top",
  legend.text=element_text(size=10,margin = margin(r = 10, unit = "pt")),
  legend.margin=margin(c(0,0,-0.25,0),unit="cm"),
  legend.title=element_blank(),
  strip.background = element_rect(fill="gray90",color="transparent"),
  panel.background = element_rect(fill = "white",colour = "white"),
  plot.background = element_rect(fill = "white",colour = "white"),
  plot.caption = element_text(size = 6, color = "gray40",hjust=1),
  plot.title = element_text(face = "bold",size=14),
  plot.subtitle = element_text(size = 8, color = "gray40"),
  panel.grid.minor = element_blank(),
  panel.grid.major.x = element_blank()
)

# Function to Fetch the StatCan Table
getTABLE<-function(x) {
  url<-paste0("https://www150.statcan.gc.ca/n1/tbl/csv/",x,"-eng.zip")
  temp<-tempfile()
  download.file(url,temp)
  if (has_warning(unzip(temp,paste0(x,".csv")))) { # Avoids html landing page
    download.file(url,temp)
  }
  unzip(temp,paste0(x,".csv"))
  rawdata<-fread(paste0(x,".csv"),encoding="UTF-8",stringsAsFactors=FALSE)
  colnames(rawdata)[1]<-"Ref_Date"
  data<-rawdata %>%
    dplyr::rename(Value=VALUE) %>%
    select(-UOM_ID,-SCALAR_ID) %>%
    dplyr::rename_all(list(~make.names(.))) # this replaces the spaces with dots in the column names
  if (class(data$Ref_Date)=="character" & !grepl("/",data[1]$Ref_Date)){
    data<-data %>%
      mutate(Ref_Date=as.yearmon(Ref_Date))
  }
  if ("GEO" %in% colnames(data)){
    data <- data %>%
      left_join(provnames,by="GEO")
  }
  sourcetable<-gsub("(\\d{2})(\\d{2})(\\d{4})$","\\1-\\2-\\3",x)
  comment(data)<-paste("Statistics Canada data table",sourcetable)
  return(data)
}

###########################
# Load the Main Data Sets #
###########################

# Main CPI Data
data<-getTABLE("18100004")

# Basket Weights
weights<-getTABLE("18100007")

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
    Ref_Date>="Jun 2021" ~ 2020
  )) %>%
  group_by(basket) %>%
  mutate(link_month=min(Ref_Date)) %>% ungroup()

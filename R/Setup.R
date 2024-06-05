# wipes previous workspace
rm(list=ls(all=TRUE)) 

# Install Packages and Load
packages<-c("broom","vars","kableExtra","tidyverse",
            "scales","zoo","dplyr","gt","testit","data.table","ggseas",
            "webshot","httr","readxl","ggrepel","treemapify","lubridate",
            "ggplot2","ggthemes","tidyr","grid","fredr","cansim")
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,type="binary") # installing from source takes significantly longer on Github Actions
  sapply(pkg, require, character.only = TRUE)
}
check.packages(packages)
# install.packages("webshot2")
# require(webshot2)
fredr_set_key(Sys.getenv("FRED_KEY"))

# Required to save gtables
# webshot:::find_phantom()
# if (is.null(webshot:::find_phantom())){
#   webshot::install_phantomjs()
# }

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
provnames<-data.frame(GEO=provinces,short=provinces2) %>%
  mutate(short=factor(short, levels = c("CAN","BC","AB","SK","MB","ON","QC","NB","NS","PE","NL","YT","NT","NU")))

# Set figure theme
theme_set(theme_minimal())
theme_update(panel.grid.minor = element_blank(),
             panel.grid.major.x = element_blank(),
             panel.background = element_rect(fill = "white",colour = "white"),
             plot.caption = element_text(size = 6, color = "gray40",hjust=1),
             plot.title = element_text(face = "bold",size=14),
             plot.subtitle = element_text(size = 8, color = "gray40"),
             plot.background = element_rect(fill = "white",colour = "white"),
             axis.title.y = element_text(size=9),
             axis.title.x = element_text(size=9),
             legend.position = "top",
             legend.text=element_text(size=10,margin = margin(r = 10, unit = "pt")),
             legend.margin=margin(c(0,0,-0.25,0),unit="cm"),
             legend.title=element_blank(),
             strip.background = element_rect(fill="gray90",color="transparent"))

# Construct own within-group seasonal adjustment with trend
getseas<-function(df,g){
  df<-df %>%
    rename(group_var=g)
  if (("value" %in% colnames(df)) | ("value_orig" %in% colnames(df))){ # need this in case you've previously used update_data()
    df<-df %>% select(-value,-value_orig)
  }
  p<-ggsdc(df,aes(Ref_Date,Value,group=group_var,color=group_var),
           method="seas")+geom_line()
  temp<-p$data %>%
    filter(component %in% c("irregular","trend")) %>%
    group_by(x,group_var) %>%
    summarise(Value=sum(y)) %>%
    group_by(group_var) %>%
    rename(Ref_Date=x) %>%
    rename(setNames("group_var", g)) %>%
    ungroup()
  return(temp)
}

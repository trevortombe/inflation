# wipes previous workspace
rm(list=ls(all=TRUE)) 

# Install Packages and Load
packages<-c("scales","zoo","dplyr","gt","testit","data.table",
            "webshot","httr","readxl","ggrepel",
            "ggplot2","ggthemes","tidyr","grid","fredr","cansim")
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,type="binary") # installing from source takes significantly longer on Github Actions
  sapply(pkg, require, character.only = TRUE)
}
check.packages(packages)
fredr_set_key(Sys.getenv("FRED_KEY"))

# Required to save gtables
webshot:::find_phantom()
if (is.null(webshot:::find_phantom())){
  webshot::install_phantomjs()
}

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

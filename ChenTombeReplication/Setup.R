rm(list=ls(all=TRUE)) # wipes previous workspace

# Load required packages
packages<-c("broom","vars","kableExtra","ggthemes","tidyverse","cansim",
            "scales","grid","fredr","lubridate","data.table")
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,type="binary") # installing from source takes significantly longer on Github Actions
  sapply(pkg, require, character.only = TRUE)
}
check.packages(packages)

# FRED Secret Key
#fredr_set_key(Sys.getenv("FRED_KEY"))

# Set figure theme
theme_set(theme_minimal())
theme_update(panel.grid.minor = element_blank(),
             panel.grid.major.x = element_blank(),
             plot.subtitle = element_text(size = 9, color = "gray40"),
             panel.background = element_rect(fill = "white",colour = "white"),
             plot.background = element_rect(fill = "white",colour = "white"),
             axis.title.y = element_text(size=9),
             axis.title.x = element_text(size=9),
             legend.title=element_blank(),
             strip.background = element_rect(fill="gray90",color="transparent"))

# Color scheme used in paper
col<-c("#CC2529","#396ab1","#3E9651","#DA7C30","#535154","#6B4C9A","#922428","#948B3D")
scale_colour_discrete <- function(...) {
  scale_colour_manual(..., values = col)
}
scale_fill_discrete <- function(...) {
  scale_fill_manual(..., values = col)
}

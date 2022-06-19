# Load required packages, fetch the latest data
source("R/Setup.R")

# Bitcoin Prices
bitcoin<-fredr("CBBTCUSD")

# Change in Bitcoin since PP-Shwarma
# bitcoin US prices since March 28
plotdata<-bitcoin %>%
  mutate(date=as.Date(date)) %>%
  filter(date>="2022-03-15")
ggplot(plotdata,aes(date,value))+
  geom_line(size=2,color=col[1])+
  geom_point(data=filter(plotdata,date=="2022-03-28"),
             color=col[1],size=2.5,stroke=2.5,fill='white',shape=21)+
  geom_point(data=filter(plotdata,date==max(date)),
             color=col[1],size=2.5,stroke=2.5,fill='white',shape=21)+
  annotate('text',x=as.Date("2022-04-02"),y=50000,hjust=0,color=col[1],
           label="Pierre Poilievre says: \"opt out\" of\ninflation by buying crypto")+
  annotate('text',x=as.Date("2022-05-09"),y=35250,hjust=0,color=col[1],vjust=0,
           label="Down 25%")+
  annotate('text',x=as.Date("2022-06-13")-2,y=23500,hjust=1,color=col[1],vjust=0,
           label="Down 50%")+
  annotate('text',x=max(plotdata$date)+2,y=filter(plotdata,date==max(date))$value,
           hjust=0,color=col[1],vjust=0,
           label=paste("Down",percent(abs(filter(plotdata,date==max(date))$value/filter(plotdata,date==as.Date("2022-03-28"))$value-1))))+
  geom_segment(x=as.Date("2022-04-01"),y=50000,
               xend=as.Date("2022-03-29"),yend=48000,
               color=col[1],size=1,arrow=arrow(length=unit(1.5,'mm')))+
  scale_y_continuous(limit=c(NA,51000),label=dollar,
                     breaks=pretty_breaks(5))+
  scale_x_date(limit=as.Date(c(min(plotdata$date),max(plotdata$date)+10)))+
  mytheme+
  labs(x="",y="US Dollars",
       caption='Graph by @trevortombe',
       title="Bitcoin Prices Since March 15",
       subtitle=paste("Source: St. Louis FRED, Series CBBTCUSD. As of",max(plotdata$date)))
ggsave("Plots/Bitcoin.png",width=8,height=4)


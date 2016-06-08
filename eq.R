 
library(data.table) ## fast read csv function
library(sqldf)      ## data munging
library(magrittr)   ## data munging
library(leaflet)    ## mapping package

## load/prep data
d <- fread("eq_dat.csv")
d <- subset(d, type=="earthquake")
d$type <- as.factor(as.character(d$type))

## classify using definitions laid out in http://www.geo.mtu.edu/UPSeis/magnitude.html
d$class <- cut(d$mag, c(0,4,5,6,7,8,100), c("Minor", "Light", "Moderate", "Strong", "Major", "Great"))
d$class2 <- cut(d$mag, c(0,4,5,6,7,8,100), c("Minor (0-3.9)", "Light (4.0-4.9)", "Moderate (5.0-5.9)", 
                                             "Strong (6.0-6.9)", "Major (7.0-7.9)", "Great (8.0+)"))
## frequency tables
table(d$class, d$year)
table(d$class2, d$year)

## leaflet plot
leaflet(data=subset(d, year==2011)) %>% addTiles() %>% addCircles(lng= ~longitude, lat= ~latitude, weight=1, radius= ~10*mag^6) %>% 
  setView(lng=-98.5561, lat=39.8106, zoom=4)



## statistical significance (is there a year/year increase?)
y <- as.data.frame(table(d$class, d$year))
names(y) <- c("Class", "Year", "Count")
y$Year <- as.numeric(as.character(y$Year))

## subsetting data
y1 <- subset(y, Class=="Minor")
y2 <- subset(y, Class=="Light")
y3 <- subset(y, Class=="Moderate")
y4 <- subset(y, Class=="Strong")
y5 <- subset(y, Class=="Major")
y6 <- subset(y, Class=="Great")

### summary stats table w/ fitted regression lines (is there a year/year increase?)
data.frame(Class=unique(y$Class), 
           Coefficient=c(round(lm(Count~Year, data=y1)$coefficient[2],2), 
                         round(lm(Count~Year, data=y2)$coefficient[2],2), 
                         round(lm(Count~Year, data=y3)$coefficient[2],2), 
                         round(lm(Count~Year, data=y4)$coefficient[2],2), 
                         round(lm(Count~Year, data=y5)$coefficient[2],2), 
                         round(lm(Count~Year, data=y6)$coefficient[2],2)),
           R_Squared=c(round(summary(lm(Count~Year, data=y1))$r.squared,4), 
                       round(summary(lm(Count~Year, data=y2))$r.squared,4), 
                       round(summary(lm(Count~Year, data=y3))$r.squared,4), 
                       round(summary(lm(Count~Year, data=y4))$r.squared,4), 
                       round(summary(lm(Count~Year, data=y5))$r.squared,4), 
                       round(summary(lm(Count~Year, data=y6))$r.squared,4)))

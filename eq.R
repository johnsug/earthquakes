
library(ggmap)
library(sqldf)

setwd("C:/Users/JS033085/Desktop/EQ")
d <- read.csv("dat.csv")

d$magg <- 10^d$mag
table(d$magType)

d2 <- sqldf("select round(latitude,0) as lat, round(longitude,0) as long, sum(magg) as magnitude 
            from d 
            where (longitude between -130 and -70) 
              and (latitude between 10 and 70)
            group by 1, 2")


g <- qmap("salina, kansas", zoom = 4) 
g + geom_tile(data=d2, aes(x=long, y=lat, alpha=magnitude), fill = 'red') + labs(x="", y="")

## to keep everything on the same scale, I'll probably need to find the top magnitude for the entire study, and plot that on every iteration 
## (placing it somewhere out of the way, like lat=0, long=0)

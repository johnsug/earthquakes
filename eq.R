
setwd("C:/Users/JS033085/Desktop/EQ")
d <- read.csv("dat.csv")

library(ggmap)
library(sqldf)

d2 <- sqldf("select round(latitude,0) as lat, round(longitude,0) as long, count(type) as count 
            from d 
            where (longitude between -130 and -70) 
              and (latitude between 10 and 70)
            group by 1, 2")


g <- qmap("salina, kansas", zoom = 4) 
g + geom_tile(data=d2, aes(x=long, y=lat, alpha=count), fill = 'red') + labs(x="", y="")


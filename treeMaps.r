# Clear workspace
rm(list=ls())

# load libraries
library(ggplot2)
library(dplyr)
library(treemap)
library(RColorBrewer )

C:\Users\t\Box Sync

setwd("C:/Users/t/Box Sync/FoodForPeace/R/")
d <- read.csv("FFPdata0912.csv", sep = ",", header = TRUE)
names(d)

d$Food.Aid <- round(d$decTotal/1000, 0)
# d$Food.Aid <- format(d$tot.thous, big.mark=",", scientific=F)

# Subset data into years
dsub <- subset(d, subset=year==2013 & Food.Aid>0)
dfif <- subset(d, subset=year==1959 & Food.Aid>0)
dsix <- subset(d, subset=year==1969 & Food.Aid>0)
dsev <- subset(d, subset=year==1979 & Food.Aid>0)
degt <- subset(d, subset=year==1989 & Food.Aid>0)
dnin <- subset(d, subset=year==1999 & Food.Aid>0)
dzer <- subset(d, subset=year==2009 & Food.Aid>0)

# Basic treemap
  treemap(dsix, index=c("region", "country"),
          vSize = "Food.Aid",
          vColor ="Food.Aid",
          type = "value",
          palette="RdYlGn",
          fontsize.labels=c(40,10),
          align.labels=list(c("center", "center"), 
          c("left", "top")),
          lowerbound.cex.labels=.66,
          position.legend = "bottom",
          #inflate.labels = TRUE,
          title = "Food Aid Decadal Totals",
          #subtitle = "US Dollars (000s)",
          fontsize.title = 24,
          fontsize.legend = 16,
          bg.labels = 0,     
          algorithm = "pivotSize",
          #sortID = "size"
  )
        

c <- ggplot(dfif, aes(x = reorder(factor(country), Food.Aid), 
     y =Food.Aid )) + geom_bar(stat = "identity")

c + coord_flip()

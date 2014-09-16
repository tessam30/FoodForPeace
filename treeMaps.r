# Clear workspace
rm(list=ls())

# load libraries
library(ggplot2)
library(dplyr)
library(treemap)
library(RColorBrewer)
library(scales)

# Install trebuchet font to be used
#library(devtools)
#install_github("extrafont", "wch")
#font_import(pattern = "trebuc")



# Define lab colors (TODO: Source this)
# Lab RGB colors
redL   	<- c("#B71234")
dredL 	<- c("#822443")
dgrayL 	<- c("#565A5C")
lblueL 	<- c("#7090B7")
dblueL 	<- c("#003359")
lgrayL	<- c("#CECFCB")

setwd("C:/Users/t/Box Sync/FoodForPeace/R/")
d <- read.csv("FFPdata0912.csv", sep = ",", header = TRUE)
names(d)

d$Food.Aid <- round(d$decTotal/1000000, 0)
# d$Food.Aid <- format(d$tot.thous, big.mark=",", scientific=F)

# Subset data into years
dsub <- subset(d, subset=year==2013 & Food.Aid>0)
dfif <- subset(d, subset=year==1959 & Food.Aid>0)
dsix <- subset(d, subset=year==1969 & Food.Aid>0)
dsev <- subset(d, subset=year==1979 & Food.Aid>0)
degt <- subset(d, subset=year==1989 & Food.Aid>0)
dnin <- subset(d, subset=year==1999 & Food.Aid>0)
dzer <- subset(d, subset=year==2009 & Food.Aid>0)

# Basic treemap function to call with subsetted data above
myTree <- function(x) {	
  tm <- treemap(x, index=c("region", "country"),
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
}      

# Create a function to keep top X records; z controls graphic title -- linked to x input.
myBar <- function(x, y, z, a) {
  dplot <- head(arrange(x, desc(Food.Aid)), n = y) 
  c <- ggplot(dplot, aes(x = reorder(factor(country), Food.Aid), 
                         y =Food.Aid, fill = "country")) + geom_bar(stat = "identity")+ scale_fill_manual(values = redL )
  d <- c + coord_flip()+labs(x ="", title = z, y = "Total Food Aid ($ US Millions)") +
    theme(legend.position = "none", panel.background=element_rect(fill=lgrayL))  + 
    scale_y_continuous(labels = dollar ) 
  print(d)
  ggsave(d, filename = paste(a, ".png"), width=11, height=8)
}


myBar(dfif, 15, "Food Aid Aggregates: 1954-1959", "fifties")
myBar(dsix, 15, "Food Aid Aggregates: 1960-1969", "sixties")
myBar(dsev, 15, "Food Aid Aggregates: 1970-1979", "seventies")
myBar(degt, 15, "Food Aid Aggregates: 1980-1989", "eighties")
myBar(dnin, 15, "Food Aid Aggregates: 1990-1999", "nineties")
myBar(dzer, 15, "Food Aid Aggregates: 2000-2009", "thous")
myBar(dsub, 15, "Food Aid Aggregates: 2009-2013", "thousten")

# Create a function for regions
myReg <- function(x) {
  p <- ggplot(x , aes(x = reorder(factor(region), decTotalReg), 
                      y = decTotalReg, fill = "Red")) + geom_bar(stat = "identity")
  p + coord_flip() + labs(x = "Receipient Country", title = "Regional Totals by Decade", y = "Total Food Aid ($)") +
    theme(legend.position = "none") + scale_y_continuous(labels = dollar)
}

# Clear workspace
rm(list=ls())

# TODO: write function to check if libraries exist, if not go out and install from CRAN.
# Check if the required libraries exist, if not install them 
required_lib =c("ggplot2","dplyr","treemap", "RColorBrewer", "scales", "doBy", "devtools")

install_required_libs<-function(){
  for(i in 1:length(required_lib)){
    if(required_lib[i] %in% rownames(installed.packages()) == FALSE)
    {install.packages(required_lib[i])}
  }
}

install_required_libs()

# Load required libraries
lapply(required_lib, require, character.only=T)

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

# Set local working directory (pick from U drive, Home drive, or P drive).
locwd <- c("U:/FoodForPeace/R/")
homewd <- c("C:/Users/t/Box Sync/FoodForPeace/R/")
pdrwd <- c("P:/GeoCenter/GIS/Projects/FoodForPeace/R")
setwd(pdrwd)
getwd()

# Load data in data frame named d
d <- read.csv("FFPdata0912.csv", sep = ",", header = TRUE)
names(d)

d$Food.Aid <- round(d$decTotal/1000000, 0)

# d$Food.Aid <- format(d$tot.thous, big.mark=",", scientific=F)

# Subset data into years (could use dplyr but use subset for other team members).
dsub <- subset(d, subset=year==2013 & Food.Aid>0)
dfif <- subset(d, subset=year==1959 & Food.Aid>0)
dsix <- subset(d, subset=year==1969 & Food.Aid>0)
dsev <- subset(d, subset=year==1979 & Food.Aid>0)
degt <- subset(d, subset=year==1989 & Food.Aid>0)
dnin <- subset(d, subset=year==1999 & Food.Aid>0)
dzer <- subset(d, subset=year==2009 & Food.Aid>0)
   

# Create a function to keep top N records;
# x = input parameter, should be the data frame for each decade
# y = input parameter, controls how many countries are retained for each graph
# z = input paramenter (text), title of the graph
# z = input paramenter (text), name of the file/graph when saved. Results are written to working directory

myBar <- function(x, y, z, a) {
  dplot <- head(arrange(x, desc(Food.Aid)), n = y) 
  c <- ggplot(dplot, aes(x = reorder(factor(country), Food.Aid), 
                         y =Food.Aid, fill = "country")) + geom_bar(stat = "identity")+ scale_fill_manual(values = redL )
  pp <- c + coord_flip()+labs(x ="", title = z, y = "($ 2013 US Millions)") +
    theme(legend.position = "none", panel.background=element_rect(fill="white"), axis.ticks.y=element_blank(),
	axis.text.y  = element_text(hjust=1, size=10, colour = dblueL), axis.ticks.x=element_blank(),
	axis.text.x  = element_text(hjust=1, size=10, colour = dblueL),
	axis.title.x = element_text(colour=dblueL, size=8),
	plot.title = element_text(lineheight=.8, colour = dblueL))  + 
    scale_y_continuous(labels = dollar ) 
  print(pp)
  ggsave(pp, filename = paste(a, ".png"), width=7.5, height=5.5)
}

# Call function with appropriate paramenters for each decade
num <- c(10)
myBar(dfif, num, "Food Assistance Top Recipients: 1954-1959", "fifties")
myBar(dsix, num, "Food Assistance Top Recipients: 1960-1969", "sixties")
myBar(dsev, num, "Food Assistance Top Recipients: 1970-1979", "seventies")
myBar(degt, num, "Food Assistance Top Recipients: 1980-1989", "eighties")
myBar(dnin, num, "Food Assistance Top Recipients: 1990-1999", "nineties")
myBar(dzer, num, "Food Assistance Top Recipients: 2000-2009", "thous")
myBar(dsub, num, "Food Assistance Top Recipients: 2009-2013", "thousten")

### Bring in regional data
dd <- read.csv("FFPdata0912_RegionTotals.csv", sep = ",", header = TRUE)
dd$Food.Aid.R <- round(dd$decTotal/1000000, 0) # Round to nearest million

dsub <- subset(dd, subset=year==2013)
dfif <- subset(dd, subset=year==1959)
dsix <- subset(dd, subset=year==1969)
dsev <- subset(dd, subset=year==1979)
degt <- subset(dd, subset=year==1989)
dnin <- subset(dd, subset=year==1999)
dzer <- subset(dd, subset=year==2009)

# Create a function for regions (similar to the one above but no y input)
myReg <- function(x, z, a) {
  p <- ggplot(x , aes(x = reorder(factor(region), Food.Aid.R), 
                      y = Food.Aid.R, fill = "region")) + geom_bar(stat = "identity") + scale_fill_manual(values = dblueL )
 pp <- p + coord_flip()+labs(x ="", title = z, y = "($ 2013 US Millions)") +
    theme(legend.position = "none", panel.background=element_rect(fill="white"), axis.ticks.y=element_blank(),
	axis.text.y  = element_text(hjust=1, size=10, colour = dblueL), axis.ticks.x=element_blank(),
	axis.text.x  = element_text(hjust=1, size=10, colour = dblueL),
	axis.title.x = element_text(colour=dblueL, size=8),
	plot.title = element_text(lineheight=.8, colour = dblueL))  + 
    scale_y_continuous(labels = dollar ) 
  print(pp)
  ggsave(pp, filename = paste(a, ".png"), width=7.5, height=5.5)
}

myReg(dfif, "Regional Totals: 1954-1959", "fiftiesR")
myReg(dsix, "Regional Totals: 1960-1969", "sixtiesR")
myReg(dsev, "Regional Totals: 1970-1979", "seventiesR")
myReg(degt, "Regional Totals: 1980-1989", "eightiesR")
myReg(dnin, "Regional Totals: 1990-1999", "ninetiesR")
myReg(dzer, "Regional Totals: 2000-2009", "thousR")
myReg(dsub, "Regional Totals: 2009-2013", "thoustenR")

# Calculate totals Aggregates for decades
summaryBy(decTotal ~ year, data = d, FUN = sum)

grandT <- summaryBy(decTotal ~ region, data = d, FUN = sum)
grandD <- as.data.frame(grandT)
grandD$decTotal.sumR <- round(grandD$decTotal.sum/1000000, 0)

# Plot grand totals
 p <- ggplot(grandD , aes(x = reorder(factor(region), decTotal.sumR), 
                      y = decTotal.sumR, fill = "region")) + geom_bar(stat = "identity") + scale_fill_manual(values = dblueL )
 pp <- p + coord_flip()+labs(x ="", title = "Regional Totals: 1954-2013", y = "($ 2013 US Millions)") +
    theme(legend.position = "none", panel.background=element_rect(fill="white"), axis.ticks.y=element_blank(),
	axis.text.y  = element_text(hjust=1, size=10, colour = dblueL), axis.ticks.x=element_blank(),
	axis.text.x  = element_text(hjust=1, size=10, colour = dblueL),
	axis.title.x = element_text(colour=dblueL, size=8),
	plot.title = element_text(lineheight=.8, colour = dblueL))  + 
    scale_y_continuous(labels = dollar ) 
  print(pp)
  ggsave(pp, filename = paste("GrandTot", ".png"), width=7.5, height=5.5)

# Calculate total Aggregates for countries
summaryBy(decTotal ~ country, data = d, FUN = sum)
countryTot <- as.data.frame(summaryBy(decTotal ~ country, data = d, FUN = sum))
countryTot$decTotal.sumD <- round(countryTot$decTotal.sum/1000000, 0)

# Plot top 25 receipients of Food Assistance
  dplot <- head(arrange(countryTot, desc(decTotal.sum)), n = 25) 
  c <- ggplot(dplot, aes(x = reorder(factor(country), decTotal.sumD), 
                         y =decTotal.sumD, fill = "country")) + geom_bar(stat = "identity")+ scale_fill_manual(values = redL )
  d <- c + coord_flip()+labs(x ="", title = "Food Assistance Top Recipients: 1954-2013", y = "($ 2013 US Millions)") +
    theme(legend.position = "none", panel.background=element_rect(fill="white"), axis.ticks.y=element_blank(),
	axis.text.y  = element_text(hjust=1, size=10, colour = dblueL), axis.ticks.x=element_blank(),
	axis.text.x  = element_text(hjust=1, size=10, colour = dblueL),
	axis.title.x = element_text(colour=dblueL, size=8),
	plot.title = element_text(lineheight=.8, colour = dblueL))  + 
    scale_y_continuous(labels = dollar ) 
  print(d)
  ggsave(d, filename = paste("CountryTot", ".png"), width=7.5, height=5.5)


## EXTRA CODE (unused) ##

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
                title = "Food Assistance Decadal Totals",
                #subtitle = "US Dollars (000s)",
                fontsize.title = 24,
                fontsize.legend = 16,
                bg.labels = 0,     
                algorithm = "pivotSize",
                #sortID = "size"
  )
}


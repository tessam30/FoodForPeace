# Clear workspace
rm(list=ls())

# load libraries
library(ggplot2)
library(dplyr)
library(treemap)
library(RColorBrewer)
library(scales)
library(doBy)

# Install trebuchet font to be used
#library(devtools)
#install_github("extrafont", "wch")
#font_import(pattern = "trebuc")

# Define lab colors (TODO: Source this)
# Lab RGB colors
redL    <- c("#B71234")
dredL 	<- c("#822443")
dgrayL 	<- c("#565A5C")
lblueL 	<- c("#7090B7")
dblueL 	<- c("#003359")
lgrayL	<- c("#CECFCB")

#setwd("U:/FoodForPeace/R/")
setwd("C:/Users/t/Box Sync/FoodForPeace/R/")
d <- read.csv("FFPdata0912.csv", sep = ",", header = TRUE)
names(d)

d$Food.Aid <- round(d$decTotal/1000000, 0)
d$Food.AidNom <- round(d$decTotalNom/1000000, 0)

# d$Food.Aid <- format(d$tot.thous, big.mark=",", scientific=F)

# Subset data into years; where x[1] = 1959, x[2] = 1953 etc...
# Apply vector of names to new subset data
x.split <- split(d, d$year)
new_names <- c("dfif", "dsix", "dsev", "degt", "dnin", "dzer", "dsub")
for (i in 1:length(x.split)) {
  assign(new_names[i], x.split[[i]])
}


# Create a function to keep top X records; z controls graphic title -- linked to x input.
myBar <- function(x, y, z, a) {
  
  # Filter the data to select top N countries of NOMINAL FOOD Aid expenditures
  dplot <- head(arrange(x, desc(Food.AidNom)), n = y) 
  
  # Initialize a ggplot bar chart using the dollars as stat
  c <- ggplot(dplot, aes(x = reorder(factor(country), Food.AidNom), 
         y =Food.AidNom, fill = "country")) + geom_bar(stat = "identity") + scale_fill_manual(values = redL )
  
  # Touch up plot to remove formatting and add USAID lab colours
  pp <- c + coord_flip()+labs(x ="", title = z, y = "($ Nominal US Millions)") +
          theme(legend.position = "none", panel.background=element_rect(fill="white"), 
          axis.ticks.y=element_blank(),
          axis.text.y  = element_text(hjust=1, size=10, colour = dblueL), 
          axis.ticks.x=element_blank(),
          axis.text.x  = element_text(hjust=1, size=10, colour = dblueL),
          axis.title.x = element_text(colour=dblueL, size=8),
          plot.title = element_text(lineheight=.8, colour = dblueL))  + 
          scale_y_continuous(labels = dollar ) 
    
  print(pp)
  
  # Save the plot in the working directory using the values in a (new_names)
  ggsave(pp, filename = paste(a, ".png"), width=7.5, height=5.5)
}

# Define top N list and names for top of each graph
topList <- c(10)
graph.title <- c("Top 10 Receipients of Food Aid: 1954-1959",
                 "Top 10 Receipients of Food Aid: 1960-1969",
                 "Top 10 Receipients of Food Aid: 1970-1979",
                 "Top 10 Receipients of Food Aid: 1980-1989",
                 "Top 10 Receipients of Food Aid: 1990-1999",
                 "Top 10 Receipients of Food Aid: 2000-2009",
                 "Top 10 Receipients of Food Aid: 2010-2013")


for (i in 1:length(new_names)) {
  myBar(get(new_names[i]), topList, graph.title[i], new_names[i])
}


### Bring in regional data

dd <- read.csv("FFPdata0912_RegionTotals.csv", sep = ",", header = TRUE)
dd$Food.Aid.R <- round(dd$decTotal/1000000, 0)  # For real numbers
dd$Food.Aid.RNom <- round(dd$decTotalNom/1000000, 0) # For nomimal numbers (default)

x.splitreg <- split(dd, dd$year)
new_namesreg <- c("dfifreg", "dsixreg", "dsevreg", "degtreg", "dninreg", "dzerreg", "dsubreg")
for (i in 1:length(x.split)) {
  assign(new_namesreg[i], x.splitreg[[i]])
}

# Create a function for regions
myReg <- function(x, z, a) {
  
  # Initialize a ggplot bar graph for showing regional totals by decade
  p <- ggplot(x , aes(x = reorder(factor(region), Food.Aid.RNom), 
                      y = Food.Aid.RNom, fill = "region")) + geom_bar(stat = "identity") + scale_fill_manual(values = dblueL )
  
  # Touch up plot to remove formatting and add USAID lab colours
  pp <- p + coord_flip()+labs(x ="", title = z, y = "($ Nominal US Millions)") +
       theme(legend.position = "none", panel.background=element_rect(fill="white"), 
          axis.ticks.y=element_blank(),
          axis.text.y  = element_text(hjust=1, size=10, colour = dblueL), 
          axis.ticks.x=element_blank(),
          axis.text.x  = element_text(hjust=1, size=10, colour = dblueL),
          axis.title.x = element_text(colour=dblueL, size=8),
          plot.title = element_text(lineheight=.8, colour = dblueL))  + 
          scale_y_continuous(labels = dollar ) 
  print(pp)
  
  # Save plot using names in vector a (new_namesreg)
  ggsave(pp, filename = paste(a, ".png"), width=7.5, height=5.5)
}


graph.titlereg <- c("Regional Totals: 1954-1959",
                 "Regional Totals: 1960-1969",
                 "Regional Totals: 1970-1979",
                 "Regional Totals: 1980-1989",
                 "Regional Totals: 1990-1999",
                 "Regional Totals: 2000-2009",
                 "Regional Totals: 2010-2013")


for (i in 1:length(new_namesreg)) {
  myReg(get(new_namesreg[i]), graph.titlereg[i], new_namesreg[i])
}



# Calculate totals Aggregates for decades
summaryBy(decTotalNom ~ year, data = d, FUN = sum)

grandT <- summaryBy(decTotalNom ~ region, data = d, FUN = sum)
grandD <- as.data.frame(grandT)
grandD$decTotalNom.sumR <- round(grandD$decTotalNom.sum/1000000, 0)

# Plot grand totals
p <- ggplot(grandD , aes(x = reorder(factor(region), decTotalNom.sumR), 
                         y = decTotalNom.sumR, fill = "region")) + geom_bar(stat = "identity") + scale_fill_manual(values = dblueL )
pp <- p + coord_flip()+labs(x ="", title = "Regional Totals: 1954-2013", y = "($ Nominal US Millions)") +
  theme(legend.position = "none", panel.background=element_rect(fill="white"), axis.ticks.y=element_blank(),
        axis.text.y  = element_text(hjust=1, size=10, colour = dblueL), axis.ticks.x=element_blank(),
        axis.text.x  = element_text(hjust=1, size=10, colour = dblueL),
        axis.title.x = element_text(colour=dblueL, size=8),
        plot.title = element_text(lineheight=.8, colour = dblueL))  + 
  scale_y_continuous(labels = dollar ) 
print(pp)
ggsave(pp, filename = paste("GrandTotNom", ".png"), width=7.5, height=5.5)


# Calculate total Aggregates for countries
summaryBy(decTotal ~ country, data = d, FUN = sum)
countryTot <- as.data.frame(summaryBy(decTotal ~ country, data = d, FUN = sum))
countryTot$decTotal.sumD <- round(countryTot$decTotal.sum/1000000, 0)

# Plot top 25 receipients of Food Aid
dplot <- head(arrange(countryTot, desc(decTotal.sum)), n = 25) 
c <- ggplot(dplot, aes(x = reorder(factor(country), decTotal.sumD), 
                       y =decTotal.sumD, fill = "country")) + geom_bar(stat = "identity")+ scale_fill_manual(values = redL )
p <- c + coord_flip()+labs(x ="", title = "Top 25 Recipients of Food Aid: 1954-2013", y = "($ 2013 US Millions)") +
  theme(legend.position = "none", panel.background=element_rect(fill="white"), axis.ticks.y=element_blank(),
        axis.text.y  = element_text(hjust=1, size=10, colour = dblueL), axis.ticks.x=element_blank(),
        axis.text.x  = element_text(hjust=1, size=10, colour = dblueL),
        axis.title.x = element_text(colour=dblueL, size=8),
        plot.title = element_text(lineheight=.8, colour = dblueL))  + 
  scale_y_continuous(labels = dollar ) 
print(p)
ggsave(p, filename = paste("CountryTot", ".png"), width=7.5, height=5.5)


# Calculate total Aggregates for countries
summaryBy(decTotalNom ~ country, data = d, FUN = sum)
countryTot <- as.data.frame(summaryBy(decTotalNom ~ country, data = d, FUN = sum))
countryTot$decTotalNom.sumD <- round(countryTot$decTotalNom.sum/1000000, 0)


# Plot top 25 receipients of Food Aid in nominal terms
dplot <- head(arrange(countryTot, desc(decTotalNom.sum)), n = 25) 
c <- ggplot(dplot, aes(x = reorder(factor(country), decTotalNom.sumD), 
                       y =decTotalNom.sumD, fill = "country")) + geom_bar(stat = "identity")+ scale_fill_manual(values = redL )
p <- c + coord_flip()+labs(x ="", title = "Top 25 Recipients of Food Aid: 1954-2013", y = "($ Nominal US Millions)") +
  theme(legend.position = "none", panel.background=element_rect(fill="white"), axis.ticks.y=element_blank(),
        axis.text.y  = element_text(hjust=1, size=10, colour = dblueL), axis.ticks.x=element_blank(),
        axis.text.x  = element_text(hjust=1, size=10, colour = dblueL),
        axis.title.x = element_text(colour=dblueL, size=8),
        plot.title = element_text(lineheight=.8, colour = dblueL))  + 
  scale_y_continuous(labels = dollar ) 
print(p)
ggsave(p, filename = paste("CountryTot", ".png"), width=7.5, height=5.5)


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
myTree(d)

# Load data to plot annual totals by funding instrument.

# Clear workspace
rm(list=ls())

# TODO: write function to check if libraries exist, if not go out and install from CRAN.
# Check if the required libraries exist, if not install them 
required_lib =c("ggplot2","dplyr","treemap", "RColorBrewer", "scales", "doBy", "devtools", "reshape2")

install_required_libs<-function(){
  for(i in 1:length(required_lib)){
    if(required_lib[i] %in% rownames(installed.packages()) == FALSE)
    {install.packages(required_lib[i])}
  }
}

install_required_libs()

# Load required libraries
lapply(required_lib, require, character.only=T)

# Define lab colors (TODO: Source this)
# Lab RGB colors
redL     <- c("#B71234")
dredL 	<- c("#822443")
dgrayL 	<- c("#565A5C")
lblueL 	<- c("#7090B7")
dblueL 	<- c("#003359")
lgrayL	<- c("#CECFCB")

# Set local working directory (pick from U drive, Home drive, or P drive).
locwd <- c("U:/FoodForPeace/R/")
homewd <- c("C:/Users/t/Box Sync/FoodForPeace/R/")
pdrwd <- c("P:/GeoCenter/GIS/Projects/FoodForPeace/R")
setwd(homewd)
getwd()

d <- read.csv("FFP_mechanisms.csv", sep = ",", header = TRUE)
names(d)

# rename variables to graph neatly
names(d) <- c("Decade", 
             "Title I", 
             "Title II Emergency", 
             "Title II Development",
             "Title III", 
             "Title IV", 
             "Section 416(b)", 
             "Local and Regional Procurement Pilot",
             "McGovern-Dole Food and Education", 
             "Title V", 
             "Food for Progress",
             "Community Development Funds", 
             "Emergency Food Security Program",
             "Overseas Contingency Operations")
             
# Restack the data on top of each other mapping the var name into the variable column
d.melt <- melt(d, id.vars = "Decade")
             
# Divide through by 1M
d.melt$value <- round(d.melt$value/1000000, 0)
             
# Write a function to make graphs for each decade
# x = decade on the filter is run
# z = title of the graph
# a = graph name

myBar <- function(x, z, a){
  c <- ggplot(filter(d.melt, Decade==x, value>0), aes(x = reorder(factor(variable), value), 
        y = value, fill = "variable")) + geom_bar(stat = "identity") +  coord_flip() + scale_fill_manual(values = lblueL )
  pp <- c + coord_flip()+labs(x ="", title = z , y = "($ 2013 US Millions)") +
     theme(legend.position = "none", panel.background=element_rect(fill="white"), axis.ticks.y=element_blank(),
     axis.text.y  = element_text(hjust=1, size=10, colour = dblueL), axis.ticks.x=element_blank(),
     axis.text.x  = element_text(hjust=1, size=10, colour = dblueL),
     axis.title.x = element_text(colour=dblueL, size=8),
     plot.title = element_text(lineheight=.8, colour = dblueL))  + 
     scale_y_continuous(labels = dollar ) 
  print(pp)
  ggsave(pp, filename = paste(a, ".png"), width=7.5, height=5.5)
}

myBar(1950, "Total Food Assistance Breakdown 1954-1959", "Portfolio.1950")
myBar(1960, "Total Food Assistance Breakdown 1960-1969", "Portfolio.1960")
myBar(1970, "Total Food Assistance Breakdown 1970-1979", "Portfolio.1970")
myBar(1980, "Total Food Assistance Breakdown 1980-1989", "Portfolio.1980")
myBar(1990, "Total Food Assistance Breakdown 1990-1999", "Portfolio.1990")
myBar(2000, "Total Food Assistance Breakdown 2000-2010", "Portfolio.2000")
myBar(2010, "Total Food Assistance Breakdown 2010-2013", "Portfolio.2010")
                           

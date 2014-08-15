# Name: FFP_eda.R
# Purpose: Exploratory data analysis of FFP data 
# Date: 8/8/2014

# Plot FFP funding over time
rm( list=ls())

# Check if the required libraries exist, if not install them 
required_lib =c("foreign", "dplyr", "ggplot2", "ggthemes")
install_required_libs()

# Create a simple function to check if libraries exist or not, load if not;
install_required_libs<-function(){
  for(i in 1:length(required_lib)){
    if(required_lib[i] %in% rownames(installed.packages()) == FALSE)
    {install.packages(required_lib[i])}
  }
}

# Load required libraries
lapply(required_lib, require, character.only=T)

wsp <- c("U:/FoodForPeace/Excel/")
tgt <- c("FFP_draft_8_7.csv")

setwd(wsp)
d <- tbl_df(read.csv(tgt, header=TRUE, sep = ",", stringsAsFactors = FALSE))
d$reg <- factor(d$region)

# Write a function that will generate graphics based on input

ptot <- function(x) {
		tmp <- d %>%
		filter(region == x)
			ggplot(tmp, aes(x = year, y = lntotal_2013)) + 
			geom_line(data=tmp[!is.na(tmp$lntotal_2013),]) + 
			facet_wrap(~country) + theme(strip.text.x = element_text(size=6), plot.title = element_text(size = 12)) +
			labs(x="Year", y="Total Funding 2013 $USD", title="Logged Total FFP Funding")
}

psh <- function(x) {
		tmp <- d %>%
		filter(region == x)
			ggplot(tmp, aes(x = year, y = yearlyShare)) + 
			geom_line(data=tmp[!is.na(tmp$yearlyShare),]) + 
			facet_wrap(~country) + theme(strip.text.x = element_text(size=6), plot.title = element_text(size = 12)) +
			labs(x="Year", y="Annual overall share", title="Yearly share of FFP funding")
}

pctv <- function(x) {
		tmp <- d %>%
		filter(region == x)
			ggplot(tmp, aes(x = year, y = countryShare)) + 
			geom_line(data=tmp[!is.na(tmp$countryShare),]) + 
			facet_wrap(~country) + theme(strip.text.x = element_text(size=6), plot.title = element_text(size = 12)) +
			labs(x="Year", y="Annual overall share", title="Cumulative share of FFP funding")
}


# Create vector of region names
myTab <- as.data.frame(table(d$reg))
myTab[1]

# run function to generate each graph iteratively, checking for outliers here
ptot("AFRICA")
psh("AFRICA")
pctv("AFRICA")

ptot("ASIA")
psh("ASIA")
pctv("ASIA")

ptot("EUROPE-SW ASIA")
psh("EUROPE-SW ASIA")
pctv("EUROPE-SW ASIA")

ptot("OCEANIA")
psh("OCEANIA")
pctv("OCEANIA")

ptot("SOUTH AMERICA")
psh("SOUTH AMERICA")
pctv("SOUTH AMERICA")

ptot("RUSSIA") + 
psh("RUSSIA")


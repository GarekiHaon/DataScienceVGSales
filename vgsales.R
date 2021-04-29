library(ggplot2)
library(dplyr)
vgsales <- read.csv(file='vgsales.csv')
#Data Cleaning
vgsales <- vgsales[!vgsales$Year == "N/A",]
vgsales[vgsales$Year == "2020", 4] <- "2009"
#Aggregating sales data by average sales (global)
yearAvg <- aggregate(Global_Sales ~ Year, data = vgsales, mean)
genreAvg <- aggregate(Global_Sales ~ Genre, data = vgsales, mean)
platformAvg <- aggregate(Global_Sales ~ Platform, data = vgsales, mean)
publisherAvg <- aggregate(Global_Sales ~ Publisher, data = vgsales, mean)
#Aggregating sales data by total sales (global)
yearTotal <- aggregate(Global_Sales ~ Year, data = vgsales, sum)
genreTotal <- aggregate(Global_Sales ~ Genre, data = vgsales, sum)
platformTotal <- aggregate(Global_Sales ~ Platform, data = vgsales, sum)
publisherTotal <- aggregate(Global_Sales ~ Publisher, data = vgsales, sum)
#Total and Average (Global) Sales plotted by Year
ggplot(data=yearAvg, aes(x=Year, y=Global_Sales, group=1)) + geom_line() + geom_point() + ggtitle("Avg Video Game Sales by Release Year") + theme(plot.title = element_text(size=25, face="bold.italic"))
ggplot(data=yearTotal, aes(x=Year, y=Global_Sales, group=1)) + geom_line() + geom_point() + ggtitle("Total Video Game Sales by Release Year") + theme(plot.title = element_text(size=25, face="bold.italic"))
#Total and Average (Global) Sales plotted by Genre
ggplot(data=genreAvg, aes(x=Genre, y=Global_Sales)) + geom_bar(stat = 'identity') + ggtitle("Avg Video Game Sales by Genre") + theme(plot.title = element_text(size=25, face="bold.italic"))
ggplot(data=genreTotal, aes(x=Genre, y=Global_Sales)) + geom_bar(stat = 'identity') + ggtitle("Total Video Game Sales by Genre") + theme(plot.title = element_text(size=25, face="bold.italic"))
#Total and Average (Global) Sales plotted by Platform
ggplot(data=platformAvg, aes(x=Platform, y=Global_Sales)) + geom_bar(stat = 'identity') + ggtitle("Avg Video Game Sales by Platform") + theme(plot.title = element_text(size=25, face="bold.italic"))
ggplot(data=platformTotal, aes(x=Platform, y=Global_Sales)) + geom_bar(stat = 'identity') + ggtitle("Total Video Game Sales by Platform") + theme(plot.title = element_text(size=25, face="bold.italic"))
#Plotting top ten Publishers by Average (Global) Sales
ggplot(subset(publisherAvg,
Publisher %in% c("Nintendo", "Valve", "Palcom", "Red Orb", "Arena Entertainment", "UEP Systems", "RedOctane", "Hello Games", "Sony Computer Entertainment Europe", "Westwood Studios")), 
aes(x=Publisher, y=Global_Sales)) + geom_bar(stat = 'identity') + ggtitle("Top 10 Publishers (Avg Sales)") + theme(plot.title = element_text(size=25, face="bold.italic"))
#Plotting top ten Publishers by Total (Global) Sales
ggplot(subset(publisherTotal,
Publisher %in% c("Nintendo", "Electronic Arts", "Activision", "Sony Computer Entertainment", "Ubisoft", "Take-Two Interactive", "THQ", "Konami Digital Entertainment", "Sega", "Namco Bandai Games")), 
aes(x=Publisher, y=Global_Sales)) + geom_bar(stat = 'identity') + ggtitle("Top 10 Publishers (Total Sales)") + theme(plot.title = element_text(size=25, face="bold.italic"))

#Aggregating Non-Global Sales by Year (Average)
yearAvgNA <- aggregate(NA_Sales ~ Year, data = vgsales, mean)
yearAvgJP <- aggregate(JP_Sales ~ Year, data = vgsales, mean)
yearAvgEU <- aggregate(EU_Sales ~ Year, data = vgsales, mean)
yearAvgOther <- aggregate(Other_Sales ~ Year, data = vgsales, mean)
yearAvgNonGlobal <- merge(yearAvgNA, yearAvgJP)
yearAvgNonGlobal <- merge(yearAvgEU, yearAvgNonGlobal)
yearAvgNonGlobal <- merge(yearAvgOther, yearAvgNonGlobal)
yearAvgNonGlobal <- yearAvgNonGlobal[-c(38),] #2017 has almost no data and thus is removed
#Aggregating Non-Global Sales by Year (Total)
yearTotalNA <- aggregate(NA_Sales ~ Year, data = vgsales, sum)
yearTotalJP <- aggregate(JP_Sales ~ Year, data = vgsales, sum)
yearTotalEU <- aggregate(EU_Sales ~ Year, data = vgsales, sum)
yearTotalOther <- aggregate(Other_Sales ~ Year, data = vgsales, sum)
yearTotalNonGlobal <- merge(yearTotalNA, yearTotalJP)
yearTotalNonGlobal <- merge(yearTotalEU, yearTotalNonGlobal)
yearTotalNonGlobal <- merge(yearTotalOther, yearTotalNonGlobal)
yearTotalNonGlobal <- yearTotalNonGlobal[-c(38),] #2017 has almost no data and thus is removed
#Plotting average sales by year (Non Global)
ggplot(data=yearAvgNonGlobal, aes(Year)) +
  geom_line(aes(y=NA_Sales, group = 1, color = "North America")) + geom_point(aes(y=NA_Sales, color = "North America")) +
  geom_line(aes(y=JP_Sales, group = 1, color = "Japan")) + geom_point(aes(y=JP_Sales, color = "Japan")) +
  geom_line(aes(y=EU_Sales, group = 1, color = "Europe")) + geom_point(aes(y=EU_Sales, color = "Europe")) +
  geom_line(aes(y=Other_Sales, group = 1, color = "Other")) + geom_point(aes(y=Other_Sales, color = "Other")) +
  xlab("Release Year") + ylab("Avg Sales (Millions)") + theme(axis.text.x = element_text(angle = 90)) + 
  labs(title = "Region Specific Average Video Game Sales per Game by Release Year (1980-2016)", color = "Region")
#Plotting total sales by year (Non Global)
ggplot(data=yearTotalNonGlobal, aes(Year)) +
  geom_line(aes(y=NA_Sales, group = 1, color = "North America")) + geom_point(aes(y=NA_Sales, color = "North America")) +
  geom_line(aes(y=JP_Sales, group = 1, color = "Japan")) + geom_point(aes(y=JP_Sales, color = "Japan")) +
  geom_line(aes(y=EU_Sales, group = 1, color = "Europe")) + geom_point(aes(y=EU_Sales, color = "Europe")) +
  geom_line(aes(y=Other_Sales, group = 1, color = "Other")) + geom_point(aes(y=Other_Sales, color = "Other")) +
  xlab("Release Year") + ylab("Total Sales (Millions)") + theme(axis.text.x = element_text(angle = 90)) + 
  labs(title = "Region Specific Total Video Game Sales by Release year (1980-2016)", color = "Region")
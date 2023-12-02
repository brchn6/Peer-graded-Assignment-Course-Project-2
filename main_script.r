setwd("/home/labs/cssagi/barc/Coursera/R_coursera/Peer-graded Assignment Course Project 2/exdata_data_NEI_data")
library(ggplot2)
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
head(NEI)
head(NEI$Emissions)
head(NEI$year)


#Q number 1
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008
data <- NEI[, c("Emissions", "year")]
head(data)


png("Q1plot.png")
# Calculate the sum of emissions for each year
sum_by_year <- aggregate(Emissions ~ year, data, sum)

# Create a bar plot
barplot(sum_by_year$Emissions, names.arg = sum_by_year$year, 
        xlab = "Year", ylab = "Total Emissions",
        main = "Total Emissions by Year")
dev.off()  # Close the device when done


#Q number 2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008?
# Subset the data for Baltimore City, Maryland (fips == "24510") and select relevant columns
subset_data_on_24510 <- NEI[NEI$fips == "24510", c("year", "Emissions")]

# Calculate the sum of emissions by year
sum_by_year <- aggregate(Emissions ~ year, data = subset_data_on_24510, sum)

# Create a bar plot and save it to a PNG file
png("Q2plot.png")
barplot(sum_by_year$Emissions, names.arg = sum_by_year$year, 
        xlab = "Year", ylab = "Total Emissions",
        main = "Total Emissions by Year Baltimore City, Maryland")
dev.off()  # Close the device when done


#Q number 3
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Of the four types of sources indicated by the  type (point, nonpoint, onroad, nonroad) variable
# , which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
# Which have seen increases in emissions from 1999–2008? 

subset_data_on_24510 <- aggregate(Emissions ~ year + type, data = NEI, sum)
subset_data_on_24510 <- Subset("Emissions" , "year" ,"type", data = NEI)

# Using the data you provided, create a line plot
p <- ggplot(subset_data_on_24510, aes(x = year, y = Emissions, group = type, color = type)) +
  geom_line() + # Add lines
  geom_point() + # Add points
  labs(title = "Emissions by Year and Type",
       x = "Year",
       y = "Total Emissions",
       color = "Type") + # Labels
  scale_y_continuous(labels = scales::comma) + # Format y axis labels with commas
  scale_x_continuous(breaks = unique(subset_data_on_24510$year)) # Ensure x-axis has all the years


ggsave("Q3plot.png", p, width = 10, height = 6)

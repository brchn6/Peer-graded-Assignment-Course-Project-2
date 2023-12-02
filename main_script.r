setwd("/home/labs/cssagi/barc/Coursera/R_coursera/Peer-graded Assignment Course Project 2/exdata_data_NEI_data")
library(ggplot2)
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Q number 1
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008
data <- NEI[, c("Emissions", "year")]

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

#Q number 4
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Subset the SCC data frame where EI.Sector contains "Coal"
SCC_SS <- SCC[grepl("Coal", SCC[["EI.Sector"]], ignore.case = TRUE), ]

# Get a list of unique SCC codes from the subsetted SCC data frame
list_of_SCC_codes <- unique(SCC_SS$SCC)

# Subset the NEI data frame based on SCC codes
NEI_subset <- NEI[NEI$SCC %in% list_of_SCC_codes, ]

# Aggregate emissions by year and type
NEI_subset_sum <- aggregate(Emissions ~ year + type, data = NEI_subset, sum)

# Create a ggplot object for visualization
p <- ggplot(NEI_subset2, aes(x = year, y = Emissions, group = type, color = type)) +
  geom_line() + # Add lines
  geom_point() + # Add points
  
  # Label the plot
  labs(title = "Emissions from Coal Combustion-Related Sources (1999-2008)",
       x = "Year",
       y = "Total Emissions",
       color = "Type")

# Save the plot as a PNG file
ggsave("Q4plot.png", p, width = 10, height = 6)

# Q number 5
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Subset the NEI data frame to select rows where fips is "24510" and type is "ON-ROAD"
subset_data_on_24510 <- NEI[NEI$fips == "24510" & NEI$type == "ON-ROAD", c("fips","year", "Emissions","type")]

# Aggregate emissions data by year
subset_data_on_24510_sum <- aggregate(Emissions ~ year + fips, data = subset_data_on_24510, sum)

# Create a ggplot object for visualization
p <- ggplot(subset_data_on_24510_sum, aes(x = factor(year), y = Emissions, fill = factor(year))) +
  geom_bar(stat = "identity") + # Create a bar plot with actual emissions values
  scale_fill_brewer(palette = "Set3") + # Use a color palette for the fill
  labs(title = "Emissions from Motor Vehicle Sources (1999–2008) in Baltimore City",
       x = "Year",
       y = "Total Emissions") +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) # Rotate x labels for clarity

# Save the plot as a PNG file with specified dimensions
ggsave("Q5plot.png", p, width = 10, height = 6)


# Q number 6
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
subset_data_on_06037 <- NEI[NEI$fips == "06037" & NEI$type == "ON-ROAD", c("fips","year", "Emissions","type")]
# Aggregate emissions data by year
subset_data_on_06037_sum <- aggregate(Emissions ~ year + fips , data = subset_data_on_06037, sum)

mergeEmissions <- rbind(subset_data_on_06037_sum, subset_data_on_24510_sum)

library(ggplot2)
library(RColorBrewer)

# Assuming mergeEmissions is your data frame containing the data.
p <- ggplot(mergeEmissions, aes(x = factor(year), y = Emissions, fill = factor(fips))) + 
  geom_bar(stat="identity", alpha = .5) + # Create the bars with a slight transparency
  geom_text(aes(label = sprintf("%.1f", Emissions)),  # Add text labels on the bars
            position = position_stack(vjust = 0.5), 
            color = "black") +
  labs(title = expression('Total PM'[2.5]*" Motor Emissions in Baltimore City & LA County from 1999 to 2008"), 
       x = "Year", 
       y = expression('Total PM'[2.5]*" Emission (in tons)")) +
  scale_fill_discrete(name = "Region", 
                      labels = c("Los Angeles County", "Baltimore City")) +# Set custom labels for the legend
                      scale_fill_brewer(palette = "Set3")  

# Save the plot as a PNG file with specified dimensions
ggsave("Q6plot.png", p, width = 10, height = 6)

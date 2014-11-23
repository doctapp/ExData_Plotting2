require(graphics)
require(plyr)

get_data <- function() {
        
        # Download the data if not already downloaded
        zipfile <- "exdata-data-NEI_data.zip"
        if (!file.exists(zipfile)) {
                url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
                download.file(url, destfile = zipfile)
                unzip(zipfile)
        }
        
        NEI <- readRDS("summarySCC_PM25.rds")
        SCC <- readRDS("Source_Classification_Code.rds")
        
        NEI$Emissions <- as.numeric(NEI$Emissions)
        NEI$year <- as.numeric(NEI$year)
        
        return(list(NEI=NEI, SCC=SCC))
}

# Load the data
data <- get_data()

# Aggregate total emissions by year
city_data <- subset(data$NEI, fips==24510)
emissions_by_year <- ddply(city_data, .(year), summarize, total=sum(Emissions))

# Create a PNG
png(file="plot2.png", width = 480, height = 480, units="px", bg="transparent")

# Plot
plot(emissions_by_year,
     type='b',
     pch=19,
     col='blue',
     ylab='Total Emissions',
     main='Total PM2.5 Emissions for Baltimore City (MD) per year')

# Write result
dev.off()

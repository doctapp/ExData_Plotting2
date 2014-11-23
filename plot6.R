require(ggplot2)
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
NEI <- data$NEI
SCC <- data$SCC

# Checks if column refers to motor vehicles
is_motor <- function(col) {
        return(grepl("(diesel|gasoline|motor|highway).*vehicle", col, ignore.case = TRUE))
}

# Get the SCCs which refer to motor vehicles
motor_scc <- SCC[is_motor(SCC$Short.Name),]

# Get the NEI subset for Baltimore (24510) and LA (06037)
city_data <- NEI[NEI$fips=='24510' | NEI$fips == '06037',]

# Get the motor vehicles subset
motor <- city_data[city_data$SCC %in% motor_scc$SCC,]

# Aggregate total emissions by year and county (i.e. fips)
emissions_by_year <- ddply(motor, .(year, fips), summarize, total = sum(Emissions))

# Use 1999 (first sample) as the reference point to show
# the evolution of each county from that point forward
emissions_1999_baseline <- ddply(emissions_by_year, .(fips), transform, total = total - total[1])

# Create a PNG
png(file = "plot6.png", width = 480, height = 480, units = "px", bg = "transparent")

# Plot
g <- ggplot(emissions_1999_baseline, aes(year, total, colour = fips))
p <- g +
        geom_point(size = 3) +
        geom_smooth(method = "lm") +
        ggtitle("Evolution of Total Emissions for Motor Vehicles per Year") +
        ylab("Total Emission Differential from 1999 Baseline") +
        scale_colour_discrete("County", labels = c("Los Angeles", "Baltimore"))

print(p)

# Write result
dev.off()
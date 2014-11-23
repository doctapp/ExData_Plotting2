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

# Get the NEI subset for Baltimore
city_data <- NEI[NEI$fips==24510,]

# Get the motor vehicles subset
motor <- city_data[city_data$SCC %in% motor_scc$SCC,]

# Aggregate total emissions by year
emissions_by_year <- ddply(motor, .(year), summarize, total = sum(Emissions))

# Create a PNG
png(file = "plot5.png", width = 480, height = 480, units = "px", bg = "transparent")

# Plot
g <- ggplot(emissions_by_year, aes(year, total))
p <- g +
        geom_point(size = 3) +
        geom_smooth(method = "lm") +
        ggtitle("Total PM2.5 Emissions for Motor Vehicles in Baltimore per Year") +
        ylab("Total Emissions")

print(p)

# Write result
dev.off()
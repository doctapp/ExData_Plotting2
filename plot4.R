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

# Checks if column refers to coal combustion
is_coal_Comb <- function(col) {
        return(grepl("comb.*coal", col, ignore.case = TRUE))
}

# Get the SCCs which refer to coal combustion
coal_comb_scc <- SCC[is_coal_Comb(SCC$Short.Name)|is_coal_Comb(SCC$EI.Sector),]

# Get the NEI subset which is coal combustion
coal_comb <- NEI[NEI$SCC %in% coal_comb_scc$SCC,]

# Aggregate total emissions by year
emissions_by_year <- ddply(coal_comb, .(year), summarize, total = sum(Emissions))

# Create a PNG
png(file = "plot4.png", width = 480, height = 480, units = "px", bg = "transparent")

# Plot
g <- ggplot(emissions_by_year, aes(year, total))
p <- g +
        geom_point(size = 3) +
        geom_smooth(method = "lm") +
        ggtitle("Total PM2.5 Emissions for Coal Combustion per Year") +
        ylab("Total Emissions")

print(p)

# Write result
dev.off()
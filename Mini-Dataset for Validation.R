# It is assumed that the downloaded folders are unzipped, and the required files
# are in the working directory.

# Load required packages
if (! require("readxl")) {
  install.packages("readxl")
  library("readxl")
}
#=============TCGA==============
# Read excel file, and use first row for column names
fNameTCGA <- "TCGA-OV-Final-Supp-Table-S2.1-13jan2011g.xls"
TCGAdata <- read_excel(path = fNameTCGA, skip = 1, col_names = TRUE)

# Convert to data frame
TCGAdata <- as.data.frame.list(TCGAdata, stringsAsFactors = FALSE)

# Hot and Cold data as specified by Naina
hot <- c("ADAM9","ITGAV","ITGA6","ITGA3","ITGB5",
         "LIMK1","FGFR2","DLST","UMPS","PAK4","GATAD2A")
cold <- c("C2orf65","DOK1","DQX1","LOXL3","SEMA4F")

# Find hot and cold genes from data
hotData <- TCGAdata[TCGAdata[,1] %in% hot,]
coldData <-TCGAdata[TCGAdata[,1] %in% cold,]
# View
# View(hotData)
# View(coldData)
# Append as a single data frame
TCGAmini <- rbind(hotData, coldData)
# View(TCGAmini)
saveRDS(file = "TCGA_mini", object = TCGAmini)
#=============FireHose===============
# Read from csv
fNameFire <- "all_lesions.conf_99.txt"
FireHosedata <- read.csv(file = fNameFire, sep = "\t")

# Extract the matching rows with what was specified by Naina
FireHosedatamini <- FireHosedata[FireHosedata$Amplitude.Threshold == "Actual Copy Change Given",]
# View(FireHosedata)
saveRDS(file = "FireHose_mini", object = FireHosedatamini)
#=============iRefIndex==============


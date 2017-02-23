# It is assumed that the downloaded folders are unzipped, and the required files
# are in the working directory.
# Use the file from the ID mapping exercise
# The path to the ID mapping files on my computer is:
Path <-
  "/Users/farzantaj/Desktop/Second Semester/BCB420/Project/R_Exercise-IDmapping/"
load(paste(Path, "ID2UniMap.RData", sep = ""))
load(paste(Path, "ID2symMap.RData", sep = ""))
head(ID2UniMap)
head(ID2symMap)

# Load required packages
# if (!any(unlist(lapply(
#   X = c("readr"),
#   FUN = require,
#   character.only = TRUE
# )))) {
#   install.packages("readr")
#   install.packages("stringr")
#   library("readr")
#   library("stringr")
# }
#=============TCGA==============
# Read excel file, and use first row for column names
fName1 <- "hgsc.bcm.edu_OV.SOLiD_DNASeq.Level_3.1.maf"
fName2 <- "broad.mit.edu_OV.IlluminaGA_DNASeq.Level_3.7.maf"
fName3 <- "genome.wustl.edu_OV.IlluminaGA_DNASeq.Level_3.1.maf"
TCGAdata1 <- read.table(fName1, sep = "\t", header = TRUE)
TCGAdata2 <- read.table(fName2, sep = "\t", header = TRUE)
TCGAdata3 <- read.table(fName3, sep = "\t", header = TRUE)
# Note that the WashU file has to rectified by adding a tab at the last column
# of the header
# Hot and Cold data as specified by Naina
hot <- c(
  "ADAM9",
  "ITGAV",
  "ITGA6",
  "ITGA3",
  "ITGB5",
  "LIMK1",
  "FGFR2",
  "DLST",
  "UMPS",
  "PAK4",
  "GATAD2A"
)
cold <- c("C2orf65", "DOK1", "DQX1", "LOXL3", "SEMA4F")
hotNcold <- c(hot, cold)
# Find hot and cold genes from data
Match1 <- TCGAdata1[TCGAdata1[, 1] %in% hotNcold, ]
Match2 <- TCGAdata2[TCGAdata2[, 1] %in% hotNcold, ]
Match3 <- TCGAdata3[TCGAdata3[, 1] %in% hotNcold, ]
# Append as a single data frame, diregard extra rows in Match3
TCGAmini <- rbind(Match1, Match2, Match3[, colnames(Match1)])
write.table("TCGA_mini.txt", x = TCGAmini, append = FALSE, sep = "\t")
# test <- read.delim("TCGA_mini.txt")
# View(test)
#=============FireHose===============
# Read from csv
fNameFire <- "all_lesions.conf_99.txt"
FireHosedata <- read.csv(file = fNameFire, sep = "\t")
# Extract the matching rows with what was specified by Naina
FireHosedatamini <-
  FireHosedata[FireHosedata$Amplitude.Threshold == "Actual Copy Change Given",]
write.table(file = "FireHose_mini.txt",
            FireHosedatamini,
            sep = "\t",
            append = FALSE)
# test <- read.csv(file = "FireHose_mini.txt", sep = "\t")
# View(test)
#=============iRefIndex==============
# Read from MITAB downloaded from iRefIndex, v14.0.
fNameiRef <- "9606.mitab.04072015.txt"
# Read from iRefIndex "9606" data
iRefData <- read.delim(fNameiRef, sep = '\t')
# Get ENSEMBLE IDs of hot and cold genes from Map file, then get UniProt IDs
hotNcold <- c(hot, cold, "M1AP")
MapIndices <- vector()
for (i in 1:length(hotNcold)) {
  temp <- which(ID2symMap[] == hotNcold[i])
  MapIndices <- c(MapIndices, temp)
}
# One is missing :-) M1AP == C2orf65
UniProtHotnCold <- unname(ID2UniMap[names(ID2symMap[MapIndices])])
# Find the indices at which the either of the interactors are in hotNcold
hotNcold_grep <- paste(... = UniProtHotnCold, collapse = "|")
Indices1 <-
  which(grepl(x = unlist(iRefData[, 1]), pattern = hotNcold_grep))
Indices2 <-
  which(grepl(x = unlist(iRefData[, 1]), pattern = hotNcold_grep))
myIndices <- unique(Indices1, Indices2)
# Subset for the rows that have the uniprotkb ID
iRefDataFinal <- iRefData[myIndices,]
write.table(file = "iRefIndex_mini.txt",
            iRefDataFinal,
            append = FALSE,
            sep = "\t")
# test <- read.csv(file = "iRefIndex_mini", sep = "\t")
# View(test)

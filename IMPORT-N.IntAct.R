# Read from Cytoscape's IntAct csv file and alert the user after job is 
# finished.
library("beepr")
library("igraph")
library("RpsiXML")
library("stringr")
install.packages("gridExtra")
library("gridExtra") #for creating pdf tables from dataframes

beep(2)

IMPORT-N.IntAct <- function(fName = "/Users/farzantaj/BioData/IntActEdgeTable.csv",
                            det.type = NULL, xS = NULL, xQ = NULL,
                            xN = 10000, xUd = "ALL", verbose = TRUE) {
  # Check given values for correctness.
  if (sum(is.na(c(xS, xQ, xUd))) < 2) {
    stop("Error: Only one value to either xS, xQ or xN should be given.")
  }
  if (xN > nrow(IntActData)) {
    stop("Error: xN is more than available interactions")
  }
  if (xQ && !(0 < xQ < 100)){
    stop("Error: xQ has undefined value.")
  }
  if (det.type && sum(!(det.type %in%
                        unique(IntActData$Detection.Method))) > 0) {
    stop("Error: One or more of given detection types is not in input file.")
  }
  # Read from the indicated csv file and assign to data frame, generates error
  # if file is not found.
  IntActData <- read.csv(fName)
  # Trim data frame to relevant columns. (((Remember to trim 'in place' after testing)))
  IntActDataClean <- IntActData[, c("SUID", "Confidence.Score.intact.miscore", 
                                    "Detection.Method.ID", "interaction",
                                    "Interaction.Type", "name",
                                    "shared.interaction", 
                                    "Primary.Interaction.Type",
                                    "Publication.DB", "Update.Date")]
  # If xQ or xN is given, calculate and set to Top. If 
  if (xQ) {
    Top <- quantile(IntActDataClean$Confidence.Score.intact.miscore,
                      probs = xQ)
  } else if (xN != 10000) {
    Top <- sort(IntActDataClean$Confidence.Score.intact.miscore,
                 decreasing = TRUE)[1:xN]
  } else {
    Top <- xS
  }
  # 
  if (!det.type) {
    IntActDataClean <- IntActDataClean[IntActDataClean$Detection.Method.ID
      %in% det.type && (IntActDataClean$Confidence.Score.intact.miscore > Top)]
  } else {
    IntActDataClean <- IntActDataClean[(
      IntActDataClean$Confidence.Score.intact.miscore > Top),]
  }
  IntActFinal <- IntActDataClean[]
  beep(2)
}
# From stringr package, split "name" column into two interactors.
?str_split_fixed
Interactors <- str_split_fixed(IntActDataClean$name, pattern = " \\(.*\\) ", 2)
IntActDataClean$Int1 <- Interactors[,1]
IntActDataClean$Int2 <- Interactors[,2]
# Delete "name" column.
IntActDataClean <- IntActDataClean[,!(colnames(IntActDataClean) %in% c("name"))]
head(IntActDataClean)

unique(IntActData[, "Interaction.Type"])
unique(IntActData[,"Confidence.Score.author.score"])
unique(IntActData[,"Confidence.Score.author.confidence"])
Unq <- unique(IntActDataClean[, "Interaction.Type"])
for (i in Unq) {
  cat(paste("<tt> </tt> ", "(", i, "), ", sep = ""), file = "iden.txt", append = TRUE)
}

head(IntActDataClean)
cat(as.character(head(IntActDataClean)), file = "head.txt")
ncol(IntActDataClean)
nrow(IntActDataClean)
colnames(IntActDataClean)

a <- c(1,2,3)
b <- c(1,2,3)
c <- c(1,2)
x <- data.frame(
  d = a,
  e = b,
  f = c
)
x
View(IntActData)
typeof(IntActData)
?save
save(IntActData, file = "IntActTable.csv", compress = TRUE)

nrow(IntActData)
ncol(IntActData)
colnames(IntActData)



updaIntActFrame <- xmlToDataFrame("/Users/farzantaj/BioData/IntActPSI25.xml")
IntActList <- xmlToList("/Users/farzantaj/BioData/human_01.xml")
View(IntActFrame)
View(IntActList)

biocLite("RpsiXML")
library("RpsiXML")


library("igraph")
ls("package:igraph")
read_graph("/Users/farzantaj/BioData/IntActGraphML.graphml", format = "graphml")


library("Rgraphviz")
ls("package:Rgraphviz")
ls("package:RpsiXML")

xmlDir <- "/Users/farzantaj/BioData/"
intactxml <- file.path(xmlDir, "IntActPSI25.xml")
IntActXML <- parsePsimi25Interaction(intactxml, INTACT.PSIMI25 , verbose = TRUE)
?parsePsimi25Interaction
iG <- graph_from_adjacency_matrix(IntActGraph)


IntActGraph <- psimi25XML2Graph(intactxml, INTACT.PSIMI25, type = "interaction"
                                , directed = TRUE)
typeof(IntActGraph)
plot(IntActGraph)
ls("package:RpsiXML")
validatePSIMI25("/Users/farzantaj/BioData/human_huri/human_huri-2017-1_01.xml",
                ignore.stderr = FALSE)


IntActList <- list()
for (filename in list.files(xmlDir)) {
  intactxml <- file.path(xmlDir, as.character(filename))
  IntActXML <- parsePsimi25Interaction(intactxml, INTACT.PSIMI25 , verbose = TRUE)
  temp <- interactions(IntActXML)
  # Find a way to append lists together
  c(IntActList, temp)
}
IntActGraph <- psimi25XML2Graph(intactxml, INTACT.PSIMI25,
                                type = "interaction")
?list
IntActInteractions
typeof(IntActInteractions)
?parsePsimi25Interaction
?psimi25XML2Graph
?append
?mapply


some <- xmlParse("/Users/farzantaj/BioData/human_01.xml")
xml_data <- xmlToList(some)
file.pa
library("graph")
library("iRefR")
library("stringr")
ls("package:iRefR")
?get_irefindex
get_irefindex(tax_id, iref_version, data_folder)
get_irefindex("9606", "current", "/Users/farzantaj/BioData/")
irefindex_80_human = get_irefindex()
View(irefindex_80_human)
??iRefR


###############################################################################
# igraph example
actors <- data.frame(name=c("Alice", "Bob", "Cecil", "David",
                            "Esmeralda"),
                     age=c(48,33,45,34,21),
                     gender=c("F","M","F","M","F"))
relations <- data.frame(from=c("Bob", "Cecil", "Cecil", "David",
                               "David", "Esmeralda"),
                        to=c("Alice", "Bob", "Alice", "Alice", "Bob", "Alice"),
                        same.dept=c(FALSE,FALSE,TRUE,FALSE,FALSE,TRUE),
                        friendship=c(4,5,5,2,1,1), advice=c(4,5,5,4,2,3))
g <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)
print(g, e=TRUE, v=TRUE)

## The opposite operation
as_data_frame(g, what="vertices")
as_data_frame(g, what="edges")




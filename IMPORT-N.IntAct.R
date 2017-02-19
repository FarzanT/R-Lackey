if (!require("readr")) {
  lapply(
    X = c("stringr", "readr", "igraph"),
    FUN = install.packages,
    character.only = TRUE
  )
}
lapply(
  X = c("stringr", "readr", "igraph"),
  FUN = library,
  character.only = TRUE
)

# xUd should be passed as a date object (Y-M-D)
IMPORT_N_IntAct <- function(fName,
                            det.method = NULL,
                            xS = NULL,
                            xQ = NULL,
                            xN = 10000,
                            xUd = NULL,
                            tax = 9606) {
  # Check given values for correctness.
  if (!(sum(is.null(xS), is.null(xQ)) < 2)) {
    stop("Only one value to either xS, xQ or xN should be given.")
  }
  if ((!(is.null(xQ))) && (!((0 < xQ) && (xQ < 100)))) {
    stop("xQ has undefined value.")
  }
  # Columns to extract ("-" and "_" mean "skip")
  relevant_columns <-
    c("cc_-_-c_ccccccc_-_-_-_-_-_-_-_-D_-_-_-_-_-")
  nchar(relevant_columns)
  # col_types = relevant_columns
  # Read and select relevant fields, drop the rest.
  system.time(
    LINES <-
      read_delim(
        file = fName,
        delim = "\t",
        escape_double = FALSE,
        col_names = TRUE,
        progress = interactive(),
        escape_backslash = FALSE,
        col_types = relevant_columns
      )
  )
  # After first read, these are the indices that pertain to each column
  # 1: Interactor A, 2: Interactor B, 3: Interaction Detection Method
  # 4: Publication Identifier, 5: Tax ID Interactor A, 6: Tax ID Interactor B
  # 7: Interaction Type, 8: Source Database, 9: Interaction Identifier
  # 10: Confidence Value, 11: Update Date
  
  # Find indices of rows with both interactors of specifided tax id.
  system.time(SpeciesIndices <- Reduce(intersect,
                                       list(
                                         # Tax ID Interactor A
                                         grep(x = unlist(LINES[, 5]),
                                              pattern = as.character(tax)),
                                         # Tax ID Interactor B
                                         grep(x = unlist(LINES[, 6]),
                                              pattern = as.character(tax))
                                       )))
  # Subset for species (argument on the right) and append to IntActSpecies
  LINES <- LINES[SpeciesIndices,]
  # Remove large integer
  rm(SpeciesIndices)
  
  # Extract and in-place replace "Confidence values" with the double itself.
  system.time(LINES[, 10] <-
                as.numeric(unlist(
                  str_extract_all(string = unlist(LINES[, 10]),
                                  pattern = "\\d\\.\\d\\d$")
                )))
  # If xQ or xN is given, calculate and set to Top. If not, set xS as top.
  if (!is.null(xQ)) {
    Top <- quantile(unlist(LINES[, 10]),
                    probs = xQ)
  } else if (!is.null(xS)) {
    # Confidence score more than xS
    Top <- xS
  } else {
    # Top N confidence scores, set to NULL for later computation
    Top <- NULL
  }
  
  # Extract and in-place replace "interaction type" IDs, save for use.
  system.time(LINES[, 7] <-
                (as.character(
                  cur_interaction_types <-
                    str_extract_all(unlist(LINES[, 7]),
                                    pattern = "[A-Z]{2}:[:digit:]{4}")
                )))
  # Extract and in-place replace "Interaction detection methods", save for use
  system.time(LINES[, 3] <-
                (as.character(
                  cur_det_types <- str_extract_all(unlist(LINES[, 3]),
                                                   pattern = "[A-Z]{2}:[:digit:]{4}")
                )))
  # Extract and in-place replace "Interactor IDs"
  system.time(LINES[, 1] <-
                as.character(str_extract_all(unlist(LINES[, 1]),
                                             pattern = "[A-Z][:alnum:]+")))
  system.time(LINES[, 2] <-
                as.character(str_extract_all(unlist(LINES[, 2]),
                                             pattern = "[A-Z][:alnum:]+")))
  # If det.method is not specified,
  if (is.null(det.method)) {
    # Use all available detection types
    # Find top N values, if Top is null
    if (is.null(Top)) {
      sort(as.vector(unlist(LINES[, 10])),
           index.return = TRUE,
           decreasing = TRUE)
      Top <- Top$ix[1:xN]
      # If update date is not specifed
      if (is.null(xUd)) {
        final_indices <- Top
      } else {
        final_indices <-
          Reduce(intersect, list(Top, unname(which(
            unlist(LINES[, 11]) > xUd
          ))))
      }
      FINAL <- LINES[final_indices,]
    } else {
      # When Top is not null (some other parameter was specified)
      if (is.null(xUd)) {
        FINAL <- LINES[(LINES[, 10] > Top),]
      } else {
        # Same trick as before
        FINAL <-
          LINES[Reduce(intersect, list(which(LINES[, 10] > Top),
                                       unname(which(
                                         unlist(LINES[, 11]) > xUd
                                       )))),]
      }
    }
  } else {
    # det.method is given, get the unique detection methods
    unq_det_types <- unlist(unique(cur_det_types))
    # Match the with ones given by the user
    matching_det_methods <-
      Reduce(intersect, list(unq_det_types, det.method))
    # Prepare for grep, by adding pipe | symbol to pattern
    match_det_grep <-
      paste(matching_det_methods, collapse = "|")
    # Find mathcing detection method indices
    match_det_indices <-
      grep(pattern = match_det_grep, x = unlist(LINES[, 3]))
    # Find top N values, if Top is null
    if (is.null(Top)) {
      sort(as.vector(unlist(LINES[match_det_indices, 10])),
           index.return = TRUE,
           decreasing = TRUE)
      Top <- Top$ix[1:xN]
      if (is.null(xUd)) {
        final_indices <- Top
      } else {
        final_indices <-
          Reduce(intersect, list(Top, unname(which(
            unlist(LINES[, 11]) > xUd
          ))))
      }
      FINAL <- LINES[final_indices, ]
    } else {
      # When Top is not null (some other parameter was specified)
      if (is.null(xUd)) {
        final_indices <-
          Reduce(intersect, list(match_det_indices,
                                 which((LINES[, 10]) > Top)))
        FINAL <- LINES[final_indices,]
      } else {
        final_indices <-
          Reduce(intersect,
                 list(match_det_indices,
                      which((LINES[, 10]) > Top)),
                 unname(which(unlist(LINES[, 11]) > xUd)))
        FINAL <- LINES[final_indices,]
      }
    }
  }
  gG <- graph_from_data_frame(d = FINAL, directed = TRUE)
  saveRDS(object = gG, file = "IntAct_Gene_Graph")
}

# Test
det.method <- c("MI:0084",
                "MI:0081",
                "MI:0055",
                "MI:0096",
                "MI:0411",
                "MI:0049")
myDate <- as.Date("2013-6-7")
system.time(IMPORT_N_IntAct(
  fName = "intact.txt",
  det.method = det.method,
  xQ = 0.75,
  xUd = myDate
))
graph <- readRDS(file = "IntAct_Gene_Graph")
gXY <- layout_with_fr(graph = graph,
                      dim = 3)

oPar <- par(mar = rep(0, 4)) # Turn margins off
plot(
  graph,
  layout = gXY,
  rescale = FALSE,
  xlim = c(min(gXY[, 1]), max(gXY[, 1])) * 1.1,
  ylim = c(min(gXY[, 2]), max(gXY[, 2])) * 1.1,
  vertex.color = heat.colors(max(degree(graph) + 1))[degree(graph) +
                                                       1],
  vertex.size = 30 + (1 * degree(graph)),
  vertex.label = "",
  edge.arrow.size = 0
)
par(oPar)

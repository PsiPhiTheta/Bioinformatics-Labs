# Thomas Hollis (BCH441, University of Toronto) -v9.3
#     - Purpose: generate overview plot of a full-length alignment in one image
#     - Bugs & issues: no bugs, no issues, no warnings
#     - Acknowledgements: thanks to Prof. Steipe's learning unit on R MSA which was of great help

###### Section 0. Excluded code used to call the function while testing ######
if (FALSE) {
  source(file = "BIN-ALI-MSA.R")
  fullAliPlot(msaT)
  fullAliPlot(msaT, lCol = "firebrick", fCol = "black", colGrad = FALSE)
  fullAliPlot(msaT, lCol = "black", fCol = "orange", colGrad = TRUE)
  fullAliPlot(msaW, lCol = "lightgrey", fCol = "skyblue", colGrad = FALSE)
  fullAliPlot(msaW, lCol = "firebrick", fCol = "black", colGrad = FALSE)
  fullAliPlot(msaW, lCol = "black", fCol = "orange", colGrad = TRUE)
}

###### Section 1. Full alignment plot function ######
fullAliPlot <- function(MsaAAMultipleAlignment, lCol = "lightgrey", fCol = "skyblue", colGrad = FALSE) {

  # 1.1 Copy the input object to local data object to avoid accidental modifications
  data <- MsaAAMultipleAlignment

  # 1.2 Create the background plot
  lenSeq <- nchar(data) # get the sequence length (here: 1269)
  xAxis <- c(1,lenSeq) # create the x axis
  numSeq <- length(data@unmasked) # get the number of sequences being aligned (here: 11)
  yAxis <- c(1,numSeq) # create the y axis
  plot(x = xAxis, y = yAxis, type = "n", ylab = "Sequences", xlab = "Position", main = "Full MSA Plot") # plot

  # 1.3 Pre-compute and create the colour palette
  if (colGrad) { # if colour gradient option is chosen, create the score colour palette
    aliScore <- msaConservationScore(data, substitutionMatrix = BLOSUM62)
    lev <- cut(aliScore, labels = FALSE, breaks = 10)
    myPal <- colorRampPalette(c("lightgrey", "firebrick")) #colour from grey to red
  } else { # else use the fCol to fill the rectangle
    myCol <- fCol
  }

  # 1.4 Add the rectangles and lines to the plot
  for (i in 1:numSeq) { # run through all lines
    currSeq <- unlist(strsplit(as.character(data@unmasked[i]),"")) # split current line to vector of char

    segRLE <- rle(currSeq == "-") # I forgot about the existance of RLE & spent 3h trying to implement it manually.
    segRLEidx <- cumsum(c(1,segRLE$lengths)) # Eventually gave up & checked Alana Man's Journal which saved me here!
    # Citation: Alan Man, available at: steipe.biochemistry.utoronto.ca/abc/students/index.php/User:Alana_Man

    for (j in 1:length(segRLE$lengths)) { # run through all positions in current line
      if (segRLE$values[j]) { # returns true only for locations with segments
        segments(segRLEidx[j], i, (segRLEidx[j]+segRLE$lengths[j]), i, col = lCol) # plot segments
        } else { # only runs for locations with rectangles
        if (colGrad) { # if colour gradient option is chosen, set the colour
          currScore <- ceiling(mean(lev[segRLEidx[j]:(segRLEidx[j]+segRLE$lengths[j])])) # ceiling for more vibrancy
          myCol <- myPal(10)[currScore] # pick the colour from the colour palette
        }
        rect(segRLEidx[j], i-0.1, (segRLEidx[j]+segRLE$lengths[j]), i+0.1, col = myCol, border = lCol) # plot rect
      }
    }
  }

  # 1.5 Add labels to the plot
  labels <- names(data@unmasked)
  for (k in 1:numSeq) { # run through all lines
    text(floor(lenSeq/2), (k+0.25), labels = labels[k]) # place labels in middle
  }
}

# END

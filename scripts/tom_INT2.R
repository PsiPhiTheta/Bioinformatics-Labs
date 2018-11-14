# Thomas Hollis (BCH441, University of Toronto) -v2.1
#     - Purpose: Phylogeny Oral Evaluation (build & analyse aphylogenetic tree)
#     - Bugs & issues: no bugs, no issues, no warnings
#     - Acknowledgements: thanks to Prof. Steipe's learning units on Phylogeny which were of great help

###### 1. Load packages and data ######
init()

if (! require(msa, quietly=TRUE)) {
  if (! exists("biocLite")) {
    source("https://bioconductor.org/biocLite.R")
  }
  biocLite("msa")
  library(msa)
}

if (! require(Biostrings, quietly=TRUE)) {
  if (! exists("biocLite")) {
    source("https://bioconductor.org/biocLite.R")
  }
  biocLite("Biostrings")
  library(Biostrings)
}

if (!require(Rphylip, quietly=TRUE)) {
  install.packages("Rphylip")
  library(Rphylip)
}

if (!require(phangorn, quietly=TRUE)) {
  install.packages("phangorn")
  library(phangorn)
}

source("makeProteinDB.R")

######  2. Produce the phylogenetic tree ######
  # 2.1 Data Preparation
    # 2.1.1 Align all sequences in the database + KILA_ESSCO (outgroup, for rooting the tree)
    mySeq <- myDB$protein$sequence # add all DB sequences
    names(mySeq) <- myDB$protein$name # add names of all DB sequences

    mySeq <- c(mySeq,
               "IDGEIIHLRAKDGYINATSMCRTAGKLLSDYTRLKTTQEFFDELSRDMGIPISELIQSFKGGRPENQGTWVHPDIAINLAQ")
    names(mySeq)[length(mySeq)] <- "KILA_ESCCO" # add KILLA_ESSCO sequence and name

    (mySeqMSA <- msaClustalOmega(AAStringSet(mySeq))) # ClustalOmega used, too many seq for MUSCLE

    # 2.1.2 Get the sequence of the SACCE APSES domain ######
    sel <- myDB$protein$name == "MBP1_SACCE" # selector for SACCE protein
    proID <- myDB$protein$ID[sel] # store SACCE protein in proID for later

    sel <- myDB$feature$ID[myDB$feature$name == "APSES fold"] # selector for APSES
    fanID <- myDB$annotation$ID[myDB$annotation$proteinID == proID &
                                  myDB$annotation$featureID == sel] # SACCE & APSES
    (start <- myDB$annotation$start[fanID]) # set start point (4)
    (end   <- myDB$annotation$end[fanID]) # set the end point (102)

    (SACCEapses <- substring(myDB$protein$sequence[proID], start, end)) #SACCE APSES seq

    # 2.1.3 Extract the APSES domains from the MSA
    (APSESmsa <- fetchMSAmotif(mySeqMSA, SACCEapses)) # stored in .utilities.R, returns matching seq
    # note this includes PSI-BLAST results which can be found in MYSPE_APSES_PSI-BLAST.json
    # note APSESmsa is of type "AAStringSet" not "MsaAAMultipleAlignment"

    # 2.1.4 Process the APSESmsa for Tree Building
    (numAli <- length(names(APSESmsa))) # get the number of alignments (52)
    (lenAli <- length(APSESmsa$MBP1_SACCE)) # get the length of the alignments (269)

    msaMatrix <- matrix(nrow = numAli, ncol = lenAli) # initialize matrix to hold all chars
    (rownames(msaMatrix) <- APSESmsa@ranges@NAMES) # assign the correct rownames
    for (i in 1:numAli) {
      msaMatrix[i, ] <- unlist(strsplit(as.character(APSESmsa[i]), "")) # fill matrix
    }
    msaMatrix[1:7, 1:14] # check result is a well defined and formatted matrix

    colMask <- logical(lenAli) # initialize a mask (logical vector to trim cols)
    limit <- round(numAli * (2/3)) # define the 2/3 threshold for rejecting a column
    for (i in 1:ncol(msaMatrix)) {  # iterate over all columns
      count <- sum(msaMatrix[ , i] == "-") # count hyphens in col
      colMask[i] <- count <= limit # write TRUE if less-or-equal to limit, FALSE if not
    }
    colMask # check mask makes sense
    sum(colMask) # check how many collumns are kept (should be 101)
    cat(sprintf("Masking %4.2f %% of columns.\n", 100*(1 - (sum(colMask)/length(colMask)) )))
    maskedMatrix <- msaMatrix[ , colMask] # remove masked cols
    ncol(maskedMatrix) # check how many cols left (should be 101)

    tomAPSESphyloSet <- character() # initialise char object, namechange to avoid confusion
    for (i in 1:nrow(maskedMatrix)) { # collapse back to string
      tomAPSESphyloSet[i] <- paste(maskedMatrix[i, ], collapse="")
    }
    names(tomAPSESphyloSet) <- rownames(maskedMatrix) # add names

    writeALN(tomAPSESphyloSet) # inspect final result
    writeMFA(tomAPSESphyloSet, myCon="tom_APSESphyloSet.mfa") # save aligned & masked to multi-FASTA

  # 2.2 Tree Building
  PROMLPATH <- "/Users/tom/Applications/phylip-3.695/exe/proml.app/Contents/MacOS" # set PROMLPATH
  list.dirs(PROMLPATH) # confirm one directory is listed only
  list.files(PROMLPATH) # confirm two files listed only "proml" and "proml.command"
  apsIn <- read.protein("tom_APSESphyloSet.mfa") # import aligned & masked from multi-FASTA
  myTree <- Rproml(apsIn, path=PROMLPATH) # run PROML to build tree (warning: 4-8h) start:6pm,end:?pm
  plot(myTree) # have a quick look before further analysis
  save(myTree, file = "tom_myTreeRproml.RData") # save locally to RData file

  # 2.3 Tree Analysis

    # 2.3.1 A few quick exploratory views of the tree
    load(file = "tom_myTreeRproml.RData")
    plot(myTree) # default type: "phylogram"
    plot(myTree, type = "unrooted") # unrooted type
    plot(myTree, type = "fan", no.margin = TRUE) # concentric circle type

    str(myTree) # inspect the tree object
    myTree$tip.label # inspect the tree object
    myTree$edge # inspect the tree object
    myTree$edge.length # inspect the tree object

    plot(myTree) # plot the tree with labels
    tiplabels(cex = 0.5, frame = "rect") # useful to find the outgroup (should be 17 here)
    edgelabels(cex = 0.5) # add edge labels - looks messy, not very useful here
    nodelabels(cex = 0.5, frame = "circle") # add node labels - looks messy, not very useful here

    Nnode(myTree) # number of nodes (should be 50)
    Nedge(myTree) # number of edges (should be 101)
    Ntip(myTree) # number of tips (should be 52)

    write.tree(myTree) # show the tree in console in Newick format

    # 2.3.2 Rearrange the tree & root it with the outgroup
    myTree <- root(myTree, outgroup = 17, resolve.root = TRUE) # change this for outgroup number
    plot(myTree) # check it out
    is.rooted(myTree) # make sure the tree is now rooted

    myTree$edge.length[1] <- 0.1 # rescale the tree to a reasonable size
    plot(myTree, cex = 0.7) # check it out

    myTree$root.edge <- mean(myTree$edge.length) * 1.5 # make the root visable
    plot(myTree, cex = 0.7, root.edge = TRUE) # check it out

    # 2.3.3 Rotate the clades (useful to compare with cladogram of species)
    nodelabels(cex = 0.5, frame = "circle") # add node labels - looks messy, not very useful here
    myTree <- rotate(myTree, node = 100)
    myTree <- rotate(myTree, node = 54)
    myTree <- rotate(myTree, node = 60)
    myTree <- rotate(myTree, node = 61)
    myTree <- rotate(myTree, node = 62)
    myTree <- rotate(myTree, node = 77)
    myTree <- rotate(myTree, node = 74)
    myTree <- rotate(myTree, node = 69)
    myTree <- rotate(myTree, node = 70)
    myTree <- rotate(myTree, node = 71)
    myTree <- rotate(myTree, node = 98)
    myTree <- rotate(myTree, node = 96)
    myTree <- rotate(myTree, node = 95)
    myTree <- rotate(myTree, node = 81)
    myTree <- rotate(myTree, node = 64)
    myTree <- rotate(myTree, node = 58)
    myTree <- rotate(myTree, node = 80)
    myTree <- rotate(myTree, node = 84)
    myTree <- rotate(myTree, node = 93)
    myTree <- rotate(myTree, node = 88)
    myTree <- rotate(myTree, node = 99) # beautify
    plot(myTree, cex = 0.7, root.edge = TRUE)

    # 2.3.4 Computing Tree Distance with reference tree
    # This is bonus tree analysis and was omitted here as it is not included in instructions.

# END

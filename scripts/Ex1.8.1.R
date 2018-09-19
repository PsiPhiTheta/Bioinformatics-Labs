pACYC184 <- list(size="4245", marker="Tet, Cam", ori="pUC19")
pUC19 <- list(size=2686, marker="ampicillin", ori="ColE1", accession="L01397", BanI=c(235, 408, 550, 1647) )
plasmidDB <- list()
plasmidDB[["pUC19"]] <- pUC19
plasmidDB[["pACYC184"]] <- pACYC184
pBR322 <- list(size=4361, marker="Amp, Tet", ori="ColE1")
plasmidDB[["pBR322"]] <- pBR322

plasmidDB[["pACYC184"]]

#Now we must retrieve all sizes from the list and find the smallest
lapply(plasmidDB, function(x) {return(x$ori)})
x <- unlist(lapply(plasmidDB, function(x) {return(x$size)}))
min(x)

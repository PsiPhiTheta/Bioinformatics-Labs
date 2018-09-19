LPSdat[1:10, 1:2] #first ten rows of first two columns
LPSdat[10:1, 1:2] #first ten rows backward of first two columns

LPSdat[sample(1:10), 1:2] #first ten rows in random order of first two columns
LPSdat[order(LPSdat[1:10,2]),1:2] #first ten rows of first two columns ordered by value in the second column ascending
LPSdat$Mo.LPS[1:10] #first ten rows of colum named Mo.LPS

LPSdat[1:10, c("Mo.LPS","Mo.ctrl")] #first ten rows of column Mo.LPS and Mo.ctrl

LPSdat$genes[nchar(LPSdat$genes)==3] #all genes with gene name over three characters long
LPSdat[grep("Il",LPSdat$genes), 1:2] #first two columns of all genes that have the string "Il" inside their name

LPSdat[(LPSdat$B.LPS - LPSdat$B.ctrl) > 2,] #all genes for which B-cells are stimulated by LPS by more than 2 log units

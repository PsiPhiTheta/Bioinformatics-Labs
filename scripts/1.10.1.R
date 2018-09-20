cnt <- 3 #numeric count
txt <- as.character(cnt) #character count
while (cnt > -1) { #break condition
    txt <- c(txt, cnt) #append new character to count
    cnt = cnt - 1 #decrement
}
(txt <- c(txt, "Lift Off!")) #append final string & print

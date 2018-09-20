cnt <- 3 #numeric count
txt <- as.character(cnt) #character count
while (cnt > 0) { #break condition
    cnt = cnt - 1 #decrement
    txt <- c(txt, cnt) #append new character to count
}
(txt <- c(txt, "Lift Off!")) #append final string & print

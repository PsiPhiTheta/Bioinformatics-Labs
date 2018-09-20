numbers <- c(16, 20, 3, 5, 9)
numbers

lastNum <- tail(numbers, 1)    
lastNum

if (lastNum < 6 | lastNum > 10) {
  (bool1 <- 1)
} else {
  (bool1 <- 2)
}

if (lastNum >= 10 & lastNum < 20) {
  (bool2 <- 1)
} else {
  (bool2 <- 2)
}

lastNum <- (lastNum/7) - ((((lastNum/7)*10)%/%1)/10)

lastNum <- lastNum*100
lastNum <- lastNum %/% 1
lastNum <- lastNum ^ (1/3)

if (lastNum == 2) {
  (bool3 <- 1)
} else {
  (bool3 <- 2)
}

countDown <- function(n) {
    cnt <- n #numeric count
    txt <- as.character(cnt) #character count
    while (cnt > 0) { #break condition
        cnt = cnt - 1 #decrement
        txt <- c(txt, cnt) #append new character to count
    }
    return (txt <- c(txt, "Lift Off!")) #append final string & print
}

myLifeDays <- function(birthday, reverse) {
    if (missing(birthday)) {
        if (missing(reverse)) { #if no arguments are provided
            print ("Enter your birthday as a string in \"YYYY-MM-DD\" format and/or the days as an integer.")
            return()
        }
    }

    if (missing(reverse)) { #if only the birthday argument is provided
        bd <- strptime(birthday, "%Y-%m-%d") # convert string to time
        now <- format(Sys.time(), "%Y-%m-%d") # convert "now" to time
        diff <- round(as.numeric(difftime(now, bd, unit="days")))
        print(sprintf("This date was %d days ago.", diff))
    } else if (missing(birthday)) { #if only the reverse argument is provided
        now <- format(Sys.time(), "%Y-%m-%d")
        print(as.Date(now) + reverse)
    } else { #if both arguments are provided
        bd <- strptime(birthday, "%Y-%m-%d") # convert string to time
        now <- format(Sys.time(), "%Y-%m-%d") # convert "now" to time
        diff <- round(as.numeric(difftime(now, bd, unit="days")))
        print(sprintf("This date was %d days ago.", diff))

        now <- format(Sys.time(), "%Y-%m-%d")
        print(as.Date(now) + reverse)
    }
}

myLifeDays("1932-09-25", 3)
myLifeDays("1932-09-25")
myLifeDays(,3)
myLifeDays(r=3,b="1970-01-01")

countDown(10)

seq(-5, 3) #sequence of integers from -5 to 3
seq(from = -2, to = 2, by = (1/3)) #sequence of numbers from -2 to 2 in intervals of 1/3
seq(length.out=30, to=100, from=1) #sequence of 30 numbers between 1 and 100. Pass the arguments in the following order: length.out, to, from.




# Thomas Hollis (BCH441, University of Toronto) -v2.1 
#     - Purpose: Add an entry to a database and output information in a desired format
#     - Bugs & issues: no bugs, no issues, no warnings
 
# --- SECTION 1: Add entry to database -----------------
#    Write and submit code that adds another philosopher to the datamodel:
#       Immanuel Kant, (1724 - 1804), Enlightenment Philosophy.
#       Works: Critique of Pure Reason (1781), Critique of Judgement (1790)
 
tempPersonID <- autoincrement(philDB$person)
temp <- data.frame(id = tempPersonID,
                  name = "Immanuel Kant",
                  born = "1724",
                  died = "1804",
                  school = "Enlightenment Philosophy",
                  stringsAsFactors = FALSE)
philDB$person <- rbind(philDB$person, temp)
 
rm(temp) #I like my code like I like my room: clean and tidy :)
 
tempBookID1 <- autoincrement(philDB$books)
temp <- data.frame(id = tempBookID1,
                  title = "Critique of Pure Reason",
                  published = "1781",
                  stringsAsFactors = FALSE)
philDB$books <- rbind(philDB$books, temp)
 
rm(temp) #I like my code like I like my room: clean and tidy :)
 
tempBookID2 <- autoincrement(philDB$books)
temp <- data.frame(id = tempBookID2,
                   title = "Critique of Judgement",
                   published = "1790",
                   stringsAsFactors = FALSE)
philDB$books <- rbind(philDB$books, temp)
 
rm(temp) #I like my code like I like my room: clean and tidy :)
 
temp <- data.frame(id = autoincrement(philDB$works),
                  personID = tempPersonID,
                  bookID = tempBookID1,
                  stringsAsFactors = FALSE)
philDB$works <- rbind(philDB$works, temp)
 
rm(temp) #I like my code like I like my room: clean and tidy :)
 
temp <- data.frame(id = autoincrement(philDB$works),
                   personID = tempPersonID,
                   bookID = tempBookID2,
                   stringsAsFactors = FALSE)
philDB$works <- rbind(philDB$works, temp)
 
rm(temp) #I like my code like I like my room: clean and tidy :)
rm(tempPersonID) #I like my code like I like my room: clean and tidy :)
rm(tempBookID1) #I like my code like I like my room: clean and tidy :)
rm(tempBookID2) #I like my code like I like my room: clean and tidy :)
 
# --- SECTION 2: Output information in desired format -----------------
#    Write and submit code that lists the books in alphabetical order,
#    followed by the author and the year of publishing. Format your output like:
#    "Analects" - Kongzi (220 BCE)
#    Show the result.
 
sel <- order(philDB$books$title)
pID <- philDB$books$id[sel]
sel <- numeric()
for (ID in pID) {
  sel <- philDB$works$personID[which(philDB$works$bookID == ID)]
  cat(sprintf("\"%s\" - %s (%s)", philDB$books$title[ID], philDB$person$name[sel], philDB$books$published[ID]))
  cat("\n")
}
 
rm(sel) #I like my code like I like my room: clean and tidy :)
rm(pID) #I like my code like I like my room: clean and tidy :)
rm(ID) #I like my code like I like my room: clean and tidy :)
 
#   The output of this is:
#   "Analects" - Kongzi (220 BCE)
#   "Being and Time" - Martin Heidegger (1927)
#   "Critique of Judgement" - Immanuel Kant (1790)
#   "Critique of Pure Reason" - Immanuel Kant (1781)
#   "Daodejing" - Laozi (530 BCE)
#   "On the Way to Language" - Martin Heidegger (1959)
#   "Zhuangzi" - Zhuangzi (300 BCE)
 
# END
library(stringr)
library(ggplot2)
library(tm)
library(tidyr)



###### CREATE DATA FRAME WITH INFO ON EACH EMAIL

# Read CSV file with email file paths
dir <- "/Users/mhinckley/Desktop/maildir/"
setwd(dir)
dirs.df <- read.csv("all_email_directories.csv")


# Clean the dirs that have "/NA" at end
dirs.df$dirs <- as.character(dirs.df$dirs)
dirs.df$last <- substr(dirs.df$dirs, nchar(dirs.df$dirs)-2, nchar(dirs.df$dirs))
dir.errors <- which(dirs.df$last == "/NA")
dirs.df$clean <- dirs.df$dirs
dirs.df$clean[dir.errors] <- substr(dirs.df$dirs[dir.errors], 1, nchar(dirs.df$dirs)-2)

# dirs is list of all email directories, grab sample to play with
set.seed(3)
dirs <- as.vector(dirs.df$clean)
samp.dirs <- sample(dirs, size = 20000)

# Set data frame
df <- data.frame("dir" = c(NA),
                 "date" = c(NA),
                 "weekday" = c(NA),
                 "from" = c(NA), 
                 "to" = c(NA), 
                 "to_list" = c(NA),
                 "cc" = c(NA),
                 "all_recipients" = c(NA),
                 "subject" = c(NA), 
                 "body" = c(NA))

# Add Directory, Date, From, To, Subject, Body 
for (a in 1:length(samp.dirs)){
  df[a, c("dir", "date", "weekday", "from", "to", "to_list", "cc", "all_recipients", "subject", "body")] <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
  
  try(msg <- scan(samp.dirs[a], what = "character", sep = "\n"))
  
      # extract the email's Directory
      try(df$dir[a] <- samp.dirs[a])
      
      # extract the Date
      try(date_row <- which(!is.na(str_locate(msg, fixed("Date: "))[,1])))
      try(date <- str_sub(msg[date_row], start = 7, end = nchar(msg[date_row])))
      try(df$date[a] <- date)
      
      # extract the From
      try(from_row <- min(which(!is.na(str_locate(msg, fixed("From: "))[,1]))))
      try(from <- str_sub(msg[from_row], start = 7, end = nchar(msg[from_row])))
      try(df$from[a] <- from)
      
      # extract the To
      try(to_row <- min(which(!is.na(str_locate(msg, fixed("To: "))[,1]))))
      try(to <- str_sub(msg[to_row], start = 5, end = nchar(msg[to_row])))
      try(df$to[a] <- to)
      
      # extract the Subject
      try(subject_row <- min(which(!is.na(str_locate(msg, fixed("Subject: "))[,1]))))
      try(subject <- str_sub(msg[subject_row], start = 10, end = nchar(msg[subject_row])))
      try(df$subject[a] <- subject)
  
}





### Clean Dates

# Convert date from chars to dates (in terms of Pacific Times)
clean_dates <- data.frame("date"= as.POSIXct(c(rep(NA, length(df$date))), tz = "US/Pacific")
                          , "weekday"=c(rep(NA, length(df$date))))

for (a in 1:length(df$date)){
  # Clean date and add to clean_dates
  date_starter <- str_locate(df$date[a], fixed(" "))
  date_cut <- str_sub(df$date[a], start = date_starter[,2] + 1)
  date_starter <- str_locate(date_cut, fixed(" -"))
  date_cut <- str_sub(date_cut, end = date_starter[,2] - 2)
  clean_dates$date[a] <- strptime(date_cut, format = "%d %b %Y %H:%M:%S", tz = "US/Eastern")
  
  # Add weekday to clean_dates
  weekday_cut <- str_sub(df$date[a], end = 3)
  clean_dates$weekday[a] <- weekday_cut
}

df$date <- clean_dates$date
df$weekday <- as.factor(clean_dates$weekday)


### Clean the To column

# Make the To's a Nested List
df$to_list <- strsplit(df$to, ", ")



### Extract the Body column

df.body <- rep(NA, length(df$dir))
df.cc <- rep(NA, length(df$dir))
for (a in 1:length(samp.dirs)){
    whole.email <- scan(samp.dirs[a], what = "character", sep = "\n")
    
    ### Extract Body data
    row.pos <- str_locate(whole.email, fixed("X-FileName")) # locate the line that starts with "X-FileName"
    row.vec <- row.pos[,1] # create vector to identify row with "X-FileName"  
    if ((sum(row.vec, na.rm = TRUE)) > 1) {
      print("ERROR, too row with 'X-FileName' many matches")
    }
    body.start <- which(row.vec == 1) + 1 # Body of email starts one row after the X-FileName
    
    # Search for signs of Reply or Forward message so we can omit those words
    body.end <- min(
      c(  length(whole.email)
          , which(!is.na(str_locate(whole.email, fixed("-Original Message-"))[,1]))
          , which(!is.na(str_locate(whole.email, fixed("--Forwarded by"))[,1]))
          , which(!is.na(str_locate(whole.email, fixed("- Forwarded by"))[,1]))
          , which(!is.na(str_locate(whole.email, fixed("____________________"))[,1]))
          , which(!is.na(str_locate(str_sub(whole.email, start = 1, end = 2), fixed(">"))[,1]))
      )
    )
    try(df$body[a] <- list(whole.email[body.start:body.end]))
    
}



### Extract the CC data

for (a in 1:length(samp.dirs)){
  whole.email <- scan(samp.dirs[a], what = "character", sep = "\n")
  xcc.row <- which(str_locate(whole.email, fixed("X-cc: "))[,2] == 6) # Which row starts with "X-cc: " 
  nchar.xcc.row <- nchar(whole.email[xcc.row]) 
  # If there is any information on the X-cc field other than the label (6 characters), then retrieve Bcc info
  try(if (nchar.xcc.row <= 6) {
    try(df$cc[a] <- NA)
  } else {
    row.pos.start <- str_locate(whole.email, fixed("Cc: ")) # locate the line that starts with "Cc: "
    row.vec.start <- row.pos.start[,1]  # create vector to identify row with "X-FileName" 
    if ((sum(row.vec.start, na.rm = TRUE)) > 1) {
      print("ERROR, too row with 'Cc: ' many matches")
    }
    cc.row.start <- which(row.vec.start == 1) # Cc starts here
    try(if (length(cc.row.start) == 0) {
      df$cc[a] <- NA
    } else {
      row.pos.end <- str_locate(whole.email, fixed("Mime-"))
      row.vec.end <- row.pos.end[,1]
      if ((sum(row.vec.end, na.rm = TRUE)) > 1) {
        print("ERROR, too row with 'Mime-Version:' many matches")
      }
      cc.row.end <- which(row.vec.end == 1) - 1 # Cc ends here
      first.cc.row <- str_sub(whole.email[cc.row.start], start = 5)
      if (cc.row.start == cc.row.end) {
        try(df$cc[a] <- strsplit(first.cc.row, ", "))
      } else {
        other.cc.rows <- str_sub(whole.email[(cc.row.start+1):cc.row.end], start = 2)
        all.cc.rows <- paste(c(first.cc.row, other.cc.rows), collapse = '')
        try(df$cc[a] <- strsplit(all.cc.rows, ", "))
      }
    })
  })
}

### Create All Recipients column
for (i in 1:length(df$dir)){
  unlisted_to <- unlist(df$to_list[i])
  unlisted_cc <- unlist(df$cc[i])
  unlisted_all <- c(unlisted_to, unlisted_cc)
  unlisted_all <- unlisted_all[!is.na(unlisted_all)]
  df$all_recipients[i] <- list(unlisted_all)
}

# Sent by Enron?
df$from_enron <- !is.na(str_locate(df$from, "@enron.com")[,1])

# Sent to Enron?
df$to_enron <- !is.na(str_locate(df$all_recipients, "enron.com")[,1])


# Is it a fwd?
all_fwds <- c(  which(!is.na(str_locate(df$subject, "FW:")[,1]))
              , which(!is.na(str_locate(df$subject, "Fw:")[,1]))
              , which(!is.na(str_locate(df$subject, "Fwd:")[,1]))
)
df$fwd <- as.logical(rep(FALSE, length(df$dir)))
df$fwd[all_fwds] <- TRUE

# Is it a reply?
all_re <- c(  which(!is.na(str_locate(df$subject, "RE:")[,1]))
                , which(!is.na(str_locate(df$subject, "Re:")[,1]))
                , which(!is.na(str_locate(df$subject, "re:")[,1]))
                , which(!is.na(str_locate(df$subject, "Reply:")[,1]))
)
df$re <- as.logical(rep(FALSE, length(df$dir)))
df$re[all_re] <- TRUE



# Clean dates - there is at least one that was an error
date_errors_low <- which(df$date <= "1997-10-01 01:01:02 EST") # Data set should be from 1998 to 2002
date_errors_high <- which(df$date >= "2003-10-01 01:01:02 EST") 
date_errors_all <- c(date_errors_low, date_errors_high)  
df <- df[-date_errors_all,]




### Other error checking

# Emails that aren't to or from an Enron email
which((df$from_enron + df$to_enron) == 0) 

# Eliminate duplicates
dupe.df <- data.frame(
    'body_dupes' = duplicated(df$body)
  , 'all_recipient_dupes' = duplicated(df$all_recipients)
  , 'from_dupes' = duplicated(df$from)
  , 'date_dupes' = duplicated(df$date)
)
dupe.df$total <- dupe.df$body_dupes + dupe.df$all_recipient_dupes + dupe.df$from_dupes + dupe.df$date_dupes
df <- df[-which(dupe.df$total >= 3),] # eliminate emails that match on 3 or more criteria

# Eliminate emails without a To
df <- df[-which(df$to == ""),]
dim(df)





###### Word Counts

sent.by.en <- df[which(df$from_enron == 1 & df$to_enron == 1),]  # Look at only emails sent by Enron
sent.by.en <- sent.by.en[order(sent.by.en$date),]
lowers <- tolower(unlist(sent.by.en$body))  # Convert to lower cases
word.v <- unlist(strsplit(lowers, "\\W"))  # Using this  regex, strsplit detects word boundaries.  Then convert to vector.
not.blanks.v <- which(word.v != "")
word.v <- word.v[not.blanks.v]
# Remove stopwords like the, and, etc
word.v <- removeWords(word.v, stopwords("english"))
word.v <- removeWords(word.v, c("20", "forwarded")) #random other words to remove
word.v <- word.v[which(word.v != "")]
#
total.word <- length(word.v)


freqs.t <- table(word.v)
freqs.t
sorted.freqs.t <- sort(freqs.t, decreasing=TRUE)

# His vs. her words
sorted.freqs.t["him"] / sorted.freqs.t["her"] # Frequency of he vs. she and him vs. her 
sorted.freqs.t["he"] / sorted.freqs.t["she"]   

# Relative frequency
sorted.rel.freq.t <- 100 * sorted.freqs.t / total.word
plot(sorted.rel.freq.t[1:20], type="b", xlab="Top Words", ylab="Percentage of Full Corpus", xaxt="n")
axis(1,1:20, labels=names(sorted.rel.freq.t[1:20]))


### Dispersion plots

# Rule
n.time.v <- seq(1:length(word.v))
w.count.v <- rep(NA,length(n.time.v))
rule.v <- which(word.v == "rules" | word.v == "rule") 
w.count.v[rule.v] <- 1
w.count.v
plot(w.count.v, main="Dispersion Plot of ‘rule’ in Corpus",
      xlab="Time", ylab="the", type="h", ylim=c(0,1), yaxt='n')

# Risk
n.time.v <- seq(1:length(word.v))
w.count.v <- rep(NA,length(n.time.v))
risk.v <- which(word.v == "risk") 
w.count.v[risk.v] <- 1
w.count.v
plot(w.count.v, main="Dispersion Plot of ‘risk’ in Corpus",
     xlab="Time", ylab="risk", type="h", ylim=c(0,1), yaxt='n')



#### TO BE DONE LATER - TEST CORRELLATIONS BETWEEN WORDS


###### Make function to find Keywords In Context

# Outputs list of words adjacent to each occurence of the keyword and plots the frequency of those adjancent terms 
kwic <- function(kw, word_vector, word_interval_size = 5, x_axis_size = 10) {
  pos <- which(word_vector == as.character(kw))
  pos_start <- pos - word_interval_size
  pos_end <- pos + word_interval_size
  interval.positions <- data.frame("kw" = pos
                  , "start" = pos_start
                  , "end" = pos_end
                  )
  interval.positions$start[which(interval.positions$start <= 0)] <- 1
  interval.positions$end[which(interval.positions$end >= length(word_vector))] <- length(word_vector)
  results <- as.list(pos)
  for (y in 1:length(pos)){
    results[[y]] <- word_vector[interval.positions$start[y]:interval.positions$end[y]]    
  }
  kwic.v <- unlist(results)
  kwic.t <- table(kwic.v)
  kwic.sorted.freqs.t <- sort(kwic.t, decreasing=TRUE)
  kwic.sorted.rel.freqs.t <- 100 * kwic.sorted.freqs.t / length(kwic.sorted.freqs.t)
  
  # Output plot
  plot(kwic.sorted.rel.freqs.t[1:x_axis_size], type="b", xlab="Words", ylab="Percent Frequency", xaxt="n")
  axis(1,1:x_axis_size, labels=names(kwic.sorted.rel.freqs.t[1:x_axis_size]))
  title(paste("Frequency of Words In Proximity to '", kw, "'", sep = ""))
  
  return (results)
}


testkwic <- kwic(kw = "power", 
                 word_vector = word.v, 
                 word_interval_size = 20, 
                 x_axis_size = 10)






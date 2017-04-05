library(stringr)
library(ggplot2)
library(tm)
library(tidyr)


###### GATHER ALL EMAIL FILE PATHS

dir <- "/Users/mhinckley/Desktop/maildir/"
setwd(dir)

level.1.files <- list.files(path=dir)

name.dirs <- c()
for (i in 1:length(level.1.files)){
  name.dirs[i] = paste(dir, level.1.files[i], sep ="")
}

all.name.dirs <- c()
sub.name.dirs <- c()
z = 0
for (x in 1:length(name.dirs)){
    sub.name.dirs <- list.files(path=name.dirs[x])
    for (y in 1:length(sub.name.dirs)){
        z = z + 1
        all.name.dirs[z] <- paste(name.dirs[x], "/", sub.name.dirs[y], sep ="")
    }
}


all.name.dirs <- c()
sub.name.dirs <- c()
file.dirs <- c()
dirs <- c() ### All relevant working directories
count1 = 0
count2 = 0

for (x in 1:length(name.dirs)){
  sub.name.dirs <- list.files(path=name.dirs[x])
  for (y in 1:length(sub.name.dirs)){
    count1 <- count1 + 1
    all.name.dirs[count1] <- paste(name.dirs[x], "/", sub.name.dirs[y], sep ="")
  }
}

for (a in 1:length(all.name.dirs)){
  file.dirs <- list.files(path=all.name.dirs[a])
  for (b in 1:length(file.dirs)){ 
    count2 <- count2 + 1
    dirs[count2] <- paste(all.name.dirs[a], "/",  file.dirs[b], sep ="")
  }
}

# Output the directories
dirs.df <- data.frame("dirs" = dirs)
write.csv(dirs.df, file = "all_email_directories.csv", row.names = FALSE)






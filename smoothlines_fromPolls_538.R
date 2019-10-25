library(RCurl) # to store html_widget on ftp server

# Enter working dirctory
setwd("WORKING_DIRECTORY")

print("working directory:")
print(getwd())


## Download csv from 538 ####

dat <- read.csv(url("https://projects.fivethirtyeight.com/polls-page/president_primary_polls.csv"))

## Prepare and select data ####

## parse date
dat[, 'date_new'] <- as.Date(as.character(dat$end_date), format = "%m/%d/%y")
min(dat$date_new)
# apparently only data after 2018-11-06, therefore only 33 states

##  reduce to national and democrates
table(dat$state)
df <- dat[dat$state == "", ]
df <- df[df$party == "DEM", ]
table(df$party)

##  reduce to candidates
canditates <- c("Warren", "Buttigieg", "Sanders","Biden", "Harris")
  
df <- df[df$answer %in% canditates, ]
df$answer<- droplevels(df$answer)
table(df$answer)

##  reduce to ratings
ratings <- c("A", "A-", "A+", "B", "B-", "B+")
df <- df[df$fte_grade %in% ratings, ]
df$fte_grade <- droplevels(df$fte_grade)

table(df$fte_grade)


## Upsampling ####

xl <- as.numeric(seq(min(df$date_new), max(df$date_new), by = 1))
df_fulltime <- data.frame(time = xl)
df_fulltime[, 'time_real'] <- as.Date(df_fulltime$time, origin = as.Date("1970-01-01"))


## Calcualate smooth line ####

## Plot all canditates separately and calculate smooth line
for (candidate in canditates){
  ddf <- df[df$answer == candidate, ] # choose candidate

  x <- as.numeric(ddf$date_new)
  y <- ddf$pct
  lo <- loess(y~x, span = 0.5)
  
  yl <- predict(lo,xl)
  df_fulltime[, make.names(candidate)] <- yl
}

## replace negative values with NA (--> otherwise looks weird!)
df_fulltime[df_fulltime < 0] <- NA


## store data ####

## store file locally
filename_current <- "US_polls_smooth_current.csv"

# rename old current file
timestamp <- Sys.time()
filename_old <- paste("US_polls_smooth_old_", timestamp, ".csv", sep = "")
file_old <- read.csv(filename_current)
write.csv(file_old, filename_old)
print("renamed old current file")

# store new current file
write.csv(df_fulltime, filename_current)
print("stored new current file")

library(ggplot2)
library(plotly) # to make ggplot interactive
library(txtplot)

# set working directory
setwd("WORKING_DIRECTORY")
print(getwd())

## Download csv from 538 ####

dat <- read.csv(url("https://projects.fivethirtyeight.com/polls-page/president_primary_polls.csv"), encoding = 'UTF-8')
table(dat$candidate_name)


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


##  reduce to top 5 candidates
canditates <- c("Warren", "Buttigieg", "Sanders","Biden", "Harris")
df <- df[df$answer %in% canditates, ]
df$answer <- droplevels(df$answer)
table(df$answer)


##  reduce to ratings
ratings <- c("A", "A-", "A+", "B", "B+")
df <- df[df$fte_grade %in% ratings, ]
df$fte_grade <- droplevels(df$fte_grade)

table(df$fte_grade)


## exclude head-to-head polls
df$notes <- as.character(df$notes)
df <- df[df$notes != "head-to-head poll", ]
table(df$notes)

# there seems to be one more head-to-head poll not correctly declared:
# remove it
df <- df[df$question_id != 92814, ]


## Upsampling ####

## One Datapoint per day for smoothing
xl <- as.numeric(seq(min(df$date_new), max(df$date_new), by = 1))
df_fulltime <- data.frame(time = xl)
# adjust date
df_fulltime[, 'time_real'] <- as.Date(df_fulltime$time, origin = as.Date("1970-01-01"))


## Calculate and plot smooth lines ####

## with ggplot (--> elegant, but how to export the smooth loess line?)
a <- ggplot(df, aes(x = date_new, y = pct)) +
   geom_point(aes(colour = fte_grade, text = paste("pollster:", pollster, "<br>", "ID:", question_id, "<br>", "s_size:", sample_size, "<br>", "rating:", fte_grade))) +
   geom_smooth(method = "loess", se = TRUE, span = 0.5) + # , n = 3
   facet_wrap( ~ candidate_name)

# create interactie version of ggplot
p <- ggplotly(a)
p


## Plot all canditates separately and calculate smooth line
# no weights
for (candidate in canditates){
  ddf <- df[df$answer == candidate, ] # choose candidate

  x <- as.numeric(ddf$date_new)
  y <- ddf$pct
  lo <- loess(y~x, span = 0.5) # , weights = ddf$weights_calc
  plot(ddf$date_new,y)
  title(candidate)
  
  yl <- predict(lo,xl)
  lines(xl, yl, col='red', lwd=2)
  df_fulltime[, make.names(candidate)] <- yl
}

## Plot all upsampled smooth lines in one graph ####

# replace negative values with NA (--> otherwise looks weird!)
df_fulltime[df_fulltime < 0] <- NA

ggplot() + 
  geom_line(data = df_fulltime, aes(x = time_real, y = Warren, colour = "Warren")) +
  geom_line(data = df_fulltime, aes(x = time_real, y = Buttigieg, colour = "Buttigieg")) +
  geom_line(data = df_fulltime, aes(x = time_real, y = Sanders, colour = "Sanders")) +
  geom_line(data = df_fulltime, aes(x = time_real, y = Biden, colour = "Biden")) +
  geom_line(data = df_fulltime, aes(x = time_real, y = Harris, colour = "Harris")) +
  scale_colour_manual("", values = c("Warren"="green", "Buttigieg"="black", 
                                     "Sanders"="blue", "Biden"="red", "Harris"="purple"))

## Store data ####

# write.csv(df_fulltime, "polls_smooth.csv")

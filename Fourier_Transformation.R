
allfile <- list.files(pattern = "csv")
allfile

dffirst <- read.csv(allfile[6], stringsAsFactors = F)
dfsecond <- read.csv(allfile[7], stringsAsFactors = F)

subsetfirst <- subset(dffirst, select=c("Crash_ID", "Crash_Date","Crash_Time","Latitude", "Longitude","Day_of_Week"))
subsetsecond <- subset(dfsecond, select=c("Crash_ID", "Crash_Date","Crash_Time","Latitude", "Longitude","Day_of_Week"))

df <- rbind(subsetfirst, subsetsecond)

Day_of_Week <- c("MON", "TUE", "WED", "THU", "FRI", "SAT", "SUN")

crashtime <- parse_date_time(df$Crash_Time, '%I:%M %p')

hour <- hour(crashtime)

df$hour <- hour

emptywkhr <- matrix(" ",1,1)

for(k in 1:nrow(df)){
  orderweek <- which(df[k,]$Day_of_Week == Day_of_Week)
  wkhr <- 24*(orderweek-1) + df[k,]$hour
  emptywkhr <- rbind(emptywkhr, wkhr)
  print(k)
}
df$wk.hr <- as.numeric(emptywkhr[2:nrow(emptywkhr)])
df$accident <- 1

weekdaysacc <- subset(df, wk.hr <= 119)
weekend <- subset(df, wk.hr > 119)
acc.weekday.df <- aggregate(accident ~ wk.hr, weekdaysacc, sum)

acc.total = aggregate(accident ~ wk.hr, df, sum)

per <- periodogram(acc.weekday.df$accident,log = 'yes',lwd = 5, ylab = 'Spectral Density')

perios = per$spec[order(per$spec, decreasing = T)]
freq <- 1/per$freq[which(per$spec == perios[4])]

acc.weekday.df$cos1 <- cos(acc.weekday.df$wk.hr / freq *2 * pi)
acc.weekday.df$sin1 <- sin(acc.weekday.df$wk.hr / freq *2 * pi)
acc.weekday.df$cos2 <- cos(acc.weekday.df$wk.hr / freq *2 * pi * 2)
acc.weekday.df$sin2 <- sin(acc.weekday.df$wk.hr / freq *2 * pi * 2)
acc.weekday.df$cos3 <- cos(acc.weekday.df$wk.hr / freq *2 * pi * 3)
acc.weekday.df$sin3 <- sin(acc.weekday.df$wk.hr / freq *2 * pi * 3)
acc.weekday.df$cos4 <- cos(acc.weekday.df$wk.hr / freq *2 * pi * 4)
acc.weekday.df$sin4 <- sin(acc.weekday.df$wk.hr / freq *2 * pi * 4)
acc.weekday.df$cos5 <- cos(acc.weekday.df$wk.hr / freq *2 * pi * 5)
acc.weekday.df$sin5 <- sin(acc.weekday.df$wk.hr / freq *2 * pi * 5)

f5 <- lm(accident ~ wk.hr + cos1 + sin1 + cos2 + sin2 + cos3 + sin3 + cos4 + sin4 + cos5 + sin5, data = acc.weekday.df)

f1 <- lm(accident ~ wk.hr + cos1 + sin1+ cos2 + sin2+ cos3 + sin3, data = acc.weekday.df)
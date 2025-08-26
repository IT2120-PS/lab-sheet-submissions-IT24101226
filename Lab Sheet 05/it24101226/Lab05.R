getwd()
setwd("C:\\Users\\it24101102\\Desktop\\IT24101102")
getwd()

data <- read.table("Data.txt", header = TRUE, sep = ",")
fix(data)
attach(data)

names(data) <- c("X1", "X2")
attach(data)

hist(X2, main = "Histogram for Number of Shareholders")
histogram <- hist(X2, main = "Histogram for Number of Shareholders", breaks = seq(130, 270, length = 8), right = FALSE)
?hist
histogram

breaks <- round(histogram$breaks)
breaks

freq <- histogram$counts
freq

mids <- histogram$mids
mids

classes <- c()
for(i in 1 : length(breaks)-1){
  classes[i] <- paste0("[", breaks[i], ",", breaks[i + 1], ")")
}
cbind(Classes = classes, Frequency = freq)

lines(mids, freq)

plot(mids, freq, type = "l", main = "Frequency polygon for shareholders", xlab = "shareholders", ylab = "Frequency", ylim = c(0, max(freq)))

cum.freq <- cumsum(freq)
new <- c()
for(i in 1 : length(breaks)){
  if(i == 1){
    new[i] = 0
  }else{
    new[i] = cum.freq[i - 1]
  }
}

plot(breaks, new, type = 'l', main = "Cumulative Frequency polygon for shareholders", xlab = "Shareholders", ylab = "Cumulative Frequency", ylim = c(0, max(cum.freq)))
cbind(Upper = breaks, CumFreq = new)

getwd()
list.files()
Delivery_Times <- read.table("Exercise - Lab 05.txt", header = TRUE)
names(Delivery_Times)
histogram1 <- hist(Delivery_Times$Delivery_Time_.minutes., main = "Histogram for Delivery Times", breaks = seq(20, 70, length = 9))

cum.freq1 <- cumsum(freq)
new <- c()
for(i in 1 : length(breaks)){
  if (i == 1){
    new[i] = 0
  }else{
    new[i] = cum.freq1[i - 1]
  }
}
plot(breaks, new, type = 'o', main = "Cumalative frequency ploygon for Delivery Time", xlab = "Time", ylab = "Cumalative frequency", ylim = c(0,max(cum.freq1)))

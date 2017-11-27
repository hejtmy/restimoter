library(ggplot2)
library(forecast)
filename <- "D:/GoogleDrive/Davis/Data/NEO-2017-11-12-20-02.csv"
filename <- "D:/GoogleDrive/Davis/Data/main-2017-11-24-15-54.csv"
filename <- "D:/GoogleDrive/Davis/Data/stopAtBeacons-2017-11-24-15-58.csv"

df_estimote <- read.csv(filename, sep = ";")
n <- nrow(df_estimote)
df_estimote$time.diff <- c(0, df_estimote$Time[2:n]-df_estimote$Time[1:n-1])
df_estimote$angle.diff <- c(0, df_estimote$Orientation[2:n]-df_estimote$Orientation[1:n-1])

points <- data.matrix(df_estimote[2:3])
df_estimote$position.diff <- euclid_distance_col(points)

df_estimote$speed <- df_estimote$position.diff/df_estimote$time.diff

plt <- ggplot(df_estimote, aes(Position.X, Position.Y, color = (Time)))
plt + geom_point()

ggplot(df_estimote, aes(Time, speed, color = Accuracy)) + geom_line()


plt <- ggplot(df_estimote, aes(Time, Orientation))
plt + geom_path()


# smoothing - maybe do interactively
df_estimote$smoothed <- 0
smooth <- function(df, level){
  df$smoothed <- level
  df$Position.X <- ma(df$Position.X, level)
  df$Position.Y <- ma(df$Position.Y, level)
  return(df)
}

df_estimote_smoothed_1 <- smooth(df_estimote, 50)

df_estimote_all <- do.call("rbind", list(df_estimote, df_estimote_smoothed_1))

plt <- ggplot(df_estimote_all, aes(Position.X, Position.Y, color = as.factor(smoothed)))
plt + geom_path()

plt <- ggplot(df_estimote_smoothed_1, aes(Position.X, Position.Y))
plt + geom_point()

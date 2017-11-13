library(ggplot2)
library(forecast)
filename <- "D:/GoogleDrive/Davis/Data/NEO-2017-11-12-20-02.csv"

df_estimote <- read.csv(filename, sep = ";")
plt <- ggplot(df_estimote, aes(Position.X, Position.Y))
plt + geom_path()

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

plt <- ggplot(df_estimote_smoothed_1, aes(Position.X, Position.Y, color = as.factor(smoothed)))
plt + geom_path()
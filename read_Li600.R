##
## Function to read an individual LI600 data file, set column names, and reformat columns
## 


read_Li600 <- function(x) {
  df <- read.csv(x, skip=3, header=F)
  headers <- read.csv(x, skip=1, header=F, nrows=1, as.is=T)
  colnames(df) <- headers
  df$Hour <- as.character(df$Hour)
  df
}
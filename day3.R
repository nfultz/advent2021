df <- read.delim("input/day3", sep="", header=FALSE, colClasses = "character")


bits <- strsplit(df$V1, "") |> do.call(what=rbind)

gamma <- (apply(bits, 2, table) |> apply(2, which.max) - 1)
epsilon <- 1 - gamma

tonum <- \(x) strtoi(paste(x, collapse=""), base = 2)
tonum(gamma) * tonum(epsilon)


oxygen <- bits
for(i in seq(ncol(bits))) {
  
  tbl <- table(oxygen[,i])
  
  j <- if(tbl[1] == tbl[2]) 1 else which.max(tbl) - 1
  
  oxygen <- oxygen[ oxygen[,i] == j , ]
  print(oxygen)
}
oxygen



o_rec <- function(data, ix) {
  if(nrow(data) == 1) return(data)
  
  tbl <- table(data[,ix])
  j <- if(tbl[1] == tbl[2]) 1 else which.max(tbl) - 1
  
  o_rec(data[ data[,ix] == j , , drop=FALSE], ix + 1)
}


o_rec(bits, 1)

co2 <- bits
for(i in seq(ncol(bits))) {
  
  tbl <- table(co2[,i])
  
  j <- if(tbl[1] == tbl[2]) 0 else which.min(tbl) - 1
  
  co2 <- co2[ co2[,i] == j , ]
}
co2

tonum(oxygen) * tonum(co2)

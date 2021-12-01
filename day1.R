d1 <- read.delim("input/day1", header=FALSE)
table(sign(diff(d1$V1)))


d2 <- cumsum(d1$V1)
(tail(d2, -2) - c(0, head(d2, -3)) ) |> diff() |> sign() |> table()


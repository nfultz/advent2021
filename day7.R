start <- readLines("input/day7") |> strsplit(',') |> do.call(what=as.numeric)

median(start)

sum(abs(start - median(start)))


# part2

m <- mean(start)

cost<- function(start, x) {
  abs(start - x) * (abs(start - x) + 1) / 2
}

cost(350, 352)

for(i in seq(floor(m-10), ceiling(m+10))) 
  cat(i, "\t", sum(cost(start, i)), '\n')

  
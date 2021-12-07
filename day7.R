start <- readLines("input/day7") |> strsplit(',') |> do.call(what=as.numeric)

median(start)

sum(abs(start - median(start)))


# part2

m <- mean(start)

for(i in seq(floor(m-10), ceiling(m+10))) 
  cat(i, "\t", sum((start - i)*(start - i + 1)/2), '\n')

  
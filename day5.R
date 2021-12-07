data <- readLines("input/day5") |> strsplit("[^0-9]+") |> do.call(what=rbind)

data <- matrix(as.numeric(df), nrow =nrow(df),dimnames = list(NULL, c('x1','y1', 'x2','y2'))) |> as.data.frame()



surface <- matrix(0, nrow=max(data[, c(1,3)]), ncol=max(data[, c(2,4)]))

horiz  <- subset(data, y1 == y2)
for(i in 1:nrow(horiz)) {
  with(horiz[i, ,drop=FALSE], {
    surface[ x1:x2, y1] <<- 1 +    surface[ x1:x2, y1]
  })
}

vert <- subset(data, x1 == x2)
for(i in 1:nrow(vert)) {
  with(vert[i, ,drop=FALSE], {
    surface[ x1, y1:y2] <<- 1 +    surface[ x1, y1:y2]
  })}

table(surface >= 2)


## Part 2


diag <- subset(data, (x1 != x2) & (y1 != y2))

for(i in 1:nrow(diag)) {
  with(diag[i, ,drop=FALSE], {
    surface[ cbind(x1:x2, y1:y2)] <<- 1 +    surface[ cbind(x1:x2, y1:y2)]
  })}

table(surface >= 2)






pts <- read.delim(textConnection("6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0"), FALSE, ",")


f <- file("input/day13")
open(f)

pts <- read.delim(f, FALSE, ',', nrows=735)

folds <- read.delim(f, FALSE, '=')





A <- Matrix::sparseMatrix( pts$V2 + 1, pts$V1 + 1)

for(i in seq_len(nrow(folds))){
  
  
  if(folds$V1[i] == 'fold along x') {
    x <- folds$V2[i]
    z <- dim(A)[2]
    w <- z - x - 1
    A[, seq(x - w+1, x)] <-   A[, seq(x - w + 1, x)] |  A[, seq(z, x + 2)] 
    A <- A[, 1:x]
  }

  if(folds$V1[i] == 'fold along y') {
    y <- folds$V2[i]
    z <- dim(A)[1]
    h <- z - y - 1
    A[seq(y - h + 1, y),] <-   A[seq(y - h + 1, y),] |  A[seq(z, y + 2),] 
    A <- A[1:y,]
  }  
    

  
  
  
  
  
}

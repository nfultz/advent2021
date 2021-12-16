f <- file("input/day14")
open(f)

X <- readLines(f, 1)

readLines(f, 1) # blank

rules <- readLines(f)

A <- matrix("", 26, 26, dimnames = list(LETTERS, LETTERS))

for(r in rules) {
  A[substr(r, 1, 1), substr(r, 2, 2)] <- substr(r, 7, 7)
  
}

next_polymer <- function(start) {
  start <- strsplit(start, "")[[1]]
  res <- ""
  
  for(i in seq(length(start) - 1)) {
    res <- append(res, 
                  c( start[i], A[start[i], start[i + 1]])
                  )
    
    
  }
  
  res <- append(res, start[i + 1])
  
  paste0(res, collapse = "")
}


start <- X
for(i in 1:10) start <- next_polymer(start)

strsplit(start, "")[[1]] |> table() |> range() |> diff()

# start <- X
# for(i in 1:40) start <- next_polymer(start)


A <- A[, colSums(A != "") > 0]
A <- A[rowSums(A != "") > 0,]


blank <- \() setNames(rep(0, ncol(A)), colnames(A))

nxt_count <- function(left, right, depth) {
  if(depth == 0) {
    res <- blank()
    res[left] <- 1
    return(res)
  }
  
  nxt_count(left, A[left, right], depth - 1) + nxt_count(A[left, right], right, depth - 1)
  
  
}

nxt_count <- memoise(nxt_count)


start <- strsplit(X, "")[[1]]

N <- length(start)

res <- blank()
res[start[N]] <- 1 # count last char

for(j in seq(N - 1)) {
  
  message(j)
  
  res <- res + nxt_count(start[j], start[j+1], 40)

}

res |> range() |> diff() |> print(digits=20)

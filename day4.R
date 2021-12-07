f <- file("input/day4")

open(f)

num <- readLines(f, 1) |> strsplit(',') |> unlist() |> as.numeric()

cards <- vector("list", 100)
i <- 0

while(length(readLines(f, 1)) > 0) { # skip blank
  cards[[i <- i + 1]] <- read.table(textConnection(readLines(f, 5)), 
                                    header=FALSE) |> as.matrix()
}


check <- function(board, i, K=nrow(board)) {
    hit <- matrix(board %in% num[1:i], K, K)
    if(K %in% c(rowSums(hit), colSums(hit))) {
      return(sum((1-hit) * board) * num[i])
    }
    -9
}

found <- FALSE
for(i in seq_along(num)) {
  for(j in seq_along(cards)) {
    z <- check(cards[[j]], i)
    if (z > 0) {
      print(c(i, j, z))
      found <- TRUE
    }
  }
  if(found) break;
}
z



## PArt 2

out <- vector("list", length(cards))
for(j in seq_along(cards)) {
  for(i in seq_along(num)) {
    z <- check(cards[[j]], i)
    if (z > 0) {
      out[[j]] <- data.frame(i, j, z)
      break
    }
  }
}
out <- do.call(rbind.data.frame, out)

subset(out, i == max(i))

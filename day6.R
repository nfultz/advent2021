
start <- readLines("input/day6") |> strsplit(',') |> do.call(what=table)


start <- start[as.character(0:8)]
start[is.na(start)] <- 0

T <- matrix(0, 8+1,8+1)

# first with clock of 0 -> 6 and 8 
T[0+1,6+1] <- 1
T[0+1,8+1] <- 1

T[cbind(1:8 + 1, 1:8)] <- 1 

start %*% T %*% T

start %*% with(eigen(T), vectors %*% diag(values^80) %*% solve(vectors)) |> sum()

start %*% with(eigen(T), vectors %*% diag(values^256) %*% solve(vectors)) |> sum()

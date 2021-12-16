
X <- read.fwf("input/day15", rep(1, 100), header=FALSE) |> as.matrix()


# Xc <- NA * X
# 
# 
# Xc[1,1] <- 0
# 
# 
# 
# while(is.na(Xc[100,100])) {
#   
#   cat(sum(is.na(Xc)), "\n")
#   
#   for(i in 1:100) {
#   for(j in 1:100) {
#       
#       Xc[i,j] <- X[i,j] + min(
#         left  = if(i > 1)   Xc[i-1, j],
#         right = if(i < 100) Xc[i+1, j],
#         up    = if(j > 1)   Xc[i, j-1],
#         left  = if(j < 100) Xc[i, j+1],
#         Xc[i,j],
#         na.rm=TRUE
#       )
# 
#   
#   }}
#   
#   
# }


# path <- function(x1,y1,x2,y2, acc=0) {
#   if(x1 == x2 && y1 == y2) return(X[x1, y1])
#   
#   min(
#     left  = if(i > 1)   path(x1,y1, x1-1, y1) + path(x1-1, y1, x2, y2),
#     right = if(i < 100) path(x1,y1, x1+1, y1) + path(x1+1, y1, x2, y2),
#     up    = if(j > 1)   path(x1,y1, x1, y1-1) + path(x1, y1-1, x2, y2),
#     down  = if(j < 100) path(x1,y1, x1, y1+1) + path(x1, y1+1, x2, y2),
#   )
#   
#   
# }
# 
# path <- memoise(path)

neighbors <- function(i, X) {
  
  c(
    left  = i - nrow(X),
    right = i + nrow(X),
    up    = if(i %% ncol(X) > 1) i - 1,
    down  = if(i %% ncol(X) != 0) i + 1
  )
  
}

djikstra <- function(X, source=1, target=length(X)) {
  
  D <- Inf * X
  P <- NA * X
  
  Q <- seq(target)
  
  D[source] <- 0
  
  while(length(Q) > 0) {
    
    u <- seq(target)[which.min(ifelse(seq(target) %in% Q, D, Inf))]
    
    Q <- setdiff(Q, u)
  
    for(n in intersect(Q, neighbors(u, X))) {
      if(D[u] + X[n] < D[n]) {
        D[n] <- D[u] + X[n]
        P[n] <- u
        
      }
      
    }  
    
  }
  
  
  list(D, P)  
}

fiveX <- function(X) {
  
  succ <- \(x) {x <- x + 1; x[x == 10] <- 1; x}
  

  tile <- function(X) {
    v <- X 
    ret <- list()
    ret[[1]] <- v
    for(i in 2:5) 
      ret[[i]] <- v <- succ(v)
    ret
  }
  
  lapply(tile(X), tile) |> lapply(do.call, what=cbind) |> do.call(what=rbind)
}

z <- djikstra(fiveX(X))



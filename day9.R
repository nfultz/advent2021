df <- read.fwf("input/day9", rep(1,100))

#df <- read.fwf("input/day9small", rep(1,10))

H <- dim(df)[1]
W <- dim(df)[2]

X  <- rbind(rep(99, W + 2),
            cbind(rep(99, H),
                  df,
                  rep(99, H)),
            rep(99, W + 2)
            ) |> as.matrix()

lowpt <- 
   X[1:H+1, 1:W+1] < X[1:H + 1, 1:W]  &   # up
   X[1:H+1, 1:W+1] < X[1:H    , 1:W + 1]  &   # left
   X[1:H+1, 1:W+1] < X[1:H + 2, 1:W + 1]  &   # down
   X[1:H+1, 1:W+1] < X[1:H + 1, 1:W + 2]     # right
   
sum(X[1:H+1, 1:W+1][lowpt] + 1) # don't forget the plus one


# part two

lowpti <- which(lowpt, arr.ind=TRUE) + 1 # +1 to go from inner to padded
# 
r <- \(x) x[,1] + (H+2)*(x[,2] - 1)
# 
# 
# 
# # takes array and pt, returns size of basin
# basin <- function(X, i, b=c()) {
#     
#   w <- arrayInd(i, dim(X)) # go from inner coords to padded 
#   if(X[w] == 99) return(c())
#   
#   up    <- (w + c(0, -1))
#   left  <- (w + c(-1, 0))
#   down  <- (w + c(0, 1))
#   right <- (w + c(1, 0))
#   
#   
#   inbasin <- 
#     (X[w]  < X[left]   || r(left)  %in% b) &
#     (X[w]  < X[right]  || r(right) %in% b) &
#     (X[w]  < X[up]     || r(up)    %in% b) &
#     (X[w]  < X[down]   || r(down)  %in% b) 
#   
#   if(!inbasin) return(b)
#   
#   b <- c(b, i)
#   
#   repeat {
#     nb <- sort(unique(unlist(c(
#       b,
#       if(! r(up) %in% b)     basin(X, r(up), b),
#       if(! r(left) %in% b)   basin(X, r(left), b),
#       if(! r(right) %in% b)  basin(X, r(right), b),
#       if(! r(down) %in% b)   basin(X, r(down), b)
#     ))))
#     
#     if(setequal(nb, b)) break;
#      b <- nb
#     
#   }
#   
# 
#   
#   list(b)
# }
# 
# 
# by(r(lowpti), 1:nrow(lowpti), basin, X=X) |> 
#   lengths() |>
#   sort() |> tail(3) |> prod()


# whole thing did not work

FLAG <- 999999999999999999999

X[X == 99] <- FLAG



findlow <- function(w) {
  
    if(X[w] == FLAG || X[w] == max(df)) return(FLAG)
  
    up    <- (w + c(0, -1))
    left  <- (w + c(-1, 0))
    down  <- (w + c(0, 1))
    right <- (w + c(1, 0))
  

    vals <- c( X[w], X[up], X[down], X[left], X[right] )
    
    if (X[w] == min(vals) ) return(r(w))
    
    
    rec <- c(
      if(X[up]    == min(vals)) findlow(up),
      if(X[down]  == min(vals)) findlow(down),
      if(X[left]  == min(vals)) findlow(left),
      if(X[right] == min(vals)) findlow(right)
    )
    
    rec <- unique(rec)
    
    if(length(rec) == 1) return(rec)
    
    return(FLAG)
    
}


basinMap <- matrix(-9, nrow(X), ncol(X))

findlow <- memoise(findlow)

for(i in seq(nrow(X))){
  for(j in seq(ncol(X))){
    l <- findlow(matrix(c(i,j), nrow=1))  
#    print(c(i,j, X[i,j], l))
    basinMap[i,j] <- l
  }
}
basinMap[basinMap == FLAG] <- NA

table(basinMap) |> sort() |> tail(3) |> prod()






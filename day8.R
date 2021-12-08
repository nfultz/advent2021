nums <- readLines("input/day8") |> strsplit('[ |]+') |> do.call(what=rbind) |> as.data.frame()

# q1

sapply(nums[,11:14], nchar) |> table( exclude = 5:6) |> sum()


#q2

row <- nums[1,1:10]
row2 <- nums[1, 11:14]

# check that all contain
apply(apply(nums[1:10], 2, nchar), 1, match, 2) |> apply(2, max, na.rm=TRUE) |> table()

# check all contain all ten digits
apply(nums[1:10], 1, \(x) length(unique(x))) |> table()


solve <- function(row) {
  sol <- rep(9, 10)
  
  n <- nchar(row)
  
  sol[n==2] <- 1
  sol[n==4] <- 4
  sol[n==3] <- 7
  sol[n==7] <- 8
  
  sets <- strsplit(unlist(row), "")
  
  eight <- letters[1:7]
  one <- sets[n==2][[1]]
  

  rightside <- lapply(sets, intersect, x=one) |> lengths()
  
  # six
  sol[n == 6 & rightside == 1] <- 6
  six <- sets[n == 6 & rightside == 1][[1]]

  # three
  sol[n == 5 & rightside == 2] <- 3
    
  
  topright <- lapply( sets, intersect, setdiff(eight, six)) |> lengths()
  
  sol[!topright & (n ==5)] <- 5
  five <- sets[!topright & (n ==5)][[1]]
  
  trbl <- lapply(sets, intersect, setdiff(eight, five)) |> lengths()
  
  sol[trbl == 2 & n == 5] <- 2

  sol[trbl == 2 & n == 6] <- 0

  sol <- setNames(sol, sapply(sets, \(x) paste(sort(x), collapse='')))
  sol
}

decode <- function(sol, row2) {
  row2 <- unlist(row2) |> strsplit("") |> lapply(sort) |> sapply(paste, collapse='')
  sol[row2] %*% 10^(3:0)
}

by(nums, 1:nrow(nums), \(r) solve(r[1:10]) |> decode(r[11:14])    ) |> sum()




target <- "target area: x=20..30, y=-10..-5"
target <- readLines("input/day17")
target <- regexec("target area: x=(?<x1>-?[0-9]+)..(?<x2>-?[0-9]+), y=(?<y1>-?[0-9]+)..(?<y2>-?[0-9]+)", target, perl = TRUE) |> regmatches(x=target)
target <- target[[1]][-1] |> as.list() |> lapply(as.numeric)



hit <- function(x,y, target) {
  with(target, x1 <= x && x <= x2 && y1 <= y && y <= y2)
}


sim <- function(dx, dy, target) {
  x <- y <- ymax <- 0
  repeat {
    x <- x + dx
    y <- y + dy
    
    if(hit(x, y, target)) return(ymax)
    if(x > target$x2) return(-99999)
    #if(dx == 0 & x < target$x1) return(-99999)
    if(y < target$y1) return(-99999)
    
    dx <- dx - sign(dx)
    dy <- dy - 1    
    
    if(dy >= 0) ymax <- y
  }
  
  
}


grid <- with(target, expand.grid(dx=seq(x2), dy=seq(y1, 100), ymax=NA))


for(i in seq(nrow(grid))) {
  
  grid$ymax[i] <- with(grid[i,], sim(dx, dy, target))
  if( i %% 10000 == 0) message(i, "\t", max(grid$ymax, na.rm=TRUE))  
}

subset(grid, ymax == max(ymax))

subset(grid, ymax > -99999) |> nrow()






data <- read.fwf(textConnection("
11111
19991
19191
19991
11111"), skip=1, rep(1,5)) |> as.matrix()



data <- read.fwf(textConnection("
5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526"), skip=1, rep(1,10)) |> as.matrix()


data <- read.fwf("input/day11", rep(1,10)) |> as.matrix()

flash <- 0
for(step in 1:1000) 
  {
  
  data <- data + 1
  
  repeat {
    i <- which.max(data)
    if(!length(i) || data[i] <= 9) break;
    flash <- flash + 1
    rc <- arrayInd(i,dim(data))
    
    for(dr in -1:1) {
      if(!((rc[1] + dr) %in% 1:nrow(data))) next; 
      for(dc in -1:1) { 
        if(!((rc[2] + dc) %in% 1:ncol(data))) next;
        #print(c(dr, dc))
        data[rc+c(dr, dc)] <- data[rc+c(dr, dc)] + 1
        
      }
    }
    
    data[i] <- NA
    
  }
  
  if(all(is.na(data))) {
    message("Synched on turn ", step, "\n")
    break
  }
  
  
  data[is.na(data)] <- 0
  
  
}

data; flash; i;

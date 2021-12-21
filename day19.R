lines <- readLines("input/day19") 
data  <- tapply(lines, 1 + c(0,  cumsum(lines == "")[-length(lines)]), textConnection, simplify = FALSE) |>
        lapply(read.csv, header=FALSE, skip=1, col.names = c('x', 'y', 'z'))


D <- lapply(data, dist)


overlap <- outer(D, D, Vectorize(\(x,y) length(intersect(x,y))))
diag(overlap) <- 0

overlap <- overlap >= 11*12/2

beta <- vector("list", length(data))
beta[[1]] <- rbind(diag(3), 0)


solver <- function(ego, alter) {
  
  egoD <- D[[ego]]
  alterD <- D[[alter]]
  
  i <- intersect(egoD, alterD)
  
  egoD2 <- egoD
  egoD2[] <- egoD %in% i
  
  egoidx <- which(colSums(as.matrix(egoD2)) > 0) 

  alterD2 <- as.matrix(alterD)
  alterD2[] <- alterD2 %in% i 
  
  alteridx <- which(colSums(as.matrix(alterD2)) > 0) 
  
  
  egoD3 <- as.matrix(egoD)[egoidx, egoidx]
  alterD3 <- as.matrix(alterD)[alteridx, alteridx]
  
  
  map <- rep(0, ncol(egoD3))
  
  for(i in 1:ncol(egoD3)) {
    map[i] <- alteridx[apply(alterD3, 2, `%in%`, egoD3[,i] ) |> apply(2, all)]
    
  }  
 
  egoPts <- sols[[ego]][egoidx,]
  alterPts <- data[[alter]][map,]
  
  #m <- lm(as.matrix(egoPts)~as.matrix(alterPts)) 
  #predict(m, data[[alter]])
  
  m <- coef(lm.fit(cbind(as.matrix(alterPts), 1), as.matrix(egoPts)))
  m <- round(m)
  beta[[alter]] <<- m
  cbind(as.matrix(data[[alter]]), 1) %*% m
}


solved <- 1

sols <- vector("list", length(data))
sols[[1]] <- data[[1]]

overlap_edges <- which(overlap, arr.ind = TRUE) |> as.data.frame()

while(length(solved) < length(data)) {
  
  todo <- subset(overlap_edges, row %in% solved & ! col %in% solved)
  
  todo <- head(todo, 1)

  print(todo)
    
  sols[[todo$col]] <- solver(todo$row, todo$col)
  
  solved <- c(solved, todo$col)
}


pts <- do.call(rbind, sols)
nrow(unique(pts))


satd <- dist(t(sapply(beta, \(x) x[4,])), method='manhattan')
max(satd)

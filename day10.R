

points = c(
  ")"= 3,
  "]"= 57,
  "}"= 1197,
  ">"= 25137
  )
points2 <- c(
  ")"= 1,
  "]"= 2,
  "}"= 3,
  ">"= 4
)

openers <- c("<", "(", "[", "{")
closers <- c(">", ")", "]", "}")

lines <- readLines("input/day10") |> strsplit("")


score <- function(line) {
  
  s <- c()
  
  for(c in line) {
    
    if(c %in% openers) {
      s <- append(s, c)      
      
    }
    else if (c %in% closers) {
      
      if(match(c, closers) == match(tail(s, 1), openers)) {
        s <- head(s, -1)
      } 
      else return(points[c])
      
    }

  }
  
  # hack in part two
  
  score <- 0
  
  for(c in rev(s)) {
    score <- score * 5 - points2[closers[match(c, openers)]]
    
  }
  
  score
}


vapply(lines, score, 0) |> Filter(f=\(x) x < 0) |> median()


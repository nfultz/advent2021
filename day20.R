f <- file("input/day20")
open(f)

f <- textConnection(
"..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

#..#.
#....
##..#
..#..
..###")



key <- readLines(f, 1)
readLines(f, 1)
data <- readLines(f) |> strsplit("") |> do.call(what = rbind)


key1 <- strsplit(key, "")[[1]]


enhance <- \(x, R, C, init='.') {
  
  #looping through ala horners
  k <- 0
  for(i in R + -1:1)
  for(j in C + -1:1) {
    z <- if(i >= 1 && j >= 1 && i <= nrow(x) && j <= ncol(x)) x[i, j]  else init
    k <- 2*k + (z == '#')
  }
  
  key1[1 + k]
}


succ <- function(data, init='.') {
  
  M <- nrow(data)
  N <- ncol(data)
  
  ret <- matrix(init, M+2, N+2)

  
  for(i in seq(0,M+1)) {
    for(j in seq(0,N+1)) {
      ret[i+1,j+1] <- enhance(data, i, j, init)
    }
  }
  
  ret
}


data |> succ(init='.') |> succ(init='#') |> table()


z <- data

for(i in 1:25)
  z <- z |> succ(init='.') |> succ(init='#')

table(z)

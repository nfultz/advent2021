
edges <- read.delim(textConnection("
dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc"), , sep='-', header=FALSE)


edges <- read.delim("input/day12", sep='-', header=FALSE)

nodes <- unique(union(edges$V1, edges$V2))
edges[] <- lapply(edges, factor, nodes)

A <- matrix(0, length(nodes), length(nodes), dimnames = list(nodes, nodes))
A[edges |> as.matrix()] <- 1
A <- A + t(A)


big_cave <- setNames(nodes == toupper(nodes), nodes)

small_cave <- setdiff(nodes[!big_cave], c('start', 'end'))

A[, "start"] <- 0


get_path <- function(path, adj, to) {
  
  current <- tail(path, 1)

  if (current == to) return(paste(path, collapse='-'))
    
  if (!big_cave[current]) {
    nsc <- max(table(factor(path, small_cave)))
    if(nsc == 2)
      adj[, intersect(path, small_cave)] <- 0
  }
  
  nxt <- names(which(adj[current, ] == 1))
  
  if (!length(nxt)) return(NULL)
  
  lapply(nxt, append, x=path) |> lapply(get_path, adj, to)
  
  
}


results <- get_path("start", A, "end") |> unlist()




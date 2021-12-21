lpair <- function(x,y) structure(list(x,y), class='lpair')

parse_pair <- function(line) {
  tokens <- strsplit(line, '')[[1]] |> as.list() |>
    Filter(f=\(x) x != ',') |>
    lapply(\(x) switch(x, "["="[", "]"="]", as.numeric(x))) |>
    structure(class='pair')
}  

DEBUG <- FALSE

reduce <- function(tokens) {
  done <- FALSE
  while(!done) {
    edited <- FALSE
    for(i in seq(length(tokens))) {
      
      x <- tokens[[i]] 
      
      if(i >= 5
           &&  x == '[' 
           && sum(head(tokens, i) == '[') - sum(head(tokens, i) == ']') >= 5 
           && (i + 3 <= length(tokens)) 
           && tokens[[i+3]] == ']' ) {
          
          if(DEBUG) browser()

          for(j in seq(i, 1)) {
            if(is.numeric(tokens[[j]])) {
              tokens[[j]] <- tokens[[j]] +  tokens[[i + 1]]
              break;
            }
          }
          
          for(j in seq(i + 3, length(tokens))) {
            if(is.numeric(tokens[[j]])) {
              tokens[[j]] <- tokens[[j]] +  tokens[[i + 2]]
              break
            }
          }
          
          tokens <- c(tokens[1:(i-1)], 0,  tokens[(i+4):length(tokens)])
          
          edited <- TRUE
          break
        }
        
      
      
      
      
    }

    if(! edited)
    for(i in seq(length(tokens))) {
      
      x <- tokens[[i]] 
      
      if (is.numeric(x) && x >= 10) {
        if(DEBUG) browser()
        tokens <- c(head(tokens, i - 1), 
                    '[', floor(x/2), ceiling(x/2)  , ']', 
                    tokens[seq(i+1, length(tokens))])
        edited <- TRUE
        break;
      }
    }
        
    done <- !edited
  }
  
  structure(tokens, class='pair')
  
}


pair_to_lpair <- function(tokens) {
  while(length(tokens) > 1) {
    for(i in seq(length(tokens) - 3)) {

      if(tokens[[i]] == '[' && tokens[[i + 3]] == ']') {
        
        
        
        tokens[[i]] <- lpair(tokens[[i+1]], tokens[[i+2]])
        tokens[i+1:3] <- NULL
        break
      }      
      
    }
  }
  
  structure(tokens[[1]], class='lpair')
}

serialize <- function(x) UseMethod("serialize", x)
serialize.lpair <- function(x) paste0(c("[", x[[1]]|> serialize(), ",",
                                             x[[2]]|> serialize(), "]"), collapse = '') 
serialize.default <- I


###
`+.pair` <- function(e1, e2) {
  reduce(c('[', e1, e2, ']'))
}

### 
magnitude         <- function(x) UseMethod("magnitude")
magnitude.pair <- function(x) magnitude(pair_to_lpair(x))
magnitude.lpair    <- function(x) 3*magnitude(x[[1]]) + 2*magnitude(x[[2]])
magnitude.default <- function(x) x



c('[[[[4,3],4],4],[7,[[8,4],9]]]',  '[1,1]') |> lapply(parse_pair) |> Reduce(f=`+`) |> paste(collapse='')


readLines("input/day18-ex2") |> lapply(parse_pair) |> Reduce(f=`+`) |> paste(collapse = '')

total <- readLines("input/day18") |> lapply(parse_pair) |> Reduce(f=`+`)
magnitude(total)



print.pair <- \(x) print(paste(x, collapse=''))

###


'[[[[[9,8],1],2],3],4]' |> parse_pair() |> reduce()
#becomes [[[[0,9],2],3],4] (the 9 has no regular number to its left, so it is not added to any regular number).

'[7,[6,[5,[4,[3,2]]]]]' |> parse_pair() |> reduce()
#becomes [7,[6,[5,[7,0]]]] (the 2 has no regular number to its right, and so it is not added to any regular number).

'[[6,[5,[4,[3,2]]]],1]' |> parse_pair() |> reduce()
#becomes [[6,[5,[7,0]]],3].

'[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]' |> parse_pair() |> reduce()
#becomes [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]] (the pair [3,2] is unaffected because the pair [7,3] is further to the left; [3,2] would explode on the next action).

'[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]' |> parse_pair() |> reduce()
#becomes [[3,[2,[8,0]]],[9,[5,[7,0]]]].




### part2

snail <- readLines("input/day18-ex3") |> lapply(parse_pair) 

m <- -1
for(a in seq_along(snail)) {
  for(b in seq_along(snail)) if(a != b) m <- max(m, magnitude(a+b))
  print(m)
}

  
  
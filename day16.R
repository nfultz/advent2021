a <- readLines("input/day16", 1)
hexToBits <- \(a) a |> strsplit("") |> 
                       sapply(strtoi, base = 16) |>  
                       lapply(intToBits) |>
                       lapply(rev) |> lapply(tail, 4) |> c(recursive=TRUE)


bits2Dec <- \(x) {
  if(length(x) > 20) {
    
    x <- bits2Dec(head(x, -20)) * (2^20) + bits2Dec(tail(x, 20))
    return(x)
  }
  
  x |> as.integer() |> paste(collapse="") |> strtoi(2)
  
}

e2 <- "38006F45291200"
hexToBits(e2)

parsePacket <- function(bits) {
  version <- bits[1:3] |> bits2Dec()
  type    <- bits[4:6] |> bits2Dec()
  
  if(type == 4) {
    i <- 2
    lit <- c()
    
    repeat {
      i <- i + 5
      lit <- c(lit, bits[i + 1:4])
      if(bits[i] == 0) break
    }
    # if(length(lit)> 30) browser()
    lit <- lit |> bits2Dec()
    if(is.na(lit)) browser()
    return(list(version=version, type=type, lit=lit, bits=i+4))
  }
  
  lengthtype <- bits[7]
  
  subpackets <- list()
  
  if(lengthtype == 0) {
    total_length <- bits[8:22] |> bits2Dec()
    
    i <- 0
    bits <- tail(bits, -22)
    repeat {
      pkt <- parsePacket(bits)
      subpackets[[length(subpackets) + 1]] <- pkt
      bits <- tail(bits, -pkt$bits)
      i <- i + pkt$bits
      if(i >= total_length) break
    }
    
    bits_used <- 22 + total_length
  }
  else if (lengthtype == 1) {
    n_subpackets <- bits[8:18] |> bits2Dec()
    
    bits_used <- 18
    
    bits <- tail(bits, -bits_used)
    
    for(i in seq(n_subpackets)) {
      
        pkt <- parsePacket(bits)
        subpackets[[length(subpackets) + 1]] <- pkt
        bits <- tail(bits, -pkt$bits)
        bits_used <- bits_used + pkt$bits

    }
    
    
    
    
  }

  
  return(list(version=version, type=type, subpackets=subpackets, bits=bits_used))
  
}

#literal
bits <- hexToBits("D2FE28")

debug(parsePacket)

parsePacket(bits)

#
bits <- hexToBits("38006F45291200")
parsePacket(bits)


"EE00D40C823060"|> hexToBits() |> parsePacket() 


"8A004A801A8002F478" |> hexToBits() |> parsePacket() 
bits <- hexToBits(a)
pkt <- parsePacket(bits)

get_ver <- \(x) x$version + sum(vapply(x$subpackets, get_ver, 0))


eval_pkt <- function(pkt) {
  
  if(pkt$type == 4) return(pkt$lit)
  
  
  subpkts <- sapply(pkt$subpackets, eval_pkt)
  
  
  switch(pkt$type + 1,
         sum(subpkts),
         prod(subpkts),
         min(subpkts),
         max(subpkts),
         stop("literal should early return"),
         if(subpkts[[1]] > subpkts[[2]]) 1 else 0,
         if(subpkts[[1]] < subpkts[[2]]) 1 else 0,
         if(subpkts[[1]] == subpkts[[2]]) 1 else 0
         )

}

"C200B40A82" |> hexToBits() |> parsePacket() |> eval_pkt()
"04005AC33890" |> hexToBits() |> parsePacket() |> eval_pkt()
"880086C3E88112" |> hexToBits() |> parsePacket() |> eval_pkt()


"CE00C43D881120" |> hexToBits() |> parsePacket() |> eval_pkt()

"D8005AC2A8F0" |> hexToBits() |> parsePacket() |> eval_pkt()
"F600BC2D8F" |> hexToBits() |> parsePacket() |> eval_pkt()
"9C005AC2F8F0" |> hexToBits() |> parsePacket() |> eval_pkt()

"9C0141080250320F1802104A08" |> hexToBits() |> parsePacket() |> eval_pkt()

a |> hexToBits() |> parsePacket() |> eval_pkt() |> print(digits=20)

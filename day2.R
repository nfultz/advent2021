df <- read.delim("input/day2", header = FALSE, sep = " ", stringsAsFactors = TRUE)

with(xtabs(V2~V1, df) |> as.list(), forward * (down - up))


df$aim <- cumsum(mapply(switch, as.character(df$V1), down=df$V2, up=-df$V2, forward=0))

df$aim2 <- cumsum(c(down=1, up=-1, forward=0)[as.character(df$V1)] * df$V2)

horiz <- sum(df[df$V1 == 'forward', "V2"])
depth <- with(df[df$V1 == 'forward', ], sum(V2 * aim))

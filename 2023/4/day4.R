library(tidyverse)

inp <- readLines("puzzle_input.txt")

# Part 1

df <- tibble(input = inp) |> 
  separate_wider_delim(input, ":", names = c("card","numbers")) |> 
  separate_wider_delim(numbers, "|", names = c("winning","mine")) |> 
  mutate(card = str_remove(card, "Card"),
         card = parse_integer(card)) |> 
  mutate(winning = map(winning, \(set) unlist(strsplit(set, " "))),
         mine = map(mine, \(set) unlist(strsplit(set, " ")))) |> 
  mutate(winning = map(winning, \(numvec) numvec[numvec != ""]),
         mine = map(mine, \(numvec) numvec[numvec != ""])) |> 
  mutate(common = map2(winning, mine, intersect)) |> 
  mutate(num_common = map_int(common, length)) |> 
  mutate(points = ifelse(num_common > 0, 2^(num_common - 1), 0))

sum(df$points)

# Part 2

# Need vector of card copy counts

copy_counts <- integer(nrow(df))
for(card in 1:nrow(df)){
  if(df$num_common[card] == 0) next
  copies <- (card + 1):(card + df$num_common[card])
  copy_counts[copies] <- copy_counts[copies] + 1
  for(i in seq_len(copy_counts[card])){
    copy_counts[copies] <- copy_counts[copies] + 1
  }
  
}

nrow(df) + sum(copy_counts)

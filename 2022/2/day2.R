library(tidyverse)

inp <- readLines("puzzle_input.txt")

# Part 1
# What would your total score be if everything goes 
# exactly according to your strategy guide?

df <- tibble(inp) |> 
  separate_wider_delim(inp, " ", names = c("them","me")) |> 
  mutate(them = case_match(them,
                           "A" ~ "Rock",
                           "B" ~ "Paper",
                           "C" ~ "Scissors"),
         me = case_match(me,
                         "X" ~ "Rock",
                         "Y" ~ "Paper",
                         "Z" ~ "Scissors")) |> 
  mutate(result = case_when(them == "Rock" & me == "Paper" ~ "Win",
                            them == "Rock" & me == "Scissors" ~ "Lose",
                            them == "Paper" & me == "Rock" ~ "Lose",
                            them == "Paper" & me == "Scissors" ~ "Win",
                            them == "Scissors" & me == "Rock" ~ "Win",
                            them == "Scissors" & me == "Paper" ~ "Lose",
                            them == me ~ "Draw")) |> 
  mutate(shape_score = case_match(me, "Rock" ~ 1, "Paper" ~ 2, "Scissors" ~ 3),
         outcome_score = case_match(result, "Win" ~ 6, "Lose" ~ 0, "Draw" ~ 3),
         total_score = shape_score + outcome_score)

sum(df$total_score)

# Part 2

df <- tibble(inp) |> 
  separate_wider_delim(inp, " ", names = c("them","me")) |> 
  mutate(them = case_match(them,
                           "A" ~ "Rock",
                           "B" ~ "Paper",
                           "C" ~ "Scissors"),
         result = case_match(me,
                             "X" ~ "Lose",
                             "Y" ~ "Draw",
                             "Z" ~ "Win")) |> 
  mutate(me = case_when(them == "Rock" & result == "Win" ~ "Paper",
                        them == "Rock" & result == "Lose" ~ "Scissors",
                        them == "Paper" & result == "Lose" ~ "Rock",
                        them == "Paper" & result == "Win" ~ "Scissors",
                        them == "Scissors" & result == "Win" ~ "Rock",
                        them == "Scissors" & result == "Lose" ~ "Paper",
                        result == "Draw" ~ them)) |> 
  mutate(shape_score = case_match(me, "Rock" ~ 1, "Paper" ~ 2, "Scissors" ~ 3),
         outcome_score = case_match(result, "Win" ~ 6, "Lose" ~ 0, "Draw" ~ 3),
         total_score = shape_score + outcome_score)

sum(df$total_score)



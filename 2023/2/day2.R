library(tidyverse)

inp <- readLines("puzzle_input.txt")

# Part 1

# which games would have been possible if the bag contained 
# only 12 red cubes, 13 green cubes, and 14 blue cubes?

df <- tibble(input = inp) |> 
  separate_wider_delim(input, ":", names = c("game","balls")) |> 
  separate_longer_delim(balls, ";") |> 
  mutate(game = str_remove(game, "Game "),
         game = as.integer(game)) |>
  mutate(num_green = str_extract(balls, "(\\d+) green", group = 1),
         num_blue = str_extract(balls, "(\\d+) blue", group = 1),
         num_red = str_extract(balls, "(\\d+) red", group = 1)) |> 
  mutate(across(starts_with("num_"), as.integer)) |> 
  replace_na(list(num_green = 0, num_red = 0, num_blue = 0))

part1_df <- df |> 
  group_by(game) |> 
  filter(all(num_green <= 13), all(num_red <= 12), all(num_blue <= 14)) |> 
  ungroup()

sum(unique(part1_df$game))  

# Part 2

part2_df <- df |> 
  group_by(game) |> 
  summarise(max_red = max(num_red),
            max_blue = max(num_blue),
            max_green = max(num_green)) |> 
  ungroup() |> 
  mutate(power = max_red * max_blue * max_green)

sum(part2_df$power)

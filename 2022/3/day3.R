library(tidyverse)

inp <- readLines("puzzle_input.txt")

# Part 1
# Find the item type that appears in both compartments of each rucksack. 
# What is the sum of the priorities of those item types?

priorities = c(letters, LETTERS)

df <- tibble(inp) |> 
  mutate(comp1 = str_sub(inp, 1, nchar(inp)/2),
         comp2 = str_sub(inp, nchar(inp)/2 + 1, nchar(inp))) |> 
  separate_longer_position(c(comp1, comp2), 1) |> 
  group_by(inp) |> 
  mutate(common = intersect(comp1, comp2)) |> 
  ungroup() |> 
  distinct(inp, common) |> 
  mutate(priority = map_int(common, \(ltr) which(priorities == ltr)))

sum(df$priority)

# Part 2

df <- tibble(inp) |> 
  mutate(grp = cumsum((row_number()-1) %% 3 == 0)) |> 
  mutate(ltrs = map(inp, \(str) unlist(strsplit(str, split = "")))) |> 
  group_by(grp) |> 
  mutate(common = Reduce(intersect, ltrs)) |> 
  ungroup() |> 
  distinct(grp, common)|> 
  mutate(priority = map_int(common, \(ltr) which(priorities == ltr)))

sum(df$priority)

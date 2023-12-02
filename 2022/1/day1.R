
inp <- readLines("puzzle_input.txt")

# Part 1
# Find the Elf carrying the most Calories. 
# How many total Calories is that Elf carrying?

elf_no <- cumsum(inp == "") + 1

by_elf <- split(inp, elf_no)

by_elf <- lapply(by_elf, \(elf) elf[elf != ""])

by_elf <- lapply(by_elf, as.integer)

by_elf_sum <- sapply(by_elf, sum)

max(by_elf_sum)

# Part 2
# Find the top three Elves carrying the most Calories. 
# How many Calories are those Elves carrying in total?

top3 <- rev(order(by_elf_sum))[1:3]

sum(by_elf_sum[top3])


# Part 1

example <- c(
  "g4ffwaaw5tt",
  "1dfaewfaweaw9",
  "sfwq9WqW",
  "4534353523sger6"
)

example <- c(
  "1abc2",
  "pqr3stu8vwx",
  "a1b2c3d4e5f",
  "treb7uchet"
)

example <- readLines("day1_puzzle.txt")

find_sum <- function(lines){
  
  chars <- strsplit(lines, split = "")
  
  nums <- lapply(chars, \(charvec) charvec[charvec %in% as.character(0:9)])
  
  comb_nums <- lapply(nums, \(numvec) paste0(numvec[1], numvec[length(numvec)]))
  
  comb_nums |> 
    unlist() |> 
    as.integer() |>
    sum()
  
}

find_sum(example)


# Part 2

example2 <- example
named_nums <- as.character(1:9)
names(named_nums) <- c("one","two","three","four","five","six","seven","eight","nine")

for(i in seq_along(example2)){
  
  for(j in 1:9){
    num_name <- names(named_nums)[j]
    
    example2[i] <- gsub(num_name,
                        paste0(num_name, 
                               named_nums[j], 
                               substr(num_name, nchar(num_name), nchar(num_name))),
                        example2[i])
  }
   
}  

find_sum(example2)
  

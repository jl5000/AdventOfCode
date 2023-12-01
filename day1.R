
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

chars <- strsplit(example, split = "")

nums <- lapply(chars, \(charvec) charvec[charvec %in% as.character(0:9)])

comb_nums <- lapply(nums, \(numvec) paste0(numvec[1], numvec[length(numvec)]))

comb_nums |> 
  unlist() |> 
  as.integer() |> 
  sum()

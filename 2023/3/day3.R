
inp <- readLines("puzzle_input.txt")

# Part 1
# What is the sum of all of the part numbers in the engine schematic?

mat <- matrix(
  unlist(strsplit(paste(inp, collapse = ""), split = "")),
  nrow = length(inp),
  ncol = nchar(inp[1]),
  byrow = TRUE
)

cell_is_part <- function(mat, row, col){
  # Not a number - cannot be a valid part
  if(!mat[row,col] %in% as.character(0:9)) return(FALSE)
  
  for(i in (row-1):(row+1)){
    if(i > 0 && i <= nrow(mat)){
      for (j in (col-1):(col+1)){
        if(j > 0 && j <= ncol(mat)){ 
          if(!mat[i, j] %in% c(as.character(0:9), "."))
            return(TRUE)
        }
      }
    }
  }
  FALSE
}

mat_is_part <- matrix(nrow = nrow(mat), ncol = ncol(mat))

for(i in 1:nrow(mat)){
  for(j in 1:ncol(mat)){
    mat_is_part[i,j] <- cell_is_part(mat, i, j)
  }
}

# Extend the TRUEs to the adjoined numbers
for(i in 1:nrow(mat)){
  for(j in 1:ncol(mat)){
    if(mat_is_part[i,j]){
      jsearch <- j - 1
      while(jsearch > 0 && mat[i, jsearch] %in% as.character(0:9)){
        mat_is_part[i,jsearch] <- TRUE
        jsearch <- jsearch - 1
      }
      
      jsearch <- j + 1
      while(jsearch <= ncol(mat) && mat[i, jsearch] %in% as.character(0:9)){
        mat_is_part[i,jsearch] <- TRUE
        jsearch <- jsearch + 1
      }
      
    }
  }
}

mat[!mat_is_part] <- "."

num_str <- apply(mat, 1, paste0) |> 
  paste(collapse = "") |> 
  strsplit("\\.") |> 
  unlist()

num_str <- num_str[num_str != ""]

sum(as.integer(num_str))
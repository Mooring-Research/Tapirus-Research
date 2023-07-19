##---------------
##This script finds the 2 and 3 way combinations of fields or covariates for modeling
##Christian A. 19July2023
##---------------
covariates = c("Precip" , "Elev", "HFI", "Road",  "NDVI", "AvgMinTemp", "AvgMaxTemp", "NPP" ) 
prefix<- "MA_m."


duovariates<- function(fields, modPrefix){
  row_names<- fields
  col_names <- row_names
  
  # Create an empty matrix
  mat <- matrix(NA, nrow = length(row_names), ncol = length(col_names))
  
  # Set row and column names
  rownames(mat) <- row_names
  colnames(mat) <- col_names
  
  # Fill in the values based on the concatenation of row and column names
  for (i in 1:length(row_names)) {
    for (j in 1:length(col_names)) {
      if (!(col_names[j] == row_names[i])){
        mat[i, j] <- paste(prefix, row_names[i], col_names[j], ".pEff", sep= '')
      }
    }
  }
  
  mat[lower.tri(mat)] <- NA
  return(mat)
}

trivariates<- function(fields,modPrefix){
  # Create an empty 3-dimensional array
  n <- length(fields)
  mat <- array(NA, dim = c(n, n, n),
               dimnames = list(fields, fields, fields))
  
  # Fill in the values based on the concatenation of field names
  for (i in 1:n) {
    for (j in 1:n) {
      for (k in 1:n) {
        mat[i, j, k] <- paste(prefix,fields[i], fields[j], fields[k],".pEff", sep = '')
      }
    }
  }
  
  # Set lower triangular elements to NA to keep only unique combinations
  for (i in 1:n) {
    for (j in 1:n) {
      for (k in 1:n) {
        if (i > j || i > k || j > k) {
          mat[i, j, k] <- NA
        }
      }
    }
  }
  
  # View the resulting 3-dimensional matrix
  return(mat)
}

triMatrix<- trivariates(covariates)


sink("covCombinations.txt", append = FALSE)

cat("***MA covariate 2-way combinations\n")
duovariates(covariates)

cat("\n**MA covariate 3-way combinations**\n")
for(i in seq(length(covariates))){
  cat("depth", i, '\n')
  for (j in seq(length(covariates))){
    for (k in seq(length(covariates))){
      if (!is.na(triMatrix[i,j,k])){
        print(triMatrix[i,j,k])
      }
      
    }
  }
}
sink()

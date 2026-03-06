mat <- matrix(1 : 9) # a matrix of 1 row and 9 column with number 1 to 9 by default

mat

mat <- matrix(1 : 9, nrow = 3, ncol = 3) # a 3 by 3 matrix with bycol = TURE

mat

mat <- matrix(1 : 9, byrow = TRUE, nrow = 3) # Same as above

mat

rowSums(mat) # adding each row 
colSums(mat) 

rowMeans(mat) # compute the mean for each row
colMeans(mat) 
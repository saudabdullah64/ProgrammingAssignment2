############################################
###########################################
## We need to write two functions. Following the example given in text
## we create first function called "makeCacheMatrix" that would create
## a matrix that will be used to hold cache of it own inverse.
## The second function called "cacheSolve" will compute the inverse of
## the matrix created in first funtion. In addition, this function
## will also check that if the inverse is already exists in the "parent"
## environment then this function will retrieve the inverse from the
## parent environment.
#############################
#############################

## Please see above comments describing this function

makeCacheMatrix <- function(mat1 = matrix()) {
  
  i <- NULL
  set <- function(mat2){
    mat1 <<- mat2
    i <<- NULL
  }
  
  get <- function() mat1
  setInver <- function(inver) i <<- inver
  getInver <- function() i
  list(set = set, get = get,
       setInver = setInver,
       getInver = getInver)
}


## Please see above comments for the description of this funtion
## Returns a matrix that is the inverse of 'mat1'

cacheSolve <- function(mat1, ...) {
  i2 <- mat1$getInver()
  if(!is.null(i2)) {
    message("the inverse is already exists and now getting cached data")
  return(i2)
}
invdata <- mat1$get() 
i2 <- solve(invdata)
mat1$setInver(i2)
i2
}
#################################

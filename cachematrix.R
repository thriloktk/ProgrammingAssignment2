## Creating a "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  setMatrix <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  getMatrix <- function() x                              
  setInverse <- function(inverse) inverseMatrix <<- inverse  
  getInverse <- function() inverseMatrix                     
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}

## Computes the inverse of the matrix returned the above function or return the matrix if inverse is already calculated

cacheSolve <- function(x, ...) {
  inverseMatrix <- x$getInverse()
  if(!is.null(inverseMatrix)) {                       
    message("Getting Cached Data")   
    return(inverseMatrix)                             
  }
  MatrixData <- x$getMatrix()                     
  inverseMatrix <- solve(MatrixData, ...)             
  x$setInverse(inverseMatrix)                         
  inverseMatrix                             
}
##Close

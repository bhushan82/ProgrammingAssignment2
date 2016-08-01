## Matrix inversion is usually a costly computation hence this special matrix will 
## cache the inverse of a matrix rather than compute it repeatedly untill original matrix change

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setmatinv <- function(invMatrix) i <<- invMatrix
  getmatinv <- function() i
  list(set = set, get = get,
       setmatinv = setmatinv,
       getmatinv = getmatinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
  i <- x$getmatinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setmatinv(i)
  i
}

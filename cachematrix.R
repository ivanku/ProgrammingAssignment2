# Matrix inversion is usually a costly computation and there is a benefit 
# to caching the inverse of a matrix rather than computing it repeatedly.
# The following pair of functions cache the inverse of a matrix
# NOTE: This implementation assumes that matrix is always invertible

# Example:
# m <- matrix(c(4,3,3,2), nrow=2, ncol=2, byrow=T)
# c <- makeCacheMatrix(m)
# cacheSolve(c)

# This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # function to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }

  # function to get the matrix
  get <- function() x

  # function to set the inverse matrix
  setInverse <- function(inverse) inv <<- inverse

  # function to get the inverse matrix
  getInverse <- function() inv

  # return the list of functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) 
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
# retrieves the inverse from the cache
cacheSolve <- function(x, ...) {
  # check if we already have calculated inverse
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # if inverse has not been calculated yet - do it now and save in cache
  data <- x$get()
  message("no cache found - calculating inverse")
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv   
}

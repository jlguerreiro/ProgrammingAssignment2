## Define a pair of functions that compute and cache the inverse of a matrix.

## Function makeCacheMatrix takes a matrix as input 
##and outputs a matrix object which is a list of three functions.
##The function get outputs the matrix, the function setinverse sets the inverse of the matrix and
#the function getinverse outputs the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function cacheSolve takes as input an object produced by the function above and
## returns the inverse of the matrix associated to it. It starts by checking whether the inverse matrix
## has already been computed. If that is the case it returns the cached inverse matrix.
## Otherwise, it does the inverse matrix computation using solve and saves it in the cache via setinverse.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

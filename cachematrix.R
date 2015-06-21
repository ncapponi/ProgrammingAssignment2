## A pair of functions to compute the inverse of a matrix and cache it, in order to avoid 
# another costly computation once it has already been calculated.

## Creates a special "matrix" object, which allow to cache its inverse 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, 
       get = get,
       setinverse = setinverse, 
       getinverse = getinverse)
}

## Calculates the inverse of the special "matrix" created with the above function. 
## If inverse is already available in the cache, no computation is necessary.
## Otherwise inverse is computed and put in the cache. 
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached inverse result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

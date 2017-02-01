## This is the second programming assignment in the R programming fundemntals course
## The functions will store an inverted matrix in the cache for faster computing

## Creates a unique "Matrix" which stores itself and It's inverse


makeCacheMatrix <- function(x = matrix()) {
  my.inverse <- NULL
  set <- function(y) {
    x <<- y
    my.inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) my.inverse <<- inverse
  getInverse <- function() my.inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Return the inverse of a cachedMatrix from the cache, or compute it incase it's not stored

cacheSolve <- function(x, ...) {
  my.inverse <- x$getInverse()
  if(!is.null(my.inverse)) {
    message("getting cached data")
    return(my.inverse)
  }
  my.matrix <- x$get()
  my.inverse <- solve(my.matrix , ...)
  x$setInverse(my.inverse)
  my.inverse
}

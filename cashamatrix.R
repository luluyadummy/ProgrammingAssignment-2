## These two functions create a special matrix that can cashe the inverse and then
## compute the inverse; if the inverse has already been calculated, the result will
## be retrieved.

## this function cashes the inverse of its own
  makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  }

## this function calculates the inverse, and if the inverse has already been calculated
## it will retrieve the inverse
  cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if (!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
  }

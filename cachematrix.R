## Sometimes it may take too long to compute the inverse of the given matrix.
## If the contents of matrix are not changing, it may make sense to cache the inverse matrix.
## Thus that when we need it again, it can be looked up in the cache rather than recomputed.

## Creates a list containing a function to set and get the value of the matrix and it's inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y)
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Returns a matrix that is the inverse of 'x', but computes it only when 'x' has been changed.
## Otherwise the inverse from the cache will be retrieved.

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
       if(!is.null(i)) {
               message("getting cached data")
               return(i)
       }
       data <- x$get()
       i <- sole(data)
       x$setInverse(i)
       i
}


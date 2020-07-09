## Caching the Inverse Matrix of a Matrix

## The makeCacheMatrix function creates a list of functions setting and 
## getting the value of matrix and its inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) i <<- inverse
      getinverse <- function() i
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## CacheSolve calculates the inverse of the matrix using 
## the list created from the above function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      i <- x$getinverse()
      if (!is.null(i)) {
            message("getting cached data")
            return (i)
      }
      matrix <- x$get()
      i <- solve(matrix, ...)
      x$setinverse(i)
      i
}

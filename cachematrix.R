#makeCacheMatrix does 4 things:
#Sets the value of a matrix
#Gets the value of a matrix
#Sets the value of the inverse of the matrix
#Gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


#cacheSolve works with the matrices defined by makeCacheMatrix to
#solve for and return the inverse of the matrix if it has not yet been calculated, or
#return the cached value of the matrix if it has already been calculated

cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
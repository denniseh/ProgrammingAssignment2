## Invert a matix and keep a cache
## Matrix inversion can be expensive, so if we need to have
## a matrix and its inverse handy for repeated operations, 
## using this cache will speed operations.
## Use: cachedMatrix <- makeCacheMatrix(m)
##      inverse <- cacheSolve(cachedMatrix)
## Here, we can repeat the last line as many times as needed 
## without using much user cpu.

## makeCacheMatrix creates a vector of functions that 
##  get and set the value of the matrix,
##  get and set the value of the inverse
## Call it with a matrix that we will store 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(mean) m <<- mean
    getinverse <- function() m
    list( set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## cacheSolve takes the vector created by makeCacheMatrix
## and tests to see if the matrix has already been solved.
## if already solved, it returns the cached value
## if not already solved, does the operation, caches the value 
## and returns it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}


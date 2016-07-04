## These functions take an invertable matric and either solve for the inverse
## or get the inverse matrix from the cache if the inverse has already been solved

## The first function, makeCacheMatrix creates a special "vector"
## which is really a list containing a function to:
  ## set the value of the matrix
  ## get the value of the matrix
  ## set the value of the inverse matrix
  ## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
  }


## The following function calculates the inverse matrix of the special "matrix" created with the above function.
## However, it first checks to see if the inverse matrix has already been calculated.
## If so, it gets the inverse matric from the cache and skips the computation.
## Otherwise, it calculates the inverse matrix of the matrix and sets the value of the inverse matrix in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}

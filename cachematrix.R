## Functions below calculate the inverse of a matrix and allow
## caching the results to reduce calculating time in case if reuse.
##
## Course assignment 2
## 20 January 2016
## Paul van der Kooy
##
## Function can be tested by the following commands:
##
## mat <- makeCacheMatrix(rbind(c(1, -1/4), c(-1/4, 1)))
## > cacheSolve(mat)
## [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## > cacheSolve(mat)
## getting cached matrix
## [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

## Create a matrix cache to store the inverse matrix for later use

makeCacheMatrix <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  }

## Return a matrix that is the inverse of 'x'
## Only calculate when the inverse is not in cache, else use the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}  
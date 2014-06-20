## The functions create a matrix and calculate its inverse. The matrix caches
## its inverse to avoid re-computation. 

## creates a Matrix with the ability to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## computes the inverse of matrix and stores it in the cache
## if the inverse is available in the cache, it is returned from cache

cacheSolve <- function(x, ...) {
  
  ## returns a matrix that is the inverse of 'x'
  
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  
  s
}





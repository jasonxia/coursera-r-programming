## The functions caching the Inverse of a Matrix

## Create a cachable matrix
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  get <- function() x
  
  setSolve <- function(solve) s <<- solve
  
  getSolve <- function() s
  
  list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


## Retrieve solve from cachable matrix
cacheSolve <- function(x, ...) {
  s <- x$getSolve()
  
  if(!is.null(s)) {
    message("getting cached solve")
    return(s)
  }
  
  data <- x$get()
  s <- solve(data) %*% data
  x$setSolve(s)
  s
}

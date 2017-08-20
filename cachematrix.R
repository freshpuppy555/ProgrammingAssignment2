## Together the functions cache a special 'matrix' and return the inverse of that matrix. If the inverse of the 
## matrix has already been solved then the inverse is taken from the cache.

## makeCacheMatrix function takes an input and caches it as a special 'matrix'.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x 
  setInv <- function(INV) inv <<- INV
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve calculates the inverse of the 'matrix' defined in makeCacheMatrix using solve(). If the inverse 
## has already been solved then the function pulls the answer from the cache.

cacheSolve <- function(x, ...) {
  
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}

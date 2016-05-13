## Matrix inversion is usually a costly computation and there may 
## be some benefit to caching the inverse of a matrix rather than 
## compute it repeatedly

# The first function, makeCacheMatrix creates a special "matrix" 
# object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  # set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # get the value of the matrix
  get <- function() x
  
  # set the value of the inverse of the matrix
  setInverse <- function(solve) m <<- solve
  
  # get the value of the inverse of the matrix
  getInverse <- function() m
  
  list(set = set,get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# cacheSolve: This function computes the inverse of the special 
# "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has 
# not changed), then the cachesolve should retrieve the inverse 
# from the cache.


cacheSolve <- function(x, ...) {
  
    m <- x$getInverse()
    
    if(!is.null(m)) {
    message("getting cached data")
    return(m)
    }
    
    data <- x$get()
  
    m <- solve(data, ...)
  
    x$setInverse(m)
    
    m
}

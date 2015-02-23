## makeCacheMatrix: This function creates a special "matrix" object that can
##   cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## Cleaning the object for the inverse matrix caching
  inverse <- NULL
  
  ## Setting the matrix and cleaning the object for the inverse matrix caching
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  ## Getting the matrix
  get <- function() x
  
  ## Setting the inverse matrix cache
  setinverse <- function(inv) inverse <<- inv
  
  ## Getting the inverse matrix from cache
  getinverse <- function() inverse
  
  ## Return the list of defined functions
  ##   This makes possible to call them from the outside of this function
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" 
##   returned by makeCacheMatrix above. If the inverse has already been
##   calculated (and the matrix has not changed), then the cachesolve should
##   retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Get the inverse matrix from the cache
  inverse <- x$getinverse()
  
  ## Check if the cache is empty
  if(!is.null(inverse)) {
    ## The inverse is in the cache
    message("getting cached data")
    ## Return the inverse fetched from cache
    return(inverse)
  }
  
  ## Get the matrix
  data <- x$get()
  
  ## Calculate the inverse
  inverse <- solve(data)
  
  ## Store the inverse matrix in the cache
  x$setinverse(inverse)
  
  ## Return the calculated inverse matrix
  inverse
}

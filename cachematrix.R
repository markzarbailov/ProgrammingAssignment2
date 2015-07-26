## The following is a pair of functions that cache and compute the inverse of a matrix.
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(matx = matrix()) 
  {
    inverse <- NULL
    set <- function(x) 
      {
        matx <<- x
        inverse <<- NULL
      }
  
    get <- function() 
      matx
    setinv <- function(inv) 
      inverse <<- inv
    getinv <- function() 
      inverse
  
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix function above. 
## If the inverse has already been calculated (and matrix has not been changed), then cacheSolve function  
## retrieves the inverse from the cache.

cacheSolve <- function(matx, ...) 
  {
    inverse <- matx$getinv()
      if(!is.null(inverse)) 
        {
          message("Getting cached data...")
        inverse
        }
    data <- matx$get()
    invserse <- solve(data, ...)
    matx$setinv(inverse)
  
    inverse
}

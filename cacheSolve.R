## cacheSolve is a function that works with the makeCacheMatrix 
## object to solve the inverse of the matrix.
## If the inverse has already been computed, it will return the 
## cached value. 

## define cacheSolve function - arg x is makeCacheMatrix object
cacheSolve <- function(x, ...) {

## set value of matrixCache = getInverse of x object    
  matrixCache <- x$getInverse()
## Check matrixCache != null. If not null, return cached data
  if(!is.null(matrixCache)) {
    message("getting cached data")
    return(matrixCache)
  }
## If matrixCache was null, set data = get of x object  
  data <- x$get()
## solve the matrix
  matrixCache <- solve(data, ...)
## assign result to setInverse of x
  x$setInverse(matrixCache)
## Return the inverse
  matrixCache
}

## makeCacheMatrix is a function that takes a matrix (x), 
## calculates the inverse of x and stores the inverse for future use.
## makeCacheMatrix returns a list of functions as below:
##   set =  set the value of the matrix
##   get =  return value of the matrix
##   setInverse = set the value of the inverse matrix
##   getInverse = return the value of the inverse matrix

##  The makeCacheMatrix is a function that has arg x, x is classed as numeric
makeCacheMatrix <- function(x = numeric()) {
  
  ## initialize matrixCache (stored inverse matrix) to null  
  matrixCache <- NULL
  
  ## Define set function
  set <- function(newMatrix) {
    ## set assigns x the value of the arg newMatrix and clears the stored inverse
    x <<- newMatrix
    matrixCache <<- NULL
  }
  
  ##  Define get function = return x
  get <- function() x
  
  ##  Define setInverse - Assign value from solve to matrixCache
  setInverse <- function(solve) matrixCache <<- solve
  
  ## Define getInverse - Return value of matrixCache  
  getInverse <- function() matrixCache
  
  
  ## Return of makeCacheMatrix = list of functions and values
  list(
    set = set, 
    
    get = get,
    
    setInverse = setInverse,
    
    getInverse = getInverse)
  
}


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

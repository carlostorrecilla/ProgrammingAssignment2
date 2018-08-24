## Coursera R programming week three assignment

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  #Create set function
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  #Create get funtion
  get <- function() x
  
  #Create the set inverse function
  setInverse <- function(solve) i <<- solve
  
  #create the get inverse function
  getInverse <- function() i
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  #Obtain the value in the cache
  i <- x$getInverse()
  #Evaluate the value
  if(!is.null(i)) {
    #If value is not null recuperate the value from cache
    message("getting cached data")
    return(i)
  }
  #If value is null, calculate it and store it in cache
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}

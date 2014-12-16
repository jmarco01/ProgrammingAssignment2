
## The makeCacheMatrix() function creates a special "matrix" object that can cache its inverse
## Internally, it actually returns a list of 4 functions which will serve that purpose:
## 1. set: sets the value of a matrix variable (x) and initialises its inverse to null
## 2. get: returns the value of the matrix variable (x)
## 3. setinv: sets the value of the inverse matrix variable in cache (cached_inv_x)
## 4. getinv: returns the value of the inverse matrix variable from cache (cached_inv_x)
##
## The argument x passed to this function is expected to be an invertible square matrix

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialise the variable cache_inv_x to null
  ## This variable will be used to cache the inverse matrix of the argument x later on
  cached_inv_x <- NULL
  
  ## Function 'set'
  ## Argument 'y' is a square matrix
  ## The scoping operator here, modifies variables on the parent environment
  set <- function(y) {
    x <<- y
    cached_inv_x <<- NULL
  }
  
  ## Function 'get'
  ## No arguments
  ## Simply returns the argument x
  get <- function() x
  
  ## 3: Function 'setinv'
  ## Takes one argumet, being the inverse matrix which is to be cached
  ## Places that inverse matrix in the parent environment using the scope operator
  setinv <- function(inv_x) cached_inv_x <<- inv_x
  
  ## 4: Function 'getinv'
  ## Takes no argumes
  ## Returns the value stored in the cache 
  getinv <- function() cached_inv_x
  
  ## The returned value (last statement), is the list containing the above 4 functions
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## The cacheSolve() function returns the inverse of a matrix x, which is passed to makeCacheMatrix()
## cacheSolve() takes one argument also called X, which is a list of functions returned by makeCacheMatrix()
## The first time it is called with a new argument, it calculates its inverse, caches the result, and returns it
## On sUbsequent calls with the same argument, instead of re-calculating the inverse matrix, it returns the cached value
cacheSolve <- function(x, ...) {
  
  ## First try to read the value from cache into a local variable
  inv_x <- x$getinv()
  
  
  ## If the cache was previously populated, the previous clause will get a non-null value
  ## The function will return that cached value to the calling environment and avoid another inverse matrix computation
  if(!is.null(inv_x)) {
    message("Getting cached data")
    return(inv_x)
  }
  
  
  ## If the cache was empty (ie. we received a null value from x$getinv()), we must compute the inverse matrix, 
  ## store it in cache for future calls, and return it to the calling environment
  
  ## The local variable 'data' declared here, is a matrix populated with the x$get call
  ## This assigns our original matrix'x' to 'data'
  data <- x$get()
  
  ## Calling the solve() function, R returns inverse matrix of 'data' and we store it in the local variable 'inv_x' 
  inv_x <- solve(data)
  
  ## Before exiting the function call, we need to store the result in cache
  ## The statement below caches the inverse matrix for future calls
  x$setinv(inv_x)
  
  ## Return the inverse matrix to the calling environment and end
  inv_x
  
}
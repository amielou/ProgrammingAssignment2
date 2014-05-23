## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a getter setter wrapper which takes the 
## matrix of numbers passed and returns a list of values and functions
## for use by the CacheSolve method

makeCacheMatrix <- function(x = matrix()) {

  #This function initializes a special "matrix" object that can cache the inverse of the matrix
  #passed in to makeCacheMatrix.
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse ## saves to cache
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Instead of directly calling the above objects getinverse method,
## calling the cacheSolve function will check first to see if it 
## has already been calculated and simply reuse the value


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  #: This function computes the inverse of the special makeCacheMatrix function above 
  #If the inverse has already been calculated, then the cachesolve will retrieve the inverse from the cache.
  i <- x$getinverse()
  if(!is.null(i)) {
    ###if i is already known (display message; break out of method; and return value of i)
    message("getting cached data")
    return(i)
    
  }
  data <- x$get()
  i <- solve(data) ## calculate the inverse of the matrix
  x$setinverse(i)  ##sets the parent x varible to the inversed matrix
  i 
  
}



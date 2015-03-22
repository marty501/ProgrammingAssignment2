# The first function makes a cacheable matrix with methods for getting and setting
# the inverse
# The cacheSolve function serves as an interface of makeCacheMatrix (the cacheable matrix)
# and checks and returns a cached inverse of the matrix if it exists or else it calculates
# it and caches the new value

## Creates a cacheable matrix - a matrix that stores its inverse matrix in the i variable
makeCacheMatrix <- function(x = matrix()){
    i <- NULL
    set <- function(y){ # setter: set x to be the matrix and removes any cached inverse value
      x <<- y
      i <<- NULL
    }
    get <- function() x    # accessor to the matrix
    setinv <- function(inv) i <<- inv
    getinv <- function() i # getter for the inverse
    list(set = set, get = get, 
         setinv = setinv,
         getinv = getinv)
}

## This function is an "interface" to the makeCacheMatrix function and
## returns a cached inverse value if it exists or else it calculates it and caches it
cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
      i <- x$getinv() # Get the inverse matrix
      if (!is.null(i)) # if it's not null - return it directly
      {
        message("getting cached data")
        return (i)
      }
      i <- solve(x$get()) # get the matrix and pass it to solve to get the inv matrix
      x$setinv(i) # cache the inverse matrix and 
      i           # return it
}

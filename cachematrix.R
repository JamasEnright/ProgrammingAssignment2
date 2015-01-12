## This function will return a copy of the inverse of the matrix stored in x
## where a cached version will be used if it has previously been calculated

## This function sets up the 'matrix' and will store and provide its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## set up basic variables.
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  ## provide the inverse cache funcionality.
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  ## set up the accessible functions as column variables
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This manupilates the matrix in x, and will create an inverse of x once
## otherwise provide a previously cached copy

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## test to see if this matrix already has an inverse
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## otherwise create and store the inverse
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

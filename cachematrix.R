## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverted <- NULL                                       ## set inverted to null
  set <- function(y) {                                   ## set inverted to null and x to y
    x <<- y
    inverted <<- NULL
  }
  get <- function() x                                    ## return the matrix
  setinverted <- function(inverted) inverted <<- solve   ## calculate inverse of the matrix
  getinverted <- function() inverted                     ## return the inverse
  list(set = set, get = get,
       setinverted = setinverted,
       getinverted = getinverted)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix
## has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inverted <- x$getinverted()                            ## retrive the inverse of the matrix
  if(!is.null(inverted)) {                               ## check if iverse was calculated before
    message("getting cached data")
    return(inverted)                                     ## if so - return inverse
  }                                                      ## otherwise
  data <- x$get()                                        ## retrive the matrix
  inverted <- solve(data, ...)                           ## calculate inverse of the matrix
  x$setinverted(inverted)                                ## save it within the matrix variable
  inverted                                               ## return the inverse
}

## Corusera Specialization Week 3 - Programming assignment.
## Create functions that CACHE the Inverse of a matrix.

## --------------------------------------------------------
## makeCacheMatrix: prepare List function objects. 
##                  It keeps the passed information becausse 
##                  of the Lexical Scoping. 
## --------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL;
  set <- function(y) {
    x <<- y;
    inverse <<- NULL;
  }
  get <- function() x
  setinverse <- function(matrixinverse) inverse <<- matrixinverse;
  getinverse <- function() inverse;
  return(list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse));
}


## --------------------------------------------------------
## makeCacheMatrix: resolve the matrix stored in the makeCacheMatrix object. 
##                  Set the inverse matrix if it is the first time that is calculated.
## --------------------------------------------------------
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrixinverse <- x$getinverse();
        if(!is.null(matrixinverse)) {
          message("getting cached data");
          return(matrixinverse);
        }
        data <- x$get()
        matrixinverse <- solve(data, ...);
        x$setinverse(matrixinverse);
        return(matrixinverse);
}

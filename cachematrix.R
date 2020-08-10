## Assumption: all matrices presented to the function pair are of a square size
## and indeed invertible.

## This is a helper function that creates a "special matrix".
## Output: list of 4 functions used in the 2nd function to cache the inverse
## of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setinverse <- function(inverse){inv <<- inverse}
  getinverse <- function(){inv}
  #the below is the return value of this function.
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function computes the inverse of the special "matrix"  returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if (!is.null(inv)){
          message("getting catched data")
          return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinverse(inv)
        inv
}

#testing:
#dummymatrix <- matrix(c(0,1,-3,-3,-4,4,-2,-2,1),3,3)
#CacheSolve(makeCacheMatrix(dummymatrix))
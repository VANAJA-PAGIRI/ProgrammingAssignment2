## This program is for Caching the Inverse of a Matrix
## here I  writing a pair of functions that cache the inverse of a matrix.

## This makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y){
    x <<-y
    inver <<-NULL
  }
  get <- function()x
  setInverse <- function(solveMatrix) inver <<-solveMatrix
  getInverse <- function() inver
  list(set=set, get=get , setInverse=setInverse, getInverse=getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then cacheSolve should retrieve the inverse from the cache.
# solve() returns inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inver <- x$getInverse()
  if(!is.null(inver)){
    message("getting cached data")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data)
  x$setInverse(inver)
  inver
}

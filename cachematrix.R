## Matrix inversion is a time consuming and costly process so there are some benefits to caching the inverse of a matrix over calculating it repeatedly.
## These functions cache the inverse of a matrix.

## The function makeCacheMatrix creates a matrix object which really contains the function to
## 1.Set the value of the matrix
## 2.Get the value of the matrix
## 3.Set the value of the inverse of the matrix
## 4.Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    ## use `<<-` to assign a value to an object in an environment
    ## different from the current environment.
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## The function cacheSolve computes the inverse of the matrix. If the inverse has been calculated
## and has not changed the function retrieves the inverse from the cache.If it has not been calculated
## it computes the inverse, sets the value in the cache
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}

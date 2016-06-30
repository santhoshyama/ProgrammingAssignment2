## The below function caches the inverse of a matrix based on the matrix provided. If the existing matrix has not changed, 
## the function cachesolve will retrieve the inverse of matrix from cache instead of recreating the same.

## This function takes input a square invertible matrix and creates a list of functions to get & Set the matrix and the inverse
## of the same.

makeCacheMatrix <- function(x = matrix()) {
  inv_mat<-NULL
  set <- function(y) {
    x <<- y
    inv_mat <<- NULL
  }
  get <- function() x
  setinvmat <- function(solve) inv_mat <<- solve
  getinvmat <- function() inv_mat
  list(set = set, get = get,
       setinvmat = setinvmat,
       getinvmat = getinvmat)
}


## This function calls the cache created in the previous function and checks if there is already a matrix inverse available 
## and retrieves the same once found. Else, it will recreate the same and prints it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_mat <- x$getinvmat()
  if(!is.null(inv_mat)) {
    message("getting cached data")
    return(inv_mat)
  }
  data <- x$get()
  inv_mat <- solve(data, ...)
  x$setinvmat(inv_mat)
  inv_mat
}

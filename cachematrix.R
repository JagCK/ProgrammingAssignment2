## Below 2 functions will create and cache the inverse of an matrix 
## if the inverse hasn't been computed. If already computed, will return
## the cached inverse matrix.

## makeCacheMatrix function creates the cache for the inverse matrix. 
## It returns a list of functions.'xi' caches the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  xi<-NULL
  set <- function(y) {
    x <<- y
    xi <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) xi<<- inv
  getinverse <- function() xi
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve function checks to see if an inverse matrix was already computed
## and cached. If not, it will compute the inverse and call a function to 
## cache the inverse matrix. This function assumes all input matrices to be
## invertible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  xi <- x$getinverse()
  if(!is.null(xi)) {
    message("Getting cached data")
    return(xi)
  }
  input<-x$get()
  xi<-solve(input)
  x$setinverse(xi)
  xi
}

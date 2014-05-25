## The following functions store matrix objects and cache their inverse. So, you can create a
## makeCacheMatrix object, set a matrix, then use cacheSolve to produce the inverse of the matrix.
## cacheSolve will either grab the cached inverse if it already exists or compute the inverse
## and store the result in the cache.


## creates a matrix object with getter and setter definitions. The inverse is initially set to NULL.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## returns the inverse of a matrix object. It either returns the cached inverse or computes it, then caches it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}


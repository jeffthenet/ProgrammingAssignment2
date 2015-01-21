# Matrix inversion is usually a costly computation so that there are benefits to caching 
# the inverse of a matrix rather than compute it repeatedly 
# These functions allow to compute and cache the inverse of a matrix.

# makeCacheMatrix function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
   #inv will store the chached inverted matrix, initialyzed to null
   invcached <- NULL

   # setting the initial matrix and thus invalidate the cache
   set <- function(y) {
      x <<- y
      invcached <<- NULL
   }
   
   # return the initial matrix
   get <- function() x
  
   # set the cached inverted matrix
   setinverse <- function(inverse2cache) invcached <<- inverse2cache

   # get the cached inverted matrix
   getinverse <- function() invcached
  
  # return the special matrix to be used by the cachesolve function
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
   # check if there is a cached version of the inverse, and if yes return it
   inv <- x$getinverse()
   if(!is.null(inv)) {
      message("getting cached inverted matrix")
      return(inv)
    }
  
   # no cache, so calculate the inverse using the solve function
   # and return the inverted matrix
   data <- x$get()
   inv <- solve(data, ...)
   # set the cache for next time
   x$setinverse(inv)
   return (inv)
}


# test code
# m <- makeCacheMatrix(matrix(c(2,0,0,0,1,0,2,2,2), 3, 3))
# inv <- cacheSolve(m) # compute the inverse
# inv <- cacheSolve(m)  # getting the cached inverse
# inv should be equal to matrix(c(0.5,0,0,0,1,0,-0.5,-1,0.5))





## This project consists of two functions makeCacheMatrix which creates a special "matrix" object that can cache its inverse and cacheSolve that computes the inverse of the special matrix returned by makeCacheMatrix
 

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
               j <- NULL
+              set <- function(y) {
+                       x <<- y
+                       j <<- NULL
+               }
+               get <- function() x
+               setInverse <- function(inverse) j <<- inverse
+               getInverse <- function() j
+               list( get = get, setInverse = setInverse, getInverse =getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          j <- x$getInverse()
+         if(!is.null(j)) {
+                  message("getting cached data")
+                  return(j)
+         }
+         data <- x$get()
+         j <- inverse(data, ...)
+         x$setInverse(j)
+         j
}

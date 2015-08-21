## cacheSolve will use function found in makeCacheMatrix to either calculate or return 
## the previous calculated inverse matrix.

## This function creates a list that contains 4 functions - setmatrix, getmatrix, setinverse, and getinverse 
 makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   setmatrix <- function(y) {
     x <<- y
     inv <<- NULL
   }
   getmatrix <- function () x
   setinverse <- function(solve) inv <<- solve
   getinverse <- function() inv
   list (setmatrix = setmatrix, getmatrix = getmatrix, 
         setinverse = setinverse, 
         getinverse = getinverse)
 }
 
 
 ## This function gives a return of the inverse variable. If the inverse variable has
 ## already been calculated, it willgive us the previous value that we calualted.
 ## If the inverse vairable has not been calcualted, it will go to the matrix,
## and return the inverse.
 cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   inv <- x$getinverse()
   if(!is.null(inv)) {
     message("getting cached data")
     return(inv)
   }
   data <- x$getmatrix()
   inv <- solve(data, ...)
   x$setinverse(inv)
   inv
}
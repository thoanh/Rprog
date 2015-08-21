 makeCacheMatrix <- function (x = matrix()){
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

 cacheSolve <- function(x, ...) {
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
## The two functions makeCacheMatrix and cacheSolve enable to cash and to
##   compute the inverse of a matrix x.

## The first function, makeCacheMatrix, takes as input variable a matrix x.
##   It returns a function-list of x with four functions as elements. 
## The get function returns the matrix x. 
## The set function takes as input a matrix y and replaces x by y, thereby 
##   causing the get function to return y in the future.
## The getinverse function returns NULL until the setinverse function was 
##   used. After that it returns the matrix i that was set by the setinverse 
##   function.
## The setinverse function takes a matrix 'inverse' as input and sets it as 
##   the matrix i that can be retrieved with the getinverse function.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function, cacheSolve, takes as input a function-list that was returned
##   for some matrix x by the makeCacheMatrix function. It returns the value
##   of the getinverse function of that list, unless that value is NULL. In
##   that case it computes and returns the inverse of x. Cachesolve asssumes
##   that x is always invertible.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        } 
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

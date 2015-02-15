## The following two functions caches and computes the inverse of  matrix
## 

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(mx){
		x <<- mx
		inv <<- NULL
	}
	get <- function() return(x);
	setinv <- function(inve) inv <<- inve;
	getinv <- function() return(inv);
	return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}



## This function computes the inverse of the matrix retuen by the makeCacheMatrix function. If the inverse has already been calculated then this function would fetch the inverse from cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)){
        	message("Getting Cached data.")
        	return(inv)
        }
        t <- x$get()
        inv <- solve(t, ...)
        x$setinv(inv)
        return(inv)
}

##
##  This function use the <<- operator which to assign a 
##  value to an object in an environment that is different
##  from the current environment. 
##  Below are two functions that are used to create a 
##   special object that stores a matrix and cache's its inverse.
##  The first function, makeCacheMatrix creates a special "matrix", 
##    which is really a list containing a function to
##     a) set the value of the matrix
##	   b) get the value of the matrix
##     c) set the value of the inverse matrix
##     d) get the value of the inverse matrix
##

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
						 x <<- y
						 inv <<- NULL
						}
	get <- function() x
    setInv <- function(inverse) inv <<- inverse
	getInv <- function() inv
	return(list(set=set, get=get, setInv=setInv, getInv=getInv))
}

## 
##  This function computes the inverse of the special "matrix" 
##	returned by makeCacheMatrix above. 
##	If the inverse has already been calculated 
##	(and the matrix has not changed), then the 
##	cachesolve should retrieve the inverse from the cache.
##  So the first time make the cache result
##
##

cacheSolve <- function(x, ...) {
	inverse <- x$getInv()
	
	if(!is.null(inverse)) {
	    message("Getting cached data!")
		return(inverse)
	}
	
	## get matrix from x parameter, that is 
	## environment variable.
	## execute solve command for  data
	inverse <- solve(x$get())
	
	## Return a matrix that is the inverse of 'x'
	return(x$setInv(inverse))
}

##
## Sample 
##  x = rbind(c(1, 2), c(3, 4))
##  x
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
##  m = makeCacheMatrix(x)
##  cacheSolve(m)
##  cacheSolve(m)
##  Getting cached data!
##      [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5
##

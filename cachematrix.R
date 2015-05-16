## These functions are made to cache potentially time-consuming computations.
## 

## The makeCacheMatrix function creates a special "vector",
## which is really a list containing a function to
# 1. set matrix
# 2. get matrix
# 3. set solve(matrix)
# 4. get solve(matrix)
makeCacheMatrix <- function(x = matrix()) {
	s <- NULL
	set <- function(y) {
		x <<- y
		s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
    	setsolve = setsolve,
    	getsolve = getsolve)
}


## cacheSolve function calculates invert of the special "vector" created with makeCacheMatrix
# if cached invert not existing it will create one and set using setsolve()

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	s <- x$getsolve()
	if(!is.null(s)) {
		message("getting cached data")
		return(s)
	}
	data <- x$get()
	s <- solve(data, ...)
	x$setsolve(s)
	s
}
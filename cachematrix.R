## The pair functions is to solve and cache the inverse of the matrix


## makeCacheMatrix is list of functions, which could set both stored matrix 
## and its cached inversed matrix and get the stored matrix and its cached 
## inversed matrix(if it is ever cached)

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
		set <- function(y){
			x <<- y
			m <<- NULL
		}
		get <- function() x
		setinv <- function(inv) m <<- inv
		getinv <- function() m
		list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cachSolve is to first check the cached space if it has an inversed matrix.
## If yes, it will return the cached inverse matrix; 
## if not, it will solve the inversed matrix, stores in the cached space and
## return it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
        	message("getting cached data")
        	return(m)
        	}
        data = x$get()
        m <- solve(data)
        x$setinv(m)
        m
        

}

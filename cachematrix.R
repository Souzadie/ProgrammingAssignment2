## Creating 2 functions
## The first one will manipulate the matrix
## The second one will invert and cache the matrix

## This function will have all the operations necessary to explore the matrix. 
## The exception is the invertion which happens using the command solve in the second function.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	# Set function is created to clean old values
	set <- function(y) {
                x <<- y
                i <<- NULL
        }
	# get is used in second function to retrieve the matrix's current values
	get <- function() x
	# Setinv used to cache inv matrix
	setinv <- function(solve) i <<- solve
	# Getinv used to get the current value, also used in the second function
	getinv <- function() i
	# List the availables functions 
	list(getinv = getinv, get = get, setinv = setinv, set = set )
}


## This function is responsible for invert or return cached result of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		i <- x$getinv()
		if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
		data <- x$get()
        i <- solve(data)
        x$setinv(i)
        i
}

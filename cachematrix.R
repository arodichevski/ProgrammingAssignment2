## This is an implementation of the cache for matrix inversion.
## The cache is the function object makeCacheMatrix which memorizes the source matrix
## and the inversed one. The source matrix is memorized during creation of the object.
## The inversed matrix is calculated only once, when it is requested the first time.
## The cacheSolve function takes as argument the cache function and yields the inverse matrix.
## The first call to cacheSolve does the matrix inversion.
## The following calls to cacheSolve with the same argument gets the result from the cache.
##
## Sample usage:
## > x <- matrix(rnorm(100), 10, 10)
## > y <- makeCacheMatrix(x)
## > z <- cacheSolve(y)
## (some code)
## > w <- cacheSolve(y)


## makeCacheMatrix - cache function with matrix and inverse matrix in it
## Argument: x - numeric square matrix (is supposed to be invertable)
## Returns: list of functions set (memorizes the input matrix), get (reads the memorized matrix),
##          setinv (memorizes the result matrix) and getinv (reads the memorized result matrix)

makeCacheMatrix <- function(x = matrix()) {
		# Initial valie for the inversed matrix
        i <- NULL
        # Prepare set, get , setinv and getinv access functions
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        # Return the list of access functions
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve - interacts with the cache function makeCacheMatrix
## Arguments: x - cache of type makeCacheMatrix
##            ... - additional arguments passed to the solve() matrix inversion function
## Returns: inverted matrix

cacheSolve <- function(x, ...) {
		## Try to get the cached matrix
        i <- x$getinv()
        ## Return the cached one
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## Otherwise calculate the inversion
        data <- x$get()
        i <- solve(data, ...)
        ## Memorize (cache) the inverted matrix
        x$setinv(i)
        ## Return the inverted matrix
        i
}

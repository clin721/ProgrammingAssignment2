## makeCacheMatrix can store a matrix and its inverse
## cacheSolve will compute the matrix inverse and cache the result

## makeCacheMatrix has 4 major functions: set, get, setinv, and getinv
## get/set will retrieve/store the matrix passed in by the parameter x
## similarly, getinv/setinv will retrieve/store the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## cacheSolve checks to see if there is already a cached inverse matrix 
#for the given matrix
## if there is no cached inverse, the inverse is calculated with the 
#solve() function and stored using the setinv() function created above
cacheSolve <- function(x, ...) {
    
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}

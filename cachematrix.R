## These two functions are used to create a special matrix object and
## then cache it. 

##We need the function like this, because sometimes
## make computation such matrix inversion is aboslutely expensive.

## If the value of the matrix does not change and we want to
## calculate the inverse, we can use previous result.

## makeCacheMatrix function will create special matrix object

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set,
         get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## cacheSolve function will display the calculation stored


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
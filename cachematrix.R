## The following two functions create a cache that stores a matrix and its inverse.
## First the mackeCacheMatrix function is called passing a matix. This function returns 
## a list containing the functions get, setinverse and getinverse. These functions are 
## defined within the makeCacheMatrix function. Passing these functions in a list preserves
## the passed matrix as well as the local valriable m.




makeCacheMatrix <- function(x = matrix()) {

        m <- NULL

        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list( get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The makecacheMatrix function will be used with cacheSolve function.  After the
## makecacheMatrix function is called and returns the list containg the functions, the
## cacheSolve function is called and passed the list that was returned from makecacheMatrix.
## This list allows the cacheSolve function to access the inital matrix passed to makecacheMatrix
## and to see if the inverse has been calculated and stored. If it has not been stored then
## the inverse is calculated and stored  and then returned. If it has already been calculated it
## will be returned. This function eliminates the need to repeatedly compute the inverse
## of a matrix if the inverse is needed more than once.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    m <- x$getinverse()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}


## There are two functions that work together here.
## The first one creates a special matrix object.
## Secondly, it allow us to get the inverse of the matrix.
## Saving this calculation in the cache.
## Therefore, if a computation was already performed
## it will not be repetead.

## It should be noted that the input matrix must be square.

## makeCacheMatrix, the first function defines the actions
## we can perform with matrixes.

makeCacheMatrix <- function(x = matrix()) {
        m <- matrix(NA)
        set <- function(m) {
                x <<- matrix(m, nrow = sqrt(length(m)), ncol = sqrt(length(m)))
                m <<- matrix(NA)
        }
        get <- function() x
        setInv <- function(inverse) m <<- inverse
        getInv <- function() m
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}

## Below is the second function, which brings us the inverse
## of a previous calculated matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m2 <- x$getInv()
        if(!is.na(m2[1,1])) {
                message("getting cached data")
                return(m2)
        }
        data <- x$get()
        m2 <- solve(data, ...)
        x$setInv(m2)
        print(m2)
}

## For the purporse of grading and testing the functions above you can write:

matrix <- makeCacheMatrix()
## Define some numbers of your choice, as in the example below
matrix$set(c(1,2,3,4))
## Let's test it...
matrix$get()
## Ok. And now it's inverse:
cacheSolve(matrix)
## This last one should give you the proof that it was the inverse:
matrix$get() %*% cacheSolve(matrix)

## Thanks for your time. Hope my notes were helpful!

## "makeCacheMatrix()" is a function that links an input
## matrix to a function, "cacheSolve()", that calculates
## the inverse of the input input matrix. If the inverse
## has already been calculated, when the "cacheSolve()" 
## function is called, the cached Inverse Matrix will be 
## retrieved. 

## The following generate square matrices with dimensions
## of 2 (M2), 3 (M3), 4 (M4) and 5 (M5). To determine
## their inverses, they may be submitted firstly to
## to "makeCacheMatrix()" and subsequently to
## "cacheSolve()" by copying and pasting the codes below
## into the R console.

## Step 0: Load the "makeCacheMatrix" and "cacheSolve"
## routines into R.

## Copy commented code below and paste into R console
## to test functions.

## Step 1: Create Input Matrices
## M2 <- matrix(sample(2^2), nrow = 2)
## M3 <- matrix(sample(3^2), nrow = 3)
## M4 <- matrix(sample(4^2), nrow = 4)
## M5 <- matrix(sample(5^2), nrow = 5)
##
## In steps below M4 is used as an example. Any square
## matrix should yield similar results
##
## Step 2
## Link an input matrix to the Inverse Function by
## executing the following code:
## mat4 <- makeCacheMatrix(M4)

## Step 3
## To print out the Inverse of input matrix execute
## the following code:
## cacheSolve(mat4)

## Step 4
## To test Inverse function, multiply the Input and
## Inverse matrices using the following 2 lines of code:
## mat4i <- cacheSolve(mat4)
## M4 %*% mat4i
## This should yield an Identity Matrix with the same
## dimensions as the Input Matrix.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
        set <- function(y) {
      	x <<- y
            i <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) i <<- solve
        getsolve <- function() i
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

cacheSolve <- function(x, ...) {
        i <- x$getsolve()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setsolve(i)
        i
}

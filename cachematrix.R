############################################
###     R Programming Assignment #2      ###
###    Caching the Inverse of Matrix     ###
############################################

## This code contains two functions. The first one (makeCacheMatrix) creates a special "matrix" object that can cache its inverse. 
## The second function (cacheSolve) computes the the inverse of a special matrix (returned by makeCacheMatrix). If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve will return the inverse from the cache.


########## CODE ############################

## The function below creates a special matrix object that can cache its inverse. It returns a list containing a function to
## 1) set the value of the matrix
## 2) get the the value of the matrix
## 3) set the value of the matrix's inverse
## 4) get the value of the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {                             # create function "makeCacheMatrix" to take an input argument "x" and coerce it to a matrix
        inv <- NULL                                                     # set variable "inv" (will assign the inverse to this variable) to NULL
        set <- function(y) {                                            # define a function "set" within this function to take an input argument "y"
                x <<- y                                                     # set "x" in a different environment from the current one equal to "y" 
                inv <<- NULL                                                # set "inv" in a different environment from the current one equal to NULL
        }
        get <- function() x                                             # define a function "get" within this function to retrieve the value of "x"
        setinv <- function(solve) inv<<-solve                           # define a function "setinv" within this function to solve for the inverse of the matrix and set its value
        getinv <- function() inv                                        # define a function "getinv" within this function to retrieve the inverse of the matrix
        list(set = set, get = get, setinv = setinv, getinv = getinv)    # create a list of the output functions and return as variable "x"
}


## The function below calculates the inverse of the special "matrix" created using the above function.
## First, it checks to see if the inverse has already been calculated. If it has, the inverse is retrieved from the cache and the computation is skipped.
## If it has not, then it calculates the inverse and sets the value of the inverse in the cache using the "setinv" function.

cacheSolve <- function(x, ...) {                                       # create function "cacheSolve" to take a variable number of input arguments, including the special matrix "x"
        inv <- x$getinv()                                              # assign "inv" the value returned by the "getinv" function within the special matrix "x"
        if(!is.null(inv)) {                                             # if "inv" is not null, then the inverse has been calculated and can be retrieved from the cache
                message("getting cached data")                             # print a message to let the user know the inverse has been found and is being retrieved
                return(inv)                                                # return the cached inverse value to "inv" and exit the function
        }                                                              # if "inv" is null, the function continues
        data <- x$get()                                                # use the "get" function within the special matrix "x" to assign the input matrix to "data"
        inv <- solve(data,...)                                         # solve for the inverse of matrix "x" and assign this value to "inv"
        x$setinv(inv)                                                  # use the "setinv" function within the special matrix "x" to set this value in the cache
        inv                                                            # return the variable "inv"
}

################################################################################
## Function: makeCacheMatrix                                                  ##
## This function is one of two functions that allow the user to make a        ##
## matrix and store it's inverse in cache for future use.                     ##
## First you must load makeCacheMatrix.R and cacheSolve.R.                    ##
## Next load a matrix into makeCacheMatrix, for example a matrix called       ##
##'input is loaded in using 'input<-makeCacheMatrix(input)' (without quotes). ##
## To caculate an inverse for matrix 'input' use 'cacheSolve(input)' again,   ##
## without quotes. If the inverse has been calculated already, the inverse    ##
## will be pulled from cache.                                                 ##
##                                                                            ##
################################################################################
#---5----|----15---|---25----|----35---|---45----|----55---|---65----|----75---#
################################################################################

makeCacheMatrix <- function(x = matrix()) {    ## This bit of code loads 2 blank
        m <- NULL                              ## matrices, x and x's inverse
        set <- function(y) {                   ## which is saved a m.   
             x <<- y                           ## For now, x and m are blank.
             m <<- NULL                        
        }
        get <- function() x                         ## This bit sets up a few 
        setsolve <- function(solve) m <<- solve     ## attributes in x: get, 
        getsolve <- function() m                    ## set, getsolve and 
        list(set = set, get = get,              ## setsolve. Another thing
             setsolve = setsolve,               ## stored in x is the input
             getsolve = getsolve)               ## matrix.
}                                               

################################################################################
## Function: cacheSolve                                                       ##
## This function is the second of two functions that allow the user to make   ##
## a matrix and store it's inverse in cache for future use.                   ##
## For futher information, see notes in makeCacheMatrix.R                     ##
##                                                                            ##
################################################################################
#---5----|----15---|---25----|----35---|---45----|----55---|---65----|----75---#
################################################################################

cacheSolve <- function(x, ...) {                ## This references the objects 
        m <- x$getsolve()                       ## made by makeCacheMatrix.R
        if(!is.null(m)) {                       ## On it's own this if statement
                message("getting cached data")  ## only references MakeCacheMatrix
                return(m)                       ## to see if the inverse is stored
        }                                       ## or not, and either returning  
        data <- x$get()                         ## a cached inverse or running and
        m <- solve(data, ...)                   ## storing an inverse.
        x$setsolve(m)                           ## For more help on this, google 
        m                                       ## object oriented programming.
}

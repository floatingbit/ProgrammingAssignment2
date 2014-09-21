
## makeCacheMatrix is a function which takes a matrix as input. It returns a 
## list of functions get, set, getInverse, setInverse using which the special
## matrix can be assigned matrix Inverses. We make use of <<- operator in the
## individual function definitions in order to assign values to variables that
## are not in the current environment (ie. that of the function)
makeCacheMatrix <- function(x = matrix()) {
    m_inverse <- NULL
    get <- function() x
    set <- function(input_matrix) {
        x <<- input_matrix
        m_inverse <<- NULL
    }
    getInverse <- function() m_inverse
    setInverse <- function(m_inv) m_inverse <<- m_inv
    
    list(set = set, get = get, getInverse = getInverse, setInverse = setInverse)
}


## cacheSolve takes special matrix as an argument and checks to see if its 
## inverse exists else the inverse is calculated using solve() and returned
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)){
            print ("Getting cached inverse")
            return (inv)
        }
        
        x$setInverse(solve(x$get(), ...))
        x$getInverse()        
}

## Below are two functions that are used to create a
## special object that caches a matrix and its inverse, 
## and solves a matrix for its inverse
 

## makeCacheMatrix stores and returns a matrix and its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL #initialize i
        
        set <- function(y) {
                x <<- y         #store matrix
                i <<- NULL      #initialize parent's i
        }
        
        get <- function(){
                x               #return matrix
        } 
        
        setinverse <- function(si) {
                i <<- si        #store inverse matrix
        }
        
        getinverse <- function() {
                i               #return inverse matrix
        }
        
        list(set = set, get = get,  #return list of functions when called empty.
             setinverse = setinverse, 
             getinverse = getinverse)   
}



## cacheSolve checks for a solved matrix and returns it or solves and
## stores inverse if it has not been previously solved.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        i <- x$getinverse()     #retrieve inverse of x
        
        if(!is.null(i)) {
                message("getting cached data")
                return(i)       #if inverse retrieved is valid return it, exit
        }
        matrix <- x$get()       #get the matrix and solve for inverse
        i <- solve(matrix, ...)
        x$setinverse(i)         #store the inverse for later retrival
        i                       #return inverse
}

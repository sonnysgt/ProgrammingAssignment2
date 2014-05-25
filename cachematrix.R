## Below are two functions that are used to create a 
## special object that stores a matrix 
## and cache's its inverse 

## The first function, makeCacheMatrix 
## creates a special "vector", which is a list 
## containing functions to

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
            
        m <- NULL
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## The cacheSolve function calculates the inverse the matrix
## created with function makeCacheMatrix. It first determines
## if the inverse matrix for the current matrix has already been 
## calculated.  
## If so, it uses the inverse matrix from the cache 
## otherwise, it calculates the the inverse matrix for the
## current matrix, and sets the value of the 
## inverse matrix in the cache via the setinverse function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

m <- x$getinverse()
if(!is.null(m)) 
{
    xm <- x$get()   # get tge current matrix
    d <- m%*%xm     # a matrix * its inverse = the Identity matrix 
    k <- nrow(m)    # number if rows in the current matrix
    d2 <- diag(k)   # compute Identity matrix to compare to
    d1 <- round(d)  # Correct any precsion issues
    im <- TRUE      # true or false flag
    
    # If all elements match the cached matrix is the inverse
    # matrix of the current matrix
    
    for(i in seq_len(nrow(d1))) 
    {
        for(j in seq_len(ncol(d1))) 
            {
                if ((d1[i, j]) != (d2[i, j]))
                    im <- FALSE
            }
    }           
    
    if (im == TRUE)
    {    
        message("getting cached data")
        return(m)
    }
    
}
xm <- x$get()
xm
m <- solve(xm)
x$setinverse(m)
m
}
## Rather than compute repeatedly the inverse of a matrix, 
## this pair of functions will cache the inverse of a matrix 

## The following function creates a special "matrix" object;
## This "matrix" object can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        
        ## Creates an empty cache
        cache <- NULL
        
        ## Creates a function to store a matrix
        ## Since you're creating a new matrix, flush the cache (cache gets null)
        setmatrix <- function(y) { 
                x <<- y         ## global x gets local y  
                cache <<- NULL  ## global cache gets null 
                }
        
        ## Creates a function to return a stored matrix 
        getmatrix <- function() x 
        
        
        ## Creates a function to cache an arguement 
        cachematrix <- function(solve) cache <<- solve 
        
        
        ## Creates a function to get the cached arguement 
        getinverse <- function() cache
        
        
        ## Creates a list to store these several functions 
        ## Which are conveniently kept in memory by *dark magic
        ## *Or by how r operates
        
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             cachematrix = cachematrix,
             getinverse = getinverse)
        }

## The following function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.

## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        ## Put the cached matrix into this function (i.e., grab the cache)
        cache <- x$getinverse()
        
        ## If the cached matrix is not null (if it exists) then return it 
        ## and end the function using return()
        if(!is.null(cache)) {
                message("Getting your cached data")
                return(cache)   
        }
        
        ## If the cached matrix is null then do the following:
                ## 1. Get the matrix...
                data <- x$getmatrix()
        
                ## 2. And then calculate the inverse...
                cache <- solve(data, ...)
        
                ## 3. And then store the result... 
                x$cachematrix(cache)
               
                ## 4. And output the inverse matrix, at the end of the function. 
                cache           
        
        }


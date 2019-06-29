## These functions work together to attach a cache to a matrix and find/update,
## store, and return the inverse of the matrix when called.

## Takes a matrix and makes a null inverse variable. Defines get and set
## functions for matrix and inverse. 

makeCacheMatrix <- function(ma = matrix()) { ## ma is the matrix variable
        
        ca<-NULL  ## ca is the cache variable, no inverse to cache yet
        
        set<- function(x){ ## accept new matrix
                ma<<-x  ## change matrix variable to new matrix
                ca<<-NULL ## clear cache
        }
        
        get<- function() ma  ## return matrix
        
        setinverse<- function(inv = NULL) ca<<-inv ## cache a matrix or clear
        
        getinverse<- function() ca ## return cache
        
        ## Make the $ operator work...
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Takes the makeCacheMatrix object, checks for a cached inverse to return, then
## calculates and caches the inverse if necessary.

cacheSolve <- function(x, ...) {
        
                inverse <- x$getinverse() ## get existing cache value
                
                if(!is.null(inverse)) {  ## if cache is not null
                        message("Cached inverse found:")
                        return(inverse) ## exit function and return cached value
                }
                
                message("Calculating inverse...") ## cache must have been null
                
                matriz <- x$get() ## get matrix value
                
                inverse <- solve(matriz, ...) ## solve for the inverse
                
                x$setinverse(inverse) ## cache the inverse
                
                inverse ## return the inverse
}

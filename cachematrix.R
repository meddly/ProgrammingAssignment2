makeCacheMatrix <- function(x = matrix()) {
        m <- NULL 
        set <- function(y) {
                x <<- y 
                m <<- NULL 
        } 
        get <- function() x 
        setinv <- function(solve) m <<- solve 
        getinv <- function() m 
        list(set = set, get = get, setinv = setinv, 
            getinv = getinv) 
} 

#ya#
# el ejemplo decia x = numeric, ahora ponemos matrix
#igual crea un objeto m vacio (NULL)
#se parece a makeVector
#pero ya no hace setmean sino setinv



cacheSolve <- function(x, ...) {
        ## Ret matrix that is the inverse of 'x' initi clock
        ptm <- proc.time() 
        ## Get Matrix "m" from Cache and look for it
        m <- x$getinv()              
        ## Check if not null, Return value read from Cache 
        if(!is.null(m)) {
                message("getting cached data") 
                #print processing time
                cat("Process Time", proc.time() - ptm) 
                return(m)
                }
        ## if "m" is NULL, get original Matrix 
        mydata <- x$get() 
        ## Calculate inverse using Solve 
        m <- solve(mydata, ...) 
        x$setinv(m) 
        cat("Process Time", proc.time() - ptm) 
        ## Returm inverse 
        m 
} 



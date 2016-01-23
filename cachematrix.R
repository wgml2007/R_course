## This function creates a special "matrix" object that can cache its inverse.
## return: a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
##         this list is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
m<- NULL
## delcare another function set where the value will be cached in 
set<- function(y){
        # use <<- to assign a value to an object in an environment 
        x<<-y
        m<<-NULL
}
get<-function() x
#calculates the inverse of non-singular matrix via the solve function
setinverse<- function(solve) m<<-solve 
# gets the inverse     
getinverse<- function() m
list(set=set, get=get, setinverse=setinverse,
     getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        #if the inverse exists, it gets it.
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
#if the inverse if not there, first it is calculated and then retrieved.
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m       
}

##test the two functions
test = function(t){
        ## t: an invertible matrix
        
        temp = makeCacheMatrix(t)
        
        start.time = Sys.time()
        cacheSolve(temp)
        dur = Sys.time() - start.time
        print(dur)
        
        start.time = Sys.time()
        cacheSolve(temp)
        dur = Sys.time() - start.time
        print(dur)
}

set.seed(0000)
r = rnorm(1000000)
t1 = matrix(r, nrow=500, ncol=500)
test(t1)


## makeCacheMatrix function creates a special matrix object that can cache its inverse.
## This help us to understand When a function is evaluated in  R 
## first R looks in a series of environments for any variables in scope.
## The evaluation environment is first, then the function's enclosing environment, 
## Which will be the global environment for functions defined in the workspace. 
## The "m" has be reference in the inside the function makeCacheMatrix



makeCacheMatrix <- function(x = matrix()){
        
        #set variable m (mean in this case) to NULL
                m <- NULL
        
        #set function sets x to the argument y and set m to null
        
                set <- function(y) {
                x <<- y
                m <<- NULL
        }

        #get returns the value of x (argument of makeVector)
                get <- function() {
        
        x
        
        }

        #returns a labeled vector of functions set, get, setinverse and getinverse

                setinverse <- function(solve){
        
                m <<- solve
        }

        # getinverse returns the value of m (from makeVector)

                getinverse <- function(){
        
                m
        
        }


        #  Returns a list of the names (set, get, setinverse and getinverse) of the associated functions to work with the cache


        list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)



}




##This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix .
## If the inverse has already been calculated (and the matrix has not changed)
##then the cachesolve retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

#attempts to get the inverse from x (if it was calculated previously)
        
        m <- x$getinverse()
        #if not null, a valued was cached, so return m
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        #since its null, set data to x from makeCacheMatrix
        
        data <- x$get()
        
        #Computing the inverse of a square matrix using solve function in R 
        
        m <- solve(data)
        
        #set m in x to calculated inverse        
        
        x$setinverse(m)
        
        
        ## Return a matrix that is the inverse of 'x'     
        
        
        m
                      
}

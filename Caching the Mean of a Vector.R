makeVector <- function(x = numeric()) {
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
        
        #returns a labeled vector of functions set, get, setmean and getmean
        
        setmean <- function(mean){
                
                m <<- mean
        }
        
        # getmean returns the value of m (from makeVector)
        
        getmean <- function(){

                m
        }
        
        #returns a labeled vector of functions set, get, setmean and getmean
        
        list(set = set, get = get,setmean = setmean,getmean = getmean)
}

"""The following function calculates the mean of the special "vector" created with the above function. However, it first checks to see if the mean has already been calculated. If so, it gets the mean from the cache and skips the computation. Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.

cachemean <- function(x, ...) {
#attempts to get the mean from x (if it was calculated previously)

        m <- x$getmean()
#if not null, a valued was cached, so return m
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
    
#since its null, set data to x from makeVector

        data <- x$get()

#calculate the mean of data  
        
        m <- mean(data, ...)
   
#set m in x to calculated mean        

        x$setmean(m)

 
       
#return mean
 
        m

}
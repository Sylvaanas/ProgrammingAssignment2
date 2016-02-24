## makeCacheMatrix stores a martix and a cached value of the inverse of the matrix. 
## It contains the following functions:
## - set()        set value of a matrix
## - get()        get value of a matrix
## - setInverse   set the cahced value (inverse of the matrix)
## - getInverse   get the cahced value (inverse of the matrix)

makeCacheMatrix <- function(x = matrix()) {
    
    ## initialize temp as NULL
    inverseMatrix<-NULL
    
    ## set matrix values
    set<-function(value){
        x<<-value
    
    ## As matrix is assigned with a new value, nullify temp
        inverseMatrix<<-NULL
    }
    
    get<-function() x
    
    ## Assign temp value
    setInverse<- function(inverse) inverseMatrix<<-inverse
    
    ## Return temp value
    getInverse<- function() inverseMatrix
	
	# Return list. Each named element is a function
	list (set=set, get=get, 
    setInverse=setInverse, getInverse=getInverse)
}



# The following function calculates the inverse of a "special" matrix 

cacheSolve <- function(x, ...) {
	
	## call get function to get temp value
	inverseMatrix <-x$getInverse()
    ## Check if temp is not NULL. If yes, return temp
    if(!is.null(inverseMatrix)){
        message("getting cached data")
        return(inverseMatrix)
    }
    ## Get matrix and calculate inverse
    data<-x$get()
    inverseMatrix<-solve(data)
    x$setInverse(inverseMatrix)
    
    ## Return a matrix that is the inverse of 'x' 
    inverseMatrix 
        
}

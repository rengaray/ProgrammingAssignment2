## The following two functions are meant to support inversing of matrix and caching the inverse matrix
## to prevent repeated computation.
## Following functions : makeCacheMatrix and cacheSolve is created to fulfill the requirement

## makeCacheMatrix function enables the following actions on the given argument x matrix
## a. setting the value of matrix
## b. getting the value of matrix
## c. setting the inverse matrix value.
## d. getting the inverse matrix value.

makeCacheMatrix <- function(x = matrix()) 
{
	invMatrix<-NULL
	set<-function (oMatrix)
	{
		x<<- oMatrix
		invMatrix<<-NULL
	}
	get<-function() x
	setinverse<-function(inverse) invMatrix <<-inverse
	getinverse<-function() invMatrix
	list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## cacheSolve function will return inverse of a matrix. If it is already computed, it will return cached result.
## Otherwise it will use makeCacheMatrix to compute and retrieve the results

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		invMatrix<-x$getinverse()
		if(!is.null(invMatrix))
		{
			message("getting the cached data.")
			return (invMatrix)
		}
		data<-x$get()
		invMatrix<-solve(data)
		x$setinverse(invMatrix)
		invMatrix
}

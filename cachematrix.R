## makeCacheMatrix creates a list of function that:
	#a. set and get value of matrix
	#b. set and get value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
						inv = NULL 
						set <- function(y){
							x <<- y
							inv <<- NULL
					}
						get <- function() x
						setinverse <- function(inverse) inv <<- inverse
						getinverse <- function() inv
						list(set=set, get=get, 
						setinverse=setinverse, 
						getinverse=getinverse)
}

##Return matrix inverse of "x" by checking:
	#a. If inverse has been calculated.
	#b. If yes, get inverse from cache & skips computation.
	#c. If no, calculate inverse and sets mean value in cache via "setinverse" function.

cacheSolve <- function(x,...) {
						inv <- x$getinverse()
						if(!is.null(inv)) {
								message("fetching cached data")
								return(inv)
						}
						data <- x$get()
						inv <- solve(data)
						x$setinverse(inv)
						inv        
}



c <- c(1,1,-4,0,0,0) 

A <- rbind(
    c(1,1,2, 1,0,0), 
    c(1,1,-1,0,1,0),
    c(-1,0,1,0,0,1)   
)

b<- c(9,2,4)
  

m <- length(b)
n <- length(c)

cb <- numeric(n)

iter <- 0 
iV <- seq(1,n)

iB <- c(3,4,5)


while (TRUE) {

  iN <- setdiff(iV,iB)
  
  cb <- numeric(n)  
  for(i in iN){
    y <- solve(A[,iB], A[,i])
    cb[i] <- c[i] - c[iB]%*%y
  }
  
  ik <- which.min(cb)
  
  if (cb[ik] >= 0 ){
    print("Solução encontrada")
    break
  } 
  solOp <- solve(A[,iB],b)   
  print(solOp)

  valOp <- (-c[iB]%*%solOp)  
  print(paste0('valor: ',valOp))

  y <- solve(A[,iB],A[,ik])   
  depre <- which.max(y) 

  if(y[depre] < 0){
    print("O Problema não tem solução")
    break

  } else {

    inp <- which(y>0) 
    min <- b[inp[1]]/y[inp[1]]
    for(i in inp){
      if(b[i]/y[i] <= min+1 ){ 
        min <- b[i]/y[i]
        ipivo <- i 
      }
    }
  }

  print(paste0('ipivo ',ipivo))

  iB <- union( setdiff(iB, iB[ipivo]), c(ik))
  iter <- iter + 1 

  if (iter >2){break}

}















 
















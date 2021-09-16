##=========================================
## file: simplexMB.R 
##=========================================
## 
## https://hector-fc.github.io/
##
##=========================================

SimplexMB <- function(A,b,c,iB) { 
    m <- length(b)
    n <- length(c)
    iter <- 0 
    iV <- seq(1,n)

    while (TRUE) {
        cb <- numeric(n)
        iN <- setdiff(iV,iB)
        
        for(i in iN){
            y <- solve(A[,iB], A[,i])
            cb[i] <- c[i] - c[iB]%*%y
        }        
        ik <- which.min(cb)        
        if (cb[ik] >= 0 ){
            xs <- numeric(n)
            solOp <- solve(A[,iB],b)
            xs[iB] <-solOp 
            valOp <- (c[iB]%*% solOp)  
            print("Solução encontrada !")      
            break
        } 

        
        y <- solve(A[,iB],A[,ik])   
        depre <- which.max(y) 

        if(y[depre] < 0){
            print("O problema é ilimitado ")
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

        iB <- union( setdiff(iB, iB[ipivo]), c(ik))
        iter <- iter + 1 

        if (iter >100){
            print("Número de iterações permitido")
            break      
        }

    }

    return(list(xs,valOp,iter))  
}

##=========================================
##  Exemplo 1  
##

vc <- c(1,1,-4,0,0,0) 
matA <- rbind(
    c(1,1,2, 1,0,0), 
    c(1,1,-1,0,1,0),
    c(-1,0,1,0,0,1)   
)

vb<- c(9,2,4)
iB <- c(4,5,6)

solPL <- SimplexMB(matA,vb,vc,iB) 
print(paste("Solução Basica: ",solPL[1]))
print(paste("valor  otimo: ",solPL[2]))
print(paste("Iteração: ", solPL[3]))

##=========================================
##  Exemplo 2  
##













 
















##=========================================
## file: simplexM2F.R 
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
            valOp <- (c[iB]%*% solOp)[1,1]  
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
    return(list(iB,xs,valOp,iter))  
}
##=========================================
##  Metodo  de duas fases 

SimplexM2F <- function(A,b,c){    
    n <- length(c)
    m <- length(b)
    
    ## Fase I
    print("Fase I")
    iB <- seq(n+1,m+n)    
    auxc <- c(rep(0,n),rep(1,m))
    auxA <- cbind(A,diag(rep(1,m)))
    solFI <- SimplexMB(auxA,b,auxc,iB) 
    names(solFI)<- c("Base","solB","valOp","iter")
    
    
    ## Fase II
    if(solFI$valOp >0 ){      
        print("O PL  não tem solução")
        return(-1)
    }else{
        print("Fase II")
        tB <- is.element(max(solFI$Base), seq(1,n))
        if(tB){             
            solFII <- SimplexMB(A,b,c,solFI$Base)
            names(solFII)<- c("Base","solB","valOp","iter")
            return(solFII)
        }
    }
}


##=========================================
##  Exemplo 1
##

print("================================")
print("Exemplo 1")

vc <- c(4,1,1) 
matA <- rbind(
    c(2,1,2), 
    c(3,3,1)
)
vb <- c(4,3)

solPL <- SimplexM2F(matA,vb,vc)

print("================================")
print(" Solução  ")
print("")

print(paste("valor otimo: ",solPL$valOp))
sl <- solPL$solB
print(paste("Solução Basica: ",sl[1],sl[2],sl[3]))

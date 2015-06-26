      #  Functions to observe data from population trajectory created by
      #  (PopSim2 or PopSim3)
      #  11/2014
################################################################################
      #  Survival
      ob.surv <- function(PHI, nmarks, nyrS, ageclass, sex, repeat.interval){
        # Define function to simulate a capture-history (CH) matrix
        simul.kf <- function(PHI, P, marked){
          n.occasions <- dim(PHI)[2] + 1
          CH <- matrix(NA, ncol = n.occasions, nrow = sum(marked))
          # Define a vector with the occasion of marking
          mark.occ <- rep(1:length(marked), marked[1:length(marked)])
          # Fill the CH matrix
          for (i in 1:sum(marked)){
            CH[i, mark.occ[i]] <- 1
            if (mark.occ[i]==n.occasions) next
            for (t in (mark.occ[i]+1):n.occasions){
               # Bernoulli trial: does individual survive occasion?
               CH[i,t] <- rbinom(1, 1, PHI[i,t-1] * CH[i,t-1])
            } #t
          } #i
        return(CH)
        }
        
        #  Setup call to simul.cjs
        # Annual number of newly marked individuals
        marked <- rep(nmarks, nyrS)

        # Define matrices with survival and recapture probabilities
        PHI <- matrix(xx$PHI[1:nyrS,2,1], ncol = nyrS, nrow = sum(marked),
                      byrow = T)
        P <- matrix(0.5, ncol = nyrS, nrow = sum(marked))
        
        outeh <- simul.kf(PHI, P, marked)
        
        #  Find first and last occassion for each animal
        first <- apply(outeh, 1, function(x){
          min(which(x == 1))
        })
        last <- apply(outeh, 1, function(x){
          if(any(x == 0, na.rm = T)){
            min(which(x == 0))
          }else{
            max(which(x == 1))
          }
        })
        
      list(eh = outeh, first = first, last = last)
      }
      
      #  Ratio data
      ob.ratio <- function(N, nobs, nyR){
        y <- apply(N[,1,], 1, sum)
        f <- apply(as.data.frame(N[,-1,1]), 1, sum)
        m <- apply(as.data.frame(N[,-1,2]), 1, sum)
        rat <- data.frame(yf = y/f, mf = m/f)
        obs <- apply(rat[1:nyR,], 1, function(x){
          yf.out <- rbinom(1, size = nobs, prob = x[1])
          mf.out <- rbinom(1, size = nobs, prob = x[2])
          c(nobs, yf.out, mf.out)
        })
        out <- cbind(t(obs), round(rat, 2))
        colnames(out) <- c("Nobs", "YOY", "Males", "True.YF", "True.MF")
      return(out)
      }
      
      #  Abundance, aerial survey
      ob.n <- function(N, varmult, obsfreq){
        tmp <- rep(NA, length(N))
        obsvec <- seq(1, length(tmp), by = obsfreq)
        obs <- rnorm(length(obsvec), N[obsvec], N[obsvec] * varmult)
        out <- data.frame(year = 1:length(N), Nest = NA)
        out[obsvec, 2] <- obs
      return(out)
      }
      
      
      
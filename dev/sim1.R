##########################  UI Inputs  #########################################
    
#     #  Controls
#     n.years <- 10
# 
#     #  Pregnancy and number of births
#     mu.preg <- 0.88
#     sd.preg <- 0.08
#     
#     #  Survival
#     mu.phi.fawn <- 0.5
#     sd.phi.fawn <- 0.2
#     mu.phi.adf <- 0.83
#     sd.phi.adf <- 0.1
#     mu.phi.adm <- 0.55
#     sd.phi.adm <- 0.1
#     
#     #  First year pop, c(fawns, does, bucks)
#     mu.N.init <- c(200, 200, 110)
#     sd.N.init <- c(0, 0, 0)

##########################  Process Models #####################################
    #  Grow Population
    sim.deer <- function(n.years, 
                         mu.N.init, sd.N.init,
                         mu.preg, sd.preg,
                         mu.phi.fawn, sd.phi.fawn,
                         mu.phi.adf, sd.phi.adf,
                         mu.phi.adm, sd.phi.adm){

      #  Initialize abundance
      N <- matrix(NA, nrow = n.years, ncol = 3)
      colnames(N) <- c("Fawns", "Does", "Bucks")
      N[1,] <- rnorm(3, mu.N.init, sd.N.init)
      
      #  Draw births, pregnancy rates and survival rates
      p.preg <- plogis(rnorm(n.years, qlogis(mu.preg), sd.preg))
      phi.fawn <- plogis(rnorm(n.years, qlogis(mu.phi.fawn), sd.phi.fawn))
      phi.adf <- plogis(rnorm(n.years, qlogis(mu.phi.adf), sd.phi.adf))
      phi.adm <- plogis(rnorm(n.years, qlogis(mu.phi.adm), sd.phi.adm))
  
      #  Do the math, make it grow...or not
      for(t in 2:n.years){
        N[t,1] <- rpois(1, N[t-1,2] * p.preg[t-1] * phi.fawn[t-1])  
        N[t,2] <- rbinom(1, round(N[t-1,2] + (N[t-1,1]/2)), phi.adf[t-1])
        N[t,3] <- rbinom(1, round(N[t-1,3] + (N[t-1,1]/2)), phi.adm[t-1])
      }
      
      #  Return population size, demographic rates and derived bits
      out <- data.frame(N.Fawns = round(N[,1]),
                        N.Does = round(N[,2]),
                        N.Bucks = round(N[,3]),
                        P.Preg = round(p.preg, 2),
                        S.Fawns = round(phi.fawn, 2),
                        S.Does = round(phi.adf, 2),
                        S.Bucks = round(phi.adm, 2),
                        Fawn.Doe = round(N[,1]/N[,2], 2),
                        Buck.Doe = round(N[,3]/N[,2], 2))
    return(out)
    }
################################################################################
#     sim.deer(5,
#              c(100, 100, 50), c(0, 10, 0),
#              1, 0,
#              0.6, 0.3,
#              0.82, 0.1,
#              0.55, 0.15)
################################################################################
    #End



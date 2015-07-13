      #  Population simulation
      #  11/2014
################################################################################
      #  Population simulation function takes a series of of inputs and returns
      #  an array with dimensions n years, 10, n sexes.
      #  The model is pre-birth pulse, two sex and assumes equal sex ratios at
      #  birth.  Harvest is incorporate through the survival term, i.e. the
      #  input survival rates should include harvest mortality.  This model has
      #  3 age classes.
################################################################################
      #  Inputs
      #  nyr = a single value describing the number of years to simulate
      #  process.error = logical, if TRUE reproduction is distributed as
      #    Poisson and survival follows a binomial process, if FALSE the matrix
      #    projection model is deterministic
      #  phi.trends = the amount of change in survival that occurs each year, a
      #    vector of length 6
      #  fec.trends = the amount of change in fecundity each year, a vector of
      #    length 3
      #  start.pop = a vector of length 4 specifying the starting size of each
      #    population demographic
      #  phi = a matrix (6 x 2) describing the min and max (columns) survival
      #    for each demographic (rows), where the first two rows describe female
      #    survival and the last two rows depict male survival
      #  fec = a matrix (3 x 2) describing the min and max (columns) fecundity
      #    for each age class of females.  Here fecundity is the number of young
      #    born to a given animal each year.
      #  prop.harv.mort = a matrix (2 X 3) describing the proportion of
      #    mortality that is due to harvest.  For example, if survival is 0.8
      #    then mortality is 0.2 and if 0.7 of that is harvest related then 0.14
      #    of the population dies by harvest and 0.06 of the population dies
      #    from other causes each year.
################################################################################
      #  Example call
xxx <-       PopSim3(
        nyr = 5,
        process.error = T,
        phi.trends = c(0,0,0,0,0,0),
        fec.trends = c(0, 0, 0),
        start.pop = c(70, 50, 140, 70, 60, 80),
        phi = matrix(c(.6,.6,.7,.7,.8,.8,.56,.56,.66,.66,.7,.75), nrow = 6,
          byrow = T),
        fec = matrix(c(0,0,.5,.5,1,1), nrow = 3, byrow = T),
        prop.harv.mort = matrix(c(0.5, 0.5, 0.6, 0.8, 0.4, 0.9), nrow = 2))
################################################################################
      PopSim3 <- function(nyr,
                          process.error,
                          phi.trends,
                          fec.trends,
                          start.pop,
                          phi,
                          fec,
                          prop.harv.mort){

        #  Initialize population size matrix
        N <- PHI <- HM <- HMN <- array(NA, dim = c(nyr, 3, 2))

        #  Create first year abundance
        N[1,,] <- start.pop

        #  Draw demographic rates, potentailly with randomly changing values and
        #  trends
        PHI[,,] <- sapply(1:6, function(i){
                          round(runif(nyr, phi[i,1], phi[i,2]), 2) +
                          phi.trends[i] * 1:nyr
        })
        FEC <- sapply(1:3, function(i){
                          round(runif(nyr, fec[i,1], fec[i,2]), 2) +
                          fec.trends[i] * 1:nyr
        })

        #  Grow the population according to known parameters
        if(process.error){
          for(y in 2:nyr){
            for(sex in 1:2){
              N[y,1,sex] <- rpois(1, ((N[y-1,1,1] * FEC[y-1,1]/2) +
                                      (N[y-1,2,1] * FEC[y-1,2]/2) +
                                      (N[y-1,3,1] * FEC[y-1,3]/2)) *
                                       PHI[y-1,1,sex])
              N[y,2,sex] <- rbinom(1,
                                  size = round(N[y-1,1,sex]),
                                  prob = PHI[y-1,2,sex])
              N[y,3,sex] <- rbinom(1,
                                  size = round(N[y-1,2,sex] + N[y-1,3,sex]),
                                  prob = PHI[y-1,3,sex])
              HM[y,,sex] <- round((1 - PHI[y,,sex]) * prop.harv.mort[sex,], 2)
              HMN[y,,sex] <- round(N[y,,sex] * HM[y,,sex])
            }  #  s
          }  #  y
        }else{
          for(y in 2:nyr){
            for(sex in 1:2){
              N[y,1,sex] <- ((N[y-1,1,1] * FEC[y-1,1]/2) +
                             (N[y-1,2,1] * FEC[y-1,2]/2) +
                             (N[y-1,3,1] * FEC[y-1,3]/2)) *
                              PHI[y-1,1,sex]
              N[y,2,sex] <- round(N[y-1,1,sex] * PHI[y-1,2,sex])
              N[y,3,sex] <- round((N[y-1,2,sex] + N[y-1,3,sex]) *
                                  PHI[y-1,3,sex])
              HM[y,,sex] <- round((1 - PHI[y,,sex]) * prop.harv.mort[sex,], 2)
              HMN[y,,sex] <- round(N[y,,sex] * HM[y,,sex])
            }  #  sex
          }  #  y
        }  #  else

        #  Gather outputs in a list
        out <- list("Year" = 1:nyr,
                    "N" = N,
                    "PHI" = PHI,
                    "FEC" = FEC,
                    "HM" = HM,
                    "HMN" = HMN)

      return(out)
      }  #  Close function

################################################################################
      #  End

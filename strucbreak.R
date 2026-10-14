############# To find change points #####################
#### TSNE is applied on multivariate data to obtain one dimensional data #####
###### Binary Segmentation CUSUM statistic algorithm is applied to find CPs #######

############ NOVEMBER 1, 2021 ###############

library(Rtsne)
library(dplyr)


# Function to find change points
CW.Anchal.multi <- function(SERIES, SIZE, iH) {
  bs = SIZE      		# block size
  ci = iH           # number of change points
  TemData = SERIES 	# Data series
  
  Mx = 0 # Block number where Cusum is bigger than threshold (max in case of multiple)
  RT = 0 
  CP = 0
  output = 0
  FINAL = 0
  op = 0
  rj = 0
  rk = 0
  rf1 = 0
  ias = 0
  TSNE = 0
  
  # TSNE 
  trialtsne <- Rtsne(SERIES, dims = 1, perplexity = 50, check_duplicates = FALSE, asp = 1)
  plot(trialtsne$Y)
  TSNE = trialtsne$Y
  l = nrow(data.frame(TSNE))
  
  if( TSNE[1] != 0) {
    i = 0
    for(i in 1:l) {
      TSNE[i] = TSNE[i] - TSNE[1]
    } 
  }
  if(TSNE[2] < 0) {
    i = 0
    for(i in 2:l) {
      TSNE[i] = TSNE[i]*(-1)
    }
  }
  TemData = TSNE # Data to be used
  
  Tcurl = round(NROW(TemData)/bs, digits = 0) # number of blocks to be created
  ConD = Tcurl * bs 
  TemData = TemData[1:ConD] # truncating
  nb = Tcurl
  z <- nrow(data.frame(TemData))
  z1 <- rep(1:nb, each = z/nb, times = 1) # Criteria to divide into Tcurl blocks
  datasplit = split(TemData, z1)
  
  k1 <- kernel("daniell", m = floor(bs^0.2))
  sv = bs/2 # No of spectral values in each block
  
  m = 1
  i = 0
  j = 0
  d = 0
  
  # all d spectral values 
  for (i in 1:nb) {
    y = data.frame(datasplit[i])
    fh = spec.pgram(y, kernel = k1)
    for (j in 1:sv) {
      d[m] = 0
      d[m] = fh$spec[j]
      m = m + 1
    }
  }
  
  sig = 0
  l = 0
  tr = 0
  M1 = matrix(data = d, nrow = sv, ncol = nb) # define matrix of spectral values block wise
  tr = M1
  hs = 1
  is = 1
  output <- matrix(data = output, nrow = 20, ncol = 2)
  FINAL <- matrix(data = FINAL, nrow = 20, ncol = 2)
  
  ConD = 0
  # the main for loop for finding CP
  for ( op in 1:iH) {
    sig = 0
    l = 0
    nb[op] = NCOL(tr)
    Tcurl = nb[op]
    ConD[op] = 0
    ConD[op] = Tcurl * bs 
    
    sv = bs/2 # No of spectral values in each block
    
    ######## NUMERATOR - constants ###########
    i <- 0
    a <- 0
    b <- 0
    
    for (i in 1:nb[op]) {
      a[i] = 0
      a[i] = sqrt((Tcurl - i)/(Tcurl*i))
      b[i] = 0
      b[i] = sqrt(i/(Tcurl*(Tcurl - i)))
    }
    ##########################################
    
    
    ########### THRESHOLD ###########
    Tl = ConD[op] # initial number of data points
    thres <- 0.8 * log(Tl, 1.1)
    #print(thres)
    ################################
    
    
    # Sum of each row which is sum of each w1 value across all blocks and so on
    MSum = 0 # Sum of each row here
    v = 0
    h = 0
    
    for (v in 1:sv) {
      MSum[v] = 0
      
      for (h in 1:nb[op]) {
        MSum[v] = tr[v,h] + MSum[v]
      }
    }
    
    ####################################
    
    #### CUMULATIVE SUM MATRIX row wise ####
    v = 0
    k = 0
    u = 1
    
    
    CN = 0
    CN = matrix(data = CN, nrow = sv, ncol = nb[op]) # cumulative sum matrix row wise across blocks
    
    for (v in 1:sv) {
      
      for (k in 1:nb[op]) {
        
        CN[v,k] = tr[v,k] + CN[v,u] 
        u = k
      }
    }
    
    ###############################################
    
    #### FINAL MATRIX OF CUSUM statistic for each frequency across every block #####
    
    v = 0
    k = 0
    hst = 0
    CW = 0
    CW = matrix(data = CW, nrow = sv, ncol = nb[op])
    
    for (v in 1:nb[op]) {
      for (k in 1:sv) {
        CW[k,v] = (abs((a[v]*CN[k,v]) - (b[v]*(MSum[k]-CN[k,v]))))/(MSum[k]/nb[op])
      }
    }  
    
    # CUSUM statistic for each block - equation 1 
    # add CW column wise
    sumf <- 0
    i <- 0
    j <- 0
    for (i in 1:nb[op]){
      sumf[i] = 0
      for(j in 1:sv){
        sumf[i] <- CW[j,i] + sumf[i]
      }
    }
    
    nc = nb[op] - 1
    Bl <- 0
    i <- 0
    for (i in 1:nc) {
      Bl[i] <- ifelse (sumf[i] > thres, sumf[i], 0)
    }
    
    Mx[op] = 0
    Mx[op] = which.max(sumf)
    RT[op] = 0
    
    RT[op] = sum(Bl) # sum of all those CUSUM statistic which is greater than zero
    CP[op] = 0
    rj[op] = 0
    rf1[op] = 0
    rf1[op] = Mx[op] - 1
    rj[op] = Mx[op] + 1
    rk[op] = rf1[op]*bs
    
    
    if( (op %% 2) == 0) {
      tr = M1[ ,rj[hs]:nb[hs]]
      hs = hs + 1
      ias = Mx[op] 
    } else {
      tr = M1[ ,1:rf1[hs]]
      while(is <= op) {
        ias = ias + Mx[is]
        is = is + 2
      }
    }
    if(Mx[op] == 1){
      output[op,1] = "No Change Point"
      break
    }
    
    CP[op] = ias * bs
    tt <- TemData[CP[op]]
    output[op,1] = which(tt == TSNE)
    output[op,2] = TemData[CP[op]]
    
  }
  
  colnames (output) <- c("Change Point", "Value at Change point")
  FINAL <- output[1:ci,1:2]
  
  #print (FINAL)
  #print (output)
  
  return (list(FINAL))
}

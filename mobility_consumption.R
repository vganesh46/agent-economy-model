##-------------------------------1. Mobility ----------------------------------


library(SpatialTools)
library(FNN)
library(ggplot2)
library(dplyr)
library(tidyr)

### initializing 

# Agents coordinates at t = 0
set.seed(1);
Xi0 = runif(1000,min = 0, max = 1);
set.seed(100)
Yi0 = runif(1000,min = 0, max = 1);
i0 = cbind(Xi0,Yi0);

# Wealths of agents at t = 0
set.seed(100);
wi0 = runif(1000,min = 1, max = 100);

# Objects coordinates (static with time)
set.seed(200);
Xk = runif(1000,min = -10, max = 10);
set.seed(300)
Yk = runif(1000,min = -10, max = 10);
k = cbind(Xk,Yk);

### setting up formulaes

a = 1;
f = 2;


# f nearest agents to agent i at t = 0

nearest2pts <- nn2(data= i0, k=3)[[1]][,2:3]
#x0 <- i0[,1]
#y0 <- i0[,2]
#xn <- apply(nearest2pts,2,function(x) x0[x])
#yn <- apply(nearest2pts,2,function(x) y0[x])

# Mean value of positions of f nearest agents to agent i at t = 0
Xf0 <- apply(nearest2pts,1,function(x) mean(i0[,1][x]))
Yf0 <- apply(nearest2pts,1,function(x) mean(i0[,2][x]))

# function for finding nearest mean

nearest.mean <- function(i,f){
  g <- f + 1
  nearestpts <- nn2(data= i, k= g)[[1]][,2:g]
  Xf <- apply(nearestpts,1,function(x) mean(i[,1][x]))
  Yf <- apply(nearestpts,1,function(x) mean(i[,2][x]))
  ifm <- cbind(Xf,Yf)
  return(ifm)
}


#distance between agent i and k objects at t = 0

gik0 <- dist2(i0,k)

#new wealth acquired by agent i at t = 0

ni0 <- apply(gik0,1,function(x) sum(x-a < 0))

# function for finding wealth acquistion

wealth.acquired <- function(i,k,a){
  gik <- dist2(i,k)
  ni <- apply(gik,1,function(x) sum(x-a < 0))
  return(ni)
}

# function for finding next position & wealth of agents with time t

next.wealth.position <- function(it,wit,k,a,f) {
    Xit <- it[,1]                       #initial position of agent i in x
    Yit <- it[,2]                       #initial position of agent i in y
    dxit <- runif(1000,min = -1,max = 1)   #random distance movement of agents i in x
    dyit <- runif(1000,min = -1,max = 1)   #random distance movement of agents i in y
    Xitinter <- Xit + dxit              #intermediate position of agent i in x
    Yitinter <- Yit + dyit              #intermediate position of agent i in y
    itinter <- cbind(Xitinter,Yitinter) #intermediate coordinates of agent i
    nit <- wealth.acquired(itinter,k,a) #new wealth acquisition of agent i 
    witnext <- wit + nit                #total wealth of agent i
    Xfmt <- nearest.mean(itinter,f)[,1] # Mean value of x coordinate for f nearest ggents to agent i
    Yfmt <- nearest.mean(itinter,f)[,2] # Mean value of y coordinate for f nearest ggents to agent i
    Xitnext <- 0.5 *(Xfmt + Xitinter)   #next position of agent i in x
    Yitnext <- 0.5 *(Yfmt + Yitinter)    #next position of agent i in y
    itnext <- cbind(Xitnext,Yitnext)    #next position coordinates of agent i
    return (list(position= itnext,wealth=witnext))
}


### simulating

# Positions of Agents from t = 1 to t = 100

t = 1
it = i0
wit = wi0

W <- matrix(0,nrow = 1000,ncol = 100)
IX <- matrix(0,nrow = 1000,ncol = 100)
IY <- matrix(0,nrow = 1000,ncol = 100)

for (t in 1:100){
new <- next.wealth.position(it,wit,k,a,f)
it <- new$position
wit <- new$wealth
IX[,t] <- it[,1]
IY[,t] <- it[,2]
W[,t] <- wit 
t = t + 1
}


#income at t = 1 to t = 100

N <- matrix(0,nrow = 1000,ncol = 1)
W1 <- cbind(N,W)[,-101]
Y <- W - W1

# # plot graphs

i100 <- cbind(Xi100 = IX[,100],Yi100 = IY[,100])
plot.init.agents <- ggplot(as.data.frame(i0), aes(x=Xi0, y=Yi0)) + geom_point() + theme_classic() + ggtitle("Initial Agent Positions")
plot.final.agents <- ggplot(as.data.frame(i100), aes(x= Xi100, y=Yi100)) + geom_point() + theme_classic()  + ggtitle("Final Agent Positions")

plot.init.agents
plot.final.agents

## -----------------------------2. consumption ----------------------------------------------


# degrees of need for goods

nAi = runif(1000,min = 1, max = 1);
nBi = runif(1000,min = 0, max = 1);
nCi = rnorm(1000,mean = 0.5, sd = 0.01)
nDi = runif(1000,min = 0, max = 0.5)

# agents appetite for goods
mAi = runif(1000,min = 0, max = 1);
mBi = runif(1000,min = 0, max = 1);
mCi = runif(1000,min = 0, max = 1);
mDi = runif(1000,min = 0, max = 1);

#marginal utility of goods at quantity q

marginal.utility <- function(n,m,q){n/q^(m)}

# price agent is willing to pay for good A

priceA <- function(wealth,marginal.utility){wealth * marginal.utility}

# function for finding market price of A with time t

marketpriceA <- function(w,nAi,mAi){
  
  dmndscheduleA <- NULL
  dsA <- NULL
  for(q in 1:10){
    dmndscheduleA  <- cbind(dmndscheduleA,priceA(w,marginal.utility(nAi,mAi,q)))
  }
  dsA <- data.frame(as.table(dmndscheduleA)) %>% select(demand = Freq) %>% arrange(desc(demand))
  
  SupplyscheduleA <- NULL
  suA <- NULL
  for(q in 1:10000){
    SupplyscheduleA[q] = q/100
  }
  suA <- data.frame(supply = SupplyscheduleA)
  rows <- data.frame(c(1:10000))
  demand_supplyA <- cbind(rows,dsA,suA)
  PqA <- unname(demand_supplyA %>% filter(supply >= demand) %>% 
                  filter(supply == min(supply)) %>% select(supply))
  return(PqA)
}

#simulating market price of A from t = 1 to t = 100

PqAt <- matrix(0,nrow = 100,ncol = 1)

for (t in 1:100){
  w <- W[,t]
  PqAt[t,1] <- as.numeric(marketpriceA(w,nAi,mAi))
  t = t + 1
}

# price agent is willing to pay for goods B,C,D

priceBCD <- function(n1,income,marginal.utility){n1 * income * marginal.utility}

# function for finding market price of B with time t

marketpriceBCD <- function(y,nBi,nCi,nDi,mBi){
  
  dmndscheduleB <- NULL
  dsB <- NULL
  for(q in 1:10){
    dmndscheduleB  <- cbind(dmndscheduleB,priceBCD((nBi/(nBi + nCi + nDi)),y,marginal.utility(nBi,mBi,q)))
  }
  dsB <- data.frame(as.table(dmndscheduleB)) %>% select(demand = Freq) %>% arrange(desc(demand))
  
  SupplyscheduleB <- NULL
  suB <- NULL
  for(q in 1:10000){
    SupplyscheduleB[q] = q/100
  }
  suB <- data.frame(supply = SupplyscheduleB)
  rows <- data.frame(c(1:10000))
  demand_supplyB <- cbind(rows,dsB,suB)
  PqB <- unname(demand_supplyB %>% filter(supply >= demand) %>% 
                  filter(supply == min(supply)) %>% select(supply))
  return(PqB)
}

#simulating market price of B from t = 1 to t = 100

PqBt <- matrix(0,nrow = 100,ncol = 1)

for (t in 1:100){
  y <- Y[,t]
  PqBt[t,1] <- as.numeric(marketpriceBCD(y,nBi,nCi,nDi,mBi))
  t = t + 1
}

#simulating market price of C from t = 1 to t = 100

PqCt <- matrix(0,nrow = 100,ncol = 1)

for (t in 1:100){
  y <- Y[,t]
  PqCt[t,1] <- as.numeric(marketpriceBCD(y,nCi,nBi,nDi,mCi))
  t = t + 1
}


#simulating market price of D from t = 1 to t = 100

PqDt <- matrix(0,nrow = 100,ncol = 1)

for (t in 1:100){
  y <- Y[,t]
  PqDt[t,1] <- as.numeric(marketpriceBCD(y,nDi,nBi,nCi,mDi))
  t = t + 1
}

rows1 <- data.frame(time = c(1:100))
final_market_prices <- cbind(rows1,PqAt,PqBt,PqCt,PqDt)
final_market_prices1 <- final_market_prices %>% gather(product,market_price,-time)

plot.market.prices <- ggplot(final_market_prices1, aes(x=time, y=market_price, group = product, colour = product)) + geom_point() + theme_classic() 
plot.market.prices

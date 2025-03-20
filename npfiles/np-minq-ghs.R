
library(tidyverse)
library(np)
options(np.messages=TRUE,
        scipen = 999)

# not working - sometimes, there is a google drive issue...
# This data developed in the main paper draft... 
load("ghs2023.RData")


## step 0 - Any correlation issues?
df <- ghs2023 

## Vectorise for np package?
## Here we are using factors and ordered
df$lnminq <- as.vector(df$lnminq)
df$age <- as.vector(df$age)


# Subsampling to see that the program runs
#ss <- sample(dim(df)[1], 500)
#dfss <- df[ss,]


bw.ghs <- npregbw(lnminq ~ lny + kids + adults + age + hhh.male + 
                 ethnic + marital + education + province + settle + month, 
               regtype="ll",
               ckertype="epanechnikov",
               ukertype="liracine",
               okertype="liracine",
               data = df,          #dfss
               bwmethod = "cv.ls", # cv.aic?
               nmulti=20) ##20

sreg.ghs <- npreg(bw=bw.ghs,resid=TRUE, data=df, newdata=df)
uhat.ghs <- residuals(sreg.ghs)
lnminq.hat.ghs = fitted(sreg.ghs)

ghs.npdata <- df %>%
  add_column(uhat.ghs, lnminq.hat.ghs)

save(bw.ghs, sreg.ghs, ghs.npdata,
     file="ghs-np.Rdata")




                       
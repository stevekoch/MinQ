
library(tidyverse)
library(np)
options(np.messages=TRUE,
        scipen = 999)

# not working - sometimes, there is a google drive issue...
#setwd('/Volumes/GoogleDrive/My Drive/Inequality/EqScales-FoodSecure')
load("data/201415/LCS2014df.RData")


## step 0 - Any correlation issues?
df <- LCS2014 %>%
  select(lnx, lny, adults, kids, hhh.male, marital, ethnic, province, settle, education,
         lnminq)

## Vectorise for np package?
## Here we are using factors and ordered
df$lnminq <- as.vector(df$lnminq)
df$kids1 <- factor(df$kids)
df$adults1 <- factor(df$adults)
df$kids2 <- ordered(df$kids)
df$adults2 <- ordered(df$adults)


# Subsampling to see that the program runs
#ss <- sample(dim(df)[1], 500)
#dfss <- df[ss,]


bw1 <- npregbw(lnminq ~ lny + education + age +
                 adults + kids + hhh.male + marital +
                 ethnic + province + settle , 
               regtype="ll",
               ckertype="epanechnikov",
               ukertype="liracine",
               okertype="liracine",
               data = df,          #dfss
               bwmethod = "cv.ls", # cv.aic?
               nmulti=20) ##20

sreg1 <- npreg(bw=bw1,resid=TRUE, data=df, newdata=df)
uhat <- residuals(sreg1)
lnminq.hat = fitted(sreg1)

# here we are using regular factors for kids and adults.
bw2 <- npregbw(lnminq ~ lny + education + age +
                 adults1 + kids1 + hhh.male + marital +
                 ethnic + province + settle , 
               regtype="ll",
               ckertype="epanechnikov",
               ukertype="liracine",
               okertype="liracine",
               data = df,          #dfss
               bwmethod = "cv.ls", # cv.aic?
               nmulti=20) ##20

sreg2 <- npreg(bw=bw2, resid=TRUE, data=df, newdata=df)
uhat2 <- residuals(sreg2)
lnminq.hat2 = fitted(sreg2)

# Here we are using ordered factor for kids and adults
bw3 <- npregbw(lnminq ~ lny + education + age +
                 adults2 + kids2 + hhh.male + marital +
                 ethnic + province + settle , 
               regtype="ll",
               ckertype="epanechnikov",
               ukertype="liracine",
               okertype="liracine",
               data = df,          #dfss
               bwmethod = "cv.ls", # cv.aic?
               nmulti=20) ##20

sreg3 <- npreg(bw=bw3,resid=TRUE, data=df, newdata=df)
uhat3 <- residuals(sreg3)
lnminq.hat3 = fitted(sreg3)

df.uhat <- df %>%
  add_column(uhat, uhat2, uhat3, lnminq.hat, lnminq.hat2, lnminq.hat3)

save(bw1, bw2, bw3, sreg1, sreg2, sreg3, df.uhat,
     file="data/np-minq-results-age.Rdata")




                       
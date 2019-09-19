
### Generate random correlation function


library(MASS)
library(ggplot2)
# Desired correlation
d.cor <- 0.35
# Desired mean of X
d.mx <- 8
# Desired range of X
d.rangex <- 4
# Desired mean of Y
d.my <- 55
# Desired range of Y
d.rangey <- 85
#N
N<-30
# Calculations to create multipliation and addition factors for mean and range of X and Y
mx.factor <- d.rangex/6
addx.factor <- d.mx - (mx.factor*3)
my.factor <- d.rangey/6
addy.factor <- d.my - (my.factor*3)
# Generate data
out <- as.data.frame(mvrnorm(N, mu = c(0,0), 
                             Sigma = matrix(c(1,d.cor,d.cor,1), ncol = 2), 
                             empirical = TRUE))
# Adjust so that values are positive and include factors to match desired means and ranges
out$V1.s <- (out$V1 - min(out$V1))*mx.factor + addx.factor
out$V2.s <- (out$V2 - min(out$V2))*my.factor + addy.factor
# Create liniear model to calculate intercept and slope
fit <- lm(out$V2.s ~ out$V1.s, data=out)
coef(fit)
# Plot scatterplot along with regression line
ggplot(out, aes(x=V1.s, y=V2.s)) + geom_point() + coord_fixed() + geom_smooth(method='lm', se=F)
# Produce summary table
summary(out)
head(out)



cor(out[,1],out[,2])
ggplot(out, aes(x=V1.s, y=V2.s)) + geom_point() + geom_smooth(method='lm', se=F)+
  xlab("Hours of Sleep") + ylab("Proportion correct")


### Permutation

round(data.frame(X=out[,3], Y=out[,4]),2)

v1 = round(data.frame(X=out[,3], Y=sample(out[,4])),2)
v2 = round(data.frame(X=out[,3], Y=sample(out[,4])),2)
v3 = round(data.frame(X=out[,3], Y=sample(out[,4])),2)

cor(v1[,1],v1[,2])
cor(v2[,1],v2[,2])
cor(v3[,1],v3[,2])

qq=NULL
for(i in 1:10000){
  q = round(data.frame(X=out[,3], Y=sample(out[,4])),2)
  qq[[i]]<-cor(q[,1],q[,2])
}

ggplot(data.frame(vals=unlist(qq)), aes(vals)) +geom_histogram(color='white')+
  geom_vline(xintercept=cor(out[,1],out[,2]), color="red", lty=1)+
  ggtitle("Results of Permutation - 10,000 runs")+
  xlab("Permuted value of 'r'")+
  ylab("Frequency")

sum(unlist(qq)>=0.35)/10000



### Bootstrapping

dd=data.frame(X=out[,3],Y=out[,4])

w1=dd[sample(nrow(dd), nrow(dd), T), ]
w2=dd[sample(nrow(dd), nrow(dd), T), ]
w3=dd[sample(nrow(dd), nrow(dd), T), ]

cor(w1[,1],w1[,2])
cor(w2[,1],w2[,2])
cor(w3[,1],w3[,2])

w1
w2
w3

ww=NULL
for(i in 1:10000){
w1=dd[sample(nrow(dd), nrow(dd), T), ]
ww[[i]]=cor(w1[,1],w1[,2])
}

ggplot(data.frame(vals=unlist(ww)), aes(vals)) +geom_histogram(color='white')+
  geom_vline(xintercept=cor(out[,1],out[,2]), color="red", lty=1)+
  ggtitle("Results of Bootstrapping - 10,000 runs")+
  xlab("Boostrapped value of 'r'")+
  ylab("Frequency")


summary(unlist(ww))
quantile(unlist(ww),.025)
quantile(unlist(ww),.975)

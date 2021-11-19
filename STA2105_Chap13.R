#Question 5

#b

qf(.95, df1=3, df2=31)

#d
qtukey(0.95, nmeans=4, df=31)

#Question 12

#a
q12<-matrix(c(19, 17, 18, 14, 15, 30, 32, 31, 29, 35), nrow=5, ncol=2, byrow=F, dimnames=list(NULL, c("A", "B")))

q12_SSTR<-5*(mean(q12[1:5,1])-mean(q12))^2+5*(mean(q12[1:5,2])-mean(q12))^2
q12_MSTR<-q12_SSTR/1
q12_MSTR

q12_SSE<-4*var(q12[1:5,1])+4*var(q12[1:5,2])
q12_MSE<-q12_SSE/8

q12_MSTR/q12_MSE #f-statistics
pf(114.1, df1=1, df2=8, lower.tail=F) #p-value
qf(.95, df1=1, df2=8) #critical value
#reject the null hypothesis

q12.df<-data.frame(q12)
colMeans(q12.df)
q12 <- as.vector(q12)
## gl == generate factor levels
## n == the number of levels
## k == the number of replications
q12_design <- gl(n=2, k=5, length=10, labels=c("1A","2B")) 
q12.df <- data.frame(q12, q12_design)
q12.df

q12.aov<-aov(q12~q12_design)
summary(q12.aov)

#c
#make new distrubiton of the subtraction of them.
q12<-matrix(c(19, 17, 18, 14, 15, 30, 32, 31, 29, 35), nrow=5, ncol=2, byrow=F, dimnames=list(NULL, c("A", "B")))
q12_D<-c(mean(q12[1:5,1])-mean(q12[1:5,2]))
(q12_D-0)/sqrt(((4*var(q12[1:5,1])+4*var(q12[1:5,2]))/8)*(1/5+1/5))
((q12_D-0)/sqrt(((4*var(q12[1:5,1])+4*var(q12[1:5,2]))/8)*(1/5+1/5)))^2
2*pt((q12_D-0)/sqrt(((4*var(q12[1:5,1])+4*var(q12[1:5,2]))/8)*(1/5+1/5)), 8, lower.tail=T)


#Question 28
q28 <- c(14, 10, 13, 15, 15, 12, 10, 8,
          8, 6, 7, 4, 5, 10, 9, 11,
          11, 13, 12, 10, 14, 8, 15, 9,
          10, 13, 9, 6, 12, 9, 7, 13)

q28_medication <- gl(n=2, k=8, length=32, labels=c("Medication","No medication"))
q28_counseling<- gl(n=2, k=16, length=32, labels=c("Counseling","No counseling"))
q28_2<- data.frame (q28, q28_medication, q28_counseling)
q28_2
q28.aov <- aov(q28 ~ q28_medication * q28_counseling)
summary(q28.aov)

mean(q28) #mean
(mean(q28[1:8])+mean(q28[17:24]))/2 #Medication mean
(mean(q28[9:16])+mean(q28[25:32]))/2 #No medication mean
mean(q28[1:16]) #Counseling mean
mean(q28[17:32]) #No counseling mean
# a=2, b=2, m=8
#SST
q28sst<-sum((q28-mean(q28))^2)
q28sst
#SSTR1
q28sstr1<-16*(((mean(q28[1:8])+mean(q28[17:24]))/2-mean(q28))^2
         +((mean(q28[9:16])+mean(q28[25:32]))/2-mean(q28))^2)
q28sstr1
#SSTR2
q28sstr2<-16*((mean(q28[1:16])-mean(q28))^2)+16*((mean(q28[17:32])-mean(q28))^2)
q28sstr2
#SSInt
q28ssint<- 8*((mean(q28[1:8])-(mean(q28[1:8])+mean(q28[17:24]))/2-mean(q28[1:16])+mean(q28))^2
            +(mean(q28[9:16]-(mean(q28[9:16])+mean(q28[25:32]))/2-mean(q28[1:16])+mean(q28)))^2
           +(mean(q28[17:24]-(mean(q28[1:8])+mean(q28[17:24]))/2-mean(q28[17:32])+mean(q28)))^2
          +(mean(q28[25:32]-(mean(q28[9:16])+mean(q28[25:32]))/2-mean(q28[17:32])+mean(q28)))^2)
q28ssint

#SSE
q28sse<-q28sst-q28sstr1-q28sstr2-q28ssint
q28sse

#MSTR1
q28mstr1<-q28sstr1/1
q28mstr1
#MSTR2
q28mstr2<-q28sstr2/1
q28mstr2
#MSint
q28msint<-q28ssint/1
q28msint
#MSE
q28mse<-q28sse/28
q28mse
0
q28mstr1/q28mse
q28mstr2/q28mse
q28msint/q28mse

qf(.95, df1=1, df2=28) #critical value
qf(.95, df1=18,df2=28)


#Question 32
q32<-matrix(c(12.6, 23.1, 33.0, 41.2, 43.2,
            11.2, 21.8, 28.2, 34.5, 45.2,
            10.7, 18.2, 18.4, 29.7, 35.9,
            8.8, 15.9, 20.1, 31.2, 37.4,
            15.1, 28.2, 39.8, 46.9, 53.2,
            14.8, 23.4, 31.4, 39.7, 49.8), ncol=5, nrow=6, byrow=T,
            dimnames=list(c("Sinchon1", "Sinchon2", "Daehakro1", "Daehakro2", "Gangnam1", "Gangnam2"),
                          c("1", "2", "3", "4", "5")))
            
mean(q32) #mean
q32_numberofpeoplemean<-c(colMeans(q32))
q32_numberofpeoplemean
q32_placemean<-c(rowMeans(q32))
q32_placemean[1]<-mean(q32_placemean[1:2])
q32_placemean[2]<-mean(q32_placemean[3:4])
q32_placemean[3]<-mean(q32_placemean[5:6])
q32_placemean<-q32_placemean[1:3]

# a=5, b=3, m=2
#SST
q32sst<-sum((q32-mean(q32))^2)
q32sst
#SSTR1
q32sstr1<-6*((q32_numberofpeoplemean[1]-mean(q32))^2
             +(q32_numberofpeoplemean[2]-mean(q32))^2
             +(q32_numberofpeoplemean[3]-mean(q32))^2
             +(q32_numberofpeoplemean[4]-mean(q32))^2
             +(q32_numberofpeoplemean[5]-mean(q32))^2)
q32sstr1
#SSTR2
q32sstr2<-10*((q32_placemean[1]-mean(q32))^2
              +(q32_placemean[2]-mean(q32))^2
              +(q32_placemean[3]-mean(q32))^2)
q32sstr2
#SSInt
q32ssint<- 2*((mean(q32[1:2,1])-(q32_numberofpeoplemean[1])-q32_placemean[1]+mean(q32))^2
              +(mean(q32[1:2,2])-(q32_numberofpeoplemean[2])-q32_placemean[1]+mean(q32))^2
              +(mean(q32[1:2,3])-(q32_numberofpeoplemean[3])-q32_placemean[1]+mean(q32))^2
              +(mean(q32[1:2,4])-(q32_numberofpeoplemean[4])-q32_placemean[1]+mean(q32))^2
              +(mean(q32[1:2,5])-(q32_numberofpeoplemean[5])-q32_placemean[1]+mean(q32))^2
              +(mean(q32[3:4,1])-(q32_numberofpeoplemean[1])-q32_placemean[2]+mean(q32))^2
              +(mean(q32[3:4,2])-(q32_numberofpeoplemean[2])-q32_placemean[2]+mean(q32))^2
              +(mean(q32[3:4,3])-(q32_numberofpeoplemean[3])-q32_placemean[2]+mean(q32))^2
              +(mean(q32[3:4,4])-(q32_numberofpeoplemean[4])-q32_placemean[2]+mean(q32))^2
              +(mean(q32[3:4,5])-(q32_numberofpeoplemean[5])-q32_placemean[2]+mean(q32))^2
              +(mean(q32[5:6,1])-(q32_numberofpeoplemean[1])-q32_placemean[3]+mean(q32))^2
              +(mean(q32[5:6,2])-(q32_numberofpeoplemean[2])-q32_placemean[3]+mean(q32))^2
              +(mean(q32[5:6,3])-(q32_numberofpeoplemean[3])-q32_placemean[3]+mean(q32))^2
              +(mean(q32[5:6,4])-(q32_numberofpeoplemean[4])-q32_placemean[3]+mean(q32))^2
              +(mean(q32[5:6,5])-(q32_numberofpeoplemean[5])-q32_placemean[3]+mean(q32))^2)
q32ssint

#SSE
q32sse<-q32sst-q32sstr1-q32sstr2-q32ssint
q32sse

#MSTR1
q32mstr1<-q32sstr1/4
q32mstr1
#MSTR2
q32mstr2<-q32sstr2/2
q32mstr2
#MSint
q32msint<-q32ssint/8
q32msint
#MSE
q32mse<-q32sse/15
q32mse

q32mstr1/q32mse
q32mstr2/q32mse
q32msint/q32mse

qf(.95, df1=8, df2=15) #critical value
qf(.95, df1=4, df2=15) #critical value
qf(.95, df1=2, df2=15) #critical value

#a
Minutes <-c(12.6, 23.1, 33.0, 41.2, 43.2,
                11.2, 21.8, 28.2, 34.5, 45.2,
                10.7, 18.2, 18.4, 29.7, 35.9,
                8.8, 15.9, 20.1, 31.2, 37.4,
                15.1, 28.2, 39.8, 46.9, 53.2,
                14.8, 23.4, 31.4, 39.7, 49.8)
NoP.fac <- as.factor(c(rep(1:5, 6)))
Location.fac <- as.factor(c(rep("Sinchon",10),rep("Daehakro", 10), rep("Gangnam",10)))
q32.df<-data.frame(NoP.fac, Location.fac, Minutes)
interaction.plot(NoP.fac, Location.fac, Minutes, type="l", main="interaction effect plot")

#Question 34
q34<-matrix(c(221, 193, 182, 183, 194, 191, 197,
              173, 160, 163, 178, 187, 201, 214,
              205, 183, 195, 214, 231, 220, 241,
              192, 181, 162, 158, 188, 179, 166), ncol=7, nrow=4, byrow=T,
            dimnames=list(c("week1", "week2", "week3", "week4"),
                          c("Mon", "Tue", "Wed", "Thur", "Fri", "Sat", "Sun")))

mean(q34) #mean
q34_dayofweekmean<-c(colMeans(q34))
q34_dayofweekmean
q34_weekmean<-c(rowMeans(q34))
q34_weekmean

#SST
q34sst<-sum((q34-mean(q34))^2)
q34sst
#SSB
q34ssb<-7*(sum((q34_weekmean-mean(q34))^2))
q34ssb
#SSTR
q34sstr<-4*(sum((q34_dayofweekmean-mean(q34))^2))
q34sstr

#SSE
q34sse<-q34sst-q34ssb-q34sstr
q34sse

#MSB
q34msb<-q34ssb/3
q34msb
#MSTR
q34mstr<-q34sstr/6
q34mstr
#MSE
q34mse<-q34sse/18
q34mse

q34mstr/q34mse
q34msb/q34mse

qf(.95, df1=6, df2=18) #critical value
qf(.95, df1=3, df2=18) #critical value

#without blocks
#SSTR
q34sstr2<-4*(sum((q34_dayofweekmean-mean(q34))^2))
q34mstr2<-q34sstr2/6
q34mstr2

q34sse2<-3*(var(q34[1:4,1])+var(q34[1:4,2])+var(q34[1:4,3])+var(q34[1:4,4])
            +var(q34[1:4,5])+var(q34[1:4,6])+var(q34[1:4,7]))
q34mse2<-q34sse2/21
q34mse2

q34mstr2/q34mse2

qf(.95, df1=6, df2=21) #critical value
            
            
            
            
            

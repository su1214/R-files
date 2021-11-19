#question 3
q3<-c(4,13,35,32,15,1)
q3_prob<-c(pnorm(-2),pnorm(-1)-pnorm(-2),pnorm(0)-pnorm(-1),pnorm(1)-pnorm(0),pnorm(2)-pnorm(1),1-pnorm(2))
q3_exp<-q3_prob*100
q3_chisq<-(q3-q3_exp)^2/q3_exp
q3_chisq_sum<-sum(q3_chisq)

q3_final<-rbind(q3_prob, q3, q3_exp, q3_chisq)
q3_final
q3_chisq_sum #df=6-0-1=5
1-pchisq(q3_chisq_sum, df=6-0-1)

chisq.test(q3,p=q3_prob)

#question 11
q11<-c(43,38,41,14)
q11_prob<-c(.45, .41, .10, .04)
chisq.test(q11,p=q11_prob)
#reject the null hypothesis.

#question 17 #a
q17_a<-matrix(c(0,1,2,3,4,5,144,91,32,11,2,0), nrow=2, ncol=6, byrow=T)
dimnames(q17_a)<-list(c("number of deaths", "frequencies"))
q17_an<-sum(q17_a[2,1:6])
q17_asamplemean<-drop(q17_a[1,1:6]%*%q17_a[2,1:6]/q17_an)
q17_aprob<-dpois(q17_a[1,1:5], lambda=q17_asamplemean)
q17_aprob<-c(q17_aprob, 1-sum(q17_aprob))
q17_aexp<-q17_an*q17_aprob
q17_a<-rbind(q17_a, q17_aexp)
(q17_aexp>=5)
q17_a[2,4]<-sum(q17_a[2,4:6])
q17_a[3,4]<-sum(q17_a[3,4:6])
q17_a<-q17_a[1:3,1:4]
q17_a
chisq_q17_a<-(q17_a[2,1:4]-q17_a[3,1:4])^2/q17_a[3,1:4]
q17_a<-rbind(q17_a, chisq_q17_a)
q17_a
sum.chisq_a<-sum(chisq_q17_a)
sum.chisq_a
1-pchisq(sum.chisq_a, df=4-1-1) #p-value 0.3768487
#can't reject the null hypothesis since the p-value is bigger than 0.05.

#b
q17_b<-matrix(c(0,1,2,3,4,5,109,65,22,3,1,0), nrow=2, ncol=6, byrow=T)
dimnames(q17_b)<-list(c("number of deaths", "frequencies"))
q17_bn<-sum(q17_b[2,1:6])
q17_bsamplemean<-drop(q17_b[1,1:6]%*%q17_b[2,1:6]/q17_bn)
q17_bprob<-dpois(q17_b[1,1:5], lambda=q17_bsamplemean)
q17_bprob<-c(q17_bprob, 1-sum(q17_bprob))
q17_bexp<-q17_bn*q17_bprob
q17_b<-rbind(q17_b, q17_bexp)
q17_b
(q17_bexp>=5)
q17_b[2,3]<-sum(q17_b[2,3:6])
q17_b[3,3]<-sum(q17_b[3,3:6])
q17_b<-q17_b[1:3,1:3]
q17_b
chisq_q17_b<-(q17_b[2,1:3]-q17_b[3,1:3])^2/q17_b[3,1:3]
q17_b<-rbind(q17_b, chisq_q17_b)
q17_b
sum.chisq_b<-sum(chisq_q17_b)
sum.chisq_b
1-pchisq(sum.chisq_b, df=3-1-1) #p-value 0.8021489
#can't reject the null hypothesis since the p-value is bigger than 0.05.


#question 26
q26<-matrix(c(22,20,48,37,26,29,58,31,15,56,35,9), nrow=4, ncol=3, byrow=T)
dimnames(q26)<-list(c("1st year","2nd year","3rd year","4th year"), c("employed","grad school", "undetermined"))

#if I use chisq.test function
q26test<-chisq.test(q26, correct=F)
q26test
q26test$expected
q26test$residuals^2

#to calculate manually
q26_rowSums<-rowSums(q26)
q26<-cbind(q26, q26_rowSums)
q26_colSums<-colSums(q26)
q26<-rbind(q26,q26_colSums)

q26_rowprob<-(q26[5,1:3]/q26[5,4])
q26<-rbind(q26, c(q26_rowprob,0))
q26_colprob<-(q26[1:4,4]/q26[5,4])
q26<-cbind(q26, c(q26_colprob,0,0))

q26_rowprob_m<-as.matrix(q26_rowprob, nrow=1, ncol=3, byrow=T) #why?
q26_rowprob_m<-t(q26_rowprob_m)
q26_colprob_m<-as.matrix(q26_colprob, nrow=3, ncol=1, byrow=F)
q26_expected<-386*q26_colprob_m%*%q26_rowprob_m
q26_expected
q26_chisq<-(q26[1:4,1:3]-q26_expected)^2/q26_expected
q26_sum.chisq<-sum(q26_chisq)
1-pchisq(q26_sum.chisq, df=(3-1)*(4-1))
#p-value so small, reject the null hypothesis. They are not indedpendent.

#question 32
q32<-matrix(c(257, 143, 262, 138), nrow=2, ncol=2, byrow=T,
            dimnames=list(c("Univ1", "Univ2"), c("employed","unemployed")))
#a
q32_employmentrate_a<-q32[1,2]/400
q32_employmentrate_a
q32_employmentrate_b<-q32[2,2]/400
q32_employmentrate_b
#choose A which has higher employment rate 0.3575 than B 0.34

#b
#if I use chisq.test function
chisq.test(q32, correct=F)

#p-value 0.7111>0.05, so can't reject the null hypothesis.

257-126-97 #A
143-74-53 #B
262-31-96 #C
136-19-56 #D

q32_1_A<-matrix(c(126, 74, 97, 53, 34, 16), nrow=3, ncol=2, byrow=T,
 dimnames=list(c("Business", "Econonmics","Applied Statistics"), c("employed","unemployed")))
q32_1_A_total<-rowSums(q32_1_A)

q32_1_B<-matrix(c(31, 19, 96, 56, 135, 63), nrow=3, ncol=2, byrow=T,
 dimnames=list(c("Business", "Econonmics","Applied Statistics"), c("employed","unemployed")))
q32_1_B_total<-rowSums(q32_1_B)

q32_1_A_rate<-q32_1_A/c(200,150,50,200,150,50)
q32_1_B_rate<-q32_1_B/c(50,152,198,50,152,198)
q32_1_A_rate
q32_1_B_rate

#e: He/she is going to choose Applied statistics in Univ B now.

#f
#if I use chisq.test function
chisq.test(q32_1_A, correct=F)
#p-value is 0.7971, can't reject the null hypothesis.
chisq.test(q32_1_B, correct=F)
#p-value is 0.5299, can't reject the null hypothesis.



#1
#a
students<- read.table("https://klevas.mif.vu.lt/~visk/Statistine_duomenu_analize/Kontroliniai/Kontroliniai_perrasymui/stud.txt",
             header=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE)

students$difference <- with(stud ,abs(k1-k2))
#b
students1 <-subset(students ,subset=(k1==0|k2==0)&(k1>0.4|k2>0.4))
paste("Studentas nr.", students1$ID,"nerase kontrolinio nr.", ifelse(students1$k2==0,2,1), sep = " ")

#2
parts <- function(u, o, n){
  x = u-3*o;
  y = u+3*o;
  
  #intervals <- cut(x:y, n)
  x0=x;
  for (part in c(1:n)){
      xi = x+ part*(6*o)/n;
      print(paste("[",x0,",",xi,"]"))
      x0=xi}
  
  pnorm(c(u), mean=o, sd=1, lower.tail=TRUE) 
  dnorm(c(u), mean=o, sd=1)
  
}

#3
sum(dbinom(90:97,100,0.96))
  
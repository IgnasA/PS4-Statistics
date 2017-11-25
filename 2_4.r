#2.
#Nuskaitom faila
att <- read.table("http://klevas.mif.vu.lt/~visk/Statistine_duomenu_analize/Kontroliniai/Kontr2/InstAttempts.txt",header=TRUE, sep="\n", na.strings="NA", dec=" ", strip.white=TRUE)

#Empirine pasiskirstymo funkcija
plot(ecdf(att$NoOfAttempts))

#Teorinis dydzio vidurkis
me=mean(att$NoOfAttempts)

#Geometrinio skirstinio parametras
prob=1/(1+me)

#Geometrine teorine pasiskirstymo f-ja
local({
  .x <- 0:4
  plotDistr(.x, pgeom(.x, prob=prob), 
  xlab="Number of Failures until Success",
  ylab="Cumulative Probability", 
  main="Geometric Distribution:  Probability of success=0.424929", 
  discrete=TRUE, cdf=TRUE)
})

#Puasono teorine pasiskirstymo f-ja
local({
  .x <- 0:4
  plotDistr(.x, ppois(.x, lambda=me), xlab="x",
  ylab="Cumulative Probability", 
  main="Poisson Distribution:  Mean=1.353333", discrete=TRUE, 
  cdf=TRUE)
})
#NEMOKEJAU VISKO NUBRAIZYTI ANT VIENOS ASIES. 

#3.
#a.
#socNet.txt pateikiami duomenys
socNet <- read.table("http://klevas.mif.vu.lt/~visk/Statistine_duomenu_analize/Kontroliniai/Kontr2/socNet.txt",header=TRUE, sep=" ", na.strings="NA", dec=" ", strip.white=TRUE)

#Suskirstykite respondentus į grupes
socNet$linkGrp <- findInterval(socNet$nOfLinks, c(6, 9))

#Sudarykite dažnių lentelę
#lytis × jungciu grupė
linkTbl <- table(socNet$gender, socNet$linkGrp)
linkTbl
# Kokią dalį minimalaus jungčių skaičiaus grupėje sudaro vyrai?
maleinfirst=(round(100*linkTbl[,1]/sum(linkTbl[,1]), 2))[2]
maleinfirst
#b.
#z-reiksme
socNet$zScore = (socNet$nOfLinks-mean(socNet$nOfLinks))/sd(socNet$nOfLinks)
#isskirtis, kai z reiksme tarp 2 ir 3
subset(socNet, socNet$zScore>2 & socNet$zScore<3)
#rastos 7 isskirtys
#Joje dominuoja moterys (6m ir 1v)
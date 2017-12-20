#1

.Table <- matrix(c(23,27,17,18,45,50,7,8,1,4), nrow = 5, ncol = 2, byrow=TRUE)
rownames(.Table) <- c('1', '2', '3', '4', '5')
colnames(.Table) <- c('Vyras', 'Moteris')
.Table

.Test <- chisq.test(.Table, correct=FALSE)
.Test

#Ats.: gauta p reiksme 0.8256>0.1, todel galime teigti,  
#kad aplinkos vertinimas nepriklauso nuo lyties

#2.1

studentai <-  read.table("http://klevas.mif.vu.lt/~visk/Statistine_duomenu_analize/Kontroliniai/Kontr4/studentai.txt", header=TRUE, sep="", na.strings="NA", dec=",", strip.white=TRUE)
studentai_frame <- as.data.frame(studentai)
studentai_frame$vid23 <- rowMeans(x=studentai_frame[2:3])

RegModel.3 <- lm(K1~vid23, data=studentai_frame)
summary(RegModel.3)

#regresijos modelio koeficientus randame is lenteles "Coefficients":
#laisvas narys = 0.4554; kr. koeficientas = 0.53338;
#p reiksmes, gana mazos (0.00000827 l. nariui ir 0.00008079 kr. koef.), todel manome, kad
#abu koeficientai regresijos lygtyje reikalingi

prognoze = RegModel.3$coefficients["(Intercept)"] + 0.6*RegModel.3$coefficients["vid23"]
#Lygtis: 0.4554+0.6(K1 reiksme)*0.53338
print(prognoze)

#Ar regresijos lygtyje reikalingas laisvas narys (reiksmingumo lygmuo ?? = 0, 1)?
#Taip, reikalingas, nes koeficienta gavome labai maza, gerokai mazesni nei ??.

#determinacijos koeficientas R^2 = 0.4692, koreguotas R^2(adjusted) = 0.448;
#koeficientai rodo tiesine priklausomybe;

#2.2

with(studentai_frame, median(K1 - vid23, na.rm=TRUE)) # median difference
with(studentai_frame, wilcox.test(K1, vid23, alternative='less', paired=TRUE))
#"ar pirmojo kontrolinio rezultatas geresnis nei dvieju likusiuju vidurkis?" 
#p-value = 0.9996, o ??=0.05, todel galime teigti, jog pirmojo kontrolinio rezultatas geresnis 
#nei dvieju likusiu vidurkis





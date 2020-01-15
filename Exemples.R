#Exemple 1
#devtools::install_github("Lyncoln/AHP2")
require(AHP)

Matriz1 = matrizfuzzy(BD1)
SomaLinhas = SomaParcial(Matriz1)
SomaLinhas2 = SomaTot(SomaLinhas)
Peso1 = Peso(SomaLinhas,SomaLinhas2)
Peso2 = frm(Peso1)
minimo(Peso2)

########Exemple2

m1 = list(matrix(c(1,0.86,0.67,0.33,0.64,1,2.5,.95,.87,.29,1,.4,2.04,0.55,1.49,1),ncol = 4, nrow = 4, byrow = T))
m2 = list(matrix(c(1,1.17,1,.39,.85,1,3,4/3,1,1/3,1,0.5,2.56,3/4,2,1), ncol = 4, nrow = 4, byrow = T ))
m3 = list(matrix(c(1,1.56,1.5,0.49,1.16,1,3.5,1.83,1.49,.4,1,0.67,3.03,1.05,2.5,1), ncol = 4, nrow = 4, byrow = T ))
exe = list(m1,m2,m3)

exe
s = SomaParcial(exe)
l = SomaTot(exe)
k = Peso(s,l)
a = frm(k)
minimo(a)
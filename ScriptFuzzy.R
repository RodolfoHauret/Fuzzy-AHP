######################################################################################
matrizfuzzy = function(matriz){
  n = length(matriz)
  listaCentral = list()
  listaSuperior = list()
  listaInferior = list()
  for(k in 1:n){
    central = unname(as.matrix(matriz[[k]]))
    m = length(central[1,])
    superior = diag(1,ncol = m, nrow = m)
    inferior = diag(1,ncol = m, nrow = m)
    for(i in 1:(m-1)){
      for(j in (i+1):m){
        incerteza = runif(1,0,0.1)
        superior[i,j] = central[i,j] + incerteza
        inferior[i,j] = central[i,j] - incerteza
        superior[j,i] = 1/inferior[i,j]
        inferior[j,i] = 1/superior[i,j]
      }
    }
    listaCentral[[k]] = central
    listaSuperior[[k]] = superior
    listaInferior[[k]] = inferior
  }
  listaGeral = list(listaInferior,listaCentral,listaSuperior)
  return(listaGeral)
}
#######################################################################################
SomaParcial = function(lista){#Lista com Listas de matrizes
  n = length(lista)
  ListaSoma = list()
  for(i in 1:n){#Acessa a i esima lista de matrizes
    lista1 = lista[[i]]#Salvando a i esima lista em um objeto
    m = length(lista1)
    ListaAux = list()
    #Matriz na qual serao salvos todas as somas da i esima lista
    for(j in 1:m){#Acessa a j esima matriz
      matriz1 = lista1[[j]]#Salvando a j esima matriz em um objeto
      soma = c()#Vetor que recebe as somas
      for(k in 1:length(matriz1[,1])){#Percorre as linhas da matriz
        aux = 0
        for(l in 1:length(matriz1[1,])){#Percorre as colunas da matriz
          aux = aux + matriz1[k,l]#Somando os valores das linhas
        }
        soma[k] = aux 
      }
      ListaAux[[j]] = soma
    }
    ListaSoma[[i]] = ListaAux
  }
  return(ListaSoma)
}
#######################################################################################
SomaTot = function(lista){
  n = length(lista)
  listaSomas = list()
  for(i in 1:n){
    lista1 = lista[[i]]
    m = length(lista1)
    aux = c()
    for(j in 1:m){
      vetor = lista1[[j]]
      somatorio = sum(vetor)
      aux[j] = somatorio
    }
    listaSomas[[i]] = aux
  }
  return(listaSomas)
}
#######################################################################################
Peso = function(somas, pesos){
  temp  = pesos
  temp1 = pesos[[1]]
  temp3 = pesos[[3]]
  pesos[[1]] = temp3
  pesos[[3]] = temp1
  qtdlistas =  length(somas) # 3
  qtdvetores = length(somas[[1]]) # 6
  ListaGeral = list()
  for(a in 1:qtdlistas){
    lista1 = somas[[a]]
    vetorp = pesos[[a]]
    for(b in 1:qtdvetores){
      listaaux = list()
      for(c in 1:qtdvetores){
        lista1.1 <- lista1[[c]]
        razao  = lista1.1/vetorp[c]
        teste1 = lista1[[c]]
        teste2 = vetorp[c]
        #print(paste0(teste1, " e ", teste2))
        listaaux[[c]] = razao
      }
      ListaGeral[[a]] = listaaux
    }
  }
  return(ListaGeral)
}
#######################################################################################
frm <- function(listageral){
  # comparar 2 a 2 dentro de um vetor
  lista_comp = list()
  central = listageral[[2]]
  # comparar dentro de central[[i]]
  for(i in 1:length(central)){
    vet = central[[i]]
    m = matrix(0,length(vet),length(vet))
    for(j in 1:length(vet)){
      for(k in 1:length(vet)){
        m[j,k] = ifelse(vet[j] >= vet[k],1,
                        formula(i,k,j,listageral))
      } # vetor_i, o linha_k e a coluna_j
    }
    lista_comp[[i]] = m
  }
  return(lista_comp)
}

# funcao que usa o true or false 
# para encontrar valor da formula

# pegar para cada caso das somas da inferiores com 
# a soma das superiores


#Função auxiliar da função frm
formula = function(vetori,j,k,listageral){
  inferior = listageral[[1]]
  central  = listageral[[2]]
  superior = listageral[[3]]
  # vetori da lista dentro de inferior
  # vetori da lista dentro de superior
  vetinf  = inferior[[vetori]]
  vetcent = central [[vetori]]
  vetsup  = superior[[vetori]]
  
  # comparar o elemento através das var
  # elementoj e elementok
  
  if(vetinf[j] <= vetsup[[k]]){
    V = (vetinf[j] - vetsup[k])/
      (-vetsup[k]+vetcent[k] -vetcent[j] + vetinf[j])
  }else{
    V = 0
  }
  return(V)
}
#####################################################################################
minimo = function(V){
  w = list()
  for(i in 1:length(V)){
    aux = V[[i]]
    d = c()
    for(j in 1: length(aux[1,])){
      d[j] = min(aux[j,])
    }
    w[[i]] = d
  }
  return(w)
}

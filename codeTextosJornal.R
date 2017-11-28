library(ngram)

err <- 0
bestvero <- 0
trocou <- 0


	#lê o texto embaralhado
	text <- readLines("frase-embaralhada.txt")
	str <- concatenate(text)
	#retira numeros, pontuacao, e espaços extra
	str <- preprocess(str, remove.punct=TRUE, remove.numbers=TRUE, fix.spacing=TRUE) 
	#separa a string em chars
	chars <- splitter(str, split.char=TRUE, split.space=TRUE)
	charvec <- strsplit(chars, " ")[[1]]
	
	#monta um dicionario de bigramas de chars
	textos <- readLines("jornais.txt")
	tex <- concatenate(textos)
	tex <- preprocess(tex, remove.punct=TRUE, remove.numbers=TRUE, fix.spacing=TRUE) 
	tex <- splitter(tex, split.char=TRUE, split.space=TRUE)
	dicionario <- ngram(tex, n=3)

	#imprime ngrams
	table_dicionario <- get.phrasetable(dicionario)
	#pega somente os trios que apareceram no minimo 50 vezes nos textos
	table_dicionario <- table_dicionario[which(table_dicionario[,2] > 5),]

	vero_old <- 0
	vero <- 0
	count <- 0
	t <- 0.01 #diff = -0.04 tem 1% de chance de trocar
	while(TRUE){
		trocou <- 0
		#repetir tantas vezes quanto o tamanho da string
		for(rep in 1:(length(charvec))){
			#sorteia 2 letras aleatórias
			i <- sample(1:length(charvec), 1)
			j <- sample(1:length(charvec), 1)
			
			#qual é trio de i antes da troca?
			aux1 <- 0
			aux12 <- 0
			aux13 <- 0
			if(i == 1)
				aux1 <- concatenate(charvec[i], charvec[i+1], charvec[i+2], "")
			if(i == length(charvec))
				aux1 <- concatenate(charvec[i-2], charvec[i-1], charvec[i], "")
			#se nao estiver nas pontas, vai afetar 3 trios
			if(i > 1 && i < length(charvec)){
				aux1 <- concatenate(charvec[i-1], charvec[i], charvec[i+1], "")
				aux12 <- concatenate(charvec[i], charvec[i+1], charvec[i+2], "")
				aux13 <- concatenate(charvec[i-2], charvec[i-1], charvec[i], "")
			}

			id1 <- match(aux1, table_dicionario$ngrams)
			id12 <- match(aux12, table_dicionario$ngrams)
			id13 <- match(aux13, table_dicionario$ngrams)
		
			#qual é trio de j antes da troca?
			aux2 <- 0
			aux22 <- 0
			aux23 <- 0
			if(j == 1)
				aux2 <- concatenate(charvec[j], charvec[j+1], charvec[j+2], "")
			if(j == length(charvec))
				aux2 <- concatenate(charvec[j-2], charvec[j-1], charvec[j], "")
			if(j > 1 && j < length(charvec)){
				aux2 <- concatenate(charvec[j-1], charvec[j], charvec[j+1], "")
				aux22 <- concatenate(charvec[j], charvec[j+1], charvec[j+2], "")
				aux23 <- concatenate(charvec[j-2], charvec[j-1], charvec[j], "")
			}

			id2 <- match(aux2, table_dicionario$ngrams)
			id22 <- match(aux22, table_dicionario$ngrams)
			id23 <- match(aux23, table_dicionario$ngrams)
		

			#qual eh a prob desses trios antes da troca?
			#soma de 3 trios afetados pelo i
			prob1 <- 0
			if(!is.na(id1)) 
				prob1 <- prob1 + table_dicionario$prop[id1]
			if(!is.na(id12)) 
				prob1 <- prob1 + table_dicionario$prop[id12]
			if(!is.na(id13)) 
				prob1 <- prob1 + table_dicionario$prop[id13]
			#mais 3 trios afetados pelo j
			if(!is.na(id2)) 
				prob1 <- prob1 + table_dicionario$prop[id2]
			if(!is.na(id22)) 
				prob1 <- prob1 + table_dicionario$prop[id22]
			if(!is.na(id23)) 
				prob1 <- prob1 + table_dicionario$prop[id23]
			
			#qual é trio de i depois da troca?
			aux1 <- 0
			aux12 <- 0
			aux13 <- 0
			if(i == 1)
				aux1 <- concatenate(charvec[j], charvec[i+1], charvec[i+2], "")
			if(i == length(charvec))
				aux1 <- concatenate(charvec[i-2], charvec[i-1], charvec[j], "")
			#se nao estiver nas pontas, vai afetar 3 trios
			if(i > 1 && i < length(charvec)){
				aux1 <- concatenate(charvec[i-1], charvec[j], charvec[i+1], "")
				aux12 <- concatenate(charvec[j], charvec[i+1], charvec[i+2], "")
				aux13 <- concatenate(charvec[i-2], charvec[i-1], charvec[j], "")
			}

			id1 <- match(aux1, table_dicionario$ngrams)
			id12 <- match(aux12, table_dicionario$ngrams)
			id13 <- match(aux13, table_dicionario$ngrams)
		
			#qual é trio de j depois da troca?
			aux2 <- 0
			aux22 <- 0
			aux23 <- 0
			if(j == 1)
				aux2 <- concatenate(charvec[i], charvec[j+1], charvec[j+2], "")
			if(j == length(charvec))
				aux2 <- concatenate(charvec[j-2], charvec[j-1], charvec[i], "")
			if(j > 1 && j < length(charvec)){
				aux2 <- concatenate(charvec[j-1], charvec[i], charvec[j+1], "")
				aux22 <- concatenate(charvec[i], charvec[j+1], charvec[j+2], "")
				aux23 <- concatenate(charvec[j-2], charvec[j-1], charvec[i], "")
			}

			id2 <- match(aux2, table_dicionario$ngrams)
			id22 <- match(aux22, table_dicionario$ngrams)
			id23 <- match(aux23, table_dicionario$ngrams)
		
			#qual eh a prob desses trios depois da troca?
			#soma de 3 trios afetados pelo i
			prob2 <- 0
			if(!is.na(id1)) 
				prob2 <- prob2 + table_dicionario$prop[id1]
			if(!is.na(id12)) 
				prob2 <- prob2 + table_dicionario$prop[id12]
			if(!is.na(id13)) 
				prob2 <- prob2 + table_dicionario$prop[id13]
			#mais 3 trios afetados pelo j
			if(!is.na(id2)) 
				prob2 <- prob2 + table_dicionario$prop[id2]
			if(!is.na(id22)) 
				prob2 <- prob2 + table_dicionario$prop[id22]
			if(!is.na(id23)) 
				prob2 <- prob2 + table_dicionario$prop[id23]
	
			#se a prob depois for maior, faz a troca
				diff <- prob2 - prob1
				if(diff >= 0){
					auxc <- charvec[i]
					charvec[i] <- charvec[j]
					charvec[j] <- auxc
				}
				else{
				#SIMULATED ANNEALING CONDITION
					p <- exp(diff/t)
					if(p*100 > sample(1:100, 1)){
						auxc <- charvec[i]
						charvec[i] <- charvec[j]
						charvec[j] <- auxc
					#	print("trocou simulated")
					 }
				}
		}
		#calcula a verossimilhanca
		vero_old <- vero
		vero <- 0
		for(i in 1:(length(charvec)-2)){
			aux1 <- concatenate(charvec[i], charvec[i+1], charvec[i+2], "")
			id1 <- match(aux1, table_dicionario$ngrams)
			if(!is.na(id1)){
				vero <- vero + table_dicionario$prop[id1]
			}
		}
		
		#sair de minimo local
		#if(!trocou){
		#	i <- sample(1:length(charvec), 1)
		#	j <- sample(1:length(charvec), 1)
		#	auxc <- charvec[i]
                #        charvec[i] <- charvec[j]
                #        charvec[j] <- auxc
		#	print("trocou")

		#}
		count<- count +1
		print(count)
		#salva o melhor
		print(vero)
		#print(charvec)
		if(vero > bestvero){
        		bestvero <- vero
                	bestcharvec <- charvec
        	}
		
		#a cada 100 iterações, o algoritmo fica 1% menos aleatorio
		if(t > 0.0001 && (count %% 100) == 0){
			t <- t - 0.0001
			#antes de passar para uma faixa mais restrita, vamos recomeçar com a melhor solução ja encontrada
			#charvec <- bestcharvec
		}
}

library(ngram)

err <- 0
bestvero <- 0
trocou <- 0


	#lê o texto
	text <- readLines("scramble2.txt")
	str <- concatenate(text)
	#retira numeros, pontuacao, e espaços extra
	str <- preprocess(str, remove.punct=TRUE, remove.numbers=TRUE, fix.spacing=TRUE) 
	#separa a string em chars
	chars <- splitter(str, split.char=TRUE, split.space=TRUE)
	charvec <- strsplit(chars, " ")[[1]]
	
	#monta um dicionario de bigramas de chars
	musicas <- readLines("musicas.txt")
	mus <- concatenate(musicas)
	mus <- preprocess(mus, remove.punct=TRUE, remove.numbers=TRUE, fix.spacing=TRUE) 
	mus <- splitter(mus, split.char=TRUE, split.space=TRUE)
	dicionario <- ngram(mus, n=2)

	#imprime ngrams
	table_dicionario <- get.phrasetable(dicionario)


	vero_old <- 0
	vero <- 0
	count <- 0
	while(TRUE){
		trocou <- 0
		#fazer para cada par
		for(i in 1:(length(charvec)-1)){
			j <- i+1
			#calcula prob antes da troca
			aux1 <- concatenate(charvec[i-1], charvec[i], "")
			aux2 <- concatenate(charvec[i], charvec[j], "")
			aux3 <- concatenate(charvec[j], charvec[j+1], "")

			id1 <- match(aux1, table_dicionario$ngrams)
			id2 <- match(aux2, table_dicionario$ngrams)
			id3 <- match(aux2, table_dicionario$ngrams)

			prob1 <- 0
			if(!is.na(id1)) 
				prob1 <- prob1 + table_dicionario$prop[id1]
			if(!is.na(id2)) 
				prob1 <- prob1 + table_dicionario$prop[id2]
			if(!is.na(id3)) 
				prob1 <- prob1 + table_dicionario$prop[id3]
			
			#calcula prob depois da troca	
			aux1 <- concatenate(charvec[i-1], charvec[j], "")
			aux2 <- concatenate(charvec[j], charvec[i], "")
			aux3 <- concatenate(charvec[i], charvec[j+1], "")

			id1 <- match(aux1, table_dicionario$ngrams)
			id2 <- match(aux2, table_dicionario$ngrams)
			id3 <- match(aux2, table_dicionario$ngrams)

			prob2 <- 0
			if(!is.na(id1)) 
				prob2 <- prob2 + table_dicionario$prop[id1]
			if(!is.na(id2)) 
				prob2 <- prob2 + table_dicionario$prop[id2]
			if(!is.na(id3)) 
				prob2 <- prob2 + table_dicionario$prop[id3]
			
			#se for maior, faz a troca
			if(prob2 >= prob1){
				auxc <- charvec[i]
				charvec[i] <- charvec[i+1]
				charvec[i+1] <- auxc
				trocou <- 1
			} 
		}
		#calcula a verossimilhanca
		vero_old <- vero
		vero <- 0
		for(i in 1:(length(charvec)-1)){
			aux1 <- concatenate(charvec[i], charvec[i+1], "")
			id1 <- match(aux1, table_dicionario$ngrams)
			if(!is.na(id1)){
				vero <- vero + table_dicionario$prop[id1]
			}
		}
		count<- count +1
		print(count)
		#salva o melhor
		print(vero)
		print(charvec)
		if(vero > bestvero){
        		bestvero <- vero
                	bestcharvec <- charvec
        	}
		#sair de minimo local
		if(!trocou){
			i <- sample(1:length(charvec), 1)
			j <- sample(1:length(charvec), 1)
			auxc <- charvec[i]
                        charvec[i] <- charvec[j]
                        charvec[j] <- auxc

		}
	}


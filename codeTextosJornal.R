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
	tex <- preprocess(tex, remove.punct=FALSE, remove.numbers=TRUE, fix.spacing=TRUE) 
	tex <- splitter(tex, split.char=TRUE, split.space=TRUE)
	dicionario <- ngram(tex, n=3)

	#imprime ngrams
	table_dicionario <- get.phrasetable(dicionario)


	vero_old <- 0
	vero <- 0
	count <- 0
	while(TRUE){
		trocou <- 0
		#repetir tantas vezes quanto o tamanho da string
		for(rep in 1:(length(charvec))){
			#sorteia 2 letras aleatórias
			i <- sample(1:length(charvec), 1)
			j <- sample(1:length(charvec), 1)
			
			#qual é trio de i antes da troca?
			if(i == 1)
				aux1 <- concatenate(charvec[i], charvec[i+1], charvec[i+2], "")
			if(i == length(charvec))
				aux1 <- concatenate(charvec[i-2], charvec[i-1], charvec[i], "")
			if(i > 1 && i < length(charvec))
				aux1 <- concatenate(charvec[i-1], charvec[i], charvec[i+1], "")

			id1 <- match(aux1, table_dicionario$ngrams)
		
			#qual é trio de j antes da troca?
			if(j == 1)
				aux2 <- concatenate(charvec[j], charvec[j+1], charvec[j+2], "")
			if(j == length(charvec))
				aux2 <- concatenate(charvec[j-2], charvec[j-1], charvec[j], "")
			if(j > 1 && j < length(charvec))
				aux2 <- concatenate(charvec[j-1], charvec[j], charvec[j+1], "")

			id2 <- match(aux2, table_dicionario$ngrams)
		

			#qual eh a prob desses trios antes da troca?
			prob1 <- 0
			if(!is.na(id1)) 
				prob1 <- prob1 + table_dicionario$prop[id1]
			if(!is.na(id2)) 
				prob1 <- prob1 + table_dicionario$prop[id2]
			
			#qual é trio de i depois da troca?
			if(i == 1)
				aux1 <- concatenate(charvec[j], charvec[i+1], charvec[i+2], "")
			if(i == length(charvec))
				aux1 <- concatenate(charvec[i-2], charvec[i-1], charvec[j], "")
			if(i > 1 && i < length(charvec))
				aux1 <- concatenate(charvec[i-1], charvec[j], charvec[i+1], "")

			id1 <- match(aux1, table_dicionario$ngrams)
		
			#qual é trio de j depois da troca?
			if(j == 1)
				aux2 <- concatenate(charvec[i], charvec[j+1], charvec[j+2], "")
			if(j == length(charvec))
				aux2 <- concatenate(charvec[j-2], charvec[j-1], charvec[i], "")
			if(j > 1 && j < length(charvec))
				aux2 <- concatenate(charvec[j-1], charvec[i], charvec[j+1], "")

			id2 <- match(aux2, table_dicionario$ngrams)
		

			#qual eh a prob desses trios depois da troca?
			prob2 <- 0
			if(!is.na(id1)) 
				prob1 <- prob1 + table_dicionario$prop[id1]
			if(!is.na(id2)) 
				prob1 <- prob1 + table_dicionario$prop[id2]
			
	
			#se a prob depois for maior, faz a troca
				if(prob2 >= prob1){
					auxc <- charvec[i]
					charvec[i] <- charvec[j]
					charvec[j] <- auxc
					trocou <- 1
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
		#if(!trocou){
		#	i <- sample(1:length(charvec), 1)
		#	j <- sample(1:length(charvec), 1)
		#	auxc <- charvec[i]
                #        charvec[i] <- charvec[j]
                #        charvec[j] <- auxc

		#}
	}


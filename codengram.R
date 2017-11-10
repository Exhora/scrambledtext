library(ngram)

#lê o texto
	text <- readLines("scrambled_text.txt")
	str <- concatenate(text)
	chars <- splitter(str, split.char=TRUE, split.space=TRUE)
ngchars <- ngram(chars, n=2)
#imprimir
table_chars <- get.phrasetable(ngchars)

#monta um dicionario de chars
	musicas <- readLines("musicas.txt")
	mus <- concatenate(musicas)
	mus <- preprocess(mus, remove.punct=TRUE, remove.numbers=TRUE, fix.spacing=TRUE) 
	mus <- splitter(mus, split.char=TRUE, split.space=TRUE)
dicionario <- ngram(mus, n=2)

#imprime ngrams
table_dicionario <- get.phrasetable(dicionario)


#Codigo de tentativa
#separar a string em chars
	charvec <- strsplit(chars, " ")[[1]]
	count <- 0
	vero_old <- 0
	while(vero >= vero_old){
	count <- count +1
#fazer para cada par
		trocou = 0
		for(i in 2:length(charvec)-1){
			aux1 <- concatenate(charvec[i], charvec[i+1], "")
			aux2 <- concatenate(charvec[i+1], charvec[i], "")

			id1 <- match(aux1, table_dicionario$ngrams)
			id2 <- match(aux2, table_dicionario$ngrams)

			if(is.na(id1) || (!is.na(id2) && (table_dicionario$prop[id2] > table_dicionario$prop[id1]))){
				auxc <- charvec[i]
				charvec[i] <- charvec[i+1]
				charvec[i+1] <- auxc
			} 
		}
		#calcula a verossimilhanca
		vero_old <- vero
		vero <- 0
		for(i in 2:length(charvec)-1){
			aux1 <- concatenate(charvec[i], charvec[i+1], "")
			id1 <- match(aux1, table_dicionario$ngrams)
			if(!is.na(id1)){
				vero <- vero + table_dicionario$prop[id1]
			}
			print(vero)
		}
	}

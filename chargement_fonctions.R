#FUNCTIONS GENERALES

	## tweets
		tweets <- function(Gn,BDD)
		  {
		  t = BDD[which(BDD$from_user_id %in% Gn$from_user_id),]
		    clean=c() ; 
		    for (x in 1:nrow(words)) 
		      {clean=c(clean,grep(words[x,1],t$text,ignore.case = TRUE))}
		    clean=unique(clean)
		  t <- t[clean,]
		    tn <- table(t$from_user_id) ; ln <- names(tn)[order(tn)]
		    on=c() ; for (x in 1:length(ln)){on=c(on,which(t$from_user_id == ln[x]))}
		  t <- t[on,]
		  t$created_at <- strftime(sub("T"," ", t$created_at, ignore.case = FALSE),"%Y-%m-%d %H:%M:%S")
		  t
		  }

	## Codage Tweetos
		codage_tweetos <- function(tweets)
		  {
		  liste_tweetos=unique(tweets$from_user_id)
		  load=txtProgressBar(min=0, max=length(liste_tweetos), style = 3)
		  codage=data.frame(user=character(0), user_id=character(0), user_realname=character(0), 
				                  location=character(0), url=character(0),  bio=character(0),
				                  verified=logical(0), created_at=character(0), 
				                  n_tweets_BDD=numeric(0), v_tweets_periode=numeric(0),
				                  n_followers=numeric(0), v_followers_periode=numeric(0))
		  for (x in 1:length(liste_tweetos))
		    {
		    setTxtProgressBar(load,x)
		    user_id=liste_tweetos[x]
		    tweets_user=filter(tweets,from_user_id == user_id)
		    tweets_user=tweets_user[order(tweets_user$created_at),]

		    last_tweet=nrow(tweets_user)
		    first_tweet=1
		    created_at=tweets_user$from_user_created_at[first_tweet]
		    verified=sum(tweets_user$from_user_verified)>0
		    
		    user_name=c() ; user_name=str_c(unique(tweets_user$from_user_name),collapse=",")
		    user_realname=c() ; user_realname=str_c('%%% ',unique(tweets_user$from_user_realname),collapse='\n\n')
		    location=c() ; location=str_c('%%% ',unique(tweets_user$location),collapse='\n\n')
		    url=c() ; url=str_c('%%% ',unique(tweets_user$from_user_url),collapse='\n\n')
		    bio=c() ; bio=str_c('%%% ',unique(tweets_user$from_user_description),collapse='\n\n')
		  
		    n_tweets_BDD=nrow(tweets_user)
		    v_tweets_periode=numeric(1)
		      if (nrow(tweets_user)==1)
			{v_tweets_periode=1}
		      else
			{for (y in 1:(nrow(tweets_user)-1))
			  {if (tweets_user$from_user_tweetcount[y] < tweets_user$from_user_tweetcount[y+1])
			    {v_tweets_periode=v_tweets_periode+(tweets_user$from_user_tweetcount[y+1]-tweets_user$from_user_tweetcount[y])}}}
		    n_followers=tweets_user$from_user_followercount[last_tweet]
		    v_followers_periode=tweets_user$from_user_followercount[last_tweet]-tweets_user$from_user_followercount[first_tweet]
		    
		    codage=rbind(codage,
				data.frame(user=user_name, user_id=user_id, user_realname=user_realname, 
				          location=location, url=url,  bio=bio,
				          verified=verified, created_at=created_at, 
				          n_tweets_BDD=n_tweets_BDD, v_tweets_periode=v_tweets_periode,
				          n_followers=n_followers, v_followers_periode=v_followers_periode))
		    }
		  codage$created_at=as.Date(codage$created_at)
		  codage
		  }

		## Convert Names into ID
		name_to_id=unique(BDD_tweets[,c("from_user_name","from_user_id")])
		name_to_id=name_to_id[-which(duplicated(name_to_id$from_user_name)),]

		convert_name <- function(names)
		  {
		  tmp=match(names,name_to_id$from_user_name,nomatch=NA)
		  
		  tmp2=integer64(length(tmp))
		  tmp2[which(!is.na(tmp))]=name_to_id$from_user_id[na.omit(tmp)]
		  tmp2[which(is.na(tmp))]=NA
		  
		  tmp2
		  }

	## Convert ID into Names
		convert_id <- function(id)
		  {
		  tmp=match(as.character(id),as.character(name_to_id$from_user_id),nomatch=NA)
		  if (any(is.na(tmp)))
		    {print("Au moins une ID inconnue")}
		  else
		    {tmp=name_to_id$from_user_name[tmp] ; tmp}
		  }

	## Extract tweet edges et apply extract tweet edges
		extract_tweet_edges <- function(tweet)
		  {
		  edges=data.frame(source_id=character(0),source=character(0),target=character(0),target_id=character(0),type=character(0),time=character(0),ID_tweet=character(0))
		  is_RT=str_starts(tweet$text,"RT\\s@[^\\:]+\\:")
		  if (is_RT) #traitement à part des RT
		    {
		    target=str_extract(tweet$text,"(?<=RT\\s@)[^\\:]+(?=\\:)")
		    edges=data.frame(source_id=tweet$from_user_id,
				      source=tweet$from_user_name,
				      target=target,
				      target_id=convert_name(target),
				      type="RT",
				      time=tweet$created_at,
				      ID_tweet=tweet$id)}
		  else
		    {if (str_ends(tweet$text,"twitter\\.com/[^/]+/status/[:digit:]+")) #RT commenté = lien du type twitter.com/status/[ID tweet] en fin de tweet
		      {
		      target=str_extract(tweet$text,"(?<=twitter\\.com/)[^/]+(?=/status/[:digit:]+)")
		      edges=data.frame(source_id=tweet$from_user_id,
				        source=tweet$from_user_name,
				        target=target,
				        target_id=convert_name(target),
				        type="RT_comment",
				        time=tweet$created_at,
				        ID_tweet=tweet$id)}
		    mentions=as.vector(str_remove(str_extract_all(tweet$text,"(?<!\\S)@[^\\s]+")[[1]],"@"))
		    l=length(mentions)
		    if (l>0) #mentions
		    {
		      edges=rbind(edges,data.frame(source_id=rep(tweet$from_user_id,l),
				                  source=rep(tweet$from_user_name,l),
				                  target=mentions,
				                  target_id=convert_name(mentions),
				                  type=rep("mention",l),
				                  time=rep(tweet$created_at,l),
				                  ID_tweet=rep(tweet$id,l)))}
		    }
		  edges  
		  }

		apply_extract_tweet_edges <- function(tweets)
		  {
		  edges=data.frame(source=character(0),target=character(0),type=character(0),time=character(0),ID_tweet=character(0))
		  pb=txtProgressBar(min=0, max=nrow(tweets), style = 3)
		  for (n in nrow(tweets):1)
		    {edges=rbind(edges,extract_tweet_edges(tweets[n,]))
		    setTxtProgressBar(pb,n)}
		  edges
		  }


#FUNCTIONS NOUVELLES VARIABLES

	## %RT
		var_pourcentage_RT <- function(tweets,users)
		  {
		  output <- data.frame(user_id=users, RTpercent=numeric(length(users)))
		  for (x in 1:length(users))
		    {
		    user_tweets <- which(tweets$from_user_id == output$user_id[x])
		    output$RTpercent[x] <- length(which(str_detect(tweets$text[user_tweets], "^(RT @)+")))/length(user_tweets)*100
		    }
		  output
		  }

		var_pourcentage_RT_via <- function(tweets,users)
		  {
		  output <- data.frame(user_id=users, RT_via_percent=numeric(length(users)))
		  for (x in 1:length(users))
		    {
		    user_tweets <- which(tweets$from_user_id == output$user_id[x])
		    output$RT_via_percent[x] <- length(which(str_detect(tweets$text[user_tweets], "(via @[^\\s:]+$)")|str_detect(tweets$text[user_tweets], "^(RT @)+")))/length(user_tweets)*100
		    }
		  output
		  }
	## Active period
		var_active_period <- function(tweets,users)
		  {
		  output <- data.frame(user_id=users, active_period=numeric(length(users)), first_vacc_tweet=numeric(length(users)), last_vacc_tweet=numeric(length(users)))
		  for (x in 1:length(users))
		    {
		    user_tweets <- tweets$created_at[which(tweets$from_user_id == output$user_id[x])]
		    dmax <- max(user_tweets) ; dmin <- min(user_tweets) ; periode <- difftime(dmax,dmin, units = "days")
		    if (periode == 0) 
		      {output$active_period[x] <- 1.0000} 
		    else 
		      {output$active_period[x] <- periode}
		    output$first_vacc_tweet[x] <- dmin ; output$last_vacc_tweet[x] <- dmax
		    }
		  output
		  }

	## Longest inactive period
		var_longest_inactive_period <- function(tweets,users)
		  {
		  output <- data.frame(user_id=users, inactive_period_max=numeric(length(users)))
		  for (x in 1:length(users))
		    {
		    user_tweets <- sort(tweets$created_at[which(tweets$from_user_id == output$user_id[x])])
		    if (length(user_tweets)>1)
		      {
		      period <- c()
		      for (y in 1:(length(user_tweets)-1))
			{period <- c(period,difftime(user_tweets[y+1],user_tweets[y], units = "days"))}
		      output$inactive_period_max[x] <- max(period)
		      }
		    else
		      {output$inactive_period_max[x] <- NA}
		    }
		  output
		  }

	## Capslock
		var_capslock_detector <- function(tweets,users)
		  {
		  output <- data.frame(user_id=users, taux_majuscules=numeric(length(users)))
		  for (x in 1:length(users))
		    {
		    user_tweets <- which(tweets$from_user_id == output$user_id[x])
		    user_tweets <- user_tweets[-grep("^(RT @)",tweets$text[user_tweets])]
		    if (length(user_tweets) == 0)
		      {output$taux_majuscules[x] <- NA}
		    else
		      {
		      tweets_without_url <- str_remove_all(tweets$text[user_tweets], "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+")
		      output$taux_majuscules[x] <- sum(str_count(tweets_without_url, "[A-Z]")) / sum(nchar(tweets_without_url))
		      }
		    }
		  output
		  }

	## N RT mentions
		var_RT_mentions <- function(tweets,users) #Calcul sur tous les tweets à l'exception de ceux produits par l'user
		  {
		  output <- data.frame(user_id=users, N_RT=numeric(length(users)), N_mentions=numeric(length(users)), N_sourceMRT=numeric(length(users)))
		  load=txtProgressBar(min=0, max=length(users), style = 3)
		  for (x in 1:length(users))
		    {
		    setTxtProgressBar(load,x)
		    user_tweets <- which(tweets$from_user_id == output$user_id[x])
		    if (length(user_tweets) == 0)
		      {output$N_RT[x] <- NA ; output$N_mentions[x] <- NA ; output$N_sourceMRT[x] <- NA}
		    else
		      {
		      user_names <- unique(tweets$from_user_name[user_tweets])
		      n_rt <- 0 ; n_m <- 0 ; ns_mrt <- 0
		      for (y in 1:length(user_names))
			{
			m <- grep(paste("(^|[^@\\w])@",user_names[y],"\\b",sep=""),tweets$text[-user_tweets],perl=T)
			if (length(m)>0)
			  {
			  n_m <- n_m + length(m)
			  ns_mrt <- ns_mrt + length(unique(tweets$from_user_id[m]))
			  n_rt <- n_rt + length(grep(paste("^(RT @)",user_names[y],"\\b",sep=""),tweets$text[-user_tweets]))
			  }
			}
		      output$N_mentions[x] <- n_m - n_rt
		      output$N_RT[x] <- n_rt
		      output$N_sourceMRT[x] <- ns_mrt
		      }
		    }
		  output
		  }

	## Repetition tweets
		var_repetition_tweet <- function(tweets,users)
		  {
		  l <- length(users) ; load=txtProgressBar(min=0, max=l, style = 3) 
		  output <- data.frame(user_id=users, repetition=numeric(l))
		  for (x in 1:l)
		    {
		    setTxtProgressBar(load,x)
		    user_tweets <- tweets$text[which(tweets$from_user_id == output$user_id[x])]
		    if (length(user_tweets) == 0)
		      {output$moyenne_repetition[x] <- NA}
		    else
		      {
		      user_tweets <- str_remove_all(user_tweets, "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+")
		      user_tweets <- str_remove_all(user_tweets, "(^|[^@\\w])@(\\w{1,15})\\b")
		      n <- 0
		      for (y in 1:length(user_tweets))
			{
			if (user_tweets[y] != "")
			  {
			  rep <- which(user_tweets == user_tweets[y])
			  if (length(rep) > 1)
			    {n <- n + length(rep) ; user_tweets[rep] <- ""}
			  }
			}
		      output$repetition[x] <- n / length(user_tweets)
		      }
		    }
		  output
		  }

	## Listes
		unaccent <- function(text) 
		  {
		  text <- gsub("['`^~\"]", " ", text)
		  text <- iconv(text, to="ASCII//TRANSLIT//IGNORE")
		  text <- gsub("['`^~\"]", "", text)
		  return(text)
		  }

		detection_listes <- function(tweets,mots_liste,min_liste,users)
		  {
		  load=txtProgressBar(min=0, max=length(users), style = 3) 
		  output <- data.frame(tweet_id=character(0), user_id=character(0), text=character(0), longueur_liste=numeric(0))
		  for (x in 1:length(users))
		    {
		    setTxtProgressBar(load,x)
		    user_tweets <- tweets[which(tweets$from_user_id == users[x]),c("id","text")]
		    if (length(user_tweets) != 0)
		      {
		      user_tweets <- user_tweets[-grep("^(RT @)",user_tweets$text),]
		      user_tweets$text <- str_remove_all(user_tweets$text, "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+")
		      user_tweets$text <- str_remove_all(user_tweets$text, "(^|[^@\\w])@(\\w{1,15})\\b")
		      user_tweets$text <- tolower(user_tweets$text)
		      user_tweets$text <- str_replace_all(user_tweets$text, "[[:punct:]]"," ")
		      user_tweets$text <- unaccent(user_tweets$text)
		      user_tweets$longueur_liste <- 0
		      for (y in 1:length(mots_liste))
			{
			present <- grep(paste("(^|[^\\w])",mots_liste[y],sep=""),user_tweets$text,perl=T)
			user_tweets$longueur_liste[present] <- user_tweets$longueur_liste[present] +1
			}
		      user_tweets <- user_tweets[which(user_tweets$longueur_liste >= min_liste),]
		      output <- rbind(output,data.frame(id=user_tweets$id, user_id=rep(users[x],nrow(user_tweets)),
				                        text=user_tweets$text, longueur_liste=user_tweets$longueur_liste))
		      }
		    }
		  return(output)
		  }

		var_taux_liste <- function(tweets,listes,users)
		  {
		  l <- length(users) ; rt <- grep("^(RT @)",tweets$text)
		  load=txtProgressBar(min=0, max=l, style = 3) 
		  output <- data.frame(user_id=users, taux_listes=numeric(l), n_listes=numeric(l), longueur_max_listes=numeric(l), longueur_moy_listes=numeric(l))
		  for (x in 1:l)
		    {
		    user_tweets <- which(tweets$from_user_id[-rt] == users[x])
		    if (length(user_tweets) == 0)
		      {output$taux_listes[x] <- NA ; output$n_listes[x] <- NA ; output$longueur_max_listes[x] <- NA ; output$longueur_moy_listes[x] <- NA}
		    else
		      {
		      user_listes <- which(listes$user_id == users[x])
		      if (length(user_listes) == 0)
			{output$taux_listes[x] <- 0 ; output$n_listes[x] <- 0 ; output$longueur_max_listes[x] <- 0 ; output$longueur_moy_listes[x] <- 0}
		      else
			{
			output$taux_listes[x] <- length(user_listes)/length(user_tweets)
			output$n_listes[x] <- length(user_listes)
			output$longueur_max_listes[x] <- max(listes$longueur_liste[user_listes])
			output$longueur_moy_listes[x] <- mean(listes$longueur_liste[user_listes])
			}
		      }
		    setTxtProgressBar(load,x)
		    }
		  return(output)
		  }

	## Usage antivaccin
		var_usage_antivac <- function(tweets,users)
		  {
		  l <- length(users) ; rt <- grep("^(RT @)",tweets$text) ; load <- txtProgressBar(min=0, max=l, style = 3)
		  output <- data.frame(user_id=users, n_antivac=numeric(l), taux_antivac=numeric(l))
		  usage_anti <- grep("anti(\\s|-)?va(c|x){1}", tweets$text[-rt], perl=T, ignore.case =T)
		  for (x in 1:l)
		    {
		    ntweet <- length(which(tweets$from_user_id[-rt]==users[x]))
		    if (ntweet == 0)
		      {output$n_antivac[x] <- NA ; output$taux_antivac[x] <- NA ; setTxtProgressBar(load,x)}
		    else
		      {
		      nantivac <- length(which(tweets$from_user_id[-rt][usage_anti]==users[x]))
		      output$n_antivac[x] <- nantivac
		      output$taux_antivac[x] <- nantivac / ntweet
		      setTxtProgressBar(load,x)
		      }
		    }
		  return(output)
		  }

	## Taux tweets généraux
		var_taux_tweets_generaux <- function(tweets,mots,users)
		  {
		  lu <- length(users) ; lm <- nrow(mots) ; load=txtProgressBar(min=0, max=lu, style = 3)
		  output <- data.frame(user_id=users, taux_tweets_generaux=numeric(lu))
		  for (x in 1:lu)
		    {
		    user_tweets <- tweets$text[which(tweets$from_user_id == users[x])]
		    user_tweets <- user_tweets[-grep("^(RT @)",user_tweets)]
		    if (length(user_tweets)==0)
		      {output$taux_tweets_generaux[x] <- NA}
		    else
		      {
		      user_tweets <- str_remove_all(user_tweets, "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+")
		      user_tweets <- str_remove_all(user_tweets, "(^|[^@\\w])@(\\w{1,15})\\b")
		      user_tweets <- tolower(user_tweets)
		      user_tweets <- str_replace_all(user_tweets, "[[:punct:]]"," ")
		      user_tweets <- str_replace_all(user_tweets, "©"," ")
		      user_tweets <- unaccent(user_tweets)
		      indices <- c()
		      for (y in 1:lm)
			{
			if (mots$type_recherche[y] == "word")
			  {
			  pattern <- paste("\\<",mots$recherche[y],"\\>",sep = "")
			  indices <- c(indices,grep(pattern,user_tweets))
			  }
			else if (mots$type_recherche[y] == "begin")
			  {
			  pattern <- paste("\\<",mots$recherche[y],sep = "")
			  indices <- c(indices,grep(pattern,user_tweets))
			  }
			else if (mots$type_recherche[y] == "reg")
			  {
			  pattern <- str_replace_all(mots$recherche[y],",","|")
			  pattern <- paste("(",pattern,")",sep="")
			  indices <- c(indices,grep(pattern,user_tweets))
			  }
			}
		      output$taux_tweets_generaux[x] <- 1-length(unique(indices)) / length(user_tweets)
		      }
		    setTxtProgressBar(load,x)
		    }
		  return(output)
		  }

#FUNCTIONS RECHERCHES MOTS CLÉS

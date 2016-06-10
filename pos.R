extractPOS = function(x, thisPOSregex) {
  # Convert input to string
  x <- as.String(x)
  # Annotate line and word
  wordAnnotation <- annotate(x, list(Maxent_Sent_Token_Annotator(), Maxent_Word_Token_Annotator()))
  # POS tag
  POSAnnotation <- annotate(x, Maxent_POS_Tag_Annotator(), wordAnnotation)
  # subset of the column word
  POSwords <- subset(POSAnnotation, type == "word")
  # Custom Marking for Regex
  tags <- sapply(POSwords$features, '[[', "POS")
  # Apply regualar expression
  thisPOSindex <- grep(thisPOSregex, tags)
  # Extract tagged words
  tokenizedAndTagged <- sprintf("%s/%s", x[POSwords][thisPOSindex], tags[thisPOSindex])
  untokenizedAndTagged <- paste(tokenizedAndTagged, collapse = " ")
  untokenizedAndTagged
}

emotions = function(TaggedData){
  # verb, noun, adjective and adverb represent emotions well 
  # http://www.aclweb.org/anthology/Y10-1013
  # Extract all emotions realted words from the POS tagger
  
  # Possive Pronouns 
  pron = lapply(TaggedData, extractPOS, "PRP")
  pron_list = unlist(str_extract_all(unlist(pron), "\\w+(?=\\/)"))
  # AdVerb
  advrb = lapply(TaggedData, extractPOS, "RB")
  advrb_list = unlist(str_extract_all(unlist(advrb), "\\w+(?=\\/)"))
  # Nouns (Tags all variagtion NN, NNS, NNP)
  noun =lapply(TaggedData, extractPOS, "NN")
  noun_list = unlist(str_extract_all(unlist(noun), "\\w+(?=\\/)"))
  # Verbs
  vrb = lapply(TaggedData, extractPOS, "VB")
  vrb_list = unlist(str_extract_all(unlist(vrb), "\\w+(?=\\/)"))
  # Adjectives
  adj = lapply(TaggedData, extractPOS, "JJ")
  adj_list = unlist(str_extract_all(unlist(adj), "\\w+(?=\\/)"))
  # Combine all extracted emotions
  combin = c(adj_list,vrb_list,noun_list,advrb_list,pron_list)
  
  # Combine all Lists
  return(combin)
}



emotionbook = function(emo_book,id,title){
  # Find the intersection terms between the book and the emotion lexicon
  angerlist = intersect(emo_book,anger)
  antlist = intersect(emo_book,anticipation)
  dislist = intersect(emo_book,disgust)
  fearlist = intersect(emo_book,fear)
  joylist = intersect(emo_book,joy)
  sadlist = intersect(emo_book,sadness)
  suplist = intersect(emo_book,surprise)
  trustlist = intersect(emo_book,trust)
  
  # Compute the emotional percenatge in the book for each emotion
  totallist = c(angerlist,antlist,dislist,fearlist,joylist
                ,sadlist,suplist,trustlist)
  tot = length(totallist)
  angerp = length(angerlist)/tot
  angerp = (round(angerp,2))
  anticipationp = length(antlist)/tot
  anticipationp = (round(anticipationp,2))
  disgustp = length(dislist)/tot
  disgustp =  (round(disgustp,2))
  fearp = length(fearlist)/tot
  fearp = (round(fearp,2))
  joyp = length(joylist)/tot
  joyp = (round(joyp,2))
  sadp = length(sadlist)/tot
  sadp = (round(sadp,2))
  surprisep = length(suplist)/tot
  surprisep = (round(surprisep,2))
  trustp = length(trustlist)/tot
  trustp = (round(trustp,2))
  
  # Combine the computation with book details
  bookemotion = as.data.frame(cbind(id,title,angerp,anticipationp,
                                    disgustp,fearp,joyp,
                                    sadp,surprisep,trustp))
  return(bookemotion)
}
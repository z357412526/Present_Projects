#a
##load libraries
require(XML)
require(curl)
require(RCurl)
require(stringr)
##extract URL from the main
mainPage <-"http://www.debates.org/index.php?page=debate-transcripts"
#links <- getHTMLLinks(mainPage)
parseLink = htmlParse(mainPage)
#listOfANodes <- getNodeSet(parseLink, "//a[@href]")
###find candidate namelist
First=getNodeSet(parseLink, "//a[contains(text(),'First')]")
findnames = unique(grep ("(1996|2000|2004|2008|2012)",
                         lapply(First,toString.XMLNode),value=TRUE))
namelist = unlist(str_extract_all(findnames,"([A-Z][a-z]+)-[A-Z][a-z]+"))
#it actually use name "McCain" instead of "Cain"
namelist[2] = "McCain-Obama"

##find out datasets needed
datasets= sapply(First, xmlGetAttr, "href")
repp = unique(grep ("(1996|2000|2004|2008|2012)",datasets,value=TRUE))

#run repp there are special cases without domain. Therefore we add them in. 
#CLEAN = grep('(http)',repp,invert=TRUE) 
#repp[CLEAN] = paste0("http://www.debates.org/", repp[CLEAN])

#b
##EXTRACT TEXTS
extract.txt= function(html){
  html = getURL(html)
  doc <- htmlParse(html, asText=TRUE)
  plain.text <- xpathSApply(doc, "//p/text()", xmlValue)
  return(plain.text)
}
##write onto txt
for (i in 1:length(repp)){
  a = extract.txt(repp[i])
  years = str_extract(repp[i],"[:digit:]{4}" ) ##EXTRACT YEAR
  write(a,file=paste(years, "debate.txt", sep = ""))
}


#C
#after taking a look at the txt file we got from #b, it is not that clean at 
# the first glance. Now we first clean it and then put them in a better manner

#helper func1---select names: 
#input- a namelist from property of files, it's 
  #a list with elements like"Obama-Romney"
  #year of speech
#output- a list candidate that year
select_name<-function(namelist,year){
  years = rev(c(1996,2000,2004,2008,2012))
  idx = seq(1,5)
  my_idx = idx[years==year]
  candidate = str_split(namelist,"-")
  return(candidate[[my_idx]])
}
#helper func2---split chunks: make candidate text in turn
#input- candidates namelist and corresponding text
#output- a table that candidate state in turn
split_chunks<-function(candi_names,candi_text_list){
  if (length(candi_names)!=length(candi_text_list)) {
    print("invalid input")
    return
  }
  idx=which(candi_names[1:(length(candi_names)-1)]
            ==candi_names[2:(length(candi_names))])
  #print(idx)
  candi_names2 = candi_names[-idx]
  for (i in rev(idx)){
    candi_text_list[i] = paste (candi_text_list[i],candi_text_list[i+1])
  }
  candi_text_list2 = candi_text_list[-(idx+1)]
  debate_tbl= cbind(candi_names2,candi_text_list2)
  colnames(debate_tbl) = c("name","text")
  return(debate_tbl)
}
##check
clean_text <-function(namelist,year){
  filename = paste(year,"debate.txt",sep="")
  #print(filename)
  textfile =  readLines(filename)
  textfile<-paste(textfile,collapse=" ")
  n_laugter = str_count(textfile,"\\(LAUGHTER\\)")
  n_applause = str_count(textfile,"\\(APPLAUSE\\)")
  cat("Count laugther and applause in: ", year)
  print(c(count_laugter=n_laugter,count_applause=n_applause))
  textfile<-gsub("\\(LAUGHTER\\)|\\(APPLAUSE\\)|\\(CROSSTALK\\)","",
                 textfile, ignore.case=TRUE)

  ##notice there always just title in the first line, to match names and text,
  ##select out the names
  names = unlist(str_extract_all(textfile, "[A-Z]+: "))
  cand1 = paste0(toupper(select_name(namelist,year)[1]),": ")#;print(cand1)
  cand2 = paste0(toupper(select_name(namelist,year)[2]),": ")#;print(cand2)
  which((names==cand1)|(names==cand2))
  candi_names = names[(names==cand1)|(names==cand2)]
  ##we just keep the debates between candidates
  text_list = str_split(textfile,pattern = "[A-Z]+: ")
  text_list = unlist(text_list)[-1] 
  candi_text_list = text_list[(names==cand1)|(names==cand2)]
  ##split chunks
  debate_tbl = split_chunks(candi_names,candi_text_list)
  return(debate_tbl)
}
test1 = clean_text(namelist,1996)

#D
sentence_spliter<-function(table){
  sentences=str_split(table[,2],pattern = "\\. |\\? |\\.\\.\\. ")
  return(sentences)
}

word_spliter<-function(table){
  words = str_extract_all(pattern="([\\w][\\w\\']*\\w|\\w)", table[,2])
  return(words)
}

#E
reconstruct<-function(table){
  # wordoflist=word_spliter(table)
  candidate = table[1:2,1]
  cand_speech1=NULL
  cand_speech2=NULL
  for (i in 1:dim(table)[1]){
    #statement of first candidate
    if (i%%2){
      cand_speech1 = paste(cand_speech1,table[i,2])
    }
    #statment of second candidate
    else{
      cand_speech2 =  paste(cand_speech2,table[i,2])
    }
  }
  rectr_tbl = cbind(candidate,c(cand_speech1,cand_speech2))
  return(rectr_tbl)
}

count_word_char<-function(table){
  rectr_tbl = reconstruct(table)
  wordoflist=word_spliter(rectr_tbl)
  count_word1 = length(wordoflist[[1]])
  count_word2 = length(wordoflist[[2]])#;print(count_word2)
  count_char1 = sum(nchar(wordoflist[[1]]))
  count_char2 = sum(nchar(wordoflist[[2]]))
  count_word = c(count_word1,count_word2)
  count_char = c(count_char1,count_char2)
  count_table = data.frame(matrix(nrow=2, ncol=4))
  count_table[,1] = rectr_tbl[,1]
  count_table[,2] = count_word
  count_table[,3] = count_char
  count_table[,4] = count_char/count_word
  colnames(count_table) = c("candidate","count_word","cound_char","average")
  return(count_table)
}
#count_word_char(test1)

##get tables
years = c(1996,2000,2004,2008,2012)
summary_table = data.frame(matrix(nrow = length(years)*2, ncol = 3))
for (i in 1:length(years)){
  table=clean_text(namelist,years[i])
  count_table = count_word_char(table)
  print(count_table)
  summary_table[(2*i-1):(i*2),] = count_table
}
colnames(summary_table) = c("candidate","count_word","count_char")
summary_table

#F
##reconstruct the data set
##for each debate with given keyword
#key is has been clean here
convert_key<-function(key){
  if (key=="I") {key="I\\b"}
  else if (key=="we") {key="we\\b|We\\b"}
  else if (key=="America{,n}") {key="American?\\b"}
  else if (key=="democra{cy,tic}") {key="democracy\\b|democratic\\b"}
  else if (key=="republic") {key="republic"}
  else if (key=="Democrat{,ic}") {key="Democrat(ic)?\\b"}
  else if (key=="Republican") {key="Republicans?\\b"}
  else if (key=="free{,dom}") {key="(F|f)ree(dom)?\\b"}
  else if (key=="war") {key="(W|w)ars?\\b"}
  else if (key=="God") {key="(G|g)od (?!bless)"}
  else if (key=="{Jesus, Christ, Christian}") {key="Jesus|Christs?\\b|Christians?"}
  else if (key=="God Bless") {key="(G|g)od bless"}
  else {print("I don't care this word")}
  return(key)
}
keys = c("I", "we", "America{,n}", 
         "democra{cy,tic}", "republic", "Democrat{,ic}", "Republican", 
         "free{,dom}", "war", "God", "God Bless", 
         "{Jesus, Christ, Christian}")

count_key<- function(table,key){
  key = convert_key(key)
  rectr_tbl = reconstruct(table)
  #candidate = rectr_tbl[,1]
  #print(candidate)
  clean_punc1 = str_replace_all(rectr_tbl[1,2],pattern="[:punct:]"," ")
  clean_punc2 = str_replace_all(rectr_tbl[2,2],pattern="[:punct:]"," ")
  count_k1 = str_count(clean_punc1,key)
  count_k2 = str_count(clean_punc2,key)
  #return(list(candidates=candidate,count=c(count_k1,count_k2)))
  return(c(count_k1,count_k2))
}

#convert_keys=NULL
#for (i in 1:length(keys)) {
 # convert_keys = c(convert_keys,convert_key(keys[i]))
#}

count_byyear<-function(year){
  table = clean_text(namelist,year)
  candidates = table[1:2,1]
  countit=NULL
  for (i in 1:length(keys)){
    temp = count_key(table,keys[i])
    countit=cbind(countit,temp)
  }
  rownames(countit)=candidates
  colnames(countit)=keys
  return(countit)
}
keywords_count_table = NULL
for (i in years){
  temp = count_byyear(i)
  keywords_count_table=rbind(count_table,temp)
}
print(keywords_count_table)












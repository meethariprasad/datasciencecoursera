sample<-c("hello i love.","mylove")
for(i in 1:length(sample)){
  test <- unlist(strsplit(sample, " "))
  t<-hunspell(test)
  t<-as.character(t)
  misspelt<-t["character(0)"!=t]
  if(length(misspelt)!=0){
    sample<-removeWords(sample,misspelt)}
}
word_token <- unlist(strsplit(sample, " "))

#read the data into 'dictionary'. Split every word into separate letters and store the list into 'letter_table'. Count the number of letters of each word and store into 'letter_length'

setwd("C:/Trexquant")
dictionary<-as.matrix(read.table('words.txt'))
letter_table<-lapply(dictionary,strsplit,split="")
letter_length<-unlist(lapply(as.matrix(letter_table),lapply,length))

#we need to use the package 'stringr'. Install the package if haven't downloaded.
#install.packages('stringr')
library('stringr')

#create a helper function 'frequency'. This function is used to count the frequency of each letters in the input wordlist, and return the letter of the specific frequency order the user wanted. The default return value is the top frequency letter.
frequency<-function(list,rank=1){
  letters=c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z')
  letters_count<-NULL
  for(i in 1:26){
    count=sum(sapply(list,function(x)sum(str_count(x,letters[i]))))
    letters_count<-c(letters_count,count)
  }
  letters_frequency=cbind.data.frame(letters,letters_count)
  pos<-order(letters_frequency[,2],decreasing=TRUE)[rank]
  fre<-as.character(letters_frequency[order(letters_frequency[,2],decreasing=TRUE)[rank],1])
  return(fre)
}


#Write a function for the driver program. The inputs includes the word we want to guess, and the letter_table & letter_length we get before)
driver_program<-function(guess_word, letter_table, letter_length){
  
  #The first step of our algorithm would be creating a subset using the length of the guessword. When we start the hangman game, the only thing we know is the word length. So we create a subset A of the dictionary using the word-length, and start guessing from the top-frequency letter in A
  length<-nchar(guess_word)
  blanks<-rep('-',length)
  line<-c(blanks,'missed:','\n')
  cat(line)
  #create the subset
  subset_list<-letter_table[letter_length==length]
  subset_table<-dictionary[letter_length==length]
  guess_letter<-frequency(subset_list)
  count=1
  guessed_letters=NULL
  missed_letters=NULL
  
  #after guessing the first letter, there are two conditions:
  #1. we've guessed the right letter, and we know the position of this letter. Under this circumstances, we could create the subset of words with the guessed letter in the exact position.
  #2. we failed to guess the right letter. Under this circumstance, we could create the subset of words without the letter we guessed
  #3. The next guessing number would be the top frequency letter of our current subset. Also, we have to judge whether we have used this guessing letter before. If we had, then we choose the 2nd highest frequency letter...
  #4. keep repeating the process, until we have tried 6 times or we have guessed out the whole word.
  while(count<=6 && any(blanks!=unlist(strsplit(guess_word,"")))){
    line<-c('guess: ',guess_letter,'\n')
    cat(line)
    if(grepl(guess_letter,guess_word)){
      pos<-which(strsplit(guess_word,"")[[1]]==guess_letter)
      blanks[pos]=guess_letter
      subset_list<-subset_list[sapply(subset_list,function(x)x[pos]==guess_letter)]
      subset_table<-subset_table[sapply(subset_list,function(x)x[pos]==guess_letter)]
      line<-c(blanks,'missed: ', missed_letters,'\n')
    }else{
      subset_list<-subset_list[!sapply(subset_table,grepl,pattern=guess_letter)]
      subset_table<-subset_table[!sapply(subset_table,grepl,pattern=guess_letter)]
      missed_letters=c(missed_letters,guess_letter)
      line<-c(blanks,'missed: ',missed_letters,'\n')
    }
    guessed_letters<-c(guessed_letters,guess_letter)
    cat(line)
    rank=1
    while(any(grepl(frequency(subset_list,rank),guessed_letters))){
      rank=rank+1
    }
    guess_letter<-frequency(subset_list,rank)
    count=count+1
  }
  
  if(all(unlist(strsplit(guess_word,""))==blanks)){
    win=TRUE
  }else{
    win=FALSE
  }
  return(win)
}

#using several word to test our driver program. I used 'abbatie', 'abbott' and 'alle' here
driver_program('abbatie',letter_table,letter_length)
driver_program('abbott',letter_table,letter_length)
driver_program('alle',letter_table,letter_length)


#to run through the whole dictionary
game<-function(guess_word, letter_table, letter_length){
  length<-nchar(guess_word)
  blanks<-rep('-',length)
  
  #get the high frequency letter in the subset
  subset_list<-letter_table[letter_length==length]
  subset_table<-dictionary[letter_length==length]
  guess_letter<-frequency(subset_list)
  
  count=1
  guessed_letters=NULL
  while(count<=6 && any(blanks!=unlist(strsplit(guess_word,"")))){
    if(grepl(guess_letter,guess_word)){
      pos<-which(strsplit(guess_word,"")[[1]]==guess_letter)
      blanks[pos]=guess_letter
      subset_list<-subset_list[sapply(subset_table,grepl,pattern=guess_letter)]
      subset_table<-subset_table[sapply(subset_table,grepl,pattern=guess_letter)]
    }else{
      subset_list<-subset_list[!sapply(subset_table,grepl,pattern=guess_letter)]
      subset_table<-subset_table[!sapply(subset_table,grepl,pattern=guess_letter)]
    }
    guessed_letters<-c(guessed_letters,guess_letter)
    rank=1
    while(any(grepl(frequency(subset_list,rank),guessed_letters))){
      rank=rank+1
    }
    guess_letter<-frequency(subset_list,rank)
    count=count+1
  }
  
  if(all(unlist(strsplit(guess_word,""))==blanks)){
    win=TRUE
  }else{
    win=FALSE
  }
  return(win)
}

result<-sapply(dictionary, game, letter_table=letter_table, letter_length=letter_length)

for(i in 1:length(dictionary)){
  result<-c(result,game(dictionary[i],letter_table,letter_length))
}
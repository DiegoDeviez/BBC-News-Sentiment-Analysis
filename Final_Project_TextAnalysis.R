install.packages("stm")
install.packages("psych")
install.packages("ggplot2")
install.packages("MASS")
install.packages("tidystm")
install.packages("tm")
install.packages("tm")
install.packages("SnowballC")
install.packages("htmltools")
install.packages("devtools")
install.packages("tidystm")
install.packages("readxl")

library(stm) # Package for sturctural topic modeling
library(psych) # function decribe
library(ggplot2) # function ggsave, ggplot
library(MASS) # function write.matrix
library(tm)
library(SnowballC)
library(htmltools)
library(devtools)
library(readxl)


rm(list = ls())
setwd("~/Desktop/Final Project") # replace with the file path

# read the text file
data = read_excel('bbcnews2.xlsx')
nrow(data)
head(data)

# remove letters that are too short (fewer than 10 words)
d = subset(data, data$nword > 10)
nrow(d)
describe(d$nword) # get average word length

# only keep columns that are needed (covariates and documents)
l = d[c('letter','sentiment')]
l$letter <- as.character(l$letter)  
table(l$sentiment)
l$sentiment <- factor(l$sentiment)
table(l$sentiment)

# preprocess
processed <- textProcessor(l$letter, metadata=l, removestopwords = TRUE,
                           customstopwords=c("news","News","bbcnews", "BBCNews", "BBCnews", "BBCnews"))
str(processed)  

# create a folder to save results
cwd = "~/Desktop/Final Project/results1"
resultpath = paste(cwd,nrow(d),sep="")
resultpath; dir.create(resultpath); 

# prepare documents
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta
vocab

#select models (decide the number of topics)
findingk <- searchK(out$documents, out$vocab, K = c(3:12), # topic range
                    prevalence = ~sentiment, data = out$meta, verbose=FALSE)
findingk$results
dfindingK = data.frame(findingk$results)
dfindingK

resultpath = paste(cwd,nrow(d),'/diagnosis',sep="")
resultpath; dir.create(resultpath); 
setwd(resultpath)

quartz(width=10, height=10)
plot(findingk)
dev.copy2pdf(file = paste("diagosis",".pdf", sep=""))
dev.off()

dfindingK

#unlist semcoh and exclus variables
dfindingK$semcoh <- unlist(dfindingK$semcoh)
dfindingK$exclus <- unlist(dfindingK$exclus)

#create plot using discrete semcoh and exclus variables
ggplot(data = dfindingK, aes(x = semcoh, y = exclus, label = K)) +
  geom_point(size = 2, shape=1) + 
  geom_text(hjust=-0.5) + theme_classic(base_size = 18)+
  labs(x = "Semantic coherence",
       y = "Exclusivity")
ggsave("semantic-exclusivity.png",w=6,h=4)

#unlist heldout and K variables
dfindingK$heldout <- unlist(dfindingK$heldout)
dfindingK$K <- unlist(dfindingK$K)

#reate plot using discrete K and heldout
ggplot(data = dfindingK, aes(x=K , y=heldout)) +
  geom_point(size = 2, shape=1) + 
  theme_classic(base_size = 18)+geom_path()+
  scale_x_continuous(breaks = seq(0, 20, by = 1)) +
  labs(x = "Number of topics",
       y = "Held-out likelihood")
ggsave("held-out likelihood.png",w=6,h=4)




# build model
tfit4 <- stm(out$documents, out$vocab, K=4, prevalence=~sentiment, max.em.its=75, data=out$meta, init.type="Spectral",seed=1)
tfit5 <- stm(out$documents, out$vocab, K=5, prevalence=~sentiment, max.em.its=75, data=out$meta, init.type="Spectral",seed=1)
tfit6 <- stm(out$documents, out$vocab, K=6, prevalence=~sentiment, max.em.its=75, data=out$meta, init.type="Spectral",seed=1)
tfit7 <- stm(out$documents, out$vocab, K=7, prevalence=~sentiment, max.em.its=75, data=out$meta, init.type="Spectral",seed=1)

# Load required packages if necessary
library(stm)  # or any package containing the extract.estimateEffect function
library(ggplot2)

# Save all the results
for (nk in 4:7) {  
  # Interpret results
  # Get example words
  if (nk == 4) {
    tfit <- tfit4
  }
  if (nk == 5) {
    tfit <- tfit5
  }
  if (nk == 6) {
    tfit <- tfit6
  }
  if (nk == 7) {
    tfit <- tfit7
  }
  
  resultpath <- file.path("~/Desktop/Final Project/Results1", nrow(d), nk)
  print(resultpath)
  dir.create(resultpath, recursive = TRUE, showWarnings = FALSE)
  setwd(resultpath)
  
  # Doc topic matrix
  doctopic <- tfit$theta
  write.matrix(doctopic, file = paste(nk, "doctopic.txt", sep = ""), sep = "\t")
  
  # Words for each topic
  label <- labelTopics(tfit, n = 20)
  capture.output(label, file = paste(nk, "label.txt", sep = ""), append = FALSE)
  
  # Topic proportion
  quartz(width = 10, height = 10)
  plot(tfit, type = "summary", xlim = c(0, .7), n = 8)
  dev.copy2pdf(file = paste(nk, "proportion.pdf", sep = ""))
  dev.off()
  
  plot(tfit, type = "labels")
  dev.copy2pdf(file = paste(nk, "keyword.pdf", sep = ""))
  
  # Get example documents
  thought <- findThoughts(tfit, texts = l$letter, n = 10)
  thought
  plotQuote(thought, width = 30, main = "Topic 3")
  capture.output(thought, file = paste(nk, "document.txt", sep = ""), append = FALSE)
  
  # Estimate effects
  est <- estimateEffect(formula = ~ sentiment, stmobj = tfit, metadata = out$meta, uncertainty = "Global")
  sest <- summary(est)
  capture.output(sest, file = paste(nk, "effects.txt", sep = ""), append = FALSE)
}


















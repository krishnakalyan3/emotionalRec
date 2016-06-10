#install.packages("devtools")
#install_github("bmschmidt/wordVectors")
library(wordVectors)
# My aim here is to enrich the dataset with emotion realted keywords from the
# google news corpus
# https://code.google.com/archive/p/word2vec/
# Link below to download google news corpus
# https://drive.google.com/file/d/0B7XkCwpI5KDYNlNUTTlSS21pQmM/edit
# Google news data sets
# Please download reference the path the load the model
model = read.vectors("GoogleNews-vectors-negative300.bin")

angervec =  nearest_to(model,model[["anger"]])
anticipationvec =  nearest_to(model,model[["anticipation"]])
disgustvec =  nearest_to(model,model[["disgust"]])
fearvec =  nearest_to(model,model[["fear"]])
joyvec =  nearest_to(model,model[["joy"]])
sadnessvec =  nearest_to(model,model[["sadness"]])
surprisevec =  nearest_to(model,model[["surprise"]])
trustvec =  nearest_to(model,model[["trust"]])

## These vectors can be added to the emotional lexicon (Top k word)

library(SPARQL) # SPARQL querying package
library(ggplot2)
library(shiny)
library(shinydashboard)
library(shinycssloaders)

#read csv of categories
main_categories <- read.csv("DATA/main_categories.csv", stringsAsFactors = FALSE,sep=";",header = FALSE)
colnames(main_categories) <- c("type")
sub_categories <- read.csv("DATA/sub_categories.csv", stringsAsFactors = FALSE,sep=";",header = TRUE)

predicates_categories <- read.csv("DATA/predicates_categories.csv", stringsAsFactors = FALSE,sep=";",header = TRUE)
predicates_dictionnary <- read.csv("DATA/predicates_dictionnary.csv", stringsAsFactors = FALSE,sep=";",header = TRUE)

affiche_element <- function(input){
  final_res <- as.data.frame(reactiveValuesToList(input))
  return(final_res)
}



#fonction qui interroge DBpedia
#type A est le type en lien avec les categories (Person, Actvity, Organisation)
query_DBpedia <- function(typeA,typeAprec,namesubject,verb,nbresults){
  #endpoints and prefix to link to DB and get ontologies
  endpoint <- "http://live.dbpedia.org/sparql"
  options <- NULL
  prefix <- c("db","http://dbpedia.org/resource/")
  sparql_prefix <- "PREFIX dbp: <http://dbpedia.org/property/>
  PREFIX dbo: <http://dbpedia.org/ontology/>
  PREFIX dc: <http://purl.org/dc/elements/1.1/>
  PREFIX dct: <http://purl.org/dc/terms/>
  PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
  PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
  "
  
  #query
  
  #beginning of the query
  beg <- 'SELECT distinct *
             WHERE {\n'
  
  #subject
  #first type
  q <- paste('?x a dbo:',typeA,' .\n',sep="")
  #precision about the type
  if (typeAprec!="All"){q <- paste(q,'?x a dbo:',typeAprec,' .\n',sep="")}
  #recuperation du label
  q <- paste(q,'?x rdfs:label ?name .\n',sep="")
  #predicate
  
  if (verb!="no"){
    verbPosition <- which(verb == predicates_dictionnary$subtitle)
  if(predicates_dictionnary$direct[verbPosition]){
  q <- paste(q,'?x ',predicates_dictionnary$original[verbPosition],' ?z .\n',sep="")}
  else
    {q <- paste(q,'?z ',predicates_dictionnary$original[verbPosition],' ?x .\n',sep="")}
  }
  
  #link to wikipedia
  q <- paste(q,'BIND (concat("http://wikipedia.org/wiki/",replace(?name," ","_")) as ?wikilink)\n',sep="")
  #name of the subject
  if (namesubject!="optionnel" & namesubject!=""){
  q <- paste(q,'BIND (CONTAINS(?name,"',namesubject,'") AS ?containsName)\n FILTER (?containsName = 1)\n',sep="")}
  
  all_query <- paste(beg,
            q,
            '}
            LIMIT ',nbresults,
             sep="")
  
  #afficher la requete en ligne de commande
  cat(file=stderr(), "The query is", all_query, "for",typeA,typeAprec,namesubject,verb,"\n")
  
  #query with prefix
  q <- paste(sparql_prefix,all_query)
  
  #send query
  res <- SPARQL(endpoint,q,ns=prefix,extra=options)$results
  #save result as dataframe
  final_res <- as.data.frame(res)
  
  return(final_res)
}




library(SPARQL) # SPARQL querying package
library(ggplot2)
library(shiny)
library(shinydashboard)
library(shinycssloaders)


#reading csv 
#categories of the subject
main_categories <- read.csv("DATA/main_categories.csv", stringsAsFactors = FALSE,sep=";",header = FALSE)
colnames(main_categories) <- c("type")
sub_categories <- read.csv("DATA/sub_categories.csv", stringsAsFactors = FALSE,sep=";",header = TRUE)
#predicates for all the categories
predicates_categories <- read.csv("DATA/predicates_categories.csv", stringsAsFactors = FALSE,sep=";",header = TRUE)
predicates_dictionnary <- read.csv("DATA/predicates_dictionnary.csv", stringsAsFactors = FALSE,sep=";",header = TRUE)


#querying DBpedia with the parameters given in the interface
query_DBpedia <- function(typeA,typeAprec,placesubject,namesubject,exactsubject,verb,nameobject,placeobject,exactobject,mindate,maxdate,nbresults){

  cat(file=stderr(), "ICI", mindate,"\n")
    
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
  q <- paste(q,'BIND(STR(?name) as ?Subject) .\n',sep="")
  #name of the subject
  if (namesubject!="optionnal" & namesubject!=""){
    if (exactsubject){
    q <- paste(q,'FILTER(?Subject = STR("',namesubject,'"))\n',sep="")  
    }else{
    q <- paste(q,'FILTER(CONTAINS(?name,"',namesubject,'")) .\n',sep="")}
  }
  
  #in case of subject is a place : add the coordinates
  q <- paste(q,'OPTIONAL{?x georss:point ?coordinates} .\n',sep="")
  
  #predicate
  if (verb!="no"){
    verbPosition <- which(verb == predicates_dictionnary$subtitle)
  if(predicates_dictionnary$direct[verbPosition]){
  q <- paste(q,'?x ',predicates_dictionnary$original[verbPosition],' ?z .\n',sep="")}
  else
    {q <- paste(q,'?z ',predicates_dictionnary$original[verbPosition],' ?x .\n',sep="")}
  
  
    #object, exists if predicate != "no"
    #if object is an uri it takes the label, otherwise it take the object himself (ie if it is a literal)
    q <- paste(q,'OPTIONAL{ ?z rdfs:label ?nameobjectURI .} \n',sep="")
    q <- paste(q,'BIND (COALESCE(STR(?nameobjectURI),concat(?z," ")) as ?Object) .\n',sep="")
    
    #add the coordinates if the object is a place
    #for the URI objects
    q <- paste(q,'OPTIONAL{?z georss:point ?place} .\n',sep="")
    #for the non URI objects
    q <- paste(q,'OPTIONAL{?px a dbo:Place .\n ?px rdfs:label ?z .\n ?px georss:point ?place} .\n',sep="")
    
    
    #filter on the name given by the user, if he gives one
    if (nameobject!="optionnal" & nameobject!=""){
      if (exactobject){
        q <- paste(q,'FILTER(?Object = STR("',nameobject,'"))\n',sep="")  
      }else{
      q <- paste(q,'FILTER(CONTAINS(?Object,"',nameobject,'")) . \n',sep="")}
    }
    
    #if object exists
    #if the object is the date (mindate and maxdate !=NA)
    #year
    if (grepl("date",verb)){  
      q <- paste(q,"BIND(strdt(?Object,xsd:date) AS ?date).\n",sep="")
      if (is.na(mindate)){}else{q <- paste(q,'FILTER(?date >= "',mindate,'-01-01"^^xsd:date).\n',sep="")}
      if (is.na(maxdate)){}else{q <- paste(q,'FILTER(?date <= "',maxdate,'-12-31"^^xsd:date).\n',sep="")}
    }
    
    
  }
  
  
  #add precision about the type of place if it is the case
  if (placeobject=="City"||placeobject=="Country"){
    q <- paste(q,'?z a dbo:',placeobject,' .\n',sep="")
  }
  
 
  #link to wikipedia
  q <- paste(q,'BIND (concat("http://wikipedia.org/wiki/",replace(?name," ","_")) as ?wikilink) .\n',sep="")


  all_query <- paste(beg,
            q,
            '}
            LIMIT ',nbresults,
             sep="")
  
  #print the query in the console
  cat(file=stderr(), "The query is", all_query, "for",typeA,typeAprec,namesubject,verb,"\n")
  
  #query with prefix
  q <- paste(sparql_prefix,all_query)
  
  #send query
  res <- SPARQL(endpoint,q,ns=prefix,extra=options)$results
  #save result as dataframe
  final_res <- as.data.frame(res)
  
  
  #setting to NULL useless columns
  final_res$x <- NULL
  final_res$name <- NULL
  if("z" %in% colnames(final_res)){final_res$z <- NULL}
  if("nameobjectURI" %in% colnames(final_res)){final_res$nameobjectURI <- NULL}
  if("px" %in% colnames(final_res)){final_res$px <- NULL}
  if("date" %in% colnames(final_res)){final_res$date <- NULL}
  
  #deleting empty columns
  final_res <- Filter(function(x)!all(is.na(x)), final_res)
  

  
  return(final_res)
}




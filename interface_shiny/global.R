library(SPARQL) # SPARQL querying package
#package to create the interface
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
query_DBpedia <- function(typeA,typeAprec,placesubject,namesubject,exactsubject,verb,verb2,
                          nameobject,exactobject,placeobject,nameobject2,exactobject2,placeobject2,
                          mindate,maxdate,mindate2,maxdate2,nbresults){
    
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
  q <- paste('?x a dbo:',typeA,' .\n',sep="")
  #precision about the type, if there is one
  if (typeAprec!="All"){q <- paste(q,'?x a dbo:',typeAprec,' .\n',sep="")}
  #getting the label of the subject, the most readable name of it
  q <- paste(q,'?x rdfs:label ?name .\n',sep="")
  #changing it from literal to string to avoid "" and languages tags
  q <- paste(q,'BIND(STR(?name) as ?Subject) .\n',sep="")
  
  #name of the subject, given by the user
  if (namesubject!="optionnal" & namesubject!=""){
    #search for whether an exact match on the name, or all those who contains the word given
    if (exactsubject){
    q <- paste(q,'FILTER(?Subject = STR("',namesubject,'"))\n',sep="")  
    }else{
    q <- paste(q,'FILTER(CONTAINS(?name,"',namesubject,'")) .\n',sep="")}
  }
  
  #if the subject is a place : add the coordinates
  q <- paste(q,'OPTIONAL{?x georss:point ?coordinates} .\n',sep="")
  
  #PREDICATE 1
  #if one predicate is chosen
  if (verb!="no"){
    #get the line of the predicate in the dictionary
    verbPosition <- which(verb == predicates_dictionnary$subtitle)
  #predicate can be direct or not
    #the predicate city is direct for person (s:person p:city o:place) and indirect for place, as it is the city of someone
  if(predicates_dictionnary$direct[verbPosition]){
  q <- paste(q,'?x ',predicates_dictionnary$original[verbPosition],' ?z .\n',sep="")}
  else
    {q <- paste(q,'?z ',predicates_dictionnary$original[verbPosition],' ?x .\n',sep="")}
  
  
    #OBJECT, exists if there is a predicate ie predicate != "no"
    #if object is an uri it takes the label, otherwise it takes the object himself (ie if it is a literal)
    q <- paste(q,'OPTIONAL{ ?z rdfs:label ?nameobjectURI .} \n',sep="")
    q <- paste(q,'BIND (COALESCE(STR(?nameobjectURI),str(?z)) as ?Object) .\n',sep="")
  
    
    #add the coordinates if the object is a place
    #CHANGE POSSIBLE HERE if take only column place ==TRUE
    #for the URI objects
    q <- paste(q,'OPTIONAL{?z georss:point ?place} .\n',sep="")
    #for the non URI objects
    q <- paste(q,'OPTIONAL{?px a dbo:Place .\n ?px rdfs:label ?z .\n ?px georss:point ?place} .\n',sep="")
    
    
    #filter on the name given by the user, if he gives one
    if (nameobject!="optionnal" & nameobject!=""){
      #search for whether an exact match on the name, or all those who contains the word given
      if (exactobject){
        q <- paste(q,'FILTER(?Object = STR("',nameobject,'"))\n',sep="")  
      }else{
      q <- paste(q,'FILTER(CONTAINS(?Object,"',nameobject,'")) . \n',sep="")}
    }
    
    #if the object is a date (mindate and maxdate !=NA) ie if the subtitle predicate reference to the word "date"
    if (grepl("date",verb)){  
      q <- paste(q,"BIND(strdt(?Object,xsd:date) AS ?date).\n",sep="")
      #user can give whether a minimum, wether a maximum, whether both of them
      if (is.na(mindate)){}else{q <- paste(q,'FILTER(?date >= "',mindate,'-01-01"^^xsd:date).\n',sep="")}
      if (is.na(maxdate)){}else{q <- paste(q,'FILTER(?date <= "',maxdate,'-12-31"^^xsd:date).\n',sep="")}
    }
    
    #if the object is a place, add precision about the type of place
    if (placeobject=="City"||placeobject=="Country"){
      q <- paste(q,'?z a dbo:',placeobject,' .\n',sep="")
    }
  }
  
  #PREDICATE 2
  #if the user chosed to add a second predicate
  if ( length(verb2)!=0){
  #if this predicate has a value
  if (verb2!="no"){
    #get the line of the predicate in the dictionary
    verbPosition2 <- which(verb2 == predicates_dictionnary$subtitle)
    #predicate can be direct or not
    if(predicates_dictionnary$direct[verbPosition2]){
      q <- paste(q,'?x ',predicates_dictionnary$original[verbPosition2],' ?zbis .\n',sep="")}
    else
    {q <- paste(q,'?zbis ',predicates_dictionnary$original[verbPosition2],' ?x .\n',sep="")}
    
    #OBJECT exists if predicates does, ie  predicate != "no"
    #if object is an uri it takes the label, otherwise it takes the object himself (ie if it is a literal)
    q <- paste(q,'OPTIONAL{ ?zbis rdfs:label ?nameobjectURIbis .} \n',sep="")
    q <- paste(q,'BIND (COALESCE(STR(?nameobjectURIbis),str(?zbis)) as ?Object2) .\n',sep="")
    
    #add the coordinates if the object is a place
    #for the URI objects
    q <- paste(q,'OPTIONAL{?zbis georss:point ?place2} .\n',sep="")
    #for the non URI objects (needs to search for the URI)
    q <- paste(q,'OPTIONAL{?pxbis a dbo:Place .\n ?pxbis rdfs:label ?zbis .\n ?pxbis georss:point ?place2} .\n',sep="")

    #filter on the name given by the user, if he gives one
    if (nameobject2!="optionnal" & nameobject2!=""){
      #exact match of the word or not
      if (exactobject2){
        q <- paste(q,'FILTER(?Object2 = STR("',nameobject2,'"))\n',sep="")  
      }else{
        q <- paste(q,'FILTER(CONTAINS(?Object2,"',nameobject2,'")) . \n',sep="")}
    }
    
    #if the object2 is a date (mindate and maxdate !=NA)
    if (grepl("date",verb2)){  
      q <- paste(q,"BIND(strdt(?Object2,xsd:date) AS ?datebis).\n",sep="")
      #min, max, both of them or neither of them
      if (is.na(mindate2)){}else{q <- paste(q,'FILTER(?datebis >= "',mindate2,'-01-01"^^xsd:date).\n',sep="")}
      if (is.na(maxdate2)){}else{q <- paste(q,'FILTER(?datebis <= "',maxdate2,'-12-31"^^xsd:date).\n',sep="")}
    }
    
    #if object2 is a place, add precision about the type of place
    if (placeobject2=="City"||placeobject2=="Country"){
      q <- paste(q,'?zbis a dbo:',placeobject2,' .\n',sep="")
    }

  }}
  
 
  #link to wikipedia
  q <- paste(q,'BIND (concat("http://wikipedia.org/wiki/",replace(?name," ","_")) as ?wikilink) .\n',sep="")


  #add end of the query
  all_query <- paste(beg,
            q,
            '}
            LIMIT ',nbresults,
             sep="")
  
  #print the query in the console
  cat(file=stderr(), "The query is", all_query, "for",typeA,typeAprec,namesubject,verb,"\n")
  
  #query with prefix
  q <- paste(sparql_prefix,all_query)
  
  #send query to endpoint
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
  if("zbis" %in% colnames(final_res)){final_res$zbis <- NULL}
  if("nameobjectURIbis" %in% colnames(final_res)){final_res$nameobjectURIbis <- NULL}
  if("pxbis" %in% colnames(final_res)){final_res$pxbis <- NULL}
  if("datebis" %in% colnames(final_res)){final_res$datebis <- NULL}
  #deleting empty columns
  final_res <- Filter(function(x)!all(is.na(x)), final_res)
  

  
  return(final_res)
}




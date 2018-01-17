library(SPARQL) # SPARQL querying package
library(ggplot2)

#read csv of categories
main_categories <- read.csv("DATA/main_categories.csv", stringsAsFactors = FALSE,sep=";",header = FALSE)
colnames(main_categories) <- c("type")
sub_categories <- read.csv("DATA/sub_categories.csv", stringsAsFactors = FALSE,sep=";",header = TRUE)

#test de connexion a DBpedia
test <- query_DBpedia("Person",200)

#fonction qui interroge DBpedia
#type A est le type en lien avec les categories (Person, Actvity, Organisation)
query_DBpedia <- function(typeA,nbresults){
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
  a <- paste('SELECT ?name
             WHERE {
             ?x a dbo:',typeA,' .
             ?x rdfs:label ?name .
             }
             LIMIT',' ',nbresults,sep="")
  
  #query with prefix
  q <- paste(sparql_prefix,a)
  
  #send query
  res <- SPARQL(endpoint,q,ns=prefix,extra=options)$results
  #save result as dataframe
  final_res <- as.data.frame(t(res))
  
  return(final_res)
}

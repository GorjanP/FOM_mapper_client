# Instructions on\ getting an API key are at http://www.bioontology.org/wiki/index.php/Annotator_User_Guide#Annotator_Web_service_Validation
# Login to BioPortal to get YourAPIKey
yourAPIKey <- "6c62f9cf-d7ec-4715-b8ab-25513966cf14"

library(stringr)
library(RCurl)
library(XML)
library(jsonlite)
library(xlsx)

#-------------START utility functions-------------#
#do something about brackets here
fixFractions <- function(s)
{
  ret <- s
  
  raw <- unlist(str_match_all(string = s, pattern = "[1-9] [1-9]/[1-9]"))
  if(length(raw) == 0)
  {
    return(ret)  
  }
  fractions <- c()
  wholes <- c()
  
  for(s in raw)
  {
    s1 <- str_split(s, pattern = " ")
    fractions <- append(fractions, s1[[1]][2])
    wholes <- append(wholes, as.numeric(s1[[1]][1]))
  }
  
  fractions <- as.vector(sapply(fractions, function(x) eval(parse(text=x))))
  #print(class(fractions))
  #print(class(wholes))
  
  
  for(i in 1:length(raw))
  {
    #print("------")
    #print(wholes[i])
    #print(fractions[i])
    num <- wholes[i] + fractions[i] 
    t_rep <- toString(num)
    ret = gsub(x = ret, pattern = raw[i], replacement = t_rep)
  }
  
  return(ret)
  
}

fixFractions_small <- function(s)
{
  ret <- s
  
  raw <- unlist(str_match_all(string = s, pattern = "[1-9]/[1-9]"))
  if(length(raw) == 0)
  {
    return(ret)  
  }
  fractions <- c()
  
  for(s in raw)
  {
    fractions <- append(fractions, s)
  }
  
  fractions <- as.vector(sapply(fractions, function(x) eval(parse(text=x))))
  #print(class(fractions))
  #print(class(wholes))
  
  
  for(i in 1:length(raw))
  {
    #print("------")
    #print(wholes[i])
    #print(fractions[i])
    num <- fractions[i] 
    t_rep <- toString(num)
    ret = gsub(x = ret, pattern = raw[i], replacement = t_rep)
  }
  
  return(ret)
  
}

#-------------END utility functions-------------#
    
 annotate_text<-function(queryText, yourAPIKey, onto=NULL){   
   
   if(is.null(onto))
 {
     print("Please choose one of the followinG:\n \"FOODON\",\n\"OF\",\n\"SNOMEDCT\",\n\"MESH\",\n\"OCHV\",\n\"RCD\".")
     return(NULL)
   }
   
   writeLines(onto)
   res <- NULL
   if(onto %in% c("FOODON", "OF"))
   {
     res <- postForm('http://data.bioontology.org/annotator',
                    "text"=queryText, 
                    "apikey"=yourAPIKey,
                    "ontologies"=onto,
                    "expand_semantic_types_hierarchy"="false",
                    "expand_class_hierarchy"="false",
                    "class_hierarchy_max_level"="0",
                    "expand_mappings"="false")
     
   }
   else if(onto %in% c('CRISP', 'LOINC', 'MEDLINEPLUS', 'MESH', 'NDDF', 'NDFRT', 'PDQ', 'RCD', 'RXNORM', 'SNMI', 'SNOMEDCT', 'VANDF'))
   {
     res <- postForm('http://data.bioontology.org/annotator',
                     "text"=queryText, 
                     "apikey"=yourAPIKey,
                     "ontologies"=onto, 
                     "semantic_types"="T168",
                     "expand_semantic_types_hierarchy"="false",
                     "expand_class_hierarchy"="false",
                     "class_hierarchy_max_level"="0",
                     "expand_mappings"="false")
     
   }

  return(res)
}

#res is a JSON file that is returned by the NCBO annotator
matchedConcepts<-function(res){
	
	x<-fromJSON(res)
	if (length(x) <= 0)
	{
	  return(NULL)
	}
	
	urls <- c()
	text<-c()
	from<-c()
	to<-c()
	matchType<-c()
	ctr<-1
	ui<-x$annotatedClass[1:nrow(x), 1]
	
	for(i in 1:nrow(x))
	{
	  #print(nrow(x$annotations[[i]]))
	  #print(x$annotations[[i]][['text']][1])
	  
	  for(j in 1:nrow(x$annotations[[i]]))
	  {
	    
	    #print(x$annotations[[i]][["text"]][[j]])
	    urls[ctr]<-ui[i]
	    text[ctr]<-x$annotations[[i]][["text"]][j]
	    from[ctr]<-x$annotations[[i]][["from"]][j]
	    to[ctr]<-x$annotations[[i]][["to"]][j]
	    matchType[ctr]<-x$annotations[[i]][["matchType"]][j]
	    ctr <- ctr + 1
    }
	}
	text<- unlist(text, use.names=FALSE)
	from<- unlist(from, use.names=FALSE)
	to<- unlist(to, use.names=FALSE)
	matchType<- unlist(matchType, use.names=FALSE)

	df<-data.frame(urls,text,from,to,matchType)
	 
	return(df)
}

check_ontology <- function(queryText = "I like banana bread.", api_key ="6c62f9cf-d7ec-4715-b8ab-25513966cf14", onto = NULL)
{
  res <- annotate_text(queryText, api_key, onto)
  #print(length(res))
  if(length(res) <= 0)
  {
    return(NULL)
  }
  concepts <- matchedConcepts(res)
  return(concepts)
}

annotate_recipes <- function(num=1000000, ontos = NULL, curated = NULL)
{
  csv_root <- NULL
  
  if(is.null(curated))
  {
    csv_root <- "csv_test"
    xls_root <- "xls_test"
    
  }
  else if(curated)
  {
    csv_root <- "csv"
    xls_root <- "xls"
  }
  else if(!curated)
  {
    csv_root <- "csv_uncur"
    xls_root <- "xls_uncur"
    
  }
  
  full_files = list.files("recipes/", full.names = TRUE)
  files = list.files("recipes/", full.names = FALSE)
  
  ctr = 1
  
  for(f in full_files)
  {

    writeLines(files[ctr])
    s <<- readChar(f, file.info(f)$size)
    
    # RECIPE FILES ARE ALREADY PREPROCESSED!!
    #s <<- gsub("[\r\n\t\f\v ][\r\n\t\f\v ]+", " ", s)
    #s <<- gsub(x = s, pattern = "\"", replacement = "")
    #s <<- gsub(x = s, pattern = "°", replacement = " degrees ")
    #s <<- iconv(s, to='ASCII//TRANSLIT')
    #s <<- fixFractions_small(fixFractions(s))
    
    for(onto in ontos)
    {
    
      concepts <- check_ontology(queryText = s, onto = onto)
      
      if(!is.null(concepts))
      {
        #print(concepts)
        file_name_xls <- paste(substring(files[ctr], 1, nchar(files[ctr])-4), '_', onto, '.xls', sep = '')
        file_path_xls <- paste("./NCBO_annotations/", xls_root, "/",onto, '/', file_name_xls,sep = '')
        
        file_name_csv <- paste(substring(files[ctr], 1, nchar(files[ctr])-4), '_', onto, '.csv', sep = '')
        file_path_csv <- paste("./NCBO_annotations/", csv_root, "/", onto, '/', file_name_csv,sep = '')
      
        #write to separate xlsx file here  
        write.xlsx(concepts, file_path_xls, sheetName = "Matched Concepts", col.names= TRUE, row.names = TRUE, append=FALSE)
        write.csv(concepts, file = file_path_csv, col.names = TRUE)
        writeLines(paste("Written to file", file_name_xls, "\n", sep=' '))
      }
    }
    ctr = ctr + 1
    invisible(do.call(file.remove, as.list(f)))  
  }
  
}

bla <- function()
{
  # csv_root can be 
  # annotate_recipes(onto = c("SNOMEDCT", "OF", "FOODON", "MESH", "RCD", "NDDF", "SNMI"))#, curated = FALSE)
  # annotate_recipes(onto = "CRISP")
  #annotate_recipes(onto = "STY")
  # ('AI-RHEUM', 'ATC', 'COSTART', 'CRISP', 'HCPCS', 'HL7', 'ICD10', 'ICD10CM', 'ICD10PCS', 'ICPC2P', 'IOBC', 'LOINC', 'MDDB', 'MEDDRA', 'MEDLINEPLUS', 'MESH', 'MSTDE', 'NCBITAXON', 'NDDF', 'NDFRT', 'OCHV', 'OF', 'OMIM', 'PDQ', 'RCD', 'RXNORM', 'SNMI', 'SNOMEDCT', 'STY', 'VANDF', 'WHO-ART')
#  annotate_recipes(onto = c('AI-RHEUM', 'ATC', 'COSTART', 'CRISP', 'HCPCS', 'HL7', 'ICD10', 'ICD10CM', 'ICPC2P', 'LOINC', 'MDDB', 'MEDDRA', 'MEDLINEPLUS', 'MSTDE', 'names.txt', 'NDFRT', 'OMIM', 'PDQ', 'RXNORM', 'VANDF', 'WHO-ART'))
  annotate_recipes(onto="NDFRT", curated = TRUE)
  
  
}
#queryText <- "Baking banana bread is one of my favorites, and I love nothing more than enjoying a slice with a nice cup of coffee. This was the inspiration for my recipe, which features a coffee infused loaf and a rich caramel glaze."		


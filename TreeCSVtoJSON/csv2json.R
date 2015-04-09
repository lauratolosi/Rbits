# Input : the tab separated CSV file, specified by its location on disk
# Output : the file to which the JSON object will be written, specified as a location on disk
# Important: the first row contains the root of the tree 

csv.to.json <- function(filein, fileout) { 
  require(stringr) 
  input <- read.csv(file = filein, sep = "\t", header = TRUE)
  json <- paste('{id: "', input$ID[1], '", name: "',input$Node[1],'", children: []}')
  for(i in 1:input$ParentID[nrow(input)]) {
    # locate all the children
    ind = which(input$ParentID == i)
    if(length(ind)>0){
      names = as.character(input$Node[ind])
      indices = input$ID[ind]
      parent.name = as.character(input$Node[i])
      idf = paste('id: "',i,'", name: "', parent.name, '", children: ', sep = "")
      end = str_locate(json, fixed(idf))[2]  # position to start inserting children
      # children
      children = paste(paste('{id: "',indices,'", name: "',names, '", children: []}', sep = ""), collapse = ", ")
      children <- paste("[", children, sep = "")
      # insert in json
      str_sub(json, end+1, end+1) <- children
    }
  }
  json1 <- gsub("children: [", "children: [\n", json, fixed = TRUE)
  write(json1, file = fileout)
}

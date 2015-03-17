#'@title Convert Google Doc HTML output to markdown
#'
#'@description Converts the HTML exported by from a Google document (retrieved
#'by \code{download_file(..., download_type='text/html')} to a markdown file,
#'pre-processing to deal with Google Doc's particular formatting.
#'
#'@param input_html an HTML file retrieved via 
#'download_file(..., download_type='text/html')
#'
#'@param output_file a filename for markdown output (optional)
#'
#'@param ... options (as a character string) to be passed to pandoc via
#'\code{\link[rmarkdown]{pandoc_convert}}
#'
#'@return If no \code{output_file} is specified, the markdown output is
#'returned as a character. If \code{output_file} is specified, the markdown is
#'written to this file and also returned invisibly.
#'
#'@seealso \code{\link{download_file}}, \code{\link[rmarkdown]{pandoc_convert}}
#'
#' @export
#' @importFrom rmarkdown pandoc_convert
gdoc_to_md <- function(input_html, output_file = NULL, ...){
  tmp_html <- tempfile()
  clean_google_doc_html(input_html, output_file = tmp_html)
  
  if(is.null(output_file)){
    outfile <- tempfile()
  } else {
    outfile <- output_file
  }
  
  pandoc_convert(input = tmp_html, from = "html", 
                 to = "markdown-raw_html-header_attributes", 
                 output = outfile, wd = getwd(), ...)
  
  if(is.null(output_file)){
    return(readChar(outfile, file.info(outfile)$size))
  } else {
    invisible(readChar(outfile, file.info(outfile)$size))
  }
}

#' @import XML
#' @importFrom stringi stri_extract_first_regex
clean_google_doc_html <- function(input_file, output_file=NULL){
  
  doc <- htmlParse(input_file)
  
  #Strip google redirects out of the links
  linknodes <- getNodeSet(doc, "//a[contains(@href, 'http://www.google.com/url?q=')]")
  for(node in linknodes){
    link <- xmlAttrs(node)["href"]
    cleanlink <- stri_extract_first_regex(URLdecode(link), "(?<=http://www.google.com/url\\?q=)http://.+(?=&sa)")
    removeAttributes(node, .attrs = "href")
    addAttributes(node, href = cleanlink)
  }
  
  # Find the classes that represent bold and italic, fix these nodes.
  styles = xpathSApply(doc, '//head/style', xmlValue)
  bold_class = stri_extract_first_regex(styles, "(?<=\\.)c\\d+(?=\\{font-weight:bold\\})")
  italic_class = stri_extract_first_regex(styles, "(?<=\\.)c\\d+(?=\\{font-style:italic\\})")
  
  boldnodes <- getNodeSet(doc, paste0("//span[contains(@class,'", bold_class, "') and not(contains(@class, '", italic_class, "'))]"))
  for(node in boldnodes){
    text <- xmlValue(node)
    strongnode <- newXMLNode("strong")
    newnode <- newXMLTextNode(text, parent=strongnode)
    removeChildren(node, names(node))
    addChildren(node, strongnode)
  }
  
  italicnodes <- getNodeSet(doc, paste0("//span[contains(@class,'", italic_class, "') and not(contains(@class, '", bold_class, "'))]"))
  for(node in italicnodes){
    text <- xmlValue(node)
    emnode <- newXMLNode("em")
    newnode <- newXMLTextNode(text, parent = emnode)
    removeChildren(node, names(node))
    addChildren(node, emnode)
  }
  
  bolditalicnodes = getNodeSet(doc, paste0("//span[contains(@class,'", italic_class, "') and contains(@class, '", bold_class, "')]"))
  for(node in bolditalicnodes){
    text <- xmlValue(node)
    strongnode <- newXMLNode("strong")
    emnode <- newXMLNode("em", parent=strongnode)
    newnode <- newXMLTextNode(text, parent=emnode)
    removeChildren(node, names(node))
    addChildren(node, strongnode)
  }
  
  if(is.null(output_file)){
    return(paste(capture.output(print(doc)), collapse = "\n"))
  } else {
    saveXML(doc, file = output_file)
    invisible(paste(capture.output(print(doc)), collapse = "\n"))
  }
}


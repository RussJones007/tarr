# rd_2_html.r
# convert rd files to html and combine


# install.packages("xml2")  # if needed
# install.packages("xml2")  # if needed
library(xml2)

combine_html_files <- function(files, out = "combined.html", add_titles = TRUE) {
  stopifnot(length(files) > 0)
  
  # Create a fresh target document
  new_doc  <- read_html("<!DOCTYPE html><html><head></head><body></body></html>")
  new_head <- xml_find_first(new_doc, "//head")
  new_body <- xml_find_first(new_doc, "//body")
  
  # Copy the <head> from the first file (deep-copy nodes with .copy = TRUE)
  first_doc  <- read_html(files[1])
  first_head <- xml_find_first(first_doc, "//head")
  if (!inherits(first_head, "xml_missing")) {
    for (node in xml_children(first_head)) {
      xml_add_child(new_head, node, .copy = TRUE)
    }
  }
  
  # Optional: simple styling to separate merged parts
  xml_add_child(
    new_head, "style",
    "section.merged-part{margin:2rem 0;padding-top:.5rem;border-top:1px solid #ddd}
     section.merged-part>h1{font-size:1.6rem;margin:0 0 1rem 0}"
  )
  
  # Append each file's <body> into its own <section>
  for (f in files) {
    doc <- read_html(f)
    b   <- xml_find_first(doc, "//body")
    
    # Wrapper section with id and class
    section <- xml_add_child(
      new_body, "section",
      id = tools::file_path_sans_ext(basename(f)),
      `class` = "merged-part"
    )
    
    # Optional title from <title> (fallback to filename)
    if (add_titles) {
      ttl_node <- xml_find_first(doc, "//title")
      ttl <- if (!inherits(ttl_node, "xml_missing")) xml_text(ttl_node) else basename(f)
      xml_add_child(section, "h1", ttl)
    }
    
    # Copy each child of the source <body> into the section
    if (!inherits(b, "xml_missing")) {
      for (child in xml_children(b)) {
        xml_add_child(section, child, .copy = TRUE)
      }
    }
  }
  
  write_html(new_doc, file = out, options = "format")
  message("Wrote: ", normalizePath(out))
}

combine_html_files(html_name(help_html), out = "combined_doc.html")

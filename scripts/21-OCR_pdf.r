# 20-OCR_pdf.r -----------------------------------------------------------------
# Used to read in pdf file images and run optical character recognition.
# Possible use in reading fax files.  For standardized test result presentation
# in the file it is possible to detect positive and negative results.
#
# Used for massive reporting by MD Group, so much of the code is targeting the faxes
# received from that source.
#
# October 3, 2020
# R Jones
# ------------------------------------------------------------------------------
# Source the support functions
file.path(here(),"Scripts/22-OCR_support_functions.r") %>% 
  source

rm(poss_pos, full.names, limit_control, pdf_files, total_time, pdf_path_raw)

# Processed the pdf files  -----------------------------------------------------
pdf_path_raw <- "C:\\Users\\RWJones\\OneDrive - Tarrant County\\COVID-19\\RightFax"
pdf_files <- list.files(path    = pdf_path_raw,
                        pattern = "\\d{5}[a-zA-Z0-9]+\\.(PDF|pdf)$",
                        full.names = T) 

#limit_control <- 0  # reset counter
process_files <- find_pos()
total_time <- system.time(
poss_pos <- map(pdf_files, ~process_files(.)) %>% 
  setNames(basename(pdf_files))
)
rm(process_files)

#pdf_files[13] %>%  basename() %>% write_clip()
#pdf_files[73] %>% basename() %>% write_clip()
duration(num = total_time["elapsed"], units = "sec")
total_time/length(pdf_files)



# move the non_md_group files to a different folder
non_md_group <- poss_pos[!map_lgl(poss_pos, ~.x$group)]
#non_md_group[[2]]$text %>% write_clip()
if(!is_empty(non_md_group)){
  non_md_files <- pdf_files[!map_lgl(poss_pos, ~.x$group)]
  new_non_md_file_names <- file.path(dirname(non_md_files),"Check",basename(non_md_files))
  file.rename(from = non_md_files, to = new_non_md_file_names)
  rm(non_md_files, new_non_md_file_names)
}
rm(non_md_group)

# since this is targeting the MD family group, move only those files that look to be from that group
md_group     <- pdf_files[map_lgl(poss_pos, ~.x$group)]
md_poss_pos  <- poss_pos[map_lgl(poss_pos, ~.x$group)]

case_count <- 0

recs <- map_df(md_poss_pos, ~extract_record(.x)) %>% 
  mutate(
    old_file_name         = orig_file, 
    first_name            = name %>% 
      str_extract(pattern = " [:alpha:]+$") %>% 
      str_trim() %>% 
      str_to_title(),
    last_name             = name %>% 
      str_extract(pattern = "^.*?(?=,)") %>% 
      str_extract(pattern = "[:alpha:]+$") %>% 
      str_trim() %>% 
      str_to_title(),
    new_file_name = file.path(
                              paste0(
                                paste(first_name, last_name, sep = "_"),
                                case_when (
                                  grepl("Pos*", lab_result, ignore.case = T) ~ "_Pos",
                                  file_pos                                   ~ "_10day",
                                  grepl("Neg*", lab_result, ignore.case = T) | !file_pos ~ "_Neg",
                                  TRUE ~ "_Unk"
                              ),
                              ".pdf")
                              ),
    new_folder = case_when(
      str_detect(new_file_name, "(_Pos|_10day)")  ~ file.path(pdf_path_raw, "Pos", new_file_name),
      str_detect(new_file_name, "_Neg")  ~ file.path(pdf_path_raw, "Neg", new_file_name)
        )
    )


recs$lab_result %>% table()
recs$file_pos %>% table()

#recs$new_folder

file.rename(from <- (file.path(pdf_path_raw, recs$old_file_name)),
            to   <- recs$new_folder)

rm(md_group, md_poss_pos, pdf_files, poss_pos, time_per_file, total_time)
rm(recs)


# Read Faxed Line Lists --------------------------------------------------------
pdf_path_raw <- "C:\\Users\\RWJones\\OneDrive - Tarrant County\\COVID-19\\RightFax"
pdf_files <- list.files(pdf_path_raw,pattern = "MD_MEDICAL_GROUP[:graph:]*", full.names = T)

pdf1 <- pdf_files[1]
pdf_info(pdf1)$pages
pdf1.pages <- pdf_pagesize(pdf1)
page1 <- image_read_pdf(path = pdf1, pages = 2, density = 72*3) %>% 
  image_rotate(degrees = 90)
page1
image_info(page1)

geo <- geometry_area(width = 2176, height = 1550, x_off = 98, y_off = 153)
image2 <- image_crop(image = page1, geometry = geo, repage = T) %>% 
  image_trim()
image2
page.text <- ocr(image2)
page.text

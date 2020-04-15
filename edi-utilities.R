#### Source Code for Data Publishing ####

# Create Template .txt files for EMLassembly ----------
# packages required: readxl

# read in metadata info and import additional templates
# write info data frames to text files for EML assembly

xlsx_to_template <- function(metadata.path, output.path, edi.filename, rights, bbox = FALSE, other.info = FALSE) {
  # define the file type for the metadata
  metadata_xlsx <- paste0(metadata.path, ".xlsx")
  
  if ("ColumnHeaders" %in% excel_sheets(path = metadata_xlsx)) {
    headers <- read_excel(path = metadata_xlsx, 
                          sheet = "ColumnHeaders", na = "NA")
    write.table(headers, paste0(output.path, "attributes_", edi.filename, ".txt"), 
                quote = FALSE, na = "", sep = "\t", row.names = FALSE)
  }
  if ("Personnel" %in% excel_sheets(path = metadata_xlsx)) {
    personnel <- read_excel(path = metadata_xlsx, 
                            sheet = "Personnel", na = "NA")
    write.table(personnel, paste0(output.path, "personnel.txt"), 
            quote = FALSE, na = "", sep = "\t", row.names = FALSE)
  }
  if ("Keywords" %in% excel_sheets(path = metadata_xlsx)) {
    keywords <- read_excel(path = metadata_xlsx, 
                          sheet = "Keywords", na = "NA")
    write.table(keywords, paste0(output.path, "keywords.txt"), 
                quote = FALSE, na = "", sep = "\t", row.names = FALSE)
  }
  if ("CategoricalVariables" %in% excel_sheets(path = metadata_xlsx)) {
    catvars <- read_excel(path = metadata_xlsx, 
                          sheet = "CategoricalVariables", na = "NA")
    write.table(catvars, paste0(output.path, "catvars_", edi.filename, ".txt"), 
                quote = FALSE, na = "", sep = "\t", row.names = FALSE)
  }
  if ("CustomUnits" %in% excel_sheets(path = metadata_xlsx)) {
    custom_units <- read_excel(path = metadata_xlsx,
                               sheet = "CustomUnits", na = "NA")
    write.table(custom_units, paste0(output.path, "custom_units.txt"), 
                quote = FALSE, na = "", sep = "\t", row.names = FALSE)
  }
  # Import abstract and methods
  template_core_metadata(path = output.path, license = rights)
  # this will not overwrite existing files

  # if theres is no additional information (default), eliminate the template
  if(isFALSE(other.info)) {
    unlink("additional_info.txt")
  }
}

## Example use:
# define path to excel file containing metadata
# growgraze_info <- "NES-LTER_Growth-Grazing_SMD_2019_11_01"
# define output filename (must be the same as the final csv)
# growgraze_file <- "nes-lter-growth-grazing-chl"
# run function
# xlsx_to_template(metadata.path = growgraze_info, edi.filename = growgraze_file, rights = "CCBY")

# Define Coverage for make_eml ----------

# date, lat, and lon columns must be identified as input for this function
# Compiles a list of geographic and temporal coverage
data_coverage <- function(dates, lat, lon) {
  # Temporal coverage 
  # Will need this in make_eml YYYY-MM-DD
  startdate <- min(dates, na.rm = TRUE)
  enddate <- max(dates, na.rm = TRUE)
  # temporal.coverage argument is expecting objects of 'character' class, not 'Date'
  startdate_as_character <- as.character(startdate)
  enddate_as_character <- as.character(enddate)

  # Geographic coverage
  # Will need this order in make_eml: North, East, South, West
  North <- round(max(lat, na.rm = TRUE), 5)
  East <- round(max(lon, na.rm = TRUE), 5)
  South <- round(min(lat, na.rm = TRUE), 5)
  West <- round(min(lon, na.rm = TRUE), 5)
  
  my_list <- list("startdate" = startdate_as_character, "enddate" = enddate_as_character,
                  "North" = North, "East" = East, "South" = South, "West" = West)
  return(my_list) 
}

# Example code: 
# define date and geospatial columns for input
# date_col <- as.Date(growgraze_EDI$date_time_UTC)
# lat_col <- growgraze_EDI$latitude
# lon_col <- growgraze_EDI$longitude
# run function
# coverage <- data_coverage(dates = date_col, lat = lat_col, lon = lon_col)

# Insert Custom Project Node ------------
# packages required: xml2

# Function inserts project node after the methods node of an xml document
# requires the existance of a parent_project.txt
# input path to xml file

project_insert <- function(edi_pkg, xml.path) {
  if (!file.exists("parent_project.txt")) {
    stop("parent_project.txt does not exist")
  }
  # read in parent project and xml file to be modified
  newnode <- read_xml("parent_project.txt", from = "xml")
  xml_file <- read_xml(paste0(xml.path, edi_pkg, ".xml"), from = "xml")

  # replace existant project node
  if (is.na(xml_find_first(xml_file, ".//project")) == FALSE) {
      # find old project node
      oldnode <- xml_find_first(xml_file, ".//project") # find project node
      # replace with new project node
      xml_replace(oldnode, newnode)
    warning("<project> node already existed but was overwritten")
  }
  # insert new project node
  if (is.na(xml_find_first(xml_file, ".//project")) == TRUE) {
    # find methods node
    methodsnode <- xml_find_first(xml_file, ".//methods")
    # add project node after methods and before dataTable
    xml_add_sibling(methodsnode, newnode, where = "after")
  }
  # validate script
  if (eml_validate(xml_file) == FALSE) {
    warning("XML document not valid")
  }
  # return(xml_file)
  write_xml(xml_file, paste0(xml.path, edi_pkg, ".xml"))
}

## Example use: 
# read in xml files exported by make_eml
# growgraze_pkg <- "knb-lter-nes.5.1"
# all objects should be of class c("xml_document" "xml_node")
# run function
# project_insert(edi_pkg = growgraze_pkg)

# Quality Assurance: Mapping Sample Locations ----------
# packages required: maps, dplyr, ggplot2

map_locs <- function(df, xvar = "longitude", yvar = "latitude", colorvar = "cruise", region = "shelf") {
  if (region == "transect") {
    nes_region <- map_data("state") %>% filter(long > -72 & lat < 42)
  }
  if (region == "shelf") {
    nes_region <- map_data("state") %>% filter(long > -77)
  } 
  
  # Map given coordinates
  ggplot(df, mapping = aes_string(x = xvar, y = yvar, color = colorvar)) +
    geom_point(size = 1) + 
    geom_polygon(data = nes_region, mapping = aes(x = long, y = lat, group = group),
                 fill = NA, color = "grey50") +
    coord_fixed(1.3) +
    theme_classic()
}

# Example code
# map_locs(df = all_cruises, xvar = "longitude", yvar = "latitude", 
#          region = "transect", colorvar = "cruise")


# Check that entityName doesn't exceed ----------
# required packages: xml2

# refers to the node(s) which exceed the character count

entityName_check <- function(xml_file) {
  for (i in 1:length(xml_find_all(xml_file, ".//entityName"))) {
    # quantify the character count in each entityName node
    n <- str_length(xml_text(xml_find_all(xml_file, ".//entityName")[i]))
    
    # if the character count exceeds 250, trim the text
    if (n > 250) {
      # print the entityNames that violate
      print(paste0("The following entityName exceeds word count: ", 
            xml_text(xml_find_all(xml_file, ".//entityName")[i])))
      # return the node to correct
      index <- i
      return(index)
    } else {
      i <- i + 1
      next
    }
  }
}


# Read in files from the REST API -----------
# required packages: plyr

# accepts the type of data (ctd metadata, bottle summary, chl, nut)
# accepts a vector of cruises to compile data from

read_from_api <- function(type, cruises) {
  # expand the cruises into a dataframe (avoids nested for loops)
  z <- expand.grid(cruises)
  
  # read in data based on the specified source
  if (type == "metadata") {
    urls <- paste0("https://nes-lter-data.whoi.edu/api/ctd/", z$Var1, "/metadata.csv")
    urls <- unlist(urls)
  }
  if (type == "summary") {
    urls <- paste0("https://nes-lter-data.whoi.edu/api/ctd/", z$Var1, "/bottle_summary.csv")
    urls <- unlist(urls)
  }
  if (type == "nutrient") {
    urls <- paste0("https://nes-lter-data.whoi.edu/api/nut/", z$Var1, ".csv")
    urls <- unlist(urls)
  }
  if (type == "chl") {
    urls <- paste0("https://nes-lter-data.whoi.edu/api/chl/", z$Var1, ".csv")
    urls <- unlist(urls)
  }
  if (type == "bottles") {
    urls <- paste0("https://nes-lter-data.whoi.edu/api/ctd/", z$Var1, "/bottles.csv")
    urls <- unlist(urls)
  }
  if (type == "underway") {
    urls <- paste0("https://nes-lter-data.whoi.edu/api/underway/", z$Var1, ".csv")
    urls <- unlist(urls)
  }
  
  ## Cruise Compilation ##
  # case: more than one cruise given
  if (length(cruises) > 1) {
    # begin compilation  
    prev_cruise <- read_csv(urls[1])
    
    # loop through urls to compile cruise data into one file
    for (k in 2:length(urls)){
      # read in data per cruise
      next_cruise <- read_csv(urls[k])
      
      # bind the next cruise to the compiled cruise dataset
      all <- plyr::rbind.fill(prev_cruise, next_cruise)
      
      # if statment to reset the previous cruises until all cruises are read in
      if(k < length(urls)) {
        prev_cruise <- all
      }
    }
    return(all)
    
    # case: only one cruise is given
  } else {
    all <- read_csv(urls)
    return(all)
  }
}

# Example code
# cruises <- c("en608","en617", "en627")
# ctd_metadata <- read_from_api(type = "metadata", cruises = cruises)

##' @importFrom stringr perl
##' @importFrom stringr str_split_fixed
##' @importFrom stringr str_extract_all
##' @importFrom stringr str_split
NULL

##' extract SNP positions from XStringSet objects
##' 
##' @param aln An XStringSet or MultipleAlignment object
##' 
##' @return An IRanges object
##' @export
getSNPs <- function (aln) {
  mm <- consensusMatrix(aln, baseOnly=TRUE)[-5,]
  mm <- which(colSums(mm > 0L) > 1L)
  mm <- IRanges(start=mm, width=1)
  mm
}

##' extract gap positions from XStringSet objects
##' 
##' @param aln A XStringSet or MultipleAlignment object
##' 
##' @return An IRanges object
##' @export
getGaps <- function (aln) {
  mf <- 1/(nrow(aln) + 1)
  gg <- maskGaps(x=aln, min.fraction=mf, min.block.width=1)
  gg <- colmask(gg)
  gg
}

##' Trim NCBI fasta definition lines
##' 
##' @param x A fasta definition line
##' 
##' @export
shortenDeflines <- function (defline) {
  x <- str_trim(str_split_fixed(defline, pattern="\\|", n=5)[,5])
  x <- str_trim(str_split_fixed(x, pattern=",", n=2)[,1])
  x <- str_split_fixed(x, pattern=" +", n=4)[,1:3]
  str_pad(paste(substr(x[,1], 1, 1), substr(x[,2], 1, 4), substr(x[,3], 1, 5),
                sep="_"), width=12, side="right")
}

##' Parse NCBI fasta definition lines
##' 
##' @param defline List or character vector of NCBI fasta deflines.
##' @param species Parse out species designations.
##' 
##' @export
parseDeflines <- function (defline, species=FALSE) {
  # first split into identifier and description at the first blank space
  x <- str_split_fixed(unlist(defline), " ", 2)
  id <- as.list(x[,1])
  desc <- as.list(x[,2])
  
  if (species) {
    x <- str_split_fixed(unlist(desc), " \\[|\\]", 3)
    desc <- x[,1]
    sp <- x[,2]
  }
  
  ## parse identifier patterns
  ## first we extract the database tags which always are 2 or 3 lowercase
  ## letters followed by a pipe.
  db_pattern <- perl("([[:lower:]]{2,3})(?=\\|)")
  db_tag <- str_extract_all(unlist(id), db_pattern)
  db_tag[vapply(db_tag, isEmpty, logical(1))] <- "identifier"
  
  ## next we split the identifier along the database tags
  rmEmpty <- function (x) {
    if (is.atomic(x)) x <- list(x)
    Map(function (x) x[nzchar(x)], x=x)
  }
  
  str_split_list <- function (string, pattern) {  
    Map( function (string, pattern) {
      rmEmpty(str_split(string, paste(pattern, collapse="|")))[[1L]]
    }, string=string, pattern=pattern, USE.NAMES=FALSE) 
  }
  
  
  ids <- str_split_list(id, db_tag)
  id_list <- list()
  for (i in seq_along(ids)) {
    id_list[[i]] <- structure(str_split_list(ids[[i]], "\\|"), names=db_tag[[i]])
  }
  
  if (species) {
    return(list(id=id_list, desc=desc, species=sp))
  } else {
    return(list(id=id_list, desc=desc))
  }
}

##' deparse NCBI fasta definition lines
##' 
##' @param ids List of identifiers.
##' @param descs List of description lines.
##' 
##' @export
deparseDeflines <- function(ids, descs) {
  
  deflines <- list()
  for (i in seq_along(ids)) {
    deflines[[i]] <-
      paste0(paste0(names(ids[[i]]), "|", ids[[i]], collapse="|"), "| ", descs[[i]])
  }
  
  return(deflines)
}

##' View sequence alignments in a web browser
##' 
##' @param msa An XStringSet or MultipleAlignment object.
##' @param start Start position of the alignment
##' @param end End position of the alignment.
##' @param width How many residues per line.
##' @param browser Which web browser to use. It should be in the PATH, 
##' or a full path specified. 
##' 
##' @export
viewMultAlign <- function(msa, 
                          start=1L,
                          end=width(msa)[1L],
                          width=80,
                          browser="google-chrome")
{
  stopifnot(require(hwriter))
  stopifnot(require(Biostrings))
  AA <- FALSE
  DNA <- FALSE
  RNA <- FALSE
  
  if (!is(msa, "XStringSet") && !is(msa, "MultipleAlignment"))
    stop("Provide either an XStringSet object or a MultipleAlignment object")
  
  if (is(msa, "MultipleAlignment")) {
    if (is(msa, "DNAMultipleAlignment"))
      msa <- as(msa, "DNAStringSet")
    else if (is(msa, "RNAMultipleAlignment"))
      msa <- as(msa, "RNAStringSet")
    else if (is(msa, "AAMultipleAlignment"))
      msa <- as(msa, "RNAStringSet")
  }
  
  if (is(msa, "AAStringSet")) {
    msa <- AAStringSet(msa, start=start, end=end)
    #  msa <- c(msa, AAStringSet(consensusString(msa, ambiguityMap=".", ...)))
    #  names(msa)[length(msa)] <- "Consensus"
    AA <- TRUE
  }
  else if (is(msa, "DNAStringSet")) {
    msa <- DNAStringSet(msa, start=start, end=end)
    #  msa <- c(msa, DNAStringSet(consensusString(msa, ambiguityMap=IUPAC_CODE_MAP)))
    #  names(msa)[length(msa)] <- "Consensus"
    DNA <- TRUE
  }
  else if (is(msa, "RNAStringSet")) {
    msa <- RNAStringSet(msa, start=start, end=end)
    #  msa <- c(msa, RNAStringSet(consensusString(msa, ambiguityMap=".", ...)))
    #  names(msa)[length(msa)] <- "Consensus"
    RNA <- TRUE
  }
  
  msa_mat <- Biostrings::as.matrix(msa)
  msa_vec <- as.vector(msa_mat)
  
  ml <- list()
  for (i in seq_len(ncol(msa_mat) %/% width + 1))
    ml[[i]] <- rbind(matrix(c(msa_vec[seq(nrow(msa_mat)*width*(i-1)+1,
                                          nrow(msa_mat)*width*i)]),
                            nrow=nrow(msa_mat), ncol=width,
                            dimnames = list(names(msa))), NA)
  
  mat <- do.call(rbind, ml)
  mat[is.na(mat)] <- "X"
  mat <- cbind(rownames(mat), rep("X", nrow(mat)), mat)
  mat_vec <- as.vector(mat)
  
  font_size <- "-1"
  font_face <- "Courier New,Courier"
  font_color <- "#000000"
  
  aa_bg_color <- c(def="#FFFFFF",
                   gap="#FFFFFF",
                   A="#999999",
                   C="#FFFF00",
                   D="#33CCFF",
                   E="#33CCFF",
                   F="#FF9900",
                   G="#999999",
                   H="#CC0000",
                   I="#FF6666",
                   K="#CC0000",
                   L="#FF6666",
                   M="#3366FF",
                   N="#3366FF",
                   P="#CC33CC",
                   Q="#3366FF",
                   R="#CC0000",
                   S="#999999",
                   T="#3366FF",
                   V="#FF6666",
                   W="#FF9900",
                   Y="#FF9900")
  
  dna_bg_color <- c(def="#FFFFFF",
                    gap="#FFFFFF",
                    A="#FF0000",
                    C="#00FF00",
                    G="#FFFF00",
                    T="#0000FF",
                    U="#0000FF")
  
  if (AA) {
    bg_col <- vector(mode="character", length=ncol(mat)*nrow(mat)) 
    bg_col[seq_len(nrow(mat)*2)]  <- aa_bg_color["def"]
    bg_col[mat_vec == "A"] <- aa_bg_color["A"]
    bg_col[mat_vec == "C"] <- aa_bg_color["C"]
    bg_col[mat_vec == "D"] <- aa_bg_color["D"]
    bg_col[mat_vec == "E"] <- aa_bg_color["E"]
    bg_col[mat_vec == "F"] <- aa_bg_color["F"]
    bg_col[mat_vec == "G"] <- aa_bg_color["G"]
    bg_col[mat_vec == "H"] <- aa_bg_color["H"]
    bg_col[mat_vec == "I"] <- aa_bg_color["I"]
    bg_col[mat_vec == "K"] <- aa_bg_color["K"]
    bg_col[mat_vec == "L"] <- aa_bg_color["L"]
    bg_col[mat_vec == "M"] <- aa_bg_color["M"]
    bg_col[mat_vec == "N"] <- aa_bg_color["N"]
    bg_col[mat_vec == "P"] <- aa_bg_color["P"]
    bg_col[mat_vec == "Q"] <- aa_bg_color["Q"]
    bg_col[mat_vec == "R"] <- aa_bg_color["R"]    
    bg_col[mat_vec == "S"] <- aa_bg_color["S"]
    bg_col[mat_vec == "T"] <- aa_bg_color["T"]
    bg_col[mat_vec == "V"] <- aa_bg_color["V"]
    bg_col[mat_vec == "W"] <- aa_bg_color["W"]
    bg_col[mat_vec == "Y"] <- aa_bg_color["Y"]
    bg_col[mat_vec == "-"] <- aa_bg_color["gap"]
    bg_col[mat_vec == "."] <- aa_bg_color["def"]
    bg_col[mat_vec == "X"] <- aa_bg_color["def"]
  }
  
  
  if (DNA || RNA) {
    bg_col <- vector(mode="character", length=ncol(mat)*nrow(mat)) 
    bg_col[seq_len(nrow(mat)*2)]  <- dna_bg_color["def"]
    bg_col[mat_vec == "A"] <- dna_bg_color["A"]
    bg_col[mat_vec == "C"] <- dna_bg_color["C"]
    bg_col[mat_vec == "G"] <- dna_bg_color["G"]
    bg_col[mat_vec == "T"] <- dna_bg_color["T"]
    bg_col[mat_vec == "-"] <- dna_bg_color["gap"]
    bg_col[mat_vec == "."] <- dna_bg_color["def"]
    bg_col[mat_vec == "X"] <- dna_bg_color["def"]
  }
  
  font_col <- rep(font_color, length=ncol(mat)*nrow(mat))
  font_col[mat_vec == "X"] <- aa_bg_color["def"]
  
  bg_col <- array(bg_col, dim(mat))
  nowrap <- c('true', rep(NA, ncol(mat)-1))
  align <- c("right", rep("center", ncol(mat)-1))
  valign <- c("NA", rep("center", ncol(mat)-1))
  
  elem <- 
    hmakeTag('font',
             hmakeTag('font',
                      hmakeTag('font', mat,
                               color=font_col),
                      face=font_face),
             size=font_size)
  
  p <- tempfile(fileext=".html")
  hwrite(elem, p, bgcolor=bg_col,
         col.nowrap=nowrap, col.align=align, col.valign=valign,
         table.border=0, table.cellspacing=0, table.cellpadding=1)
  browseURL(p, browser)
  #unlink(p)
  return(invisible(NULL))
}

##' display a html file in a browser
##' 
##' @param html file path or html encoded character string
##' @param browser browser
##' @param unlink remove temporary file
##' 
##' @export 
displayHTML <- function (html, browser=getOption("browser"), unlink=TRUE)
{
  if (!file.exists(html)) { 
    f_tmp <- tempfile(fileext=".html")
    writeLines(html, f_tmp)
  } else {
    f_tmp <- html
    unlink <- FALSE
  }
  
  browseURL(url=f_tmp, browser=browser)
  
  if (unlink) {
    Sys.sleep(2)
    unlink(f_tmp)
  }
}

##' generate random tags for minisequencing
##' 
##' @param n number of tags to generate
##' @param size length of tags
##' @param GC_percent average GC content of tags
##' @param max_rep maximum number of identical bases in a row
##' 
##' @export
generateTags <- function (n=20, size=20, GC_percent=60, max_rep=4)
{
  
  stopifnot(require(Biostrings))
  
  if (missing(n))
    stop("Provide the number of tags you want to generate")
  
  bases <- c("A", "T", "G", "C")
  probability <- c((100-GC_percent)/2, (100-GC_percent)/2, GC_percent/2, GC_percent/2)/100
  
  tags <- DNAStringSet()
  for (i in seq_len(n)) {
    base_vector <- rep(1,5)
    while (any(rle(base_vector)$lengths > 4)) {
      base_vector <- sample(bases, size=size, replace=TRUE, prob=probability)
    }
    tags[i] <- DNAStringSet(paste(base_vector, collapse=""))
    names(tags[i]) <- paste0("tag", i)
  }
  tags
  
}


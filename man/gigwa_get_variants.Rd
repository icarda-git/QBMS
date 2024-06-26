% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gigwa.R
\name{gigwa_get_variants}
\alias{gigwa_get_variants}
\title{Get Available Variants in the Selected GIGWA Run}
\usage{
gigwa_get_variants(
  max_missing = 1,
  min_maf = 0.5,
  samples = NULL,
  start = NULL,
  end = NULL,
  referenceName = NULL
)
}
\arguments{
\item{max_missing}{Maximum missing ratio (by sample) between 0 and 1 (default is 1 for 100\%).}

\item{min_maf}{Minimum Minor Allele Frequency (MAF) between 0 and 1 (default is 0 for 0\%).}

\item{samples}{A list of samples subset (default is NULL, which will retrieve for all samples).}

\item{start}{Start position of region (zero-based, inclusive) (e.g., 19750802).}

\item{end}{End position of region (zero-based, exclusive)    (e.g., 19850125).}

\item{referenceName}{Reference sequence name    (e.g., '6H' in the Barley LI-AM).}
}
\value{
A data.frame that has the first 4 columns describing attributes of the SNP 
(rs#: variant name, alleles: reference allele / alternative allele, chrom: chromosome name, 
and pos: position in bp), while the following columns describe the SNP value for a 
single sample line using numerical coding 0, 1, and 2 for reference, heterozygous, and 
alternative/minor alleles.
}
\description{
Query the variants (e.g., SNPs markers) in the selected GIGWA run that match a given criteria.
}
\examples{
if (interactive()) {
  # Configure your GIGWA connection
  set_qbms_config("https://gigwa.southgreen.fr/gigwa/", 
                  time_out = 300, engine = "gigwa", no_auth = TRUE)

  # Select a database by name
  gigwa_set_db("Sorghum-JGI_v1")

  # Select a project by name
  gigwa_set_project("Nelson_et_al_2011")
  
  # Select a specific run by name
  gigwa_set_run("run1")
  
  # Get the marker matrix 
  marker_matrix <- gigwa_get_variants(max_missing = 0.2, 
                                      min_maf = 0.35, 
                                      samples = c("ind1", "ind3", "ind7"))
}
}
\author{
Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
}

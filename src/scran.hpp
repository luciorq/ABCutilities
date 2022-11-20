#ifndef __SCRAN__
#define __SCRAN__

#include <Rcpp.h>

extern "C" {

/* Normalization.

SEXP pool_size_factors(SEXP, SEXP, SEXP, SEXP);

SEXP subset_and_divide(SEXP, SEXP, SEXP, SEXP);

// Cell cycle calling.

SEXP cyclone_scores (SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

// Correlation calclulations.

SEXP get_null_rho(SEXP, SEXP, SEXP, SEXP);

SEXP get_null_rho_design(SEXP, SEXP, SEXP, SEXP, SEXP);

SEXP test_rnorm(SEXP, SEXP, SEXP);

SEXP compute_rho_pairs(SEXP, SEXP, SEXP);

SEXP combine_rho(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

// Test shuffling functions.

SEXP test_shuffle_vector(SEXP, SEXP, SEXP, SEXP);

SEXP test_shuffle_matrix(SEXP, SEXP, SEXP);

// Variance calculations.
SEXP fit_linear_model(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP fit_oneway(SEXP, SEXP, SEXP);

SEXP calc_log_count_stats(SEXP, SEXP, SEXP, SEXP, SEXP);

SEXP calc_log_expected(SEXP, SEXP, SEXP, SEXP, SEXP);

SEXP calc_log_sqdiff(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

// DE analysis functions.

SEXP combine_simes(SEXP, SEXP);

SEXP overlap_exprs(SEXP, SEXP, SEXP, SEXP);

// Clustering functions.

SEXP get_scaled_ranks(SEXP, SEXP, SEXP, SEXP);

SEXP build_snn_rank(SEXP);

SEXP build_snn_number(SEXP);

// Miscellaneous functions.

SEXP get_residuals(SEXP, SEXP, SEXP, SEXP, SEXP);

SEXP compute_CV2(SEXP, SEXP, SEXP, SEXP);

SEXP shuffle_matrix(SEXP, SEXP, SEXP);
*/
}

#endif // __SCRAN__
// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// col_merge_dgCMatrices_rcpp
Eigen::SparseMatrix<double> col_merge_dgCMatrices_rcpp(List mat_list, List mat_colnames, std::vector<std::string> all_colnames);
RcppExport SEXP _Matrix_Layered_col_merge_dgCMatrices_rcpp(SEXP mat_listSEXP, SEXP mat_colnamesSEXP, SEXP all_colnamesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< List >::type mat_list(mat_listSEXP);
    Rcpp::traits::input_parameter< List >::type mat_colnames(mat_colnamesSEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type all_colnames(all_colnamesSEXP);
    rcpp_result_gen = Rcpp::wrap(col_merge_dgCMatrices_rcpp(mat_list, mat_colnames, all_colnames));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_Matrix_Layered_col_merge_dgCMatrices_rcpp", (DL_FUNC) &_Matrix_Layered_col_merge_dgCMatrices_rcpp, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_Matrix_Layered(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

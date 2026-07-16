#include <string>

#include "Utils.h"
#include "compat.h"

// [[Rcpp::export]]
SEXP get_dd_val(int i, SEXP rho, SEXP default_value, bool force=false) {
    // TODO: check args
#if R_VERSION >= R_Version(4, 6, 0)
    // Use the public `...` binding API (added in R 4.6.0) rather than reaching
    // into the DOTSXP pairlist. R_findDotsEnv walks the parent environments, so
    // this keeps the inheriting lookup the old findVar(R_DotsSymbol, rho) did.
    SEXP dots_env = R_findDotsEnv(rho);

    if (dots_env == R_EmptyEnv) {
        Rf_error("Unable to find ..%d - used in an incorrect context, no ... to look in", i);
    }

    if (R_DotsLength(dots_env) < i) {
        Rf_error("Unable to find ..%d - the ... does not contain %d elements", i, i);
    }

    // R_GetDotType / R_DotsElt use 1-based indexing, like the R function ...elt()
    R_DotType_t type = R_GetDotType(i, dots_env);

    if (type == R_DotTypeMissing) {
        return R_MissingArg;
    } else if (type == R_DotTypeDelayed && !force) {
        // an unforced promise: do not force it, hand back the caller's default
        return default_value;
    }

    // a plain value, an already-forced promise, or force was requested:
    // R_DotsElt forces the element if needed (like ...elt())
    return R_DotsElt(i, dots_env);
#else
    SEXP dots = findVar(R_DotsSymbol, rho);

    if (TYPEOF(dots) == DOTSXP && dots != R_UnboundValue) {
        if (length(dots) >= i) {
            dots = nthcdr(dots, i - 1);
            SEXP val = CAR(dots);

            if (TYPEOF(val) == PROMSXP) {
                if (force) {
                    return Rf_eval(val, rho);
                } else if (PRVALUE(val) == R_UnboundValue) {
                    return default_value;
                } else {
                    return PRVALUE(val);
                }
            } else {
                return val;
            }
        } else {
            Rf_error("Unable to find ..%d - the ... does not contain %d elements", i, i);
        }
    } else {
        Rf_error("Unable to find ..%d - used in an incorrect context, no ... to look in", i);
    }

    return default_value;
#endif
}

// [[Rcpp::export]]
SEXP reassign_function(SEXP target_fun, SEXP new_fun) {
  if (TYPEOF(target_fun) != CLOSXP) Rf_error("target_fun must be a function");
  if (TYPEOF(new_fun) != CLOSXP) Rf_error("new_fun must be a function");

  // A closure's body is stored in its CDR slot, so we can replace it in place
  // through the public SETCDR() instead of the non-API SET_BODY(). R_ClosureBody
  // (added in R 4.5.0, backported in compat.h) is the API accessor for the body.
  SETCDR(target_fun, R_ClosureBody(new_fun));

  return R_NilValue;
}

// [[Rcpp::export]]
SEXP create_duplicate(SEXP target) {
  if (isNull(target)) Rf_error("target must not be null");

  return duplicate(target);
}

// [[Rcpp::export]]
std::string environment_name(SEXP env) {
    if (R_IsPackageEnv(env) == TRUE) {
        // cf. builtin.c:432 do_envirName
        return CHAR(STRING_ELT(R_PackageEnvName(env), 0));
    } else if (R_IsNamespaceEnv(env) == TRUE) {
        // cf. builtin.c:434 do_envirName
        return CHAR(STRING_ELT(R_NamespaceEnvSpec(env), 0));
    } else {
        return "";
    }
}

// [[Rcpp::export]]
std::string environment_name_as_code(SEXP env) {
    if (env == R_EmptyEnv) {
        return "emptyenv()";
    } else if (env == R_GlobalEnv) {
        return ".GlobalEnv";
    } else if (env == R_BaseEnv || env == R_BaseNamespace) {
        return ".BaseNamespaceEnv";
    } else {
        std::string name = environment_name(env);
        if (!name.empty()) {
            return "getNamespace(\"" + name + "\")";
        } else {
            return "";
        }
    }
}

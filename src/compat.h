#ifndef GENTHAT_COMPAT_H
#define GENTHAT_COMPAT_H

// Compatibility shims so that genthat compiles both against the modern,
// API-compliant R C interface and against older versions of R.
//
// Recent versions of R have been hiding long-standing but non-API entry
// points (ATTRIB, ENCLOS, CLOENV, Rf_findVarInFrame, ...) and providing
// proper API replacements instead.  Here we always use the new API names and,
// for the versions of R that predate them, define them in terms of the old
// entry points (which are still available there).
//
// See "Writing R Extensions", section "Moving into C API compliance", in
// particular the backports listed under "Some backports".

#include <Rversion.h>
#include <Rinternals.h>

// R_ClosureEnv / R_ClosureBody and R_ParentEnv were added in R 4.5.0 as API
// replacements for the (now non-API) CLOENV, BODY and ENCLOS macros.
#if R_VERSION < R_Version(4, 5, 0)
# define R_ClosureEnv(x)  CLOENV(x)
# define R_ClosureBody(x) BODY(x)
# define R_ParentEnv(x)   ENCLOS(x)
#endif

// R_getVarEx (a get0()-like, non-inheriting frame lookup) was added in R 4.5.0
// as an API replacement for the non-API Rf_findVarInFrame.
#if R_VERSION < R_Version(4, 5, 0)
static inline SEXP R_getVarEx(SEXP sym, SEXP env, Rboolean inherits, SEXP ifnotfound) {
    SEXP val = inherits ? Rf_findVar(sym, env) : Rf_findVarInFrame(env, sym);
    return (val == R_UnboundValue) ? ifnotfound : val;
}
#endif

// R_mapAttrib (iterate over an object's attributes) was added in R 4.6.0 as an
// API replacement for directly walking the non-API ATTRIB pairlist.  The
// backport below is the one given in "Writing R Extensions"; on the versions
// of R that need it the ATTRIB/TAG/CAR/CDR macros are still available.
#if R_VERSION < R_Version(4, 6, 0)
static inline SEXP R_mapAttrib(SEXP x, SEXP (*FUN)(SEXP, SEXP, void *), void *data) {
    PROTECT_INDEX api;
    SEXP a = ATTRIB(x);
    SEXP val = NULL;

    PROTECT_WITH_INDEX(a, &api);
    while (a != R_NilValue) {
        SEXP tag = PROTECT(TAG(a));
        SEXP attr = PROTECT(CAR(a));
        val = FUN(tag, attr, data);
        UNPROTECT(2); /* tag, attr */
        if (val != NULL)
            break;
        REPROTECT(a = CDR(a), api);
    }
    UNPROTECT(1); /* a */
    return val;
}
#endif

#endif // GENTHAT_COMPAT_H

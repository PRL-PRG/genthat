#include <Rcpp.h>
#include <R.h>

#include <string>
#include <vector>

#include "Utils.h"
#include "compat.h"

using namespace Rcpp;
using namespace std;

// Collects an object's (tag, value) attribute pairs so they can be iterated in
// C++ afterwards. Used as the callback for R_mapAttrib (the API replacement for
// walking the non-API ATTRIB pairlist). The callback itself must not raise a
// C++ exception, as it is invoked from R's C code, so it only accumulates the
// SEXPs; the actual (possibly throwing) serialization happens once it returns.
struct attrib_pair {
    SEXP tag;
    SEXP value;
};

static SEXP collect_attrib(SEXP tag, SEXP value, void *data) {
    static_cast<std::vector<attrib_pair> *>(data)->push_back({tag, value});
    return nullptr; // keep iterating
}

// options for deparse
// from Defn.h
#define KEEPINTEGER 	1
#define SHOWATTRIBUTES 	4
#define KEEPNA			64
#define HEXNUMERIC      256
#define DIGITS16        512

extern "C" {
    SEXP Rf_deparse1(SEXP call, Rboolean abbrev, int opts);
    SEXP Rf_eval(SEXP e, SEXP rho);
}

class serialization_error : public runtime_error {
public:
    serialization_error(string details) : runtime_error("Serialization error: " + details) {}
};

class sexp_not_supported_error : public serialization_error {
public:
    sexp_not_supported_error(string sexp_type) : serialization_error("SEXP type " + sexp_type + " not supported!") {}
};

class cycle_error : public serialization_error {
public:
    cycle_error() : serialization_error("Serialized data structure contains cycle!") {}
};

static const set<string> UNARY_FUNS = {
    // TODO are these all?
    "+", "-", "!", "~", "?"
};

static const set<string> BASE_INFIX_FUNS = {
    "<-", "=", "<<-", "+", "-", "*", "/", "^", "==", "!=", "<", "<=", ">=", ">", "&", "|", "!", "&&", "||", "~"
};

static const set<string> BASE_INFIX_FUNS_NO_SPACE = {
    ":", "::", ":::", "$", "@"
};

// "keywords" that need to be escaped
// cf. https://stat.ethz.ch/R-manual/R-devel/library/base/html/Reserved.html
static const set<string> KEYWORDS = {
    "if", "else", "repeat", "while", "function", "for", "in", "next", "break",
    "TRUE", "FALSE", "NULL", "Inf", "NaN", "NA", "NA_integer_", "NA_real_",
    "NA_complex_", "NA_character_", "..."
};

static const set<string> UNSUPPORTED_EXTPTR_CLASSES = {
    "RegisteredNativeSymbol", "DLLHandle", "DLLInfoReference"
};

bool is_digit(char c) {
    return c >= '0' && c <= '9';
}

// A syntactically valid name consists of letters, numbers and the dot or underline
// characters and starts with a letter or the dot not followed by a number
// cf. https://stat.ethz.ch/R-manual/R-devel/library/base/html/make.names.html
bool is_syntactically_valid_name(std::string const & name) {
    // dot followed by number must be escaped
    if (name[0] == '.' && is_digit(name[1]))
        return false;
    // number at the beginning must be escaped
    if (is_digit(name[0]))
        return false;
    // underscore followed by anything must be escaped
    if (name[0] == '_')
        return false;
    // we have checked the first character special cases, just make sure letters are valid now
    for (char c : name) {
        if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || is_digit(c) || c == '.' || c == '_')
            continue;
        return false;
    }
    return true;
}

static const map<SEXP, string> SPEC_ATTRIBUTES_NAMES = {
    {R_DimSymbol, ".Dim"},
    {R_DimNamesSymbol, ".Dimnames"},
    {R_TspSymbol, ".Tsp"},
    {R_NamesSymbol, ".Names"},
    {R_LevelsSymbol, ".Label"}
};

static SEXP GENTHAT_EXTRACTED_CLOSURE_SYM = Rf_install("genthat_extracted_closure");

class Serializer {
private:
    static string attribute_name(SEXP const s) {
        auto e = SPEC_ATTRIBUTES_NAMES.find(s);
        if (e != SPEC_ATTRIBUTES_NAMES.end()) {
            return e->second;
        } else {
            string tag = CHAR(PRINTNAME(s));
            return escape_name(tag);
        }
    }

    string wrap_in_attributes(SEXP const s, string const &s_str) {
        RObject protected_s(s);

        vector<string> elems;

        vector<attrib_pair> attribs;
        R_mapAttrib(s, collect_attrib, &attribs);

        for (auto const &a : attribs) {
            SEXP tag = a.tag;

            if (tag == Rf_install("srcref")) {
                continue;
            } else if (tag == R_NamesSymbol) {
                if (XLENGTH(a.value) == 0) {
                    // naming for lists is taken care of in serialize VECSXP, but
                    // there is a special case of empty names attribute (cf. #116).
                    elems.push_back("names=character()");
                } else if (TYPEOF(s) != VECSXP) {
                    // for lists we handle it directly while creating the list
                    // call (eg. list(a=1, b=2)), but for vectors we do not
                    // since they are handled by deparse call
                    elems.push_back("names=" + serialize(a.value, true));
                }
            } else {
                string name = attribute_name(tag);

                if (name.find("genthat_") == 0) {
                    continue;
                }

                elems.push_back(name + "=" + serialize(a.value, true));
            }
        }

        string a_str;
        if (elems.size() > 0) {
            a_str += elems[0];

            for (auto i = next(begin(elems)); i != end(elems); ++i) {
                a_str += ", " + *i;
            }
        }

        return a_str.empty() ? s_str : "structure(" + s_str + ", " + a_str + ")";
    }

    string format_argument(SEXP const arg) {
        SEXP arg_name = TAG(arg);
        SEXP arg_value = CAR(arg);

        string name;

        switch (TYPEOF(arg_name)) {
        case NILSXP:
            name = "";
            break;
        case SYMSXP:
            name = serialize(arg_name, false);
            break;
        default:
            throw serialization_error("Unexpected SEXPTYPE in function arguments: " + to_string(TYPEOF(arg_name)));
        }

        string value = serialize(arg_value, false);
        string res;

        if (!name.empty() && !value.empty()) {
            res = name + "=" + value;
        } else if (name.empty()) {
            res = value;
        } else if (value.empty()) {
            res = name;
        } else {
            res = "";
        }

        return res;
    }

    string format_arguments(SEXP const args, string const &sep=", ") {
        string res = "";

        for (SEXP arg = args; !Rf_isNull(arg); arg = CDR(arg)) {
            res += format_argument(arg);
            res += Rf_isNull(CDR(arg)) ? "" : sep;
        }

        return res;
    }

    string get_element_name(SEXP const names, int i) {
        string name;

        if (!Rf_isNull(names)) {
            name = CHAR(STRING_ELT(names, i));
        }

        return name;
    }

    SEXP extract_closure(SEXP fun) {
        // genthat:::extract_closure(fun)
        auto call = Rf_lang2(
            Rf_lang3(R_TripleColonSymbol, Rf_mkString("genthat"), Rf_mkString("extract_closure")),
            fun
        );

        return Rcpp_eval(call, R_GlobalEnv);
    }

    string concatenate(StringVector v, string sep) {
        string res;

        for (int i = 0; i < v.size(); i++) {
            res += v(i);
            res += i + 1 < v.size() ? sep : "";
        }

        return res;
    }

    // contains the list of visited environments so far
    // it is used for the ENVSXP serialization
    set<SEXP> visited_environments;

    // seen externals
    vector<SEXP> externals_;

public:
    static bool is_unary_fun(string const &fun) {
        return UNARY_FUNS.find(fun) != UNARY_FUNS.end();
    }

    static bool is_infix_fun_no_space(string const &fun) {
        return BASE_INFIX_FUNS_NO_SPACE.find(fun) != BASE_INFIX_FUNS_NO_SPACE.end();
    }

    static bool is_infix_fun(string const &fun) {
        if (BASE_INFIX_FUNS.find(fun) != BASE_INFIX_FUNS.end()) {
            return true;
        } else if (BASE_INFIX_FUNS_NO_SPACE.find(fun) != BASE_INFIX_FUNS_NO_SPACE.end()) {
            return true;
        } else if (fun[0] == '%' && fun[fun.size() - 1] == '%') {
            return true;
        } else {
            return false;
        }
    }

    static string escape_name(string const &name) {
        if (name.empty()) {
            return name;
        } else if (KEYWORDS.find(name) != KEYWORDS.end() || !is_syntactically_valid_name(name)) {
            return "`" + name + "`";
        } else {
            return name;
        }
    }

    string serialize_binary(SEXP s) {
        if (TYPEOF(s) == EXTPTRSXP) {
            for (const auto &x : UNSUPPORTED_EXTPTR_CLASSES) {
                if (Rf_inherits(s, x.c_str())) {
                    throw serialization_error("EXTPTR with class " +  x + " is not supported");
                }
            }
        }

        const vector<SEXP>::const_iterator pos =
            find_if(externals_.begin(), externals_.end(), [s](SEXP x)->bool { return x == s; });

        int idx;

        if (pos != externals_.end()) {
            idx = pos - externals_.begin() + 1;
        } else {
            externals_.push_back(RObject(s));
            idx = externals_.size();
        }

        return ".ext." + to_string(idx);
    }

    string serialize(SEXP s, bool quote) {
        if (Rf_isS4(s)) {
            return serialize_binary(s);
        }

        switch (TYPEOF(s)) {
        case NILSXP:
            return "NULL";
        case VECSXP: { /* lists */
            RObject protected_s(s);

            SEXP names = Rf_getAttrib(s, R_NamesSymbol);
            int size = XLENGTH(s);
            string args;

            for (int i = 0 ; i < size ; i++) {
                string value = serialize(VECTOR_ELT(s, i), true);
                string name = escape_name(get_element_name(names, i));

                args += name.empty() ? "" : name + "=";
                args += value;
                args += i + 1 < size ? ", " : "";
            }

            return wrap_in_attributes(s, "list(" + args + ")");
        }
        case LISTSXP: { /* pairlists */
            RObject protected_s(s);

            SEXP names = Rf_getAttrib(s, R_NamesSymbol);
            string args;
            int i = 0;

            for (SEXP con = s; con != R_NilValue; con = CDR(con)) {
                string value = serialize(CAR(con), false);

                if (!Rf_isNull(names)) {
                    string name = escape_name(CHAR(STRING_ELT(names, i++)));
                    args += name.empty() ? "" : name + "=";
                }

                args += value;
                args += CDR(con) != R_NilValue ? ", " : "";
            }

            return wrap_in_attributes(s, "alist(" + args + ")");
        }
        case LGLSXP:
        case INTSXP:
        case REALSXP:
        case CPLXSXP:
        case STRSXP:
        case RAWSXP:
        case EXPRSXP:
        case SPECIALSXP:
        case BUILTINSXP: {
            // all the primitive vectors should be serialized by SEXP deparse1(SEXP call, Rboolean abbrev, int opts)
            StringVector deparsed = Rf_deparse1(s, FALSE, KEEPINTEGER | KEEPNA | DIGITS16);
            string res = concatenate(deparsed, "\n");
            if (res.length() > 512) {
                return serialize_binary(s);
            } else {
                string res_with_attributes = wrap_in_attributes(s, res);
                return res_with_attributes;
            }
        }
        case SYMSXP: {
            RObject protected_s(s);
            string symbol = string(CHAR(PRINTNAME(s)));

            if (symbol.empty()) {
                return quote ? "quote(expr=)" : "";
            } else {
                symbol = escape_name(symbol);
                return quote ? "quote(" + symbol  + ")" : symbol;
            }
        }
        case ENVSXP: {
            RObject protected_s(s);

            if (visited_environments.find(s) != visited_environments.end()) {
                throw cycle_error();
            }

            visited_environments.insert(s);

            SEXP parent = R_ParentEnv(s);
            SEXP names = R_lsInternal3(s, TRUE, FALSE);
            int n = XLENGTH(names);

            string elems;
            for (int i = 0; i < n; i++) {
                const char *key = CHAR(STRING_ELT(names, i));
                SEXP value = R_getVarEx(Rf_install(key), s, FALSE, R_UnboundValue);

                elems += escape_name(key) + "=" + serialize(value, true);
                elems += i + 1 < n ? ", " : "";
            }

            string parent_env_arg;
            if (!Rf_isNull(parent) && visited_environments.find(parent) == visited_environments.end()) {
                std::string parent_env;

                if (parent == R_EmptyEnv) {
                    parent_env = "emptyenv()";
                } else if (parent == R_GlobalEnv) {
                    parent_env = "globalenv()";
                } else if (parent == R_BaseEnv || parent == R_BaseNamespace) {
                    parent_env = "baseenv()";
                } else if (R_IsPackageEnv(parent) == TRUE) {
                    // cf. builtin.c:432 do_envirName
                    string parent_name = CHAR(STRING_ELT(R_PackageEnvName(parent), 0));

                    parent_env = "as.environment(\"" + parent_name + "\")";
                } else if (R_IsNamespaceEnv(parent) == TRUE) {
                    // cf. builtin.c:434 do_envirName
                    string parent_name = CHAR(STRING_ELT(R_NamespaceEnvSpec(parent), 0));

                    parent_env = "getNamespace(\"" + parent_name + "\")";
                } else {
                    parent_env = serialize(parent, false);
                }

                parent_env_arg = ", parent=" + parent_env;
            } else {
                // this means that we mush have seen it (unless it is null)
                // and so it will be eventually linked
                // this seems better than to leave it empty:
                // list2env(..., parent=emptyenv()) than list2env(...)
                parent_env_arg = ", parent=emptyenv()";
            }

            visited_environments.erase(s);

            return "list2env(list(" + elems + ")" + parent_env_arg + ")";
        }
        case LANGSXP: {
            RObject protected_s(s);
            SEXP name = CAR(s);
            string fun;

            if (TYPEOF(name) == SYMSXP) {
                fun = string(CHAR(PRINTNAME(name)));
            } else if (TYPEOF(name) == LANGSXP) {
                fun = serialize(name, false);
            } else {
                throw serialization_error("Unknown CAR(x: LANGSXP): " + TYPEOF(name));
            }

            string res;
            s = CDR(s);

            if (is_unary_fun(fun) && CADR(s) == R_NilValue) {
                SEXP rhs = CAR(s);

                res = fun + serialize(rhs, false);
            } else if (is_infix_fun(fun)) {
                SEXP lhs = CAR(s);
                SEXP rhs = CADR(s);

                string space = is_infix_fun_no_space(fun) ? "" : " ";

                res =
                    serialize(lhs, false) +
                    space + fun + space +
                    serialize(rhs, false);

            } else if (fun[0] == '[') {
                string collection = serialize(CAR(s), false);
                string subset = format_arguments(CDR(s));
                string close;

                if (fun == "[") {
                    close = "]";
                } else if (fun == "[[") {
                    close = "]]";
                } else {
                    throw serialization_error("Unknown sub-setting operator: " + fun);
                }

                res = collection + fun + subset + close;
            } else if (fun == "function") {
                string args = format_arguments(CAR(s));
                string body = serialize(CADR(s), false);

                res = fun + "(" + args + ") " + body;
            } else if (fun == "{") {
                string args = format_arguments(s, "\n\t");
                res = "{\n\t" + args + "\n}";
            } else if (fun == "(") {
                string args = format_arguments(s);
                res = "(" + args + ")";
            } else {
                string args = format_arguments(s);

                if (TYPEOF(name) == SYMSXP) {
                    fun = escape_name(fun);
                }

                res = fun + "(" + args + ")";
            }

            return res;
        }
        case S4SXP:
        case DOTSXP:
        case EXTPTRSXP:
            return serialize_binary(s);
        case BCODESXP:
            throw sexp_not_supported_error("BCODESXP");
        case WEAKREFSXP:
            throw sexp_not_supported_error("WEAKREFSXP");
        case CLOSXP: {
            RObject protected_s(s);

            SEXP extracted = extract_closure(s);
            SEXP env = R_ClosureEnv(extracted);
            string env_code = environment_name_as_code(env);

            // if this is empty, the environment is not a empty / base / package / namespace environment
            // and therefore we need to serialize it as long as it has not already been serialized
            if (env_code.empty() && visited_environments.find(env) == visited_environments.end()) {
                env_code = serialize(env, false);
            }

            // temporary remove genthat_* attributes since we do not control the deparsing
            // while it is tempting to remove the SHOWATTRIBUTES flag, that will unfortunately
            // remove the attributes from all
            RObject extracted_obj(extracted);
            std::map<std::string, SEXP> attrs;

            for (auto const& name : extracted_obj.attributeNames()) {
                if (name.find("genthat_") == 0) {
                    attrs[name] = extracted_obj.attr(name); 
                    extracted_obj.attr(name) = R_NilValue;
                }
            }

            auto fun_code = concatenate(Rf_deparse1(extracted, FALSE, KEEPINTEGER | SHOWATTRIBUTES | KEEPNA | DIGITS16), "\n");

            // we put them back
            for (auto const& x : attrs) {
                extracted_obj.attr(x.first) = x.second;
            }

            string res;

            if (env_code.empty()) {
                res = fun_code;
            } else {
                res = "genthat::with_env(" + fun_code + ", env=" + env_code + ")";
            }

            return res;
        }
        case CHARSXP:
            throw sexp_not_supported_error("CHARSXP");
        case PROMSXP: {
            SEXP forced = Rf_eval(s, Environment::global_env());
            return serialize(forced, quote);
        }
        default:
            throw sexp_not_supported_error("unknown");
        }
    }

    StringVector serialize_value(SEXP s) {
        return StringVector::create(serialize(s, false));
    }

    Environment externals(Environment target=Environment::empty_env().new_child(true)) {
        for (int i=0; i<externals_.size(); i++) {
            target[".ext." + to_string(i + 1)] = externals_[i];
        }

        return target;
    }

    int externals_size() {
        return externals_.size();
    }
};

// [[Rcpp::export]]
StringVector serialize_value(SEXP s) {
    Serializer serializer;

    auto result = serializer.serialize_value(s);

    if (serializer.externals_size() > 0) {
        auto externals = serializer.externals();
        result.attr("externals") = externals;
    }

    return result;
}

// [[Rcpp::export]]
bool is_infix_fun_no_space(std::string const &fun) {
    return Serializer::is_infix_fun_no_space(fun);
}

// [[Rcpp::export]]
bool is_infix_fun(std::string const &fun) {
    return Serializer::is_infix_fun(fun);
}

// [[Rcpp::export]]
std::string escape_name(std::string const &name) {
    return Serializer::escape_name(name);
}

RCPP_MODULE(SerializerModule) {
  class_<Serializer>("Serializer")
      .default_constructor()
      .method("serialize_value", &Serializer::serialize_value)
      .method("externals", &Serializer::externals)
  ;
}

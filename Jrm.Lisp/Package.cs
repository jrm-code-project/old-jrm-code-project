using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;

namespace Lisp
{
    public class Package
    {
        private static List<Package> AllPackages = new List<Package> ();
	    private static Package ClosPackage = new Package ("CLOS",new string []{});
        private static Package CommonLispPackage;
        private static Package CommonLispUserPackage = new Package("COMMON-LISP-USER",
            new string [] {"CL-USER"});
        private static Package KeywordPackage = new KeywordPackage ();

        private static Package SystemPackage = new Package ("SYSTEM",
            new string [] { "SYS" });

        private string name;
        private List<string> nicknames;
        internal List<Symbol> PresentSymbolList;
        internal List<Symbol> ExternalSymbolList;
        private List<Package> PackageUseList;

        protected Package (string name, string [] nicknames)
        {
            this.name = String.Intern(name);
            this.nicknames = new List<string> ();
            this.PresentSymbolList = new List<Symbol> ();
            this.ExternalSymbolList = new List<Symbol> ();
            this.PackageUseList = new List<Package> ();
            if (nicknames != null)
                foreach (string nickname in nicknames)
                    this.nicknames.Add (nickname);
            AllPackages.Add (this);
        }

        public string Name
        {
            get
            {
                return this.name;
            }
        }

        public ReadOnlyCollection<string> Nicknames
        {
            get
            {
                return new ReadOnlyCollection<string> (this.nicknames);
            }
        }

        public override string ToString ()
        {
            return "#<PACKAGE \"" + this.name + "\">";
        }

        static public Package Clos
        {
            get
            {
                return ClosPackage;
            }
        }

        static public Package CommonLisp
        {
            get
            {
                return CommonLispPackage;
            }
        }

        static public Package CommonLispUser
        {
            get
            {
                return CommonLispUserPackage;
            }
        }

        static public Package Keyword
        {
            get
            {
                return KeywordPackage;
            }
        }

        static public Package Sys
        {
            get
            {
                return SystemPackage;
            }
        }

        public void Export (Symbol sym)
        {
            if (this.ExternalSymbolList.Contains (sym))
                return;
            this.ExternalSymbolList.Add (sym);
        }

        public virtual Symbol FindSymbol (string name)
        {
            Predicate<Symbol> matchString = new Predicate<Symbol> (new StringMatcher (name).MatchesSymbol);
            Symbol probe = this.PresentSymbolList.Find (matchString);
            if (probe == null) {
                foreach (Package u in this.PackageUseList) {
                    probe = u.ExternalSymbolList.Find (matchString);
                    if (probe != null)
                        return probe;
                }
                return null;
            }
            else
                return probe;
        }

        public virtual Symbol Intern (string name)
        {
            if (name == null)
                throw new ArgumentNullException ("name");
            Predicate<Symbol> matchString = new Predicate<Symbol> (new StringMatcher (name).MatchesSymbol);
            Symbol probe = this.PresentSymbolList.Find (matchString);
            if (probe != null)
                return probe;
            else {
                foreach (Package u in this.PackageUseList) {
                    probe = u.ExternalSymbolList.Find (matchString);
                    if (probe != null)
                        return probe;
                }
                probe = new Symbol (name, this);
                this.PresentSymbolList.Add (probe);
                return probe;
            }
        }

        static Package ()
        {
            CommonLispPackage = new Package ("COMMON-LISP", new string [] {"CL"});
            CommonLispPackage.Export (CommonLispPackage.Intern ("&ALLOW-OTHER-KEYS"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("&AUX"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("&BODY"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("&ENVIRONMENT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("&KEY"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("&OPTIONAL"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("&REST"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("&WHOLE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("*"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("**"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("***"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("*BREAK-ON-SIGNALS*"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("*COMPILE-FILE-PATHNAME*"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("*COMPILE-FILE-TRUENAME*"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("*COMPILE-PRINT*"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("*COMPILE-VERBOSE*"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("*DEBUG-IO*"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("*DEBUGGER-HOOK*"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("*DEFAULT-PATHNAME-DEFAULTS*"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("*ERROR-OUTPUT*"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("*FEATURES*"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("*GENSYM-COUNTER*"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("*LOAD-PATHNAME*"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("*LOAD-PRINT*"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("*LOAD-TRUENAME*"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("*LOAD-VERBOSE*"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("*MACROEXPAND-HOOK*"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("*MODULES*"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("*PACKAGE*"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("*PRINT-ARRAY*"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("*PRINT-BASE*"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("*PRINT-CASE*"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("*PRINT-CIRCLE*"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("*PRINT-ESCAPE*"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("*PRINT-GENSYM*"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("*PRINT-LENGTH*"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("*PRINT-LEVEL*"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("*PRINT-LINES*"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("*PRINT-MISER-WIDTH*"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("*PRINT-PPRINT-DISPATCH*"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("*PRINT-PRETTY*"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("*PRINT-RADIX*"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("*PRINT-READABLY*"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("*PRINT-RIGHT-MARGIN*"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("*QUERY-IO*"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("*RANDOM-STATE*"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("*READ-BASE*"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("*READ-DEFAULT-FLOAT-FORMAT*"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("*READ-EVAL*"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("*READ-SUPPRESS*"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("*READTABLE*"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("*STANDARD-INPUT*"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("*STANDARD-OUTPUT*"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("*TERMINAL-IO*"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("*TRACE-OUTPUT*"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("+"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("++"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("+++"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("-"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("/"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("//"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("///"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("/="));
            CommonLispPackage.Export (CommonLispPackage.Intern ("1+"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("1-"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("<"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("<="));
            CommonLispPackage.Export (CommonLispPackage.Intern ("="));
            CommonLispPackage.Export (CommonLispPackage.Intern (">"));
            CommonLispPackage.Export (CommonLispPackage.Intern (">="));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ABORT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ABS"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ACONS"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ACOS"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ACOSH"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ADD-METHOD"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ADJOIN"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ADJUST-ARRAY"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ADJUSTABLE-ARRAY-P"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ALLOCATE-INSTANCE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ALPHA-CHAR-P"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ALPHANUMERICP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("AND"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("APPEND"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("APPLY"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("APROPOS"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("APROPOS-LIST"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("AREF"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ARITHMETIC-ERROR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ARITHMETIC-ERROR-OPERANDS"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ARITHMETIC-ERROR-OPERATION"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ARRAY"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ARRAY-DIMENSION"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ARRAY-DIMENSION-LIMIT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ARRAY-DIMENSIONS"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ARRAY-DISPLACEMENT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ARRAY-ELEMENT-TYPE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ARRAY-HAS-FILL-POINTER-P"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ARRAY-IN-BOUNDS-P"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ARRAY-RANK"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ARRAY-RANK-LIMIT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ARRAY-ROW-MAJOR-INDEX"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ARRAY-TOTAL-SIZE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ARRAY-TOTAL-SIZE-LIMIT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ARRAYP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ASH"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ASIN"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ASINH"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ASSERT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ASSOC"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ASSOC-IF"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ASSOC-IF-NOT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ATAN"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ATANH"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ATOM"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("BASE-CHAR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("BASE-STRING"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("BIGNUM"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("BIT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("BIT-AND"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("BIT-ANDC1"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("BIT-ANDC2"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("BIT-EQV"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("BIT-IOR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("BIT-NAND"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("BIT-NOR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("BIT-NOT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("BIT-ORC1"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("BIT-ORC2"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("BIT-VECTOR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("BIT-VECTOR-P"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("BIT-XOR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("BLOCK"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("BOOLE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("BOOLE-1"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("BOOLE-2"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("BOOLE-AND"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("BOOLE-ANDC1"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("BOOLE-ANDC2"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("BOOLE-C1"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("BOOLE-C2"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("BOOLE-CLR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("BOOLE-EQV"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("BOOLE-IOR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("BOOLE-NAND"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("BOOLE-NOR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("BOOLE-ORC1"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("BOOLE-ORC2"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("BOOLE-SET"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("BOOLE-XOR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("BOOLEAN"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("BOTH-CASE-P"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("BOUNDP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("BREAK"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("BROADCAST-STREAM"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("BROADCAST-STREAM-STREAMS"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("BUILT-IN-CLASS"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("BUTLAST"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("BYTE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("BYTE-POSITION"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("BYTE-SIZE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CAAAAR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CAAADR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CAAAR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CAADAR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CAADDR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CAADR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CAAR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CADAAR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CADADR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CADAR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CADDAR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CADDDR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CADDR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CADR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CALL-ARGUMENTS-LIMIT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CALL-METHOD"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CALL-NEXT-METHOD"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CAR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CASE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CATCH"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CCASE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CDAAAR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CDAADR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CDAAR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CDADAR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CDADDR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CDADR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CDAR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CDDAAR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CDDADR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CDDAR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CDDDAR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CDDDDR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CDDDR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CDDR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CDR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CEILING"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CELL-ERROR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CELL-ERROR-NAME"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CERROR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CHANGE-CLASS"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CHAR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CHAR-CODE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CHAR-CODE-LIMIT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CHAR-DOWNCASE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CHAR-EQUAL"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CHAR-GREATERP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CHAR-INT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CHAR-LESSP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CHAR-NAME"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CHAR-NOT-EQUAL"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CHAR-NOT-GREATERP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CHAR-NOT-LESSP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CHAR-UPCASE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CHAR/="));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CHAR<"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CHAR<="));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CHAR="));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CHAR>"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CHAR>="));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CHARACTER"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CHARACTERP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CHECK-TYPE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CIS"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CLASS"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CLASS-NAME"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CLASS-OF"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CLEAR-INPUT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CLEAR-OUTPUT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CLOSE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CLRHASH"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CODE-CHAR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("COERCE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("COMPILATION-SPEED"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("COMPILE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("COMPILE-FILE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("COMPILE-FILE-PATHNAME"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("COMPILED-FUNCTION"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("COMPILED-FUNCTION-P"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("COMPILER-MACRO"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("COMPILER-MACRO-FUNCTION"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("COMPLEMENT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("COMPLEX"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("COMPLEXP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("COMPUTE-APPLICABLE-METHODS"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("COMPUTE-RESTARTS"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CONCATENATE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CONCATENATED-STREAM"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CONCATENATED-STREAM-STREAMS"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("COND"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CONDITION"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CONJUGATE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CONS"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CONSP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CONSTANTLY"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CONSTANTP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CONTINUE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CONTROL-ERROR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("COPY-ALIST"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("COPY-LIST"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("COPY-PPRINT-DISPATCH"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("COPY-READTABLE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("COPY-SEQ"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("COPY-STRUCTURE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("COPY-SYMBOL"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("COPY-TREE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("COS"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("COSH"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("COUNT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("COUNT-IF"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("COUNT-IF-NOT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("CTYPECASE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DEBUG"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DECF"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DECLAIM"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DECLARATION"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DECLARE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DECODE-FLOAT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DECODE-UNIVERSAL-TIME"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DEFCLASS"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DEFCONSTANT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DEFGENERIC"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DEFINE-COMPILER-MACRO"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DEFINE-CONDITION"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DEFINE-METHOD-COMBINATION"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DEFINE-MODIFY-MACRO"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DEFINE-SETF-EXPANDER"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DEFINE-SYMBOL-MACRO"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DEFMACRO"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DEFMETHOD"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DEFPACKAGE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DEFPARAMETER"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DEFSETF"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DEFSTRUCT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DEFTYPE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DEFUN"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DEFVAR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DELETE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DELETE-DUPLICATES"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DELETE-FILE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DELETE-IF"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DELETE-IF-NOT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DELETE-PACKAGE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DENOMINATOR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DEPOSIT-FIELD"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DESCRIBE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DESCRIBE-OBJECT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DESTRUCTURING-BIND"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DIGIT-CHAR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DIGIT-CHAR-P"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DIRECTORY"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DIRECTORY-NAMESTRING"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DISASSEMBLE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DIVISION-BY-ZERO"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DO"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DO*"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DO-ALL-SYMBOLS"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DO-EXTERNAL-SYMBOLS"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DO-SYMBOLS"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DOCUMENTATION"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DOLIST"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DOTIMES"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DOUBLE-FLOAT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DOUBLE-FLOAT-EPSILON"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DOUBLE-FLOAT-NEGATIVE-EPSILON"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DPB"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DRIBBLE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("DYNAMIC-EXTENT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ECASE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ECHO-STREAM"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ECHO-STREAM-INPUT-STREAM"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ECHO-STREAM-OUTPUT-STREAM"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ED"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("EIGHTH"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ELT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ENCODE-UNIVERSAL-TIME"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("END-OF-FILE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ENDP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ENOUGH-NAMESTRING"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ENSURE-DIRECTORIES-EXIST"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ENSURE-GENERIC-FUNCTION"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("EQ"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("EQL"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("EQUAL"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("EQUALP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ERROR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ETYPECASE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("EVAL"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("EVAL-WHEN"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("EVENP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("EVERY"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("EXP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("EXPORT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("EXPT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("EXTENDED-CHAR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FBOUNDP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FCEILING"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FDEFINITION"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FFLOOR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FIFTH"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FILE-AUTHOR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FILE-ERROR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FILE-ERROR-PATHNAME"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FILE-LENGTH"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FILE-NAMESTRING"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FILE-POSITION"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FILE-STREAM"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FILE-STRING-LENGTH"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FILE-WRITE-DATE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FILL"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FILL-POINTER"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FIND"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FIND-ALL-SYMBOLS"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FIND-CLASS"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FIND-IF"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FIND-IF-NOT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FIND-METHOD"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FIND-PACKAGE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FIND-RESTART"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FIND-SYMBOL"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FINISH-OUTPUT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FIRST"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FIXNUM"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FLET"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FLOAT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FLOAT-DIGITS"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FLOAT-PRECISION"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FLOAT-RADIX"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FLOAT-SIGN"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FLOATING-POINT-INEXACT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FLOATING-POINT-INVALID-OPERATION"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FLOATING-POINT-OVERFLOW"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FLOATING-POINT-UNDERFLOW"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FLOATP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FLOOR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FMAKUNBOUND"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FORCE-OUTPUT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FORMAT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FORMATTER"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FOURTH"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FRESH-LINE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FROUND"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FTRUNCATE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FTYPE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FUNCALL"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FUNCTION"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FUNCTION-KEYWORDS"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FUNCTION-LAMBDA-EXPRESSION"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("FUNCTIONP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("GCD"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("GENERIC-FUNCTION"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("GENSYM"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("GENTEMP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("GET"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("GET-DECODED-TIME"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("GET-DISPATCH-MACRO-CHARACTER"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("GET-INTERNAL-REAL-TIME"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("GET-INTERNAL-RUN-TIME"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("GET-MACRO-CHARACTER"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("GET-OUTPUT-STREAM-STRING"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("GET-PROPERTIES"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("GET-SETF-EXPANSION"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("GET-UNIVERSAL-TIME"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("GETF"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("GETHASH"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("GO"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("GRAPHIC-CHAR-P"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("HANDLER-BIND"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("HANDLER-CASE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("HASH-TABLE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("HASH-TABLE-COUNT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("HASH-TABLE-P"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("HASH-TABLE-REHASH-SIZE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("HASH-TABLE-REHASH-THRESHOLD"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("HASH-TABLE-SIZE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("HASH-TABLE-TEST"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("HOST-NAMESTRING"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("IDENTITY"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("IF"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("IGNORABLE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("IGNORE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("IGNORE-ERRORS"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("IMAGPART"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("IMPORT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("IN-PACKAGE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("INCF"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("INITIALIZE-INSTANCE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("INLINE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("INPUT-STREAM-P"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("INSPECT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("INTEGER"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("INTEGER-DECODE-FLOAT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("INTEGER-LENGTH"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("INTEGERP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("INTERACTIVE-STREAM-P"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("INTERN"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("INTERNAL-TIME-UNITS-PER-SECOND"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("INTERSECTION"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("INVALID-METHOD-ERROR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("INVOKE-DEBUGGER"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("INVOKE-RESTART"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("INVOKE-RESTART-INTERACTIVELY"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ISQRT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("KEYWORD"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("KEYWORDP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LABELS"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LAMBDA"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LAMBDA-LIST-KEYWORDS"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LAMBDA-PARAMETERS-LIMIT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LAST"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LCM"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LDB"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LDB-TEST"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LDIFF"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LEAST-NEGATIVE-DOUBLE-FLOAT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LEAST-NEGATIVE-LONG-FLOAT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LEAST-NEGATIVE-SHORT-FLOAT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LEAST-NEGATIVE-SINGLE-FLOAT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LEAST-POSITIVE-DOUBLE-FLOAT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LEAST-POSITIVE-LONG-FLOAT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LEAST-POSITIVE-NORMALIZED-LONG-FLOAT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LEAST-POSITIVE-SHORT-FLOAT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LEAST-POSITIVE-SINGLE-FLOAT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LENGTH"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LET"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LET*"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LISP-IMPLEMENTATION-TYPE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LISP-IMPLEMENTATION-VERSION"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LIST"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LIST*"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LIST-ALL-PACKAGES"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LIST-LENGTH"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LISTEN"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LISTP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LOAD"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LOAD-LOGICAL-PATHNAME-TRANSLATIONS"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LOAD-TIME-VALUE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LOCALLY"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LOG"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LOGAND"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LOGANDC1"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LOGANDC2"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LOGBITP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LOGCOUNT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LOGEQV"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LOGICAL-PATHNAME"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LOGICAL-PATHNAME-TRANSLATIONS"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LOGIOR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LOGNAND"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LOGNOR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LOGNOT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LOGORC1"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LOGORC2"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LOGTEST"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LOGXOR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LONG-FLOAT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LONG-FLOAT-EPSILON"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LONG-FLOAT-NEGATIVE-EPSILON"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LONG-SITE-NAME"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LOOP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LOOP-FINISH"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("LOWER-CASE-P"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MACHINE-INSTANCE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MACHINE-TYPE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MACHINE-VERSION"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MACRO-FUNCTION"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MACROEXPAND"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MACROEXPAND-1"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MACROLET"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MAKE-ARRAY"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MAKE-BROADCAST-STREAM"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MAKE-CONCATENATED-STREAM"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MAKE-CONDITION"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MAKE-DISPATCH-MACRO-CHARACTER"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MAKE-ECHO-STREAM"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MAKE-HASH-TABLE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MAKE-INSTANCE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MAKE-INSTANCES-OBSOLETE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MAKE-LIST"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MAKE-LOAD-FORM"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MAKE-LOAD-FORM-SAVING-SLOTS"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MAKE-METHOD"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MAKE-PACKAGE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MAKE-PATHNAME"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MAKE-RANDOM-STATE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MAKE-SEQUENCE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MAKE-STRING"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MAKE-STRING-INPUT-STREAM"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MAKE-STRING-OUTPUT-STREAM"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MAKE-SYMBOL"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MAKE-SYNONYM-STREAM"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MAKE-TWO-WAY-STREAM"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MAKUNBOUND"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MAP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MAP-INTO"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MAPC"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MAPCAN"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MAPCAR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MAPCON"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MAPHASH"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MAPL"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MAPLIST"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MASK-FIELD"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MAX"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MEMBER"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MEMBER-IF"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MEMBER-IF-NOT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MERGE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MERGE-PATHNAMES"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("METHOD"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("METHOD-COMBINATION"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("METHOD-COMBINATION-ERROR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("METHOD-QUALIFIERS"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MIN"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MINUSP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MISMATCH"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MOD"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MOST-NEGATIVE-DOUBLE-FLOAT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MOST-NEGATIVE-FIXNUM"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MOST-NEGATIVE-LONG-FLOAT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MOST-NEGATIVE-SHORT-FLOAT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MOST-NEGATIVE-SINGLE-FLOAT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MOST-POSITIVE-DOUBLE-FLOAT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MOST-POSITIVE-FIXNUM"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MOST-POSITIVE-LONG-FLOAT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MOST-POSITIVE-SHORT-FLOAT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MOST-POSITIVE-SINGLE-FLOAT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MUFFLE-WARNING"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MULTIPLE-VALUE-BIND"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MULTIPLE-VALUE-CALL"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MULTIPLE-VALUE-LIST"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MULTIPLE-VALUE-PROG1"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MULTIPLE-VALUE-SETQ"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("MULTIPLE-VALUES-LIMIT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("NAME-CHAR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("NAMESTRING"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("NBUTLAST"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("NCONC"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("NEXT-METHOD-P"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("NIL"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("NINTERSECTION"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("NINTH"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("NO-APPLICABLE-METHOD"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("NO-NEXT-METHOD"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("NOT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("NOTANY"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("NOTEVERY"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("NOTINLINE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("NRECONC"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("NREVERSE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("NSET-DIFFERENCE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("NSET-EXCLUSIVE-OR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("NSTRING-CAPITALIZE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("NSTRING-DOWNCASE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("NSTRING-UPCASE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("NSUBLIS"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("NSUBST"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("NSUBST-IF"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("NSUBST-IF-NOT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("NSUBSTITUTE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("NSUBSTITUTE-IF"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("NSUBSTITUTE-IF-NOT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("NTH"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("NTH-VALUE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("NTHCDR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("NULL"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("NUMBER"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("NUMBERP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("NUMERATOR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("NUNION"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ODDP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("OPEN"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("OPEN-STREAM-P"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("OPTIMIZE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("OR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("OTHERWISE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("OUTPUT-STREAM-P"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PACKAGE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PACKAGE-ERROR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PACKAGE-ERROR-PACKAGE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PACKAGE-NAME"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PACKAGE-NICKNAMES"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PACKAGE-SHADOWING-SYMBOLS"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PACKAGE-USE-LIST"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PACKAGE-USED-BY-LIST"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PACKAGEP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PAIRLIS"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PARSE-ERROR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PARSE-INTEGER"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PARSE-NAMESTRING"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PATHNAME"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PATHNAME-DEVICE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PATHNAME-DIRECTORY"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PATHNAME-HOST"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PATHNAME-MATCH-P"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PATHNAME-NAME"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PATHNAME-TYPE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PATHNAME-VERSION"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PATHNAMEP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PEEK-CHAR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PHASE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PI"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PLUSP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("POP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("POSITION"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("POSITION-IF"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("POSITION-IF-NOT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PPRINT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PPRINT-DISPATCH"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PPRINT-EXIT-IF-LIST-EXHAUSTED"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PPRINT-FILL"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PPRINT-INDENT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PPRINT-LINEAR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PPRINT-LOGICAL-BLOCK"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PPRINT-NEWLINE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PPRINT-POP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PPRINT-TAB"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PPRINT-TABULAR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PRIN1"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PRIN1-TO-STRING"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PRINC"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PRINC-TO-STRING"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PRINT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PRINT-NOT-READABLE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PRINT-NOT-READABLE-OBJECT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PRINT-OBJECT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PRINT-UNREADABLE-OBJECT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PROBE-FILE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PROCLAIM"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PROG"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PROG*"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PROG1"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PROG2"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PROGN"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PROGRAM-ERROR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PROGV"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PROVIDE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PSETF"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PSETQ"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PUSH"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("PUSHNEW"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("QUOTE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("RANDOM"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("RANDOM-STATE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("RANDOM-STATE-P"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("RASSOC"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("RASSOC-IF"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("RASSOC-IF-NOT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("RATIO"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("RATIONAL"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("RATIONALIZE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("RATIONALP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("READ"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("READ-BYTE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("READ-CHAR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("READ-CHAR-NO-HANG"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("READ-DELIMITED-LIST"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("READ-FROM-STRING"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("READ-LINE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("READ-PRESERVING-WHITESPACE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("READ-SEQUENCE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("READER-ERROR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("READTABLE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("READTABLE-CASE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("READTABLEP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("REAL"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("REALP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("REALPART"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("REDUCE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("REINITIALIZE-INSTANCE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("REM"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("REMF"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("REMHASH"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("REMOVE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("REMOVE-DUPLICATES"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("REMOVE-IF"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("REMOVE-IF-NOT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("REMOVE-METHOD"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("REMPROP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("RENAME-FILE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("RENAME-PACKAGE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("REPLACE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("REQUIRE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("REST"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("RESTART"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("RESTART-BIND"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("RESTART-CASE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("RESTART-NAME"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("RETURN"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("RETURN-FROM"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("REVAPPEND"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("REVERSE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ROOM"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ROTATEF"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ROUND"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ROW-MAJOR-AREF"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("RPLACA"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("RPLACD"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SAFETY"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SATISFIES"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SBIT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SCALE-FLOAT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SCHAR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SEARCH"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SECOND"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SEQUENCE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SERIOUS-CONDITION"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SET"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SET-DIFFERENCE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SET-DISPATCH-MACRO-CHARACTER"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SET-EXCLUSIVE-OR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SET-MACRO-CHARACTER"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SET-PPRINT-DISPATCH"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SET-SYNTAX-FROM-CHAR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SETF"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SETQ"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SEVENTH"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SHADOW"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SHADOWING-IMPORT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SHARED-INITIALIZE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SHIFTF"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SHORT-FLOAT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SHORT-FLOAT-EPSILON"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SHORT-FLOAT-NEGATIVE-EPSILON"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SHORT-SITE-NAME"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SIGNAL"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SIGNED-BYTE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SIGNUM"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SIMPLE-ARRAY"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SIMPLE-BASE-STRING"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SIMPLE-BIT-VECTOR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SIMPLE-BIT-VECTOR-P"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SIMPLE-CONDITION"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SIMPLE-CONDITION-FORMAT-ARGUMENTS"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SIMPLE-CONDITION-FORMAT-CONTROL"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SIMPLE-ERROR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SIMPLE-STRING"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SIMPLE-STRING-P"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SIMPLE-TYPE-ERROR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SIMPLE-VECTOR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SIMPLE-VECTOR-P"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SIMPLE-WARNING"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SIN"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SINGLE-FLOAT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SINGLE-FLOAT-EPSILON"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SINGLE-FLOAT-NEGATIVE-EPSILON"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SINH"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SIXTH"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SLEEP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SLOT-BOUNDP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SLOT-EXISTS-P"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SLOT-MAKUNBOUND"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SLOT-MISSING"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SLOT-UNBOUND"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SLOT-VALUE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SOFTWARE-TYPE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SOFTWARE-VERSION"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SOME"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SORT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SPACE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SPECIAL"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SPECIAL-OPERATOR-P"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SPEED"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SQRT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("STABLE-SORT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("STANDARD"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("STANDARD-CHAR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("STANDARD-CHAR-P"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("STANDARD-CLASS"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("STANDARD-GENERIC-FUNCTION"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("STANDARD-METHOD"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("STANDARD-OBJECT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("STEP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("STORAGE-CONDITION"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("STORE-VALUE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("STREAM"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("STREAM-ELEMENT-TYPE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("STREAM-ERROR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("STREAM-ERROR-STREAM"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("STREAM-EXTERNAL-FORMAT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("STREAMP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("STRING"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("STRING-CAPITALIZE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("STRING-DOWNCASE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("STRING-EQUAL"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("STRING-GREATERP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("STRING-LEFT-TRIM"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("STRING-LESSP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("STRING-NOT-EQUAL"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("STRING-NOT-GREATERP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("STRING-NOT-LESSP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("STRING-RIGHT-TRIM"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("STRING-STREAM"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("STRING-TRIM"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("STRING-UPCASE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("STRING/="));
            CommonLispPackage.Export (CommonLispPackage.Intern ("STRING<"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("STRING<="));
            CommonLispPackage.Export (CommonLispPackage.Intern ("STRING="));
            CommonLispPackage.Export (CommonLispPackage.Intern ("STRING>"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("STRING>="));
            CommonLispPackage.Export (CommonLispPackage.Intern ("STRINGP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("STRUCTURE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("STRUCTURE-CLASS"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("STRUCTURE-OBJECT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("STYLE-WARNING"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SUBLIS"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SUBSEQ"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SUBSETP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SUBST"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SUBST-IF"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SUBST-IF-NOT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SUBSTITUTE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SUBSTITUTE-IF"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SUBSTITUTE-IF-NOT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SUBTYPEP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SVREF"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SXHASH"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SYMBOL"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SYMBOL-FUNCTION"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SYMBOL-MACROLET"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SYMBOL-NAME"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SYMBOL-PACKAGE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SYMBOL-PLIST"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SYMBOL-VALUE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SYMBOLP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SYNONYM-STREAM"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("SYNONYM-STREAM-SYMBOL"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("T"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("TAGBODY"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("TAILP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("TAN"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("TANH"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("TENTH"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("TERPRI"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("THE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("THIRD"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("THROW"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("TIME"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("TRACE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("TRANSLATE-LOGICAL-PATHNAME"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("TRANSLATE-PATHNAME"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("TREE-EQUAL"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("TRUENAME"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("TRUNCATE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("TWO-WAY-STREAM"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("TWO-WAY-STREAM-INPUT-STREAM"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("TWO-WAY-STREAM-OUTPUT-STREAM"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("TYPE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("TYPE-ERROR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("TYPE-ERROR-DATUM"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("TYPE-ERROR-EXPECTED-TYPE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("TYPE-OF"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("TYPECASE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("TYPEP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("UNBOUND-SLOT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("UNBOUND-SLOT-INSTANCE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("UNBOUND-VARIABLE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("UNDEFINED-FUNCTION"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("UNEXPORT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("UNINTERN"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("UNION"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("UNLESS"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("UNREAD-CHAR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("UNSIGNED-BYTE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("UNTRACE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("UNUSE-PACKAGE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("UNWIND-PROTECT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("UPDATE-INSTANCE-FOR-DIFFERENT-CLASS"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("UPDATE-INSTANCE-FOR-REDEFINED-CLASS"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("UPGRADED-ARRAY-ELEMENT-TYPE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("UPGRADED-COMPLEX-PART-TYPE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("UPPER-CASE-P"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("USE-PACKAGE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("USE-VALUE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("USER-HOMEDIR-PATHNAME"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("VALUES"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("VALUES-LIST"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("VARIABLE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("VECTOR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("VECTOR-POP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("VECTOR-PUSH"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("VECTOR-PUSH-EXTEND"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("VECTORP"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("WARN"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("WARNING"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("WHEN"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("WILD-PATHNAME-P"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("WITH-ACCESSORS"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("WITH-COMPILATION-UNIT"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("WITH-CONDITION-RESTARTS"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("WITH-HASH-TABLE-ITERATOR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("WITH-INPUT-FROM-STRING"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("WITH-OPEN-FILE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("WITH-OPEN-STREAM"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("WITH-OUTPUT-TO-STRING"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("WITH-PACKAGE-ITERATOR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("WITH-SIMPLE-RESTART"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("WITH-SLOTS"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("WITH-STANDARD-IO-SYNTAX"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("WRITE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("WRITE-BYTE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("WRITE-CHAR"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("WRITE-LINE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("WRITE-SEQUENCE"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("WRITE-STRING"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("WRITE-TO-STRING"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("Y-OR-N-P"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("YES-OR-NO-P"));
            CommonLispPackage.Export (CommonLispPackage.Intern ("ZEROP"));
        }
    }

    public class KeywordPackage : Package
    {
        public KeywordPackage ()
            : base ("KEYWORD",  new string [] { "" })
        {
        }
        public override Symbol Intern (string name)
        {
            Predicate<Symbol> matchString = new Predicate<Symbol> (new StringMatcher (name).MatchesSymbol);
            Symbol probe = this.PresentSymbolList.Find (matchString);
            if (probe == null) {

                probe = new Keyword (name);
                this.PresentSymbolList.Add (probe);
                this.ExternalSymbolList.Add (probe);
            }
            return probe;
        }
    }

}

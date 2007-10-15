using System;
using System.Collections;
using System.IO;

namespace Lisp
{
    public delegate object Thunk ();

    [CLSCompliant(true)]
    public sealed class CL
    {
        // Static holder.
        CL ()
        {
        }

        static readonly Symbol QuoteList = Package.CommonLisp.FindSymbol ("LIST");

        // *PACKAGE*
        private static readonly ValueCell<Lisp.Environment> globalEnvironment
            = new ValueCell<Lisp.Environment> (Lisp.Environment.Default);

        [ThreadStatic]
        private static ValueCell<Lisp.Environment> environment;

        private static Lisp.Environment Environment
        {
            get
            {
                if (environment == null)
                    environment = globalEnvironment;
                return environment.Value;
            }

            set
            {
                if (environment == null)
                    environment = globalEnvironment;
                environment.Value = value;
            }
        }

        // *PACKAGE*
        private static readonly ValueCell<Package> globalPackage
            = new ValueCell<Package> (Package.CommonLispUser);

        [ThreadStatic]
        private static ValueCell<Package> package;

        public static Package Package
        {
            get
            {
                if (package == null)
                    package = globalPackage;
                return package.Value;
            }

            set
            {
                if (package == null)
                    package = globalPackage;
                package.Value = value;
            }
        }

        // *READ-SUPPRESS*
        private static readonly ValueCell<bool> globalReadSuppress
                = new ValueCell<bool> (false);

        [ThreadStatic]
        static ValueCell<bool> readSuppress;

        public static bool ReadSuppress
        {
            get
            {
                if (readSuppress == null)
                    readSuppress = globalReadSuppress;
                return readSuppress.Value;
            }
            set
            {
                if (readSuppress == null)
                    readSuppress = globalReadSuppress;
                readSuppress.Value = value;
            }
        }

        public static object BindReadSuppress (bool value, Thunk thunk)
        {
            ValueCell<bool> oldReadSuppress = readSuppress;
            try {
                readSuppress = new ValueCell<bool> (value);
                return thunk ();
            }
            finally {
                readSuppress = oldReadSuppress;
            }
        }

        // *READTABLE*
        private static readonly ValueCell<Readtable> globalReadtable
            = new ValueCell<Readtable> (new Readtable ());

        [ThreadStatic]
        private static ValueCell<Readtable> readtable;

        public static Readtable Readtable
        {
            get
            {
                if (readtable == null)
                    readtable = globalReadtable;
                return readtable.Value;
            }

            set
            {
                if (readtable == null)
                    readtable = globalReadtable;
                readtable.Value = value;
            }
        }

        public static StandardObject AddMethod
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return CLOS.AddMethod;
            }
        }

	// Append
        public static object Append (object left, object right)
        {
             return CL.Reconc (CL.Reverse (left), right);
        }

        // APPLY
        public static object Apply (object op, params object [] rands)
        {
            if (rands.Length == 0)
               return ResolveFunctionSpecifier (op).DynamicInvoke ();
              Cons arglist = (Cons) rands [rands.Length - 1];
            for (int i = rands.Length - 2; i > -1; i--)
               arglist = CL.Cons (rands [i], arglist);
            int limit = CL.Length (arglist);
            object [] argarray = new object [limit];

            for (int i = 0; i < limit; i++) {
                argarray [i] = arglist.Car;
                arglist = (Cons) arglist.Cdr;
            }
            return ResolveFunctionSpecifier (op).DynamicInvoke ((object)argarray);

        }


        public static Cons Assq (object element, Cons alist)
        {
            Cons ec = null;
            while (alist != null) {
              object entry = alist.Car;
              ec = entry as Cons;
              if (ec == null) throw new NotImplementedException();
              if (ec.Car == element) break;
              object tail = alist.Cdr;
              if (tail == null)
                  return null;
              Cons ttail = tail as Cons;
              if (ttail == null) throw new NotImplementedException();
              alist = ttail;
              }
            return ec;
        }

        public static Cons Assq (object element, object alist)
        {
            if (alist == null) return null;
            Cons ac = alist as Cons;
            if (ac == null) throw new NotImplementedException();
            return Assq (element, ac);
        }

        // CADR
        public static object Cadr (Cons thing)
        {
            return (thing == null) ? null : CL.Car (thing.Cdr);
        }


        public static object Cadr (object thing)
        {
            return (thing == null) ? null : CL.Car (CL.Cdr (thing));
        }

        // CADDR
        public static object Caddr (Cons thing)
        {
            return (thing == null) ? null : CL.Car (CL.Cdr (thing.Cdr));
        }


        public static object Caddr (object thing)
        {
            return (thing == null) ? null : CL.Car (CL.Cdr (CL.Cdr (thing)));
        }

        // CADDDR
        public static object Cadddr (Cons thing)
        {
            return (thing == null) ? null : CL.Car (CL.Cdr (CL.Cdr (thing.Cdr)));
        }


        public static object Cadddr (object thing)
        {
            return (thing == null) ? null : CL.Car (CL.Cdr (CL.Cdr (CL.Cdr (thing))));
        }


        // CAR
        public static object Car (Cons thing)
        {
            return (thing == null) ? null : thing.Car;
        }

        public static object Car (object thing)
        {
            if (thing == null)
                return null;
            Cons pair = thing as Cons;
            if (pair != null)
                return pair.Car;
            throw new NotFiniteNumberException ();
        }

        // CDAR
        public static object Cdar (Cons thing)
        {
            return (thing == null) ? null : CL.Cdr (thing.Car);
        }

        public static object Cdar (object thing)
        {
            return (thing == null) ? null : CL.Cdr (CL.Car(thing));
        }


        // CDR
        public static object Cdr (Cons thing)
        {
            return (thing == null) ? null : thing.Cdr;
        }

        public static object Cdr (object thing)
        {
            if (thing == null)
                return null;
            Cons pair = thing as Cons;
            if (pair != null)
                return pair.Cdr;
            throw new NotImplementedException ("CDR of non-list");
        }

        // CLASS-NAME
        public static StandardObject ClassName
        {
            get
            {
                return CLOS.ClassName;
            }
        }

        // CONS
        public static Cons Cons (object car, object cdr)
        {
            return new Cons (car, cdr);
        }

        public static StandardObject EnsureGenericFunction
        {
            get
            {
                throw new NotImplementedException ();
                //return CLOS.EnsureGenericFunction;
            }
        }

        // EVAL
        // Actually a simple evaluator, not the `real thing' yet.
        static public object Eval (object form)
        {
            return SimpleEvaluator.Eval (form);
        }

        static public bool Eq (object left, object right)
        {
            return Object.ReferenceEquals (left, right);
        }

        static public bool Eql (object left, object right)
        {
            if (left.Equals (right))
                return true;
            throw new NotImplementedException ();
        }

        static public Package FindPackage (string name)
        {
            throw new NotImplementedException ();
        }

        static public object Identity (object item)
        {
            return item;
        }

        // INTERN
        static public Symbol Intern (string s)
        {
            if (s == null)
                throw new ArgumentNullException ("s");
            return Intern (s, CL.Package);
        }

        static public Symbol Intern (string s, Package package)
        {
            if (s == null)
                throw new ArgumentNullException ("s");
            if (package == null)
                throw new ArgumentNullException ("package");
            return package.Intern (s);
        }

        static public Cons List () {
            return null;
        }

        static public Cons List (object arg0, params object [] restArguments) {
                   return new Cons (arg0, Lisp.Cons.SubvectorToList (restArguments, 0, restArguments.Length));
        }

        static public int Length (object obj)
        {
            if (obj == null)
                return 0;
            ICollection c = obj as ICollection;
            if (c != null)
                return c.Count;
            else {
                throw new NotImplementedException ();
            }
        }

        public static StandardObject MakeInstance
        {
            get
            {
                return CLOS.MakeInstance;
            }
        }

        // MAKE-STRING-INPUT-STREAM
        static public StringReader MakeStringInputStream (string s)
        {
            if (s == null)
                throw new ArgumentNullException ("s");
            return new StringReader (s);
        }

        static public StringReader MakeStringInputStream (string s, int start)
        {
            if (s == null)
                throw new ArgumentNullException ("s");
            return new StringReader (s.Substring (start));
        }

        static public StringReader MakeStringInputStream (string s, int start, int end)
        {
            if (s == null)
                throw new ArgumentNullException ("s");
            return new StringReader (s.Substring(start,end - start));
        }

        static object MapForEffect (object functionSpecifier, object [] sequences)
        {
            throw new NotImplementedException ();
        }

        static Cons MapToList (object functionSpecifier, object [] sequences)
        {
            switch (sequences.Length) {
                case 1:
                    return MapToList1 (functionSpecifier, sequences [0]);
                default:
                    throw new NotImplementedException ();
            }
        }

        static Cons MapToList1 (object functionSpecifier, object sequence)
        {
            if (sequence == null)
                return null;
            object [] sa = sequence as object [];
            if (sa != null)
                return MapVectorToList1 (functionSpecifier, sa);
            else {
                Cons sc = sequence as Cons;
                if (sc != null)
                    return MapListToList1 (functionSpecifier, sc);
                else {
                    throw new NotImplementedException ();
                }
            }
        }

        static Cons MapListToList1 (object functionSpecifier, Cons sequence)
        {
            Delegate function = ResolveFunctionSpecifier (functionSpecifier);
            Cons answer = null;
            while (sequence != null) {
                answer = new Cons (function.DynamicInvoke (sequence.Car), answer);
                object temp = sequence.Cdr;
                if (temp == null)
                    break;
                Cons next = temp as Cons;
                if (next == null)
                    throw new NotImplementedException ();
                sequence = next;
            }
            return (Cons) CL.Reverse (answer);
        }

        static Cons MapVectorToList1 (object functionSpecifier, object [] sequence)
        {
            Delegate function = ResolveFunctionSpecifier (functionSpecifier);
            Cons answer = null;
            foreach (object element in sequence)
                answer = Cons (function.DynamicInvoke (element), answer);
            return (Cons) CL.Reverse (answer);
        }

        static public Cons Memq (object item, object list)
        {
            if (list == null) return null;
            Cons pair = list as Cons;
            if (pair == null) throw new NotImplementedException();
            if (item == pair.Car)
                return pair;
            else
                return Memq (item, pair.Cdr);
        }

        static Delegate ResolveFunctionSpecifier (object functionSpecifier)
        {
            Delegate fasd = functionSpecifier as Delegate;
            if (fasd != null)
                return fasd;
            else
                throw new NotImplementedException ();
        }

        static public object Map (object sequenceTypeSpecifier, object functionSpecifier, params object [] sequences)
        {
            if (functionSpecifier == null)
                throw new ArgumentNullException ("functionSpecifier");
            else if (sequenceTypeSpecifier == null)
                return MapForEffect (functionSpecifier, sequences);
            else if (sequenceTypeSpecifier == QuoteList)
                return MapToList (functionSpecifier, sequences);
            else
                throw new NotImplementedException ();
        }

        delegate bool EqualityTest (object left, object right);
        delegate object KeySelector (object item);

        static int Position (object item, Cons list)
        {
            int answer = 0;
            while (list != null) {
                if (item == list.Car)
                    break;

                object tail = list.Cdr;
                if (tail == null)
                    return -1;
                Cons tail1 = tail as Cons;
                if (tail1 == null)
                    throw new NotImplementedException ();
                answer += 1;
                list = tail1;
            }
            return answer;
        }

        static public int Position (object item, object sequence)
        {
            if (sequence == null)
                return -1;
            Cons list = sequence as Cons;
            if (list != null) {
                return Position (item, list);
            }
            return Position (item, sequence,
                             KW.FromEnd, false,
                             KW.Key, new KeySelector (CL.Identity),
                             KW.Test, new EqualityTest (CL.Eql),
                             KW.Start, 0,
                             KW.End, CL.Length (sequence));
        }

        static public int Position (object item, object sequence,
                                    object key0, object val0,
                                    object key1, object val1,
                                    object key2, object val2,
                                    object key3, object val3,
                                    object key4, object val4)
        {
            throw new NotImplementedException ();
        }

        static public object Read ()
        {
            throw new NotImplementedException ("Read");
        }

        static public object Read (TextReader inputStream)
        {
            if (inputStream == null)
                throw new ArgumentNullException ("inputStream");
            return Read (inputStream, true, null, false);
        }

        static public object Read (TextReader inputStream, bool isEofError)
        {
            if (inputStream == null)
                throw new ArgumentNullException ("inputStream");
            return Read (inputStream, isEofError, null, false);
        }

        static public object Read (TextReader inputStream, bool isEofError, object eofValue)
        {
            if (inputStream == null)
                throw new ArgumentNullException ("inputStream");
            return Read (inputStream, isEofError, eofValue, false);
        }

        static public object Read (TextReader inputStream, bool isEofError, object eofValue, bool isRecursive)
        {
            if (inputStream == null)
                throw new ArgumentNullException ("inputStream");
            ReaderStep step = new InitialReaderStep (inputStream, isEofError, eofValue);
            while (!step.isFinal) {
                step = step.NextStep ();
            }
            return step.Result;
        }

        static public object ReadFromString (string source)
        {
            if (source == null)
                throw new ArgumentNullException ("source");
            return ReadFromString1 (source, true, null, 0, source.Length, false);
        }

        static public object ReadFromString (string source, bool isEofError)
        {
            if (source == null)
                throw new ArgumentNullException ("source");
            return ReadFromString1 (source, isEofError, null, 0, source.Length, false);
        }

        static public object ReadFromString (string source, bool isEofError, object eofValue)
        {
            if (source == null)
                throw new ArgumentNullException ("source");
            return ReadFromString1 (source, isEofError, eofValue, 0, source.Length, false);
        }

        static public object ReadFromString (string source, bool isEofError, object eofValue, params object [] keywordArguments)
        {
            Utility.Ignore (source);
            Utility.Ignore (isEofError);
            Utility.Ignore (eofValue);
            Utility.Ignore (keywordArguments);
            throw new NotImplementedException ("ReadFromString");
        }

        static object ReadFromString1 (string source, bool isEofError, object eofValue, int start, int end, bool preserveWhitespace)
        {
            TextReader stream = MakeStringInputStream (source, start, end);
            return preserveWhitespace 
                ? ReadPreservingWhitespace (stream, isEofError, eofValue)
                : Read (stream, isEofError, eofValue);
	    }
 
        static public object ReadPreservingWhitespace ()
        {
            throw new NotImplementedException ("Read");
        }

        static public object ReadPreservingWhitespace (TextReader inputStream)
        {
            Utility.Ignore (inputStream);
            throw new NotImplementedException ("ReadPreservingWhitespace");
        }

        static public object ReadPreservingWhitespace (TextReader inputStream, bool isEofError)
        {
            Utility.Ignore (inputStream);
            Utility.Ignore (isEofError);
            throw new NotImplementedException ("ReadPreservingWhitespace");
        }

        static public object ReadPreservingWhitespace (TextReader inputStream, bool isEofError, object eofValue)
        {
            Utility.Ignore (inputStream);
            Utility.Ignore (isEofError);
            Utility.Ignore (eofValue);
            throw new NotImplementedException ("ReadPreservingWhitespace");
        }

        static public object ReadPreservingWhitespace (TextReader inputStream, bool isEofError, object eofValue, bool isRecursive)
        {
            Utility.Ignore (inputStream);
            Utility.Ignore (isEofError);
            Utility.Ignore (eofValue);
            Utility.Ignore (isRecursive);
            throw new NotImplementedException ("ReadPreservingWhitespace");
        }

        // RECONC
        static public object Reconc (object tail, object head)
        {
            if (tail == null)
                return head;
            Cons ctail = tail as Cons;
            if (ctail != null)
                // yeah I'll fix it...
                return Reconc (ctail.Cdr, new Cons (ctail.Car, head));
            else 
                throw new NotImplementedException ();
        }
        static public Cons Reverse (Cons list)
        {
            return (Cons) CL.Reconc (list, null);
        }

        static public object Reverse (object list)
        {
            return CL.Reconc (list, null);
        }

        static public Cons RemoveIf (Delegate predicate, Cons sequence)
        {
            if (sequence == null)
                return null;
            else if ((bool) predicate.DynamicInvoke (sequence.Car))
                return RemoveIf (predicate, (Cons) sequence.Cdr);
            else
                return new Cons (sequence.Car, RemoveIf (predicate, (Cons) sequence.Cdr));
        }

        static public object RemoveIf (object predicate, object sequence)
        {
            if (sequence == null)
                return null;
            Cons seq = sequence as Cons;
            if (seq != null)
                return RemoveIf (ResolveFunctionSpecifier (predicate), seq);
            else
            throw new NotImplementedException ("RemoveIf");
        }

        // SYMBOL-FUNCTION
        static public Delegate SymbolFunction (object sym)
        {
            Symbol sym1 = sym as Symbol;
            if (sym1 != null)
                return SymbolFunction (sym1);
            else
                throw new ArgumentException ("wrong type", "sym");
        }

        static public Delegate SymbolFunction (Symbol sym)
        {
            return CL.Environment.SymbolFunction (sym);
        }

        static public object SymbolValue (object sym)
        {
            return CL.Environment.SymbolValue ((Symbol) sym);
        }

        static public object SetSymbolValue (object sym, object value)
        {
            Symbol realSymbol = sym as Symbol;
            return SetSymbolValue (realSymbol, value);
        }

        static public object SetSymbolValue (Symbol sym, object value)
        {
            return CL.Environment.Setq (sym, value);
        }
    }
}

using System;
using System.Collections;
using System.IO;

namespace Lisp
{
    public delegate object Thunk ();

    [CLSCompliant (true)]
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

        public static StandardInstance AddMethod
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

        public static ConsList<T> Append<T> (ConsList<T> left, ConsList<T> right)
        {
            return CL.Reconc<T> (CL.Reverse<T> (left), right);
        }

        // APPLY
        public static object Apply (object op, params object [] operands)
        {
            if (operands == null)
                return ResolveFunctionSpecifier (op).DynamicInvoke ();
            else if (operands.Length == 0)
                return ResolveFunctionSpecifier (op).DynamicInvoke ();
            Cons arglist = (Cons) operands [operands.Length - 1];
            for (int i = operands.Length - 2; i > -1; i--)
                arglist = CL.Cons (operands [i], arglist);
            int limit = CL.Length (arglist);
            object [] argarray = new object [limit];

            for (int i = 0; i < limit; i++) {
                argarray [i] = arglist.Car;
                arglist = (Cons) arglist.Cdr;
            }
            return ResolveFunctionSpecifier (op).DynamicInvoke ((object) argarray);

        }


        public static Cons Assq (object element, Cons alist)
        {
            Cons ec = null;
            while (alist != null) {
                object entry = alist.Car;
                ec = entry as Cons;
                if (ec == null)
                    throw new NotImplementedException ();
                if (ec.Car == element)
                    break;
                object tail = alist.Cdr;
                if (tail == null)
                    return null;
                Cons ttail = tail as Cons;
                if (ttail == null)
                    throw new NotImplementedException ();
                alist = ttail;
            }
            return ec;
        }

        public static Cons Assq (object element, object alist)
        {
            if (alist == null)
                return null;
            Cons ac = alist as Cons;
            if (ac == null)
                throw new NotImplementedException ();
            return Assq (element, ac);
        }

        public static object Caar (Cons thing)
        {
            return (thing == null) ? null : CL.Car (thing.Car);
        }

        public static object Caar (object thing)
        {
            return (thing == null) ? null : CL.Car (CL.Car (thing));
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
            throw new NotImplementedException ("wrong type argument");
        }

        // CDAR
        public static object Cdar (Cons thing)
        {
            return (thing == null) ? null : CL.Cdr (thing.Car);
        }

        public static object Cdar (object thing)
        {
            return (thing == null) ? null : CL.Cdr (CL.Car (thing));
        }


        // CDDR
        public static object Cddr (Cons thing)
        {
            return (thing == null) ? null : CL.Cdr (thing.Cdr);
        }

        public static object Cddr (object thing)
        {
            return (thing == null) ? null : CL.Cdr (CL.Cdr (thing));
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
            throw new NotImplementedException ("wrong type argument");
        }

        // CLASS-NAME
        public static StandardInstance ClassName
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

        public static StandardInstance EnsureGenericFunction
        {
            get
            {
                //throw new NotImplementedException ();
                return CLOS.EnsureGenericFunction;
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
            if (left == null) return right == null;
            else if (right == null) return false;
            else if (left.Equals (right))
                return true;
            else if (left is Symbol || right is Symbol)
                return false;
            throw new NotImplementedException ();
        }

        static public bool Eql<T> (T left, T right)
        {
            return Object.ReferenceEquals (left, right);
        }

        static public TItem Find<TItem, TKey> (TKey item, System.Collections.Generic.ICollection<TItem> elements, object key0, object arg0)
        {
            return Find<TItem, TKey> (item, elements, new object [] { key0, arg0 });
        }

        static TItem Find<TItem, Tkey> (Tkey item, System.Collections.Generic.ICollection<TItem> sequence, object [] arguments)
        {
            KeywordArgument<bool> fromEnd = new KeywordArgument<bool> (KW.FromEnd);
            KeywordArgument<object> test = new KeywordArgument<object> (KW.Test);
            KeywordArgument<int> start = new KeywordArgument<int> (KW.Start);
            KeywordArgument<int> end = new KeywordArgument<int> (KW.End);
            KeywordArgument<object> key = new KeywordArgument<object> (KW.Key);

            KeywordArgumentBase.ProcessKeywordArguments (
                new KeywordArgumentBase [] { fromEnd, test, start, end, key },
                arguments,
                false);
            if (!key.Supplied)
                throw new NotImplementedException ();
            return Find<TItem, Tkey> (item, sequence,
                                     fromEnd.Supplied ? fromEnd.Value : false,
                                     test.Supplied ? ResolveFunctionSpecifier<EqualityTest<Tkey>> (test.Value) : new EqualityTest<Tkey> (CL.Eql<Tkey>),
                                     start.Supplied ? start.Value : 0,
                                     end.Supplied ? end.Value : CL.Length (sequence),
                                     ResolveFunctionSpecifier<Function1<Tkey, TItem>> (key.Value));
        }

        static TItem Find<TItem, Tkey> (Tkey item, System.Collections.Generic.ICollection<TItem> sequence, bool fromEnd, EqualityTest<Tkey> test, int start, int end, Function1<Tkey, TItem> key)
        {
            if (fromEnd)
                throw new NotImplementedException ();
            int count = 0;
            foreach (TItem element in sequence) {
                if (count < start) {
                    count += 1;
                    continue;
                }
                if (count >= end)
                    break;
                Tkey ekey = key (element);
                if (test (item, ekey))
                    return element;
                count += 1;
            }
            return default (TItem);
        }


        static public Package FindPackage (string name)
        {
            throw new NotImplementedException ();
        }

        static public object Identity (object item)
        {
            return item;
        }

        static public T Identity<T> (T item)
        {
            return item;
        }

        // INTERN
        static public Symbol Intern (string symbolName)
        {
            if (symbolName == null)
                throw new ArgumentNullException ("symbolName");
            return Intern (symbolName, CL.Package);
        }

        static public Symbol Intern (string symbolName, Package package)
        {
            if (symbolName == null)
                throw new ArgumentNullException ("symbolName");
            if (package == null)
                throw new ArgumentNullException ("package");
            return package.Intern (symbolName);
        }

        static public Cons List ()
        {
            return null;
        }

        static public Cons List (object arg0, params object [] restArguments)
        {
            return new Cons (arg0, (restArguments == null)
                                   ? new Cons (null, null)
                                   : Lisp.Cons.SubvectorToList (restArguments, 0, restArguments.Length));
        }

        static public ConsList<T> List<T> (T arg0, params T [] restArguments) where T : class
        {
            return new ConsList<T> (arg0, (restArguments == null)
                                               ? new ConsList<T> ((T) null, null)
                                               : Lisp.ConsList<T>.SubvectorToList (restArguments, 0, restArguments.Length));
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

        public static StandardInstance MakeInstance
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
            return new StringReader (s.Substring (start, end - start));
        }

        static object MapForEffect (object functionSpecifier, object [] sequences)
        {
            throw new NotImplementedException ();
        }

        static O MapForEffect<I, O> (object functionSpecifier, object [] sequences)
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

        static ConsList<T> MapToList<I, T> (object functionSpecifier, object [] sequences)
        {
            switch (sequences.Length) {
                case 1:
                    return MapToList1<I, T> (functionSpecifier, sequences [0]);
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

        static ConsList<T> MapToList1<I, T> (object functionSpecifier, object sequence)
        {
            if (sequence == null)
                return null;
            I [] sa = sequence as I [];
            if (sa != null)
                return MapVectorToList1<I, T> (functionSpecifier, sa);
            else {
                ConsList<I> sc = sequence as ConsList<I>;
                if (sc != null)
                    return MapListToList1<I, T> (functionSpecifier, sc);
                else {
                    ICollection si = sequence as ICollection;
                    if (si != null)
                        return MapCollectionToList1<T> (functionSpecifier, si);
                    else
                        throw new NotImplementedException ();

                }
            }
        }

        static ConsList<T> MapCollectionToList1<T> (object functionSpecifier, ICollection sequence)
        {
            Delegate function = ResolveFunctionSpecifier (functionSpecifier);
            ConsList<T> reverseAnswer = null;
            foreach (object element in sequence) {
                T item = (T) function.DynamicInvoke (element);
                reverseAnswer = new ConsList<T> (item, reverseAnswer);
            }
            return CL.Reverse<T> (reverseAnswer);
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

        static ConsList<T> MapListToList1<I, T> (object functionSpecifier, ConsList<I> sequence)
        {
            Delegate function = ResolveFunctionSpecifier (functionSpecifier);
            ConsList<T> answer = null;
            while (sequence != null) {
                answer = new ConsList<T> ((T) function.DynamicInvoke (sequence.Car), answer);
                object temp = sequence.Cdr;
                if (temp == null)
                    break;
                ConsList<I> next = temp as ConsList<I>;
                if (next == null)
                    throw new NotImplementedException ();
                sequence = next;
            }
            return CL.Reverse<T> (answer);
        }

        static ConsList<T> MapVectorToList1<I, T> (object functionSpecifier, I [] sequence)
        {
            Delegate function = ResolveFunctionSpecifier (functionSpecifier);
            ConsList<T> answer = null;
            foreach (I element in sequence)
                answer = new ConsList<T> ((T) function.DynamicInvoke (element), answer);
            return CL.Reverse<T> (answer);
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
            if (list == null)
                return null;
            Cons pair = list as Cons;
            if (pair == null)
                throw new NotImplementedException ();
            return (item == pair.Car)
                ? pair
                : Memq (item, pair.Cdr);
        }

        static public ConsList<T> Memq<T> (T item, ConsList<T> list)
        {
            return
                (list == null) ? null
                : object.ReferenceEquals (item, list.Car) ? list
                : Memq (item, list.Cdr);
        }

        static Delegate ResolveFunctionSpecifier (object functionSpecifier)
        {
            Delegate fasd = functionSpecifier as Delegate;
            if (fasd != null)
                return fasd;
            else
                throw new NotImplementedException ();
        }

        static T ResolveFunctionSpecifier<T> (object functionSpecifier) where T : class
        {
            T answer = functionSpecifier as T;
            if (answer != null)
                return answer;
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

        //static public object Map<TOut, TIn> (object sequenceTypeSpecifier, object functionSpecifier, params object [] sequences)
        //{
        //    if (functionSpecifier == null)
        //        throw new ArgumentNullException ("functionSpecifier");
        //    else if (sequenceTypeSpecifier == null)
        //        return MapForEffect<TOut, TIn> (functionSpecifier, sequences);
        //    else if (sequenceTypeSpecifier == QuoteList)
        //        return MapToList<TOut, TIn> (functionSpecifier, sequences);
        //    else
        //        throw new NotImplementedException ();
        //}


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
                             KW.Key, new Function1 (CL.Identity),
                             KW.Test, new EqualityTest (CL.Eql),
                             KW.Start, 0,
                             KW.End, CL.Length (sequence));
        }

        static public int Position (object item, object sequence,
                            object key0, object val0)
        {
            return Position (item, sequence, new object [] { key0, val0 });
        }

        static public int Position (object item, object sequence,
                            object key0, object val0,
                            object key1, object val1)
        {
            throw new NotImplementedException ();
        }

        static public int Position (object item, object sequence,
                            object key0, object val0,
                            object key1, object val1,
                            object key2, object val2)
        {
            throw new NotImplementedException ();
        }

        static public int Position (object item, object sequence,
                            object key0, object val0,
                            object key1, object val1,
                            object key2, object val2,
                            object key3, object val3)
        {
            throw new NotImplementedException ();
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

        static int Position (object item, object sequence, object [] arguments)
        {
            KeywordArgument<bool> fromEnd = new KeywordArgument<bool> (KW.FromEnd);
            KeywordArgument<object> test = new KeywordArgument<object> (KW.Test);
            KeywordArgument<int> start = new KeywordArgument<int> (KW.Start);
            KeywordArgument<int> end = new KeywordArgument<int> (KW.End);
            KeywordArgument<object> key = new KeywordArgument<object> (KW.Key);

            KeywordArgumentBase.ProcessKeywordArguments (
                new KeywordArgumentBase [] { fromEnd, test, start, end, key },
                arguments,
                false);

            return Position (item, sequence,
                fromEnd.Supplied ? fromEnd.Value : false,
                test.Supplied ? ResolveFunctionSpecifier<EqualityTest> (test.Value) : new EqualityTest (CL.Eql),
                start.Supplied ? start.Value : 0,
                end.Supplied ? end.Value : CL.Length (sequence),
                key.Supplied ? ResolveFunctionSpecifier<Function1> (key.Value) : new Function1 (CL.Identity));
        }

        static int Position (object item, object sequence, bool fromEnd, EqualityTest test, int start, int end, Function1 key)
        {
            if (sequence == null)
                return -1;
            Cons list = sequence as Cons;
            if (list != null)
                return Position (item, list, fromEnd, test, start, end, key);
            else
                throw new NotImplementedException ();
        }

        static int Position (object item, Cons sequence, bool fromEnd, EqualityTest test, int start, int end, Function1 key)
        {
            if (fromEnd)
                throw new NotImplementedException ();

            int answer = 0;
            while (true) {
                if (answer >= end)
                    return -1;
                if (answer >= start
                    && test (item, key (sequence.Car)))
                    break;

                answer += 1;
                sequence = (Cons) sequence.Cdr;
            }
            return answer;
        }

        static public int Position<TKey> (TKey item, IList sequence,
            object key0, object val0)
        {
            return Position<TKey> (item, sequence, new object [] { key0, val0 });
        }

        static int Position<TKey> (TKey item, IList sequence, object [] arguments)
        {
            KeywordArgument<bool> fromEnd = new KeywordArgument<bool> (KW.FromEnd);
            KeywordArgument<object> test = new KeywordArgument<object> (KW.Test);
            KeywordArgument<int> start = new KeywordArgument<int> (KW.Start);
            KeywordArgument<int> end = new KeywordArgument<int> (KW.End);
            KeywordArgument<Function1<TKey>> key = new KeywordArgument<Function1<TKey>> (KW.Key);

            KeywordArgumentBase.ProcessKeywordArguments (
                new KeywordArgumentBase [] { fromEnd, test, start, end, key },
                arguments,
                false);
            if (!key.Supplied)
                throw new NotImplementedException ();

            return Position<TKey> (item, sequence,
                fromEnd.Supplied ? fromEnd.Value : false,
                test.Supplied ? ResolveFunctionSpecifier<EqualityTest<TKey>> (test.Value) : new EqualityTest<TKey> (CL.Eql),
                start.Supplied ? start.Value : 0,
                end.Supplied ? end.Value : CL.Length (sequence),
                ResolveFunctionSpecifier<Function1<TKey>> (key.Value));
        }

        static int Position<TKey> (TKey item, IList sequence, bool fromEnd, EqualityTest<TKey> test, int start, int end, Function1<TKey> key)
        {
            if (fromEnd)
                throw new NotImplementedException ();
            int count = 0;
            foreach (object element in sequence) {
                if (count < start) {
                    count += 1;
                    continue;
                }
                if (count >= end)
                    break;
                TKey ekey = key (element);
                if (test (item, ekey))
                    return count;
                count += 1;
            }
            return -1;
        }


        static public int Position<TItem, TKey> (TKey item, System.Collections.Generic.IList<TItem> sequence,
                    object key0, object val0)
        {
            return Position<TItem, TKey> (item, sequence, new object [] { key0, val0 });
        }

        static int Position<TItem, TKey> (TKey item, System.Collections.Generic.IList<TItem> sequence, object [] arguments)
        {
            KeywordArgument<bool> fromEnd = new KeywordArgument<bool> (KW.FromEnd);
            KeywordArgument<object> test = new KeywordArgument<object> (KW.Test);
            KeywordArgument<int> start = new KeywordArgument<int> (KW.Start);
            KeywordArgument<int> end = new KeywordArgument<int> (KW.End);
            KeywordArgument<Function1<TKey, TItem>> key = new KeywordArgument<Function1<TKey, TItem>> (KW.Key);

            KeywordArgumentBase.ProcessKeywordArguments (
                new KeywordArgumentBase [] { fromEnd, test, start, end, key },
                arguments,
                false);
            if (!key.Supplied)
                throw new NotImplementedException ();

            return Position<TItem, TKey> (item, sequence,
                fromEnd.Supplied ? fromEnd.Value : false,
                test.Supplied ? ResolveFunctionSpecifier<EqualityTest<TKey>> (test.Value) : new EqualityTest<TKey>(CL.Eql),
                start.Supplied ? start.Value : 0,
                end.Supplied ? end.Value : CL.Length (sequence),
                ResolveFunctionSpecifier<Function1<TKey,TItem>> (key.Value));
        }

        static int Position<TItem, TKey> (TKey item, System.Collections.Generic.IList<TItem> sequence, bool fromEnd, EqualityTest<TKey> test, int start, int end, Function1<TKey, TItem> key)
        {
            if (fromEnd)
                throw new NotImplementedException ();
            int count = 0;
            foreach (TItem element in sequence) {
                if (count < start) {
                    count += 1;
                    continue;
                }
                if (count >= end)
                    break;
                TKey ekey = key (element);
                if (test (item, ekey))
                    return count;
                count += 1;
            }
            return -1;
        }




        static public int PositionIf (object predicate, object sequence)
        {
            if (sequence == null)
                return -1;
            Cons list = sequence as Cons;
            if (list != null) {
                return PositionIf (predicate, list);
            }
            throw new NotImplementedException ("PositionIf on non-list");
        }

        static public int PositionIf<T> (Predicate<T> predicate, object sequence)
        {
            if (sequence == null)
                return -1;
            ConsList<T> cl = sequence as ConsList<T>;
            if (cl != null)
                return PositionIf<T> ((Predicate<T>) predicate, cl);
            else {
                Cons list = sequence as Cons;
                if (list != null)
                    return PositionIf (predicate, list);
                else
                    throw new NotImplementedException ();
            }
        }

        static public int PositionIf<T> (Predicate<T> predicate, ConsList<T> sequence)
        {
            int pos = 0;
            foreach (T element in sequence) {
                if (predicate (element))
                    return pos;
                pos += 1;
            }
            return -1;
        }

        static public int PositionIf<T> (Predicate<T> predicate, Cons sequence)
        {
            int pos = 0;
            foreach (T element in sequence) {
                if (predicate (element))
                    return pos;
                pos += 1;
            }
            return -1;
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

        static public ConsList<T> Reconc<T> (ConsList<T> tail, ConsList<T> head)
        {
            if (tail == null)
                return head;
            // yeah I'll fix it...
            return Reconc<T> (tail.Cdr, new ConsList<T> (tail.Car, head));
        }

        static public Cons Reverse (Cons list)
        {
            return (Cons) CL.Reconc (list, null);
        }

        static public ConsList<T> Reverse<T> (ConsList<T> list)
        {
            return CL.Reconc<T> (list, null);
        }

        static public object Reverse (object list)
        {
            return CL.Reconc (list, null);
        }

        static public Cons RemoveIf (Delegate predicate, Cons sequence)
        {
            if (predicate == null)
                throw new ArgumentNullException ("predicate");
            if (sequence == null)
                return null;
            else if ((bool) predicate.DynamicInvoke (sequence.Car))
                return RemoveIf (predicate, (Cons) sequence.Cdr);
            else
                return new Cons (sequence.Car, RemoveIf (predicate, (Cons) sequence.Cdr));
        }

        static public object RemoveIf (object predicate, object sequence)
        {
            if (predicate == null)
                throw new ArgumentNullException ("predicate");
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

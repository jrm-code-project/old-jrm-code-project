using System;
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

        // CONS
        public static Cons Cons (object car, object cdr)
        {
            return new Cons (car, cdr);
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

        static public object Read ()
        {
            throw new NotImplementedException ("Read");
        }

        static public object Read (TextReader inputStream)
        {
            throw new NotImplementedException ("Read");
        }

        static public object Read (TextReader inputStream, bool isEofError)
        {
            throw new NotImplementedException ("Read");
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
    }
}

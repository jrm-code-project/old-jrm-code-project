using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Lisp
{
    public delegate ReaderMacroStep ReaderMacroFunction (ReaderContext context, char character);

    public abstract class ReaderMacroStep
    {
        readonly bool isFinal;
        protected ReaderMacroStep (bool isFinal)
        {
            this.isFinal = isFinal;
        }

        public bool IsFinal
        {
            get
            {
                return this.isFinal;
            }
        }

        public abstract ReaderMacroStep NextStep ();

        public abstract int ResultCount{get;}
 
        public abstract object Result (int i);
    }

    abstract class NonFinalReaderMacroStep : ReaderMacroStep
    {
        protected readonly ReaderContext context;
        protected NonFinalReaderMacroStep (ReaderContext context)
            : base (false)
        {
            this.context = context;
        }

        public override int ResultCount
        {
            get
            {
                throw new ImplementationBugException ("ResultCount of NonFinalReaderMacroStep");
            }
        }

        public override object Result (int i)
        {
                throw new ImplementationBugException ("Result of NonFinalReaderMacroStep");
        }
    }

    class FinalReaderMacroStep : ReaderMacroStep
    {
        internal object [] results;

        // Reader macros can return zero or one value.
        public FinalReaderMacroStep ()
            : base (true)
        {
            this.results = new object [] {};
        }

        public FinalReaderMacroStep (object result)
            : base (true)
        {
            this.results = new object [] {result};
        }

        public override ReaderMacroStep NextStep ()
        {
            throw new ImplementationBugException ("Called NextStep() on FinalReaderMacroStep");
        }

        public override int ResultCount
        {
            get
            {
                return this.results.Length;
            }
        }

        public override object Result (int i)
        {
                return this.results[i];
        }
    }

     class ReadDoubleQuote : NonFinalReaderMacroStep
    {
        public ReadDoubleQuote (ReaderContext context, char doubleQuote)
            : base (context)
        {
            Utility.Ignore (doubleQuote);
        }

        public static ReaderMacroStep ReaderMacroFunction (ReaderContext context, char doubleQuote)
        {
            return new ReadDoubleQuote (context, doubleQuote);
        }

        override public ReaderMacroStep NextStep ()
        {
            StringBuilder answer = new StringBuilder ();
            while (true) {
                int xx;
                char x;

                xx = this.context.InputStream.Read ();
                if (xx == -1)
                    throw new NotImplementedException ();
                else {
                    x = (char) xx;
                }

                if (x == '"') {
                    return (CL.ReadSuppress == true)
                        ? new FinalReaderMacroStep()
                        : new FinalReaderMacroStep (answer.ToString ());
                }
                else if (x == '\\') {
                    int yy = this.context.InputStream.Read ();
                    if (yy == -1)
                        throw new NotImplementedException ();
                    else {
                        answer.Append ((char) yy);
                    }
                }
                else {
                    answer.Append (x);
                }
            }
        }
    }



    class ReadLeftParen : NonFinalReaderMacroStep
    {
        ReadLeftParen (ReaderContext context, char lParen)
            : base (context)
        {
            Utility.Ignore (lParen);
        }

        public static ReaderMacroStep ReaderMacroFunction (ReaderContext context, char lParen)
        {
            return new ReadLeftParen (context, lParen);
        }

        public override ReaderMacroStep NextStep ()
        {
            Cons head = null;
            object tail = null;

            Readtable currentReadtable = CL.Readtable;
            while (true) {
                int xx;
                char x;
                // Discard whitespace
                do {
                    xx = this.context.InputStream.Peek ();
                    if (xx == -1) {
                        throw new NotImplementedException ();
                    }
                    else {
                        x = (char) xx;
                    }
                }
                while (currentReadtable.IsWhitespaceSyntax (x) && (this.context.InputStream.Read () != -1));
                if (x == ')') {
                    this.context.InputStream.Read (); // discard the close paren
                    return (CL.ReadSuppress == true)
                    ? new FinalReaderMacroStep ()
                    : new FinalReaderMacroStep (CL.Reconc (head, tail));
                }
                //else if (x == '.') {
                //    throw new NotImplementedException ();
                //}
                else {
                    object next = CL.Read (this.context.InputStream, true, null, true); // recursive read
                    if (CL.ReadSuppress != true) {
                        head = CL.Cons (next, head);
                    }
                }
            }
        }
    }
}
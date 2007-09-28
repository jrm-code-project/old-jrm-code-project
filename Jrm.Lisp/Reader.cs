using System;
using System.IO;
using System.Text;

namespace Lisp
{
    public class ReaderContext
    {
        readonly TextReader inputStream;
        readonly bool isEofError;
        readonly object eofValue;

        public ReaderContext (TextReader inputStream, bool isEofError, object eofValue)
        {
            this.inputStream = inputStream;
            this.isEofError = isEofError;
            this.eofValue = eofValue;
        }

        public TextReader InputStream
        {
            get
            {
                return this.inputStream;
            }
        }

        public bool IsEofError
        {
            get
            {
                return this.isEofError;
            }
        }

        public object EofValue
        {
            get
            {
                return this.eofValue;
            }
        }
    }

    abstract class ReaderStep
    {
        internal readonly bool isFinal;
        protected ReaderStep (bool isFinal)
        {
            this.isFinal = isFinal;
        }

        public abstract ReaderStep NextStep ();

        public abstract object Result
        {
            get;
        }
    }

    abstract class NonFinalReaderStep : ReaderStep
    {
        protected readonly ReaderContext context;
        protected NonFinalReaderStep (ReaderContext context)
            : base (false)
        {
            this.context = context;
        }

        public override object Result
        {
            get
            {
                throw new ImplementationBugException ("Result of NonFinalReaderStep");
            }
        }
    }

    class InitialReaderStep : NonFinalReaderStep
    {
        // used to start the reader state machine
        public InitialReaderStep (TextReader inputStream, bool isEofError, object eofValue)
            : base (new ReaderContext (inputStream, isEofError, eofValue))
        {
        }

        // used by other steps to return to this state
        public InitialReaderStep (ReaderContext context)
            : base (context)
        {
        }

        public override ReaderStep NextStep ()
        {
            // 1. If at end of file, end-of-file processing is performed as specified in read.
            // Otherwise, one character, x, is read from the input stream, and dispatched according
            // to the syntax type of x to one of steps 2 to 7.
            int xx = this.context.InputStream.Read ();
            if (xx == -1) {
                if (this.context.IsEofError) {
                    throw new NotImplementedException ();
                }
                else {
                    return new FinalReaderStep (this.context.EofValue);
                }
            }
            else {
                char x = (char) xx;
                switch (CL.Readtable.GetSyntaxType (x)) {
                    case SyntaxType.Constituent:
                        return new ReaderStep7 (this.context, x);

                    case SyntaxType.Invalid:
                        return new ReaderStep2 (this.context, x);

                    case SyntaxType.MultipleEscape:
                        return new ReaderStep6 (this.context, x);

                    case SyntaxType.NonTerminatingMacroChar:
                        return new ReaderStep4 (this.context, x);

                    case SyntaxType.SingleEscape:
                        return new ReaderStep5 (this.context, x);

                    case SyntaxType.TerminatingMacroChar:
                        return new ReaderStep4 (this.context, x);

                    case SyntaxType.Whitespace:
                        return new ReaderStep3 (this.context, x);

                    default:
                        throw new NotImplementedException ();
                }
            }
        }
    }

    class ReaderStep2 : NonFinalReaderStep
    {
        public ReaderStep2 (ReaderContext context, char x)
            : base (context)
        {
            Utility.Ignore (x);
            throw new NotImplementedException ();
        }

        public override ReaderStep NextStep ()
        {
            throw new NotImplementedException ();
        }
    }

    class ReaderStep3 : NonFinalReaderStep
    {
        public ReaderStep3 (ReaderContext context, char x)
            : base (context)
        {
            Utility.Ignore (x);
        }

        public override ReaderStep NextStep ()
        {
            // If x is a whitespace[2] character, then it is discarded and step 1 is re-entered.
            return new InitialReaderStep (this.context);
        }
    }

    internal class ReaderStep4 : NonFinalReaderStep
    {
        char x;

        public ReaderStep4 (ReaderContext context, char x)
            : base (context)
        {
            this.x = x;
        }

        // If x is a terminating or non-terminating macro character
        // then its associated reader macro function is called with
        // two arguments, the input stream and x.

        public override ReaderStep NextStep ()
        {
            ReaderMacroFunction macroFunction = CL.Readtable.GetMacroFunction (this.x);
            ReaderMacroStep readerMacroStep = macroFunction (this.context, this.x);
            while (!(readerMacroStep = readerMacroStep.NextStep ()).IsFinal) {
            }

            // The reader macro function may return zero values or one value.
            // If one value is returned, then that value is returned
            // as the result of the read operation; the algorithm is done.
            return (readerMacroStep.ResultCount == 0)
                ? (ReaderStep) new InitialReaderStep (this.context)
                : (ReaderStep) new FinalReaderStep (readerMacroStep.Result (0));
        }

    }

    internal class ReaderStep5 : NonFinalReaderStep
    {
        public ReaderStep5 (ReaderContext context, char x)
            : base (context)
        {
            Utility.Ignore (x);
        }

        public override ReaderStep NextStep ()
        {
            throw new NotImplementedException ();
        }
    }

    internal class ReaderStep6 : NonFinalReaderStep
    {
        public ReaderStep6 (ReaderContext context, char x)
            : base (context)
        {
            Utility.Ignore (x);
        }

        public override ReaderStep NextStep ()
        {
            throw new NotImplementedException ();
        }
    }

    internal class ReaderStep7 : NonFinalReaderStep
    {
        char x;

        public ReaderStep7 (ReaderContext context, char x)
            : base (context)
        {
            this.x = x;
        }

        override public ReaderStep NextStep ()
        {
            // If x is a constituent character, then it begins a token.
            // UNLESS it is a colon.
            StringBuilder tokenBuffer = new StringBuilder ();

            if (x != ':') {
                switch (CL.Readtable.Case) {
                    case ReadtableCase.Downcase:
                        tokenBuffer.Append(Char.ToLowerInvariant (x));
                        break;

                    case ReadtableCase.Invert:
                        throw new NotImplementedException ();
                        break;

                    case ReadtableCase.Preserve:
                        tokenBuffer.Append (x);
                        break;

                    case ReadtableCase.Upcase:
                        tokenBuffer.Append (Char.ToUpperInvariant (x));
                        break;
                }
            }
            return new ReaderStep8 (this.context, tokenBuffer, x == ':' ? "" : null);
        }
    }

    internal class ReaderStep8 : NonFinalReaderStep
    {
        StringBuilder tokenBuffer;
        string packagePrefix;

        public ReaderStep8 (ReaderContext context, StringBuilder tokenBuffer, string packagePrefix)
            : base (context)
        {
            this.tokenBuffer = tokenBuffer;
            this.packagePrefix = packagePrefix;
        }

        public override ReaderStep NextStep ()
        {
            // At this point a token is being accumulated, and an even number
            // of multiple escape characters have been encountered. If at end
            // of file, step 10 is entered. Otherwise, a character, y, is read,
            // and one of the following actions is performed according to its
            // syntax type:
            int yy = this.context.InputStream.Peek ();
            if (yy == -1) {
                return new ReaderStep10 (this.context, this.packagePrefix, this.tokenBuffer.ToString ());
            }
            char y = (char) yy;
            switch (CL.Readtable.GetSyntaxType (y)) {
                case SyntaxType.Constituent:
                case SyntaxType.NonTerminatingMacroChar:
                    // If y is a constituent or non-terminating macro character:

                    // -- If y is a character with case, it might be replaced with the corresponding
                    // character of the opposite case, depending on the readtable case
                    // of the current readtable, as outlined in Section 23.1.2 (Effect of Readtable
                    // Case on the Lisp Reader).
                    // -- Y is appended to the token being built.
                    // -- Step 8 is repeated.
                    this.context.InputStream.Read ();
                    switch (CL.Readtable.Case) {
                        case ReadtableCase.Downcase:
                            tokenBuffer.Append (Char.ToLowerInvariant (y));
                            break;

                        case ReadtableCase.Invert:
                            throw new NotImplementedException ();
                            break;

                        case ReadtableCase.Preserve:
                            tokenBuffer.Append (y);
                            break;

                        case ReadtableCase.Upcase:
                            tokenBuffer.Append (Char.ToUpperInvariant (y));
                            break;
                    }
                    return this;

                case SyntaxType.TerminatingMacroChar:
                    // If y is a terminating macro character, then it terminates the token.
                    // First the character y is unread (see unread-char), and then step 10 is entered.
                    // InputStream.UnreadChar (yy);
                    return new ReaderStep10 (this.context, this.packagePrefix, this.tokenBuffer.ToString ());

                case SyntaxType.Whitespace:
                    // If y is a whitespace[2] character, then it terminates the token.
                    // InputStream.Read ();
                    return new ReaderStep10 (this.context, this.packagePrefix, this.tokenBuffer.ToString ());

                default:
                    throw new NotImplementedException ();
            }
        }
    }

        internal class ReaderStep10 : NonFinalReaderStep
        {
            string packagePrefix;
            string token;

            public ReaderStep10 (ReaderContext context, string packagePrefix, string token)
                : base (context)
            {
                this.packagePrefix = packagePrefix;
                this.token = token;
            }

            public override ReaderStep NextStep ()
            {
                if (CL.ReadSuppress)
                    // Toss the symbol and read the next thing.
                    return new InitialReaderStep (this.context);

                    // handle numbers here
                else
                    return new TokenToSymbol (this.packagePrefix == null ? CL.Package : CL.FindPackage (packagePrefix), this.token);
            }
        }

        internal class TokenToSymbol : ReaderStep
        {
            Package package;
            string token;

            public TokenToSymbol (Package package, string token)
                : base (false)
            {
                this.package = package;
                this.token = token;
            }

            public override object Result
            {
                get
                {
                    throw new ImplementationBugException ("Result of NonFinalReaderStep");
                }
            }

            public override ReaderStep NextStep ()
            {
                if (this.package == null)
                    return new FinalReaderStep (new Symbol (token));
                else
                    return new FinalReaderStep (CL.Intern (token, package));
            }
        }

        class FinalReaderStep : ReaderStep
        {
            internal object result;

            public FinalReaderStep (object result)
                : base (true)
            {
                this.result = result;
            }

            public override ReaderStep NextStep ()
            {
                throw new ImplementationBugException ("Called NextStep() on FinalReaderStep");
            }

            public override object Result
            {
                get
                {
                    return this.result;
                }
            }
        }
}


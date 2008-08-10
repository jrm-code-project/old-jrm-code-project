using System;
using System.Collections;
using System.Diagnostics;

namespace Microcode
{
    interface ILambda
    {
        string [] Formals { get; }
    }

    class Lambda : SCode, ILambda, ISystemPair
    {
        static long evaluationCount;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly string [] formals;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode body;

        public Lambda (string [] formals, SCode body)
            : base (TC.LAMBDA)
        {
            if (body == null)
                throw new ArgumentNullException ("body");
            if (formals == null)
                throw new ArgumentNullException ("formals");
            this.formals = formals;
            this.body = body;
        }

        public Lambda (object formals, object body)
            : base (TC.LAMBDA)
        {
            object [] cdrArray = (object []) formals;
            string [] sformals = new string [cdrArray.Length];

            for (int i = 0; i < sformals.Length; i++)
                sformals [i] = (string) cdrArray [i];
            this.formals = sformals;
            this.body = EnsureSCode (body);
        }

        public SCode Body
        {
            [DebuggerStepThrough]
            get
            {
                return this.body;
            }
        }

        public string Name
        {
            [DebuggerStepThrough]
            get
            {
                return this.formals [0];
            }
        }

        public string [] Formals
        {
            [DebuggerStepThrough]
            get
            {
                return this.formals;
            }
        }

        static int [] formalOffsetCount = new int [512 + 256];
        public int LexicalOffset (string name)
        {
            //int theirs = Array.IndexOf<object>(this.formals, name);

            for (int i = 0; i < formals.Length; i++)
                if ((object)name == (object) formals [i]) {
                    formalOffsetCount [i] += 1;
                    //if (theirs != ours)
                    //    throw new NotImplementedException ();
                    return i;
                }
            //if (theirs != ours)
            //    throw new NotImplementedException ();
            return -1;
        }

        internal override object EvalStep (Interpreter interpreter, object etc)
        {
            Lambda.evaluationCount += 1;
            return interpreter.Return (new Closure (this, interpreter.Environment));
        }


        #region ISystemPair Members

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemPairCar
        {
            get
            {
                return UnwrapQuoted (this.body);
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemPairCdr
        {
            get
            {
                return this.formals;
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        #endregion

        internal override SCode Optimize (CompileTimeEnvironment ctenv)
        {
            return new Lambda (this.formals, this.body.Optimize (ctenv.Extend (this.formals)));
        }

        #region ILambda Members

        string [] ILambda.Formals
        {
            get { return this.Formals; }
        }

        #endregion
    }

    sealed class ExtendedLambda : SCode, ILambda, ISystemHunk3
    {
        static long evaluationCount;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public readonly string [] formals;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public readonly SCode body;

        public readonly uint required;

        public readonly uint optional;

        public readonly bool rest;

        public ExtendedLambda (string [] formals, object body, uint required, uint optional, bool rest)
            : base (TC.EXTENDED_LAMBDA)
        {
            this.body = EnsureSCode (body);
            this.formals = formals;
            this.required = required;
            this.optional = optional;
            this.rest = rest;
        }

        public ExtendedLambda (Hunk3 init)
            : base (TC.EXTENDED_LAMBDA)
        {
            object [] cdrArray = (object []) init.Cxr1;
            string [] sformals = new string [cdrArray.Length];
            for (int i = 0; i < sformals.Length; i++)
                sformals [i] = (string) cdrArray [i];
            this.body = EnsureSCode (init.Cxr0);
            this.formals = sformals;
            uint code = (uint) (int) (init.Cxr2);
            this.optional = code & 0xFF;
            this.required = (code >> 8) & 0xFF;
            this.rest = ((code >> 16) & 0x1) == 0x1;
        }

        internal override object EvalStep (Interpreter interpreter, object etc)
        {
            ExtendedLambda.evaluationCount += 1;
            return interpreter.Return (new ExtendedClosure (this, interpreter.Environment));
        }

        public string Name
        {
            [DebuggerStepThrough]
            get
            {
                return this.formals [0];
            }
        }

        static int [] formalOffsetCount = new int [16];
        public int LexicalOffset (string name)
        {
            for (int i = 0; i < formals.Length; i++)
                if ((object) name == (object) formals [i]) {
                    formalOffsetCount [i] += 1;
                    //if (theirs != ours)
                    //    throw new NotImplementedException ();
                    return i;
                }
            //if (theirs != ours)
            //    throw new NotImplementedException ();
            return -1;
        }


        #region ISystemHunk3 Members
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemHunk3Cxr0
        {
            get
            {
                return UnwrapQuoted (this.body);
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemHunk3Cxr1
        {
            [DebuggerStepThrough]
            get
            {
                return this.formals;
            }
            set
            {
                throw new NotImplementedException ();
            }
        }
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemHunk3Cxr2
        {
            get
            {
                return this.optional + (256 * (this.required + (this.rest ? 256 : 0)));

            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        #endregion

        internal override SCode Optimize (CompileTimeEnvironment ctenv)
        {
            return new ExtendedLambda (this.formals, this.body.Optimize (ctenv.Extend (this.formals)), this.required, this.optional, this.rest);
        }

        #region ILambda Members

        public string [] Formals
        {
            get { return this.formals; }
        }

        #endregion
    }

}

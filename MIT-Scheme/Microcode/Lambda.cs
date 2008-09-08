using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;

namespace Microcode
{
    interface ILambda
    {
        string [] Formals { get; }
        SCode Body { get; }
    }

    sealed class Lambda : SCode, ILambda, ISystemPair
    {
#if DEBUG
        static long evaluationCount;
#endif
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

        public string Name
        {
            [DebuggerStepThrough]
            get
            {
                return this.formals [0];
            }
        }

        static int [] formalOffsetCount = new int [512 + 256];
        public int LexicalOffset (string name)
        {
            //int theirs = Array.IndexOf<object>(this.formals, name);

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


        public override SCode Bind (CompileTimeEnvironment ctenv)
        {
            SCode optimizedBody = this.body.Bind (ctenv.Extend (this.formals));
            return this.body == optimizedBody
                ? this
                : new Lambda (this.formals, optimizedBody);
        }

        public override bool EvalStep (out object answer, ref SCode expression, ref Environment environment)
        {
#if DEBUG
            Lambda.evaluationCount += 1;
#endif
            answer = new Closure (this, environment);
            return false;
        }

        public override HashSet<string> FreeVariables ()
        {
            HashSet<string> bodyFree = this.body.FreeVariables ();
            bodyFree.ExceptWith (this.formals);
            return bodyFree;
        }

        public override bool NeedsTheEnvironment ()
        {
            return this.body.NeedsTheEnvironment ();
        }

        #region ILambda Members

        public string [] Formals
        {
            [DebuggerStepThrough]
            get { return this.formals; }
        }

        public SCode Body
        {
            [DebuggerStepThrough]
            get
            {
                return this.body;
            }
        }

        #endregion

        #region ISystemPair Members

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemPairCar
        {
            [DebuggerStepThrough]
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

        #endregion
    }

    sealed class ExtendedLambda : SCode, ILambda, ISystemHunk3
    {
#if DEBUG
        static long evaluationCount;
#endif
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


        public override SCode Bind (CompileTimeEnvironment ctenv)
        {
            SCode optimized = this.body.Bind (ctenv.Extend (this.formals));
            return optimized == this.body
                ? this
                : new ExtendedLambda (this.formals, optimized, this.required, this.optional, this.rest);
        }


        public override bool EvalStep (out object answer, ref SCode expression, ref Environment environment)
        {
#if DEBUG
            ExtendedLambda.evaluationCount += 1;
#endif
            answer = new ExtendedClosure (this, environment);
            return false;
        }

        public override HashSet<string> FreeVariables ()
        {
            HashSet<string> bodyFree = this.body.FreeVariables ();
            bodyFree.ExceptWith (this.formals);
            return bodyFree;
        }

        public override bool NeedsTheEnvironment ()
        {
            return this.body.NeedsTheEnvironment ();
        }


        #region ILambda Members

        public string [] Formals
        {
            [DebuggerStepThrough]
            get { return this.formals; }
        }

        public SCode Body
        {
            [DebuggerStepThrough]
            get
            {
                return this.body;
            }
        }

        #endregion

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
    }
}

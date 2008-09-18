using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;

namespace Microcode
{
    public interface ILambda
    {
        int LexicalOffset (string name); 
        string [] Formals { get; }
        SCode Body { get; }
        string Name { get; }
    }

    [Serializable]
    sealed class Lambda : SCode, ILambda, ISystemPair
    {
#if DEBUG
        [NonSerialized]
        static long creationCount;
        [NonSerialized]
        static long evaluationCount;
#endif
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly string [] formals;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode body;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly string name;

        Lambda (string name, string [] formals, SCode body)
            : base (TC.LAMBDA)
        {
#if DEBUG
            Lambda.creationCount += 1;
#endif

            this.formals = formals;
            this.body = body;
            this.name = name;
        }

        public static SCode Make (string name, string [] formals, SCode body)
        {
            if (body == null)
                throw new ArgumentNullException ("body");
            if (formals == null)
                throw new ArgumentNullException ("formals");

            if (body.NeedsValueCells (formals))
                return new Lambda (name, formals, body);
            else
                return SimpleLambda.Make (name, formals, body);
        }

        public static SCode Make (object name, object formals, object body)
        {

            if (formals == null)
                throw new ArgumentNullException ("formals");

            string sname = (string) name;
            object [] cdrArray = (object []) formals;
            string [] sformals = new string [cdrArray.Length];

            for (int i = 0; i < sformals.Length; i++)
                sformals [i] = (string) cdrArray [i];
            return Make (sname, sformals, EnsureSCode (body));
        }

        public string Name
        {
            [DebuggerStepThrough]
            get
            {
                return this.name;
            }
        }

        [SchemePrimitive ("LAMBDA?", 1, true)]
        public static bool IsLambda (out object answer, object arg)
        {
            answer = arg is Lambda || arg is SimpleLambda || arg is TrivialLambda;
            return false;
        }

        [SchemePrimitive ("LEXPR?", 1, true)]
        public static bool IsLexpr (out object answer, object arg)
        {
            answer = false;
            return false;
        }

        [NonSerialized]
        static int [] formalOffsetCount = new int [512 + 256];
        public int LexicalOffset (string name)
        {
            for (int i = 0; i < formals.Length; i++)
                if ((object) name == (object) formals [i]) {
                    formalOffsetCount [i] += 1;
                    return i;
                }
            return -1;
        }
        
        public override SCode Bind (BindingTimeEnvironment ctenv)
        {
            SCode optimizedBody = this.body.Bind (ctenv.Extend (this));
            return this.body == optimizedBody
                ? this
                : new Lambda (this.name, this.formals, optimizedBody);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Lambda.evaluationCount += 1;
#endif
            answer = new Closure (this, environment);
            return false;
        }

        public override IList<object> FreeVariables ()
        {
            IList<object> answer = new List<object> ();
            foreach (object var in this.body.FreeVariables ())
                if (!answer.Contains (var)) answer.Add (var);
            foreach (object var in this.formals)
                if (answer.Contains (var)) answer.Remove (var);
            return answer;
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
                string [] fakeFormals = new string [this.formals.Length + 1];
                fakeFormals [0] = this.name;
                Array.Copy (this.formals, 0, fakeFormals, 1, formals.Length);
                return fakeFormals;
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        #endregion

        public override bool NeedsValueCells (object [] formals)
        {
            // Should check for shadowing.
            return this.body.NeedsValueCells (formals);
        }
#if DEBUG
        public override string Key ()
        {
            throw new NotImplementedException ();
        }
#endif
    }

    [Serializable]
    sealed class ExtendedLambda : SCode, ILambda, ISystemHunk3
    {
#if DEBUG
        [NonSerialized]
        static long creationCount;
        [NonSerialized]
        static long evaluationCount;
#endif
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly string [] formals;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode body;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly string name;

        public readonly uint required;

        public readonly uint optional;

        public readonly bool rest;

        public ExtendedLambda (string name, string [] formals, object body, uint required, uint optional, bool rest)
            : base (TC.EXTENDED_LAMBDA)
        {
#if DEBUG
            ExtendedLambda.creationCount += 1;
#endif

            this.name = name;
            this.body = EnsureSCode (body);
            this.formals = formals;
            this.required = required;
            this.optional = optional;
            this.rest = rest;
        }

        public ExtendedLambda (Hunk3 init)
            : base (TC.EXTENDED_LAMBDA)
        {
#if DEBUG
            ExtendedLambda.creationCount += 1;
#endif

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
                return this.name;
            }
        }

        [NonSerialized]
        static int [] formalOffsetCount = new int [16];
        public int LexicalOffset (string name)
        {
            for (int i = 0; i < formals.Length; i++)
                if ((object) name == (object) formals [i]) {
                    formalOffsetCount [i] += 1;
                    return i;
                }
            //if (theirs != ours)
            //    throw new NotImplementedException ();
            return -1;
        }


        public override SCode Bind (BindingTimeEnvironment ctenv)
        {
            SCode optimized = this.body.Bind (ctenv.Extend (this));
            return optimized == this.body
                ? this
                : new ExtendedLambda (this.name, this.formals, optimized, this.required, this.optional, this.rest);
        }


        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            ExtendedLambda.evaluationCount += 1;
#endif
            answer = new ExtendedClosure (this, environment);
            return false;
        }

        public override IList<object> FreeVariables ()
        {
            IList<object> answer = new List<object> ();
            foreach (object var in this.body.FreeVariables())
                if (! answer.Contains(var)) answer.Add(var);
            foreach (object var in this.formals)
                if (answer.Contains (var)) answer.Remove (var);
            return answer;
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

        public override bool NeedsValueCells (object [] formals)
        {
            //Should check for shadowing names, but...
            return this.body.NeedsValueCells (formals);
        }
#if DEBUG
        public override string Key ()
        {
            throw new NotImplementedException ();
        }
#endif
    }

    [Serializable]
    sealed class SimpleLambda : SCode, ILambda, ISystemPair
    {
#if DEBUG
        [NonSerialized]
        static long creationCount;
        [NonSerialized]
        static long evaluationCount;
#endif
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly string [] formals;

        IList<object> free;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode body;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly string name;

        SimpleLambda (string name, string [] formals, IList<object> free, SCode body)
            : base (TC.LAMBDA)
        {
#if DEBUG
            SimpleLambda.creationCount += 1;
#endif
            this.formals = formals;
            this.free = free;
            this.body = body;
            this.name = name;
        }

        public string Name
        {
            [DebuggerStepThrough]
            get
            {
                return this.name;
            }
        }

        public static SCode Make (string name, string [] formals, SCode body)
        {
            IList<object> freeInBody = new List<object>();
            foreach (object var in body.FreeVariables ())
                if (!formals.Contains<object> (var)) freeInBody.Add (var);
            if (freeInBody.Count == 0)
                return new TrivialLambda (name, formals, body);
            else
                return new SimpleLambda (name, formals, freeInBody, body);
        }

        [NonSerialized]
        static int [] formalOffsetCount = new int [512 + 256];
        public int LexicalOffset (string name)
        {
            for (int i = 0; i < formals.Length; i++)
                if ((object) name == (object) formals [i]) {
                    formalOffsetCount [i] += 1;
                    return i;
                }
            return -1;
        }

        public override SCode Bind (BindingTimeEnvironment ctenv)
        {
            SCode optimizedBody = this.body.Bind (ctenv.Extend (this));
            return this.body == optimizedBody
                ? this
                : new SimpleLambda (this.name, this.formals, this.free, optimizedBody);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            SimpleLambda.evaluationCount += 1;
#endif
            answer = new SimpleClosure (this, environment);
            return false;
        }

        public override IList<object> FreeVariables ()
        {
            return this.free;
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
                string [] fakeFormals = new string [this.formals.Length + 1];
                fakeFormals [0] = this.name;
                Array.Copy (this.formals, 0, fakeFormals, 1, formals.Length);
                return fakeFormals;
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        #endregion

        public override bool NeedsValueCells (object [] formals)
        {
            return this.body.NeedsValueCells (formals);
        }
#if DEBUG
        public override string Key ()
        {
            return "lambda-" + this.serialNumber.ToString ();
        }
#endif
    }

    [Serializable]
    sealed class TrivialLambda : SCode, ILambda, ISystemPair
    {
#if DEBUG
        [NonSerialized]
        static long creationCount;
        [NonSerialized]
        static long evaluationCount;
#endif
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly string [] formals;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode body;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly string name;

        internal TrivialLambda (string name, string [] formals, SCode body)
            : base (TC.LAMBDA)
        {
#if DEBUG
            TrivialLambda.creationCount += 1;
#endif
            this.formals = formals;
            this.body = body;
            this.name = name;
        }

        public string Name
        {
            [DebuggerStepThrough]
            get
            {
                return this.name;
            }
        }

        [NonSerialized]
        static int [] formalOffsetCount = new int [512 + 256];
        public int LexicalOffset (string name)
        {
            for (int i = 0; i < formals.Length; i++)
                if ((object) name == (object) formals [i]) {
                    formalOffsetCount [i] += 1;
                    return i;
                }
            return -1;
        }

        public override SCode Bind (BindingTimeEnvironment ctenv)
        {
            SCode optimizedBody = this.body.Bind (ctenv.Extend (this));
            return this.body == optimizedBody
                ? this
                : new TrivialLambda (this.name, this.formals, optimizedBody);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            TrivialLambda.evaluationCount += 1;
#endif
            answer = new TrivialClosure (this, environment);
            return false;
        }

        public override IList<object> FreeVariables ()
        {
            IList<object> answer = new List<object> ();
            foreach (object var in this.body.FreeVariables ())
                if (!answer.Contains (var)) answer.Add (var);
            foreach (object var in this.formals)
                if (answer.Contains (var)) answer.Remove (var);
            return answer;
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
                string [] fakeFormals = new string [this.formals.Length + 1];
                fakeFormals [0] = this.name;
                Array.Copy (this.formals, 0, fakeFormals, 1, formals.Length);
                return fakeFormals;
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        #endregion

        public override bool NeedsValueCells (object [] formals)
        {
            return this.body.NeedsValueCells (formals);
        }
#if DEBUG
        public override string Key ()
        {
            return "tlambda-" + this.serialNumber.ToString ();
        }
#endif
    }

}

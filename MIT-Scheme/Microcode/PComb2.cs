using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Runtime.Serialization;

namespace Microcode
{
    [Serializable]
    class PrimitiveCombination2 : SCode, ISystemHunk3
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<string> histogram = new Histogram<String> ();
        string histogramKey;

        protected Type rand0Type;
        protected Type rand1Type;
#endif

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly Primitive2 rator;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        [NonSerialized]
        protected PrimitiveMethod2 method;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly SCode rand0;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly SCode rand1;

        protected PrimitiveCombination2 (Primitive2 rator, SCode rand0, SCode rand1)
            : base (TC.PCOMB2)
        {
            this.rator = rator;
            this.method = rator.Method;
            this.rand0 = rand0;
            this.rand1 = rand1;
#if DEBUG
            this.histogramKey = rator.ToString() + " " + rand0.GetType ().Name.ToString () + " " + rand1.GetType ().Name.ToString ();
            rand0Type = rand0.GetType ();
            rand1Type = rand1.GetType ();
#endif
        }

        [OnDeserialized ()]
        internal void OnDeserializedMethod (StreamingContext context)
        {
            this.method = rator.Method;
        }

        public Primitive2 Rator
        {
            [DebuggerStepThrough]
            get
            {
                return this.rator;
            }
        }

        public SCode Rand0
        {
            [DebuggerStepThrough]
            get
            {
                return this.rand0;
            }
        }

        public SCode Rand1
        {
            [DebuggerStepThrough]
            get
            {
                return this.rand1;
            }
        }

        public static SCode Make (Primitive2 rator, SCode rand0, SCode rand1)
        {
            if (rator == null)
                throw new ArgumentNullException ("rator");

            return (Configuration.EnableSuperOperators && rand0 is Argument && rand1 is Argument) ? PrimitiveCombination2AA.Make (rator, (Argument) rand0, (Argument) rand1)
                : (Configuration.EnableSuperOperators && rand0 is Argument) ? PrimitiveCombination2AS.Make (rator, (Argument) rand0, rand1)
                : (Configuration.EnableSuperOperators && rand1 is Argument) ? PrimitiveCombination2SA.Make (rator, rand0, (Argument) rand1)
                : new PrimitiveCombination2 (rator, rand0, rand1);



        }

        public static SCode Make (object rator, object rand0, object rand1)
        {
            return Make ((Primitive2) rator, EnsureSCode (rand0), EnsureSCode (rand1));
        }

        public static SCode Make (Hunk3 init)
        {
            //Primitive2 rator = (Primitive2) init.Cxr0;
            //if (rator == Primitive.Find ("EQ?", 2))
            //    return new PrimitiveEq (init.Cxr1, init.Cxr2);
            return Make ((Primitive2) init.Cxr0, init.Cxr1, init.Cxr2);
        }

        [SchemePrimitive ("PRIMITIVE-COMBINATION2?", 1, true)]
        public static bool IsPrimitiveCombination2 (out object answer, object arg)
        {
            answer = arg is PrimitiveCombination2;
            return false;
        }

        public override SCode Bind (LexicalMap ctenv)
        {
            SCode optRand0 = this.rand0.Bind (ctenv);
            SCode optRand1 = this.rand1.Bind (ctenv);
            return 
                
                //optRand1 is Argument ? PrimitiveCombination2SA.Make (rator, optRand0, (Argument) optRand1)
                //: optRand1 is LexicalVariable ? PrimitiveCombination2SL.Make (rator, optRand0, (LexicalVariable) optRand1)
                //: optRand1 is Quotation ?  PrimitiveCombination2SQ.Make (rator, optRand0, (Quotation)optRand1)
                //(Configuration.EnableSuperOperators && optRand0 is LexicalVariable) ? PrimitiveCombination2LS.Make (rator, (LexicalVariable) optRand0, optRand1)
                //: (Configuration.EnableSuperOperators && optRand0 is Quotation) ? PrimitiveCombination2QS.Make (rator, (Quotation) optRand0, optRand1)
                //: (Configuration.EnableSuperOperators && optRand1 is Quotation) ? PrimitiveCombination2SQ.Make (rator, optRand0, (Quotation) optRand1)
                //: 
                (optRand0 == this.rand0 && optRand1 == this.rand1) ? this
                : PrimitiveCombination2.Make (this.rator, optRand0, optRand1);
        }


        public override bool CallsTheEnvironment ()
        {
            return this.rand0.CallsTheEnvironment ()
                || this.rand1.CallsTheEnvironment ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.rand0);
            noteCalls (this.rand1);
            histogram.Note (this.histogramKey);
            ratorHistogram.Note (this.rator);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            unev = this.rand0;
            env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame1 (this, environment, ev1));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
            Debug.WriteLineIf (Primitive.Noisy, this.rator.ToString ());
#endif
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
                    expression = tci.Expression;
                    environment = tci.Environment;
                    return true;
                }
                else
                    throw new NotImplementedException ();
            }
            else return false;
        }

        public override bool MutatesAny (object [] formals)
        {
            return this.rand0.MutatesAny (formals)
                || this.rand1.MutatesAny (formals);
        }


        public override bool UsesAny (object [] formals)
        {
            return this.rand0.UsesAny (formals)
                || this.rand1.UsesAny (formals);

        }

        #region ISystemHunk3 Members
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemHunk3Cxr0
        {
            get
            {
                return UnwrapQuoted (this.rator);
            }
            set
            {
                throw new NotImplementedException ();
            }
        }
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemHunk3Cxr1
        {
            get
            {
                return UnwrapQuoted (this.rand0);
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
                return UnwrapQuoted (this.rand1);
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        #endregion

        public override SCode Alpha (object from, object to)
        {
            throw new NotImplementedException ();
        }

        public override bool IsLetrecBody (object [] formals, object [] remainingFormals)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    sealed class PrimitiveCombination2Frame0 : SubproblemContinuation<PrimitiveCombination2>, ISystemVector
    {

        public PrimitiveCombination2Frame0 (PrimitiveCombination2 expression, Environment environment)
            : base (expression, environment)
        {
        }

        #region ISystemVector Members

        public int SystemVectorSize
        {
            get { throw new NotImplementedException (); }
        }

        public object SystemVectorRef (int index)
        {
            throw new NotImplementedException ();
        }

        public object SystemVectorSet (int index, object newValue)
        {
            throw new NotImplementedException ();
        }

        #endregion

        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object value)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    sealed class PrimitiveCombination2Frame1 : SubproblemContinuation<PrimitiveCombination2>, ISystemVector
    {
        readonly object ev1;

        internal PrimitiveCombination2Frame1 (PrimitiveCombination2 expression, Environment environment, object ev1)
            : base (expression, environment)
        {
            this.ev1 = ev1;
        }

        #region ISystemVector Members

        public int SystemVectorSize
        {
            get { throw new NotImplementedException (); }
        }

        public object SystemVectorRef (int index)
        {
            throw new NotImplementedException ();
        }

        public object SystemVectorSet (int index, object newValue)
        {
            throw new NotImplementedException ();
        }

        #endregion

        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object value)
        {
            throw new NotImplementedException ();
        }
    }

//    [Serializable]
//    class PrimitiveCombination2QS : PrimitiveCombination2
//    {
//        object quoted;

//        PrimitiveCombination2QS (Primitive2 rator, Quotation rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.quoted = rand0.Quoted;
//        }

//        public static SCode Make (Primitive2 rator, Quotation rand0, SCode rand1)
//        {
//            return 
//                (rand1 is Argument) ? PrimitiveCombination2QA.Make (rator,  rand0, (Argument) rand1)
//                 : new PrimitiveCombination2QS (rator, rand0, rand1);
//        }

//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ("Should need binding.");
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            noteCalls (this.rand1);
//#endif

//            Control unev = this.rand1;
//            Environment env = environment;
//            object ev1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Q2Frame1 (this, environment));
//                //answer = Interpreter.UnwindStack;
//                //environment = env;
//                //return false;
//            }

//            // It is expensive to bounce down to invoke the procedure
//            // we invoke it directly and pass along the ref args.
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.rator);
//#endif
//            if (this.method (out answer, this.quoted, ev1)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null;
//                    expression = tci.Expression;
//                    environment = tci.Environment;
//                    return true;
//                }
//                else
//                    throw new NotImplementedException ();
//            }
//            else return false;
//        }

//    }

//    class PrimitiveCombination2QA : PrimitiveCombination2
//    {
//        protected readonly object quoted;
//        readonly int argOffset;

//        protected PrimitiveCombination2QA (Primitive2 rator, Quotation rand0, Argument rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.quoted = rand0.Quoted;
//            this.argOffset = rand1.Offset;
//        }

//        public static SCode Make (Primitive2 rator, Quotation rand0, Argument rand1)
//        {
//            return (rand1 is Argument0) ? PrimitiveCombination2QA0.Make (rator, rand0, (Argument0) rand1)
//                : (rand1 is Argument1) ? PrimitiveCombination2QA1.Make (rator, rand0, (Argument1) rand1)
//                :  new PrimitiveCombination2QA (rator, rand0, rand1);
//        }

//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ("Should need binding.");
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//#endif
//            // It is expensive to bounce down to invoke the procedure
//            // we invoke it directly and pass along the ref args.
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.rator);
//#endif
//            if (this.method (out answer, this.quoted, environment.ArgumentValue(this.argOffset))) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null;
//                    expression = tci.Expression;
//                    environment = tci.Environment;
//                    return true;
//                }
//                else
//                    throw new NotImplementedException ();
//            }
//            else return false;
//        }

//    }

//    class PrimitiveCombination2QA0 : PrimitiveCombination2QA
//    {
//        PrimitiveCombination2QA0 (Primitive2 rator, Quotation rand0, Argument0 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Primitive2 rator, Quotation rand0, Argument0 rand1)
//        {
//            return  new PrimitiveCombination2QA0 (rator, rand0, rand1);
//        }

//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ("Should need binding.");
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//#endif
//            // It is expensive to bounce down to invoke the procedure
//            // we invoke it directly and pass along the ref args.
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.rator);
//#endif
//            if (this.method (out answer, this.quoted, environment.Argument0Value)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null;
//                    expression = tci.Expression;
//                    environment = tci.Environment;
//                    return true;
//                }
//                else
//                    throw new NotImplementedException ();
//            }
//            else return false;
//        }

//    }

//    class PrimitiveCombination2QA1 : PrimitiveCombination2QA
//    {
//        PrimitiveCombination2QA1 (Primitive2 rator, Quotation rand0, Argument1 rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Primitive2 rator, Quotation rand0, Argument1 rand1)
//        {
//            return  new PrimitiveCombination2QA1 (rator, rand0, rand1);
//        }

//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ("Should need binding.");
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//#endif
//            // It is expensive to bounce down to invoke the procedure
//            // we invoke it directly and pass along the ref args.
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.rator);
//#endif
//            if (this.method (out answer, this.quoted, environment.Argument1Value)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null;
//                    expression = tci.Expression;
//                    environment = tci.Environment;
//                    return true;
//                }
//                else
//                    throw new NotImplementedException ();
//            }
//            else return false;
//        }

//    }

    [Serializable]
    class PrimitiveCombination2AS : PrimitiveCombination2
    {
        int a0offset;

        PrimitiveCombination2AS (Primitive2 rator, Argument rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.a0offset = rand0.Offset;
        }

        public static SCode Make (Primitive2 rator, Argument rand0, SCode rand1)
        {
            return  new PrimitiveCombination2AS (rator, rand0, rand1);
        }

        public override SCode Bind (LexicalMap ctenv)
        {
            throw new NotImplementedException ("Should need binding.");
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.rand1);
#endif
            //Control unev = this.rand1;
            //Environment env = environment;
            //object ev1 = null;
            //while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            //if (ev1 == Interpreter.UnwindStack) {
            //    ((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
            //    answer = Interpreter.UnwindStack;
            //    environment = env;
            //    return false;
            //}

            Control unev = this.rand1;
            Environment env = environment;
            object ev0 = environment.ArgumentValue (this.a0offset);
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Q2Frame1 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
#endif
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
                    expression = tci.Expression;
                    environment = tci.Environment;
                    return true;
                }
                else
                    throw new NotImplementedException ();
            }
            else return false;
        }

    }


    [Serializable]
    class PrimitiveCombination2SA : PrimitiveCombination2
    {
        int a1offset;

        PrimitiveCombination2SA (Primitive2 rator, SCode rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.a1offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Argument rand1)
        {
            return new PrimitiveCombination2SA (rator, rand0, rand1);
        }

        public override SCode Bind (LexicalMap ctenv)
        {
            throw new NotImplementedException ("Should need binding.");
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.rand0);
#endif
            //Control unev = this.rand1;
            //Environment env = environment;
            //object ev1 = null;
            //while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            //if (ev1 == Interpreter.UnwindStack) {
            //    ((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
            //    answer = Interpreter.UnwindStack;
            //    environment = env;
            //    return false;
            //}

            Control unev = this.rand0;
            Environment env = environment;
            object ev1 = environment.ArgumentValue (this.a1offset);
            object ev0 = null;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Q2Frame1 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
#endif
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
                    expression = tci.Expression;
                    environment = tci.Environment;
                    return true;
                }
                else
                    throw new NotImplementedException ();
            }
            else return false;
        }

    }

    [Serializable]
    class PrimitiveCombination2AA : PrimitiveCombination2
    {
        int a0offset;
        int a1offset;

        PrimitiveCombination2AA (Primitive2 rator, Argument rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.a0offset = rand0.Offset;
            this.a1offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Argument rand0, Argument rand1)
        {
            return new PrimitiveCombination2AA (rator, rand0, rand1);
        }

        public override SCode Bind (LexicalMap ctenv)
        {
            throw new NotImplementedException ("Should need binding.");
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif


            object ev1 = environment.ArgumentValue (this.a1offset);
            object ev0 = environment.ArgumentValue (this.a0offset);

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            Primitive.hotPrimitives.Note (this.rator);
#endif
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
                    expression = tci.Expression;
                    environment = tci.Environment;
                    return true;
                }
                else
                    throw new NotImplementedException ();
            }
            else return false;
        }

    }


//    [Serializable]
//    class PrimitiveCombination2SL : PrimitiveCombination2
//    {
//        protected readonly string l2name;
//        protected readonly int l2depth;
//        protected readonly int l2offset;

//        protected PrimitiveCombination2SL (Primitive2 rator, SCode rand0, LexicalVariable rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.l2name = rand1.varname;
//            this.l2depth = rand1.Depth;
//            this.l2offset = rand1.Offset;
//        }

//        public static SCode Make (Primitive2 rator, SCode rand0, LexicalVariable rand1)
//        {
//            return (rand0 is Argument) ? PrimitiveCombination2AL.Make (rator, (Argument)rand0, rand1)
//                : new PrimitiveCombination2SL (rator, rand0, rand1);
//        }

//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ("Should need binding.");
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            noteCalls (this.rand0);
//#endif
//            //Control unev = this.rand1;
//            //Environment env = environment;
//            //object ev1 = null;
//            //while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            //if (ev1 == Interpreter.UnwindStack) {
//            //    ((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
//            //    answer = Interpreter.UnwindStack;
//            //    environment = env;
//            //    return false;
//            //}

//            Control unev = this.rand0;
//            Environment env = environment;
//            object ev1 = null;
//            if (environment.FastLexicalRef (out ev1, this.l2name, this.l2depth, this.l2offset))
//                throw new NotImplementedException ();
//            object ev0 = null;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            if (ev0 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Q2Frame1 (this, environment));
//                //answer = Interpreter.UnwindStack;
//                //environment = env;
//                //return false;
//            }

//            // It is expensive to bounce down to invoke the procedure
//            // we invoke it directly and pass along the ref args.
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.rator);
//#endif
//            if (this.method (out answer, ev0, ev1)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null;
//                    expression = tci.Expression;
//                    environment = tci.Environment;
//                    return true;
//                }
//                else
//                    throw new NotImplementedException ();
//            }
//            else return false;
//        }

//    }

//    [Serializable]
//    class PrimitiveCombination2LS : PrimitiveCombination2
//    {
//        protected readonly object l0name;
//        protected readonly int l0depth;
//        protected readonly int l0offset;

//        protected PrimitiveCombination2LS (Primitive2 rator, LexicalVariable rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.l0name = rand0.Name;
//            this.l0depth = rand0.Depth;
//            this.l0offset = rand0.Offset;
//        }

//        public static SCode Make (Primitive2 rator, LexicalVariable rand0, SCode rand1)
//        {
//            return
//                (rand1 is Argument) ? PrimitiveCombination2LA.Make (rator, rand0, (Argument) rand1)
//                : (rand1 is Quotation) ? PrimitiveCombination2LQ.Make (rator, rand0, (Quotation) rand1)
//                : new PrimitiveCombination2LS (rator, rand0, rand1);
//        }

//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ("Should not need binding.");
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            noteCalls (this.rand1);
//#endif
//            Control unev = this.rand1;
//            Environment env = environment;

//            object ev1 = null;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Q2Frame1 (this, environment));
//                //answer = Interpreter.UnwindStack;
//                //environment = env;
//                //return false;
//            }

//            object ev0 = null;
//            if (environment.FastLexicalRef (out ev0, this.l0name, this.l0depth, this.l0offset))
//                throw new NotImplementedException ();
//            // It is expensive to bounce down to invoke the procedure
//            // we invoke it directly and pass along the ref args.
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.rator);
//#endif
//            if (this.method (out answer, ev0, ev1)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null;
//                    expression = tci.Expression;
//                    environment = tci.Environment;
//                    return true;
//                }
//                else
//                    throw new NotImplementedException ();
//            }
//            else return false;
//        }
//    }

//    [Serializable]
//    class PrimitiveCombination2SQ : PrimitiveCombination2
//    {
//#if DEBUG
//        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
//        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
//#endif
//        protected readonly object unquotedRand1;

//        PrimitiveCombination2SQ (Primitive2 rator, SCode rand0, Quotation rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.unquotedRand1 = rand1.Quoted;
//        }

//        public static SCode Make (Primitive2 rator, SCode rand0, Quotation rand1)
//        {
//            return (rand0 is Argument) ? PrimitiveCombination2AQ.Make (rator, (Argument) rand0, rand1)
//                //: (rand0 is LexicalVariable) ? PrimitiveCombination2LQ.Make (rator, (LexicalVariable) rand0, rand1)
//                : new PrimitiveCombination2SQ (rator, rand0, rand1);
//        }

//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ("Should not need binding.");
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            noteCalls (this.rand0);
//            ratorHistogram.Note (this.rator);
//            rand0TypeHistogram.Note (this.rand0Type);
//#endif
//            Control unev = this.rand0;
//            Environment env = environment;
//            object ev0 = null;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            if (ev0 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Q2Frame1 (this, environment));
//                //answer = Interpreter.UnwindStack;
//                //environment = env;
//                //return false;
//            }

//            // It is expensive to bounce down to invoke the procedure
//            // we invoke it directly and pass along the ref args.
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.rator);
//#endif
//            if (this.method (out answer, ev0, this.unquotedRand1)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null;
//                    expression = tci.Expression;
//                    environment = tci.Environment;
//                    return true;
//                }
//                else
//                    throw new NotImplementedException ();
//            }
//            else return false;
//        }

//    }

//    [Serializable]
//    sealed class PrimitiveCombination2AL : PrimitiveCombination2
//    {
//        readonly int a0offset;
//        readonly string l1name;
//        readonly int l1depth;
//        readonly int l1offset;

//        PrimitiveCombination2AL (Primitive2 rator, Argument rand0, LexicalVariable rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.a0offset = rand0.Offset;
//            this.l1name = rand1.varname;
//            this.l1depth = rand1.Depth;
//            this.l1offset = rand1.Offset;
//        }

//        public static SCode Make (Primitive2 rator, Argument rand0, LexicalVariable rand1)
//        {
//            return new PrimitiveCombination2AL (rator, rand0, rand1);
//        }

//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ("Should need binding.");
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            Primitive.hotPrimitives.Note (this.rator);
//#endif
//            object ev2 = null;
//            if (environment.FastLexicalRef (out ev2, this.l1name, this.l1depth, this.l1offset))
//                throw new NotImplementedException ();
//            if (this.method (out answer, environment.ArgumentValue (this.a0offset), ev2)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null;
//                    expression = tci.Expression;
//                    environment = tci.Environment;
//                    return true;
//                }
//                else
//                    throw new NotImplementedException ();
//            }
//            else return false;
//        }
//    }

//    [Serializable]
//    class PrimitiveCombination2AQ : PrimitiveCombination2
//    {
//        #if DEBUG
//        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
//#endif
//        readonly int a1offset;
//        protected readonly object unquotedRand1;

//        protected PrimitiveCombination2AQ (Primitive2 rator, Argument rand0, Quotation rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.a1offset = rand0.Offset;
//            this.unquotedRand1 = rand1.Quoted;
//        }

//        public static SCode Make (Primitive2 rator, Argument rand0, Quotation rand1)
//        {
//            return (rand0 is Argument0) ? PrimitiveCombination2A0Q.Make (rator, (Argument0) rand0, rand1)
//                : (rand0 is Argument1) ? PrimitiveCombination2A1Q.Make (rator, (Argument1) rand0, rand1)
//                :
//            new PrimitiveCombination2AQ (rator, rand0, rand1);
//        }

//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ("Should need binding.");
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            Primitive.hotPrimitives.Note (this.rator);
//            ratorHistogram.Note (this.rator);
//#endif
//            if (this.method (out answer, environment.ArgumentValue (this.a1offset), this.unquotedRand1)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null;
//                    expression = tci.Expression;
//                    environment = tci.Environment;
//                    return true;
//                }
//                else
//                    throw new NotImplementedException ();
//            }
//            else return false;
//        }
//    }

//    [Serializable]
//    class PrimitiveCombination2A0Q : PrimitiveCombination2AQ
//    {
//#if DEBUG
//        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
//#endif

//        PrimitiveCombination2A0Q (Primitive2 rator, Argument0 rand0, Quotation rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Primitive2 rator, Argument0 rand0, Quotation rand1)
//        {
//            return  new PrimitiveCombination2A0Q (rator, rand0, rand1);
//        }

//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ("Should need binding.");
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            Primitive.hotPrimitives.Note (this.rator);
//            ratorHistogram.Note (this.rator);
//#endif
//            if (this.method (out answer, environment.Argument0Value, this.unquotedRand1)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null;
//                    expression = tci.Expression;
//                    environment = tci.Environment;
//                    return true;
//                }
//                else
//                    throw new NotImplementedException ();
//            }
//            else return false;
//        }
//    }
 
//    [Serializable]
//    class PrimitiveCombination2A1Q : PrimitiveCombination2AQ
//    {
//#if DEBUG
//        static Histogram<Primitive2> ratorHistogram = new Histogram<Primitive2> ();
//#endif

//        PrimitiveCombination2A1Q (Primitive2 rator, Argument1 rand0, Quotation rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (Primitive2 rator, Argument1 rand0, Quotation rand1)
//        {
//            return new PrimitiveCombination2A1Q (rator, rand0, rand1);
//        }

//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ("Should need binding.");
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            Primitive.hotPrimitives.Note (this.rator);
//            ratorHistogram.Note (this.rator);
//#endif
//            if (this.method (out answer, environment.Argument1Value, this.unquotedRand1)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null;
//                    expression = tci.Expression;
//                    environment = tci.Environment;
//                    return true;
//                }
//                else
//                    throw new NotImplementedException ();
//            }
//            else return false;
//        }
//    }

//    [Serializable]
//    sealed class PrimitiveCombination2LA : PrimitiveCombination2
//    {
//        object l1name;
//        int l1depth;
//        int l1offset;
//        int a2offset;

//        PrimitiveCombination2LA (Primitive2 rator, LexicalVariable rand0, Argument rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.l1name = rand0.Name;
//            this.l1depth = rand0.Depth;
//            this.l1offset = rand0.Offset;
//            this.a2offset = rand1.Offset;
//        }

//        public static SCode Make (Primitive2 rator, LexicalVariable rand0, Argument rand1)
//        {
//            return new PrimitiveCombination2LA (rator, rand0, rand1);
//        }

//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ("Should not need binding.");
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            Primitive.hotPrimitives.Note (this.rator);
//#endif
//            object l1value = null;
//            if (environment.FastLexicalRef (out l1value, this.l1name, this.l1depth, this.l1offset))
//                throw new NotImplementedException ();

//            if (this.method (out answer, l1value, environment.ArgumentValue (this.a2offset))) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null;
//                    expression = tci.Expression;
//                    environment = tci.Environment;
//                    return true;
//                }
//                else
//                    throw new NotImplementedException ();
//            }
//            else return false;
//        }
//    }

//    [Serializable]
//    class PrimitiveCombination2LQ : PrimitiveCombination2
//    {
//#if DEBUG
//        [NonSerialized]
//        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
//#endif
//        readonly object l1name;
//        readonly int l1depth;
//        readonly int l1offset;
//        readonly object a2quoted;

//        PrimitiveCombination2LQ (Primitive2 rator, LexicalVariable rand0, Quotation rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.l1name = rand0.Name;
//            this.l1depth = rand0.Depth;
//            this.l1offset = rand0.Offset;
//            this.a2quoted = rand1.Quoted;
//        }

//        public static SCode Make (Primitive2 rator, LexicalVariable rand0, Quotation rand1)
//        {
//            return new PrimitiveCombination2LQ (rator, rand0, rand1);
//        }

//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ("Should need binding.");
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            Primitive.hotPrimitives.Note (this.rator);
//            procedureHistogram.Note (this.rator);
//#endif
//            object l1value = null;
//            if (environment.FastLexicalRef (out l1value, this.l1name, this.l1depth, this.l1offset))
//                throw new NotImplementedException ();

//            if (this.method (out answer, l1value, this.a2quoted)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    answer = null;
//                    expression = tci.Expression;
//                    environment = tci.Environment;
//                    return true;
//                }
//                else
//                    throw new NotImplementedException ();
//            }
//            else return false;
//        }
//    }

//    [Serializable]
//    sealed class PrimitiveEQ2 : SCode, ISystemHunk3
//    {
//#if DEBUG

//        [NonSerialized]
//        static Dictionary<Type, long> histogram0 = new Dictionary<Type, long> ();

//        [NonSerialized]
//        static Dictionary<Type, long> histogram1 = new Dictionary<Type, long> ();
//#endif

//        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
//        readonly Primitive2 rator;

//        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
//        [NonSerialized]
//        PrimitiveMethod2 method;

//        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
//        readonly SCode rand0;

//        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
//        readonly SCode rand1;

//        PrimitiveEQ2 (Primitive2 rator, SCode rand0, SCode rand1)
//            : base (TC.PCOMB2)
//        {
//            this.rator = rator;
//            this.method = rator.Method;
//            this.rand0 = rand0;
//            this.rand1 = rand1;
//        }

//        [OnDeserialized ()]
//        internal void OnDeserializedMethod (StreamingContext context)
//        {
//            this.method = rator.Method;
//        }

//        public Primitive2 Rator
//        {
//            [DebuggerStepThrough]
//            get
//            {
//                return this.rator;
//            }
//        }

//        public SCode Rand0
//        {
//            [DebuggerStepThrough]
//            get
//            {
//                return this.rand0;
//            }
//        }

//        public SCode Rand1
//        {
//            [DebuggerStepThrough]
//            get
//            {
//                return this.rand1;
//            }
//        }

//        public static SCode Make (Primitive2 rator, object rand0, object rand1)
//        {
//            if (rator == null)
//                throw new ArgumentNullException ("rator");


//                //if (rand1 is int && (int) rand1 == 1) Debugger.Break ();
//                SCode srand0 = EnsureSCode (rand0);
//                SCode srand1 = EnsureSCode (rand1);
//            return new PrimitiveEQ2 (rator, srand0, srand1);
//        }

//        public static SCode Make (Hunk3 init)
//        {
//            //Primitive2 rator = (Primitive2) init.Cxr0;
//            //if (rator == Primitive.Find ("EQ?", 2))
//            //    return new PrimitiveEq (init.Cxr1, init.Cxr2);
//            return Make ((Primitive2) init.Cxr0, init.Cxr1, init.Cxr2);
//        }

//        public override SCode Bind (LexicalMap ctenv)
//        {
//            SCode optRand0 = this.rand0.Bind (ctenv);
//            SCode optRand1 = this.rand1.Bind (ctenv);
//            //if (optRand0 is Argument0)
//            //    return new PrimitiveCombination2LA0 (this.rator, optRand0, optRand1);
//            //else
//            return optRand0 == this.rand0
//                && optRand1 == this.rand1
//                ? this
//                : new PrimitiveEQ2 (this.rator, optRand0, optRand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            noteCalls (this.rand0);
//            noteCalls (this.rand1);
//#endif
//            Control unev = this.rand1;
//            Environment env = environment;
//            object ev1 = null;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new PrimitiveEQ2Frame0 (this, environment));
//                answer = Interpreter.UnwindStack;
//                environment = env;
//                return false;
//            }

//            unev = this.rand0;
//            env = environment;
//            object ev0 = null;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            if (ev0 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new PrimitiveEQ2Frame1 (this, environment, ev1));
//                answer = Interpreter.UnwindStack;
//                environment = env;
//                return false;
//            }

//            // It is expensive to bounce down to invoke the procedure
//            // we invoke it directly and pass along the ref args.
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.rator);
//#endif
//            return ObjectModel.Eq (out answer, ev0, ev1);
//        }

//        public override bool MutatesAny (object [] formals)
//        {
//            return this.rand0.MutatesAny (formals)
//                || this.rand1.MutatesAny (formals);
//        }

//         public override bool CallsTheEnvironment ()
//        {
//            throw new NotImplementedException ();
//        }

//        public override bool UsesAny (object [] formals)
//        {
//            throw new NotImplementedException ();
//        }
//       #region ISystemHunk3 Members
//        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
//        public object SystemHunk3Cxr0
//        {
//            get
//            {
//                return UnwrapQuoted (this.rator);
//            }
//            set
//            {
//                throw new NotImplementedException ();
//            }
//        }
//        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
//        public object SystemHunk3Cxr1
//        {
//            get
//            {
//                return UnwrapQuoted (this.rand0);
//            }
//            set
//            {
//                throw new NotImplementedException ();
//            }
//        }
//        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
//        public object SystemHunk3Cxr2
//        {
//            get
//            {
//                return UnwrapQuoted (this.rand1);
//            }
//            set
//            {
//                throw new NotImplementedException ();
//            }
//        }

//        #endregion

//    }

//    [Serializable]
//    sealed class PrimitiveEQ2Frame0 : SubproblemContinuation<PrimitiveEQ2>, ISystemVector
//    {

//        public PrimitiveEQ2Frame0 (PrimitiveEQ2 expression, Environment environment)
//            : base (expression, environment)
//        {
//        }

//        #region ISystemVector Members

//        public int SystemVectorSize
//        {
//            get { throw new NotImplementedException (); }
//        }

//        public object SystemVectorRef (int index)
//        {
//            throw new NotImplementedException ();
//        }

//        public object SystemVectorSet (int index, object newValue)
//        {
//            throw new NotImplementedException ();
//        }

//        #endregion

//        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object value)
//        {
//            throw new NotImplementedException ();
//        }
//    }

//    [Serializable]
//    sealed class PrimitiveEQ2Frame1 : SubproblemContinuation<PrimitiveEQ2>, ISystemVector
//    {
//        readonly object ev1;

//        internal PrimitiveEQ2Frame1 (PrimitiveEQ2 expression, Environment environment, object ev1)
//            : base (expression, environment)
//        {
//            this.ev1 = ev1;
//        }

//        #region ISystemVector Members

//        public int SystemVectorSize
//        {
//            get { throw new NotImplementedException (); }
//        }

//        public object SystemVectorRef (int index)
//        {
//            throw new NotImplementedException ();
//        }

//        public object SystemVectorSet (int index, object newValue)
//        {
//            throw new NotImplementedException ();
//        }

//        #endregion

//        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object value)
//        {
//            throw new NotImplementedException ();
//        }
//    }
}
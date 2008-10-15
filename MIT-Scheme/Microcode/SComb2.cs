using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Runtime.Serialization;

namespace Microcode
{
    [Serializable]
    class Combination2 : SCode, ISystemHunk3
    {
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<string> histogram = new Histogram<String> ();
        protected readonly string histogramKey;

        protected readonly Type ratorType;
        protected readonly Type rand0Type;
        protected readonly Type rand1Type;
#endif
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly SCode rator;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly SCode rand0;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly SCode rand1;

        protected Combination2 (SCode rator, SCode rand0, SCode rand1)
            : base (TC.COMBINATION_2)
        {
            this.rator = rator;
            this.rand0 = rand0;
            this.rand1 = rand1;
#if DEBUG
            this.histogramKey = rator.GetType().ToString () + " " + rand0.GetType ().Name.ToString () + " " + rand1.GetType ().Name.ToString ();
            ratorType = rator.GetType ();
            rand0Type = rand0.GetType ();
            rand1Type = rand1.GetType ();
#endif
        }

        public static SCode Make (SCode rator, SCode rand0, SCode rand1)
        {            
            Lambda lrator = rator as Lambda;


            return (Configuration.EnableSuperOperators
                    && rand0 == Quotation.Unassigned
                    && rand1 == Quotation.Unassigned
                    && lrator != null
                    && lrator.IsLetrecLambda ()) ? Letrec2.Make (lrator)
                : (Configuration.EnableSuperOperators && rator is Argument && rand0 is Argument && rand1 is Argument) ? Combination2AAA.Make ((Argument) rator, (Argument) rand0, (Argument) rand1)
                : (Configuration.EnableSuperOperators && rator is Argument && rand0 is Argument) ? Combination2AAS.Make ((Argument) rator, (Argument) rand0, rand1)
                : (Configuration.EnableSuperOperators && rator is Argument && rand1 is Argument) ? Combination2ASA.Make ((Argument) rator, rand0, (Argument) rand1)
                : (Configuration.EnableSuperOperators && rand0 is Argument && rand1 is Argument) ? Combination2SAA.Make (rator, (Argument) rand0, (Argument) rand1)
                : (Configuration.EnableSuperOperators && rator is Argument) ? Combination2ASS.Make ((Argument) rator, rand0, rand1)
                : (Configuration.EnableSuperOperators && rand0 is Argument) ? Combination2SAS.Make (rator, (Argument) rand0, rand1)
                : (Configuration.EnableSuperOperators && rand1 is Argument) ? Combination2SSA.Make (rator, rand0, (Argument) rand1)
                : new Combination2 (rator, rand0, rand1);
        }

        public static SCode Make (object rator, object rand0, object rand1)
        {
            SCode srator = EnsureSCode (rator);
            SCode srand0 = EnsureSCode (rand0);
            SCode srand1 = EnsureSCode (rand1);
            return Make (srator, srand0, srand1);
        }

        public static SCode Make (Hunk3 init)
        {
            return Make (init.Cxr0, init.Cxr1, init.Cxr2);
        }

        public SCode Operand0
        {
            [DebuggerStepThrough]
            get
            {
                return this.rand0;
            }
        }

        public SCode Operand1
        {
            [DebuggerStepThrough]
            get
            {
                return this.rand1;
            }
        }

        public SCode Operator
        {
            [DebuggerStepThrough]
            get
            {
                return this.rator;
            }
        }

        [SchemePrimitive ("COMBINATION2?", 1, true)]
        public static bool IsCombination2 (out object answer, object arg)
        {
            answer = arg is Combination2;
            return false;
        }

        #region ISystemHunk3 Members
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemHunk3Cxr0
        {
            get
            {
                return UnwrapQuoted (rator);
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
                return UnwrapQuoted (rand0);
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
                return UnwrapQuoted (rand1);
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        #endregion

        public override SCode Bind (LexicalMap ctenv)
        {
            SCode optRator = this.rator.Bind (ctenv);
            SCode optRand0 = this.rand0.Bind (ctenv);
            SCode optRand1 = this.rand1.Bind (ctenv);
            return  (optRator == this.rator && optRand0 == this.rand0 && optRand1 == this.rand1) ? this
                : Combination2.Make (optRator, optRand0, optRand1);
        }
        
        public override bool CallsTheEnvironment ()
        {
            return this.rator.CallsTheEnvironment ()
                || this.rand0.CallsTheEnvironment ()
                || this.rand1.CallsTheEnvironment ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.rator);
            noteCalls (this.rand0);
            noteCalls (this.rand1);
            histogram.Note (this.histogramKey);
            ratorTypeHistogram.Note (this.ratorType);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
#endif

            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object ev0;
            env = environment;
            unev = this.rand0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object evop;
            env = environment;
            unev = this.rator;
            while (unev.EvalStep (out evop, ref unev, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, ev0, ev1));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            // expression = (SCode) evop;
            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }

        public override bool MutatesAny (object [] formals)
        {
            return this.rator.MutatesAny (formals)
                || this.rand0.MutatesAny (formals)
                || this.rand1.MutatesAny (formals);
        }

        public override bool UsesAny (object [] formals)
        {
            return this.rator.UsesAny (formals)
                || this.rand0.UsesAny (formals)
                || this.rand1.UsesAny (formals);
        }

        public override SCode Alpha (object from, object to)
        {
            SCode arator = this.rator.Alpha (from, to);
            SCode arand0 = this.rand0.Alpha (from, to);
            SCode arand1 = this.rand1.Alpha (from, to);
            return (arator == this.rator && arand0 == this.rand0 && arand1 == this.rand1) 
                ? this 
                : Combination2.Make (arator, arand0, arand1);
        }

        public override bool IsLetrecBody (object [] formals, object [] remainingFormals)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class Combination2Frame0 : SubproblemContinuation<Combination2>, ISystemVector
    {

        public Combination2Frame0 (Combination2 combination2, Environment environment)
            : base (combination2, environment)
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
            object ev0 = null;
            Environment env = environment;
            Control unev0 = this.expression.Operand0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object evop = null;
            env = environment;
            Control unevop = this.expression.Operator;
            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, value);
        }
    }

    [Serializable]
    class Combination2Frame1 : SubproblemContinuation<Combination2>, ISystemVector
    {
        readonly object ev1;

        public Combination2Frame1 (Combination2 combination2, Environment environment, object ev1)
            : base (combination2, environment)
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
            object evop = null;
            Environment env = environment;
            Control unevop = this.expression.Operator;
            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, value, this.ev1);
        }
    }

    [Serializable]
    class Combination2Frame2 : SubproblemContinuation<Combination2>, ISystemVector
    {
        readonly object ev0;
        readonly object ev1;

        public Combination2Frame2 (Combination2 combination2, Environment environment, object ev0, object ev1)
            : base (combination2, environment)
        {
            this.ev0 = ev0;
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
            object evop = null;
            Environment env = environment;
            Control unevop = this.expression.Operator;
            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, value, this.ev0, this.ev1);
        }
    }

//    [Serializable]
//    class Combination2LSS : Combination2
//    {
//#if DEBUG
//        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        protected object ratorName;
//        protected int ratorDepth;
//        protected int ratorOffset;

//        protected Combination2LSS (LexicalVariable rator, SCode rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.ratorName = rator.Name;
//            this.ratorDepth = rator.Depth;
//            this.ratorOffset = rator.Offset;
//        }

//        public static SCode Make (LexicalVariable rator, SCode rand0, SCode rand1)
//        {
//            return new Combination2LSS (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            noteCalls (this.rand0);
//            noteCalls (this.rand1);
//            rand0TypeHistogram.Note (this.rand0Type);
//            rand1TypeHistogram.Note (this.rand1Type);
//#endif

//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination2LSSFrame0 (this, environment));
//                //environment = env;
//                //answer = Interpreter.UnwindStack;
//                //return false;
//            }

//            object ev0;
//            env = environment;
//            unev = this.rand0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            if (ev0 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination2LSSFrame1 (this, environment, ev1));
//                //environment = env;
//                //answer = Interpreter.UnwindStack;
//                //return false;
//            }

//            object evop;
//            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
//                throw new NotImplementedException ();

//            // expression = (SCode) evop;
//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    [Serializable]
//    class Combination2L1SS : Combination2LSS
//    {
//#if DEBUG
//        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        protected Combination2L1SS (LexicalVariable1 rator, SCode rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable1 rator, SCode rand0, SCode rand1)
//        {
//            return 
//                (rand0 is Argument) ? Combination2L1AS.Make (rator, (Argument) rand0, rand1)
//                : new Combination2L1SS (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            noteCalls (this.rand0);
//            noteCalls (this.rand1);
//            rand0TypeHistogram.Note (this.rand0Type);
//            rand1TypeHistogram.Note (this.rand1Type);
//#endif

//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
//                //environment = env;
//                //answer = Interpreter.UnwindStack;
//                //return false;
//            }

//            object ev0;
//            env = environment;
//            unev = this.rand0;
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            if (ev0 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
//                //environment = env;
//                //answer = Interpreter.UnwindStack;
//                //return false;
//            }

//            object evop = null;
//            if (environment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
//                throw new NotImplementedException ();

//            // expression = (SCode) evop;
//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
//        }
//    }

//    [Serializable]
//    class Combination2L1AS : Combination2LSS
//    {
//#if DEBUG
//        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        readonly int argOffset;

//        protected Combination2L1AS (LexicalVariable1 rator, Argument rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.argOffset = rand0.Offset;
//        }

//        public static SCode Make (LexicalVariable1 rator, Argument rand0, SCode rand1)
//        {
//            return
//                (rand0 is Argument0) ? Combination2L1A0S.Make (rator, (Argument0) rand0, rand1)
//                : (rand0 is Argument1) ? Combination2L1A1S.Make (rator, (Argument1) rand0, rand1)
//                : new Combination2L1AS (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            noteCalls (this.rand0);
//            noteCalls (this.rand1);
//            rand1TypeHistogram.Note (this.rand1Type);
//#endif

//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
//                //environment = env;
//                //answer = Interpreter.UnwindStack;
//                //return false;
//            }

//            object evop = null;
//            if (environment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
//                throw new NotImplementedException ();

//            // expression = (SCode) evop;
//            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.ArgumentValue(argOffset), ev1);
//        }
//    }

//        [Serializable]
//    class Combination2L1A0S : Combination2LSS
//    {
//#if DEBUG
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        protected Combination2L1A0S (LexicalVariable1 rator, Argument0 rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable1 rator, Argument0 rand0, SCode rand1)
//        {
//            return new Combination2L1A0S (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            noteCalls (this.rand1);
//            rand1TypeHistogram.Note (this.rand1Type);
//#endif

//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
//                //environment = env;
//                //answer = Interpreter.UnwindStack;
//                //return false;
//            }

//            object evop = null;
//            if (environment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
//                throw new NotImplementedException ();

//            // expression = (SCode) evop;
//            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument0Value, ev1);
//        }
//    }

//        [Serializable]
//    class Combination2L1A1S : Combination2LSS
//    {
//#if DEBUG
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//#endif
//        protected Combination2L1A1S (LexicalVariable1 rator, Argument1 rand0, SCode rand1)
//            : base (rator, rand0, rand1)
//        {
//        }

//        public static SCode Make (LexicalVariable1 rator, Argument1 rand0, SCode rand1)
//        {
//            return new Combination2L1A1S (rator, rand0, rand1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            noteCalls (this.rand1);
//            rand1TypeHistogram.Note (this.rand1Type);
//#endif

//            object ev1;
//            Environment env = environment;
//            Control unev = this.rand1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
//                //environment = env;
//                //answer = Interpreter.UnwindStack;
//                //return false;
//            }

//            object evop = null;
//            if (environment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
//                throw new NotImplementedException ();

//            // expression = (SCode) evop;
//            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument1Value, ev1);
//        }
//    }

[Serializable]
class Combination2AAA : Combination2
{
        protected int a1offset;
        
        Combination2AAA (Argument rator, Argument rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.a1offset = rand0.Offset;
        }

        public static SCode Make (Argument rator, Argument rand0, Argument rand1)
        {
            return new Combination2AAA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
throw new NotImplementedException();
    }
}

[Serializable]
class Combination2AAS : Combination2
{
#if DEBUG
    static Histogram<Type> rand1TypeHistogram = new Histogram<Type>();
#endif
    public int ratorOffset;
    public int rand0Offset;

    Combination2AAS (Argument rator, Argument rand0, SCode rand1)
        : base (rator, rand0, rand1)
    {
        this.ratorOffset = rator.Offset;
        this.rand0Offset = rand0.Offset;
    }

    public static SCode Make (Argument rator, Argument rand0, SCode rand1)
    {
        return new Combination2AAS (rator, rand0, rand1);
    }

    public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    {
#if DEBUG
        Warm ();
        noteCalls (this.rand1);
        rand1TypeHistogram.Note (this.rand1Type);
#endif

        object ev1;
        Environment env = environment;
        Control unev = this.rand1;
        while (unev.EvalStep (out ev1, ref unev, ref env)) { };
        if (ev1 == Interpreter.UnwindStack) {
            throw new NotImplementedException ();
            //((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
            //environment = env;
            //answer = Interpreter.UnwindStack;
            //return false;
        }

        object ev0 = environment.ArgumentValue (this.rand0Offset);

        object evop = environment.ArgumentValue (this.ratorOffset);

        return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
    }


}

[Serializable]
class Combination2ASA : Combination2
{
#if DEBUG
    static Histogram<Type> rand0TypeHistogram = new Histogram<Type>();
#endif
    public int ratorOffset;
    public int rand1Offset;

    Combination2ASA (Argument rator, SCode rand0, Argument rand1)
        : base (rator, rand0, rand1)
    {
        this.ratorOffset = rator.Offset;
        this.rand1Offset = rand1.Offset;
    }

    public static SCode Make (Argument rator, SCode rand0, Argument rand1)
    {
        return new Combination2ASA (rator, rand0, rand1);
    }

    public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    {
#if DEBUG
        Warm ();
        noteCalls (this.rand0);
        rand0TypeHistogram.Note (this.rand0Type);
#endif

        object ev1 = environment.ArgumentValue (this.rand1Offset);

        object ev0;
        Environment env = environment;
        Control unev = this.rand0;
        while (unev.EvalStep (out ev0, ref unev, ref env)) { };
        if (ev0 == Interpreter.UnwindStack) {
            throw new NotImplementedException ();
            //((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
            //environment = env;
            //answer = Interpreter.UnwindStack;
            //return false;
        }

        object evop = environment.ArgumentValue (this.ratorOffset);

        return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
    }
}

[Serializable]
class Combination2SAA: Combination2
{
#if DEBUG
    static Histogram<Type> procedureTypeHistogram = new Histogram<Type>();
#endif
    public int arg0Offset;
    public int arg1Offset;

    Combination2SAA (SCode rator, Argument rand0, Argument rand1)
        : base (rator, rand0, rand1)
    {
        this.arg0Offset = rand0.Offset;
        this.arg1Offset = rand1.Offset;
    }

    public static SCode Make (SCode rator, Argument rand0, Argument rand1)
    {
        return new Combination2SAA (rator, rand0, rand1);
    }

    public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    {
#if DEBUG
        Warm ();
        noteCalls (this.rator);
        procedureTypeHistogram.Note (this.ratorType);
#endif

        object ev1 = environment.ArgumentValue (this.arg1Offset);

        object ev0 = environment.ArgumentValue (this.arg0Offset);

        object evop;
        Environment env = environment;
        Control unev = this.rator;
        while (unev.EvalStep (out evop, ref unev, ref env)) { };
        if (evop == Interpreter.UnwindStack) {
            throw new NotImplementedException ();
            //((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, ev0, ev1));
            //environment = env;
            //answer = Interpreter.UnwindStack;
            //return false;
        }

        // expression = (SCode) evop;
        return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
    }


}

class Combination2SSA : Combination2
{
#if DEBUG
    static Histogram<Type> ratorTypeHistogram = new Histogram<Type>();
    static Histogram<Type> rand0TypeHistogram = new Histogram<Type>();
#endif

    public int arg1Offset;
    Combination2SSA (SCode rator, SCode rand0, Argument rand1)
        : base (rator, rand0, rand1)
    {
        this.arg1Offset = rand1.Offset;
    }

    public static SCode Make (SCode rator, SCode rand0, Argument rand1)
    {
        return new Combination2SSA (rator, rand0, rand1);
    }

    public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    {
#if DEBUG
        Warm ();
        noteCalls (this.rator);
        noteCalls (this.rand0);
        ratorTypeHistogram.Note (this.ratorType);
        rand0TypeHistogram.Note (this.rand0Type);
#endif

        object ev1 = environment.ArgumentValue (this.arg1Offset);

        object ev0;
        Environment env = environment;
        Control unev = this.rand0;
        while (unev.EvalStep (out ev0, ref unev, ref env)) { };
        if (ev0 == Interpreter.UnwindStack) {
            throw new NotImplementedException ();
            //((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
            //environment = env;
            //answer = Interpreter.UnwindStack;
            //return false;
        }

        object evop;
        env = environment;
        unev = this.rator;
        while (unev.EvalStep (out evop, ref unev, ref env)) { };
        if (evop == Interpreter.UnwindStack) {
            throw new NotImplementedException ();
            //((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, ev0, ev1));
            //environment = env;
            //answer = Interpreter.UnwindStack;
            //return false;
        }

        // expression = (SCode) evop;
        return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
    }


}

[Serializable]
class Combination2SAS : Combination2
{
#if DEBUG
    static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
    static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
    static Histogram<string> histogram = new Histogram<String> ();
#endif
    protected int a1offset;

    Combination2SAS (SCode rator, Argument rand0, SCode rand1)
        : base (rator, rand0, rand1)
    {
        this.a1offset = rand0.Offset;
    }

    public static SCode Make (SCode rator, Argument rand0, SCode rand1)
    {
        return new Combination2SAS (rator, rand0, rand1);
    }

    public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    {
#if DEBUG
        Warm ();
        noteCalls (this.rator);
        noteCalls (this.rand1);
        histogram.Note (this.histogramKey);
        ratorTypeHistogram.Note (this.ratorType);
        rand1TypeHistogram.Note (this.rand1Type);
#endif

        object ev1 = null;
        Environment env = environment;
        Control unev1 = this.rand1;
        while (unev1.EvalStep (out ev1, ref unev1, ref env)) { };
        if (ev1 == Interpreter.UnwindStack) {
            throw new NotImplementedException ();
            //((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
            //environment = env;
            //answer = Interpreter.UnwindStack;
            //return false;
        }

        object evop = null;
        env = environment;
        Control unevop = this.rator;
        while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
        if (evop == Interpreter.UnwindStack) {
            throw new NotImplementedException ();
            //((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, ev0, ev1));
            //environment = env;
            //answer = Interpreter.UnwindStack;
            //return false;
        }

        // expression = (SCode) evop;
        return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.ArgumentValue (a1offset), ev1);
    }
}

class Combination2ASS : Combination2
{
#if DEBUG
    static Histogram<Type> rand0TypeHistogram = new Histogram<Type>();
    static Histogram<Type> rand1TypeHistogram = new Histogram<Type>();
#endif
    public int pOffset;
    Combination2ASS (Argument rator, SCode rand0, SCode rand1)
        : base (rator, rand0, rand1)
    {
        this.pOffset = rator.Offset;
    }

    public static SCode Make (Argument rator, SCode rand0, SCode rand1)
    {
        return new Combination2ASS (rator, rand0, rand1);
    }

    public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
    {
#if DEBUG
        Warm ();
        noteCalls (this.rand0);
        noteCalls (this.rand1);
        rand0TypeHistogram.Note (this.rand0Type);
        rand1TypeHistogram.Note (this.rand1Type);
#endif

        object ev1;
        Environment env = environment;
        Control unev = this.rand1;
        while (unev.EvalStep (out ev1, ref unev, ref env)) { };
        if (ev1 == Interpreter.UnwindStack) {
            throw new NotImplementedException ();
            //((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
            //environment = env;
            //answer = Interpreter.UnwindStack;
            //return false;
        }

        object ev0;
        env = environment;
        unev = this.rand0;
        while (unev.EvalStep (out ev0, ref unev, ref env)) { };
        if (ev0 == Interpreter.UnwindStack) {
            throw new NotImplementedException ();
            //((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
            //environment = env;
            //answer = Interpreter.UnwindStack;
            //return false;
        }

        object evop = environment.ArgumentValue (this.pOffset);
        return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
    }


}

    [Serializable]
    class Letrec2 : Combination2
    {

        protected Letrec2 (LambdaBase rator)
            : base (rator, Quotation.Unassigned, Quotation.Unassigned)
        {
        }

        public static SCode Make (LambdaBase rator)
        {
            return new Letrec2 (rator);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif

            object evop = ((Lambda) this.rator).Close (environment);
            return Interpreter.Call (out answer, ref expression, ref environment, evop, ReferenceTrap.Unassigned, ReferenceTrap.Unassigned);
        }

        public override SCode Alpha (object from, object to)
        {
            SCode arator = this.rator.Alpha (from, to);
            SCode arand0 = this.rand0.Alpha (from, to);
            SCode arand1 = this.rand1.Alpha (from, to);
            return (arator == this.rator && arand0 == this.rand0 && arand1 == this.rand1)
                ? this
                : Combination2.Make (arator, arand0, arand1);
        }

        public override bool IsLetrecBody (object [] formals, object [] remainingFormals)
        {
            throw new NotImplementedException ();
        }
    }
}
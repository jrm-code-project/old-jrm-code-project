using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    [Serializable]
    class Combination3 : Combination
    {
#if DEBUG
        static Histogram<Type> c0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> c1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> c2TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> c3TypeHistogram = new Histogram<Type> ();
        protected Type c0Type;
        protected Type c1Type;
        protected Type c2Type;
        protected Type c3Type;
#endif
        protected Combination3 (SCode [] components)
            : base (components)
        {
#if DEBUG
            this.c0Type = components [0].GetType ();
            this.c1Type = components [1].GetType ();
            this.c2Type = components [2].GetType ();
            this.c3Type = components [3].GetType ();
#endif
        }
        protected Combination3 (SCode rator, SCode rand0, SCode rand1, SCode rand2)
            : this (new SCode [] { rator, rand0, rand1, rand2 })
        {
        }

        public static SCode Make (SCode rator, SCode rand0, SCode rand1, SCode rand2)
        {
            return
                //(rand2 is Argument) ? Combination3SSSA.Make (rator, rand0, rand1, (Argument) rand2)
                //(rand2 is Quotation) ? Combination3SSSQ.Make (rator, rand0, rand1, (Quotation) rand2)
                //(rator is Argument) ? Combination3A.Make ((Argument) rator, rand0, rand1, rand2) :
                //(rator is FreeVariable) ? Combination3F.Make ((FreeVariable) rator, rand0, rand1, rand2) :
                //(rator is Quotation) ? Combination3Q.Make ((Quotation) rator, rand0, rand1, rand2) :
                //(rator is TopLevelVariable) ? Combination3T.Make ((TopLevelVariable) rator, rand0, rand1, rand2) :
                //(rand0 is Argument) ? Combination3XA.Make (rator, (Argument) rand0, rand1, rand2) :
                //(rand0 is StaticVariable) ? Combination3XS.Make (rator, (StaticVariable) rand0, rand1, rand2) :
                //(rand0 is Quotation) ? Combination3XQ.Make (rator, (Quotation) rand0, rand1, rand2) :
                new Combination3 (rator, rand0, rand1, rand2);
        }

        internal override PartialResult PartialEval (PartialEnvironment environment)
        {
            PartialResult rator = this.components [0].PartialEval (environment);
            PartialResult rand0 = this.components [1].PartialEval (environment);
            PartialResult rand1 = this.components [2].PartialEval (environment);
            PartialResult rand2 = this.components [3].PartialEval (environment);
            return new PartialResult (rator.Residual == this.components [0] &&
                rand0.Residual == this.components [1] &&
                rand1.Residual == this.components [2] &&
                rand2.Residual == this.components [3] ? this : Combination3.Make (rator.Residual, rand0.Residual, rand1.Residual, rand2.Residual));
        }

        public override void CollectFreeVariables (HashSet<Symbol> freeVariableSet)
        {
            this.components [0].CollectFreeVariables (freeVariableSet);
            this.components [1].CollectFreeVariables (freeVariableSet);
            this.components [2].CollectFreeVariables (freeVariableSet);
            this.components [3].CollectFreeVariables (freeVariableSet);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.components [0]);
            NoteCalls (this.components [1]);
            NoteCalls (this.components [2]);
            NoteCalls (this.components [3]);
            c0TypeHistogram.Note (this.c0Type);
            c1TypeHistogram.Note (this.c1Type);
            c2TypeHistogram.Note (this.c2Type);
            c3TypeHistogram.Note (this.c3Type);
            SCode.location = "Combination3";
#endif
            object ev2;
            Environment env = environment;
            Control unev = this.components [3];
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination3";
#endif
            if (ev2 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination3Frame0 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object ev1;
            env = environment;
            unev = this.components [2];
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination3";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination3Frame1 (this, environment, ev2));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object ev0;
            env = environment;
            unev = this.components [1];
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination3";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination3Frame2 (this, environment, ev1, ev2));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object evop;
            env = environment;
            unev = this.components [0];
            while (unev.EvalStep (out evop, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Combination3";
#endif
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination3Frame3 (this, environment, ev0, ev1, ev2));
                //environment = env;
                //answer = Interpreter.UnwindStack;
                //return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1, ev2);
        }
    }

    [Serializable]
    class Combination3Frame0 : SubproblemContinuation<Combination3>, ISystemVector
    {
        public Combination3Frame0 (Combination3 combination, Environment environment)
            : base (combination, environment)
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

        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object ev2)
        {
            object ev1;
            Environment env = environment;
            Control unevop = this.expression.Components [2];
            while (unevop.EvalStep (out ev1, ref unevop, ref env)) { };
#if DEBUG
            SCode.location = "Combination3";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination3Frame1 (this.expression, environment, ev2));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object ev0;
            env = environment;
            unevop = this.expression.Components [1];
            while (unevop.EvalStep (out ev0, ref unevop, ref env)) { };
#if DEBUG
            SCode.location = "Combination3";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object evop;
            env = environment;
            unevop = this.expression.Operator;
            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
#if DEBUG
            SCode.location = "Combination3";
#endif
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1, ev2);
        }
    }

    [Serializable]
    class Combination3Frame1 : SubproblemContinuation<Combination3>, ISystemVector
    {
        readonly object ev2;
        public Combination3Frame1 (Combination3 combination, Environment environment, object ev2)
            : base (combination, environment)
        {
            this.ev2 = ev2;
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

        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object ev1)
        {
            object ev0;
            Environment env = environment;
            Control unevop = this.expression.Components [1];
            while (unevop.EvalStep (out ev0, ref unevop, ref env)) { };
#if DEBUG
            SCode.location = "Combination3";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object evop;
            env = environment;
            unevop = this.expression.Operator;
            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
#if DEBUG
            SCode.location = "Combination3";
#endif
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1, this.ev2);
        }
    }

    [Serializable]
    class Combination3Frame2 : SubproblemContinuation<Combination3>, ISystemVector
    {
        readonly object ev1;
        readonly object ev2;
        public Combination3Frame2 (Combination3 combination, Environment environment, object ev1, object ev2)
            : base (combination, environment)
        {
            this.ev1 = ev1;
            this.ev2 = ev2;
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
            object evop;
            Environment env = environment;
            Control unevop = this.expression.Operator;
            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
#if DEBUG
            SCode.location = "Combination3";
#endif
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, value, this.ev1, this.ev2);
        }
    }

    [Serializable]
    class Combination3Frame3 : SubproblemContinuation<Combination3>, ISystemVector
    {
        readonly object ev0;
        readonly object ev1;
        readonly object ev2;

        public Combination3Frame3 (Combination3 combination3, Environment environment, object ev0, object ev1, object ev2)
            : base (combination3, environment)
        {
            this.ev0 = ev0;
            this.ev1 = ev1;
            this.ev2 = ev2;
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
            return Interpreter.Call (out answer, ref expression, ref environment, value, this.ev0, this.ev1, this.ev2);
        }
    }

//    [Serializable]
//    class Combination3A : Combination3
//    {
//#if DEBUG
//        static Histogram<Type> c1TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> c2TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> c3TypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly int ratorOffset;
//        protected Combination3A (Argument rator, SCode rand0, SCode rand1, SCode rand2)
//            : base (rator, rand0, rand1, rand2)
//        {
//            this.ratorOffset = rator.Offset;
//        }

//        public static SCode Make (Argument rator, SCode rand0, SCode rand1, SCode rand2)
//        {
//            return
//                (rator is Argument0) ? Combination3A0.Make ((Argument0) rator, rand0, rand1, rand2) :
//                (rator is Argument1) ? Combination3A1.Make ((Argument1) rator, rand0, rand1, rand2) :
//                new Combination3A (rator, rand0, rand1, rand2);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.components [1]);
//            NoteCalls (this.components [2]);
//            NoteCalls (this.components [3]);
//            c1TypeHistogram.Note (this.c1Type);
//            c2TypeHistogram.Note (this.c2Type);
//            c3TypeHistogram.Note (this.c3Type);
//            SCode.location = "Combination3A";
//#endif
//            object ev2;
//            Environment env = environment;
//            Control unev = this.components [3];
//            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3A";
//#endif
//            if (ev2 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame0 (this, environment));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            object ev1;
//            env = environment;
//            unev = this.components [2];
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3A";
//#endif
//            if (ev1 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame1 (this, environment, ev2));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            object ev0;
//            env = environment;
//            unev = this.components [1];
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3A";
//#endif
//            if (ev0 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame2 (this, environment, ev1, ev2));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, environment.ArgumentValue (this.ratorOffset), ev0, ev1, ev2);
//        }
//    }

//    [Serializable]
//    class Combination3A0 : Combination3A
//    {
//#if DEBUG
//        static Histogram<Type> c1TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> c2TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> c3TypeHistogram = new Histogram<Type> ();
//#endif
//        protected Combination3A0 (Argument0 rator, SCode rand0, SCode rand1, SCode rand2)
//            : base (rator, rand0, rand1, rand2)
//        {
//        }

//        public static SCode Make (Argument0 rator, SCode rand0, SCode rand1, SCode rand2)
//        {
//            return
//                new Combination3A0 (rator, rand0, rand1, rand2);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.components [1]);
//            NoteCalls (this.components [2]);
//            NoteCalls (this.components [3]);
//            c1TypeHistogram.Note (this.c1Type);
//            c2TypeHistogram.Note (this.c2Type);
//            c3TypeHistogram.Note (this.c3Type);
//            SCode.location = "Combination3A0";
//#endif
//            object ev2;
//            Environment env = environment;
//            Control unev = this.components [3];
//            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3A0";
//#endif
//            if (ev2 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame0 (this, environment));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            object ev1;
//            env = environment;
//            unev = this.components [2];
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3A0";
//#endif
//            if (ev1 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame1 (this, environment, ev2));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            object ev0;
//            env = environment;
//            unev = this.components [1];
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3A0";
//#endif
//            if (ev0 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame2 (this, environment, ev1, ev2));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, environment.Argument0Value, ev0, ev1, ev2);
//        }
//    }

//    [Serializable]
//    class Combination3A1 : Combination3A
//    {
//#if DEBUG
//        static Histogram<Type> c1TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> c2TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> c3TypeHistogram = new Histogram<Type> ();
//#endif
//        protected Combination3A1 (Argument1 rator, SCode rand0, SCode rand1, SCode rand2)
//            : base (rator, rand0, rand1, rand2)
//        {
//        }

//        public static SCode Make (Argument1 rator, SCode rand0, SCode rand1, SCode rand2)
//        {
//            return
//                new Combination3A1 (rator, rand0, rand1, rand2);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.components [1]);
//            NoteCalls (this.components [2]);
//            NoteCalls (this.components [3]);
//            c1TypeHistogram.Note (this.c1Type);
//            c2TypeHistogram.Note (this.c2Type);
//            c3TypeHistogram.Note (this.c3Type);
//            SCode.location = "Combination3A1";
//#endif
//            object ev2;
//            Environment env = environment;
//            Control unev = this.components [3];
//            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3A1";
//#endif
//            if (ev2 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame0 (this, environment));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            object ev1;
//            env = environment;
//            unev = this.components [2];
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3A1";
//#endif
//            if (ev1 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame1 (this, environment, ev2));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            object ev0;
//            env = environment;
//            unev = this.components [1];
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3A1";
//#endif
//            if (ev0 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame2 (this, environment, ev1, ev2));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, environment.Argument1Value, ev0, ev1, ev2);
//        }
//    }

//    [Serializable]
//    class Combination3F : Combination3
//    {
//#if DEBUG
//        static Histogram<Type> c1TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> c2TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> c3TypeHistogram = new Histogram<Type> ();
//        static Histogram<Symbol> ratorNames = new Histogram<Symbol> ();
//#endif
//        public readonly Symbol ratorName;
//        protected ValueCell ratorCell;

//        protected Combination3F (FreeVariable rator, SCode rand0, SCode rand1, SCode rand2)
//            : base (rator, rand0, rand1, rand2)
//        {
//            this.ratorName = rator.Name;
//        }

//        public static SCode Make (FreeVariable rator, SCode rand0, SCode rand1, SCode rand2)
//        {
//            return
//                (rand0 is FreeVariable) ? Combination3FF.Make (rator, (FreeVariable) rand0, rand1, rand2) :
//                (rand0 is Quotation) ? Combination3FQ.Make (rator, (Quotation) rand0, rand1, rand2) :
//                (rand2 is Quotation) ? Combination3FXXQ.Make (rator, rand0, rand1, (Quotation) rand2) :
//                new Combination3F (rator, rand0, rand1, rand2);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.components [1]);
//            NoteCalls (this.components [2]);
//            NoteCalls (this.components [3]);
//            c1TypeHistogram.Note (this.c1Type);
//            c2TypeHistogram.Note (this.c2Type);
//            c3TypeHistogram.Note (this.c3Type);
//            ratorNames.Note (this.ratorName);
//            SCode.location = "Combination3F";
//#endif
//            object ev2;
//            Environment env = environment;
//            Control unev = this.components [3];
//            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3F";
//#endif
//            if (ev2 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame0 (this, environment));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            object ev1;
//            env = environment;
//            unev = this.components [2];
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3F";
//#endif
//            if (ev1 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame1 (this, environment, ev2));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            object ev0;
//            env = environment;
//            unev = this.components [1];
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3F";
//#endif
//            if (ev0 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame2 (this, environment, ev1, ev2));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            object evop;

//            if (this.ratorCell == null) {
//                Environment baseEnvironment = environment.BaseEnvironment;

//                if (baseEnvironment.FreeReference (out this.ratorCell, this.ratorName))
//                    throw new NotImplementedException ("Error with free variable " + this.ratorName);
//            }
//            if (this.ratorCell.GetValue (out evop))
//                throw new NotImplementedException ("Error with free variable " + this.ratorName);

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1, ev2);
//        }
//    }

//    [Serializable]
//    class Combination3FF : Combination3F
//    {
//#if DEBUG
//        static Histogram<Type> c2TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> c3TypeHistogram = new Histogram<Type> ();
//        static Histogram<Symbol> ratorNames = new Histogram<Symbol> ();
//        static Histogram<Symbol> rand0Names = new Histogram<Symbol> ();
//#endif
//        public readonly Symbol rand0Name;
//        protected ValueCell rand0Cell;

//        protected Combination3FF (FreeVariable rator, FreeVariable rand0, SCode rand1, SCode rand2)
//            : base (rator, rand0, rand1, rand2)
//        {
//            this.rand0Name = rand0.Name;
//        }

//        public static SCode Make (FreeVariable rator, FreeVariable rand0, SCode rand1, SCode rand2)
//        {
//            return
//                (rand1 is Argument) ? Combination3FFA.Make (rator, rand0, (Argument) rand1, rand2) :
//                new Combination3FF (rator, rand0, rand1, rand2);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.components [2]);
//            NoteCalls (this.components [3]);
//            c2TypeHistogram.Note (this.c2Type);
//            c3TypeHistogram.Note (this.c3Type);
//            ratorNames.Note (this.ratorName);
//            rand0Names.Note (this.rand0Name);
//            SCode.location = "Combination3FF";
//#endif
//            object ev2;
//            Environment env = environment;
//            Control unev = this.components [3];
//            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3FF";
//#endif
//            if (ev2 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame0 (this, environment));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            object ev1;
//            env = environment;
//            unev = this.components [2];
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3FF";
//#endif
//            if (ev1 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame1 (this, environment, ev2));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            object ev0;
//            if (this.rand0Cell == null) {
//                Environment baseEnvironment = environment.BaseEnvironment;

//                if (baseEnvironment.FreeReference (out this.rand0Cell, this.rand0Name))
//                    throw new NotImplementedException ("Error with free variable " + this.rand0Name);
//            }
//            if (this.rand0Cell.GetValue (out ev0))
//                throw new NotImplementedException ("Error with free variable " + this.rand0Name);

//            object evop;

//            if (this.ratorCell == null) {
//                Environment baseEnvironment = environment.BaseEnvironment;

//                if (baseEnvironment.FreeReference (out this.ratorCell, this.ratorName))
//                    throw new NotImplementedException ("Error with free variable " + this.ratorName);
//            }
//            if (this.ratorCell.GetValue (out evop))
//                throw new NotImplementedException ("Error with free variable " + this.ratorName);

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1, ev2);
//        }
//    }

//    [Serializable]
//    class Combination3FFA : Combination3FF
//    {
//#if DEBUG
//        static Histogram<Type> c3TypeHistogram = new Histogram<Type> ();
//        static Histogram<Symbol> ratorNames = new Histogram<Symbol> ();
//        static Histogram<Symbol> rand0Names = new Histogram<Symbol> ();
//#endif
//        public readonly int rand1Offset;

//        protected Combination3FFA (FreeVariable rator, FreeVariable rand0, Argument rand1, SCode rand2)
//            : base (rator, rand0, rand1, rand2)
//        {
//            this.rand1Offset = rand1.Offset;
//        }

//        public static SCode Make (FreeVariable rator, FreeVariable rand0, Argument rand1, SCode rand2)
//        {
//            return
//                (rand1 is Argument0) ? Combination3FFA0.Make (rator, rand0, (Argument0) rand1, rand2) :
//                (rand1 is Argument1) ? Combination3FFA1.Make (rator, rand0, (Argument1) rand1, rand2) :
//                new Combination3FFA (rator, rand0, rand1, rand2);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.components [3]);
//            c3TypeHistogram.Note (this.c3Type);
//            ratorNames.Note (this.ratorName);
//            rand0Names.Note (this.rand0Name);
//            SCode.location = "Combination3FFA";
//#endif
//            object ev2;
//            Environment env = environment;
//            Control unev = this.components [3];
//            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3FFA";
//#endif
//            if (ev2 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame0 (this, environment));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            object ev1 = environment.ArgumentValue (this.rand1Offset);

//            object ev0;
//            if (this.rand0Cell == null) {
//                Environment baseEnvironment = environment.BaseEnvironment;

//                if (baseEnvironment.FreeReference (out this.rand0Cell, this.rand0Name))
//                    throw new NotImplementedException ("Error with free variable " + this.rand0Name);
//            }
//            if (this.rand0Cell.GetValue (out ev0))
//                throw new NotImplementedException ("Error with free variable " + this.rand0Name);

//            object evop;

//            if (this.ratorCell == null) {
//                Environment baseEnvironment = environment.BaseEnvironment;

//                if (baseEnvironment.FreeReference (out this.ratorCell, this.ratorName))
//                    throw new NotImplementedException ("Error with free variable " + this.ratorName);
//            }
//            if (this.ratorCell.GetValue (out evop))
//                throw new NotImplementedException ("Error with free variable " + this.ratorName);

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1, ev2);
//        }
//    }

//    [Serializable]
//    class Combination3FFA0 : Combination3FFA
//    {
//#if DEBUG
//        static Histogram<Type> c3TypeHistogram = new Histogram<Type> ();
//        static Histogram<Symbol> ratorNames = new Histogram<Symbol> ();
//        static Histogram<Symbol> rand0Names = new Histogram<Symbol> ();
//#endif

//        protected Combination3FFA0 (FreeVariable rator, FreeVariable rand0, Argument0 rand1, SCode rand2)
//            : base (rator, rand0, rand1, rand2)
//        {
//        }

//        public static SCode Make (FreeVariable rator, FreeVariable rand0, Argument0 rand1, SCode rand2)
//        {
//            return
//                new Combination3FFA0 (rator, rand0, rand1, rand2);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.components [3]);
//            c3TypeHistogram.Note (this.c3Type);
//            ratorNames.Note (this.ratorName);
//            rand0Names.Note (this.rand0Name);
//            SCode.location = "Combination3FFA0";
//#endif
//            object ev2;
//            Environment env = environment;
//            Control unev = this.components [3];
//            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3FFA0";
//#endif
//            if (ev2 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame0 (this, environment));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            object ev1 = environment.Argument0Value;

//            object ev0;
//            if (this.rand0Cell == null) {
//                Environment baseEnvironment = environment.BaseEnvironment;

//                if (baseEnvironment.FreeReference (out this.rand0Cell, this.rand0Name))
//                    throw new NotImplementedException ("Error with free variable " + this.rand0Name);
//            }
//            if (this.rand0Cell.GetValue (out ev0))
//                throw new NotImplementedException ("Error with free variable " + this.rand0Name);

//            object evop;

//            if (this.ratorCell == null) {
//                Environment baseEnvironment = environment.BaseEnvironment;

//                if (baseEnvironment.FreeReference (out this.ratorCell, this.ratorName))
//                    throw new NotImplementedException ("Error with free variable " + this.ratorName);
//            }
//            if (this.ratorCell.GetValue (out evop))
//                throw new NotImplementedException ("Error with free variable " + this.ratorName);

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1, ev2);
//        }
//    }

//    [Serializable]
//    class Combination3FFA1 : Combination3FFA
//    {
//#if DEBUG
//        static Histogram<Type> c3TypeHistogram = new Histogram<Type> ();
//        static Histogram<Symbol> ratorNames = new Histogram<Symbol> ();
//        static Histogram<Symbol> rand0Names = new Histogram<Symbol> ();
//#endif

//        protected Combination3FFA1 (FreeVariable rator, FreeVariable rand0, Argument1 rand1, SCode rand2)
//            : base (rator, rand0, rand1, rand2)
//        {
//        }

//        public static SCode Make (FreeVariable rator, FreeVariable rand0, Argument1 rand1, SCode rand2)
//        {
//            return
//                new Combination3FFA1 (rator, rand0, rand1, rand2);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.components [3]);
//            c3TypeHistogram.Note (this.c3Type);
//            ratorNames.Note (this.ratorName);
//            rand0Names.Note (this.rand0Name);
//            SCode.location = "Combination3FFA1";
//#endif
//            object ev2;
//            Environment env = environment;
//            Control unev = this.components [3];
//            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3FFA1";
//#endif
//            if (ev2 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame0 (this, environment));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            object ev1 = environment.Argument1Value;

//            object ev0;
//            if (this.rand0Cell == null) {
//                Environment baseEnvironment = environment.BaseEnvironment;

//                if (baseEnvironment.FreeReference (out this.rand0Cell, this.rand0Name))
//                    throw new NotImplementedException ("Error with free variable " + this.rand0Name);
//            }
//            if (this.rand0Cell.GetValue (out ev0))
//                throw new NotImplementedException ("Error with free variable " + this.rand0Name);

//            object evop;

//            if (this.ratorCell == null) {
//                Environment baseEnvironment = environment.BaseEnvironment;

//                if (baseEnvironment.FreeReference (out this.ratorCell, this.ratorName))
//                    throw new NotImplementedException ("Error with free variable " + this.ratorName);
//            }
//            if (this.ratorCell.GetValue (out evop))
//                throw new NotImplementedException ("Error with free variable " + this.ratorName);

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1, ev2);
//        }
//    }

//    [Serializable]
//    class Combination3FQ : Combination3F
//    {
//#if DEBUG
//        static Histogram<Type> c2TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> c3TypeHistogram = new Histogram<Type> ();
//        static Histogram<Symbol> ratorNames = new Histogram<Symbol> ();
//#endif
//        public readonly object rand0Value;

//        protected Combination3FQ (FreeVariable rator, Quotation rand0, SCode rand1, SCode rand2)
//            : base (rator, rand0, rand1, rand2)
//        {
//            this.rand0Value = rand0.Quoted;
//        }

//        public static SCode Make (FreeVariable rator, Quotation rand0, SCode rand1, SCode rand2)
//        {
//            return
//                new Combination3FQ (rator, rand0, rand1, rand2);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.components [2]);
//            NoteCalls (this.components [3]);
//            c2TypeHistogram.Note (this.c2Type);
//            c3TypeHistogram.Note (this.c3Type);
//            ratorNames.Note (this.ratorName);
//            SCode.location = "Combination3FQ";
//#endif
//            object ev2;
//            Environment env = environment;
//            Control unev = this.components [3];
//            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3FQ";
//#endif
//            if (ev2 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame0 (this, environment));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            object ev1;
//            env = environment;
//            unev = this.components [2];
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3FQ";
//#endif
//            if (ev1 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame1 (this, environment, ev2));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            object ev0 = this.rand0Value;

//            object evop;

//            if (this.ratorCell == null) {
//                Environment baseEnvironment = environment.BaseEnvironment;

//                if (baseEnvironment.FreeReference (out this.ratorCell, this.ratorName))
//                    throw new NotImplementedException ("Error with free variable " + this.ratorName);
//            }
//            if (this.ratorCell.GetValue (out evop))
//                throw new NotImplementedException ("Error with free variable " + this.ratorName);

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1, ev2);
//        }
//    }

//    [Serializable]
//    class Combination3FXXQ : Combination3F
//    {
//#if DEBUG
//        static Histogram<Type> c1TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> c2TypeHistogram = new Histogram<Type> ();
//        static Histogram<Symbol> ratorNames = new Histogram<Symbol> ();
//#endif
//        public readonly object rand2Value;

//        protected Combination3FXXQ (FreeVariable rator, SCode rand0, SCode rand1, Quotation rand2)
//            : base (rator, rand0, rand1, rand2)
//        {
//            this.rand2Value = rand2.Quoted;
//        }

//        public static SCode Make (FreeVariable rator, SCode rand0, SCode rand1, Quotation rand2)
//        {
//            return
//                new Combination3FXXQ (rator, rand0, rand1, rand2);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.components [1]);
//            NoteCalls (this.components [2]);
//            c1TypeHistogram.Note (this.c1Type);
//            c2TypeHistogram.Note (this.c2Type);
//            ratorNames.Note (this.ratorName);
//            SCode.location = "Combination3FXXQ";
//#endif
//            object ev2 = this.rand2Value;

//            object ev1;
//            Environment env = environment;
//            Control unev = this.components [2];
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3FXXQ";
//#endif
//            if (ev1 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame1 (this, environment, ev2));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            object ev0;
//            env = environment;
//            unev = this.components [1];
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3FXXQ";
//#endif
//            if (ev0 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame2 (this, environment, ev1, ev2));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            object evop;

//            if (this.ratorCell == null) {
//                Environment baseEnvironment = environment.BaseEnvironment;

//                if (baseEnvironment.FreeReference (out this.ratorCell, this.ratorName))
//                    throw new NotImplementedException ("Error with free variable " + this.ratorName);
//            }
//            if (this.ratorCell.GetValue (out evop))
//                throw new NotImplementedException ("Error with free variable " + this.ratorName);

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1, ev2);
//        }
//    }

//    [Serializable]
//    class Combination3Q : Combination3
//    {
//#if DEBUG
//        static Histogram<Type> c1TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> c2TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> c3TypeHistogram = new Histogram<Type> ();
//        static Histogram<Symbol> ratorNames = new Histogram<Symbol> ();
//#endif
//        public readonly object operatorValue;

//        protected Combination3Q (Quotation rator, SCode rand0, SCode rand1, SCode rand2)
//            : base (rator, rand0, rand1, rand2)
//        {
//            this.operatorValue = rator.Quoted;
//        }

//        public static SCode Make (Quotation rator, SCode rand0, SCode rand1, SCode rand2)
//        {
//            return
//                (rand0 is Quotation) ? Combination3QQ.Make (rator, (Quotation) rand0, rand1, rand2) :
//                (rand2 is Quotation) ? new Combination3QXXQ (rator, rand0, rand1, (Quotation) rand2) :
//                new Combination3Q (rator, rand0, rand1, rand2);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.components [1]);
//            NoteCalls (this.components [2]);
//            NoteCalls (this.components [3]);
//            c1TypeHistogram.Note (this.c1Type);
//            c2TypeHistogram.Note (this.c2Type);
//            c3TypeHistogram.Note (this.c3Type);
//            SCode.location = "Combination3Q";
//#endif
//            object ev2;
//            Environment env = environment;
//            Control unev = this.components [3];
//            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3Q";
//#endif
//            if (ev2 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame0 (this, environment));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            object ev1;
//            env = environment;
//            unev = this.components [2];
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3Q";
//#endif
//            if (ev1 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame1 (this, environment, ev2));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            object ev0;
//            env = environment;
//            unev = this.components [1];
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3Q";
//#endif
//            if (ev0 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame2 (this, environment, ev1, ev2));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, this.operatorValue, ev0, ev1, ev2);
//        }
//    }

//    [Serializable]
//    class Combination3QQ : Combination3Q
//    {
//#if DEBUG
//        static Histogram<Type> c2TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> c3TypeHistogram = new Histogram<Type> ();
//        static Histogram<Symbol> ratorNames = new Histogram<Symbol> ();
//#endif
//        public readonly object rand0Value;

//        protected Combination3QQ (Quotation rator, Quotation rand0, SCode rand1, SCode rand2)
//            : base (rator, rand0, rand1, rand2)
//        {
//            this.rand0Value = rand0.Quoted;
//        }

//        public static SCode Make (Quotation rator, Quotation rand0, SCode rand1, SCode rand2)
//        {
//            return
//                new Combination3QQ (rator, rand0, rand1, rand2);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.components [2]);
//            NoteCalls (this.components [3]);
//            c2TypeHistogram.Note (this.c2Type);
//            c3TypeHistogram.Note (this.c3Type);
//            SCode.location = "Combination3QQ";
//#endif
//            object ev2;
//            Environment env = environment;
//            Control unev = this.components [3];
//            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3QQ";
//#endif
//            if (ev2 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame0 (this, environment));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            object ev1;
//            env = environment;
//            unev = this.components [2];
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3QQ";
//#endif
//            if (ev1 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame1 (this, environment, ev2));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, this.operatorValue, this.rand0Value, ev1, ev2);
//        }
//    }

//    [Serializable]
//    sealed class Combination3QXXQ : Combination3Q
//    {
//#if DEBUG
//        static Histogram<Type> c1TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> c2TypeHistogram = new Histogram<Type> ();
//        static Histogram<Symbol> ratorNames = new Histogram<Symbol> ();
//#endif
//        public readonly object rand2Value;

//        internal Combination3QXXQ (Quotation rator, SCode rand0, SCode rand1, Quotation rand2)
//            : base (rator, rand0, rand1, rand2)
//        {
//            this.rand2Value = rand2.Quoted;
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.components [1]);
//            NoteCalls (this.components [2]);
//            c1TypeHistogram.Note (this.c1Type);
//            c2TypeHistogram.Note (this.c2Type);
//            SCode.location = "Combination3QXXQ";
//#endif

//            object ev1;
//            Environment env = environment;
//            Control unev = this.components [2];
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3QXXQ";
//#endif
//            if (ev1 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame1 (this, environment, ev2));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            object ev0;
//            env = environment;
//            unev = this.components [1];
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3QXXQ";
//#endif
//            if (ev0 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame2 (this, environment, ev1, ev2));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, this.operatorValue, ev0, ev1, this.rand2Value);
//        }
//    }

//    [Serializable]
//    class Combination3T : Combination3
//    {
//#if DEBUG
//        static Histogram<Type> c1TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> c2TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> c3TypeHistogram = new Histogram<Type> ();
//        static Histogram<Symbol> ratorNames = new Histogram<Symbol> ();
//#endif
//        protected ValueCell ratorCell;

//        protected Combination3T (TopLevelVariable rator, SCode rand0, SCode rand1, SCode rand2)
//            : base (rator, rand0, rand1, rand2)
//        {
//            this.ratorCell = rator.valueCell;
//        }

//        public static SCode Make (TopLevelVariable rator, SCode rand0, SCode rand1, SCode rand2)
//        {
//            return
//                (rand0 is Argument) ? Combination3TA.Make (rator, (Argument) rand0, rand1, rand2) :
//                (rand0 is Quotation) ? Combination3TQ.Make (rator, (Quotation) rand0, rand1, rand2) :
//                (rand0 is StaticVariable) ? Combination3TS.Make (rator, (StaticVariable) rand0, rand1, rand2) :
//               // (rand1 is Argument) ? Combination3TXA.Make (rator, rand0, (Argument) rand1, rand2) :
//                (rand1 is Quotation) ? Combination3TXQ.Make (rator, rand0, (Quotation) rand1, rand2) :
//               // (rand1 is StaticVariable) ? Combination3TXS.Make (rator, rand0, (StaticVariable) rand1, rand2) :
//                new Combination3T (rator, rand0, rand1, rand2);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.components [1]);
//            NoteCalls (this.components [2]);
//            NoteCalls (this.components [3]);
//            c1TypeHistogram.Note (this.c1Type);
//            c2TypeHistogram.Note (this.c2Type);
//            c3TypeHistogram.Note (this.c3Type);
//            SCode.location = "Combination3T";
//#endif
//            object ev2;
//            Environment env = environment;
//            Control unev = this.components [3];
//            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3T";
//#endif
//            if (ev2 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame0 (this, environment));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            object ev1;
//            env = environment;
//            unev = this.components [2];
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3T";
//#endif
//            if (ev1 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame1 (this, environment, ev2));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            object ev0;
//            env = environment;
//            unev = this.components [1];
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3T";
//#endif
//            if (ev0 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame2 (this, environment, ev1, ev2));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            object evop;

//            if (this.ratorCell.GetValue (out evop))
//                throw new NotImplementedException ("Error with free variable " + this.Operator);

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1, ev2);
//        }
//    }

//    [Serializable]
//    class Combination3TA : Combination3T
//    {
//#if DEBUG
//        static Histogram<Type> c2TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> c3TypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly int rand0Offset;

//        protected Combination3TA (TopLevelVariable rator, Argument rand0, SCode rand1, SCode rand2)
//            : base (rator, rand0, rand1, rand2)
//        {
//            this.rand0Offset = rand0.Offset;
//        }

//        public static SCode Make (TopLevelVariable rator, Argument rand0, SCode rand1, SCode rand2)
//        {
//            return
//                (rand0 is Argument0) ? Combination3TA0.Make (rator, (Argument0) rand0, rand1, rand2) :
//                (rand0 is Argument1) ? Combination3TA1.Make (rator, (Argument1) rand0, rand1, rand2) :
//                new Combination3TA (rator, rand0, rand1, rand2);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.components [2]);
//            NoteCalls (this.components [3]);
//            c2TypeHistogram.Note (this.c2Type);
//            c3TypeHistogram.Note (this.c3Type);
//            SCode.location = "Combination3TA";
//#endif
//            object ev2;
//            Environment env = environment;
//            Control unev = this.components [3];
//            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3TA";
//#endif
//            if (ev2 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame0 (this, environment));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            object ev1;
//            env = environment;
//            unev = this.components [2];
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3TA";
//#endif
//            if (ev1 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame1 (this, environment, ev2));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            object ev0 = environment.ArgumentValue(this.rand0Offset);

//            object evop;

//            if (this.ratorCell.GetValue (out evop))
//                throw new NotImplementedException ("Error with free variable " + this.Operator);

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1, ev2);
//        }
//    }

//    [Serializable]
//    class Combination3TA0 : Combination3T
//    {
//#if DEBUG
//        static Histogram<Type> c2TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> c3TypeHistogram = new Histogram<Type> ();
//#endif

//        protected Combination3TA0 (TopLevelVariable rator, Argument0 rand0, SCode rand1, SCode rand2)
//            : base (rator, rand0, rand1, rand2)
//        {
//        }

//        public static SCode Make (TopLevelVariable rator, Argument0 rand0, SCode rand1, SCode rand2)
//        {
//            return
//                (rand1 is Argument) ? Combination3TA0A.Make (rator, rand0, (Argument) rand1, rand2) :
//                new Combination3TA0 (rator, rand0, rand1, rand2);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.components [2]);
//            NoteCalls (this.components [3]);
//            c2TypeHistogram.Note (this.c2Type);
//            c3TypeHistogram.Note (this.c3Type);
//            SCode.location = "Combination3TA0";
//#endif
//            object ev2;
//            Environment env = environment;
//            Control unev = this.components [3];
//            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3TA0";
//#endif
//            if (ev2 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame0 (this, environment));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            object ev1;
//            env = environment;
//            unev = this.components [2];
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3TA0";
//#endif
//            if (ev1 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame1 (this, environment, ev2));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            object ev0 = environment.Argument0Value;

//            object evop;

//            if (this.ratorCell.GetValue (out evop))
//                throw new NotImplementedException ("Error with free variable " + this.Operator);

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1, ev2);
//        }
//    }

//    [Serializable]
//    class Combination3TA0A : Combination3TA0
//    {
//#if DEBUG
//        static Histogram<Type> c3TypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly int rand1Offset;

//        protected Combination3TA0A (TopLevelVariable rator, Argument0 rand0, Argument rand1, SCode rand2)
//            : base (rator, rand0, rand1, rand2)
//        {
//            this.rand1Offset = rand1.Offset;
//        }

//        public static SCode Make (TopLevelVariable rator, Argument0 rand0, Argument rand1, SCode rand2)
//        {
//            return
//                (rand1 is Argument0) ? Combination3TA0A0.Make (rator, rand0, (Argument0) rand1, rand2) :
//                (rand1 is Argument1) ? Combination3TA0A1.Make (rator, rand0, (Argument1) rand1, rand2) :
//                new Combination3TA0A (rator, rand0, rand1, rand2);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.components [3]);
//            c3TypeHistogram.Note (this.c3Type);
//            SCode.location = "Combination3TA0A";
//#endif
//            object ev2;
//            Environment env = environment;
//            Control unev = this.components [3];
//            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3TA0A";
//#endif
//            if (ev2 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame0 (this, environment));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            object ev1 = environment.ArgumentValue (this.rand1Offset);

//            object ev0 = environment.Argument0Value;

//            object evop;

//            if (this.ratorCell.GetValue (out evop))
//                throw new NotImplementedException ("Error with free variable " + this.Operator);

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1, ev2);
//        }
//    }

//    [Serializable]
//    class Combination3TA0A0 : Combination3TA0A
//    {
//#if DEBUG
//        static Histogram<Type> c3TypeHistogram = new Histogram<Type> ();
//#endif

//        protected Combination3TA0A0 (TopLevelVariable rator, Argument0 rand0, Argument0 rand1, SCode rand2)
//            : base (rator, rand0, rand1, rand2)
//        {
//        }

//        public static SCode Make (TopLevelVariable rator, Argument0 rand0, Argument0 rand1, SCode rand2)
//        {
//            return
//                new Combination3TA0A0 (rator, rand0, rand1, rand2);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.components [3]);
//            c3TypeHistogram.Note (this.c3Type);
//            SCode.location = "Combination3TA0A0";
//#endif
//            object ev2;
//            Environment env = environment;
//            Control unev = this.components [3];
//            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3TA0A0";
//#endif
//            if (ev2 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame0 (this, environment));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            object ev1 = environment.Argument0Value;

//            object ev0 = environment.Argument0Value;

//            object evop;

//            if (this.ratorCell.GetValue (out evop))
//                throw new NotImplementedException ("Error with free variable " + this.Operator);

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1, ev2);
//        }
//    }

//    [Serializable]
//    class Combination3TA0A1 : Combination3TA0A
//    {
//#if DEBUG
//        static Histogram<Type> c3TypeHistogram = new Histogram<Type> ();
//#endif

//        protected Combination3TA0A1 (TopLevelVariable rator, Argument0 rand0, Argument1 rand1, SCode rand2)
//            : base (rator, rand0, rand1, rand2)
//        {
//        }

//        public static SCode Make (TopLevelVariable rator, Argument0 rand0, Argument1 rand1, SCode rand2)
//        {
//            return
//                (rand2 is Quotation) ? Combination3TA0A1Q.Make (rator, rand0, rand1, (Quotation) rand2) :
//                new Combination3TA0A1 (rator, rand0, rand1, rand2);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.components [3]);
//            c3TypeHistogram.Note (this.c3Type);
//            SCode.location = "Combination3TA0A1";
//#endif
//            object ev2;
//            Environment env = environment;
//            Control unev = this.components [3];
//            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3TA0A1";
//#endif
//            if (ev2 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame0 (this, environment));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            object ev1 = environment.Argument1Value;

//            object ev0 = environment.Argument0Value;

//            object evop;

//            if (this.ratorCell.GetValue (out evop))
//                throw new NotImplementedException ("Error with free variable " + this.Operator);

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1, ev2);
//        }
//    }

//    [Serializable]
//    sealed class Combination3TA0A1Q : Combination3TA0A1
//    {
//        public readonly object rand2Value;

//        protected Combination3TA0A1Q (TopLevelVariable rator, Argument0 rand0, Argument1 rand1, Quotation rand2)
//            : base (rator, rand0, rand1, rand2)
//        {
//            this.rand2Value = rand2.Quoted;
//        }

//        public static SCode Make (TopLevelVariable rator, Argument0 rand0, Argument1 rand1, Quotation rand2)
//        {
//            return
//                new Combination3TA0A1Q (rator, rand0, rand1, rand2);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination3TA0A1Q");
//#endif
//            object ev2 = this.rand2Value;

//            object ev1 = environment.Argument1Value;

//            object ev0 = environment.Argument0Value;

//            object evop;

//            if (this.ratorCell.GetValue (out evop))
//                throw new NotImplementedException ("Error with free variable " + this.Operator);

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1, ev2);
//        }
//    }

//    [Serializable]
//    class Combination3TA1 : Combination3TA
//    {
//#if DEBUG
//        static Histogram<Type> c2TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> c3TypeHistogram = new Histogram<Type> ();
//#endif

//        protected Combination3TA1 (TopLevelVariable rator, Argument1 rand0, SCode rand1, SCode rand2)
//            : base (rator, rand0, rand1, rand2)
//        {
//        }

//        public static SCode Make (TopLevelVariable rator, Argument1 rand0, SCode rand1, SCode rand2)
//        {
//            return
//                new Combination3TA1 (rator, rand0, rand1, rand2);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.components [2]);
//            NoteCalls (this.components [3]);
//            c2TypeHistogram.Note (this.c2Type);
//            c3TypeHistogram.Note (this.c3Type);
//            SCode.location = "Combination3TA1";
//#endif
//            object ev2;
//            Environment env = environment;
//            Control unev = this.components [3];
//            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3TA1";
//#endif
//            if (ev2 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame0 (this, environment));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            object ev1;
//            env = environment;
//            unev = this.components [2];
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3TA1";
//#endif
//            if (ev1 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame1 (this, environment, ev2));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            object ev0 = environment.Argument1Value;

//            object evop;

//            if (this.ratorCell.GetValue (out evop))
//                throw new NotImplementedException ("Error with free variable " + this.Operator);

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1, ev2);
//        }
//    }

//    [Serializable]
//    class Combination3TQ : Combination3T
//    {
//#if DEBUG
//        static Histogram<Type> c2TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> c3TypeHistogram = new Histogram<Type> ();
//#endif
//        protected readonly object rand0Value;

//        protected Combination3TQ (TopLevelVariable rator, Quotation rand0, SCode rand1, SCode rand2)
//            : base (rator, rand0, rand1, rand2)
//        {
//            this.rand0Value = rand0.Quoted;
//        }

//        public static SCode Make (TopLevelVariable rator, Quotation rand0, SCode rand1, SCode rand2)
//        {
//            return
//             //   (rand1 is Argument) ? Combination3TQA.Make (rator, rand0, (Argument) rand1, rand2) :
//                (rand1 is Quotation) ? Combination3TQQ.Make (rator, rand0, (Quotation) rand1, rand2) :
//             //   (rand1 is StaticVariable) ? Combination3TQS.Make (rator, rand0, (StaticVariable) rand1, rand2) :
//                new Combination3TQ (rator, rand0, rand1, rand2);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.components [2]);
//            NoteCalls (this.components [3]);
//            c2TypeHistogram.Note (this.c2Type);
//            c3TypeHistogram.Note (this.c3Type);
//            SCode.location = "Combination3TQ";
//#endif
//            object ev2;
//            Environment env = environment;
//            Control unev = this.components [3];
//            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3TQ";
//#endif
//            if (ev2 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame0 (this, environment));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            object ev1;
//            env = environment;
//            unev = this.components [2];
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3TQ";
//#endif
//            if (ev1 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame1 (this, environment, ev2));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            object evop;

//            if (this.ratorCell.GetValue (out evop))
//                throw new NotImplementedException ("Error with free variable " + this.Operator);

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, this.rand0Value, ev1, ev2);
//        }
//    }

//    [Serializable]
//    class Combination3TQQ : Combination3TQ
//    {
//#if DEBUG
//        static Histogram<Type> c3TypeHistogram = new Histogram<Type> ();
//#endif
//        protected readonly object rand1Value;

//        protected Combination3TQQ (TopLevelVariable rator, Quotation rand0, Quotation rand1, SCode rand2)
//            : base (rator, rand0, rand1, rand2)
//        {
//            this.rand1Value = rand1.Quoted;
//        }

//        public static SCode Make (TopLevelVariable rator, Quotation rand0, Quotation rand1, SCode rand2)
//        {
//            return
//                (rand2 is Quotation) ? new Combination3TQQQ (rator, rand0, rand1, (Quotation) rand2) :
//                new Combination3TQQ (rator, rand0, rand1, rand2);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.components [3]);
//            c3TypeHistogram.Note (this.c3Type);
//            SCode.location = "Combination3TQQ";
//#endif
//            object ev2;
//            Environment env = environment;
//            Control unev = this.components [3];
//            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3TQQ";
//#endif
//            if (ev2 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame0 (this, environment));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            object evop;

//            if (this.ratorCell.GetValue (out evop))
//                throw new NotImplementedException ("Error with free variable " + this.Operator);

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, this.rand0Value, this.rand1Value, ev2);
//        }
//    }

//    [Serializable]
//    sealed class Combination3TQQQ : Combination3TQQ
//    {
//        protected readonly object rand2Value;

//        internal Combination3TQQQ (TopLevelVariable rator, Quotation rand0, Quotation rand1, Quotation rand2)
//            : base (rator, rand0, rand1, rand2)
//        {
//            this.rand2Value = rand2.Quoted;
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("Combination3TQQQ");
//#endif

//            object evop;

//            if (this.ratorCell.GetValue (out evop))
//                throw new NotImplementedException ("Error with free variable " + this.Operator);

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, this.rand0Value, this.rand1Value, this.rand2Value);
//        }
//    }

//    [Serializable]
//    class Combination3TS : Combination3T
//    {
//#if DEBUG
//        static Histogram<Type> c2TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> c3TypeHistogram = new Histogram<Type> ();
//#endif
//        protected readonly Symbol rand0Name;
//        protected readonly int rand0Offset;

//        protected Combination3TS (TopLevelVariable rator, StaticVariable rand0, SCode rand1, SCode rand2)
//            : base (rator, rand0, rand1, rand2)
//        {
//            this.rand0Name = rand0.Name;
//            this.rand0Offset = rand0.Offset;
//        }

//        public static SCode Make (TopLevelVariable rator, StaticVariable rand0, SCode rand1, SCode rand2)
//        {
//            return
//                new Combination3TS (rator, rand0, rand1, rand2);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.components [2]);
//            NoteCalls (this.components [3]);
//            c2TypeHistogram.Note (this.c2Type);
//            c3TypeHistogram.Note (this.c3Type);
//            SCode.location = "Combination3TS";
//#endif
//            object ev2;
//            Environment env = environment;
//            Control unev = this.components [3];
//            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3TS";
//#endif
//            if (ev2 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame0 (this, environment));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            object ev1;
//            env = environment;
//            unev = this.components [2];
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3TS";
//#endif
//            if (ev1 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame1 (this, environment, ev2));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            object ev0;
//            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
//                throw new NotImplementedException ();

//            object evop;

//            if (this.ratorCell.GetValue (out evop))
//                throw new NotImplementedException ("Error with free variable " + this.Operator);

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1, ev2);
//        }
//    }

//    [Serializable]
//    class Combination3TXQ : Combination3T
//    {
//#if DEBUG
//        static Histogram<Type> c1TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> c3TypeHistogram = new Histogram<Type> ();
//        static Histogram<Symbol> ratorNames = new Histogram<Symbol> ();
//#endif
//        public readonly object rand1Value;

//        protected Combination3TXQ (TopLevelVariable rator, SCode rand0, Quotation rand1, SCode rand2)
//            : base (rator, rand0, rand1, rand2)
//        {
//            this.rand1Value = rand1.Quoted;
//        }

//        public static SCode Make (TopLevelVariable rator, SCode rand0, Quotation rand1, SCode rand2)
//        {
//            return
//                new Combination3TXQ (rator, rand0, rand1, rand2);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.components [1]);
//            NoteCalls (this.components [3]);
//            c1TypeHistogram.Note (this.c1Type);
//            c3TypeHistogram.Note (this.c3Type);
//            SCode.location = "Combination3TXQ";
//#endif
//            object ev2;
//            Environment env = environment;
//            Control unev = this.components [3];
//            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3TXQ";
//#endif
//            if (ev2 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame0 (this, environment));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            object ev1 = this.rand1Value;

//            object ev0;
//            env = environment;
//            unev = this.components [1];
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3TXQ";
//#endif
//            if (ev0 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame2 (this, environment, ev1, ev2));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            object evop;

//            if (this.ratorCell.GetValue (out evop))
//                throw new NotImplementedException ("Error with free variable " + this.Operator);

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1, ev2);
//        }
//    }

//    [Serializable]
//    class Combination3XA : Combination3
//    {
//#if DEBUG
//        static Histogram<Type> c0TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> c2TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> c3TypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly int rand0Offset;
//        protected Combination3XA (SCode rator, Argument rand0, SCode rand1, SCode rand2)
//            : base (rator, rand0, rand1, rand2)
//        {
//            this.rand0Offset = rand0.Offset;
//        }

//        public static SCode Make (SCode rator, Argument rand0, SCode rand1, SCode rand2)
//        {
//            return
//                (rand0 is Argument0) ? Combination3XA0.Make (rator, (Argument0) rand0, rand1, rand2) :
//                (rand0 is Argument1) ? Combination3XA1.Make (rator, (Argument1) rand0, rand1, rand2) :
//                new Combination3XA (rator, rand0, rand1, rand2);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.components [0]);
//            NoteCalls (this.components [2]);
//            NoteCalls (this.components [3]);
//            c0TypeHistogram.Note (this.c0Type);
//            c2TypeHistogram.Note (this.c2Type);
//            c3TypeHistogram.Note (this.c3Type);
//            SCode.location = "Combination3XA";
//#endif
//            object ev2;
//            Environment env = environment;
//            Control unev = this.components [3];
//            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3XA";
//#endif
//            if (ev2 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame0 (this, environment));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            object ev1;
//            env = environment;
//            unev = this.components [2];
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3XA";
//#endif
//            if (ev1 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame1 (this, environment, ev2));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            object ev0 = environment.ArgumentValue (this.rand0Offset);

//            object evop;
//            env = environment;
//            unev = this.components [0];
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3XA";
//#endif
//            if (evop == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination3Frame3 (this, environment, ev0, ev1, ev2));
//                //environment = env;
//                //answer = Interpreter.UnwindStack;
//                //return false;
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1, ev2);
//        }
//    }

//    [Serializable]
//    class Combination3XA0 : Combination3XA
//    {
//#if DEBUG
//        static Histogram<Type> c0TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> c2TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> c3TypeHistogram = new Histogram<Type> ();
//#endif
//        protected Combination3XA0 (SCode rator, Argument0 rand0, SCode rand1, SCode rand2)
//            : base (rator, rand0, rand1, rand2)
//        {
//        }

//        public static SCode Make (SCode rator, Argument0 rand0, SCode rand1, SCode rand2)
//        {
//            return
//                new Combination3XA0 (rator, rand0, rand1, rand2);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.components [0]);
//            NoteCalls (this.components [2]);
//            NoteCalls (this.components [3]);
//            c0TypeHistogram.Note (this.c0Type);
//            c2TypeHistogram.Note (this.c2Type);
//            c3TypeHistogram.Note (this.c3Type);
//            SCode.location = "Combination3XA0";
//#endif
//            object ev2;
//            Environment env = environment;
//            Control unev = this.components [3];
//            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3XA0";
//#endif
//            if (ev2 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame0 (this, environment));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            object ev1;
//            env = environment;
//            unev = this.components [2];
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3XA0";
//#endif
//            if (ev1 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame1 (this, environment, ev2));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            object ev0 = environment.Argument0Value;

//            object evop;
//            env = environment;
//            unev = this.components [0];
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3XA0";
//#endif
//            if (evop == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination3Frame3 (this, environment, ev0, ev1, ev2));
//                //environment = env;
//                //answer = Interpreter.UnwindStack;
//                //return false;
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1, ev2);
//        }
//    }

//    [Serializable]
//    class Combination3XA1 : Combination3XA
//    {
//#if DEBUG
//        static Histogram<Type> c0TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> c2TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> c3TypeHistogram = new Histogram<Type> ();
//#endif
//        protected Combination3XA1 (SCode rator, Argument1 rand0, SCode rand1, SCode rand2)
//            : base (rator, rand0, rand1, rand2)
//        {
//        }

//        public static SCode Make (SCode rator, Argument1 rand0, SCode rand1, SCode rand2)
//        {
//            return
//                new Combination3XA1 (rator, rand0, rand1, rand2);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.components [0]);
//            NoteCalls (this.components [2]);
//            NoteCalls (this.components [3]);
//            c0TypeHistogram.Note (this.c0Type);
//            c2TypeHistogram.Note (this.c2Type);
//            c3TypeHistogram.Note (this.c3Type);
//            SCode.location = "Combination3XA1";
//#endif
//            object ev2;
//            Environment env = environment;
//            Control unev = this.components [3];
//            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3XA1";
//#endif
//            if (ev2 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame0 (this, environment));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            object ev1;
//            env = environment;
//            unev = this.components [2];
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3XA1";
//#endif
//            if (ev1 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame1 (this, environment, ev2));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            object ev0 = environment.Argument1Value;

//            object evop;
//            env = environment;
//            unev = this.components [0];
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3XA1";
//#endif
//            if (evop == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination3Frame3 (this, environment, ev0, ev1, ev2));
//                //environment = env;
//                //answer = Interpreter.UnwindStack;
//                //return false;
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1, ev2);
//        }
//    }

//    [Serializable]
//    class Combination3XS : Combination3
//    {
//#if DEBUG
//        static Histogram<Type> c0TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> c2TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> c3TypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly Symbol rand0Name;
//        public readonly int rand0Offset;

//        protected Combination3XS (SCode rator, StaticVariable rand0, SCode rand1, SCode rand2)
//            : base (rator, rand0, rand1, rand2)
//        {
//            this.rand0Name = rand0.Name;
//            this.rand0Offset = rand0.Offset;
//        }

//        public static SCode Make (SCode rator, StaticVariable rand0, SCode rand1, SCode rand2)
//        {
//            return
//                new Combination3XS (rator, rand0, rand1, rand2);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.components [0]);
//            NoteCalls (this.components [2]);
//            NoteCalls (this.components [3]);
//            c0TypeHistogram.Note (this.c0Type);
//            c2TypeHistogram.Note (this.c2Type);
//            c3TypeHistogram.Note (this.c3Type);
//            SCode.location = "Combination3XS";
//#endif
//            object ev2;
//            Environment env = environment;
//            Control unev = this.components [3];
//            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3XS";
//#endif
//            if (ev2 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame0 (this, environment));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            object ev1;
//            env = environment;
//            unev = this.components [2];
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3XS";
//#endif
//            if (ev1 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame1 (this, environment, ev2));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            object ev0;
//            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
//                throw new NotImplementedException();

//            object evop;
//            env = environment;
//            unev = this.components [0];
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3XS";
//#endif
//            if (evop == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination3Frame3 (this, environment, ev0, ev1, ev2));
//                //environment = env;
//                //answer = Interpreter.UnwindStack;
//                //return false;
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1, ev2);
//        }
//    }

//    [Serializable]
//    class Combination3XQ : Combination3
//    {
//#if DEBUG
//        static Histogram<Type> c0TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> c2TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> c3TypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object rand0Value;

//        protected Combination3XQ (SCode rator, Quotation rand0, SCode rand1, SCode rand2)
//            : base (rator, rand0, rand1, rand2)
//        {
//            this.rand0Value = rand0.Quoted;
//        }

//        public static SCode Make (SCode rator, Quotation rand0, SCode rand1, SCode rand2)
//        {
//            return
//                new Combination3XQ (rator, rand0, rand1, rand2);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            NoteCalls (this.components [0]);
//            NoteCalls (this.components [2]);
//            NoteCalls (this.components [3]);
//            c0TypeHistogram.Note (this.c0Type);
//            c2TypeHistogram.Note (this.c2Type);
//            c3TypeHistogram.Note (this.c3Type);
//            SCode.location = "Combination3XQ";
//#endif
//            object ev2;
//            Environment env = environment;
//            Control unev = this.components [3];
//            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3XQ";
//#endif
//            if (ev2 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame0 (this, environment));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            object ev1;
//            env = environment;
//            unev = this.components [2];
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3XQ";
//#endif
//            if (ev1 == Interpreter.UnwindStack) {
//                ((UnwinderState) env).AddFrame (new Combination3Frame1 (this, environment, ev2));
//                environment = env;
//                answer = Interpreter.UnwindStack;
//                return false;
//            }

//            object ev0 = this.rand0Value;

//            object evop;
//            env = environment;
//            unev = this.components [0];
//            while (unev.EvalStep (out evop, ref unev, ref env)) { };
//#if DEBUG
//            SCode.location = "Combination3XQ";
//#endif
//            if (evop == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination3Frame3 (this, environment, ev0, ev1, ev2));
//                //environment = env;
//                //answer = Interpreter.UnwindStack;
//                //return false;
//            }

//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1, ev2);
//        }
//    }

}

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;

namespace Microcode
{
    [Serializable]
    class Let2 : Combination2
    {
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();

        protected Type bodyType;
#endif
        public SCode body;
        protected Let2 (Lambda rator, SCode rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.body = rator.Body;
#if DEBUG
            if (rator.Body != null)
                this.bodyType = rator.Body.GetType ();
#endif
        }

        static SCode StandardMake (Lambda rator, SCode arg0, SCode arg1)
        {
            return
                (rator is SimpleLambda) ? SimpleLet2.Make ((SimpleLambda) rator, arg0, arg1) :
                (rator is StaticLambda) ? StaticLet2.Make ((StaticLambda) rator, arg0, arg1) :
                new Let2 (rator, arg0, arg1);
        }

        public static SCode Make (Lambda rator, SCode arg0, SCode arg1)
        {
            return StandardMake (rator, arg0, arg1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            NoteCalls (this.rand0);
            NoteCalls (this.rand1);
            ratorTypeHistogram.Note (this.ratorType);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "Let2";
#endif
            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Let2";
#endif
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
#if DEBUG
            SCode.location = "Let2";
#endif
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
#if DEBUG
            SCode.location = "Let2";
#endif
            if (evop == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, ev0, ev1));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }

    }

    [Serializable]
    class StaticLet2 : Let2
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly StaticLambda lambda;
        public StaticMapping staticMapping;
        protected StaticLet2 (StaticLambda rator, SCode rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.lambda = rator;
        }

        static SCode StandardMake (StaticLambda rator, SCode arg0, SCode arg1)
        {
            return
                (arg0 is Quotation) ? StaticLet2Q.Make (rator, (Quotation) arg0, arg1) :
                new StaticLet2 (rator, arg0, arg1);
        }

        public static SCode Make (StaticLambda rator, SCode arg0, SCode arg1)
        {
            return StandardMake (rator, arg0, arg1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            NoteCalls (this.rand0);
            NoteCalls (this.rand1);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "StaticLet2";
#endif
            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "StaticLet2";
#endif
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
#if DEBUG
            SCode.location = "StaticLet2";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object [] cells = environment.GetValueCells (this.lambda.StaticMapping);
#if DEBUG
            SCode.location = "StaticLet2";
#endif
            // StaticClosure cl = new StaticClosure ((StaticLambda) this.rator, environment);
            StaticClosure cl = new StaticClosure (this.lambda, environment.BaseEnvironment, cells);

            expression = this.body;
            environment = new StaticEnvironment (cl, new object [] { ev0, ev1 });
            answer = null;
            return true;
        }
    }

    [Serializable]
    class StaticLet2Q : StaticLet2
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand0Value;
        protected StaticLet2Q (StaticLambda rator, Quotation rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Value = rand0.Quoted;
        }

        static SCode StandardMake (StaticLambda rator, Quotation arg0, SCode arg1)
        {
            return
                (arg1 is Quotation) ? StaticLet2QQ.Make (rator, arg0, (Quotation) arg1) :
                new StaticLet2Q (rator, arg0, arg1);
        }

        public static SCode Make (StaticLambda rator, Quotation arg0, SCode arg1)
        {
            return StandardMake (rator, arg0, arg1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            NoteCalls (this.rand1);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "StaticLet2Q";
#endif
            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "StaticLet2Q";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object ev0 = this.rand0Value;

            object [] cells = environment.GetValueCells (this.lambda.StaticMapping);
#if DEBUG
            SCode.location = "StaticLet2Q";
#endif
            // StaticClosure cl = new StaticClosure ((StaticLambda) this.rator, environment);
            StaticClosure cl = new StaticClosure (this.lambda, environment.BaseEnvironment, cells);

            expression = this.body;
            environment = new StaticEnvironment (cl, new object [] { ev0, ev1 });
            answer = null;
            return true;
        }
    }

    [Serializable]
    class StaticLet2QQ : StaticLet2Q
    {
#if DEBUG
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Value;
        protected StaticLet2QQ (StaticLambda rator, Quotation rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value = rand1.Quoted;
        }

        static SCode StandardMake (StaticLambda rator, Quotation arg0, Quotation arg1)
        {
            return
               //(arg0.Quoted == ReferenceTrap.Unassigned &&
               // arg1.Quoted == ReferenceTrap.Unassigned &&
               // rator.Body is Letrec2Body) ? Letrec2.Make (rator, arg0) :
                new StaticLet2QQ (rator, arg0, arg1);
        }

        public static SCode Make (StaticLambda rator, Quotation arg0, Quotation arg1)
        {
            return StandardMake (rator, arg0, arg1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            SCode.location = "StaticLet2QQ";
#endif
            object ev1 = this.rand1Value;
            object ev0 = this.rand0Value;

            object [] cells = environment.GetValueCells (this.lambda.StaticMapping);
#if DEBUG
            SCode.location = "StaticLet2QQ";
#endif
            // StaticClosure cl = new StaticClosure ((StaticLambda) this.rator, environment);
            StaticClosure cl = new StaticClosure (this.lambda, environment.BaseEnvironment, cells);

            expression = this.body;
            environment = new StaticEnvironment (cl, new object [] { ev0, ev1 });
            answer = null;
            return true;
        }
    }

//    [Serializable]
//    class Letrec2 : StaticLet2QQ
//    {
//        public readonly SimpleLambda innerLambda;
//        protected Letrec2 (StaticLambda rator, Quotation rand0, Quotation rand1)
//            : base (rator, rand0, rand1)
//        {
//            this.innerLambda = ((Letrec2Body) rator.Body).lambda;
//        }

//        public static SCode Make (StaticLambda rator, Quotation arg0, Quotation arg1)
//        {
//            return
//                new Letrec2 (rator, arg0, arg1);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            SCode.location = "Letrec2";
//#endif
//            object [] outerStaticCells = environment.StaticCells;
//            object [] innerStaticCells = new object [outerStaticCells.Length + 1];
//            for (int i = 0; i < outerStaticCells.Length; i++) {
//                innerStaticCells [i + 1] = outerStaticCells [i];
//            }
//            SimpleClosure rec = new SimpleClosure (this.innerLambda, environment.BaseEnvironment, innerStaticCells);
//            innerStaticCells [0] = rec;
//            answer = rec;
//            return false;


//            //            object [] valueCells = environment.GetValueCells (this.lambda.StaticMapping);
//            //            object [] foo = environment.StaticCells;
//            //            if (foo != valueCells) throw new NotImplementedException ();
//            //#if DEBUG
//            //            SCode.location = "Letrec1";
//            //#endif
//            //            StaticClosure cl = new StaticClosure (this.lambda, environment.BaseEnvironment, valueCells);
//            //            Environment closed = new StaticEnvironment (cl, new object [] { this.rand0Value });

//            //            this.innerLambda.closeCount += 1;
//            //            object [] staticCells = closed.GetValueCells (this.innerLambda.StaticMapping);
//            //#if DEBUG
//            //            SCode.location = "Letrec1";
//            //#endif
//            //            // Use the base environment for lookup.
//            //            SimpleClosure newValue = new SimpleClosure (this.innerLambda, closed.BaseEnvironment, staticCells);

//            //            if (closed.AssignArgument0 (out answer, newValue)) throw new NotImplementedException ();

//            //            answer = newValue;
//            //            return false;
//        }
//    }


    [Serializable]
    class SimpleLet2 : Let2
    {
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();

        protected Type bodyType;
#endif
        protected SimpleLet2 (Lambda rator, SCode rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
#if DEBUG
            if (rator.Body != null)
                this.bodyType = rator.Body.GetType ();
#endif
        }

        static SCode StandardMake (SimpleLambda rator, SCode arg0, SCode arg1)
        {
            return
                new SimpleLet2 (rator, arg0, arg1);
        }

        public static SCode Make (SimpleLambda rator, SCode arg0, SCode arg1)
        {
            return StandardMake (rator, arg0, arg1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            NoteCalls (this.rand0);
            NoteCalls (this.rand1);
            ratorTypeHistogram.Note (this.ratorType);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "SimpleLet2";
#endif
            object ev1;
            Environment env = environment;
            Control unev = this.rand1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "SimpleLet2";
#endif
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
#if DEBUG
            SCode.location = "SimpleLet2";
#endif
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
#if DEBUG
            SCode.location = "SimpleLet2";
#endif
            if (evop == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, ev0, ev1));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }

    }

}

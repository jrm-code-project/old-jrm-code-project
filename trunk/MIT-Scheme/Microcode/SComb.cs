using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Runtime.Serialization;

namespace Microcode
{
    [Serializable]
    class Combination : SCode, ISystemVector
    {
#if DEBUG
        static Histogram<int> combinationSizeHistogram = new Histogram<int>();
#endif

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly SCode [] components;

        protected Combination (SCode [] components)
            : base (TC.COMBINATION)
        {
            object oper = components [0];
            this.components = components;
        }

        Combination (object [] components)
            : base (TC.COMBINATION)
        {
            SCode[] comps = new SCode [components.Length];
            for (int i = 0; i < components.Length; i++)
                comps [i] = EnsureSCode (components [i]);
            this.components = comps;
        }

        Combination(Cons components)
            : base (TC.COMBINATION)
        {
            SCode[] comps = new SCode[components.Length()];
            int i = 0;
            while (components != null)
            {
                SCode component = EnsureSCode (components.Car);
                comps[i] = component;
                components = (Cons)(components.Cdr);
                i += 1;
            }
            this.components = comps;
        }

        public static SCode Make (object [] components)
        {
            object oper = components [0];

            if (Configuration.EnableSuperOperators) {
                switch (components.Length) {
                    case 0:
                        throw new NotImplementedException ("shouldn't be possible");

                    case 1:
                        //Debugger.Break ();
                        return Combination0.Make (oper);

                    case 2:
                        throw new NotImplementedException ("combo 1");

                    case 3:
                        throw new NotImplementedException ("combo 2");

                    case 4:
                        return Combination3.Make (EnsureSCode (oper), EnsureSCode (components [1]), EnsureSCode (components [2]), EnsureSCode (components [3]));

                    case 5:
                        return Combination4.Make (EnsureSCode (oper), EnsureSCode (components [1]), EnsureSCode (components [2]), EnsureSCode (components [3]), EnsureSCode (components [4]));

                    default:
                        return new Combination (components);
                }
            }
            else
                return new Combination (components);
        }

        public SCode [] Components
        {
            [DebuggerStepThrough]
            get
            {
                return this.components;
            }
        }

        public SCode Operator
        {
            [DebuggerStepThrough]
            get
            {
                return this.components[0];
            }
        }

        [SchemePrimitive ("COMBINATION?", 1, true)]
        public static bool IsCombination (out object answer, object arg)
        {
            answer = arg is Combination || arg is Combination0 || arg is Combination3 || arg is Combination4;
            return false;
        }

         public override SCode Bind (LexicalMap ctenv)
        {
            bool anyChange = false;
            SCode [] opt = new SCode [this.components.Length];
            for (int i = 0; i < this.components.Length; i++) {
                opt [i] = this.components [i].Bind (ctenv);
                anyChange = anyChange || (opt [i] != this.components [i]);
            }
            return anyChange
                ? Combination.Make (opt)
                : this;
        }

         public override bool CallsTheEnvironment ()
         {
             foreach (SCode component in this.components)
                 if (component.CallsTheEnvironment ()) 
                     return true;
             return false;
         }

#if DEBUG
         public override string Key ()
         {
             string answer = "(combination ";
             foreach (SCode component in components)
                 answer += component.Key ();
             return answer + ")";
         }
#endif

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination.EvalStep");
            combinationSizeHistogram.Note (this.components.Length);
#endif
            object rator = null;
            object [] evaluated = new object [this.components.Length - 1];
            int counter = this.components.Length - 1;
            while (counter > -1) {
                Control expr = components [counter];
#if DEBUG
                noteCalls ((SCode)expr);
#endif
                Environment env = environment;
                object ev;
                while (expr.EvalStep (out ev, ref expr, ref env)) { };
#if DEBUG
                SCode.location = "Combination.EvalStep.1";
#endif
                if (ev == Interpreter.UnwindStack) {
                    ((UnwinderState) env).AddFrame (new CombinationFrame (this, environment, evaluated, counter));
                    environment = env;
                    answer = Interpreter.UnwindStack;
                    return false;
                }
                if (counter == 0) {
                    rator = ev;
                    break;
                }
                else
                    evaluated [--counter] = ev;
            }
            switch (evaluated.Length) {
                case 0:
                    return Interpreter.Call (out answer, ref expression, ref environment, rator);
                case 1:
                    Debugger.Break ();
                    return Interpreter.Call (out answer, ref expression, ref environment, rator, evaluated[0]);
                case 2:
                    Debugger.Break ();
                    return Interpreter.Call (out answer, ref expression, ref environment, rator, evaluated [0], evaluated[1]);
                case 3:
                    return Interpreter.Call (out answer, ref expression, ref environment, rator, evaluated [0], evaluated [1], evaluated[2]);
                case 4:
                    return Interpreter.Call (out answer, ref expression, ref environment, rator, evaluated [0], evaluated [1], evaluated [2], evaluated[3]);
                default:
                    return Interpreter.Apply (out answer, ref expression, ref environment, rator, evaluated);
            }
        }

        internal static object FromList (Cons cons)
        {
            return Combination.Make (cons.ToVector ());
        }

        public override bool MutatesAny (object [] formals)
        {
            foreach (SCode component in this.components)
                if (component.MutatesAny (formals))
                    return true;
            return false;
        }

        #region ISystemVector Members
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public int SystemVectorSize
        {
            get { return this.components.Length; }
        }

        public object SystemVectorRef (int index)
        {
            return UnwrapQuoted (this.components [index]);
        }

        public object SystemVectorSet (int index, object newValue)
        {
            throw new NotImplementedException ();
        }

        #endregion

        public override SCode Substitute (object name, object newObject)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    sealed class CombinationFrame : SubproblemContinuation<Combination>, ISystemVector
    {
        object [] evaluated;
        int counter;

        public CombinationFrame (Combination combination, Environment environment, object [] evaluated, int counter)
            :base (combination, environment)
        {
            this.evaluated = (object []) (evaluated.Clone());
            this.counter = counter;
        }

        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object value)
        {
            object [] evaluated = this.evaluated;
            int counter = this.counter;
            evaluated [counter--] = value;

            while (counter > -1) {
                Control expr = this.expression.Components [counter];
                Environment env = environment;
                object ev = null;
                while (expr.EvalStep (out ev, ref expr, ref env)) { };
                if (ev == Interpreter.UnwindStack) {
                    ((UnwinderState) env).AddFrame (new CombinationFrame (this.expression, environment, evaluated, counter));
                    environment = env;
                    answer = Interpreter.UnwindStack;
                    return false;
                }
                evaluated [counter--] = ev;
            }

            return Interpreter.Apply (out answer, ref expression, ref environment, evaluated [0], evaluated);

         }

        #region ISystemVector Members

        public int SystemVectorSize
        {
            get { return this.evaluated.Length + 4; }
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
    }

    [Serializable]
    class Combination0 : SCode, ISystemVector
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode rator;

        protected Combination0 (SCode rator)
            : base (TC.COMBINATION)
        {
            if (rator == null)
                throw new ArgumentNullException ("rator");
            this.rator = rator;
        }

        Combination0 (object rator)
            : base (TC.COMBINATION)
        {
            this.rator = EnsureSCode (rator);
        }

        static SCode SpecialMake (StaticLambda rator)
        {
            // Can you believe this happens too often?!
           // Debug.WriteLine ("; Optimize (let () <lambdaBody>) => <lambdaBody>");
            Debug.Write (".");
            return rator.Body;
        }

        public static SCode Make (object rator)
        {
            return 
                (rator is LexicalVariable) ? Combination0L.Make ((LexicalVariable)rator)
                // Combination of no arguments simply applied, just insert the lambdaBody.
                // This confuses the pretty printer, though.
                //: (rator is StaticLambda && ((Lambda) rator).Formals.Length == 0) ? SpecialMake ((StaticLambda) rator)
                : (rator is StaticLambda && ((Lambda) rator).Formals.Length == 0) ? SpecialCombination0.Make ((StaticLambda) rator)
                : new Combination0 (rator);
        }

        public SCode Operator
        {
            [DebuggerStepThrough]
            get
            {
                return this.rator;
            }
        }

        public override SCode Bind (LexicalMap ctenv)
        {
            SCode optimizedRator = this.rator.Bind (ctenv);
            return optimizedRator == this.rator 
                ? this
                : Combination0.Make (optimizedRator);
        }

        public override bool CallsTheEnvironment ()
        {
            return this.rator.CallsTheEnvironment ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination0.EvalStep");
            noteCalls (this.rator);
#endif
            object evop = null;
            Control unevop = this.rator;
            Environment env = environment;
            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination0Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop);
        }

        public override bool MutatesAny (object [] formals)
        {
            return this.rator.MutatesAny (formals);
        }

        #region ISystemVector Members

        public int SystemVectorSize
        {
            get { return 1; }
        }

        public object SystemVectorRef (int index)
        {
            if (index == 0)
                return this.rator;
            else
                throw new NotImplementedException ();
        }

        public object SystemVectorSet (int index, object newValue)
        {
            throw new NotImplementedException ();
        }

        #endregion


        public override SCode Substitute (object name, object newObject)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    sealed class Combination0Frame0 : SubproblemContinuation<Combination0>, ISystemVector
    {

        public Combination0Frame0 (Combination0 combination0, Environment environment)
            : base (combination0, environment)
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
            return Interpreter.Call (out answer, ref expression, ref environment, value);
        }
    }

    class SpecialCombination0 : Combination0
    {
        string name;
        object [] formals;
        SCode body;

        protected SpecialCombination0 (StaticLambda rator)
            : base (rator)
        {
            this.name = rator.Name;
            this.formals = rator.Formals;
            this.body = rator.Body;
        }

        public static SCode Make (StaticLambda rator)
        {
            return new SpecialCombination0 (rator);
        }


        public override SCode Bind (LexicalMap ctenv)
        {
            // skip the lambda expression
            SCode optimizedBody = this.body.Bind (ctenv);
            return optimizedBody == this.body
                ? this
                : Combination0.Make (new StaticLambda (this.name, this.formals, optimizedBody));
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("SpecialCombination0.EvalStep");
#endif
            expression = this.body;
            answer = null;
            return true;
        }
    }


    [Serializable]
    class Combination0L : Combination0
    {
        public object ratorName;
        public int ratorDepth;
        public int ratorOffset;

        protected Combination0L (LexicalVariable rator)
            : base (rator)
        {
            this.ratorName = rator.Name;
            this.ratorDepth = rator.Depth;
            this.ratorOffset = rator.Offset;
        }

        public static SCode Make (LexicalVariable rator)
        {
            return 
                (rator is Argument) ? Combination0A.Make ((Argument) rator)
                : (rator is LexicalVariable1) ? Combination0L1.Make ((LexicalVariable1) rator)
                : new Combination0L (rator);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination0L.EvalStep");
#endif
            object evop;
            if (environment.FastLexicalRef (out evop, this.ratorName, this.ratorDepth, this.ratorOffset))
                throw new NotImplementedException ();
            return Interpreter.Call (out answer, ref expression, ref environment, evop);
        }
    }

    [Serializable]
    class Combination0A : Combination0L
    {
        protected Combination0A (Argument rator)
            : base (rator)
        {
        }

        public static SCode Make (Argument rator)
        {
            return (rator is Argument0) ? Combination0A0.Make ((Argument0)rator)
                : (rator is Argument1) ? Combination0A1.Make ((Argument1)rator)
                : new Combination0A (rator);
        }
        
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination0A.EvalStep");
#endif
            return Interpreter.Call (out answer, ref expression, ref environment, environment.ArgumentValue (this.ratorOffset));
        }
    }

    [Serializable]
    class Combination0A0 : Combination0A
    {
        protected Combination0A0 (Argument0 rator)
            : base (rator)
        {
        }

        public static SCode Make (Argument0 rator)
        {
            return  new Combination0A0 (rator);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination0A0.EvalStep");
#endif
            return Interpreter.Call (out answer, ref expression, ref environment, environment.Argument0Value);
        }
    }

    [Serializable]
    class Combination0A1 : Combination0A
    {
        protected Combination0A1 (Argument1 rator)
            : base (rator)
        {
        }

        public static SCode Make (Argument1 rator)
        {
            return new Combination0A1 (rator);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination0A1.EvalStep");
#endif
            return Interpreter.Call (out answer, ref expression, ref environment, environment.Argument1Value);
        }
    }

    [Serializable]
    class Combination0L1 : Combination0L
    {
        protected Combination0L1 (LexicalVariable1 rator)
            : base (rator)
        {
        }

        public static SCode Make (LexicalVariable1 rator)
        {
            return  new Combination0L1 (rator);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination0L1.EvalStep");
#endif
            object evop;
            if (environment.FastLexicalRef1 (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException();
            return Interpreter.Call (out answer, ref expression, ref environment, evop);
        }
    }

//    [Serializable]
//    class Combination3L1SSS : Combination3
//    {
//#if DEBUG
//        static Histogram<Type> c1TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> c2TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> c3TypeHistogram = new Histogram<Type> ();
//#endif
//        protected object lambdaName;
//        protected int argOffset;

//        protected Combination3L1SSS (LexicalVariable1 rator, SCode rand0, SCode rand1, SCode rand2)
//            : base (rator, rand0, rand1, rand2)
//        {
//            this.lambdaName = rator.Name;
//            this.argOffset = rator.Offset;
//        }

//        public static SCode Make (LexicalVariable1 rator, SCode rand0, SCode rand1, SCode rand2)
//        {
//            return (rand2 is Argument) ? Combination3L1SSA.Make (rator, rand0, rand1, (Argument) rand2)
//                : new Combination3L1SSS (rator, rand0, rand1, rand2);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            noteCalls (this.components [0]);
//            noteCalls (this.components [1]);
//            noteCalls (this.components [2]);
//            noteCalls (this.components [3]);
//            c1TypeHistogram.Note (this.c1Type);
//            c2TypeHistogram.Note (this.c2Type);
//            c3TypeHistogram.Note (this.c3Type);

//#endif
//            object ev2;
//            Environment env = environment;
//            Control unev = this.components [3];
//            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
//            if (ev2 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination3Frame0 (this, environment));
//                //environment = env;
//                //answer = Interpreter.UnwindStack;
//                //return false;
//            }

//            object ev1;
//            env = environment;
//            unev = this.components [2];
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination3Frame1 (this, environment, ev2));
//                //environment = env;
//                //answer = Interpreter.UnwindStack;
//                //return false;
//            }

//            object ev0;
//            env = environment;
//            unev = this.components [1];
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            if (ev0 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination3Frame2 (this, environment, ev1, ev2));
//                //environment = env;
//                //answer = Interpreter.UnwindStack;
//                //return false;
//            }

//            object evop;
//            if (environment.FastLexicalRef1 (out evop, this.lambdaName, this.argOffset))
//                throw new NotImplementedException ();
//            //env = environment;
//            //unev = this.components [0];
//            //while (unev.EvalStep (out evop, ref unev, ref env)) { };
//            //if (evop == Interpreter.UnwindStack) {
//            //    throw new NotImplementedException ();
//            //    //((UnwinderState) env).AddFrame (new Combination3Frame3 (this, environment, ev0, ev1, ev2));
//            //    //environment = env;
//            //    //answer = Interpreter.UnwindStack;
//            //    //return false;
//            //}

//            // expression = (SCode) evop;
//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1, ev2);
//        }

//    }

//    [Serializable]
//    class Combination3L1SSA : Combination3L1SSS
//    {
//#if DEBUG
//        static Histogram<Type> c1TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> c2TypeHistogram = new Histogram<Type> ();
//#endif

//        protected int a2offset;

//        protected Combination3L1SSA (LexicalVariable1 rator, SCode rand0, SCode rand1, Argument rand2)
//            : base (rator, rand0, rand1, rand2)
//        {
//            this.a2offset = rand2.Offset;
//        }

//        public static SCode Make (LexicalVariable1 rator, SCode rand0, SCode rand1, Argument rand2)
//        {
//            return new Combination3L1SSA (rator, rand0, rand1, rand2);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            noteCalls (this.components [1]);
//            noteCalls (this.components [2]);
//            c1TypeHistogram.Note (this.c1Type);
//            c2TypeHistogram.Note (this.c2Type);

//#endif
//            object ev2 = environment.ArgumentValue (this.a2offset);

//            object ev1;
//            Environment env = environment;
//            Control unev = this.components [2];
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination3Frame1 (this, environment, ev2));
//                //environment = env;
//                //answer = Interpreter.UnwindStack;
//                //return false;
//            }

//            object ev0;
//            env = environment;
//            unev = this.components [1];
//            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
//            if (ev0 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination3Frame2 (this, environment, ev1, ev2));
//                //environment = env;
//                //answer = Interpreter.UnwindStack;
//                //return false;
//            }

//            object evop;
//            if (environment.FastLexicalRef1 (out evop, this.lambdaName, this.argOffset))
//                throw new NotImplementedException ();
//            //env = environment;
//            //unev = this.components [0];
//            //while (unev.EvalStep (out evop, ref unev, ref env)) { };
//            //if (evop == Interpreter.UnwindStack) {
//            //    throw new NotImplementedException ();
//            //    //((UnwinderState) env).AddFrame (new Combination3Frame3 (this, environment, ev0, ev1, ev2));
//            //    //environment = env;
//            //    //answer = Interpreter.UnwindStack;
//            //    //return false;
//            //}

//            // expression = (SCode) evop;
//            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1, ev2);
//        }

//    }

    [Serializable]
    class Combination4 : Combination
    {
        protected Combination4 (SCode rator, SCode rand0, SCode rand1, SCode rand2, SCode rand3)
            : base (new SCode [] { rator, rand0, rand1, rand2, rand3 })
        {
        }

        public static SCode Make (SCode rator, SCode rand0, SCode rand1, SCode rand2, SCode rand3)
        {
            return new Combination4 (rator, rand0, rand1, rand2, rand3);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination4.EvalStep.0");
            noteCalls (this.components [0]);
            noteCalls (this.components [1]);
            noteCalls (this.components [2]);
            noteCalls (this.components [3]);
            noteCalls (this.components [4]);
            SCode.location = "Combination4.EvalStep";
#endif
            object ev3 = null;
            Environment env = environment;
            Control unev3 = this.components [4];
            while (unev3.EvalStep (out ev3, ref unev3, ref env)) { };
#if DEBUG
                        SCode.location = "Combination4.EvalStep.1";
#endif
            if (ev3 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                //environment = env;
                //answer = Interpreter.UnwindStack;
                //return false;
            }
            object ev2 = null;
            env = environment;
            Control unev2 = this.components [3];
            while (unev2.EvalStep (out ev2, ref unev2, ref env)) { };
#if DEBUG
            SCode.location = "Combination4.EvalStep.2";
#endif
            if (ev2 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                //environment = env;
                //answer = Interpreter.UnwindStack;
                //return false;
            }

            object ev1 = null;
            env = environment;
            Control unev1 = this.components [2];
            while (unev1.EvalStep (out ev1, ref unev1, ref env)) { };
#if DEBUG
            SCode.location = "Combination4.EvalStep.3";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                //environment = env;
                //answer = Interpreter.UnwindStack;
                //return false;
            }

            object ev0 = null;
            env = environment;
            Control unev0 = this.components [1];
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
            SCode.location = "Combination4.EvalStep.4";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination4Frame3 (this, environment, ev1, ev2, ev3));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object evop = null;
            env = environment;
            Control unevop = this.components [0];
            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
#if DEBUG
            SCode.location = "Combination4.EvalStep.5";
#endif
            if (evop == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination4Frame4 (this, environment, ev0, ev1, ev2, ev3));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }
#if DEBUG
            SCode.location = "Combination4.EvalStep.6";
#endif
            // expression = (SCode) evop;
            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1, ev2, ev3);
        }

    }

    [Serializable]
    class Combination4Frame3 : SubproblemContinuation<Combination4>, ISystemVector
    {
        readonly object ev1;
        readonly object ev2;
        readonly object ev3;

        public Combination4Frame3 (Combination4 combination, Environment environment, object ev1, object ev2, object ev3)
            : base (combination, environment)
        {
            this.ev1 = ev1;
            this.ev2 = ev2;
            this.ev3 = ev3;
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
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, value, this.ev1, this.ev2, this.ev3);
        }
    }


    [Serializable]
    class Combination4Frame4 : SubproblemContinuation<Combination4>, ISystemVector
    {
        readonly object ev0;
        readonly object ev1;
        readonly object ev2;
        readonly object ev3;

        public Combination4Frame4 (Combination4 combination, Environment environment, object ev0, object ev1, object ev2, object ev3)
            : base (combination, environment)
        {
            this.ev0 = ev0;
            this.ev1 = ev1;
            this.ev2 = ev2;
            this.ev3 = ev3;
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
            return Interpreter.Call (out answer, ref expression, ref environment, value, this.ev0, this.ev1, this.ev2, this.ev3);
        }
    }


    [Serializable]
    sealed class PrimitiveCombination0 : SCode
    {

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly Primitive0 procedure;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        [NonSerialized]
        PrimitiveMethod0 method;

        public PrimitiveCombination0 (Primitive0 procedure)
            : base (TC.PCOMB0)
        {
            if (procedure == null) throw new ArgumentNullException ("procedure");
            this.procedure = procedure;
            this.method = procedure.Method;
        }

        [OnDeserialized ()]
        internal void OnDeserializedMethod (StreamingContext context)
        {
            this.method = procedure.Method;
        } 

        public Primitive0 Operator
        {
            [DebuggerStepThrough]
            get
            {
                return this.procedure;
            }
        }

        [SchemePrimitive ("PRIMITIVE-COMBINATION0?", 1, true)]
        public static bool IsPrimitiveCombination0 (out object answer, object arg)
        {
            answer = arg is PrimitiveCombination0;
            return false;
        }

        public override SCode Bind (LexicalMap ctenv)
        {
            return this;
        }

        public override bool CallsTheEnvironment ()
        {
            return false;
        }

        public override bool EvalStep (out object answer, ref Control expression,ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCombination0.EvalStep");
            Primitive.hotPrimitives.Note (this.procedure);
            Debug.WriteLineIf (Primitive.Noisy, this.procedure.ToString ());
#endif 
            if (this.method (out answer)) {
                throw new NotImplementedException ();
            }
            else return false;
        }

        public override bool MutatesAny (object [] formals)
        {
            return false;
        }

        public override SCode Substitute (object name, object newObject)
        {
            throw new NotImplementedException ();
        }
    }
    
    [Serializable]
    class PrimitiveCombination3 : SCode
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand2TypeHistogram = new Histogram<Type> ();
        static Histogram<string> histogram = new Histogram<String> ();
        protected string histogramKey;

        protected Type rand0Type;
        protected Type rand1Type;
        protected Type rand2Type;
#endif
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly Primitive3 procedure;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        [NonSerialized]
        protected PrimitiveMethod3 method;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly SCode arg0;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly SCode arg1;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly SCode arg2;

        protected PrimitiveCombination3 (Primitive3 procedure, SCode arg0, SCode arg1, SCode arg2)
            : base (TC.PCOMB3)
        {
            if (procedure == null) throw new ArgumentNullException ("procedure");
            this.procedure = procedure;
            this.method = procedure.Method;
            this.arg0 = arg0;
            this.arg1 = arg1;
            this.arg2 = arg2;
#if DEBUG
            this.histogramKey = procedure.ToString () + " " + this.arg0.GetType ().Name.ToString () + " " + this.arg1.GetType ().Name.ToString () + " " + this.arg2.GetType ().ToString ();
            rand0Type = this.arg0.GetType ();
            rand1Type = this.arg1.GetType ();
            rand2Type = this.arg2.GetType ();
#endif
        }

        public static SCode Make (Primitive3 procedure, SCode arg0, SCode arg1, SCode arg2)
        {
            return
                (arg1 is Quotation) ? PrimitiveCombination3SQ.Make (procedure, arg0, (Quotation) arg1, arg2)
                : new PrimitiveCombination3 (procedure, arg0, arg1, arg2);
        }

        public static SCode Make (object procedure, object arg0, object arg1, object arg2)
        {
            return
                PrimitiveCombination3.Make ((Primitive3) procedure, EnsureSCode(arg0), EnsureSCode(arg1), EnsureSCode(arg2));
        }

        [OnDeserialized ()]
        internal void OnDeserializedMethod (StreamingContext context)
        {
            this.method = procedure.Method;
        } 

        public Primitive3 Operator
        {
            [DebuggerStepThrough]
            get
            {
                return this.procedure;
            }
        }

        public SCode Operand0
        {
            [DebuggerStepThrough]
            get
            {
                return this.arg0;
            }
        }

        public SCode Operand1
        {
            [DebuggerStepThrough]
            get
            {
                return this.arg1;
            }
        }

        public SCode Operand2
        {
            [DebuggerStepThrough]
            get
            {
                return this.arg2;
            }
        }


        [SchemePrimitive ("PRIMITIVE-COMBINATION3?", 1, true)]
        public static bool IsPrimitiveCombination3 (out object answer, object arg)
        {
            answer = arg is PrimitiveCombination3;
            return false;
        }

        public override SCode Bind (LexicalMap ctenv)
        {
            SCode o0 = this.Operand0.Bind (ctenv);
            SCode o1 = this.Operand1.Bind (ctenv);
            SCode o2 = this.Operand2.Bind (ctenv);
            return (o0 == this.Operand0
                && o1 == this.Operand1
                && o2 == this.Operand2) ? this
                : new PrimitiveCombination3 (this.procedure, o0, o1, o2);
        }

        public override bool CallsTheEnvironment ()
        {
            return this.arg0.CallsTheEnvironment ()
                || this.arg1.CallsTheEnvironment ()
                || this.arg2.CallsTheEnvironment ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.arg0);
            noteCalls (this.arg1);
            noteCalls (this.arg2);
            histogram.Note (this.histogramKey);
            ratorHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
            rand2TypeHistogram.Note (this.rand2Type);
            SCode.location = "PrimitiveCombination3.EvalStep";
#endif
            object ev2;
            Environment env = environment;
            Control unev = this.arg2;
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3.EvalStep.1";
#endif
            if (ev2 == Interpreter.UnwindStack) throw new NotImplementedException ();

            object ev1;
            env = environment;
            unev = this.arg1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3.EvalStep.2";
#endif
            if (ev1 == Interpreter.UnwindStack) throw new NotImplementedException ();

            object ev0;
            env = environment;
            unev = this.arg0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };

#if DEBUG
            SCode.location = "PrimitiveCombination3.EvalStep.3";
#endif
            if (ev0 == Interpreter.UnwindStack) throw new NotImplementedException (); 

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            Debug.WriteLineIf (Primitive.Noisy, this.procedure.ToString ());
            SCode.location = this.procedure.Name;
#endif
            if (this.method (out answer, ev0, ev1, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }

        public override bool MutatesAny (object [] formals)
        {
            return this.arg0.MutatesAny (formals)
                || this.arg1.MutatesAny (formals)
                || this.arg2.MutatesAny (formals);
        }

        public override SCode Substitute (object name, object newObject)
        {
            throw new NotImplementedException ();
        }
    }

    class PrimitiveCombination3SQ : PrimitiveCombination3
    {
#if DEBUG
        [NonSerialized]
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand2TypeHistogram = new Histogram<Type> ();
        static Histogram<string> histogram = new Histogram<String> ();
#endif
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public readonly object arg1Value;


        PrimitiveCombination3SQ (Primitive3 procedure, SCode arg0, Quotation arg1, SCode arg2)
            : base (procedure, arg0, arg1, arg2)
        {
            this.arg1Value = arg1.Quoted;
        }

        public static SCode Make (Primitive3 procedure, SCode arg0, Quotation arg1, SCode arg2)
        {
            return
                 new PrimitiveCombination3SQ (procedure, arg0, arg1, arg2);
        }



        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCombination3SQ.EvalStep");
            noteCalls (this.arg0);
            noteCalls (this.arg2);
            histogram.Note (this.histogramKey);
            ratorHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            rand2TypeHistogram.Note (this.rand2Type);
#endif
            object ev2;
            Environment env = environment;
            Control unev = this.arg2;
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
            if (ev2 == Interpreter.UnwindStack) throw new NotImplementedException ();

            object ev0;
            env = environment;
            unev = this.arg0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) throw new NotImplementedException ();

            // It may be expensive to bounce down to invoke the procedure
            // we should invoke it directly and pass along the ref args.
            // Calling directly may break tail recursion for primitives
            // that call back.
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            Debug.WriteLineIf (Primitive.Noisy, this.procedure.ToString ());
#endif
            if (this.method (out answer, ev0, this.arg1Value, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }


//    [Serializable]
//    class PrimitiveCombination3LSS : PrimitiveCombination3
//    {
//#if DEBUG
//        [NonSerialized]
//        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
//        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> rand2TypeHistogram = new Histogram<Type> ();
//#endif
//        protected object l0name;
//        protected int l0depth;
//        protected int l0offset;

//        public PrimitiveCombination3LSS (Primitive3 procedure, LexicalVariable arg0, object arg1, object arg2)
//            : base (procedure, arg0, arg1, arg2)
//        {
//            this.l0name = arg0.Name;
//            this.l0depth = arg0.Depth;
//            this.l0offset = arg0.Offset;
//        }

//        public static PrimitiveCombination3LSS Make (Primitive3 procedure, LexicalVariable arg0, object arg1, object arg2)
//        {
//            return new PrimitiveCombination3LSS (procedure, arg0, arg1, arg2);
//        }

//        public override SCode Bind (LexicalMap ctenv)
//        {
//            throw new NotImplementedException ("Shouldn't be necessary.");
//        }
 
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//            noteCalls (this.arg0);
//            noteCalls (this.arg1);
//            noteCalls (this.arg2);
//            ratorHistogram.Note (this.procedure);
//            rand1TypeHistogram.Note (this.rand1Type);
//            rand2TypeHistogram.Note (this.rand2Type);
//#endif
//            object ev2;
//            Environment env = environment;
//            Control unev = this.arg2;
//            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
//            if (ev2 == Interpreter.UnwindStack) throw new NotImplementedException ();

//            object ev1;
//            env = environment;
//            unev = this.arg1;
//            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
//            if (ev1 == Interpreter.UnwindStack) throw new NotImplementedException ();

//            object ev0;
//            if (environment.LexicalRef (out ev0, this.l0name, this.l0depth, this.l0offset))
//                throw new NotImplementedException ();
//            // It may be expensive to bounce down to invoke the procedure
//            // we should invoke it directly and pass along the ref args.
//            // Calling directly may break tail recursion for primitives
//            // that call back.
//#if DEBUG
//            Primitive.hotPrimitives.Note (this.procedure);
//            Debug.WriteLineIf (Primitive.Noisy, this.procedure.ToString ());
//#endif
//            if (this.method (out answer, ev0, ev1, ev2)) {
//                TailCallInterpreter tci = answer as TailCallInterpreter;
//                if (tci != null) {
//                    expression = tci.Expression;
//                    environment = tci.Environment;
//                    answer = null;
//                    return true;
//                }
//                throw new NotImplementedException ();
//            }
//            return false;
//        }

//    }

}

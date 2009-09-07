using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;

namespace Microcode
{
    [Serializable]
    class Let1 : Combination1
    {
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> randTypeHistogram = new Histogram<Type> ();

        protected Type bodyType;
#endif
        protected object [] formals;
        public SCode body;

        protected Let1 (Lambda rator, SCode rand)
            : base (rator, rand)
        {
            formals = rator.Formals;
            body = rator.Body;
#if DEBUG
            if (body != null)
                this.bodyType = body.GetType ();
#endif
        }

        static SCode RewriteAsIdentity (SCode arg0)
        {
            //Debug.Write ("\n; Rewrite Let1 as identity.");
            return arg0;
        }

        static SCode RewriteAsSequence (SCode arg0, SCode body)
        {
            //Debug.Write ("\n; Rewrite Let1 as sequence.");
            return Sequence2.Make (arg0, body);
        }

        static SCode StandardMake (Lambda rator, SCode arg0)
        {
            object [] formals = rator.Formals;
            SCode body = rator.Body;

            return
                //(body is Variable) ? ((((Variable) body).Name == formals [0]) ? RewriteAsIdentity (arg0) : RewriteAsSequence(arg0, body)) :
                (rator is SimpleLambda) ? SimpleLet1.Make ((SimpleLambda) rator, arg0) :
                (arg0 is LexicalVariable) ? Let1L.Make (rator, (LexicalVariable) arg0) :
                (arg0 is Quotation) ? Let1Q.Make (rator, (Quotation) arg0) :
                new Let1 (rator, arg0);
        }

        public static SCode Make (Lambda rator, SCode arg0)
        {
            object [] formals = rator.Formals;
            SCode body = rator.Body;

            //if (!body.Uses (formals [0])) {
            //    Debug.Write ("\nSimplifying useless let.");
            //    return Sequence2.Make (arg0, body);
            //}
            //if (body is Variable) {
            //    if (((Variable) body).Name == formals [0]) {
            //        Debug.Write ("\nEta reduce operator.");
            //        return arg0;
            //    }
            //    else {
            //        Debugger.Break ();
            //        return Sequence2.Make (arg0, body);
            //    }
            //}
            //else if (body is Disjunction) {
            //    SCode pred = ((Disjunction) body).Predicate;
            //    SCode alt = ((Disjunction) body).Alternative;
            //    if (!alt.Uses (formals [0])) {
            //        Debug.Write ("\n; Hoisting disjunction.");
            //        return Disjunction.Make (Combination1.Make (Lambda.Make (rator.Name, formals, pred), arg0), alt);
            //    }
            //}
            //else if (body is Conditional) {
            //    SCode pred = ((Conditional) body).Predicate;
            //    SCode cons = ((Conditional) body).Consequent;
            //    SCode alt = ((Conditional) body).Alternative;
            //    if (cons.Uses (formals [0]) || alt.Uses (formals [0]))
            //        return StandardMake (rator, arg0);
            //    else {
            //        Debug.Write ("\n; Hoisting Conditional.");
            //        return Conditional.Make (Combination1.Make (Lambda.Make (rator.Name, formals, pred), arg0), cons, alt);
            //    }
            //}
            //else if (body is Combination1 &&
            //    ((Combination1) body).Operator is Lambda &&
            //    !((Combination1) body).Operator.Uses (formals [0])) {
            //    Debug.Write ("\n; Drifting LET.");
            //    return Combination1.Make (((Combination1) body).Operator,
            //                              Combination1.Make (Lambda.Make (rator.Name, formals, ((Combination1) body).Operand),
            //                                                              arg0));
            //}
            //else if (body is Combination1 &&
            //         ((Combination1) body).Operator is Lambda &&
            //         !((Combination1) body).Operand.Uses (formals [0])) {
            //    //Debugger.Break ();
            //}

            //else if (body is PrimitiveCombination1) {
            //    SCode arg = ((PrimitiveCombination1) body).Operand;
            //    if (arg is Variable && ((Variable) arg).Name == formals [0]) {
            //        Debug.Write ("\n; Eliding LET");
            //        return PrimitiveCombination1.Make (((PrimitiveCombination1) body).Operator, arg0);
            //    }
            //}
            return
                StandardMake (rator, arg0);


        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Let1.EvalStep");
            noteCalls (this.rator);
            noteCalls (this.rand);
            ratorTypeHistogram.Note (this.ratorType);
            randTypeHistogram.Note (this.randType);
#endif

            object evarg = null;
            Control unev = this.rand;
            Environment env = environment;
            while (unev.EvalStep (out evarg, ref unev, ref env)) { };
            if (evarg == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Let1Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object evop = null;
            Control unevop = this.rator;
            env = environment;
            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination1Frame1 (this, environment, evarg));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, evarg);
        }

        public object [] Formals
        {
            get
            {
                return this.formals;
            }
        }

        public SCode Body
        {
            get
            {
                return this.body;
            }
        }
    }

    [Serializable]
    sealed class Let1Frame0 : SubproblemContinuation<Let1>, ISystemVector
    {
        public Let1Frame0 (Let1 combination1, Environment environment)
            : base (combination1, environment)
        {
        }

        #region ISystemVector Members

        public int SystemVectorSize
        {
            get { return 3; }
        }

        public object SystemVectorRef (int index)
        {
            switch (index) {
                case 0: return ReturnCode.COMB_1_PROCEDURE;
                case 1: return this.expression;
                case 2: return this.environment;
                default:
                    throw new NotImplementedException ();
            }
        }

        public object SystemVectorSet (int index, object newValue)
        {
            throw new NotImplementedException ();
        }

        #endregion

        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object value)
        {
            object evop = null;
            Control unevop = this.expression.Operator;
            Environment env = this.environment;
            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
            if (evop == Interpreter.UnwindStack) 
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, value);
        }
    }

    [Serializable]
    class Let1L : Let1
    {
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object argumentName;
        public readonly int argumentDepth;
        public readonly int argumentOffset;

        protected Let1L (Lambda rator, LexicalVariable rand)
            : base (rator, rand)
        {
            this.argumentName = rand.Name;
            this.argumentDepth = rand.Depth;
            this.argumentOffset = rand.Offset;
        }

        public static SCode Make (Lambda rator, LexicalVariable arg0)
        {
            return 
                (arg0 is LexicalVariable1) ? Unimplemented() :
                (arg0 is Argument) ? Let1A.Make (rator, (Argument) arg0) :
                new Let1L (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rator);
            noteCalls (this.rand);
            ratorTypeHistogram.Note (this.ratorType);
            SCode.location = "Let1L.EvalStep";
#endif

            object evarg;
            if (environment.FastLexicalRef (out evarg, this.argumentName, this.argumentDepth, this.argumentOffset))
                throw new NotImplementedException ();

            object evop;
            Control unevop = this.rator;
            Environment env = environment;
            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination1Frame1 (this, environment, evarg));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, evarg);
        }

    }

    [Serializable]
    class Let1A : Let1L
    {
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
#endif

        protected Let1A (Lambda rator, Argument rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (Lambda rator, Argument arg0)
        {
            return
                (arg0 is Argument0) ? Let1A0.Make (rator, (Argument0) arg0) :
                (arg0 is Argument1) ? Let1A1.Make (rator, (Argument1) arg0) :
                new Let1A (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("-");
            noteCalls (this.rator);
            noteCalls (this.rand);
            ratorTypeHistogram.Note (this.ratorType);
            SCode.location = "Let1L.EvalStep";
#endif

            object evarg;
            if (environment.FastLexicalRef (out evarg, this.argumentName, this.argumentDepth, this.argumentOffset))
                throw new NotImplementedException ();

            object evop;
            Control unevop = this.rator;
            Environment env = environment;
            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination1Frame1 (this, environment, evarg));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, evarg);
        }

    }

    [Serializable]
    class Let1A0 : Let1A
    {
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
#endif

        protected Let1A0 (Lambda rator, Argument0 rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (Lambda rator, Argument0 arg0)
        {
            return
                new Let1A0 (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("-");
            noteCalls (this.rator);
            noteCalls (this.rand);
            ratorTypeHistogram.Note (this.ratorType);
            SCode.location = "Let1L.EvalStep";
#endif

            object evarg;
            if (environment.FastLexicalRef (out evarg, this.argumentName, this.argumentDepth, this.argumentOffset))
                throw new NotImplementedException ();

            object evop;
            Control unevop = this.rator;
            Environment env = environment;
            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination1Frame1 (this, environment, evarg));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, evarg);
        }

    }

    [Serializable]
    class Let1A1 : Let1A
    {
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
#endif

        protected Let1A1 (Lambda rator, Argument1 rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (Lambda rator, Argument1 arg0)
        {
            return
                new Let1A1 (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rator);
            ratorTypeHistogram.Note (this.ratorType);
            SCode.location = "Let1A1.EvalStep";
#endif
            object evop;
            Control unevop = this.rator;
            Environment env = environment;
            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination1Frame1 (this, environment, evarg));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, environment.Argument1Value);
        }
    }

    [Serializable]
    class Let1Q : Let1
    {
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object argumentValue;

        protected Let1Q (Lambda rator, Quotation rand)
            : base (rator, rand)
        {
            this.argumentValue = rand.Quoted;
        }

        public static SCode Make (Lambda rator, Quotation arg0)
        {
            return
                //(arg0.Quoted == ReferenceTrap.Unassigned
                //&& rator.Body is Sequence2
                //&& (((Sequence2) rator.Body).First is Assignment)
                //&& ((Assignment) ((Sequence2) rator.Body).First).Name == rator.Formals [0]
                //&& ((Assignment) ((Sequence2) rator.Body).First).Value is Lambda
                //&& (((Sequence2) rator.Body).Second is Variable)
                //&& ((Variable) ((Sequence2) rator.Body).Second).Name == rator.Formals [0]) ? Letrec1.Make (rator) :
                new Let1Q (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Let1Q.EvalStep");
            noteCalls (this.rator);
            ratorTypeHistogram.Note (this.ratorType);
#endif
            object evop;
            Control unevop = this.rator;
            Environment env = environment;
            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination1Frame1 (this, environment, evarg));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, this.argumentValue);
        }
    }

    [Serializable]
    class SimpleLet1 : Let1
    {
#if DEBUG
        static Histogram<Type> bodyTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> randTypeHistogram = new Histogram<Type> ();
#endif
        protected SimpleLet1 (SimpleLambda rator, SCode rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (SimpleLambda rator, SCode arg0)
        {
            return
                (arg0 is LexicalVariable) ? SimpleLet1L.Make (rator, (LexicalVariable) arg0) :
                //(arg0 is PrimitiveCar) ? SimpleLet1Car.Make (rator, (PrimitiveCar) arg0) :
                //(arg0 is PrimitiveCdr) ? SimpleLet1Cdr.Make (rator, (PrimitiveCdr) arg0) :
                //(arg0 is StaticLambda) ? SimpleLet1StaticLambda.Make (rator, (StaticLambda) arg0) :
                (arg0 is Quotation) ? SimpleLet1Q.Make (rator, (Quotation) arg0) :
                new SimpleLet1 (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.body);
            bodyTypeHistogram.Note (this.bodyType);
            noteCalls (this.rand);
            randTypeHistogram.Note (this.randType);
            SCode.location = "SimpleLet1.EvalStep";
#endif
            object evarg;
            Control unev = this.rand;
            Environment env = environment;
            while (unev.EvalStep (out evarg, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "SimpleLet1.EvalStep.1";
#endif
            if (evarg == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new SimpleLet1Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.rator, environment);

            expression = this.body;
            environment = new SmallEnvironment1 (cl, evarg);
            answer = null;
            return true;
        }
    }

    [Serializable]
    sealed class SimpleLet1Frame0 : SubproblemContinuation<SimpleLet1>, ISystemVector
    {
        public SimpleLet1Frame0 (SimpleLet1 combination1, Environment environment)
            : base (combination1, environment)
        {
        }

        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object value)
        {
            SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.expression.rator, environment);

            expression = this.expression.body;
            environment = new SmallEnvironment1 (cl, value);
            answer = null;
            return true;
        }

        #region ISystemVector Members

                public int SystemVectorSize
        {
            get { return 3; }
        }

        public object SystemVectorRef (int index)
        {
            switch (index) {
                case 0: return ReturnCode.COMB_1_PROCEDURE;
                case 1: return this.expression;
                case 2: return this.environment;
                default:
                    throw new NotImplementedException ();
            }
        }

        public object SystemVectorSet (int index, object newValue)
        {
            throw new NotImplementedException ();
        }

        #endregion
    }

    [Serializable]
    class SimpleLet1L : SimpleLet1
    {
#if DEBUG
        static Histogram<Type> bodyTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> randTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object argName;
        public readonly int argDepth;
        public readonly int argOffset;

        protected SimpleLet1L (SimpleLambda rator, LexicalVariable rand)
            : base (rator, rand)
        {
            this.argName = rand.Name;
            this.argDepth = rand.Depth;
            this.argOffset = rand.Offset;
        }

        public static SCode Make (SimpleLambda rator, LexicalVariable arg0)
        {
            return
                (arg0 is Argument) ? SimpleLet1A.Make (rator, (Argument) arg0) :
                (arg0 is LexicalVariable1) ? SimpleLet1L1.Make (rator, (LexicalVariable1) arg0) :
                new SimpleLet1L (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.body);
            bodyTypeHistogram.Note (this.bodyType);
            SCode.location = "SimpleLet1L.EvalStep";
#endif

            object evarg;
            if (environment.FastLexicalRef (out evarg, this.argName, this.argDepth, this.argOffset))
                throw new NotImplementedException ();

            SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.rator, environment);

            expression = this.body;
            environment = new SmallEnvironment1 (cl, evarg);
            answer = null;
            return true;
        }
    }

    [Serializable]
    class SimpleLet1A : SimpleLet1L
    {
#if DEBUG
        static Histogram<Type> bodyTypeHistogram = new Histogram<Type> ();
#endif
        protected SimpleLet1A (SimpleLambda rator, Argument rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (SimpleLambda rator, Argument arg0)
        {
            return
                (arg0 is Argument0) ? SimpleLet1A0.Make (rator, (Argument0) arg0) :
                (arg0 is Argument1) ? SimpleLet1A1.Make (rator, (Argument1) arg0) :
                new SimpleLet1A (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.body);
            bodyTypeHistogram.Note (this.bodyType);
            SCode.location = "SimpleLet1A.EvalStep";
#endif
            object evarg = environment.ArgumentValue (this.argOffset);

            SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.rator, environment);

            expression = this.body;
            environment = new SmallEnvironment1 (cl, evarg);
            answer = null;
            return true;
        }
    }

    [Serializable]
    sealed class SimpleLet1A0 : SimpleLet1A
    {
#if DEBUG
        static Histogram<Type> bodyTypeHistogram = new Histogram<Type> ();
#endif
        SimpleLet1A0 (SimpleLambda rator, Argument0 rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (SimpleLambda rator, Argument0 arg0)
        {
            return
                new SimpleLet1A0 (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.body);
            bodyTypeHistogram.Note (this.bodyType);
            SCode.location = "SimpleLet1LA0.EvalStep";
#endif
            object evarg = environment.Argument0Value;
            SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.rator, environment);
            expression = this.body;
            environment = new SmallEnvironment1 (cl, evarg);
            answer = null;
            return true;
        }
    }

    [Serializable]
    sealed class SimpleLet1A1 : SimpleLet1A
    {
#if DEBUG
        static Histogram<Type> bodyTypeHistogram = new Histogram<Type> ();
#endif

        SimpleLet1A1 (SimpleLambda rator, Argument1 rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (SimpleLambda rator, Argument1 arg0)
        {
            return
                new SimpleLet1A1 (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.body);
            bodyTypeHistogram.Note (this.bodyType);
            SCode.location = "SimpleLet1LA1.EvalStep";
#endif
            object evarg = environment.Argument1Value;
            SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.rator, environment);
            expression = this.body;
            environment = new SmallEnvironment1 (cl, evarg);
            answer = null;
            return true;
        }
    }

    [Serializable]
    sealed class SimpleLet1L1 : SimpleLet1L
    {
#if DEBUG
        static Histogram<Type> bodyTypeHistogram = new Histogram<Type> ();
#endif

        SimpleLet1L1 (SimpleLambda rator, LexicalVariable1 rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (SimpleLambda rator, LexicalVariable1 arg0)
        {
            return
                new SimpleLet1L1 (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.body);
            bodyTypeHistogram.Note (this.bodyType);
            SCode.location = "SimpleLet1L1.EvalStep";
#endif
            object evarg;
            if (environment.FastLexicalRef1 (out evarg, this.argName, this.argOffset))
                throw new NotImplementedException ();

            SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.rator, environment);

            expression = this.body;
            environment = new SmallEnvironment1 (cl, evarg);
            answer = null;
            return true;
        }
    }

    [Serializable]
    class SimpleLet1Car : SimpleLet1
    {
#if DEBUG
        static Histogram<Type> bodyTypeHistogram = new Histogram<Type> ();
#endif
        protected SimpleLet1Car (SimpleLambda rator, PrimitiveCar rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (SimpleLambda rator, PrimitiveCar arg0)
        {
            return
                (arg0 is PrimitiveCarL) ? SimpleLet1CarL.Make (rator, (PrimitiveCarL) arg0) :
                new SimpleLet1Car (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            Unimplemented ();
#if DEBUG
            Warm ("-");
            noteCalls (this.body);
            bodyTypeHistogram.Note (this.bodyType);
            SCode.location = "SimpleLet1Car.EvalStep";
#endif

            object evarg;
            Control unev = this.rand;
            Environment env = environment;
            while (unev.EvalStep (out evarg, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "SimpleLet1Car.EvalStep.1";
#endif
            if (evarg == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new SimpleLet1Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }
            Cons temp = evarg as Cons;
            if (temp == null) throw new NotImplementedException ();

            SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.rator, environment);

            expression = this.body;
            environment = new SmallEnvironment1 (cl, temp.Car);
            answer = null;
            return true;
        }
    }

    [Serializable]
    class SimpleLet1CarL : SimpleLet1Car
    {
#if DEBUG
        static Histogram<Type> bodyTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand0Name;
        public readonly int rand0Depth;
        public readonly int rand0Offset;

        protected SimpleLet1CarL (SimpleLambda rator, PrimitiveCarL rand)
            : base (rator, rand)
        {
            this.rand0Name = rand.OperandName;
            this.rand0Depth = rand.OperandDepth;
            this.rand0Offset = rand.OperandOffset;
        }

        public static SCode Make (SimpleLambda rator, PrimitiveCarL arg0)
        {
            return
                (arg0 is PrimitiveCarA) ? SimpleLet1CarA.Make (rator, (PrimitiveCarA) arg0) :
                (arg0 is PrimitiveCarL1) ? SimpleLet1CarL1.Make (rator, (PrimitiveCarL1) arg0) :
                new SimpleLet1CarL (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("SimpleLet1CarL.EvalStep");
            noteCalls (this.body);
            bodyTypeHistogram.Note (this.bodyType);
#endif
            object temp;
            if (environment.FastLexicalRef (out temp, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();
            Cons evarg = temp as Cons;
            if (evarg == null) throw new NotImplementedException ();

            SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.rator, environment);

            expression = this.body;
            environment = new SmallEnvironment1 (cl, evarg.Car);
            answer = null;
            return true;

        }
    }

    [Serializable]
    class SimpleLet1CarA : SimpleLet1CarL
    {
#if DEBUG
        static Histogram<Type> bodyTypeHistogram = new Histogram<Type> ();
#endif
        protected SimpleLet1CarA (SimpleLambda rator, PrimitiveCarA rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (SimpleLambda rator, PrimitiveCarA arg0)
        {
            return
                (arg0 is PrimitiveCarA0) ? SimpleLet1CarA0.Make (rator, (PrimitiveCarA0) arg0) :
                (arg0 is PrimitiveCarA1) ? SimpleLet1CarA1.Make (rator, (PrimitiveCarA1) arg0) :
                new SimpleLet1CarA (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.body);
            bodyTypeHistogram.Note (this.bodyType);
            SCode.location = "SimpleLet1CarA.EvalStep";
#endif
            Cons evarg = environment.ArgumentValue (this.rand0Offset) as Cons;
            if (evarg == null) throw new NotImplementedException ();

            SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.rator, environment);

            expression = this.body;
            environment = new SmallEnvironment1 (cl, evarg.Car);
            answer = null;
            return true;
        }
    }

    [Serializable]
    class SimpleLet1CarA0 : SimpleLet1CarA
    {
#if DEBUG
        static Histogram<Type> bodyTypeHistogram = new Histogram<Type> ();
#endif
        protected SimpleLet1CarA0 (SimpleLambda rator, PrimitiveCarA0 rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (SimpleLambda rator, PrimitiveCarA0 arg0)
        {
            return
                //(rator.Body is Conditional) ? SimpleLet1CarA0Cond.Make (rator, (Conditional) rator.Body, arg0) :
                //(rator.Body is Sequence2) ? SimpleLet1CarA0Z2.Make ( rator, arg0)
                new SimpleLet1CarA0 (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("SimpleLet1CarA0.EvalStep");
            noteCalls (this.body);
            bodyTypeHistogram.Note (this.bodyType);
#endif

            Cons evarg = environment.Argument0Value as Cons;
            if (evarg == null) throw new NotImplementedException ();

            SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.rator, environment);

            expression = this.body;
            environment = new SmallEnvironment1 (cl, evarg.Car);
            answer = null;
            return true;
        }
    }

    [Serializable]
    class SimpleLet1CarA0Cond : SimpleLet1CarA0
    {
#if DEBUG
        static Histogram<Type> predicateTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();

        public readonly Type predicateType;
        public readonly Type consequentType;
        public readonly Type alternativeType;

#endif
        public readonly SCode predicate;
        public readonly SCode consequent;
        public readonly SCode alternative;

        protected SimpleLet1CarA0Cond (SimpleLambda rator, SCode predicate, SCode consequent, SCode alternative, PrimitiveCarA0 rand)
            : base (rator, rand)
        {
            this.predicate = predicate;
            this.consequent = consequent;
            this.alternative = alternative;
#if DEBUG
            this.predicateType = predicate.GetType ();
            this.consequentType = consequent.GetType ();
            this.alternativeType = alternative.GetType ();
#endif
        }

        public static SCode Make (SimpleLambda rator, Conditional body, PrimitiveCarA0 arg0)
        {
            return
                (body.Predicate is PrimitiveIsPairA0) ? SimpleLet1CarA0PairA0.Make (rator, (PrimitiveIsPairA0) body.Predicate, body.Consequent, body.Alternative, arg0) :
                new SimpleLet1CarA0Cond (rator, body.Predicate, body.Consequent, body.Alternative, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("SimpleLet1CarA0Cond.EvalStep");
#endif

            Cons evarg = environment.Argument0Value as Cons;
            if (evarg == null) throw new NotImplementedException ();


            SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.rator, environment);

            environment = new SmallEnvironment1 (cl, evarg.Car);

#if DEBUG
            noteCalls (this.predicate);
            predicateTypeHistogram.Note (this.predicateType);
#endif
            object ev;
            Control unev = this.predicate;
            Environment env = environment;

            while (unev.EvalStep (out ev, ref unev, ref env)) { };
            if (ev == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new ConditionalFrame (this, environment));
                //environment = env;
                //answer = Interpreter.UnwindStack;
                //return false;

            }

            if ((ev is bool) && (bool) ev == false) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }


        }

    }

    [Serializable]
    class SimpleLet1CarA0PairA0 : SimpleLet1CarA0Cond
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected SimpleLet1CarA0PairA0 (SimpleLambda rator, PrimitiveIsPairA0 predicate, SCode consequent, SCode alternative, PrimitiveCarA0 rand)
            : base (rator, predicate, consequent, alternative, rand)
        {
        }

        public static SCode Make (SimpleLambda rator, PrimitiveIsPairA0 predicate, SCode consequent, SCode alternative, PrimitiveCarA0 rand)
        {
            return
                (consequent is Conditional) ? SimpleLet1CarA0PairA0Cond.Make (rator, predicate, (Conditional) consequent, alternative, rand) :
                new SimpleLet1CarA0PairA0 (rator, predicate, consequent, alternative, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif

            Cons evarg = environment.Argument0Value as Cons;
            if (evarg == null) throw new NotImplementedException ();


            SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.rator, environment);

            environment = new SmallEnvironment1 (cl, evarg.Car);

            if (!(evarg.Car is Cons)) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }


        }

    }

    [Serializable]
    class SimpleLet1CarA0PairA0Cond : SimpleLet1CarA0PairA0
    {
#if DEBUG
        static Histogram<Type> predicate1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequent1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternative1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();

        public readonly Type predicate1Type;
        public readonly Type consequent1Type;
        public readonly Type alternative1Type;
#endif
        public readonly SCode predicate1;
        public readonly SCode consequent1;
        public readonly SCode alternative1;

        protected SimpleLet1CarA0PairA0Cond (SimpleLambda rator, PrimitiveIsPairA0 predicate, Conditional consequent, SCode alternative, PrimitiveCarA0 rand)
            : base (rator, predicate, consequent, alternative, rand)
        {
            this.predicate1 = consequent.Predicate;
            this.consequent1 = consequent.Consequent;
            this.alternative1 = consequent.Alternative;
#if DEBUG
            this.predicate1Type = this.predicate1.GetType ();
            this.consequent1Type = this.consequent1.GetType ();
            this.alternative1Type = this.alternative1.GetType ();
#endif
        }

        public static SCode Make (SimpleLambda rator, PrimitiveIsPairA0 predicate, Conditional consequent, SCode alternative, PrimitiveCarA0 rand)
        {
            return
                (consequent.Alternative is Combination1LCdrL) ? SLet1CA0PA0CondComb1LCdrL.Make (rator, predicate, consequent, consequent.Predicate, consequent.Consequent, (Combination1LCdrL) consequent.Alternative, alternative, rand) :
                 new SimpleLet1CarA0PairA0Cond (rator, predicate, consequent, alternative, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("SimpleLet1CarA0PairA0Cond.EvalStep");
#endif
            Cons evarg = environment.Argument0Value as Cons;
            if (evarg == null) throw new NotImplementedException ();

            SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.rator, environment);
            environment = new SmallEnvironment1 (cl, evarg.Car);

            if (!(evarg.Car is Cons)) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType); ;
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.predicate1);
                predicate1TypeHistogram.Note (this.predicate1Type);
#endif
                object ev;
                Control unev = this.predicate1;
                Environment env = environment;

                while (unev.EvalStep (out ev, ref unev, ref env)) { };
                if (ev == Interpreter.UnwindStack) {
                    throw new NotImplementedException ();
                    //((UnwinderState) env).AddFrame (new ConditionalFrame (this, environment));
                    //environment = env;
                    //answer = Interpreter.UnwindStack;
                    //return false;
                }

                if ((ev is bool) && (bool) ev == false) {
#if DEBUG
                    noteCalls (this.alternative1);
                    alternative1TypeHistogram.Note (this.alternative1Type);
#endif
                    expression = this.alternative1;
                    answer = null;
                    return true;
                }
                else {
#if DEBUG
                    noteCalls (this.consequent1);
                    consequent1TypeHistogram.Note (this.consequent1Type);
#endif
                    expression = this.consequent1;
                    answer = null;
                    return true;
                }
            }
        }
    }

    [Serializable]
    class SLet1CA0PA0CondComb1LCdrL : SimpleLet1CarA0PairA0Cond
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> predicate1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequent1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternative1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object altRatorName;
        public readonly int altRatorDepth;
        public readonly int altRatorOffset;
        public readonly object altRandName;
        public readonly int altRandDepth;
        public readonly int altRandOffset;

        protected SLet1CA0PA0CondComb1LCdrL (SimpleLambda rator, PrimitiveIsPairA0 predicate, Conditional consequent,
            SCode consPredicate, SCode consConsequent, Combination1LCdrL consAlternative,
            SCode alternative, PrimitiveCarA0 rand)
            : base (rator, predicate, consequent, alternative, rand)
        {
            this.altRatorName = consAlternative.ratorName;
            this.altRatorDepth = consAlternative.ratorDepth;
            this.altRatorOffset = consAlternative.ratorOffset;
            this.altRandName = consAlternative.rand0Name;
            this.altRandDepth = consAlternative.rand0Depth;
            this.altRandOffset = consAlternative.rand0Offset;
        }

        static public SCode Make (SimpleLambda rator, PrimitiveIsPairA0 predicate, Conditional consequent,
            SCode consPredicate, SCode consConsequent, Combination1LCdrL consAlternative,
            SCode alternative, PrimitiveCarA0 rand)
        {
            return
                (consPredicate is PrimitiveIsEqCarA0L) ? SComb1Fragment3.Make (rator, predicate, consequent, (PrimitiveIsEqCarA0L) consPredicate, consConsequent, consAlternative, alternative, rand) :
                new SLet1CA0PA0CondComb1LCdrL (rator, predicate, consequent, consPredicate, consConsequent, consAlternative, alternative, rand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("SLet1CA0PA0CondComb1LCdrL.EvalStep");
#endif
            Cons evarg = environment.Argument0Value as Cons;
            if (evarg == null) throw new NotImplementedException ();

            SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.rator, environment);
            environment = new SmallEnvironment1 (cl, evarg.Car);

            if (!(evarg.Car is Cons)) {
#if DEBUG
                noteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
            else {
#if DEBUG
                noteCalls (this.predicate1);
                predicate1TypeHistogram.Note (this.predicate1Type);
#endif
                object ev;
                Control unev = this.predicate1;
                Environment env = environment;

                while (unev.EvalStep (out ev, ref unev, ref env)) { };
                if (ev == Interpreter.UnwindStack) {
                    throw new NotImplementedException ();
                    //((UnwinderState) env).AddFrame (new ConditionalFrame (this, environment));
                    //environment = env;
                    //answer = Interpreter.UnwindStack;
                    //return false;
                }

                if ((ev is bool) && (bool) ev == false) {

                    object alt1Evrandtemp;
                    if (environment.FastLexicalRef (out alt1Evrandtemp, this.altRandName, this.altRandDepth, this.altRandOffset))
                        throw new NotImplementedException ();

                    object alt1Evrand = ((Cons) alt1Evrandtemp).Cdr;

                    object evop;
                    if (environment.FastLexicalRef (out evop, this.altRatorName, this.altRatorDepth, this.altRatorOffset))
                        throw new NotImplementedException ();
                    return Interpreter.Call (out answer, ref expression, ref environment, evop, alt1Evrand);
                    //#if DEBUG
                    //                    noteCalls (this.alternative1);
                    //                    alternative1TypeHistogram.Note (this.alternative1Type);
                    //                    Debug.WriteLineIf (Primitive.Noisy, "    => #f");
                    //#endif
                    //                    expression = this.alternative1;
                    //                    answer = null;
                    //                    return true;
                }
                else {
#if DEBUG
                    noteCalls (this.consequent1);
                    consequent1TypeHistogram.Note (this.consequent1Type);
#endif
                    expression = this.consequent1;
                    answer = null;
                    return true;
                }
            }
        }

    }

//    [Serializable]
//    class SComb1Fragment3 : SLet1CA0PA0CondComb1LCdrL
//    {
//#if DEBUG
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> predicate1TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> consequent1TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternative1TypeHistogram = new Histogram<Type> ();
//#endif
//        public readonly object pred1Rand1Name;
//        public readonly int pred1Rand1Depth;
//        public readonly int pred1Rand1Offset;

//        protected SComb1Fragment3 (SimpleLambda rator, PrimitiveIsPairA0 predicate, Conditional consequent,
//            PrimitiveIsEqCarA0L consPredicate, SCode consConsequent, Combination1LCdrL consAlternative,
//            SCode alternative, PrimitiveCarA0 rand)
//            : base (rator, predicate, consequent, consPredicate, consConsequent, consAlternative, alternative, rand)
//        {
//            this.pred1Rand1Name = consPredicate.rand1Name;
//            this.pred1Rand1Depth = consPredicate.rand1Depth;
//            this.pred1Rand1Offset = consPredicate.rand1Offset;

//        }

//        static public SCode Make (SimpleLambda rator, PrimitiveIsPairA0 predicate, Conditional consequent,
//            PrimitiveIsEqCarA0L consPredicate, SCode consConsequent, Combination1LCdrL consAlternative,
//            SCode alternative, PrimitiveCarA0 rand)
//        {
//            return
//                 new SComb1Fragment3 (rator, predicate, consequent, consPredicate, consConsequent, consAlternative, alternative, rand);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("SComb1Fragment3.EvalStep");
//#endif
//            Cons evarg = environment.Argument0Value as Cons;
//            if (evarg == null) throw new NotImplementedException ();

//            SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.rator, environment);
//            environment = new SmallEnvironment1 (cl, evarg.Car);

//            if (!(evarg.Car is Cons)) {
//#if DEBUG
//                noteCalls (this.alternative);
//                alternativeTypeHistogram.Note (this.alternativeType);
//#endif
//                expression = this.alternative;
//                answer = null;
//                return true;
//            }
//            else {
//                object ev;
//                object pred1Arg1;
//                if (environment.FastLexicalRef (out pred1Arg1, this.pred1Rand1Name, this.pred1Rand1Depth, this.pred1Rand1Offset))
//                    throw new NotImplementedException ();
//                object pred1Arg0 = ((Cons) evarg.Car).Car;

//                ObjectModel.Eq (out ev, pred1Arg0, pred1Arg1);
//                //Control unev = this.predicate1;
//                //Environment env = environment;

//                //while (unev.EvalStep (out ev, ref unev, ref env)) { };
//                //if (ev == Interpreter.UnwindStack) {
//                //    throw new NotImplementedException ();
//                //    //((UnwinderState) env).AddFrame (new ConditionalFrame (this, environment));
//                //    //environment = env;
//                //    //answer = Interpreter.UnwindStack;
//                //    //return false;
//                //}

//                if ((ev is bool) && (bool) ev == false) {

//                    object alt1Evrandtemp;
//                    if (environment.FastLexicalRef (out alt1Evrandtemp, this.altRandName, this.altRandDepth, this.altRandOffset))
//                        throw new NotImplementedException ();

//                    object alt1Evrand = ((Cons) alt1Evrandtemp).Cdr;

//                    object evop;
//                    if (environment.FastLexicalRef (out evop, this.altRatorName, this.altRatorDepth, this.altRatorOffset))
//                        throw new NotImplementedException ();
//                    return Interpreter.Call (out answer, ref expression, ref environment, evop, alt1Evrand);
//                    //#if DEBUG
//                    //                    noteCalls (this.alternative1);
//                    //                    alternative1TypeHistogram.Note (this.alternative1Type);
//                    //                    Debug.WriteLineIf (Primitive.Noisy, "    => #f");
//                    //#endif
//                    //                    expression = this.alternative1;
//                    //                    answer = null;
//                    //                    return true;
//                }
//                else {
//#if DEBUG
//                    noteCalls (this.consequent1);
//                    consequent1TypeHistogram.Note (this.consequent1Type);
//#endif
//                    expression = this.consequent1;
//                    answer = null;
//                    return true;
//                }
//            }
//        }

//    }

    [Serializable]
    class SimpleLet1CarA0Z2 : SimpleLet1CarA0
    {
        public readonly SCode first;
        public readonly SCode second;

        protected SimpleLet1CarA0Z2 (SimpleLambda rator, PrimitiveCarA0 rand)
            : base (rator, rand)
        {
            this.first = ((Sequence2) rator.Body).First;
            this.second = ((Sequence2) rator.Body).Second;
        }

        public static new SCode Make (SimpleLambda rator, PrimitiveCarA0 arg0)
        {
            return new SimpleLet1CarA0Z2 (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.first);
            noteCalls (this.second);
#endif
            Cons evarg = environment.Argument0Value as Cons;
            if (evarg == null) throw new NotImplementedException ();


            SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.rator, environment);
            environment = new SmallEnvironment1 (cl, evarg.Car);


            Control unev = this.first;
            Environment env = environment;
            while (unev.EvalStep (out answer, ref unev, ref env)) { };
            if (answer == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Sequence2Frame0 ((Sequence2) this.body, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            expression = this.second;
            return true;
        }

    }

    [Serializable]
    class SimpleLet1CarA1 : SimpleLet1CarA
    {
#if DEBUG
        static Histogram<Type> bodyTypeHistogram = new Histogram<Type> ();
#endif
        protected SimpleLet1CarA1 (SimpleLambda rator, PrimitiveCarA1 rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (SimpleLambda rator, PrimitiveCarA1 arg0)
        {
            return new SimpleLet1CarA1 (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("SimpleLet1CarA1.EvalStep");
            noteCalls (this.body);
            bodyTypeHistogram.Note (this.bodyType);
#endif

            Cons evarg = environment.Argument1Value as Cons;
            if (evarg == null) throw new NotImplementedException ();

            SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.rator, environment);

            expression = this.body;
            environment = new SmallEnvironment1 (cl, evarg.Car);
            answer = null;
            return true;
        }
    }

    [Serializable]
    class SimpleLet1CarL1 : SimpleLet1CarL
    {
#if DEBUG
        static Histogram<Type> bodyTypeHistogram = new Histogram<Type> ();
#endif
        protected SimpleLet1CarL1 (SimpleLambda rator, PrimitiveCarL1 rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (SimpleLambda rator, PrimitiveCarL1 arg0)
        {
            return new SimpleLet1CarL1 (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("SimpleLet1CarL1.EvalStep");
            noteCalls (this.body);
            bodyTypeHistogram.Note (this.bodyType);
#endif
            object temp;
            if (environment.FastLexicalRef1 (out temp, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
            Cons evarg = temp as Cons;
            if (evarg == null) throw new NotImplementedException ();

            SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.rator, environment);

            expression = this.body;
            environment = new SmallEnvironment1 (cl, evarg.Car);
            answer = null;
            return true;
        }
    }

    [Serializable]
    class SimpleLet1Cdr : SimpleLet1
    {
#if DEBUG
        static Histogram<Type> bodyTypeHistogram = new Histogram<Type> ();
#endif
        SCode cdrArg;

        protected SimpleLet1Cdr (SimpleLambda rator, PrimitiveCdr rand)
            : base (rator, rand)
        {
            this.cdrArg = rand.Operand;
        }

        public static SCode Make (SimpleLambda rator, PrimitiveCdr arg0)
        {
            return
                (arg0 is PrimitiveCdrL) ? SimpleLet1CdrL.Make (rator, (PrimitiveCdrL) arg0) :
                new SimpleLet1Cdr (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.body);
            bodyTypeHistogram.Note (this.bodyType);
            SCode.location = "SimpleLet1Cdr.EvalStep";
#endif
            object temp;
            Environment env = environment;
            Control expr = this.cdrArg;
            while (expr.EvalStep (out temp, ref expr, ref env)) { };
#if DEBUG
            SCode.location = "SimpleLet1Cdr.EvalStep.1";
#endif
            if (temp == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            Cons evarg = temp as Cons;
            if (evarg == null) throw new NotImplementedException ();

            SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.rator, environment);

            expression = this.body;
            environment = new SmallEnvironment1 (cl, evarg.Cdr);
            answer = null;
            return true;
        }
    }

    [Serializable]
    class SimpleLet1CdrL : SimpleLet1Cdr
    {
#if DEBUG
        static Histogram<Type> bodyTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand0Name;
        public readonly int rand0Depth;
        public readonly int rand0Offset;

        protected SimpleLet1CdrL (SimpleLambda rator, PrimitiveCdrL rand)
            : base (rator, rand)
        {
            this.rand0Name = rand.OperandName;
            this.rand0Depth = rand.OperandDepth;
            this.rand0Offset = rand.OperandOffset;
        }

        public static SCode Make (SimpleLambda rator, PrimitiveCdrL arg0)
        {
            return
                (arg0 is PrimitiveCdrA) ? SimpleLet1CdrA.Make (rator, (PrimitiveCdrA) arg0) :
                (arg0 is PrimitiveCdrL1) ? SimpleLet1CdrL1.Make (rator, (PrimitiveCdrL1) arg0) :
                new SimpleLet1CdrL (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("SimpleLet1CdrL.EvalStep");
            noteCalls (this.body);
            bodyTypeHistogram.Note (this.bodyType);
#endif
            object temp;
            if (environment.FastLexicalRef (out temp, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();
            Cons evarg = temp as Cons;
            if (evarg == null) throw new NotImplementedException ();

            SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.rator, environment);

            expression = this.body;
            environment = new SmallEnvironment1 (cl, evarg.Cdr);
            answer = null;
            return true;
        }
    }

    [Serializable]
    class SimpleLet1CdrA : SimpleLet1CdrL
    {
#if DEBUG
        static Histogram<Type> bodyTypeHistogram = new Histogram<Type> ();
#endif
        protected SimpleLet1CdrA (SimpleLambda rator, PrimitiveCdrA rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (SimpleLambda rator, PrimitiveCdrA arg0)
        {
            return
                (arg0 is PrimitiveCdrA0) ? SimpleLet1CdrA0.Make (rator, (PrimitiveCdrA0) arg0) :
                (arg0 is PrimitiveCdrA1) ? SimpleLet1CdrA1.Make (rator, (PrimitiveCdrA1) arg0) :
                new SimpleLet1CdrA (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.body);
            bodyTypeHistogram.Note (this.bodyType);
            SCode.location = "SimpleLet1CdrA.EvalStep";
#endif
            Cons evarg = environment.ArgumentValue (this.rand0Offset) as Cons;
            if (evarg == null) throw new NotImplementedException ();

            SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.rator, environment);

            expression = this.body;
            environment = new SmallEnvironment1 (cl, evarg.Cdr);
            answer = null;
            return true;
        }
    }

    [Serializable]
    class SimpleLet1CdrA0 : SimpleLet1CdrA
    {
#if DEBUG
        static Histogram<Type> bodyTypeHistogram = new Histogram<Type> ();
#endif
        protected SimpleLet1CdrA0 (SimpleLambda rator, PrimitiveCdrA0 rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (SimpleLambda rator, PrimitiveCdrA0 arg0)
        {
            return
                 new SimpleLet1CdrA0 (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("SimpleLet1CdrA0.EvalStep");
            noteCalls (this.body);
            bodyTypeHistogram.Note (this.bodyType);
#endif
            Cons evarg = environment.Argument0Value as Cons;
            if (evarg == null) throw new NotImplementedException ();

            SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.rator, environment);

            expression = this.body;
            environment = new SmallEnvironment1 (cl, evarg.Cdr);
            answer = null;
            return true;
        }
    }

    [Serializable]
    class SimpleLet1CdrA1 : SimpleLet1CdrA
    {
#if DEBUG
        static Histogram<Type> bodyTypeHistogram = new Histogram<Type> ();
#endif
        protected SimpleLet1CdrA1 (SimpleLambda rator, PrimitiveCdrA1 rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (SimpleLambda rator, PrimitiveCdrA1 arg0)
        {
            return new SimpleLet1CdrA1 (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("SimpleLet1CdrA1.EvalStep");
            noteCalls (this.body);
            bodyTypeHistogram.Note (this.bodyType);
#endif

            Cons evarg = environment.Argument1Value as Cons;
            if (evarg == null) throw new NotImplementedException ();

            SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.rator, environment);

            expression = this.body;
            environment = new SmallEnvironment1 (cl, evarg.Cdr);
            answer = null;
            return true;
        }
    }

    [Serializable]
    class SimpleLet1CdrL1 : SimpleLet1CdrL
    {
#if DEBUG
        static Histogram<Type> bodyTypeHistogram = new Histogram<Type> ();
#endif
        protected SimpleLet1CdrL1 (SimpleLambda rator, PrimitiveCdrL1 rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (SimpleLambda rator, PrimitiveCdrL1 arg0)
        {
            return new SimpleLet1CdrL1 (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.body);
            bodyTypeHistogram.Note (this.bodyType);
            SCode.location = "SimpleLet1CdrL1.EvalStep";
#endif
            object temp;
            if (environment.FastLexicalRef1 (out temp, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
            Cons evarg = temp as Cons;
            if (evarg == null) throw new NotImplementedException ();

            SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.rator, environment);

            expression = this.body;
            environment = new SmallEnvironment1 (cl, evarg.Cdr);
            answer = null;
            return true;
        }
    }

    [Serializable]
    class SimpleLet1StaticLambda : SimpleLet1
    {
#if DEBUG
        static Histogram<Type> bodyTypeHistogram = new Histogram<Type> ();
#endif
        public readonly StaticLambda argLambda;

        protected SimpleLet1StaticLambda (SimpleLambda rator, StaticLambda rand)
            : base (rator, rand)
        {
            this.argLambda = rand;
        }

        public static SCode Make (SimpleLambda rator, StaticLambda arg0)
        {
            return new SimpleLet1StaticLambda (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.body);
            bodyTypeHistogram.Note (this.bodyType);
            SCode.location = "SimpleLet1StaticLambda.EvalStep";
#endif
            expression = this.body;
            environment = new SmallEnvironment1 (new SimpleClosure ((SimpleLambda) this.rator, environment),
                                                 new StaticClosure (this.argLambda, environment));
            answer = null;
            return true;
        }
    }

    [Serializable]
    class SimpleLet1Q : SimpleLet1
    {
#if DEBUG
        static Histogram<Type> bodyTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object arg0Value;

        protected SimpleLet1Q (SimpleLambda rator, Quotation rand)
            : base (rator, rand)
        {
            this.arg0Value = rand.Quoted;
        }

        public static SCode Make (SimpleLambda rator, Quotation arg0)
        {
            return new SimpleLet1Q (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("SimpleLet1Q");
            noteCalls (this.body);
            bodyTypeHistogram.Note (this.bodyType);
#endif

            object evarg = this.arg0Value;

            SimpleClosure cl = new SimpleClosure ((SimpleLambda) this.rator, environment);

            expression = this.body;
            environment = new SmallEnvironment1 (cl, evarg);
            answer = null;
            return true;
        }

    }

    [Serializable]
    class Letrec1 : Let1
    {
#if DEBUG
        static Histogram<Type> ratorTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Lambda recursiveProc;

        Letrec1 (Lambda rator)
            : base (rator, Quotation.Unassigned)
        {
            this.recursiveProc = (Lambda) ((Assignment) ((Sequence2) rator.Body).First).Value;
        }

        public static SCode Make (Lambda rator)
        {
            return new Letrec1 (rator);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Letrec1.EvalStep");
#endif
            answer = new SmallEnvironment1 (((Lambda) this.rator).Close (environment), recursiveProc).Argument0Value;
            return false;
        }
    }
}

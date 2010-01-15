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
            //if (body is Disjunction) {
            //    HashSet<Symbol> freeInArms = new HashSet<Symbol> ();
            //    ((Disjunction) body).Alternative.CollectFreeVariables (freeInArms);
            //    if (!freeInArms.Contains ((Symbol) formals [0])) {
            //        return
            //            Disjunction.Make (Combination1.Make (Lambda.Make (rator.Name, rator.Formals, ((Disjunction) body).Predicate), arg0),
            //                          ((Disjunction) body).Alternative);
            //    }
            //}
            if (body is Conditional) {
                HashSet<Symbol> freeInArms = new HashSet<Symbol> ();
                ((Conditional) body).Consequent.CollectFreeVariables (freeInArms);
                ((Conditional) body).Alternative.CollectFreeVariables (freeInArms);
                if (!freeInArms.Contains ((Symbol) formals [0])) {
                    return
                    Conditional.Make (Combination1.Make (Lambda.Make (rator.Name, rator.Formals, ((Conditional) body).Predicate), arg0),
                                      ((Conditional) body).Consequent,
                                      ((Conditional) body).Alternative);
                }
            }
            //if (body is Sequence2) {
            //    HashSet<Symbol> freeInArms = new HashSet<Symbol> ();
            //    ((Sequence2) body).Second.CollectFreeVariables (freeInArms);
            //    if (!freeInArms.Contains ((Symbol) formals [0])) {
            //       // Debugger.Break ();
            //        return
            //            Sequence2.Make (Combination1.Make (Lambda.Make (rator.Name, rator.Formals, ((Sequence2) body).First), arg0),
            //                            ((Sequence2) body).Second);
            //    }
            //}

            return
                //(body is Variable) ? ((((Variable) body).Name == formals [0]) ? RewriteAsIdentity (arg0) : RewriteAsSequence(arg0, body)) :
                (rator is SimpleLambda) ? SimpleLet1.Make ((SimpleLambda) rator, arg0) :
                (rator is StaticLambda) ? StaticLet1.Make ((StaticLambda) rator, arg0) :
                (arg0 is Argument) ? Let1A.Make (rator, (Argument) arg0) :
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
            Warm ("-");
            NoteCalls (this.rator);
            NoteCalls (this.rand);
            ratorTypeHistogram.Note (this.ratorType);
            randTypeHistogram.Note (this.randType);
            SCode.location = "Let1";
#endif

            object evarg = null;
            Control unev = this.rand;
            Environment env = environment;
            while (unev.EvalStep (out evarg, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "Let1";
#endif
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
#if DEBUG
            SCode.location = "Let1";
#endif
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination1Frame1 (this, environment, evarg));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            return ((IApplicable) evop).Call (out answer, ref expression, ref environment, evarg);
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
    class Let1A : Let1
    {
        public readonly int randOffset;

        protected Let1A (Lambda rator, Argument rand)
            : base (rator, rand)
        {
            this.randOffset = rand.Offset;
        }

        public static SCode Make (Lambda rator, Argument arg0)
        {
            return
                (arg0 is Argument0) ? Let1A0.Make (rator, (Argument0) arg0) :
                 new Let1A (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);

            SCode.location = "Let1A";
#endif
            object ev0 = environment.ArgumentValue (this.randOffset);

            object evop = null;
            Control unevop = this.rator;
            Environment env = environment;
            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
#if DEBUG
            SCode.location = "Let1A";
#endif
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination1Frame1 (this, environment, evarg));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            return ((IApplicable) evop).Call (out answer, ref expression, ref environment, ev0);
        }
    }

    [Serializable]
    class Let1A0 : Let1A
    {
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
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);

            SCode.location = "Let1A0";
#endif
            object ev0 = environment.Argument0Value;

            object evop = null;
            Control unevop = this.rator;
            Environment env = environment;
            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
#if DEBUG
            SCode.location = "Let1A0";
#endif
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination1Frame1 (this, environment, evarg));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            return ((IApplicable) evop).Call (out answer, ref expression, ref environment, ev0);
        }
    }

    [Serializable]
    class Let1Q : Let1
    {
        public readonly object randValue;

        protected Let1Q (Lambda rator, Quotation rand)
            : base (rator, rand)
        {
            this.randValue = rand.Quoted;
        }

        static SCode StandardMake (Lambda rator, Quotation arg0)
        {
            return
                new Let1Q (rator, arg0);
        }

        public static SCode Make (Lambda rator, Quotation arg0)
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
            Warm ("-");
            NoteCalls (this.rator);

            SCode.location = "Let1Q";
#endif

            object evop = null;
            Control unevop = this.rator;
            Environment env = environment;
            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
#if DEBUG
            SCode.location = "Let1Q";
#endif
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination1Frame1 (this, environment, evarg));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            return ((IApplicable) evop).Call (out answer, ref expression, ref environment, this.randValue);
        }
    }

    [Serializable]
    class SimpleLet1 : Let1
    {
#if DEBUG
        static Histogram<Type> bodyTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> randTypeHistogram = new Histogram<Type> ();
        readonly Type bodyType;
#endif
        public readonly SimpleLambda lambda;

        protected SimpleLet1 (SimpleLambda rator, SCode rand)
            : base (rator, rand)
        {
            this.lambda = rator;
            //this.staticMapping = rator.GetStaticMapping ();
#if DEBUG
            this.bodyType = rator.Body.GetType();
#endif
        }

        public static SCode Make (SimpleLambda rator, SCode arg0)
        {
            return
                (arg0 is Argument) ? SimpleLet1A.Make (rator, (Argument) arg0) :
                (arg0 is SimpleLambda) ? SimpleLet1Lambda.Make (rator, (SimpleLambda) arg0) :
                (arg0 is StaticVariable) ? SimpleLet1S.Make (rator, (StaticVariable) arg0) :
                (arg0 is Quotation) ? SimpleLet1Q.Make (rator, (Quotation) arg0) :
                new SimpleLet1 (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.lambda.Body);
            NoteCalls (this.rand);
            randTypeHistogram.Note (this.randType);
            bodyTypeHistogram.Note (this.bodyType);
            SCode.location = "SimpleLet1";
#endif

            object evarg = null;
            Control unev = this.rand;
            Environment env = environment;
            while (unev.EvalStep (out evarg, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "SimpleLet1";
#endif
            if (evarg == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Let1Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            // this.closeCount += 1;
            // Use the base environment for lookup.
            object [] cells = environment.GetValueCells (this.lambda.StaticMapping);
#if DEBUG
            SCode.location = "SimpleLet1";
#endif
            SimpleClosure cl = new SimpleClosure (this.lambda, environment.BaseEnvironment, cells);


//            object evop = null;
//            Control unevop = this.rator;
//            env = environment;
//            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
//#if DEBUG
//            SCode.location = "SimpleLet1";
//#endif
//            if (evop == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new Combination1Frame1 (this, environment, evarg));
//                //answer = Interpreter.UnwindStack;
//                //environment = env;
//                //return false;
//            }
            expression = this.lambda.Body;
            environment = new SmallEnvironment1 (cl, evarg);
            answer = null; // keep the compiler happy
            return true;

           // return cl.Call (out answer, ref expression, ref environment, evarg);
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
            object evop = null;
            Control unevop = this.expression.rator;
            Environment env = environment;
            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
#if DEBUG
            SCode.location = "SimpleLet1";
#endif
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination1Frame1 (this, environment, evarg));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            return ((SimpleClosure) evop).Call (out answer, ref expression, ref environment, value);
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
    class SimpleLet1A : SimpleLet1
    {
#if DEBUG
        static Histogram<Type> bodyTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int randOffset;

        protected SimpleLet1A (SimpleLambda rator, Argument rand)
            : base (rator, rand)
        {
            this.randOffset = rand.Offset;

        }

        public static SCode Make (SimpleLambda rator, Argument arg0)
        {
            //if (arg0 is Argument0) Debugger.Break ();
            return
                (arg0 is Argument0) ? SimpleLet1A0.Make (rator, (Argument0) arg0) :
                new SimpleLet1A (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            bodyTypeHistogram.Note (this.bodyType);
            SCode.location = "SimpleLet1A";
#endif
            object evarg = environment.ArgumentValue (this.randOffset);

            // this.closeCount += 1;
            // Use the base environment for lookup.
            object [] cells = environment.GetValueCells (this.lambda.StaticMapping);
#if DEBUG
            SCode.location = "SimpleLet1A";
#endif
            SimpleClosure cl = new SimpleClosure (this.lambda, environment.BaseEnvironment, cells);


            //            object evop = null;
            //            Control unevop = this.rator;
            //            env = environment;
            //            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
            //#if DEBUG
            //            SCode.location = "SimpleLet1";
            //#endif
            //            if (evop == Interpreter.UnwindStack) {
            //                throw new NotImplementedException ();
            //                //((UnwinderState) env).AddFrame (new Combination1Frame1 (this, environment, evarg));
            //                //answer = Interpreter.UnwindStack;
            //                //environment = env;
            //                //return false;
            //            }
            expression = this.lambda.Body;
            environment = new SmallEnvironment1 (cl, evarg);
            answer = null; // keep the compiler happy
            return true;

            // return cl.Call (out answer, ref expression, ref environment, evarg);
        }

    }

    [Serializable]
    class SimpleLet1A0 : SimpleLet1A
    {
#if DEBUG
        static Histogram<Type> bodyTypeHistogram = new Histogram<Type> ();
#endif

        protected SimpleLet1A0 (SimpleLambda rator, Argument0 rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (SimpleLambda rator, Argument0 arg0)
        {
            //if (arg0 is Argument0) Debugger.Break ();
            return
                new SimpleLet1A0 (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            bodyTypeHistogram.Note (this.bodyType);
            SCode.location = "SimpleLet1A0";
#endif

            object evarg = environment.Argument0Value;

            // this.closeCount += 1;
            // Use the base environment for lookup.
            object [] cells = environment.GetValueCells (this.lambda.StaticMapping);
#if DEBUG
            SCode.location = "SimpleLet1A0";
#endif
            SimpleClosure cl = new SimpleClosure (this.lambda, environment.BaseEnvironment, cells);


            //            object evop = null;
            //            Control unevop = this.rator;
            //            env = environment;
            //            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
            //#if DEBUG
            //            SCode.location = "SimpleLet1";
            //#endif
            //            if (evop == Interpreter.UnwindStack) {
            //                throw new NotImplementedException ();
            //                //((UnwinderState) env).AddFrame (new Combination1Frame1 (this, environment, evarg));
            //                //answer = Interpreter.UnwindStack;
            //                //environment = env;
            //                //return false;
            //            }
            expression = this.lambda.Body;
            environment = new SmallEnvironment1 (cl, evarg);
            answer = null; // keep the compiler happy
            return true;

            // return cl.Call (out answer, ref expression, ref environment, evarg);
        }

    }

    [Serializable]
    class SimpleLet1Q : SimpleLet1
    {
#if DEBUG
        static Histogram<Type> bodyTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object randValue;

        protected SimpleLet1Q (SimpleLambda rator, Quotation rand)
            : base (rator, rand)
        {
            this.randValue = rand.Quoted;

        }

        public static SCode Make (SimpleLambda rator, Quotation arg0)
        {
            //if (arg0 is Argument0) Debugger.Break ();
            return
                new SimpleLet1Q (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            bodyTypeHistogram.Note (this.bodyType);
            SCode.location = "SimpleLet1Q";
#endif
            // this.closeCount += 1;
            // Use the base environment for lookup.
            object [] cells = environment.GetValueCells (this.lambda.StaticMapping);
#if DEBUG
            SCode.location = "SimpleLet1Q";
#endif
            SimpleClosure cl = new SimpleClosure (this.lambda, environment.BaseEnvironment, cells);


            //            object evop = null;
            //            Control unevop = this.rator;
            //            env = environment;
            //            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
            //#if DEBUG
            //            SCode.location = "SimpleLet1";
            //#endif
            //            if (evop == Interpreter.UnwindStack) {
            //                throw new NotImplementedException ();
            //                //((UnwinderState) env).AddFrame (new Combination1Frame1 (this, environment, evarg));
            //                //answer = Interpreter.UnwindStack;
            //                //environment = env;
            //                //return false;
            //            }
            expression = this.lambda.Body;
            environment = new SmallEnvironment1 (cl, this.randValue);
            answer = null; // keep the compiler happy
            return true;

            // return cl.Call (out answer, ref expression, ref environment, evarg);
        }

    }


    [Serializable]
    class SimpleLet1S : SimpleLet1
    {
#if DEBUG
        static Histogram<Type> bodyTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol randName;
        public readonly int randOffset;

        protected SimpleLet1S (SimpleLambda rator, StaticVariable rand)
            : base (rator, rand)
        {
            this.randName = rand.Name;
            this.randOffset = rand.Offset;

        }

        public static SCode Make (SimpleLambda rator, StaticVariable arg0)
        {
            //if (arg0 is Argument0) Debugger.Break ();
            return
                new SimpleLet1S (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rator);
            bodyTypeHistogram.Note (this.bodyType);
            SCode.location = "SimpleLet1S";
#endif
            object evarg;
            if (environment.StaticValue (out evarg, this.randName, this.randOffset))
                throw new NotImplementedException ();

            // this.closeCount += 1;
            // Use the base environment for lookup.
            object [] cells = environment.GetValueCells (this.lambda.StaticMapping);
#if DEBUG
            SCode.location = "SimpleLet1A";
#endif
            SimpleClosure cl = new SimpleClosure (this.lambda, environment.BaseEnvironment, cells);


            //            object evop = null;
            //            Control unevop = this.rator;
            //            env = environment;
            //            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
            //#if DEBUG
            //            SCode.location = "SimpleLet1";
            //#endif
            //            if (evop == Interpreter.UnwindStack) {
            //                throw new NotImplementedException ();
            //                //((UnwinderState) env).AddFrame (new Combination1Frame1 (this, environment, evarg));
            //                //answer = Interpreter.UnwindStack;
            //                //environment = env;
            //                //return false;
            //            }
            expression = this.lambda.Body;
            environment = new SmallEnvironment1 (cl, evarg);
            answer = null; // keep the compiler happy
            return true;

            // return cl.Call (out answer, ref expression, ref environment, evarg);
        }

    }

    [Serializable]
    class SimpleLet1Lambda : SimpleLet1
    {
#if DEBUG
        static Histogram<Type> bodyTypeHistogram = new Histogram<Type> ();
#endif
        public readonly SimpleLambda randLambda;

        protected SimpleLet1Lambda (SimpleLambda rator, SimpleLambda rand)
            : base (rator, rand)
        {
            this.randLambda = rand;
        }

        public static SCode Make (SimpleLambda rator, SimpleLambda arg0)
        {
            return
                new SimpleLet1Lambda (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.lambda.Body);
            bodyTypeHistogram.Note (this.bodyType);
            SCode.location = "SimpleLet1Lambda";
#endif

            this.randLambda.closeCount += 1;
            object [] randCells = environment.GetValueCells (this.randLambda.StaticMapping);
#if DEBUG
            SCode.location = "SimpleLet1Lambda";
#endif
            // Use the base environment for lookup.
            object evarg = new SimpleClosure (this.randLambda, environment.BaseEnvironment, randCells);
            object [] cells;

            if (this.randLambda.StaticMapping.OffsetCode == this.lambda.StaticMapping.OffsetCode
                && this.randLambda.StaticMapping.OffsetCode != -1) {
                cells = randCells;
            }
            else {
                cells = environment.GetValueCells (this.lambda.StaticMapping);
            }

            // this.closeCount += 1;
            // Use the base environment for lookup.
#if DEBUG
            SCode.location = "SimpleLet1Lambda";
#endif
            SimpleClosure cl = new SimpleClosure (this.lambda, environment.BaseEnvironment, cells);


            //            object evop = null;
            //            Control unevop = this.rator;
            //            env = environment;
            //            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
            //#if DEBUG
            //            SCode.location = "SimpleLet1";
            //#endif
            //            if (evop == Interpreter.UnwindStack) {
            //                throw new NotImplementedException ();
            //                //((UnwinderState) env).AddFrame (new Combination1Frame1 (this, environment, evarg));
            //                //answer = Interpreter.UnwindStack;
            //                //environment = env;
            //                //return false;
            //            }
            expression = this.lambda.Body;
            environment = new SmallEnvironment1 (cl, evarg);
            answer = null; // keep the compiler happy
            return true;

            // return cl.Call (out answer, ref expression, ref environment, evarg);
        }

    }

    [Serializable]
    class StaticLet1 : Let1
    {
#if DEBUG
        static Histogram<Type> bodyTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> randTypeHistogram = new Histogram<Type> ();
#endif
        public readonly StaticLambda lambda;
        public StaticMapping staticMapping;
        protected StaticLet1 (StaticLambda rator, SCode rand)
            : base (rator, rand)
        {
            this.lambda = rator;
        }

        public static SCode Make (StaticLambda rator, SCode arg0)
        {
            return
                //(arg0 is PrimitiveCar) ? SimpleLet1Car.Make (rator, (PrimitiveCar) arg0) :
                //(arg0 is PrimitiveCdr) ? SimpleLet1Cdr.Make (rator, (PrimitiveCdr) arg0) :
                //(arg0 is StaticLambda) ? SimpleLet1StaticLambda.Make (rator, (StaticLambda) arg0) :
                (arg0 is Argument) ? StaticLet1A.Make (rator, (Argument) arg0) :
                (arg0 is Quotation) ? StaticLet1Q.Make (rator, (Quotation) arg0) :
                new StaticLet1 (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.body);
            bodyTypeHistogram.Note (this.bodyType);
            NoteCalls (this.rand);
            randTypeHistogram.Note (this.randType);
            SCode.location = "StaticLet1";
#endif
            object evarg;
            Control unev = this.rand;
            Environment env = environment;
            while (unev.EvalStep (out evarg, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "StaticLet1";
#endif
            if (evarg == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new StaticLet1Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object [] cells = environment.GetValueCells (this.lambda.StaticMapping);
#if DEBUG
            SCode.location = "StaticLet1";
#endif
            // StaticClosure cl = new StaticClosure ((StaticLambda) this.rator, environment);
            StaticClosure cl = new StaticClosure (this.lambda, environment.BaseEnvironment, cells);

            expression = this.body;
            environment = new StaticEnvironment (cl, new object [] {evarg});
            answer = null;
            return true;
        }
    }

    [Serializable]
    sealed class StaticLet1Frame0 : SubproblemContinuation<StaticLet1>, ISystemVector
    {
        public StaticLet1Frame0 (StaticLet1 combination1, Environment environment)
            : base (combination1, environment)
        {
        }

        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object value)
        {
            StaticClosure cl = new StaticClosure (this.expression.lambda, environment.BaseEnvironment, environment.GetValueCells (this.expression.staticMapping));

            expression = this.expression.body;
            environment = new StaticEnvironment (cl, new object [] {value});
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
    class StaticLet1A : StaticLet1
    {
#if DEBUG
        static Histogram<Type> bodyTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand0Offset;

        protected StaticLet1A (StaticLambda rator, Argument rand)
            : base (rator, rand)
        {
            this.rand0Offset = rand.Offset;
        }

        public static SCode Make (StaticLambda rator, Argument arg0)
        {
            return
                (arg0 is Argument0) ? StaticLet1A0.Make (rator, (Argument0) arg0) :
                new StaticLet1A (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.body);
            bodyTypeHistogram.Note (this.bodyType);
            SCode.location = "StaticLet1A";
#endif
            object ev0 = environment.ArgumentValue (this.rand0Offset);
            // StaticClosure cl = new StaticClosure ((StaticLambda) this.rator, environment);
            object [] valueCells = environment.GetValueCells (this.lambda.StaticMapping);
#if DEBUG
            SCode.location = "StaticLet1A";
#endif
            StaticClosure cl = new StaticClosure (this.lambda, environment.BaseEnvironment, valueCells);

            expression = this.body;
            environment = new StaticEnvironment (cl, new object [] { ev0 });
            answer = null;
            return true;
        }
    }

    [Serializable]
    class StaticLet1A0 : StaticLet1A
    {
#if DEBUG
        static Histogram<Type> bodyTypeHistogram = new Histogram<Type> ();
#endif

        protected StaticLet1A0 (StaticLambda rator, Argument0 rand)
            : base (rator, rand)
        {
        }

        public static SCode Make (StaticLambda rator, Argument0 arg0)
        {
            return
                new StaticLet1A0 (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.body);
            bodyTypeHistogram.Note (this.bodyType);
            SCode.location = "StaticLet1A0";
#endif
            object ev0 = environment.Argument0Value;
            // StaticClosure cl = new StaticClosure ((StaticLambda) this.rator, environment);
            object [] valueCells = environment.GetValueCells (this.lambda.StaticMapping);
#if DEBUG
            SCode.location = "StaticLet1A0";
#endif
            StaticClosure cl = new StaticClosure (this.lambda, environment.BaseEnvironment, valueCells);

            expression = this.body;
            environment = new StaticEnvironment (cl, new object [] { ev0 });
            answer = null;
            return true;
        }
    }


    [Serializable]
    class StaticLet1Q : StaticLet1
    {
#if DEBUG
        static Histogram<Type> bodyTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand0Value;

        protected StaticLet1Q (StaticLambda rator, Quotation rand)
            : base (rator, rand)
        {
            this.rand0Value = rand.Quoted;
        }

        public static SCode Make (StaticLambda rator, Quotation arg0)
        {
            return
                //(arg0 is PrimitiveCar) ? SimpleLet1Car.Make (rator, (PrimitiveCar) arg0) :
                //(arg0 is PrimitiveCdr) ? SimpleLet1Cdr.Make (rator, (PrimitiveCdr) arg0) :
                //(arg0 is StaticLambda) ? SimpleLet1StaticLambda.Make (rator, (StaticLambda) arg0) :
                //(arg0 is Quotation) ? StaticLet1Q.Make (rator, (Quotation) arg0) :
                (arg0.Quoted == ReferenceTrap.Unassigned &&
                rator.Body is Letrec1Body) ? Letrec1.Make (rator, arg0) :
                new StaticLet1Q (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.body);
            bodyTypeHistogram.Note (this.bodyType);
            SCode.location = "StaticLet1Q";
#endif
            // StaticClosure cl = new StaticClosure ((StaticLambda) this.rator, environment);
            object [] valueCells = environment.GetValueCells (this.lambda.StaticMapping);
#if DEBUG
            SCode.location = "StaticLet1Q";
#endif
            StaticClosure cl = new StaticClosure (this.lambda, environment.BaseEnvironment, valueCells);

            expression = this.body;
            environment = new StaticEnvironment (cl, new object [] { this.rand0Value });
            answer = null;
            return true;
        }
    }

    [Serializable]
    class Letrec1 : StaticLet1Q
    {
        public readonly SimpleLambda innerLambda;
        protected Letrec1 (StaticLambda rator, Quotation rand)
            : base (rator, rand)
        {
            this.innerLambda = ((Letrec1Body) rator.Body).lambda;
        }

        public static SCode Make (StaticLambda rator, Quotation arg0)
        {
            return
                new Letrec1 (rator, arg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "Letrec1";
#endif
            object [] outerStaticCells = environment.StaticCells;
            object [] innerStaticCells = new object [outerStaticCells.Length + 1];
            for (int i = 0; i < outerStaticCells.Length; i++) {
                innerStaticCells [i + 1] = outerStaticCells [i];
            }
            SimpleClosure rec = new SimpleClosure (this.innerLambda, environment.BaseEnvironment, innerStaticCells);
            innerStaticCells [0] = rec;
            answer = rec;
            return false;


//            object [] valueCells = environment.GetValueCells (this.lambda.StaticMapping);
//            object [] foo = environment.StaticCells;
//            if (foo != valueCells) throw new NotImplementedException ();
//#if DEBUG
//            SCode.location = "Letrec1";
//#endif
//            StaticClosure cl = new StaticClosure (this.lambda, environment.BaseEnvironment, valueCells);
//            Environment closed = new StaticEnvironment (cl, new object [] { this.rand0Value });

//            this.innerLambda.closeCount += 1;
//            object [] staticCells = closed.GetValueCells (this.innerLambda.StaticMapping);
//#if DEBUG
//            SCode.location = "Letrec1";
//#endif
//            // Use the base environment for lookup.
//            SimpleClosure newValue = new SimpleClosure (this.innerLambda, closed.BaseEnvironment, staticCells);

//            if (closed.AssignArgument0 (out answer, newValue)) throw new NotImplementedException ();

//            answer = newValue;
//            return false;
        }
    }

}

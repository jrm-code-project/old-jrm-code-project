using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;

namespace Microcode
{
    [Serializable]
    class POr2 : Disjunction
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();

        protected Type rand0Type;
        protected Type rand1Type;
#endif
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public readonly Primitive2 procedure;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        [NonSerialized]
        protected PrimitiveMethod2 method;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public readonly SCode rand0;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public readonly SCode rand1;

        protected POr2 (PrimitiveCombination2 predicate, SCode alternative)
            : base (predicate, alternative)
        {
            this.procedure = predicate.Rator;
            this.method = this.procedure.Method;
            this.rand0 = predicate.Operand0;
            this.rand1 = predicate.Operand1;
#if DEBUG
            rand0Type = rand0.GetType ();
            rand1Type = rand1.GetType ();
#endif
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode alternative)
        {
            return
                (predicate is PrimitiveIsEq) ? POrIsEq.Make ((PrimitiveIsEq) predicate, alternative) :
                //(predicate is PrimitiveIsObjectType) ? POrObjectType.Make ((PrimitiveIsObjectType) predicate, alternative) :
                (predicate.Operand0 is Argument) ? POr2A.Make (predicate, alternative) :
                (predicate.Operand0 is Quotation) ? POr2Q.Make (predicate, alternative) :
                (predicate.Operand0 is StaticVariable) ? POr2S.Make (predicate, alternative) :
                (predicate.Operand1 is Argument) ? POr2XA.Make (predicate, alternative) :
                (predicate.Operand1 is Quotation) ? POr2XQ.Make (predicate, alternative) :
                (predicate.Operand1 is StaticVariable) ? POr2XS.Make (predicate, alternative) :
                new POr2 (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "POr2.EvalStep";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "POr2.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            unev = this.rand0;
            env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "POr2.EvalStep";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            SCode.location = this.procedure.Name.ToString ();
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }
#if DEBUG
            SCode.location = "POr2.EvalStep";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POr2.EvalStep";
#endif
                expression = this.alternative;
                return true;
            }
            else
                return false;
        }
    }

    [Serializable]
    class POr2A : POr2
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand0Offset;

        protected POr2A (PrimitiveCombination2 predicate, SCode alternative)
            : base (predicate, alternative)
        {
            this.rand0Offset = ((Argument) predicate.Operand0).Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode alternative)
        {
            return
                (predicate.Operand0 is Argument0) ? POr2A0.Make (predicate, alternative) :
                (predicate.Operand1 is Argument) ? POr2AA.Make (predicate, alternative) :
                (predicate.Operand1 is Quotation) ? POr2AQ.Make (predicate, alternative) :
                (predicate.Operand1 is StaticVariable) ? POr2AS.Make (predicate, alternative) :
                new POr2A (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "POr2A.EvalStep";
#endif

            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "POr2A.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }
            object ev0 = environment.ArgumentValue (this.rand0Offset);

#if DEBUG
            SCode.location = this.procedure.Name.ToString ();
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }
#if DEBUG
            SCode.location = "POr2A.EvalStep";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POr2A.EvalStep";
#endif
                expression = this.alternative;
                return true;
            }
            else
                return false;
        }
    }

    [Serializable]
    class POr2A0 : POr2A
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected POr2A0 (PrimitiveCombination2 predicate, SCode alternative)
            : base (predicate, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode alternative)
        {
            return
                (predicate.Operand1 is Argument) ? POr2A0A.Make (predicate, alternative) :
                (predicate.Operand1 is Quotation) ? POr2A0Q.Make (predicate, alternative) :
                (predicate.Operand1 is StaticVariable) ? POr2A0S.Make (predicate, alternative) :
                new POr2A0 (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "POr2A0.EvalStep";
#endif

            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "POr2A0.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            object ev0 = environment.Argument0Value;

#if DEBUG
            SCode.location = this.procedure.Name.ToString ();
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }
#if DEBUG
            SCode.location = "POr2A0.EvalStep";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POr2A0.EvalStep";
#endif
                expression = this.alternative;
                return true;
            }
            else
                return false;
        }
    }

    [Serializable]
    class POr2A0A : POr2A0
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Offset;
        protected POr2A0A (PrimitiveCombination2 predicate, SCode alternative)
            : base (predicate, alternative)
        {
            this.rand1Offset = ((Argument) predicate.Operand1).Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode alternative)
        {
            return
                (predicate.Operand1 is Argument0) ? POr2A0A0.Make (predicate, alternative) :
                new POr2A0A (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "POr2A0A.EvalStep";
#endif

            object ev1 = environment.ArgumentValue (this.rand1Offset);
            object ev0 = environment.Argument0Value;

#if DEBUG
            SCode.location = this.procedure.Name.ToString ();
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }
#if DEBUG
            SCode.location = "POr2A1.EvalStep";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POr2A1.EvalStep";
#endif
                expression = this.alternative;
                return true;
            }
            else
                return false;
        }
    }

    [Serializable]
    class POr2A0A0 : POr2A0A
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected POr2A0A0 (PrimitiveCombination2 predicate, SCode alternative)
            : base (predicate, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode alternative)
        {
            return
                new POr2A0A0 (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "POr2A0A0.EvalStep";
#endif

            object ev0 = environment.Argument0Value;

#if DEBUG
            SCode.location = this.procedure.Name.ToString ();
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev0)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }
#if DEBUG
            SCode.location = "POr2A0A0.EvalStep";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POr2A0A0.EvalStep";
#endif
                expression = this.alternative;
                return true;
            }
            else
                return false;
        }
    }

    [Serializable]
    class POr2A0Q : POr2A0
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Value;

        protected POr2A0Q (PrimitiveCombination2 predicate, SCode alternative)
            : base (predicate, alternative)
        {
            this.rand1Value = ((Quotation) predicate.Operand1).Quoted;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode alternative)
        {
            return
                new POr2A0Q (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "POr2A0Q.EvalStep";
#endif

            object ev1 = this.rand1Value;
            object ev0 = environment.Argument0Value;

#if DEBUG
            SCode.location = this.procedure.Name.ToString ();
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }
#if DEBUG
            SCode.location = "POr2A0Q.EvalStep";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POr2A0Q.EvalStep";
#endif
                expression = this.alternative;
                return true;
            }
            else
                return false;
        }
    }

    [Serializable]
    class POr2A0S : POr2A0
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        protected POr2A0S (PrimitiveCombination2 predicate, SCode alternative)
            : base (predicate, alternative)
        {
            this.rand1Name = ((StaticVariable) predicate.Operand1).Name;
            this.rand1Offset = ((StaticVariable) predicate.Operand1).Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode alternative)
        {
            return
                new POr2A0S (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "POr2A0S.EvalStep";
#endif

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.Argument0Value;

#if DEBUG
            SCode.location = this.procedure.Name.ToString ();
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }
#if DEBUG
            SCode.location = "POr2A0S.EvalStep";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POr2A0S.EvalStep";
#endif
                expression = this.alternative;
                return true;
            }
            else
                return false;
        }
    }

    [Serializable]
    class POr2AA : POr2A
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Offset;
        protected POr2AA (PrimitiveCombination2 predicate, SCode alternative)
            : base (predicate, alternative)
        {
            this.rand1Offset = ((Argument) predicate.Operand1).Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode alternative)
        {
            return
                (predicate.Operand1 is Argument0) ? POr2AA0.Make (predicate, alternative) :
                new POr2AA (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "POr2AA.EvalStep";
#endif

            object ev1 = environment.ArgumentValue (this.rand1Offset);
            object ev0 = environment.ArgumentValue (this.rand0Offset);

#if DEBUG
            SCode.location = this.procedure.Name.ToString ();
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }
#if DEBUG
            SCode.location = "POr2AA.EvalStep";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POr2AA.EvalStep";
#endif
                expression = this.alternative;
                return true;
            }
            else
                return false;
        }
    }

    [Serializable]
    class POr2AA0 : POr2AA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected POr2AA0 (PrimitiveCombination2 predicate, SCode alternative)
            : base (predicate, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode alternative)
        {
            return
                new POr2AA0 (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "POr2AA0.EvalStep";
#endif
            object ev1 = environment.Argument0Value;
            object ev0 = environment.ArgumentValue (this.rand1Offset);

#if DEBUG
            SCode.location = this.procedure.Name.ToString ();
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev0)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }
#if DEBUG
            SCode.location = "POr2AA0.EvalStep";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POr2AA0.EvalStep";
#endif
                expression = this.alternative;
                return true;
            }
            else
                return false;
        }
    }

    [Serializable]
    class POr2AQ : POr2A
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Value;

        protected POr2AQ (PrimitiveCombination2 predicate, SCode alternative)
            : base (predicate, alternative)
        {
            this.rand1Value = ((Quotation) predicate.Operand1).Quoted;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode alternative)
        {
            return
                new POr2AQ (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "POr2AQ.EvalStep";
#endif

            object ev1 = this.rand1Value;
            object ev0 = environment.ArgumentValue (this.rand0Offset);

#if DEBUG
            SCode.location = this.procedure.Name.ToString ();
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }
#if DEBUG
            SCode.location = "POr2AQ.EvalStep";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POr2AQ.EvalStep";
#endif
                expression = this.alternative;
                return true;
            }
            else
                return false;
        }
    }

    [Serializable]
    class POr2AS : POr2A
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        protected POr2AS (PrimitiveCombination2 predicate, SCode alternative)
            : base (predicate, alternative)
        {
            this.rand1Name = ((StaticVariable) predicate.Operand1).Name;
            this.rand1Offset = ((StaticVariable) predicate.Operand1).Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode alternative)
        {
            return
                new POr2AS (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "POr2AS.EvalStep";
#endif

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = environment.ArgumentValue (this.rand0Offset);

#if DEBUG
            SCode.location = this.procedure.Name.ToString ();
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }
#if DEBUG
            SCode.location = "POr2AS.EvalStep";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POr2AS.EvalStep";
#endif
                expression = this.alternative;
                return true;
            }
            else
                return false;
        }
    }

    [Serializable]
    class POr2SA : POr2S
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Offset;
        protected POr2SA (PrimitiveCombination2 predicate, SCode alternative)
            : base (predicate, alternative)
        {
            this.rand1Offset = ((Argument) predicate.Operand1).Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode alternative)
        {
            return
                (predicate.Operand1 is Argument0) ? POr2SA0.Make (predicate, alternative) :
                new POr2SA (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "POr2SA.EvalStep";
#endif

            object ev1 = environment.ArgumentValue (this.rand1Offset);
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

#if DEBUG
            SCode.location = this.procedure.Name.ToString ();
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }
#if DEBUG
            SCode.location = "POr2SA.EvalStep";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POr2SA.EvalStep";
#endif
                expression = this.alternative;
                return true;
            }
            else
                return false;
        }
    }

    [Serializable]
    class POr2SA0 : POr2SA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected POr2SA0 (PrimitiveCombination2 predicate, SCode alternative)
            : base (predicate, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode alternative)
        {
            return
                new POr2SA0 (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "POr2SA0.EvalStep";
#endif
            object ev1 = environment.Argument0Value;
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

#if DEBUG
            SCode.location = this.procedure.Name.ToString ();
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev0)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }
#if DEBUG
            SCode.location = "POr2SA0.EvalStep";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POr2SA0.EvalStep";
#endif
                expression = this.alternative;
                return true;
            }
            else
                return false;
        }
    }

    [Serializable]
    class POr2SQ : POr2S
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Value;

        protected POr2SQ (PrimitiveCombination2 predicate, SCode alternative)
            : base (predicate, alternative)
        {
            this.rand1Value = ((Quotation) predicate.Operand1).Quoted;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode alternative)
        {
            return
                new POr2SQ (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "POr2SQ.EvalStep";
#endif

            object ev1 = this.rand1Value;
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

#if DEBUG
            SCode.location = this.procedure.Name.ToString ();
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }
#if DEBUG
            SCode.location = "POr2SQ.EvalStep";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POr2SQ.EvalStep";
#endif
                expression = this.alternative;
                return true;
            }
            else
                return false;
        }
    }

    [Serializable]
    class POr2SS : POr2S
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        protected POr2SS (PrimitiveCombination2 predicate, SCode alternative)
            : base (predicate, alternative)
        {
            this.rand1Name = ((StaticVariable) predicate.Operand1).Name;
            this.rand1Offset = ((StaticVariable) predicate.Operand1).Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode alternative)
        {
            return
                new POr2SS (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "POr2SS.EvalStep";
#endif

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

#if DEBUG
            SCode.location = this.procedure.Name.ToString ();
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }
#if DEBUG
            SCode.location = "POr2SS.EvalStep";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POr2SS.EvalStep";
#endif
                expression = this.alternative;
                return true;
            }
            else
                return false;
        }
    }

    [Serializable]
    class POr2Q : POr2
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand0Value;

        protected POr2Q (PrimitiveCombination2 predicate, SCode alternative)
            : base (predicate, alternative)
        {
            this.rand0Value = ((Quotation) predicate.Operand0).Quoted;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode alternative)
        {
            return
                (predicate.Operand1 is Argument) ? POr2QA.Make (predicate, alternative) :
                (predicate.Operand1 is Quotation) ? Unimplemented () :
                (predicate.Operand1 is StaticVariable) ? POr2QS.Make (predicate, alternative) :
                new POr2Q (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "POr2Q.EvalStep";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "POr2Q.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }
            object ev0 = this.rand0Value;
#if DEBUG
            SCode.location = this.procedure.Name.ToString ();
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }
#if DEBUG
            SCode.location = "POr2Q.EvalStep";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POr2Q.EvalStep";
#endif
                expression = this.alternative;
                return true;
            }
            else
                return false;
        }
    }

    [Serializable]
    class POr2QA : POr2Q
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Offset;

        protected POr2QA (PrimitiveCombination2 predicate, SCode alternative)
            : base (predicate, alternative)
        {
            this.rand1Offset = ((Argument) predicate.Operand1).Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode alternative)
        {
            return
                (predicate.Operand1 is Argument0) ? POr2QA0.Make (predicate, alternative) :
                new POr2QA (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "POr2QA.EvalStep";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);
            object ev0 = this.rand0Value;
#if DEBUG
            SCode.location = this.procedure.Name.ToString ();
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }
#if DEBUG
            SCode.location = "POr2QA.EvalStep";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POr2QA.EvalStep";
#endif
                expression = this.alternative;
                return true;
            }
            else
                return false;
        }
    }

    [Serializable]
    class POr2QA0 : POr2QA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected POr2QA0 (PrimitiveCombination2 predicate, SCode alternative)
            : base (predicate, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode alternative)
        {
            return
                new POr2QA0 (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "POr2QA0.EvalStep";
#endif
            object ev1 = environment.Argument0Value;
            object ev0 = this.rand0Value;
#if DEBUG
            SCode.location = this.procedure.Name.ToString ();
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }
#if DEBUG
            SCode.location = "POr2QA0.EvalStep";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POr2QA0.EvalStep";
#endif
                expression = this.alternative;
                return true;
            }
            else
                return false;
        }
    }

    [Serializable]
    class POr2QS : POr2Q
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        protected POr2QS (PrimitiveCombination2 predicate, SCode alternative)
            : base (predicate, alternative)
        {
            this.rand1Name = ((StaticVariable) predicate.Operand1).Name;
            this.rand1Offset = ((StaticVariable) predicate.Operand1).Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode alternative)
        {
            return
                new POr2QS (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            procedureHistogram.Note (this.procedure);
            SCode.location = "POr2QS.EvalStep";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();
            object ev0 = this.rand0Value;
#if DEBUG
            SCode.location = this.procedure.Name.ToString ();
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }
#if DEBUG
            SCode.location = "POr2QS.EvalStep";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POr2QS.EvalStep";
#endif
                expression = this.alternative;
                return true;
            }
            else
                return false;
        }
    }

    [Serializable]
    class POr2S : POr2
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand0Name;
        public readonly int rand0Offset;

        protected POr2S (PrimitiveCombination2 predicate, SCode alternative)
            : base (predicate, alternative)
        {
            this.rand0Name = ((StaticVariable) predicate.Operand0).Name;
            this.rand0Offset = ((StaticVariable) predicate.Operand0).Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode alternative)
        {
            return
                (predicate.Operand1 is Quotation) ? POr2SQ.Make (predicate, alternative) :
                new POr2S (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            procedureHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "POr2S.EvalStep";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "POr2S.EvalStep";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

#if DEBUG
            SCode.location = this.procedure.Name.ToString ();
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }
#if DEBUG
            SCode.location = "POr2S.EvalStep";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POr2S.EvalStep";
#endif
                expression = this.alternative;
                return true;
            }
            else
                return false;
        }
    }

    [Serializable]
    class POr2XA : POr2
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Offset;

        protected POr2XA (PrimitiveCombination2 predicate, SCode alternative)
            : base (predicate, alternative)
        {
            this.rand1Offset = ((Argument) predicate.Operand1).Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode alternative)
        {
            return
                (predicate.Operand1 is Argument0) ? POr2XA0.Make (predicate, alternative) :
                new POr2XA (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "POr2XA.EvalStep";
#endif
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "POrXA.EvalStep";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            SCode.location = this.procedure.Name.ToString ();
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }
#if DEBUG
            SCode.location = "POr2XA.EvalStep";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POr2XA.EvalStep";
#endif
                expression = this.alternative;
                return true;
            }
            else
                return false;
        }
    }

    [Serializable]
    class POr2XA0 : POr2XA
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected POr2XA0 (PrimitiveCombination2 predicate, SCode alternative)
            : base (predicate, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode alternative)
        {
            return
                new POr2XA0 (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "POr2XA0.EvalStep";
#endif
            object ev1 = environment.Argument0Value;

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "POrXA0.EvalStep";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            SCode.location = this.procedure.Name.ToString ();
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }
#if DEBUG
            SCode.location = "POr2XA0.EvalStep";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POr2XA0.EvalStep";
#endif
                expression = this.alternative;
                return true;
            }
            else
                return false;
        }
    }

    [Serializable]
    class POr2XQ : POr2
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Value;

        protected POr2XQ (PrimitiveCombination2 predicate, SCode alternative)
            : base (predicate, alternative)
        {
            this.rand1Value = ((Quotation) predicate.Operand1).Quoted;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode alternative)
        {
            return
                new POr2XQ (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "POr2XQ.EvalStep";
#endif
            object ev1 = this.rand1Value;

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "POrXQ.EvalStep";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            SCode.location = this.procedure.Name.ToString ();
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }
#if DEBUG
            SCode.location = "POr2XQ.EvalStep";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POr2XQ.EvalStep";
#endif
                expression = this.alternative;
                return true;
            }
            else
                return false;
        }
    }

    [Serializable]
    class POr2XS : POr2
    {
#if DEBUG
        static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        protected POr2XS (PrimitiveCombination2 predicate, SCode alternative)
            : base (predicate, alternative)
        {
            this.rand1Name = ((StaticVariable) predicate.Operand1).Name;
            this.rand1Offset = ((StaticVariable) predicate.Operand1).Offset;
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode alternative)
        {
            return
                new POr2XS (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            procedureHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "POr2XS.EvalStep";
#endif
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            Control unev = this.rand0;
            Environment env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "POrXS.EvalStep";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

#if DEBUG
            SCode.location = this.procedure.Name.ToString ();
            Primitive.hotPrimitives.Note (this.procedure);
#endif
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0, ev1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
                }
            }
#if DEBUG
            SCode.location = "POr2XS.EvalStep";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POr2XS.EvalStep";
#endif
                expression = this.alternative;
                return true;
            }
            else
                return false;
        }
    }

    [Serializable]
    class POrObjectType : POr2
    {

#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected POrObjectType (PrimitiveCombination2 predicate, SCode alternative)
            : base (predicate, alternative)
        {
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode alternative)
        {
            return
                (predicate.Operand0 is Quotation) ? POrObjectTypeQ.Make (predicate, alternative) :
                new POrObjectType (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            NoteCalls (this.rand1);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "POrObjectType";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "POrObjectType";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            unev = this.rand0;
            env = environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "POrObjectType";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            if (ObjectModel.IsObjectType (out answer, ev0, ev1))
                throw new NotImplementedException ();

#if DEBUG
            SCode.location = "POrObjectType";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POrObjectType";
#endif
                expression = this.alternative;
                return true;
            }
            else
                return false;
        }
    }

    [Serializable]
    class POrObjectTypeQ : POr2
    {

#if DEBUG
        static Histogram<TC> rand0ValueHistogram = new Histogram<TC> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly TC rand0Value;

        protected POrObjectTypeQ (PrimitiveCombination2 predicate, SCode alternative)
            : base (predicate, alternative)
        {
            rand0Value = (TC) ((Quotation) (predicate.Operand0)).Quoted;
        }

        static SCode MakeOr1 (Primitive1 primitive, SCode operand1, SCode alternative)
        {
            return POr1.Make (PrimitiveCombination1.Make (primitive, operand1), alternative);
        }

        public static SCode Make (PrimitiveCombination2 predicate, SCode alternative)
        {
            TC code = (TC) ((Quotation) (predicate.Operand0)).Quoted;
            SCode operand1 = predicate.Operand1;
            return
                (code == TC.BIG_FIXNUM) ? POr1.Make (PrimitiveCombination1.Make (Primitive.IsBigFixnum, operand1), alternative) :
                (code == TC.BIG_FLONUM) ? POr1.Make (PrimitiveCombination1.Make (Primitive.IsBigFlonum, operand1), alternative) :
                (code == TC.COMPLEX) ? POr1.Make (PrimitiveCombination1.Make (Primitive.IsComplex, operand1), alternative) :
                (code == TC.ENTITY) ? POr1.Make (PrimitiveCombination1.Make (Primitive.IsEntity, operand1), alternative) :
                //(code == TC.EXTENDED_PROCEDURE) ? POr1.Make (PrimitiveCombination1.Make (Primitive.IsExtendedProcedure, operand1), alternative) :
                (code == TC.FIXNUM) ? POr1.Make (PrimitiveCombination1.Make (Primitive.IsFixnum, operand1), alternative) :
                (code == TC.INTERNED_SYMBOL) ? MakeOr1 (Primitive.IsFixnum, operand1, alternative) :
                (code == TC.PRIMITIVE) ? MakeOr1 (Primitive.IsPrimitiveProcedure, operand1, alternative) :
                //(code == TC.PROCEDURE) ? MakeOr1 (Primitive.IsProcedure, operand1, alternative) :
                (code == TC.RATNUM) ? MakeOr1 (Primitive.IsRatnum, operand1, alternative) :
                (code == TC.UNINTERNED_SYMBOL) ? MakeOr1 (Primitive.IsUninternedSymbol, operand1, alternative) :
                (code == TC.VECTOR) ? MakeOr1 (Primitive.IsVector, operand1, alternative) :
                (code == TC.WEAK_CONS) ? MakeOr1 (Primitive.IsWeakCons, operand1, alternative) :
                (operand1 is Argument) ? POrObjectTypeQA.Make (predicate, (Argument) operand1, alternative) :
                new POrObjectTypeQ (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            rand0ValueHistogram.Note (this.rand0Value);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "POrObjectTypeQ";
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "POrObjectTypeQ";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            if (ObjectModel.IsObjectType (out answer, (int) this.rand0Value, ev1))
                throw new NotImplementedException ();

#if DEBUG
            SCode.location = "POrObjectTypeQ";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POrObjectTypeQ";
#endif
                expression = this.alternative;
                return true;
            }
            else
                return false;
        }
    }

    [Serializable]
    class POrObjectTypeQA : POrObjectTypeQ
    {

#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Offset;

        protected POrObjectTypeQA (PrimitiveCombination2 predicate, Argument rand1, SCode alternative)
            : base (predicate, alternative)
        {
            this.rand1Offset = rand1.Offset;
        }


        public static SCode Make (PrimitiveCombination2 predicate, Argument rand1, SCode alternative)
        {
            return
                new POrObjectTypeQA (predicate, rand1, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("POrObjectTypeQA");
#endif

            object ev1 = environment.ArgumentValue (this.rand1Offset);

            if (ObjectModel.IsObjectType (out answer, (int) this.rand0Value, ev1))
                throw new NotImplementedException ();

#if DEBUG
            SCode.location = "POrObjectTypeQA";
#endif

            if ((answer is bool) && (bool) answer == false) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "POrObjectTypeQA";
#endif
                expression = this.alternative;
                return true;
            }
            else
                return false;
        }
    }

}


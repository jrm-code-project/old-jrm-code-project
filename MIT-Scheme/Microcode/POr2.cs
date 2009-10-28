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
            static Histogram<Primitive2> procedureHistogram = new Histogram<Primitive2>();
            [NonSerialized]
            static Histogram<Type> rand0TypeHistogram = new Histogram<Type>();
            static Histogram<Type> rand1TypeHistogram = new Histogram<Type>();
            static Histogram<Type> alternativeTypeHistogram = new Histogram<Type>();

            protected Type rand0Type;
            protected Type rand1Type;
#endif
            [DebuggerBrowsable(DebuggerBrowsableState.Never)]
            public readonly Primitive2 procedure;

            [DebuggerBrowsable(DebuggerBrowsableState.Never)]
            [NonSerialized]
            protected PrimitiveMethod2 method;

            [DebuggerBrowsable(DebuggerBrowsableState.Never)]
            public readonly SCode rand0;

            [DebuggerBrowsable(DebuggerBrowsableState.Never)]
            public readonly SCode rand1;

            protected POr2(PrimitiveCombination2 predicate, SCode alternative)
                : base(predicate, alternative)
            {
                this.procedure = predicate.Rator;
                this.method = this.procedure.Method;
                this.rand0 = predicate.Operand0;
                this.rand1 = predicate.Operand1;
#if DEBUG
                rand0Type = rand0.GetType();
                rand1Type = rand1.GetType();
#endif
            }

            public static SCode Make(PrimitiveCombination2 predicate, SCode alternative)
            {
                return
                    new POr2(predicate, alternative);
            }

            public override bool EvalStep(out object answer, ref Control expression, ref Environment environment)
            {
#if DEBUG
                Warm("-");
                NoteCalls(this.rand0);
                NoteCalls(this.rand1);
                procedureHistogram.Note(this.procedure);
                rand0TypeHistogram.Note(this.rand0Type);
                rand1TypeHistogram.Note(this.rand1Type);
                SCode.location = "POr2.EvalStep";
#endif
                Control unev = this.rand1;
                Environment env = environment;
                object ev1;
                while (unev.EvalStep(out ev1, ref unev, ref env)) { };
                if (ev1 == Interpreter.UnwindStack)
                {
                    throw new NotImplementedException();
                }

                unev = this.rand0;
                env = environment;
                object ev0;
                while (unev.EvalStep(out ev0, ref unev, ref env)) { };
                if (ev0 == Interpreter.UnwindStack)
                {
                    throw new NotImplementedException();
                    //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                    //answer = Interpreter.UnwindStack;
                    //environment = env;
                    //return false;
                }

                // It is expensive to bounce down to invoke the procedure
                // we invoke it directly and pass along the ref args.
                if (this.method(out answer, ev0, ev1))
                {
                    TailCallInterpreter tci = answer as TailCallInterpreter;
                    if (tci != null)
                    {
                        answer = null; // dispose of the evidence
                        // set up the interpreter for a tail call
                        Control cExpression = tci.Expression;
                        Environment cEnvironment = tci.Environment;
                        while (cExpression.EvalStep(out answer, ref cExpression, ref cEnvironment)) { };
                    }
                }

                if ((answer is bool) && (bool)answer == false)
                {
#if DEBUG
                    NoteCalls(this.alternative);
                    alternativeTypeHistogram.Note(this.alternativeType);
#endif
                    expression = this.alternative;
                    return true;
                }
                else
                    return false;
            }
        }
    }


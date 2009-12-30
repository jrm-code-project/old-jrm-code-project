using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;

namespace Microcode
{
    [Serializable]
    class POr1 : Disjunction
    {
#if DEBUG
        static Histogram<Primitive1> procedureHistogram = new Histogram<Primitive1> ();
        static Histogram<Type> arg0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();

        protected readonly Type arg0Type;
#endif
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public readonly Primitive1 procedure;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        [NonSerialized]
        protected PrimitiveMethod1 method;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public readonly SCode arg0;

        protected POr1 (PrimitiveCombination1 predicate, SCode alternative)
            : base (predicate, alternative)
        {
            this.procedure = predicate.Operator;
            this.method = this.procedure.Method;
            this.arg0 = predicate.Operand;
#if DEBUG
            this.arg0Type = this.arg0.GetType ();
#endif
        }

        //static SCode SpecialMake (PrimitiveCarL predicate, SCode SCode alternative)
        //{
        //    return
        //         POr1L.Make ((PrimitiveCombination1L) PrimitiveCombination1L.Make (predicate.Operator, (LexicalVariable) predicate.Operand), alternative);
        //}

        //static SCode InvertConditional (PrimitiveNot predicate, SCode SCode alternative)
        //{
        //    //Debug.Write ("\n; InvertConditional");
        //    return Conditional.Make (predicate.Operand, alternative, consequent);
        //}

        public static SCode Make (PrimitiveCombination1 predicate, SCode alternative)
        {
            return
                //(Configuration.EnableInvertConditional && predicate is PrimitiveNot) ? InvertConditional ((PrimitiveNot) predicate, alternative) :
                //(predicate is PrimitiveCarL) ? SpecialMake ((PrimitiveCarL) predicate, alternative) :
                //(predicate is PrimitiveCdr) ? Unimplemented() :
                (predicate is PrimitiveIsSymbol) ? POrIsSymbol.Make ((PrimitiveIsSymbol) predicate, alternative) :
                (predicate is PrimitiveIsType<long>) ? POrIsType<long>.Make ((PrimitiveIsType<long>) predicate, alternative) :
                (predicate is PrimitiveIsType<double>) ? POrIsType<double>.Make ((PrimitiveIsType<double>) predicate, alternative) :
                (predicate is PrimitiveIsType<Complex>) ? POrIsType<Complex>.Make ((PrimitiveIsType<Complex>) predicate, alternative) :
                (predicate is PrimitiveIsType<char>) ? POrIsType<char>.Make ((PrimitiveIsType<char>) predicate, alternative) :
                (predicate is PrimitiveIsType<int>) ? POrIsType<int>.Make ((PrimitiveIsType<int>) predicate, alternative) :
                //(predicate is PrimitiveIsNegative) ? POrIsNegative.Make ((PrimitiveIsNegative) predicate, alternative) :
                //(predicate is PrimitiveIsNull) ? POrIsNull.Make ((PrimitiveIsNull) predicate, alternative) :
                (predicate is PrimitiveIsType<Cons>) ? POrIsType<Cons>.Make ((PrimitiveIsType<Cons>) predicate, alternative) :
                (predicate is PrimitiveIsType<Primitive>) ? POrIsType<Primitive>.Make ((PrimitiveIsType<Primitive>) predicate, alternative) :
                (predicate is PrimitiveIsType<Ratnum>) ? POrIsType<Ratnum>.Make ((PrimitiveIsType<Ratnum>) predicate, alternative) :
                (predicate is PrimitiveIsType<Record>) ? POrIsType<Record>.Make ((PrimitiveIsType<Record>) predicate, alternative) :
                (predicate is PrimitiveIsType<object []>) ? POrIsType<object []>.Make ((PrimitiveIsType<object[]>) predicate, alternative) :
                //(predicate is PrimitiveCombination1L) ? POr1L.Make ((PrimitiveCombination1L) predicate, alternative) :
                //(predicate is PrimitiveCombination1Q) ? Unimplemented () :
                //(consequent is LexicalVariable) ? POr1SL.Make (predicate, (LexicalVariable) alternative) :
                //(consequent is Quotation) ? POr1SQ.Make (predicate, (Quotation) alternative) :
                //(alternative is LexicalVariable) ? POr1SSL.Make (predicate, (LexicalVariable) alternative) :
                //(alternative is Quotation) ? POr1SSQ.Make (predicate, (Quotation) alternative) :
                new POr1 (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("POr1.EvalStep");
            NoteCalls (this.arg0);
            procedureHistogram.Note (this.procedure);
            arg0TypeHistogram.Note (this.arg0Type);
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, closureEnvironment));
                //answer = Interpreter.Unwind;
                //closureEnvironment = env;
                //return false;
            }

            //Console.WriteLine ("Procedure: {2} Method: {0}, Arg: {1}", this.method.Method.Name, ev0, this.procedure);
            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
            if (this.method (out answer, ev0)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null; // dispose of the evidence
                    // set up the interpreter for a tail call
                    Control cExpression = tci.Expression;
                    Environment cEnvironment = tci.Environment;
                    while (cExpression.EvalStep (out answer, ref cExpression, ref cEnvironment)) { };
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

    [Serializable]
    class POrIsSymbol : POr1
    {
#if DEBUG
        static Histogram<Type> arg0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected POrIsSymbol (PrimitiveIsSymbol predicate, SCode alternative)
            : base (predicate, alternative)
        {
        }

        public static SCode Make (PrimitiveIsSymbol predicate, SCode alternative)
        {
            return
                (predicate is PrimitiveIsSymbolA) ? POrIsSymbolA.Make ((PrimitiveIsSymbolA) predicate, alternative) :
                (predicate is PrimitiveIsSymbolS) ? POrIsSymbolS.Make ((PrimitiveIsSymbolS) predicate, alternative) :
                new POrIsSymbol (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("POrIsSymbol");
            NoteCalls (this.arg0);
            arg0TypeHistogram.Note (this.arg0Type);
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, closureEnvironment));
                //answer = Interpreter.Unwind;
                //closureEnvironment = env;
                //return false;
            }

            Symbol ev0sym = ev0 as Symbol;
            if (ev0sym != null && ev0sym.IsInterned ()) {
                answer = Constant.sharpT;
                return false;
            }
            else {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                answer = null;
                expression = this.alternative;
                return true;
            }
        }
    }

    [Serializable]
    class POrIsSymbolA : POrIsSymbol
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int argumentOffset;
        protected POrIsSymbolA (PrimitiveIsSymbolA predicate, SCode alternative)
            : base (predicate, alternative)
        {
            this.argumentOffset = predicate.arg0Offset;
        }

        public static SCode Make (PrimitiveIsSymbolA predicate, SCode alternative)
        {
            return
                (predicate is PrimitiveIsSymbolA0) ? POrIsSymbolA0.Make ((PrimitiveIsSymbolA0) predicate, alternative) :
                (predicate is PrimitiveIsSymbolA1) ? POrIsSymbolA1.Make ((PrimitiveIsSymbolA1) predicate, alternative) :
                new POrIsSymbolA (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("POrIsSymbolA");
#endif
            Symbol ev0sym = environment.ArgumentValue (this.argumentOffset) as Symbol;
            if (ev0sym != null && ev0sym.IsInterned ()) {
                answer = Constant.sharpT;
                return false;
            }
            else {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                answer = null;
                expression = this.alternative;
                return true;
            }
        }
    }

    [Serializable]
    class POrIsSymbolA0 : POrIsSymbolA
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected POrIsSymbolA0 (PrimitiveIsSymbolA0 predicate, SCode alternative)
            : base (predicate, alternative)
        {
        }

        public static SCode Make (PrimitiveIsSymbolA0 predicate, SCode alternative)
        {
            return
                new POrIsSymbolA0 (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("POrIsSymbolA0");
#endif
            Symbol ev0sym = environment.Argument0Value as Symbol;
            if (ev0sym != null && ev0sym.IsInterned ()) {
                answer = Constant.sharpT;
                return false;
            }
            else {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                answer = null;
                expression = this.alternative;
                return true;
            }
        }
    }

    [Serializable]
    class POrIsSymbolA1 : POrIsSymbolA
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected POrIsSymbolA1 (PrimitiveIsSymbolA1 predicate, SCode alternative)
            : base (predicate, alternative)
        {
        }

        public static SCode Make (PrimitiveIsSymbolA1 predicate, SCode alternative)
        {
            return
                new POrIsSymbolA1 (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("POrIsSymbolA1");
#endif
            Symbol ev0sym = environment.Argument1Value as Symbol;
            if (ev0sym != null && ev0sym.IsInterned ()) {
                answer = Constant.sharpT;
                return false;
            }
            else {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                answer = null;
                expression = this.alternative;
                return true;
            }
        }
    }

    [Serializable]
    class POrIsSymbolS : POrIsSymbol
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand0Name;
        public readonly int rand0Offset;

        protected POrIsSymbolS (PrimitiveIsSymbolS predicate, SCode alternative)
            : base (predicate, alternative)
        {
            this.rand0Name = predicate.varname;
            this.rand0Offset = predicate.offset;
        }

        public static SCode Make (PrimitiveIsSymbolS predicate, SCode alternative)
        {
            return
                new POrIsSymbolS (predicate, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("POrIsSymbolS");
#endif
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            Symbol ev0sym = ev0 as Symbol;
            if (ev0sym != null && ev0sym.IsInterned ()) {
                answer = Constant.sharpT;
                return false;
            }
            else {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                answer = null;
                expression = this.alternative;
                return true;
            }
        }
    }

}

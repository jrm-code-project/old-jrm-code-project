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
                    (predicate is PrimitiveIsEq) ? POrIsEq.Make ((PrimitiveIsEq) predicate, alternative) :
                    (predicate is PrimitiveIsObjectType) ? POrObjectType.Make ((PrimitiveIsObjectType) predicate, alternative) :
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
#if DEBUG
                SCode.location = "POr2.EvalStep";
#endif
                if (ev1 == Interpreter.UnwindStack)
                {
                    throw new NotImplementedException();
                }

                unev = this.rand0;
                env = environment;
                object ev0;
                while (unev.EvalStep(out ev0, ref unev, ref env)) { };
#if DEBUG
                SCode.location = "POr2.EvalStep";
#endif
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
#if DEBUG
                SCode.location = "POr2.EvalStep";
#endif

                if ((answer is bool) && (bool)answer == false)
                {
#if DEBUG
                    SCode.location = "-";
                    NoteCalls(this.alternative);
                    alternativeTypeHistogram.Note(this.alternativeType);
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

            object ev1 = environment.ArgumentValue(this.rand1Offset);

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


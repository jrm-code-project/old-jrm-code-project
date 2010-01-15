using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;

namespace Microcode
{
    [Serializable]
    class PrimitiveIsObjectType : PrimitiveCombination2
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type>();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type>();
#endif

        protected PrimitiveIsObjectType (Primitive2 rator, SCode rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        static SCode RewriteAsSameType (PrimitiveObjectType left, SCode right)
        {
            return PrimitiveCombination2.Make (Primitive.IsSameType, left.Operand, right);
        }

        public static new SCode Make (Primitive2 rator, SCode rand0, SCode rand1)
        {
            return
                (rand0 is PrimitiveObjectType &&
                Configuration.EnableObjectTypePrimitives) ? RewriteAsSameType ((PrimitiveObjectType) rand0, rand1) :
                (rand0 is Quotation) ? PrimitiveIsObjectTypeQ.Make (rator, (Quotation) rand0, rand1) :
                //(rand1 is Quotation) ? PrimitiveIsObjectTypeSQ.Make (rator, rand0, (Quotation) rand1) :
                (rand1 is Argument) ? PrimitiveIsObjectTypeXA.Make (rator, rand0, (Argument) rand1) :
                new PrimitiveIsObjectType (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            NoteCalls (this.rand1);
            rand0TypeHistogram.Note(this.rand0Type);
            rand1TypeHistogram.Note(this.rand1Type);
            SCode.location = "PrimitiveIsObjectType";
#endif
            // Eval argument1
            object ev1;

            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveIsObjectType";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // Eval argument0
            object ev0;

            unev = this.rand0;
            env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveIsObjectType.EvalStep";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            if (ObjectModel.IsPrimitiveObjectType (out answer, ev0, ev1))
                throw new NotImplementedException();
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsObjectTypeQ : PrimitiveIsObjectType
    {
        public readonly TC rand0Value;

        protected PrimitiveIsObjectTypeQ (Primitive2 rator, Quotation rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
            this.rand0Value = (TC) (int) rand0.Quoted;
        }

        public static SCode AlwaysFalse (SCode rand1)
        {
#if DEBUG
            Debug.WriteLine ("Folding compiled-entry? => false");
#endif
            return Sequence2.Make (rand1, Quotation.Make (false));
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, SCode rand1)
        {
            TC code = (TC) rand0.Quoted;
            return
                (! Configuration.EnableObjectTypePrimitives) ? new PrimitiveIsObjectTypeQ (rator, rand0, rand1) :
                (code == TC.BIG_FIXNUM) ? PrimitiveCombination1.Make (Primitive.IsBigFixnum, rand1) :
                (code == TC.BIG_FLONUM) ? PrimitiveCombination1.Make (Primitive.IsBigFlonum, rand1) :
                (code == TC.COMPILED_ENTRY) ? PrimitiveCombination1.Make (Primitive.IsCompiledEntry, rand1) :
                (code == TC.COMPLEX) ? PrimitiveCombination1.Make (Primitive.IsComplex, rand1) :
                (code == TC.ENTITY) ? PrimitiveCombination1.Make (Primitive.IsEntity, rand1) :
                (code == TC.FIXNUM) ? PrimitiveCombination1.Make (Primitive.IsFixnum, rand1) :
                (code == TC.INTERNED_SYMBOL) ? PrimitiveCombination1.Make (Primitive.IsSymbol, rand1) :
                (code == TC.RATNUM) ? PrimitiveCombination1.Make (Primitive.IsRatnum, rand1) :
                (code == TC.UNINTERNED_SYMBOL) ? PrimitiveCombination1.Make (Primitive.IsUninternedSymbol, rand1) :
                (code == TC.VECTOR) ? PrimitiveCombination1.Make (Primitive.IsVector, rand1) :
                (code == TC.WEAK_CONS) ? PrimitiveCombination1.Make (Primitive.IsWeakCons, rand1) :
                //(rand1 is Quotation) ? PrimitiveIsObjectTypeQQ.Make (rator, rand0, (Quotation) rand1) :
                (rand1 is Argument) ? PrimitiveIsObjectTypeQA.Make (rator, rand0, (Argument) rand1) :
                 new PrimitiveIsObjectTypeQ (rator, rand0, rand1);
        }


        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand1);
            SCode.location = "PrimitiveIsObjectTypeQ";
#endif
            // Eval argument1
            object ev1;

            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveIsObjectTypeQ";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            if (ObjectModel.IsPrimitiveObjectType (out answer, this.rand0Value, ev1))
                throw new NotImplementedException();
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsObjectTypeQA : PrimitiveIsObjectTypeQ
    {
#if DEBUG
        static Histogram<TC> tcHistogram = new Histogram<TC> ();
#endif
        public readonly int rand1Offset;
        protected PrimitiveIsObjectTypeQA (Primitive2 rator, Quotation rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, Argument rand1)
        {
            TC code = (TC) rand0.Quoted;
            return
                 new PrimitiveIsObjectTypeQA (rator, rand0, rand1);
        }


        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            tcHistogram.Note (this.rand0Value);
            SCode.location = "PrimitiveIsObjectTypeQA";
#endif
            // Eval argument1
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            if (ObjectModel.IsPrimitiveObjectType (out answer, this.rand0Value, ev1))
                throw new NotImplementedException ();
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsObjectTypeXA : PrimitiveIsObjectType
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif

        public readonly int rand1Offset;
        protected PrimitiveIsObjectTypeXA (Primitive2 rator, SCode rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitiveIsObjectTypeXA0.Make (rator, rand0, (Argument0) rand1) :
                 new PrimitiveIsObjectTypeXA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PrimitiveIsObjectTypeXA";
#endif
            // Eval argument1
            object ev1 = environment.ArgumentValue (this.rand1Offset);

            // Eval argument0
            object ev0;

            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveIsObjectTypeXA";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            if (ObjectModel.IsPrimitiveObjectType (out answer, ev0, ev1))
                throw new NotImplementedException ();
            return false;
        }

    }

    [Serializable]
    class PrimitiveIsObjectTypeXA0 : PrimitiveIsObjectTypeXA
    {
#if DEBUG
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif


        protected PrimitiveIsObjectTypeXA0 (Primitive2 rator, SCode rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Argument0 rand1)
        {
            return
                 new PrimitiveIsObjectTypeXA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PrimitiveIsObjectTypeXA0";
#endif
            // Eval argument1
            object ev1 = environment.Argument0Value;

            // Eval argument0
            object ev0;

            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveIsObjectTypeXA0";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            if (ObjectModel.IsPrimitiveObjectType (out answer, ev0, ev1))
                throw new NotImplementedException ();
            return false;
        }

    }
}

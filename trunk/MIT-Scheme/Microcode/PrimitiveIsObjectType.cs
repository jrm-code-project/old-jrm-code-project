using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    [Serializable]
    class PrimitiveIsObjectType : PrimitiveCombination2
    {
        protected PrimitiveIsObjectType (Primitive2 rator, SCode rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static new SCode Make (Primitive2 rator, SCode rand0, SCode rand1)
        {
            return
                (rand0 is Quotation) ? PrimitiveIsObjectTypeQ.Make (rator, (Quotation) rand0, rand1) :
                //(rand1 is Quotation) ? PrimitiveIsObjectTypeSQ.Make (rator, rand0, (Quotation) rand1) :
                new PrimitiveIsObjectType (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.rand0);
            NoteCalls (this.rand1);
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

        public static SCode Make (Primitive2 rator, Quotation rand0, SCode rand1)
        {
            TC code = (TC) rand0.Quoted;
            return
                (! Configuration.EnableObjectTypePrimitives) ? new PrimitiveIsObjectTypeQ (rator, rand0, rand1) :
                (code == TC.BIG_FIXNUM) ? PrimitiveCombination1.Make (Primitive.IsBigFixnum, rand1) :
                (code == TC.BIG_FLONUM) ? PrimitiveCombination1.Make (Primitive.IsBigFlonum, rand1) :
                
                // There are no compiled entries, so simply eval the operand and return false.
                // Other optimizations may improve this sequence.
                (code == TC.COMPILED_ENTRY) ? Sequence2.Make (rand1, Quotation.Make (false)) :
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

#if NIL

    [Serializable]
    class PrimitiveIsObjectTypeQL : PrimitiveIsObjectTypeQ
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveIsObjectTypeQL (Primitive2 rator, Quotation rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveIsObjectTypeQA.Make (rator, rand0, (Argument) rand1) :
                (rand1 is LexicalVariable1) ? PrimitiveIsObjectTypeQL1.Make (rator, rand0, (LexicalVariable1) rand1) :
                new PrimitiveIsObjectTypeQL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PrimitiveIsObjectTypeQA : PrimitiveIsObjectTypeQL
    {
        protected PrimitiveIsObjectTypeQA (Primitive2 rator, Quotation rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitiveIsObjectTypeQA0.Make (rator, rand0, (Argument0) rand1) :
                (rand1 is Argument1) ? PrimitiveIsObjectTypeQA1.Make (rator, rand0, (Argument1) rand1) :
                new PrimitiveIsObjectTypeQA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("IsPrimitiveObjectTypeQA.EvalStep");
#endif
            if (ObjectModel.IsPrimitiveObjectType (out answer, this.rand0Value , environment.ArgumentValue(this.rand1Offset)))
                throw new NotImplementedException();
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsObjectTypeQA0 : PrimitiveIsObjectTypeQA
    {
        PrimitiveIsObjectTypeQA0 (Primitive2 rator, Quotation rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, Argument0 rand1)
        {
            return
                 new PrimitiveIsObjectTypeQA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsObjectTypeQA0.EvalStep");
#endif
            if (ObjectModel.IsPrimitiveObjectType (out answer, this.rand0Value , environment.Argument0Value))
                throw new NotImplementedException();
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsObjectTypeQA1 : PrimitiveIsObjectTypeQA
    {
        PrimitiveIsObjectTypeQA1 (Primitive2 rator, Quotation rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, Argument1 rand1)
        {
            return
                 new PrimitiveIsObjectTypeQA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsObjectTypeQA1.EvalStep");
#endif
            if (ObjectModel.IsPrimitiveObjectType (out answer, this.rand0Value, environment.Argument1Value))
                throw new NotImplementedException();
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsObjectTypeQL1 : PrimitiveIsObjectTypeQL
    {
        PrimitiveIsObjectTypeQL1 (Primitive2 rator, Quotation rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, LexicalVariable1 rand1)
        {
            return
                 new PrimitiveIsObjectTypeQL1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsObjectTypeQL1.EvalStep");
#endif
            // Eval argument1
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            if (ObjectModel.IsPrimitiveObjectType (out answer, this.rand0Value, ev1))
                throw new NotImplementedException();
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsObjectTypeQQ : PrimitiveIsObjectTypeQ
    {
        public readonly object rand1Value;

        PrimitiveIsObjectTypeQQ (Primitive2 rator, Quotation rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value =  rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, Quotation rand0, Quotation rand1)
        {
            return
                 new PrimitiveIsObjectTypeQQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            if (ObjectModel.IsPrimitiveObjectType (out answer, this.rand0Value , (this.rand1Value)))
                throw new NotImplementedException();
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsObjectTypeSL : PrimitiveIsObjectType
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveIsObjectTypeSL (Primitive2 rator, SCode rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, SCode rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveIsObjectTypeSA.Make (rator, rand0, (Argument) rand1) :
                (rand1 is LexicalVariable1) ? PrimitiveIsObjectTypeSL1.Make (rator, rand0, (LexicalVariable1) rand1) :
                new PrimitiveIsObjectTypeSL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.rand0);
#endif
            // Eval argument1
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            // Eval argument0
            object ev0;

            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            if (ObjectModel.IsPrimitiveObjectType (out answer, ( ev0), (ev1)))
                throw new NotImplementedException();
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsObjectTypeSA : PrimitiveIsObjectTypeSL
    {
        protected PrimitiveIsObjectTypeSA (Primitive2 rator, SCode rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitiveIsObjectTypeSA0.Make (rator, rand0, (Argument0) rand1) :
                (rand1 is Argument1) ? PrimitiveIsObjectTypeSA1.Make (rator, rand0, (Argument1) rand1) :
                new PrimitiveIsObjectTypeSA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.rand1);
#endif


            // Eval argument0
            object ev0;

            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            if (ObjectModel.IsPrimitiveObjectType (out answer, ( ev0), (environment.ArgumentValue(this.rand1Offset))))
                throw new NotImplementedException();
            return false;
        }

    }

    [Serializable]
    sealed class PrimitiveIsObjectTypeSA0 : PrimitiveIsObjectTypeSA
    {
        PrimitiveIsObjectTypeSA0 (Primitive2 rator, SCode rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Argument0 rand1)
        {
            return
                new PrimitiveIsObjectTypeSA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PrimitiveIsObjectTypeSA0.EvalStep";
#endif
            // Eval argument0
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveIsObjectTypeSA0.EvalStep";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            if (ObjectModel.IsPrimitiveObjectType (out answer, ev0, environment.Argument0Value))
                throw new NotImplementedException ();
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsObjectTypeSA1 : PrimitiveIsObjectTypeSA
    {
        PrimitiveIsObjectTypeSA1 (Primitive2 rator, SCode rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Argument1 rand1)
        {
            return

                new PrimitiveIsObjectTypeSA1 (rator, rand0, rand1);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveIsObjectTypeSA1.EvalStep");
            noteCalls (this.rand0);
#endif

            // Eval argument0
            object ev0;

            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            if (ObjectModel.IsPrimitiveObjectType (out answer, ev0, environment.Argument1Value))
                throw new NotImplementedException();
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveIsObjectTypeSL1 : PrimitiveIsObjectTypeSL
    {
        PrimitiveIsObjectTypeSL1 (Primitive2 rator, SCode rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, SCode rand0, LexicalVariable1 rand1)
        {
            return
                new PrimitiveIsObjectTypeSL1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            noteCalls (this.rand0);
            SCode.location = "PrimitiveIsObjectTypeSL1.EvalStep";
#endif
            // Eval argument1
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            // Eval argument0
            object ev0;
            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveIsObjectTypeSL1.EvalStep";
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
    sealed class PrimitiveIsObjectTypeSQ : PrimitiveIsObjectType
    {
        public readonly object rand1Value;

        PrimitiveIsObjectTypeSQ (Primitive2 rator, SCode rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value =  rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, SCode rand0, Quotation rand1)
        {
            return
                new PrimitiveIsObjectTypeSQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.rand0);
#endif
            // Eval argument0
            object ev0;

            Control unev = this.rand0;
            Environment env = environment;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            if (ObjectModel.IsPrimitiveObjectType (out answer, ( ev0), (this.rand1Value)))
                throw new NotImplementedException();
            return false;
        }
    }
#endif
}

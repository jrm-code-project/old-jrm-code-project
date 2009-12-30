using System;
using System.Collections.Generic;
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

#if NIL
    [Serializable]
    class PrimitiveIsObjectTypeA : PrimitiveIsObjectType
    {
        protected PrimitiveIsObjectTypeA (Primitive2 rator, Argument rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, SCode rand1)
        {
            return
                (rand0 is Argument0) ? PrimitiveIsObjectTypeA0.Make (rator, (Argument0) rand0, rand1)
                : (rand0 is Argument1) ? PrimitiveIsObjectTypeA1.Make (rator, (Argument1) rand0, rand1)
                : (rand1 is LexicalVariable) ? PrimitiveIsObjectTypeAL.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveIsObjectTypeAQ.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveIsObjectTypeA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.rand1);
#endif
            // Eval argument1
            object ev1;

            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // Eval argument0
            if (ObjectModel.IsPrimitiveObjectType (out answer, ( environment.ArgumentValue(this.rand0Offset)), (ev1)))
                throw new NotImplementedException();
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsObjectTypeA0 : PrimitiveIsObjectTypeA
    {
        protected PrimitiveIsObjectTypeA0 (Primitive2 rator, Argument0 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, SCode rand1)
        {
            return
                (rand1 is LexicalVariable) ? PrimitiveIsObjectTypeA0L.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveIsObjectTypeA0Q.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveIsObjectTypeA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.rand0);
            noteCalls (this.rand1);
#endif
            // Eval argument1
            object ev1;

            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            if (ObjectModel.IsPrimitiveObjectType (out answer, ( environment.Argument0Value), (ev1)))
                throw new NotImplementedException();
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsObjectTypeA0L : PrimitiveIsObjectTypeA0
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveIsObjectTypeA0L (Primitive2 rator, Argument0 rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveIsObjectTypeA0A.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? PrimitiveIsObjectTypeA0L1.Make (rator, rand0, (LexicalVariable1) rand1)
                : new PrimitiveIsObjectTypeA0L (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            // Eval argument1
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            if (ObjectModel.IsPrimitiveObjectType (out answer, ( environment.Argument0Value), (ev1)))
                throw new NotImplementedException();
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsObjectTypeA0A : PrimitiveIsObjectTypeA0L
    {
        protected PrimitiveIsObjectTypeA0A (Primitive2 rator, Argument0 rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitiveIsObjectTypeA0A0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? PrimitiveIsObjectTypeA0A1.Make (rator, rand0, (Argument1) rand1)
                : new PrimitiveIsObjectTypeA0A (rator, rand0, rand1);
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
    class PrimitiveIsObjectTypeA0A0 : PrimitiveIsObjectTypeA0A
    {
        protected PrimitiveIsObjectTypeA0A0 (Primitive2 rator, Argument0 rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Argument0 rand1)
        {
            throw new NotImplementedException ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }



    [Serializable]
    class PrimitiveIsObjectTypeA0A1 : PrimitiveIsObjectTypeA0A
    {
        protected PrimitiveIsObjectTypeA0A1 (Primitive2 rator, Argument0 rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Argument1 rand1)
        {
            return
                new PrimitiveIsObjectTypeA0A1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            if (ObjectModel.IsPrimitiveObjectType (out answer, ( environment.Argument0Value), (environment.Argument1Value)))
                throw new NotImplementedException();
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsObjectTypeA0L1 : PrimitiveIsObjectTypeA0L
    {
        protected PrimitiveIsObjectTypeA0L1 (Primitive2 rator, Argument0 rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, LexicalVariable1 rand1)
        {
            return
                new PrimitiveIsObjectTypeA0L1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            // Eval argument1
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            if (ObjectModel.IsPrimitiveObjectType (out answer, ( environment.Argument0Value), (ev1)))
                throw new NotImplementedException();
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsObjectTypeA0Q : PrimitiveIsObjectTypeA0
    {
        public readonly object rand1Value;

        protected PrimitiveIsObjectTypeA0Q (Primitive2 rator, Argument0 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value =  rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Quotation rand1)
        {
            return
                new PrimitiveIsObjectTypeA0Q (rator, rand0, (Quotation) rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            if (ObjectModel.IsPrimitiveObjectType (out answer, ( environment.Argument0Value), (this.rand1Value)))
                throw new NotImplementedException();
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsObjectTypeA1 : PrimitiveIsObjectTypeA
    {
        protected PrimitiveIsObjectTypeA1 (Primitive2 rator, Argument1 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, SCode rand1)
        {
            return
                (rand1 is LexicalVariable) ? PrimitiveIsObjectTypeA1L.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveIsObjectTypeA1Q.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveIsObjectTypeA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.rand1);
#endif
            // Eval argument1
            object ev1;

            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            if (ObjectModel.IsPrimitiveObjectType (out answer, ( environment.Argument1Value), (ev1)))
                throw new NotImplementedException();
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsObjectTypeA1L : PrimitiveIsObjectTypeA1
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveIsObjectTypeA1L (Primitive2 rator, Argument1 rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveIsObjectTypeA1A.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? PrimitiveIsObjectTypeA1L1.Make (rator, rand0, (LexicalVariable1) rand1)
                : new PrimitiveIsObjectTypeA1L (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();

#endif
            // Eval argument1
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            if (ObjectModel.IsPrimitiveObjectType (out answer, ( environment.Argument1Value), (ev1)))
                throw new NotImplementedException();
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsObjectTypeA1A : PrimitiveIsObjectTypeA1L
    {
        protected PrimitiveIsObjectTypeA1A (Primitive2 rator, Argument1 rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? PrimitiveIsObjectTypeA1A0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? Unimplemented ()
                : new PrimitiveIsObjectTypeA1A (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif

            if (ObjectModel.IsPrimitiveObjectType (out answer, ( environment.Argument1Value), (environment.ArgumentValue(this.rand1Offset))))
                throw new NotImplementedException();
            return false;
        }

    }



    [Serializable]
    class PrimitiveIsObjectTypeA1A0 : PrimitiveIsObjectTypeA1A
    {
        protected PrimitiveIsObjectTypeA1A0 (Primitive2 rator, Argument1 rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, Argument0 rand1)
        {
            return
                 new PrimitiveIsObjectTypeA1A0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            if (ObjectModel.IsPrimitiveObjectType (out answer, ( environment.Argument1Value), (environment.Argument0Value)))
                throw new NotImplementedException();
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsObjectTypeA1A1 : PrimitiveIsObjectTypeA1A
    {
        protected PrimitiveIsObjectTypeA1A1 (Primitive2 rator, Argument1 rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument0 rand0, Argument1 rand1)
        {
            throw new NotImplementedException ();
            //new PrimitiveIsObjectTypeA1A1 (rator, rand0, rand1);
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
    class PrimitiveIsObjectTypeA1L1 : PrimitiveIsObjectTypeA1L
    {
        protected PrimitiveIsObjectTypeA1L1 (Primitive2 rator, Argument1 rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, LexicalVariable1 rand1)
        {
            return
                new PrimitiveIsObjectTypeA1L1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            // Eval argument1
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            // Eval argument0
            object ev0 =  environment.Argument1Value;

            // Greater-than-fixnum?
            if (ObjectModel.IsPrimitiveObjectType (out answer, ( ev0), (ev1)))
                throw new NotImplementedException();
            return false; throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PrimitiveIsObjectTypeA1Q : PrimitiveIsObjectTypeA1
    {
        public readonly object rand1Value;

        protected PrimitiveIsObjectTypeA1Q (Primitive2 rator, Argument1 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value =  rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, Argument1 rand0, Quotation rand1)
        {
            return
                new PrimitiveIsObjectTypeA1Q (rator, rand0, (Quotation) rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            if (ObjectModel.IsPrimitiveObjectType (out answer, ( environment.Argument1Value), (this.rand1Value)))
                throw new NotImplementedException();
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsObjectTypeAL : PrimitiveIsObjectTypeA
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveIsObjectTypeAL (Primitive2 rator, Argument rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, Argument rand0, LexicalVariable rand1)
        {
            return
                (rand1 is Argument) ? PrimitiveIsObjectTypeAA.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? Unimplemented ()
                : new PrimitiveIsObjectTypeAL (rator, rand0, rand1);
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
    class PrimitiveIsObjectTypeAA : PrimitiveIsObjectTypeAL
    {
        protected PrimitiveIsObjectTypeAA (Primitive2 rator, Argument rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, Argument rand1)
        {
            return
                (rand1 is Argument0) ? Unimplemented ()
                : (rand1 is Argument1) ? PrimitiveIsObjectTypeAA1.Make (rator, rand0, (Argument1) rand1)
                : new PrimitiveIsObjectTypeAA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            if (ObjectModel.IsPrimitiveObjectType (out answer, ( environment.ArgumentValue (this.rand0Offset)), (environment.ArgumentValue(this.rand1Offset))))
                throw new NotImplementedException();
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsObjectTypeAA0 : PrimitiveIsObjectTypeAA
    {
        protected PrimitiveIsObjectTypeAA0 (Primitive2 rator, Argument rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, Argument0 rand1)
        {
            return new PrimitiveIsObjectTypeAA0 (rator, rand0, rand1);
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
    class PrimitiveIsObjectTypeAA1 : PrimitiveIsObjectTypeAA
    {
        protected PrimitiveIsObjectTypeAA1 (Primitive2 rator, Argument rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, Argument1 rand1)
        {
            return new PrimitiveIsObjectTypeAA1 (rator, rand0, rand1);
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
    class PrimitiveIsObjectTypeAL1 : PrimitiveIsObjectTypeAL
    {
        protected PrimitiveIsObjectTypeAL1 (Primitive2 rator, Argument rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, Argument rand0, LexicalVariable1 rand1)
        {
            return new PrimitiveIsObjectTypeAL1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class PrimitiveIsObjectTypeAQ : PrimitiveIsObjectTypeA
    {
        public readonly object rand1Value;

        protected PrimitiveIsObjectTypeAQ (Primitive2 rator, Argument rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value =  rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, Argument rand0, Quotation rand1)
        {
            return
                new PrimitiveIsObjectTypeAQ (rator, rand0, (Quotation) rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            if (ObjectModel.IsPrimitiveObjectType (out answer, ( environment.ArgumentValue(this.rand0Offset)), (this.rand1Value)))
                throw new NotImplementedException();
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsObjectTypeL1 : PrimitiveIsObjectTypeL
    {
        protected PrimitiveIsObjectTypeL1 (Primitive2 rator, LexicalVariable1 rand0, SCode rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, SCode rand1)
        {
            return
                (rand1 is LexicalVariable) ? PrimitiveIsObjectTypeL1L.Make (rator, rand0, (LexicalVariable) rand1)
                : (rand1 is Quotation) ? PrimitiveIsObjectTypeL1Q.Make (rator, rand0, (Quotation) rand1)
                : new PrimitiveIsObjectTypeL1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.rand1);
#endif
            // Eval argument1
            object ev1;

            Control unev = this.rand1;
            Environment env = environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            // Eval argument0
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            if (ObjectModel.IsPrimitiveObjectType (out answer, ( ev0), (ev1)))
                throw new NotImplementedException();
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsObjectTypeL1L : PrimitiveIsObjectTypeL1
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveIsObjectTypeL1L (Primitive2 rator, LexicalVariable1 rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, LexicalVariable rand1)
        {
            return
                 (rand1 is Argument) ? PrimitiveIsObjectTypeL1A.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? PrimitiveIsObjectTypeL1L1.Make (rator, rand0, (LexicalVariable1) rand1)
                : new PrimitiveIsObjectTypeL1L (rator, rand0, rand1);
        }
        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            // Eval argument1
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            // Eval argument0
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();


            if (ObjectModel.IsPrimitiveObjectType (out answer, ( ev0), (ev1)))
                throw new NotImplementedException();
            return false;
        }

    }

    [Serializable]
    class PrimitiveIsObjectTypeL1A : PrimitiveIsObjectTypeL1L
    {
        protected PrimitiveIsObjectTypeL1A (Primitive2 rator, LexicalVariable1 rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, Argument rand1)
        {
            return
                 (rand1 is Argument0) ? PrimitiveIsObjectTypeL1A0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? PrimitiveIsObjectTypeL1A1.Make (rator, rand0, (Argument1) rand1)
                : new PrimitiveIsObjectTypeL1A (rator, rand0, rand1);
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
    class PrimitiveIsObjectTypeL1A0 : PrimitiveIsObjectTypeL1A
    {
        protected PrimitiveIsObjectTypeL1A0 (Primitive2 rator, LexicalVariable1 rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, Argument0 rand1)
        {
            return
                 new PrimitiveIsObjectTypeL1A0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            // Eval argument0
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            if (ObjectModel.IsPrimitiveObjectType (out answer, ( ev0), (environment.Argument0Value)))
                throw new NotImplementedException();
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsObjectTypeL1A1 : PrimitiveIsObjectTypeL1A
    {
        protected PrimitiveIsObjectTypeL1A1 (Primitive2 rator, LexicalVariable1 rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, Argument1 rand1)
        {
            return
                 new PrimitiveIsObjectTypeL1A1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif

            // Eval argument0
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            if (ObjectModel.IsPrimitiveObjectType (out answer, ( ev0), (environment.Argument1Value)))
                throw new NotImplementedException();
            return false;
        }

    }

    [Serializable]
    class PrimitiveIsObjectTypeL1L1 : PrimitiveIsObjectTypeL1L
    {
        protected PrimitiveIsObjectTypeL1L1 (Primitive2 rator, LexicalVariable1 rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, LexicalVariable1 rand1)
        {
            return
                 new PrimitiveIsObjectTypeL1L1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            // Eval argument1
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            // Eval argument0
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();

            if (ObjectModel.IsPrimitiveObjectType (out answer, ( ev0), (ev1)))
                throw new NotImplementedException();
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsObjectTypeL1Q : PrimitiveIsObjectTypeL1
    {
        public readonly object rand1Value;

        protected PrimitiveIsObjectTypeL1Q (Primitive2 rator, LexicalVariable1 rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value =  rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable1 rand0, Quotation rand1)
        {
            return
                  new PrimitiveIsObjectTypeL1Q (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            object ev0;
            if (environment.FastLexicalRef1 (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
            if (ObjectModel.IsPrimitiveObjectType (out answer, ( ev0), (this.rand1Value)))
                throw new NotImplementedException();
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsObjectTypeLL : PrimitiveIsObjectTypeL
    {
        public readonly object rand1Name;
        public readonly int rand1Depth;
        public readonly int rand1Offset;

        protected PrimitiveIsObjectTypeLL (Primitive2 rator, LexicalVariable rand0, LexicalVariable rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Name = rand1.Name;
            this.rand1Depth = rand1.Depth;
            this.rand1Offset = rand1.Offset;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, LexicalVariable rand1)
        {
            return
                 (rand1 is Argument) ? PrimitiveIsObjectTypeLA.Make (rator, rand0, (Argument) rand1)
                : (rand1 is LexicalVariable1) ? PrimitiveIsObjectTypeLL1.Make (rator, rand0, (LexicalVariable1) rand1)
                : new PrimitiveIsObjectTypeLL (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            // Eval argument1
            object ev1;
            if (environment.FastLexicalRef (out ev1, this.rand1Name, this.rand1Depth, this.rand1Offset))
                throw new NotImplementedException ();

            // Eval argument0
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if (ObjectModel.IsPrimitiveObjectType (out answer, ( ev0), (ev1)))
                throw new NotImplementedException();
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsObjectTypeLA : PrimitiveIsObjectTypeLL
    {
        protected PrimitiveIsObjectTypeLA (Primitive2 rator, LexicalVariable rand0, Argument rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, Argument rand1)
        {
            return
                 (rand1 is Argument0) ? PrimitiveIsObjectTypeLA0.Make (rator, rand0, (Argument0) rand1)
                : (rand1 is Argument1) ? PrimitiveIsObjectTypeLA1.Make (rator, rand0, (Argument1) rand1)
                : new PrimitiveIsObjectTypeLA (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();
            if (ObjectModel.IsPrimitiveObjectType (out answer, ( ev0) , ( environment.ArgumentValue(this.rand1Offset))))
                throw new NotImplementedException();
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsObjectTypeLA0 : PrimitiveIsObjectTypeLA
    {
        protected PrimitiveIsObjectTypeLA0 (Primitive2 rator, LexicalVariable rand0, Argument0 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, Argument0 rand1)
        {
            return
                 new PrimitiveIsObjectTypeLA0 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();
            if (ObjectModel.IsPrimitiveObjectType (out answer, ( ev0), (environment.Argument0Value)))
                throw new NotImplementedException();
            return false;
        }
    }

    [Serializable]
    class PrimitiveIsObjectTypeLA1 : PrimitiveIsObjectTypeLA
    {
        protected PrimitiveIsObjectTypeLA1 (Primitive2 rator, LexicalVariable rand0, Argument1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, Argument1 rand1)
        {
            return
                 new PrimitiveIsObjectTypeLA1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            // Eval argument0
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if (ObjectModel.IsPrimitiveObjectType (out answer, ( ev0), (environment.Argument1Value)))
                throw new NotImplementedException();
            return false;
        }

    }

    [Serializable]
    class PrimitiveIsObjectTypeLL1 : PrimitiveIsObjectTypeLL
    {
        protected PrimitiveIsObjectTypeLL1 (Primitive2 rator, LexicalVariable rand0, LexicalVariable1 rand1)
            : base (rator, rand0, rand1)
        {
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, LexicalVariable1 rand1)
        {
            return
                 new PrimitiveIsObjectTypeLL1 (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.rand0);
            noteCalls (this.rand1);
#endif
            // Eval argument1
            object ev1;
            if (environment.FastLexicalRef1 (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            // Eval argument0
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();

            if (ObjectModel.IsPrimitiveObjectType (out answer, ( ev0), (ev1)))
                throw new NotImplementedException();
            return false;
        }


    }

    [Serializable]
    class PrimitiveIsObjectTypeLQ : PrimitiveIsObjectTypeL
    {
        public readonly object rand1Value;

        protected PrimitiveIsObjectTypeLQ (Primitive2 rator, LexicalVariable rand0, Quotation rand1)
            : base (rator, rand0, rand1)
        {
            this.rand1Value =  rand1.Quoted;
        }

        public static SCode Make (Primitive2 rator, LexicalVariable rand0, Quotation rand1)
        {
            return new PrimitiveIsObjectTypeLQ (rator, rand0, rand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif
            object ev0;
            if (environment.FastLexicalRef (out ev0, this.rand0Name, this.rand0Depth, this.rand0Offset))
                throw new NotImplementedException ();
            if (ObjectModel.IsPrimitiveObjectType (out answer, ( ev0), (this.rand1Value)))
                throw new NotImplementedException();
            return false;
        }
    }
#endif

    [Serializable]
    class PrimitiveIsObjectTypeQ : PrimitiveIsObjectType
    {
#if DEBUG
        static Histogram<TC> tcHistogram = new Histogram<TC>();
#endif
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
                //(code == TC.ACCESS) ? PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("ACCESS?", 1), rand1)
                //: (code == TC.ASSIGNMENT) ? PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("ASSIGNMENT?", 1), rand1)
                (code == TC.BIG_FIXNUM) ? PrimitiveCombination1.Make (Primitive.IsBigFixnum, rand1) :
                (code == TC.BIG_FLONUM) ? PrimitiveCombination1.Make (Primitive.IsBigFlonum, rand1) :
                //: (code == TC.BROKEN_HEART) ? PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("BROKEN-HEART?", 1), rand1)
                //: (code == TC.COMBINATION) ? PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("COMBINATION?", 1), rand1)
                //: (code == TC.COMBINATION_1) ? PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("COMBINATION1?", 1), rand1)
                //: (code == TC.COMBINATION_2) ? PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("COMBINATION2?", 1), rand1)
                //: (code == TC.COMMENT) ? PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("COMMENT?", 1), rand1)
                //: (code == TC.COMPILED_CODE_BLOCK) ? PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("COMPILED_CODE_BLOCK?", 1), rand1)

                // There are no compiled entries, so simply eval the operand and return false.
                // Other optimizations may improve this sequence.
                (code == TC.COMPILED_ENTRY) ? Sequence2.Make (rand1, Quotation.Make (false)) :
                (code == TC.COMPLEX) ? PrimitiveCombination1.Make (Primitive.IsComplex, rand1) :
                //: (code == TC.CONDITIONAL) ? PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("CONDITIONAL?", 1), rand1)
                //: (code == TC.CONTROL_POINT) ? PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("CONTROL-POINT?", 1), rand1)
                //: (code == TC.DEFINITION) ? PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("DEFINITION?", 1), rand1)
                //: (code == TC.DELAY) ? PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("DELAY?", 1), rand1)
                //: (code == TC.DELAYED) ? PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("DELAYED?", 1), rand1)
                //: (code == TC.DISJUNCTION) ? PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("DISJUNCTION?", 1), rand1)
                (code == TC.ENTITY) ? PrimitiveCombination1.Make (Primitive.IsEntity, rand1) :
                //: (code == TC.EXTENDED_LAMBDA) ? PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("EXTENDED-LAMBDA?", 1), rand1)
                //(code == TC.EXTENDED_PROCEDURE) ? PrimitiveCombination1.Make (Primitive.IsExtendedProcedure, rand1) :
                //: (code == TC.ENVIRONMENT) ? PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("ENVIRONMENT?", 1), rand1)
                (code == TC.FIXNUM) ? PrimitiveCombination1.Make (Primitive.IsFixnum, rand1) :
                //: (code == TC.HUNK3_B) ? PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("HUNK3B?", 1), rand1)
                (code == TC.INTERNED_SYMBOL) ? PrimitiveCombination1.Make (Primitive.IsSymbol, rand1) :
                //: (code == TC.LAMBDA) ? PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("LAMBDA?", 1), rand1)
                //: (code == TC.LEXPR) ? PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("LEXPR?", 1), rand1)
                //: (code == TC.MANIFEST_CLOSURE) ? PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("MANIFEST-CLOSURE?", 1), rand1)
                //: (code == TC.MANIFEST_NM_VECTOR) ? PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("MANIFEST-NM-VECTOR?", 1), rand1)
                //: (code == TC.PCOMB0) ? PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("PRIMITIVE-COMBINATION0?", 1), rand1)
                //: (code == TC.PCOMB1) ? PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("PRIMITIVE-COMBINATION1?", 1), rand1)
                //: (code == TC.PCOMB2) ? PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("PRIMITIVE-COMBINATION2?", 1), rand1)
                //: (code == TC.PCOMB3) ? PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("PRIMITIVE-COMBINATION3?", 1), rand1)
                //: (code == TC.PRIMITIVE) ? PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("PRIMITIVE?", 1), rand1)
                //: (code == TC.PROCEDURE) ? PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("PROCEDURE?", 1), rand1)
                (code == TC.RATNUM) ? PrimitiveCombination1.Make (Primitive.IsRatnum, rand1) :
                //: (code == TC.REFERENCE_TRAP) ? PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("REFERENCE-TRAP?", 1), rand1)
                //: (code == TC.RETURN_CODE) ? PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("RETURN-CODE?", 1), rand1)
                //: (code == TC.SCODE_QUOTE) ? PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("SCODE-QUOTE?", 1), rand1)
                //: (code == TC.SEQUENCE_2) ? PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("SEQUENCE2?", 1), rand1)
                //: (code == TC.SEQUENCE_3) ? PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("SEQUENCE3?", 1), rand1)
                //: (code == TC.STACK_ENVIRONMENT) ? PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("STACK-ENVIRONMENT?", 1), rand1)
                //: (code == TC.THE_ENVIRONMENT) ? PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("THE-ENVIRONMENT?", 1), rand1)
                (code == TC.UNINTERNED_SYMBOL) ? PrimitiveCombination1.Make (Primitive.IsUninternedSymbol, rand1) :
                //: (code == TC.VARIABLE) ? PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("VARIABLE?", 1), rand1)
                (code == TC.VECTOR) ? PrimitiveCombination1.Make (Primitive.IsVector, rand1) :
                (code == TC.WEAK_CONS) ? PrimitiveCombination1.Make (Primitive.IsWeakCons, rand1) :
                //: Unimplemented ();
                //(rand1 is LexicalVariable) ? PrimitiveIsObjectTypeQL.Make (rator, rand0, (LexicalVariable) rand1) :
                //(rand1 is Quotation) ? PrimitiveIsObjectTypeQQ.Make (rator, rand0, (Quotation) rand1) :
                (rand1 is Argument) ? PrimitiveIsObjectTypeQA.Make (rator, rand0, (Argument) rand1) :
                 new PrimitiveIsObjectTypeQ (rator, rand0, rand1);
        }


        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            tcHistogram.Note (this.rand0Value);
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

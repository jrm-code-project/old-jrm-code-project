using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;

namespace Microcode
{
    [Serializable] 
    class PCondIsType<SType> : PCond1
    {
#if DEBUG
        static Histogram<Type> arg0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif

        protected PCondIsType (PrimitiveIsType<SType> predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsType<SType> predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsTypeA<SType>) ? PCondIsTypeA<SType>.Make ((PrimitiveIsTypeA<SType>) predicate, consequent, alternative) :
                (predicate is PrimitiveIsTypeCar<SType>) ? PCondIsTypeCar<SType>.Make ((PrimitiveIsTypeCar<SType>) predicate, consequent, alternative) :
                (predicate is PrimitiveIsTypeS<SType>) ? PCondIsTypeS<SType>.Make ((PrimitiveIsTypeS<SType>) predicate, consequent, alternative) :
                (consequent is Quotation) ? PCondIsTypeXQ<SType>.Make (predicate, (Quotation) consequent, alternative) :
                new PCondIsType<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            arg0TypeHistogram.Note (this.arg0Type);
            SCode.location = "PCondIsType";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveIsTypeFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }
#if DEBUG
            SCode.location = "PCondIsType";
#endif
            if (ev0 is SType) {
#if DEBUG
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
            else {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null;
                return true;
            } 
        }
    }

    [Serializable]
    class PCondIsTypeA<SType> : PCondIsType<SType>
    {
        public readonly int predicateOffset;
        protected PCondIsTypeA (PrimitiveIsTypeA<SType> predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.predicateOffset = predicate.offset;
        }

        public static SCode Make (PrimitiveIsTypeA<SType> predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsTypeA0<SType>) ? PCondIsTypeA0<SType>.Make ((PrimitiveIsTypeA0<SType>) predicate, consequent, alternative) :
                (predicate is PrimitiveIsTypeA1<SType>) ? PCondIsTypeA1<SType>.Make ((PrimitiveIsTypeA1<SType>) predicate, consequent, alternative) :
                new PCondIsTypeA<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsTypeA");
#endif
            expression = environment.ArgumentValue (this.predicateOffset) is SType
                ? this.consequent
                : this.alternative;
            answer = null;
#if DEBUG
            NoteCalls ((SCode) expression);
#endif
            return true;
        }
    }

    [Serializable]
    class PCondIsTypeA0<SType> : PCondIsTypeA<SType>
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsTypeA0 (PrimitiveIsTypeA0<SType> predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        internal static SCode Make (PrimitiveIsTypeA0<SType> predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCondIsTypeA0A<SType>.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? PCondIsTypeA0Q<SType>.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is Argument) ? PCondIsTypeA0XA<SType>.Make (predicate, consequent, (Argument) alternative) :
                (alternative is StaticVariable) ? PCondIsTypeA0XS<SType>.Make (predicate, consequent, (StaticVariable) alternative) :
                (alternative is Quotation) ? PCondIsTypeA0XQ<SType>.Make (predicate, consequent, (Quotation) alternative) :
               new PCondIsTypeA0<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            if (environment.Argument0Value is SType)
                consequentTypeHistogram.Note (this.consequentType);
            else
                alternativeTypeHistogram.Note (this.alternativeType);
            SCode.location = "PCondIsTypeA0";
#endif
            expression = (environment.Argument0Value is SType)
                ? this.consequent
                : this.alternative;
            answer = null;
#if DEBUG
            SCode.location = "-";
            NoteCalls ((SCode) expression);
#endif
            return true;
        }
    }

    [Serializable]
    class PCondIsTypeA0A<SType> : PCondIsTypeA0<SType>
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int consequentOffset;
        protected PCondIsTypeA0A (PrimitiveIsTypeA0<SType> predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        internal static SCode Make (PrimitiveIsTypeA0<SType> predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondIsTypeA0A0<SType>.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? PCondIsTypeA0A1<SType>.Make (predicate, (Argument1) consequent, alternative) :
               new PCondIsTypeA0A<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsTypeA0A");
#endif
            if (environment.Argument0Value is SType) {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
            else {
 #if DEBUG
            SCode.location = "-";
            NoteCalls ((SCode) this.alternative);
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsTypeA0A0<SType> : PCondIsTypeA0A<SType>
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsTypeA0A0 (PrimitiveIsTypeA0<SType> predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        internal static SCode Make (PrimitiveIsTypeA0<SType> predicate, Argument0 consequent, SCode alternative)
        {
            return
               new PCondIsTypeA0A0<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsTypeA0A0");
#endif
            object obj = environment.Argument0Value;

            if (obj is SType) {
                answer = obj;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls ((SCode) this.alternative);
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsTypeA0A1<SType> : PCondIsTypeA0A<SType>
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsTypeA0A1 (PrimitiveIsTypeA0<SType> predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        internal static SCode Make (PrimitiveIsTypeA0<SType> predicate, Argument1 consequent, SCode alternative)
        {
            return
               new PCondIsTypeA0A1<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsTypeA0A1");
#endif
            if (environment.Argument0Value is SType) {
                answer = environment.Argument1Value;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls ((SCode) this.alternative);
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsTypeA0Q<SType> : PCondIsTypeA0<SType>
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;
        protected PCondIsTypeA0Q (PrimitiveIsTypeA0<SType> predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        internal static SCode Make (PrimitiveIsTypeA0<SType> predicate, Quotation consequent, SCode alternative)
        {
            return
               new PCondIsTypeA0Q<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsTypeA0Q");
#endif
            if (environment.Argument0Value is SType) {
                answer = this.consequentValue;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls ((SCode) this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsTypeA0Q";
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsTypeA0XA<SType> : PCondIsTypeA0<SType>
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int alternativeOffset;
        protected PCondIsTypeA0XA (PrimitiveIsTypeA0<SType> predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        internal static SCode Make (PrimitiveIsTypeA0<SType> predicate, SCode consequent, Argument alternative)
        {
            return
               (alternative is Argument0) ? PCondIsTypeA0XA0<SType>.Make (predicate, consequent, (Argument0) alternative) :
               (alternative is Argument1) ? PCondIsTypeA0XA1<SType>.Make (predicate, consequent, (Argument1) alternative) :
               new PCondIsTypeA0XA<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsTypeA0XA");
#endif
            if (environment.Argument0Value is SType) {
#if DEBUG
                SCode.location = "-";
                NoteCalls ((SCode) this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsTypeA0XA";
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
            else {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
            }
        }
    }

    [Serializable]
    sealed class PCondIsTypeA0XA0<SType> : PCondIsTypeA0XA<SType>
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeOffset;
        PCondIsTypeA0XA0 (PrimitiveIsTypeA0<SType> predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        internal static SCode Make (PrimitiveIsTypeA0<SType> predicate, SCode consequent, Argument0 alternative)
        {
            return
               new PCondIsTypeA0XA0<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsTypeA0XA0");
#endif
            if (environment.Argument0Value is SType) {
#if DEBUG
                SCode.location = "-";
                NoteCalls ((SCode) this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsTypeA0XA";
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }
    [Serializable]
    sealed class PCondIsTypeA0XA1<SType> : PCondIsTypeA0XA<SType>
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeOffset;
        PCondIsTypeA0XA1 (PrimitiveIsTypeA0<SType> predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        internal static SCode Make (PrimitiveIsTypeA0<SType> predicate, SCode consequent, Argument1 alternative)
        {
            return
               new PCondIsTypeA0XA1<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsTypeA0XA1");
#endif
            if (environment.Argument0Value is SType) {
#if DEBUG
                SCode.location = "-";
                NoteCalls ((SCode) this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsTypeA0XA1";
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
            else {
                answer = environment.Argument1Value;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsTypeA0XS<SType> : PCondIsTypeA0<SType>
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol alternativeName;
        public readonly int alternativeOffset;
        protected PCondIsTypeA0XS (PrimitiveIsTypeA0<SType> predicate, SCode consequent, StaticVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeOffset = alternative.Offset;
        }

        internal static SCode Make (PrimitiveIsTypeA0<SType> predicate, SCode consequent, StaticVariable alternative)
        {
            return
               new PCondIsTypeA0XS<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsTypeA0XS");
#endif
            if (environment.Argument0Value is SType) {
#if DEBUG
                SCode.location = "-";
                NoteCalls ((SCode) this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsTypeA0XS";
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
            else {
                if (environment.StaticValue (out answer, this.alternativeName, this.alternativeOffset))
                    throw new NotImplementedException ();
                return false;
            }
        }
    }


    [Serializable]
    class PCondIsTypeA0XQ<SType> : PCondIsTypeA0<SType>
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCondIsTypeA0XQ (PrimitiveIsTypeA0<SType> predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        internal static SCode Make (PrimitiveIsTypeA0<SType> predicate, SCode consequent, Quotation alternative)
        {
            return
               new PCondIsTypeA0XQ<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsTypeA0XQ");
#endif
            if (environment.Argument0Value is SType) {
#if DEBUG
                SCode.location = "-";
                NoteCalls ((SCode) this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsTypeA0XQ";
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
            else {
                answer = this.alternativeValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsTypeA1<SType> : PCondIsTypeA<SType>
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type>();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type>();
#endif
        protected PCondIsTypeA1(PrimitiveIsTypeA1<SType> predicate, SCode consequent, SCode alternative)
            : base(predicate, consequent, alternative)
        {
        }

        internal static SCode Make(PrimitiveIsTypeA1<SType> predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is Quotation) ? PCondIsTypeA1Q<SType>.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is Quotation) ? PCondIsTypeA1XQ<SType>.Make (predicate,  consequent, (Quotation) alternative) :
                new PCondIsTypeA1<SType>(predicate, consequent, alternative);
        }

        public override bool EvalStep(out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("PCondIsTypeA1");
#endif
            if (environment.Argument1Value is SType)
            {
#if DEBUG
                SCode.location = "-";
                NoteCalls(this.consequent);
                consequentTypeHistogram.Note(this.consequentType);
                SCode.location = "PCondIsTypeA1";
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
            else
            {
#if DEBUG
                SCode.location = "-";
                NoteCalls(this.alternative);
                alternativeTypeHistogram.Note(this.alternativeType);
                SCode.location = "PCondIsTypeA1";
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsTypeA1Q<SType> : PCondIsTypeA1<SType>
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;
        protected PCondIsTypeA1Q (PrimitiveIsTypeA1<SType> predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        internal static SCode Make (PrimitiveIsTypeA1<SType> predicate, Quotation consequent, SCode alternative)
        {
            return
                new PCondIsTypeA1Q<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsTypeA1Q");
#endif
            if (environment.Argument1Value is SType) {
                answer = this.consequentValue;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsTypeA1Q";
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsTypeA1XQ<SType> : PCondIsTypeA1<SType>
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCondIsTypeA1XQ (PrimitiveIsTypeA1<SType> predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        internal static SCode Make (PrimitiveIsTypeA1<SType> predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondIsTypeA1XQ<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsTypeA1XQ");
#endif
            if (environment.Argument1Value is SType) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsTypeA1XQ";
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
            else {
                answer = this.alternativeValue;
                return false;
            } 
        }
    }

    [Serializable]
    class PCondIsTypeCar<SType> : PCondIsType<SType>
    {
#if DEBUG
        static Histogram<Type> arg0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
        public Type operandType;
#endif
        public readonly SCode operand;
        protected PCondIsTypeCar (PrimitiveIsTypeCar<SType> predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.operand = predicate.randArg;
#if DEBUG
            this.operandType = this.operand.GetType ();
#endif
        }

        public static SCode Make (PrimitiveIsTypeCar<SType> predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsTypeCarA<SType>) ? PCondIsTypeCarA<SType>.Make ((PrimitiveIsTypeCarA<SType>) predicate, consequent, alternative) :
                (predicate is PrimitiveIsTypeCarS<SType>) ? PCondIsTypeCarS<SType>.Make ((PrimitiveIsTypeCarS<SType>) predicate, consequent, alternative) :
                new PCondIsTypeCar<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.operand);
            arg0TypeHistogram.Note (this.operandType);
            SCode.location = "PCondIsTypeCar";
#endif
            Control unev0 = this.operand;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsTypeCar";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveIsTypeFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }

            if (((Cons)ev0).Car is SType) {
#if DEBUG
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
            else {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null;
                return true;
            } 
        }
    }

    [Serializable]
    class PCondIsTypeCarA<SType> : PCondIsTypeCar<SType>
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int predicateOffset;
        protected PCondIsTypeCarA (PrimitiveIsTypeCarA<SType> predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.predicateOffset = predicate.offset;
        }

        public static SCode Make (PrimitiveIsTypeCarA<SType> predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsTypeCarA0<SType>) ? PCondIsTypeCarA0<SType>.Make ((PrimitiveIsTypeCarA0<SType>) predicate, consequent, alternative) :
                (predicate is PrimitiveIsTypeCarA1<SType>) ? PCondIsTypeCarA1<SType>.Make ((PrimitiveIsTypeCarA1<SType>) predicate, consequent, alternative) :
                new PCondIsTypeCarA<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsTypeCarA");
#endif
            object ev0 = environment.ArgumentValue (this.predicateOffset);

            if (((Cons) ev0).Car is SType) {
#if DEBUG
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
            else {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsTypeCarA0<SType> : PCondIsTypeCarA<SType>
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsTypeCarA0 (PrimitiveIsTypeCarA0<SType> predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsTypeCarA0<SType> predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is Quotation) ? PCondIsTypeCarA0Q<SType>.Make (predicate, (Quotation) consequent, alternative) :
                new PCondIsTypeCarA0<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsTypeCarA0");
#endif
            object ev0 = environment.Argument0Value;

            if (((Cons) ev0).Car is SType) {
#if DEBUG
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
            else {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsTypeCarA0Q<SType> : PCondIsTypeCarA0<SType>
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;

        protected PCondIsTypeCarA0Q (PrimitiveIsTypeCarA0<SType> predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveIsTypeCarA0<SType> predicate, Quotation consequent, SCode alternative)
        {
            return
                new PCondIsTypeCarA0Q<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsTypeCarA0Q");
#endif
            object ev0 = environment.Argument0Value;

            if (((Cons) ev0).Car is SType) {
                answer = this.consequent;
                return false;
            }
            else {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsTypeCarA1<SType> : PCondIsTypeCarA<SType>
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsTypeCarA1 (PrimitiveIsTypeCarA1<SType> predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsTypeCarA1<SType> predicate, SCode consequent, SCode alternative)
        {
            return
                new PCondIsTypeCarA1<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsTypeCarA1");
#endif
            object ev0 = environment.Argument1Value;

            if (((Cons) ev0).Car is SType) {
#if DEBUG
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
            else {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsTypeCarS<SType> : PCondIsTypeCar<SType>
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol predicateName;
        public readonly int predicateOffset;
        protected PCondIsTypeCarS (PrimitiveIsTypeCarS<SType> predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.predicateName = predicate.name;
            this.predicateOffset = predicate.offset;
        }

        public static SCode Make (PrimitiveIsTypeCarS<SType> predicate, SCode consequent, SCode alternative)
        {
            return
                new PCondIsTypeCarS<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsTypeCarS");
#endif
            object ev0;
            if (environment.StaticValue (out ev0, this.predicateName, this.predicateOffset))
                throw new NotImplementedException();

            if (((Cons) ev0).Car is SType) {
#if DEBUG
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
            else {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }
    }

//    [Serializable]
//    class PCondIsTypeA0A : PCondIsTypeA0
//    {
//        protected PCondIsTypeA0A (PrimitiveIsTypeA0 predicate, Argument consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveIsTypeA0 predicate, Argument consequent, SCode alternative)
//        {
//            return
//                (consequent is Argument0) ? PCondIsTypeA0A0.Make (predicate, (Argument0) consequent, alternative) :
//                (consequent is Argument1) ? Unimplemented() :
//                (alternative is LexicalVariable) ? Unimplemented() :
//                (alternative is Quotation) ? Unimplemented() :
//                new PCondIsTypeA0A (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ();
//#endif
//            object ev0 = environment.Argument0Value;


//            if (!(ev0 is Cons)) {
//#if DEBUG
//                noteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                answer = null;

//                return true;
//            }
//            else {
//                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    class PCondIsTypeA0A0 : PCondIsTypeA0A
//    {
//        protected PCondIsTypeA0A0 (PrimitiveIsTypeA0 predicate, Argument0 consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveIsTypeA0 predicate, Argument0 consequent, SCode alternative)
//        {
//            return
//                (alternative is LexicalVariable) ? Unimplemented () :
//                (alternative is Quotation) ? PCondIsTypeA0A0Q.Make (predicate, consequent, (Quotation) alternative) :
//                new PCondIsTypeA0A0 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("PCondIsTypeA0A0.EvalStep");
//#endif
//            object ev0 = environment.Argument0Value;
//            if (ev0 is Cons) {
//                answer = ev0;
//                return false;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                answer = null;
//                return true;
//            }
//        }
//    }

//    [Serializable]
//    sealed class PCondIsTypeA0A0Q : PCondIsTypeA0A0
//    {
//        readonly object alternativeValue;

//        PCondIsTypeA0A0Q (PrimitiveIsTypeA0 predicate, Argument0 consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }

//        internal static SCode Make (PrimitiveIsTypeA0 predicate, Argument0 consequent, Quotation alternative)
//        {
//            return
//                new PCondIsTypeA0A0Q (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("PCondIsTypeA0A0Q.EvalStep");
//#endif
//            object ev0 = environment.Argument0Value;
//            answer = (ev0 is Cons) ? ev0 : this.alternativeValue;
//            return false;
//        }
//    }

//    [Serializable]
//    sealed class PCondIsTypeA0QQ : PCondIsTypeA0Q
//    {
//        PCondIsTypeA0QQ (PrimitiveIsTypeA0 predicate, Quotation consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        { }
//        internal static SCode Make (PrimitiveIsTypeA0 predicate, Quotation consequent, Quotation alternative)
//        {
//            if (consequent.Quoted == alternative.Quoted) {
//                Debug.WriteLine ("; Optimize (if <expr> <literal> <literal>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, consequent);
//            }
//            else if (Configuration.EnableTrueUnspecific && consequent.Quoted == Constant.Unspecific) {
//                Debug.WriteLine ("; Optimize (if <expr> <unspecific> <literal>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, alternative);
//            }
//            else if (Configuration.EnableTrueUnspecific && alternative.Quoted == Constant.Unspecific) {
//                Debug.WriteLine ("; Optimize (if <expr> <literal> <unspecific>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, consequent);
//            }
//            return new PCondIsTypeA0QQ (predicate, consequent, alternative);
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException ();
//        }


//    }

//    [Serializable]
//    class PCondIsTypeA0SA : PCondIsTypeA0S
//    {
//        protected PCondIsTypeA0SA (PrimitiveIsTypeA0 predicate, SCode consequent, Argument alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveIsTypeA0 predicate, SCode consequent, Argument alternative)
//        {
//            return
//                (alternative is Argument0) ? PCondIsTypeA0SA0.Make (predicate, consequent, (Argument0) alternative) :
//                (alternative is Argument1) ? PCondIsTypeA0SA1.Make (predicate, consequent, (Argument1) alternative) :
//                new PCondIsTypeA0SA (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("PCondIsTypeA0SA.EvalStep");
//#endif
//            throw new NotImplementedException ();
//            object ev0 = environment.Argument0Value;

//            if (!(ev0 is Cons)) {
//                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.consequent);

//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            }


//        }


//    }

//    [Serializable]
//    sealed class PCondIsTypeA0SA0 : PCondIsTypeA0SA
//    {
//        PCondIsTypeA0SA0 (PrimitiveIsTypeA0 predicate, SCode consequent, Argument0 alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveIsTypeA0 predicate, SCode consequent, Argument0 alternative)
//        {
//            return
//                new PCondIsTypeA0SA0 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("PCondIsTypeA0SA1.EvalStep");
//#endif
//            object temp = environment.Argument0Value;
//            if (temp is Cons) {
//#if DEBUG
//                noteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            }
//            else {
//                answer = temp;
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    sealed class PCondIsTypeA0SA1 : PCondIsTypeA0SA
//    {
//        PCondIsTypeA0SA1 (PrimitiveIsTypeA0 predicate, SCode consequent, Argument1 alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveIsTypeA0 predicate, SCode consequent, Argument1 alternative)
//        {
//            return
//                new PCondIsTypeA0SA1 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("PCondIsTypeA0SA1.EvalStep");
//#endif
//            if (environment.Argument0Value is Cons) {
//#if DEBUG
//                noteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            }
//            else {
//                answer = environment.Argument1Value;
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    sealed class PCondIsTypeA0SQ : PCondIsTypeA0
//    {
//#if DEBUG
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type>();
//#endif
//        public readonly object alternativeValue;

//        PCondIsTypeA0SQ (PrimitiveIsTypeA0 predicate, SCode consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }

//        internal static SCode Make (PrimitiveIsTypeA0 predicate, SCode consequent, Quotation alternative)
//        {
//            return new PCondIsTypeA0SQ (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("PCondIsTypeA0SQ.EvalStep");
//#endif
//            if (environment.Argument0Value is Cons) {
//#if DEBUG
//                SCode.location = "-";
//                noteCalls (this.consequent);
//                consequentTypeHistogram.Note (this.consequentType);
//                SCode.location = "PCondIsTypeA0SQ.EvalStep";
//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            }
//            else {
//                answer = this.alternativeValue;
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    class PCondIsTypeA1Q : PCondIsTypeA1
//    {
//        public readonly object consequentValue;
//        protected PCondIsTypeA1Q (PrimitiveIsTypeA1 predicate, Quotation consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.consequentValue = consequent.Quoted;
//        }
//        internal static SCode Make (PrimitiveIsTypeA1 predicate, Quotation consequent, SCode alternative)
//        {
//            return
//                (alternative is LexicalVariable) ? PCondIsTypeA1QL.Make (predicate, consequent, (LexicalVariable) alternative) :
//                (alternative is Quotation) ? PCondIsTypeA1QQ.Make (predicate, consequent, (Quotation) alternative) :
//                new PCondIsTypeA1Q (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("PCondIsTypeA1Q.EvalStep");
//#endif
//            object ev0 = environment.Argument1Value;

//            if (ev0 is Cons) {
//                answer = this.consequentValue;
//                return false;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                answer = null;
//                return true;
//            }  
//        }
//    }

//    [Serializable]
//    class PCondIsTypeA1QQ : PCondIsTypeA1Q
//    {
//        protected PCondIsTypeA1QQ (PrimitiveIsTypeA1 predicate, Quotation consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        { }
//        internal static SCode Make (PrimitiveIsTypeA1 predicate, Quotation consequent, Quotation alternative)
//        {
//            if (consequent.Quoted == alternative.Quoted) {
//                Debug.WriteLine ("; Optimize (if <expr> <literal> <literal>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, consequent);
//            }
//            else if (Configuration.EnableTrueUnspecific && consequent.Quoted == Constant.Unspecific) {
//                Debug.WriteLine ("; Optimize (if <expr> <unspecific> <literal>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, alternative);
//            }
//            else if (Configuration.EnableTrueUnspecific && alternative.Quoted == Constant.Unspecific) {
//                Debug.WriteLine ("; Optimize (if <expr> <literal> <unspecific>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, consequent);
//            }
//            throw new NotImplementedException ();
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException ();
//        }


//    }


//    [Serializable]
//    class PCondIsTypeA1SA : PCondIsTypeA1S
//    {

//        protected PCondIsTypeA1SA (PrimitiveIsTypeA1 predicate, SCode consequent, Argument alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }
//        internal static SCode Make (PrimitiveIsTypeA1 predicate, SCode consequent, Argument alternative)
//        {
//            return
//                (alternative is Argument0) ? PCondIsTypeA1SA0.Make (predicate, consequent, (Argument0) alternative) :
//                (alternative is Argument1) ? Unimplemented () :
//                new PCondIsTypeA1SA (predicate, consequent, alternative);

//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            Unimplemented ();
//#if DEBUG
//            Warm ("PCondIsTypeA1SL.EvalStep");
//#endif
//            object ev0 = environment.Argument1Value;

//            if (!(ev0 is Cons)) {
//                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.consequent);

//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            }

//        }


//    }

//    [Serializable]
//    sealed class PCondIsTypeA1SA0 : PCondIsTypeA1SA
//    {
//#if DEBUG
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type>();
//#endif

//        PCondIsTypeA1SA0 (PrimitiveIsTypeA1 predicate, SCode consequent, Argument0 alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveIsTypeA1 predicate, SCode consequent, Argument0 alternative)
//        {
//            return
//                new PCondIsTypeA1SA0 (predicate, consequent, alternative);

//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("PCondIsTypeA1SA0.EvalStep");
//#endif
//            object ev0 = environment.Argument1Value;

//            if (ev0 is Cons) {
//#if DEBUG
//                noteCalls (this.consequent);
//                consequentTypeHistogram.Note (this.consequentType);
//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            }
//            else {
//                answer = environment.Argument0Value;
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    class PCondIsTypeA1SQ : PCondIsTypeA1
//    {
//        public readonly object alternativeValue;

//        protected PCondIsTypeA1SQ (PrimitiveIsTypeA1 predicate, SCode consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }

//        internal static SCode Make (PrimitiveIsTypeA1 predicate, SCode consequent, Quotation alternative)
//        {
//            return new PCondIsTypeA1SQ (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("PCondIsTypeA1SQ.EvalStep");
//#endif
//            object ev0 = environment.Argument1Value;


//            if (!(ev0 is Cons)) {

//                answer = this.alternativeValue;
//                return false;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            }
//        }
//    }

//    [Serializable]
//    class PCondIsTypeAA : PCondIsTypeA
//    {
//        protected PCondIsTypeAA (PrimitiveIsTypeA predicate, Argument consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        { }

//        static public SCode Make (PrimitiveIsTypeA predicate, Argument consequent, SCode alternative)
//        {
//            throw new NotImplementedException ();
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException ();
//        }

//    }

//    [Serializable]
//    class PCondIsTypeAQ : PCondIsTypeA
//    {
//        public readonly object consequentValue;

//        protected PCondIsTypeAQ (PrimitiveIsTypeA predicate, Quotation consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.consequentValue = consequent.Quoted;
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            #region EvalStepBody
//#if DEBUG
//            Warm ();
//            noteCalls (this.arg0);
//#endif
//            object ev0 = environment.ArgumentValue (this.predicateOffset);


//            if (!(ev0 is Cons)) {
//#if DEBUG
//                noteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                answer = null;
//                return true;
//            }
//            else {
//                answer = this.consequentValue;
//                return false;
//            }
//            #endregion

//        }

//        internal static SCode Make (PrimitiveIsTypeA predicate, Quotation consequent, SCode alternative)
//        {
//            return
//                (alternative is LexicalVariable) ? PCondIsTypeAQL.Make (predicate, consequent, (LexicalVariable) alternative)
//                : (alternative is Quotation) ? PCondIsTypeAQQ.Make (predicate, consequent, (Quotation) alternative)
//                : new PCondIsTypeAQ (predicate, consequent, alternative);
//        }
//    }

//    [Serializable]
//    class PCondIsTypeAQQ : PCondIsTypeAQ
//    {
//        protected PCondIsTypeAQQ (PrimitiveIsTypeA predicate, Quotation consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {

//            throw new NotImplementedException ();
//        }

//        internal static SCode Make (PrimitiveIsTypeA predicate, Quotation consequent, Quotation alternative)
//        {
//            if (consequent.Quoted == alternative.Quoted) {
//                Debug.WriteLine ("; Optimize (if <expr> <literal> <literal>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, consequent);
//            }
//            else if (Configuration.EnableTrueUnspecific && consequent.Quoted == Constant.Unspecific) {
//                Debug.WriteLine ("; Optimize (if <expr> <unspecific> <literal>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, alternative);
//            }
//            else if (Configuration.EnableTrueUnspecific && alternative.Quoted == Constant.Unspecific) {
//                Debug.WriteLine ("; Optimize (if <expr> <literal> <unspecific>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, consequent);
//            }
//            return new PCondIsTypeAQQ (predicate, consequent, alternative);
//        }
//    }

//    [Serializable]
//    class PCondIsTypeASQ : PCondIsTypeA
//    {
//        public readonly object alternativeValue;

//        protected PCondIsTypeASQ (PrimitiveIsTypeA predicate, SCode consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("PCondIsTypeASQ.EvalStep");
//#endif
//            object ev0 = environment.ArgumentValue (predicateOffset);


//            if (!(ev0 is Cons)) {

//                answer = this.alternativeValue;
//                return false;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            }
//        }

//        internal static PCondIsTypeA Make (PrimitiveIsTypeA predicate, SCode consequent, Quotation alternative)
//        {
//            return new PCondIsTypeASQ (predicate, consequent, alternative);
//        }
//    }

    [Serializable]
    class PCondIsTypeS<SType> : PCondIsType<SType>
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol predicateName;
        public readonly int predicateOffset;
        protected PCondIsTypeS (PrimitiveIsTypeS<SType> predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.predicateName = predicate.varname;
            this.predicateOffset = predicate.offset;
        }

        public static SCode Make (PrimitiveIsTypeS<SType> predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is Quotation) ? PCondIsTypeSQ<SType>.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is Quotation) ? PCondIsTypeSXQ<SType>.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsTypeS<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsTypeS");
#endif
            object evpred;
            if (environment.StaticValue (out evpred, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();

            expression = evpred is SType
                ? this.consequent
                : this.alternative;
            answer = null;
#if DEBUG
            if (evpred is SType)
                consequentTypeHistogram.Note (this.consequentType);
            else
                alternativeTypeHistogram.Note (this.alternativeType);
            NoteCalls ((SCode) expression);
#endif
            return true;
        }
    }

    [Serializable]
    class PCondIsTypeSQ<SType> : PCondIsTypeS<SType>
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;

        protected PCondIsTypeSQ (PrimitiveIsTypeS<SType> predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveIsTypeS<SType> predicate, Quotation consequent, SCode alternative)
        {
            return
                new PCondIsTypeSQ<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsTypeSQ");
#endif
            object evpred;
            if (environment.StaticValue (out evpred, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();

            if (evpred is SType) {
                answer = this.consequentValue;
                return false;
            }
            else {
#if DEBUG
                alternativeTypeHistogram.Note (this.alternativeType);
                NoteCalls (this.alternative);
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsTypeSXQ<SType> : PCondIsTypeS<SType>
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;

        protected PCondIsTypeSXQ (PrimitiveIsTypeS<SType> predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveIsTypeS<SType> predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondIsTypeSXQ<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsTypeSXQ");
#endif
            object evpred;
            if (environment.StaticValue (out evpred, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();

            if (evpred is SType)  {
#if DEBUG
                SCode.location = "-";
                consequentTypeHistogram.Note (this.consequentType);
                NoteCalls (this.consequent);
                SCode.location = "PCondIsTypeSXQ";
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
            else {
                answer = this.alternativeValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsTypeXQ<SType> : PCondIsType<SType>
    {
#if DEBUG
        static Histogram<Type> arg0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;

        protected PCondIsTypeXQ (PrimitiveIsType<SType> predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveIsType<SType> predicate, Quotation consequent, SCode alternative)
        {
            return
                new PCondIsTypeXQ<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            arg0TypeHistogram.Note (this.arg0Type);
            SCode.location = "PCondIsTypeXQ";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveIsTypeFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }
#if DEBUG
            SCode.location = "PCondIsTypeXQ";
#endif
            if (ev0 is SType) {
                answer = this.consequentValue;
                return false;
            }
            else {
#if DEBUG
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }

    }


//    [Serializable]
//    class PCondIsTypeSQQ : PCondIsTypeSQ
//    {
//        public readonly object alternativeValue;

//        protected PCondIsTypeSQQ (PrimitiveIsType predicate, Quotation consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }


//        public static SCode Make (PrimitiveIsType predicate, Quotation consequent, Quotation alternative)
//        {
//            if (consequent.Quoted == alternative.Quoted) {
//                Debug.WriteLine ("; Optimize (if <expr> <literal> <literal>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, consequent);
//            }
//            else if (Configuration.EnableTrueUnspecific && consequent.Quoted == Constant.Unspecific) {
//                Debug.WriteLine ("; Optimize (if <expr> <unspecific> <literal>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, alternative);
//            }
//            else if (Configuration.EnableTrueUnspecific && alternative.Quoted == Constant.Unspecific) {
//                Debug.WriteLine ("; Optimize (if <expr> <literal> <unspecific>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, consequent);
//            }

//            return new PCondIsTypeSQQ (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            #region EvalStepBody
//#if DEBUG
//            Warm ();
//            noteCalls (this.arg0);
//#endif
//            Control unev0 = this.arg0;
//            Environment env = environment;
//            object ev0;
//            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
//            if (ev0 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveIsTypeFrame0 (this, environment));
//                //answer = Interpreter.UnwindStack;
//                //environment = env;
//                //return false;
//            }


//            if (!(ev0 is Cons)) {
//                answer = this.alternativeValue;
//                return false;
//            }
//            else {
//                answer = this.consequentValue;
//                return false;
//            }
//            #endregion
//        }
//    }



//    [Serializable] class PCondIsTypeSSA : PCondIsTypeSSL
//    {

//        protected PCondIsTypeSSA (PrimitiveIsType predicate, SCode consequent, Argument alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (PrimitiveIsType predicate, SCode consequent, Argument alternative)
//        {
//            return
//                (alternative is Argument0) ? PCondIsTypeSSA0.Make (predicate, consequent, (Argument0) alternative) :
//                (alternative is Argument1) ? Unimplemented () :
//                new PCondIsTypeSSA (predicate, consequent, alternative);
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            Unimplemented ();
//#if DEBUG
//            Warm ("PCondIsTypeSSL.EvalStep");
//            noteCalls (this.arg0);
//#endif
//            Control unev0 = this.arg0;
//            Environment env = environment;
//            object ev0;
//            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
//            if (ev0 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveIsTypeFrame0 (this, environment));
//                //answer = Interpreter.UnwindStack;
//                //environment = env;
//                //return false;
//            }



//            if (!(ev0 is Cons)) {
//                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            }
//        }
//    }

//    [Serializable]
//    sealed class PCondIsTypeSSA0 : PCondIsTypeSSA
//    {

//        PCondIsTypeSSA0 (PrimitiveIsType predicate, SCode consequent, Argument0 alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (PrimitiveIsType predicate, SCode consequent, Argument0 alternative)
//        {
//            return
//                new PCondIsTypeSSA0 (predicate, consequent, alternative);
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            noteCalls (this.arg0);
//            SCode.location = "PCondIsTypeSSA0.EvalStep";
//#endif
//            Control unev0 = this.arg0;
//            Environment env = environment;
//            object ev0;
//            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
//            if (ev0 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveIsTypeFrame0 (this, environment));
//                //answer = Interpreter.UnwindStack;
//                //environment = env;
//                //return false;
//            }

//            if (ev0 is Cons) {
//#if DEBUG
//                noteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            }
//            else {
//                answer = environment.Argument0Value;
//                return false;
//            }
//        }
//    }


//    [Serializable]
//    sealed class PCondIsTypeSSQ : PCondIsType
//    {
//        public readonly object alternativeValue;

//        PCondIsTypeSSQ (PrimitiveIsType predicate, SCode consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }

//        public static SCode Make (PrimitiveIsType predicate, SCode consequent, Quotation alternative)
//        {
//            return new PCondIsTypeSSQ (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("PCondIsTypeSSQ.EvalStep");
//            noteCalls (this.arg0);
//#endif
//            Control unev0 = this.arg0;
//            Environment env = environment;
//            object ev0;
//            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
//            if (ev0 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveIsTypeFrame0 (this, environment));
//                //answer = Interpreter.UnwindStack;
//                //environment = env;
//                //return false;
//            }

//            if (ev0 is Cons) {
//#if DEBUG
//                noteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            }
//            else {
//                answer = this.alternativeValue;
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    class PCondIsTypeCar : PCondIsType
//    {
//#if DEBUG
//        static Histogram<Type> arg0TypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif

//        protected PCondIsTypeCar (PrimitiveIsTypeCar predicate, SCode consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (PrimitiveIsTypeCar predicate, SCode consequent, SCode alternative)
//        {
//            return
//                //(predicate is PrimitiveIsTypeCaar) ? PCondIsTypeCarCar.Make ((PrimitiveIsTypeCarCar) predicate, consequent, alternative) :
//                (predicate is PrimitiveIsTypeCarL) ? PCondIsTypeCarL.Make ((PrimitiveIsTypeCarL) predicate, consequent, alternative) :
//                (consequent is LexicalVariable) ? PCondIsTypeCarSL.Make (predicate, (LexicalVariable) consequent, alternative) :
//                (consequent is Quotation) ? PCondIsTypeCarSQ.Make (predicate, (Quotation) consequent, alternative) :
//                 (alternative is LexicalVariable) ? PCondIsTypeCarSSL.Make (predicate, consequent, (LexicalVariable) alternative) :
//                 (alternative is Quotation) ? PCondIsTypeCarSSQ.Make (predicate, consequent, (Quotation) alternative) :
//                 new PCondIsTypeCar (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException();
//#if DEBUG
//            Warm ("PCondIsTypeCar.EvalStep");
//            noteCalls (this.arg0);
//            arg0TypeHistogram.Note (this.arg0Type);
//#endif
//            Control unev0 = this.arg0;
//            Environment env = environment;
//            object ev0;
//            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
//            if (ev0 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveIsTypeCarFrame0 (this, environment));
//                //answer = Interpreter.UnwindStack;
//                //environment = env;
//                //return false;
//            }


//            if (!(ev0 is Cons)) {
//#if DEBUG
//                noteCalls (this.alternative);
//                alternativeTypeHistogram.Note (this.alternativeType);
//#endif
//                expression = this.alternative;
//                answer = null;
//                return true;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.consequent);
//                consequentTypeHistogram.Note (this.consequentType);
//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            }
//        }
//    }



//    [Serializable] class PCondIsTypeCarA : PCondIsTypeCarL
//    {
//        protected PCondIsTypeCarA (PrimitiveIsTypeCarA predicate, SCode consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (PrimitiveIsTypeCarA predicate, SCode consequent, SCode alternative)
//        {
//            return
//                (predicate is PrimitiveIsTypeCarA0) ? PCondIsTypeCarA0.Make ((PrimitiveIsTypeCarA0) predicate, consequent, alternative)
//                : (predicate is PrimitiveIsTypeCarA1) ? PCondIsTypeCarA1.Make ((PrimitiveIsTypeCarA1) predicate, consequent, alternative)
//                : (consequent is LexicalVariable) ? PCondIsTypeCarAL.Make (predicate, (LexicalVariable) consequent, alternative)
//                : (consequent is Quotation) ? PCondIsTypeCarAQ.Make (predicate, (Quotation) consequent, alternative)
//                : (alternative is LexicalVariable) ? PCondIsTypeCarASL.Make (predicate, consequent, (LexicalVariable) alternative)
//                : (alternative is Quotation) ? PCondIsTypeCarASQ.Make (predicate, consequent, (Quotation) alternative)
//                : new PCondIsTypeCarA (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException();
//#if DEBUG
//            Warm ("PCondIsTypeCarA.EvalStep");
//#endif
//            expression = environment.ArgumentValue (this.predicateOffset) is Cons
//                ? this.consequent
//                : this.alternative;
//            answer = null;
//#if DEBUG
//            noteCalls ((SCode) expression);
//#endif
//            return true;
//        }
//    }

//    [Serializable] class PCondIsTypeCarA0 : PCondIsTypeCarA
//    {
//#if DEBUG
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type>();
//        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type>();
//#endif
//        protected PCondIsTypeCarA0 (PrimitiveIsTypeCarA0 predicate, SCode consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveIsTypeCarA0 predicate, SCode consequent, SCode alternative)
//        {
//            return
//                (consequent is LexicalVariable) ? PCondIsTypeCarA0L.Make (predicate, (LexicalVariable) consequent, alternative) :
//                //(consequent is SimpleLet1CarA0) ? PCondIsTypeCarA0SimpleLet1CarA0.Make (predicate, (SimpleLet1CarA0) consequent, alternative) :
//                //(consequent is PCondIsEqCarA0LA0) ? PCondIsTypeCarA0Fragment6.Make (predicate, (PCondIsEqCarA0LA0) consequent, alternative) :
//                //: (consequent is SComb1Fragment3) ? PCondIsTypeCarFragment4.Make (predicate, (SComb1Fragment3) consequent, alternative) :
//                (consequent is Quotation) ? PCondIsTypeCarA0Q.Make (predicate, (Quotation) consequent, alternative) :
//                (alternative is LexicalVariable) ? PCondIsTypeCarA0SL.Make (predicate, consequent, (LexicalVariable) alternative) :
//                (alternative is Quotation) ? PCondIsTypeCarA0SQ.Make (predicate, consequent, (Quotation) alternative) :
//               new PCondIsTypeCarA0 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("-");
//            SCode.location = "PCondIsTypeCarA0.EvalStep";
//#endif
//            Cons temp = environment.Argument0Value as Cons;
//            if (temp == null) throw new NotImplementedException ();
//            if (temp.Car is Cons) {
//                expression = this.consequent;
//#if DEBUG
//                SCode.location = "-";
//                consequentTypeHistogram.Note (this.consequentType);
//                noteCalls (this.consequent);
//#endif
//            }
//            else {
//                expression = this.alternative;
//#if DEBUG
//                SCode.location = "-";
//                alternativeTypeHistogram.Note (this.alternativeType);
//                noteCalls (this.alternative);
//#endif
//            }
//            answer = null;
//            return true;
//        }
//    }



//    [Serializable] class PCondIsTypeCarA0A : PCondIsTypeCarA0L
//    {
//        protected PCondIsTypeCarA0A (PrimitiveIsTypeCarA0 predicate, Argument consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveIsTypeCarA0 predicate, Argument consequent, SCode alternative)
//        {
//            return
//                (consequent is Argument0) ? PCondIsTypeCarA0A0.Make (predicate, (Argument0) consequent, alternative) :
//                (consequent is Argument1) ? Unimplemented() :
//                (alternative is LexicalVariable) ? Unimplemented() :
//                (alternative is Quotation) ? Unimplemented() :
//                new PCondIsTypeCarA0A (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException();
//#if DEBUG
//            Warm ();
//#endif
//            object ev0 = environment.Argument0Value;


//            if (!(ev0 is Cons)) {
//#if DEBUG
//                noteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                answer = null;

//                return true;
//            }
//            else {
//                if (environment.FastLexicalRef (out answer, this.consequentName, this.consequentDepth, this.consequentOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//        }
//    }

//    [Serializable] class PCondIsTypeCarA0A0 : PCondIsTypeCarA0A
//    {
//        protected PCondIsTypeCarA0A0 (PrimitiveIsTypeCarA0 predicate, Argument0 consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveIsTypeCarA0 predicate, Argument0 consequent, SCode alternative)
//        {
//            return
//                (alternative is LexicalVariable) ? Unimplemented () :
//                (alternative is Quotation) ? PCondIsTypeCarA0A0Q.Make (predicate, consequent, (Quotation) alternative) :
//                new PCondIsTypeCarA0A0 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException();
//#if DEBUG
//            Warm ("PCondIsTypeCarA0A0.EvalStep");
//#endif
//            object ev0 = environment.Argument0Value;
//            if (ev0 is Cons) {
//                answer = ev0;
//                return false;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                answer = null;
//                return true;
//            }
//        }
//    }

//    [Serializable]
//    sealed class PCondIsTypeCarA0A0Q : PCondIsTypeCarA0A0
//    {
//        readonly object alternativeValue;

//        PCondIsTypeCarA0A0Q (PrimitiveIsTypeCarA0 predicate, Argument0 consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }

//        internal static SCode Make (PrimitiveIsTypeCarA0 predicate, Argument0 consequent, Quotation alternative)
//        {
//            return
//                new PCondIsTypeCarA0A0Q (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException();
//#if DEBUG
//            Warm ("PCondIsTypeCarA0A0Q.EvalStep");
//#endif
//            object ev0 = environment.Argument0Value;
//            answer = (ev0 is Cons) ? ev0 : this.alternativeValue;
//            return false;
//        }
//    }



//    [Serializable] class PCondIsTypeCarA0Q : PCondIsTypeCarA0
//    {
//        public readonly object consequentValue;

//        protected PCondIsTypeCarA0Q (PrimitiveIsTypeCarA0 predicate, Quotation consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.consequentValue = consequent.Quoted;
//        }

//        internal static SCode Make (PrimitiveIsTypeCarA0 predicate, Quotation consequent, SCode alternative)
//        {
//            return
//                (alternative is LexicalVariable) ? PCondIsTypeCarA0QL.Make (predicate, consequent, (LexicalVariable) alternative) :
//                (alternative is Quotation) ? PCondIsTypeCarA0QQ.Make (predicate, consequent, (Quotation) alternative) :
//                new PCondIsTypeCarA0Q (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("PCondIsTypeCarA0Q.EvalStep");
//#endif
//            Cons temp = environment.Argument0Value as Cons;
//            if (temp == null) throw new NotImplementedException ();
//            if (temp.Car is Cons) {
//                answer = this.consequentValue;
//                return false;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                answer = null;
//                return true;
//            }
//        }
//    }


//    [Serializable] class PCondIsTypeCarA0QQ : PCondIsTypeCarA0Q
//    {
//        protected PCondIsTypeCarA0QQ (PrimitiveIsTypeCarA0 predicate, Quotation consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        { }
//        internal static SCode Make (PrimitiveIsTypeCarA0 predicate, Quotation consequent, Quotation alternative)
//        {
//            if (consequent.Quoted == alternative.Quoted) {
//                Debug.WriteLine ("; Optimize (if <expr> <literal> <literal>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, consequent);
//            }
//            else if (Configuration.EnableTrueUnspecific && consequent.Quoted == Constant.Unspecific) {
//                Debug.WriteLine ("; Optimize (if <expr> <unspecific> <literal>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, alternative);
//            }
//            else if (Configuration.EnableTrueUnspecific && alternative.Quoted == Constant.Unspecific) {
//                Debug.WriteLine ("; Optimize (if <expr> <literal> <unspecific>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, consequent);
//            }
//            return new PCondIsTypeCarA0QQ (predicate, consequent, alternative);
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException ();
//        }


//    }



//    [Serializable] class PCondIsTypeCarA0SA : PCondIsTypeCarA0SL
//    {
//        protected PCondIsTypeCarA0SA (PrimitiveIsTypeCarA0 predicate, SCode consequent, Argument alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveIsTypeCarA0 predicate, SCode consequent, Argument alternative)
//        {
//            return
//                (alternative is Argument0) ? PCondIsTypeCarA0SA0.Make (predicate, consequent, (Argument0) alternative) :
//                (alternative is Argument1) ? PCondIsTypeCarA0SA1.Make (predicate, consequent, (Argument1) alternative) :
//                new PCondIsTypeCarA0SA (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException();

//#if DEBUG
//            Warm ("PCondIsTypeCarA0SA.EvalStep");
//#endif
//            throw new NotImplementedException ();
//            object ev0 = environment.Argument0Value;

//            if (!(ev0 is Cons)) {
//                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.consequent);

//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            }


//        }


//    }

//    [Serializable]
//    sealed class PCondIsTypeCarA0SA0 : PCondIsTypeCarA0SA
//    {
//        PCondIsTypeCarA0SA0 (PrimitiveIsTypeCarA0 predicate, SCode consequent, Argument0 alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveIsTypeCarA0 predicate, SCode consequent, Argument0 alternative)
//        {
//            return
//                new PCondIsTypeCarA0SA0 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException();

//#if DEBUG
//            Warm ("PCondIsTypeCarA0SA1.EvalStep");
//#endif
//            object temp = environment.Argument0Value;
//            if (temp is Cons) {
//#if DEBUG
//                noteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            }
//            else {
//                answer = temp;
//                return false;
//            }
//        }
//    }

//    [Serializable]
//    sealed class PCondIsTypeCarA0SA1 : PCondIsTypeCarA0SA
//    {
//        PCondIsTypeCarA0SA1 (PrimitiveIsTypeCarA0 predicate, SCode consequent, Argument1 alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveIsTypeCarA0 predicate, SCode consequent, Argument1 alternative)
//        {
//            return
//                new PCondIsTypeCarA0SA1 (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException();

//#if DEBUG
//            Warm ("PCondIsTypeCarA0SA1.EvalStep");
//#endif
//            if (environment.Argument0Value is Cons) {
//#if DEBUG
//                noteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            }
//            else {
//                answer = environment.Argument1Value;
//                return false;
//            }
//        }
//    }



//    [Serializable]
//    sealed class PCondIsTypeCarA0SQ : PCondIsTypeCarA0
//    {
//#if DEBUG
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type>();
//#endif
//        public readonly object alternativeValue;

//        PCondIsTypeCarA0SQ (PrimitiveIsTypeCarA0 predicate, SCode consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }

//        internal static SCode Make (PrimitiveIsTypeCarA0 predicate, SCode consequent, Quotation alternative)
//        {
//            return new PCondIsTypeCarA0SQ (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("PCondIsTypeCarA0SQ.EvalStep");
//#endif
//            Cons temp = environment.Argument0Value as Cons;
//            if (temp == null) throw new NotImplementedException ();
//            if (temp.Car is Cons) {
//#if DEBUG
//                SCode.location = "-";
//                noteCalls (this.consequent);
//                consequentTypeHistogram.Note (this.consequentType);
//                SCode.location = "PCondIsTypeCarA0SQ.EvalStep";
//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            }
//            else {
//                answer = this.alternativeValue;
//                return false;
//            }
//        }
//    }

//    [Serializable] class PCondIsTypeCarA1 : PCondIsTypeCarA
//    {
//#if DEBUG
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
//        static Histogram <Type> alternativeTypeHistogram = new Histogram<Type> ();
//#endif
//        protected PCondIsTypeCarA1 (PrimitiveIsTypeCarA1 predicate, SCode consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException();

//#if DEBUG
//            Warm ("PCondIsTypeCarA1.EvalStep");
//#endif
//            if (environment.Argument1Value is Cons) {
//#if DEBUG
//                SCode.location = "-";
//                noteCalls (this.consequent);
//                consequentTypeHistogram.Note (this.consequentType);
//                SCode.location = "PCondIsTypeCarA1.EvalStep.1";
//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            } else {
//#if DEBUG
//                SCode.location = "-";
//                noteCalls (this.alternative);
//                alternativeTypeHistogram.Note (this.alternativeType);
//                SCode.location = "PCondIsTypeCarA1.EvalStep.2";
//#endif
//                expression = this.alternative;
//                answer = null;
//                return true;
//            }
//        }

//        internal static SCode Make (PrimitiveIsTypeCarA1 predicate, SCode consequent, SCode alternative)
//        {
//            return
//                (consequent is LexicalVariable) ? PCondIsTypeCarA1L.Make (predicate, (LexicalVariable) consequent, alternative)
//                : (consequent is Quotation) ? PCondIsTypeCarA1Q.Make (predicate, (Quotation) consequent, alternative)
//                : (alternative is LexicalVariable) ? PCondIsTypeCarA1SL.Make (predicate, consequent, (LexicalVariable) alternative)
//                : (alternative is Quotation) ? PCondIsTypeCarA1SQ.Make (predicate, consequent, (Quotation) alternative)
//                : new PCondIsTypeCarA1 (predicate, consequent, alternative);
//        }
//    }



//    [Serializable] class PCondIsTypeCarA1Q : PCondIsTypeCarA1
//    {
//        public readonly object consequentValue;
//        protected PCondIsTypeCarA1Q (PrimitiveIsTypeCarA1 predicate, Quotation consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.consequentValue = consequent.Quoted;
//        }
//        internal static SCode Make (PrimitiveIsTypeCarA1 predicate, Quotation consequent, SCode alternative)
//        {
//            return
//                (alternative is LexicalVariable) ? PCondIsTypeCarA1QL.Make (predicate, consequent, (LexicalVariable) alternative) :
//                (alternative is Quotation) ? PCondIsTypeCarA1QQ.Make (predicate, consequent, (Quotation) alternative) :
//                new PCondIsTypeCarA1Q (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException();
//#if DEBUG
//            Warm ("PCondIsTypeCarA1Q.EvalStep");
//#endif
//            object ev0 = environment.Argument1Value;

//            if (ev0 is Cons) {
//                answer = this.consequentValue;
//                return false;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                answer = null;
//                return true;
//            }  
//        }
//    }


//    [Serializable] class PCondIsTypeCarA1SA : PCondIsTypeCarA1SL
//    {

//        protected PCondIsTypeCarA1SA (PrimitiveIsTypeCarA1 predicate, SCode consequent, Argument alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }
//        internal static SCode Make (PrimitiveIsTypeCarA1 predicate, SCode consequent, Argument alternative)
//        {
//            return
//                (alternative is Argument0) ? PCondIsTypeCarA1SA0.Make (predicate, consequent, (Argument0) alternative) :
//                (alternative is Argument1) ? Unimplemented () :
//                new PCondIsTypeCarA1SA (predicate, consequent, alternative);

//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            Unimplemented ();
//#if DEBUG
//            Warm ("PCondIsTypeCarA1SL.EvalStep");
//#endif
//            object ev0 = environment.Argument1Value;

//            if (!(ev0 is Cons)) {
//                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.consequent);

//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            }

//        }


//    }

//    [Serializable]
//    sealed class PCondIsTypeCarA1SA0 : PCondIsTypeCarA1SA
//    {
//#if DEBUG
//        static Histogram<Type> consequentTypeHistogram = new Histogram<Type>();
//#endif

//        PCondIsTypeCarA1SA0 (PrimitiveIsTypeCarA1 predicate, SCode consequent, Argument0 alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        internal static SCode Make (PrimitiveIsTypeCarA1 predicate, SCode consequent, Argument0 alternative)
//        {
//            return
//                new PCondIsTypeCarA1SA0 (predicate, consequent, alternative);

//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException();
//#if DEBUG
//            Warm ("PCondIsTypeCarA1SA0.EvalStep");
//#endif
//            object ev0 = environment.Argument1Value;

//            if (ev0 is Cons) {
//#if DEBUG
//                noteCalls (this.consequent);
//                consequentTypeHistogram.Note (this.consequentType);
//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            }
//            else {
//                answer = environment.Argument0Value;
//                return false;
//            }
//        }
//    }

//    [Serializable] class PCondIsTypeCarA1SQ : PCondIsTypeCarA1
//    {
//        public readonly object alternativeValue;

//        protected PCondIsTypeCarA1SQ (PrimitiveIsTypeCarA1 predicate, SCode consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }

//        internal static SCode Make (PrimitiveIsTypeCarA1 predicate, SCode consequent, Quotation alternative)
//        {
//            return new PCondIsTypeCarA1SQ (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException();
//#if DEBUG
//            Warm ("PCondIsTypeCarA1SQ.EvalStep");
//#endif
//            object ev0 = environment.Argument1Value;


//            if (!(ev0 is Cons)) {

//                answer = this.alternativeValue;
//                return false;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            }
//        }
//    }



//    [Serializable] class PCondIsTypeCarAA : PCondIsTypeCarAL
//    {
//        protected PCondIsTypeCarAA (PrimitiveIsTypeCarA predicate, Argument consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        { }

//        static public SCode Make (PrimitiveIsTypeCarA predicate, Argument consequent, SCode alternative)
//        {
//            throw new NotImplementedException ();
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException ();
//        }

//    }



//    [Serializable] class PCondIsTypeCarAQ : PCondIsTypeCarA
//    {
//        public readonly object consequentValue;

//        protected PCondIsTypeCarAQ (PrimitiveIsTypeCarA predicate, Quotation consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.consequentValue = consequent.Quoted;
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException();
//            #region EvalStepBody
//#if DEBUG
//            Warm ();
//            noteCalls (this.arg0);
//#endif
//            object ev0 = environment.ArgumentValue (this.predicateOffset);


//            if (!(ev0 is Cons)) {
//#if DEBUG
//                noteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                answer = null;
//                return true;
//            }
//            else {
//                answer = this.consequentValue;
//                return false;
//            }
//            #endregion

//        }

//        internal static SCode Make (PrimitiveIsTypeCarA predicate, Quotation consequent, SCode alternative)
//        {
//            return
//                (alternative is LexicalVariable) ? PCondIsTypeCarAQL.Make (predicate, consequent, (LexicalVariable) alternative)
//                : (alternative is Quotation) ? PCondIsTypeCarAQQ.Make (predicate, consequent, (Quotation) alternative)
//                : new PCondIsTypeCarAQ (predicate, consequent, alternative);
//        }
//    }



//    [Serializable] class PCondIsTypeCarAQQ : PCondIsTypeCarAQ
//    {
//        protected PCondIsTypeCarAQQ (PrimitiveIsTypeCarA predicate, Quotation consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {

//            throw new NotImplementedException ();
//        }

//        internal static SCode Make (PrimitiveIsTypeCarA predicate, Quotation consequent, Quotation alternative)
//        {
//            if (consequent.Quoted == alternative.Quoted) {
//                Debug.WriteLine ("; Optimize (if <expr> <literal> <literal>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, consequent);
//            }
//            else if (Configuration.EnableTrueUnspecific && consequent.Quoted == Constant.Unspecific) {
//                Debug.WriteLine ("; Optimize (if <expr> <unspecific> <literal>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, alternative);
//            }
//            else if (Configuration.EnableTrueUnspecific && alternative.Quoted == Constant.Unspecific) {
//                Debug.WriteLine ("; Optimize (if <expr> <literal> <unspecific>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, consequent);
//            }
//            return new PCondIsTypeCarAQQ (predicate, consequent, alternative);
//        }
//    }


//    [Serializable] class PCondIsTypeCarASQ : PCondIsTypeCarA
//    {
//        public readonly object alternativeValue;

//        protected PCondIsTypeCarASQ (PrimitiveIsTypeCarA predicate, SCode consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException();
//#if DEBUG
//            Warm ("PCondIsTypeCarASQ.EvalStep");
//#endif
//            object ev0 = environment.ArgumentValue (predicateOffset);


//            if (!(ev0 is Cons)) {

//                answer = this.alternativeValue;
//                return false;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            }
//        }

//        internal static PCondIsTypeCarA Make (PrimitiveIsTypeCarA predicate, SCode consequent, Quotation alternative)
//        {
//            return new PCondIsTypeCarASQ (predicate, consequent, alternative);
//        }
//    }


//    [Serializable]
//    class PCondIsTypeCarSQ : PCondIsTypeCar
//    {
//        public readonly object consequentValue;

//        protected PCondIsTypeCarSQ (PrimitiveIsTypeCar predicate, Quotation consequent, SCode alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.consequentValue = consequent.Quoted;
//        }

//        public static SCode Make (PrimitiveIsTypeCar predicate, Quotation consequent, SCode alternative)
//        {
//            return
//                (alternative is LexicalVariable) ? PCondIsTypeCarSQL.Make (predicate, consequent, (LexicalVariable) alternative) :
//                (alternative is Quotation) ? PCondIsTypeCarSQQ.Make (predicate, consequent, (Quotation) alternative) :
//                new PCondIsTypeCarSQ (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException();
//#if DEBUG
//            Warm ("-");
//            noteCalls (this.arg0);
//            SCode.location = "PCondIsTypeCarSQ.EvalStep";
//#endif
//            Control unev0 = this.arg0;
//            Environment env = environment;
//            object ev0;
//            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
//#if DEBUG
//                        SCode.location = "PCondIsTypeCarSQ.EvalStep.1";
//#endif
//            if (ev0 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveIsTypeCarFrame0 (this, environment));
//                //answer = Interpreter.UnwindStack;
//                //environment = env;
//                //return false;
//            }

//            if (ev0 is Cons) {
//                answer = this.consequentValue;
//                return false;
//            } 
//            else {
//#if DEBUG
//                noteCalls (this.alternative);
//#endif
//                expression = this.alternative;
//                answer = null;
//                return true;
//            }
//        }
//    }



//    [Serializable]
//    class PCondIsTypeCarSQQ : PCondIsTypeCarSQ
//    {
//        public readonly object alternativeValue;

//        protected PCondIsTypeCarSQQ (PrimitiveIsTypeCar predicate, Quotation consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }


//        public static SCode Make (PrimitiveIsTypeCar predicate, Quotation consequent, Quotation alternative)
//        {
//            if (consequent.Quoted == alternative.Quoted) {
//                Debug.WriteLine ("; Optimize (if <expr> <literal> <literal>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, consequent);
//            }
//            else if (Configuration.EnableTrueUnspecific && consequent.Quoted == Constant.Unspecific) {
//                Debug.WriteLine ("; Optimize (if <expr> <unspecific> <literal>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, alternative);
//            }
//            else if (Configuration.EnableTrueUnspecific && alternative.Quoted == Constant.Unspecific) {
//                Debug.WriteLine ("; Optimize (if <expr> <literal> <unspecific>) => (begin <expr> <literal>)");
//                return Sequence2.Make (predicate, consequent);
//            }

//            return new PCondIsTypeCarSQQ (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException();
//            #region EvalStepBody
//#if DEBUG
//            Warm ();
//            noteCalls (this.arg0);
//#endif
//            Control unev0 = this.arg0;
//            Environment env = environment;
//            object ev0;
//            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
//            if (ev0 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveIsTypeCarFrame0 (this, environment));
//                //answer = Interpreter.UnwindStack;
//                //environment = env;
//                //return false;
//            }


//            if (!(ev0 is Cons)) {
//                answer = this.alternativeValue;
//                return false;
//            }
//            else {
//                answer = this.consequentValue;
//                return false;
//            }
//            #endregion
//        }
//    }



//    [Serializable] class PCondIsTypeCarSSA : PCondIsTypeCarSSL
//    {

//        protected PCondIsTypeCarSSA (PrimitiveIsTypeCar predicate, SCode consequent, Argument alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (PrimitiveIsTypeCar predicate, SCode consequent, Argument alternative)
//        {
//            return
//                (alternative is Argument0) ? PCondIsTypeCarSSA0.Make (predicate, consequent, (Argument0) alternative) :
//                (alternative is Argument1) ? Unimplemented () :
//                new PCondIsTypeCarSSA (predicate, consequent, alternative);
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            Unimplemented ();
//#if DEBUG
//            Warm ("PCondIsTypeCarSSL.EvalStep");
//            noteCalls (this.arg0);
//#endif
//            Control unev0 = this.arg0;
//            Environment env = environment;
//            object ev0;
//            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
//            if (ev0 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveIsTypeCarFrame0 (this, environment));
//                //answer = Interpreter.UnwindStack;
//                //environment = env;
//                //return false;
//            }



//            if (!(ev0 is Cons)) {
//                if (environment.FastLexicalRef (out answer, this.alternativeName, this.alternativeDepth, this.alternativeOffset))
//                    throw new NotImplementedException ();
//                return false;
//            }
//            else {
//#if DEBUG
//                noteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            }
//        }
//    }

//    [Serializable]
//    sealed class PCondIsTypeCarSSA0 : PCondIsTypeCarSSA
//    {

//        PCondIsTypeCarSSA0 (PrimitiveIsTypeCar predicate, SCode consequent, Argument0 alternative)
//            : base (predicate, consequent, alternative)
//        {
//        }

//        public static SCode Make (PrimitiveIsTypeCar predicate, SCode consequent, Argument0 alternative)
//        {
//            return
//                new PCondIsTypeCarSSA0 (predicate, consequent, alternative);
//        }
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException();
//#if DEBUG
//            Warm ("-");
//            noteCalls (this.arg0);
//            SCode.location = "PCondIsTypeCarSSA0.EvalStep";
//#endif
//            Control unev0 = this.arg0;
//            Environment env = environment;
//            object ev0;
//            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
//            if (ev0 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveIsTypeCarFrame0 (this, environment));
//                //answer = Interpreter.UnwindStack;
//                //environment = env;
//                //return false;
//            }

//            if (ev0 is Cons) {
//#if DEBUG
//                noteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            }
//            else {
//                answer = environment.Argument0Value;
//                return false;
//            }
//        }
//    }


//    [Serializable]
//    sealed class PCondIsTypeCarSSQ : PCondIsTypeCar
//    {
//        public readonly object alternativeValue;

//        PCondIsTypeCarSSQ (PrimitiveIsTypeCar predicate, SCode consequent, Quotation alternative)
//            : base (predicate, consequent, alternative)
//        {
//            this.alternativeValue = alternative.Quoted;
//        }

//        public static SCode Make (PrimitiveIsTypeCar predicate, SCode consequent, Quotation alternative)
//        {
//            return new PCondIsTypeCarSSQ (predicate, consequent, alternative);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//            throw new NotImplementedException();
//#if DEBUG
//            Warm ("PCondIsTypeCarSSQ.EvalStep");
//            noteCalls (this.arg0);
//#endif
//            Control unev0 = this.arg0;
//            Environment env = environment;
//            object ev0;
//            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
//            if (ev0 == Interpreter.UnwindStack) {
//                throw new NotImplementedException ();
//                //((UnwinderState) env).AddFrame (new PrimitiveIsTypeCarFrame0 (this, environment));
//                //answer = Interpreter.UnwindStack;
//                //environment = env;
//                //return false;
//            }

//            if (ev0 is Cons) {
//#if DEBUG
//                noteCalls (this.consequent);
//#endif
//                expression = this.consequent;
//                answer = null;
//                return true;
//            }
//            else {
//                answer = this.alternativeValue;
//                return false;
//           }
//        }
//    }

}

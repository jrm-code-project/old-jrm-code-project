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
        protected PCondIsType (PrimitiveIsType<SType> predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsType<SType> predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate.Operand is Argument)  ? PCondIsTypeA<SType>.Make (predicate, consequent, alternative) :
                (predicate.Operand is StaticVariable) ? PCondIsTypeS<SType>.Make (predicate, consequent, alternative) :
                //(predicate is PrimitiveIsType<SType>) ? PCondIsTypeCar<SType>.Make ((PrimitiveIsType<SType>) predicate, consequent, alternative) :
                //(predicate.Operand is StaticVariable) ? PCondIsTypeS<SType>.Make (predicate, consequent, alternative) :
                (consequent is Argument) ? PCondIsTypeXA<SType>.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? PCondIsTypeXQ<SType>.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? PCondIsTypeXS<SType>.Make (predicate, (StaticVariable) consequent, alternative):
                (alternative is Argument) ? PCondIsTypeXXA<SType>.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCondIsTypeXXQ<SType>.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? PCondIsTypeXXS<SType>.Make (predicate, consequent, (StaticVariable) alternative) :
                new PCondIsType<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
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
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
            else {
#if DEBUG
                NoteCalls (this.alternative);
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
        protected PCondIsTypeA (PrimitiveIsType<SType> predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.predicateOffset = ((Argument) predicate.Operand).Offset;
        }

        public static new SCode Make (PrimitiveIsType<SType> predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate.Operand is Argument0) ? PCondIsTypeA0<SType>.Make (predicate, consequent, alternative) :
                (predicate.Operand is Argument1) ? PCondIsTypeA1<SType>.Make (predicate, consequent, alternative) :
                (consequent is Quotation) ? PCondIsTypeAQ<SType>.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is Quotation) ? PCondIsTypeAXQ<SType>.Make (predicate, consequent, (Quotation) alternative) :
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
        protected PCondIsTypeA0 (PrimitiveIsType<SType> predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        internal static new SCode Make (PrimitiveIsType<SType> predicate, SCode consequent, SCode alternative)
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
            Warm ("PCondIsTypeA0");
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
        public readonly int consequentOffset;
        protected PCondIsTypeA0A (PrimitiveIsType<SType> predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        internal static SCode Make (PrimitiveIsType<SType> predicate, Argument consequent, SCode alternative)
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
        protected PCondIsTypeA0A0 (PrimitiveIsType<SType> predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        internal static SCode Make (PrimitiveIsType<SType> predicate, Argument0 consequent, SCode alternative)
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
        protected PCondIsTypeA0A1 (PrimitiveIsType<SType> predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        internal static SCode Make (PrimitiveIsType<SType> predicate, Argument1 consequent, SCode alternative)
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
        public readonly object consequentValue;
        protected PCondIsTypeA0Q (PrimitiveIsType<SType> predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        internal static SCode Make (PrimitiveIsType<SType> predicate, Quotation consequent, SCode alternative)
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
        public readonly int alternativeOffset;
        protected PCondIsTypeA0XA (PrimitiveIsType<SType> predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        internal static SCode Make (PrimitiveIsType<SType> predicate, SCode consequent, Argument alternative)
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
        public readonly object alternativeOffset;
        PCondIsTypeA0XA0 (PrimitiveIsType<SType> predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        internal static SCode Make (PrimitiveIsType<SType> predicate, SCode consequent, Argument0 alternative)
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
        public readonly object alternativeOffset;
        PCondIsTypeA0XA1 (PrimitiveIsType<SType> predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        internal static SCode Make (PrimitiveIsType<SType> predicate, SCode consequent, Argument1 alternative)
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
        public readonly Symbol alternativeName;
        public readonly int alternativeOffset;
        protected PCondIsTypeA0XS (PrimitiveIsType<SType> predicate, SCode consequent, StaticVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeOffset = alternative.Offset;
        }

        internal static SCode Make (PrimitiveIsType<SType> predicate, SCode consequent, StaticVariable alternative)
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
        public readonly object alternativeValue;
        protected PCondIsTypeA0XQ (PrimitiveIsType<SType> predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        internal static SCode Make (PrimitiveIsType<SType> predicate, SCode consequent, Quotation alternative)
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
        protected PCondIsTypeA1(PrimitiveIsType<SType> predicate, SCode consequent, SCode alternative)
            : base(predicate, consequent, alternative)
        {
        }

        internal static SCode Make(PrimitiveIsType<SType> predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCondIsTypeA1A<SType>.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? PCondIsTypeA1Q<SType>.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? PCondIsTypeA1S<SType>.Make (predicate, (StaticVariable) consequent, alternative) :
                (alternative is Argument) ? PCondIsTypeA1XA<SType>.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCondIsTypeA1XQ<SType>.Make (predicate,  consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? PCondIsTypeA1XS<SType>.Make (predicate, consequent, (StaticVariable) alternative) :
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
                SCode.location = "PCondIsTypeA1";
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsTypeA1A<SType> : PCondIsTypeA1<SType>
    {
        public readonly int consequentOffset;
        protected PCondIsTypeA1A (PrimitiveIsType<SType> predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        internal static SCode Make (PrimitiveIsType<SType> predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondIsTypeA1A0<SType>.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? PCondIsTypeA1A1<SType>.Make (predicate, (Argument1) consequent, alternative) :
               new PCondIsTypeA1A<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsTypeA1A");
#endif
            if (environment.Argument1Value is SType) {
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
    class PCondIsTypeA1A0<SType> : PCondIsTypeA1A<SType>
    {
        protected PCondIsTypeA1A0 (PrimitiveIsType<SType> predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        internal static SCode Make (PrimitiveIsType<SType> predicate, Argument1 consequent, SCode alternative)
        {
            return
               new PCondIsTypeA1A0<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsTypeA1A0");
#endif
            object obj = environment.Argument1Value;

            if (obj is SType) {
                answer = environment.Argument0Value;
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
    class PCondIsTypeA1A1<SType> : PCondIsTypeA1A<SType>
    {
        protected PCondIsTypeA1A1 (PrimitiveIsType<SType> predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        internal static SCode Make (PrimitiveIsType<SType> predicate, Argument1 consequent, SCode alternative)
        {
            return
               new PCondIsTypeA1A1<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsTypeA1A1");
#endif
            if (environment.Argument1Value is SType) {
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
    class PCondIsTypeA1Q<SType> : PCondIsTypeA1<SType>
    {
        public readonly object consequentValue;
        protected PCondIsTypeA1Q (PrimitiveIsType<SType> predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        internal static SCode Make (PrimitiveIsType<SType> predicate, Quotation consequent, SCode alternative)
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
                SCode.location = "PCondIsTypeA1Q";
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsTypeA1S<SType> : PCondIsTypeA1<SType>
    {
        public readonly Symbol consequentName;
        public readonly int consequentOffset;

        protected PCondIsTypeA1S (PrimitiveIsType<SType> predicate, StaticVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentOffset = consequent.Offset;
        }

        internal static SCode Make (PrimitiveIsType<SType> predicate, StaticVariable consequent, SCode alternative)
        {
            return
                new PCondIsTypeA1S<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsTypeA1S");
#endif
            if (environment.Argument1Value is SType) {
                if (environment.StaticValue (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                SCode.location = "PCondIsTypeA1S";
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsTypeA1XA<SType> : PCondIsTypeA1<SType>
    {
        public readonly int alternativeOffset;
        protected PCondIsTypeA1XA (PrimitiveIsType<SType> predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        internal static SCode Make (PrimitiveIsType<SType> predicate, SCode consequent, Argument alternative)
        {
            return
               (alternative is Argument0) ? PCondIsTypeA1XA0<SType>.Make (predicate, consequent, (Argument0) alternative) :
               (alternative is Argument1) ? PCondIsTypeA1XA1<SType>.Make (predicate, consequent, (Argument1) alternative) :
               new PCondIsTypeA1XA<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsTypeA1XA");
#endif
            if (environment.Argument1Value is SType) {
#if DEBUG
                SCode.location = "-";
                NoteCalls ((SCode) this.consequent);
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
    sealed class PCondIsTypeA1XA0<SType> : PCondIsTypeA1XA<SType>
    {
        public readonly object alternativeOffset;
        PCondIsTypeA1XA0 (PrimitiveIsType<SType> predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        internal static SCode Make (PrimitiveIsType<SType> predicate, SCode consequent, Argument0 alternative)
        {
            return
               new PCondIsTypeA1XA0<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsTypeA1XA0");
#endif
            if (environment.Argument1Value is SType) {
#if DEBUG
                SCode.location = "-";
                NoteCalls ((SCode) this.consequent);
                SCode.location = "PCondIsTypeA1XA";
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
    sealed class PCondIsTypeA1XA1<SType> : PCondIsTypeA0XA<SType>
    {
        public readonly object alternativeOffset;
        PCondIsTypeA1XA1 (PrimitiveIsType<SType> predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        internal static SCode Make (PrimitiveIsType<SType> predicate, SCode consequent, Argument1 alternative)
        {
            return
               new PCondIsTypeA1XA1<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsTypeA1XA1");
#endif
            object evp = environment.Argument1Value;
            if (evp is SType) {
#if DEBUG
                SCode.location = "-";
                NoteCalls ((SCode) this.consequent);
                SCode.location = "PCondIsTypeA1XA1";
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
            else {
                answer = evp;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsTypeA1XQ<SType> : PCondIsTypeA1<SType>
    {
        public readonly object alternativeValue;
        protected PCondIsTypeA1XQ (PrimitiveIsType<SType> predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        internal static SCode Make (PrimitiveIsType<SType> predicate, SCode consequent, Quotation alternative)
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
                NoteCalls (this.consequent);
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
    class PCondIsTypeA1XS<SType> : PCondIsTypeA1<SType>
    {
        public readonly Symbol alternativeName;
        public readonly int alternativeOffset;

        protected PCondIsTypeA1XS (PrimitiveIsType<SType> predicate, SCode consequent, StaticVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeOffset = alternative.Offset;
        }

        internal static SCode Make (PrimitiveIsType<SType> predicate, SCode consequent, StaticVariable alternative)
        {
            return
                new PCondIsTypeA1XS<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsTypeA1XS");
#endif
            if (environment.Argument1Value is SType) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                SCode.location = "PCondIsTypeA1XS";
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
    class PCondIsTypeAQ<SType> : PCondIsTypeA<SType>
    {
        public readonly object consequentValue;
        protected PCondIsTypeAQ (PrimitiveIsType<SType> predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        internal static SCode Make (PrimitiveIsType<SType> predicate, Quotation consequent, SCode alternative)
        {
            return
                new PCondIsTypeAQ<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsTypeAQ");
#endif
            if (environment.ArgumentValue(this.predicateOffset) is SType) {
                answer = this.consequentValue;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                SCode.location = "PCondIsTypeAQ";
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsTypeAXQ<SType> : PCondIsTypeA<SType>
    {
        public readonly object alternativeValue;
        protected PCondIsTypeAXQ (PrimitiveIsType<SType> predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        internal static SCode Make (PrimitiveIsType<SType> predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondIsTypeAXQ<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsTypeAXQ");
#endif
            if (environment.ArgumentValue(this.predicateOffset) is SType) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                SCode.location = "PCondIsTypeAXQ";
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
        public readonly SCode operand;
        protected PCondIsTypeCar (PrimitiveIsTypeCar<SType> predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.operand = predicate.randArg;
        }

        public static new SCode Make (PrimitiveIsTypeCar<SType> predicate, SCode consequent, SCode alternative)
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
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
            else {
#if DEBUG
                NoteCalls (this.alternative);
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
        public readonly int predicateOffset;
        protected PCondIsTypeCarA (PrimitiveIsTypeCarA<SType> predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.predicateOffset = predicate.offset;
        }

        public static new SCode Make (PrimitiveIsTypeCarA<SType> predicate, SCode consequent, SCode alternative)
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
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
            else {
#if DEBUG
                NoteCalls (this.alternative);
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
        protected PCondIsTypeCarA0 (PrimitiveIsTypeCarA0<SType> predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveIsTypeCarA0<SType> predicate, SCode consequent, SCode alternative)
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
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
            else {
#if DEBUG
                NoteCalls (this.alternative);
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
        public readonly object consequentValue;

        protected PCondIsTypeCarA0Q (PrimitiveIsTypeCarA0<SType> predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static new SCode Make (PrimitiveIsTypeCarA0<SType> predicate, Quotation consequent, SCode alternative)
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
        protected PCondIsTypeCarA1 (PrimitiveIsTypeCarA1<SType> predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveIsTypeCarA1<SType> predicate, SCode consequent, SCode alternative)
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
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
            else {
#if DEBUG
                NoteCalls (this.alternative);
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
        public readonly Symbol predicateName;
        public readonly int predicateOffset;
        protected PCondIsTypeCarS (PrimitiveIsTypeCarS<SType> predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.predicateName = predicate.name;
            this.predicateOffset = predicate.offset;
        }

        public static new SCode Make (PrimitiveIsTypeCarS<SType> predicate, SCode consequent, SCode alternative)
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
#endif
                expression = this.consequent;
                answer = null;
                return true;
            }
            else {
#if DEBUG
                NoteCalls (this.alternative);
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }
    }
    [Serializable]
    class PCondIsTypeS<SType> : PCondIsType<SType>
    {
        public readonly Symbol predicateName;
        public readonly int predicateOffset;
        protected PCondIsTypeS (PrimitiveIsType<SType> predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.predicateName = ((StaticVariable) predicate.Operand).Name;
            this.predicateOffset = ((StaticVariable) predicate.Operand).Offset;
        }

        public static new SCode Make (PrimitiveIsType<SType> predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCondIsTypeSA<SType>.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? PCondIsTypeSQ<SType>.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? PCondIsTypeSS<SType>.Make (predicate, (StaticVariable) consequent, alternative) :
                (alternative is Argument) ? PCondIsTypeSXA<SType>.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCondIsTypeSXQ<SType>.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? PCondIsTypeSXS<SType>.Make (predicate, consequent, (StaticVariable) alternative) :
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
            NoteCalls ((SCode) expression);
#endif
            return true;
        }
    }

    [Serializable]
    class PCondIsTypeSA<SType> : PCondIsTypeS<SType>
    {
        public readonly int consequentOffset;

        protected PCondIsTypeSA (PrimitiveIsType<SType> predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveIsType<SType> predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondIsTypeSA0<SType>.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? PCondIsTypeSA1<SType>.Make (predicate, (Argument1) consequent, alternative) :
                new PCondIsTypeSA<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsTypeSA");
#endif
            object evpred;
            if (environment.StaticValue (out evpred, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();

            if (evpred is SType) {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
            else {
#if DEBUG
                NoteCalls (this.alternative);
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsTypeSA0<SType> : PCondIsTypeSA<SType>
    {
        protected PCondIsTypeSA0 (PrimitiveIsType<SType> predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveIsType<SType> predicate, Argument0 consequent, SCode alternative)
        {
            return
                new PCondIsTypeSA0<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsTypeSA0");
#endif
            object evpred;
            if (environment.StaticValue (out evpred, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();

            if (evpred is SType) {
                answer = environment.Argument0Value;
                return false;
            }
            else {
#if DEBUG
                NoteCalls (this.alternative);
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsTypeSA1<SType> : PCondIsTypeSA<SType>
    {
        protected PCondIsTypeSA1 (PrimitiveIsType<SType> predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveIsType<SType> predicate, Argument1 consequent, SCode alternative)
        {
            return
                new PCondIsTypeSA1<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsTypeSA1");
#endif
            object evpred;
            if (environment.StaticValue (out evpred, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();

            if (evpred is SType) {
                answer = environment.Argument1Value;
                return false;
            }
            else {
#if DEBUG
                NoteCalls (this.alternative);
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsTypeSQ<SType> : PCondIsTypeS<SType>
    {
        public readonly object consequentValue;

        protected PCondIsTypeSQ (PrimitiveIsType<SType> predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static new SCode Make (PrimitiveIsType<SType> predicate, Quotation consequent, SCode alternative)
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
                NoteCalls (this.alternative);
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsTypeSS<SType> : PCondIsTypeS<SType>
    {
        public readonly Symbol consequentName;
        public readonly int consequentOffset;

        protected PCondIsTypeSS (PrimitiveIsType<SType> predicate, StaticVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveIsType<SType> predicate, StaticVariable consequent, SCode alternative)
        {
            return
                new PCondIsTypeSS<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsTypeSS");
#endif
            object evpred;
            if (environment.StaticValue (out evpred, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();

            if (evpred is SType) {
                if (environment.StaticValue (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
#if DEBUG
                NoteCalls (this.alternative);
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsTypeSXA<SType> : PCondIsTypeS<SType>
    {
        public readonly int alternativeOffset;

        protected PCondIsTypeSXA (PrimitiveIsType<SType> predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveIsType<SType> predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0)? PCondIsTypeSXA0<SType>.Make (predicate, consequent, (Argument0) alternative) :
                 (alternative is Argument1) ? PCondIsTypeSXA1<SType>.Make (predicate, consequent, (Argument0) alternative) :
                new PCondIsTypeSXA<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsTypeSXA");
#endif
            object evpred;
            if (environment.StaticValue (out evpred, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();

            if (evpred is SType) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                SCode.location = "PCondIsTypeSXA";
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
    class PCondIsTypeSXA0<SType> : PCondIsTypeSXA<SType>
    {
        protected PCondIsTypeSXA0 (PrimitiveIsType<SType> predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveIsType<SType> predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCondIsTypeSXA0<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsTypeSXA0");
#endif
            object evpred;
            if (environment.StaticValue (out evpred, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();

            if (evpred is SType) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                SCode.location = "PCondIsTypeSXA0";
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
    class PCondIsTypeSXA1<SType> : PCondIsTypeSXA<SType>
    {
        protected PCondIsTypeSXA1 (PrimitiveIsType<SType> predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveIsType<SType> predicate, SCode consequent, Argument1 alternative)
        {
            return
                new PCondIsTypeSXA1<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsTypeSXA1");
#endif
            object evpred;
            if (environment.StaticValue (out evpred, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();

            if (evpred is SType) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                SCode.location = "PCondIsTypeSXA1";
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
    class PCondIsTypeSXQ<SType> : PCondIsTypeS<SType>
    {
        public readonly object alternativeValue;

        protected PCondIsTypeSXQ (PrimitiveIsType<SType> predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveIsType<SType> predicate, SCode consequent, Quotation alternative)
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
    class PCondIsTypeSXS<SType> : PCondIsTypeS<SType>
    {
        public readonly Symbol alternativeName;
        public readonly int alternativeOffset;

        protected PCondIsTypeSXS (PrimitiveIsType<SType> predicate, SCode consequent, StaticVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveIsType<SType> predicate, SCode consequent, StaticVariable alternative)
        {
            return
                new PCondIsTypeSXS<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsTypeSXS");
#endif
            object evpred;
            if (environment.StaticValue (out evpred, this.predicateName, this.predicateOffset))
                throw new NotImplementedException ();

            if (evpred is SType) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                SCode.location = "PCondIsTypeSXS";
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
    class PCondIsTypeXA<SType> : PCondIsType<SType>
    {
        public readonly int consequentOffset;

        protected PCondIsTypeXA (PrimitiveIsType<SType> predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveIsType<SType> predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondIsTypeXA0<SType>.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? PCondIsTypeXA1<SType>.Make (predicate, (Argument1) consequent, alternative) :
                new PCondIsTypeXA<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            SCode.location = "PCondIsTypeXA";
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
            SCode.location = "PCondIsTypeXA";
#endif
            if (ev0 is SType) {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
            else {
#if DEBUG
                NoteCalls (this.alternative);
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }

    }

    [Serializable]
    class PCondIsTypeXA0<SType> : PCondIsType<SType>
    {

        protected PCondIsTypeXA0 (PrimitiveIsType<SType> predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsType<SType> predicate, Argument0 consequent, SCode alternative)
        {
            return
                new PCondIsTypeXA0<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            SCode.location = "PCondIsTypeXA0";
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
            SCode.location = "PCondIsTypeXA0";
#endif
            if (ev0 is SType) {
                answer = environment.Argument0Value ;
                return false;
            }
            else {
#if DEBUG
                NoteCalls (this.alternative);
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }

    }

    [Serializable]
    class PCondIsTypeXA1<SType> : PCondIsTypeA<SType>
    {

        protected PCondIsTypeXA1 (PrimitiveIsType<SType> predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveIsType<SType> predicate, Argument1 consequent, SCode alternative)
        {
            return
                new PCondIsTypeXA1<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            SCode.location = "PCondIsTypeXA1";
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
            SCode.location = "PCondIsTypeXA1";
#endif
            if (ev0 is SType) {
                answer = environment.Argument1Value;
                return false;
            }
            else {
#if DEBUG
                NoteCalls (this.alternative);
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }

    }

    [Serializable]
    class PCondIsTypeXQ<SType> : PCondIsType<SType>
    {

        public readonly object consequentValue;

        protected PCondIsTypeXQ (PrimitiveIsType<SType> predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static new SCode Make (PrimitiveIsType<SType> predicate, Quotation consequent, SCode alternative)
        {
            return
                new PCondIsTypeXQ<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
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
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }

    }

    [Serializable]
    class PCondIsTypeXS<SType> : PCondIsType<SType>
    {

        public readonly Symbol consequentName;
        public readonly int consequentOffset;

        protected PCondIsTypeXS (PrimitiveIsType<SType> predicate, StaticVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentOffset = consequent.Offset;
        }

        public static new SCode Make (PrimitiveIsType<SType> predicate, StaticVariable consequent, SCode alternative)
        {
            return
                new PCondIsTypeXS<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            SCode.location = "PCondIsTypeXS";
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
            SCode.location = "PCondIsTypeXS";
#endif
            if (ev0 is SType) {
                if (environment.StaticValue (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
#if DEBUG
                NoteCalls (this.alternative);
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }

    }

    [Serializable]
    class PCondIsTypeXXA<SType> : PCondIsType<SType>
    {

        public readonly int alternativeOffset;

        protected PCondIsTypeXXA (PrimitiveIsType<SType> predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveIsType<SType> predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCondIsTypeXXA0<SType>.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCondIsTypeXXA1<SType>.Make (predicate, consequent, (Argument1) alternative) :
                new PCondIsTypeXXA<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            SCode.location = "PCondIsTypeXXA";
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
            SCode.location = "PCondIsTypeXXA";
#endif
            if (ev0 is SType) {
#if DEBUG
                NoteCalls (this.consequent);
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
    class PCondIsTypeXXA0<SType> : PCondIsTypeXXA<SType>
    {
        protected PCondIsTypeXXA0 (PrimitiveIsType<SType> predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveIsType<SType> predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCondIsTypeXXA0<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            SCode.location = "PCondIsTypeXXA0";
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
            SCode.location = "PCondIsTypeXXA0";
#endif
            if (ev0 is SType) {
#if DEBUG
                NoteCalls (this.consequent);
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
    class PCondIsTypeXXA1<SType> : PCondIsTypeXXA<SType>
    {
        protected PCondIsTypeXXA1 (PrimitiveIsType<SType> predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static new SCode Make (PrimitiveIsType<SType> predicate, SCode consequent, Argument1 alternative)
        {
            return
                new PCondIsTypeXXA1<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            SCode.location = "PCondIsTypeXXA1";
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
            SCode.location = "PCondIsTypeXXA1";
#endif
            if (ev0 is SType) {
#if DEBUG
                NoteCalls (this.consequent);
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
    class PCondIsTypeXXQ<SType> : PCondIsType<SType>
    {

        public readonly object alternativeValue;

        protected PCondIsTypeXXQ (PrimitiveIsType<SType> predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static new SCode Make (PrimitiveIsType<SType> predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondIsTypeXXQ<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            SCode.location = "PCondIsTypeXXQ";
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
            SCode.location = "PCondIsTypeXXQ";
#endif
            if (ev0 is SType) {
#if DEBUG
                NoteCalls (this.consequent);
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
    class PCondIsTypeXXS<SType> : PCondIsType<SType>
    {
        public readonly Symbol alternativeName;
        public readonly int alternativeOffset;

        protected PCondIsTypeXXS (PrimitiveIsType<SType> predicate, SCode consequent, StaticVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeOffset = alternative.Offset;
        }

        public static new SCode Make (PrimitiveIsType<SType> predicate, SCode consequent, StaticVariable alternative)
        {
            return
                new PCondIsTypeXXS<SType> (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            SCode.location = "PCondIsTypeXXS";
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
            SCode.location = "PCondIsTypeXXS";
#endif
            if (ev0 is SType) {
#if DEBUG
                NoteCalls (this.consequent);
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
}

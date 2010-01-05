using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;

namespace Microcode
{
    [Serializable]
    class PCondIsNull : PCond1
    {
#if DEBUG
        static Histogram<Type> arg0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsNull (PrimitiveIsNull predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsNull predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsNullA) ? PCondIsNullA.Make ((PrimitiveIsNullA) predicate, consequent, alternative) :
                (predicate is PrimitiveIsNullQ) ? PCondIsNullQ.Make ((PrimitiveIsNullQ) predicate, consequent, alternative) :
                (predicate is PrimitiveIsNullS) ? PCondIsNullS.Make ((PrimitiveIsNullS) predicate, consequent, alternative) :
                (consequent is Argument) ? PCondIsNullXA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? PCondIsNullXQ.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? PCondIsNullXS.Make (predicate, (StaticVariable) consequent, alternative) :
                (alternative is Argument) ? PCondIsNullXXA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCondIsNullXXQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? PCondIsNullXXS.Make (predicate, consequent, (StaticVariable) alternative) :
                new PCondIsNull (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            arg0TypeHistogram.Note (this.arg0Type);
            SCode.location = "PCondIsNull";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsNull";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveIsNullFrame0 (this, closureEnvironment));
                //answer = Interpreter.Unwind;
                //closureEnvironment = env;
                //return false;
            }

            if (ev0 == null) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsNull";
#endif
                expression = this.consequent;
                answer = null; // keep c# compiler happy
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsNull";
#endif
                expression = this.alternative;
                answer = null; // keep c# compiler happy
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsNullA : PCondIsNull
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int predicateArgumentOffset;

        protected PCondIsNullA (PrimitiveIsNullA predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.predicateArgumentOffset = predicate.arg0Offset;
        }

        public static SCode Make (PrimitiveIsNullA predicate, SCode consequent, SCode alternative)
        {
            return
                (predicate is PrimitiveIsNullA0) ? PCondIsNullA0.Make ((PrimitiveIsNullA0) predicate, consequent, alternative) :
                (predicate is PrimitiveIsNullA1) ? PCondIsNullA1.Make ((PrimitiveIsNullA1) predicate, consequent, alternative) :
                (consequent is Argument) ? PCondIsNullAA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? PCondIsNullAQ.Make (predicate, (Quotation) consequent, alternative) :
                //: (alternative is LexicalVariable) ? PCondIsNullASL.Make (predicate, consequent, (LexicalVariable) alternative)
                (alternative is Quotation) ? PCondIsNullAXQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsNullA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            SCode.location = "PCondIsNullA";
#endif
            if (environment.ArgumentValue (this.predicateArgumentOffset) == null) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsNullA";
#endif
                expression = this.consequent;
                answer = null; // keep c# compiler happy
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsNullA";
#endif
                expression = this.alternative;
                answer = null; // keep c# compiler happy
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsNullA0 : PCondIsNullA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int predicateArgumentOffset;

        protected PCondIsNullA0 (PrimitiveIsNullA0 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsNullA0 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCondIsNullA0A.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? PCondIsNullA0Q.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? PCondIsNullA0S.Make (predicate, (StaticVariable) consequent, alternative) :
                (alternative is Argument) ? PCondIsNullA0XA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCondIsNullA0XQ.Make (predicate, consequent, (Quotation) alternative) :
                (alternative is StaticVariable) ? PCondIsNullA0XS.Make (predicate, consequent, (StaticVariable) alternative) :
                new PCondIsNullA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullA0");
#endif
            if (environment.Argument0Value == null) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsNullA0";
#endif
                expression = this.consequent;
                answer = null; // keep c# compiler happy
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsNullA0";
#endif
                expression = this.alternative;
                answer = null; // keep c# compiler happy
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsNullA0A : PCondIsNullA0
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int consequentOffset;

        protected PCondIsNullA0A (PrimitiveIsNullA0 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveIsNullA0 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondIsNullA0A0.Make (predicate, (Argument0) consequent, alternative) :
                 (consequent is Argument1) ? PCondIsNullA0A1.Make (predicate, (Argument1) consequent, alternative) :
                //: (alternative is LexicalVariable) ? PCondIsNullASL.Make (predicate, consequent, (LexicalVariable) alternative)
                //(alternative is Quotation) ? PCondIsNullA1XQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsNullA0A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullA0A");
#endif
            if (environment.Argument0Value == null) {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsNullA0A";
#endif
                expression = this.alternative;
                answer = null; // keep c# compiler happy
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsNullA0A0 : PCondIsNullA0A
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsNullA0A0 (PrimitiveIsNullA0 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsNullA0 predicate, Argument0 consequent, SCode alternative)
        {
            return
                //: (alternative is LexicalVariable) ? PCondIsNullASL.Make (predicate, consequent, (LexicalVariable) alternative)
                //(alternative is Quotation) ? PCondIsNullA1XQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsNullA0A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullA0A0");
#endif
            if (environment.Argument0Value == null) {
                answer = null;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsNullA0A0";
#endif
                expression = this.alternative;
                answer = null; // keep c# compiler happy
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsNullA0A1 : PCondIsNullA0A
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsNullA0A1 (PrimitiveIsNullA0 predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsNullA0 predicate, Argument1 consequent, SCode alternative)
        {
            return
                //: (alternative is LexicalVariable) ? PCondIsNullASL.Make (predicate, consequent, (LexicalVariable) alternative)
                //(alternative is Quotation) ? PCondIsNullA1XQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsNullA0A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullA0A1");
#endif
            if (environment.Argument0Value == null) {
                answer = environment.Argument1Value;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsNullA0A1";
#endif
                expression = this.alternative;
                answer = null; // keep c# compiler happy
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsNullA0Q : PCondIsNullA0
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;
        protected PCondIsNullA0Q (PrimitiveIsNullA0 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveIsNullA0 predicate, Quotation consequent, SCode alternative)
        {
            return
                //: (alternative is LexicalVariable) ? PCondIsNullASL.Make (predicate, consequent, (LexicalVariable) alternative)
                //(alternative is Quotation) ? PCondIsNullA1XQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsNullA0Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullA0Q");
#endif
            if (environment.Argument0Value == null) {
                answer = this.consequentValue;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsNullA0Q";
#endif
                expression = this.alternative;
                answer = null; // keep c# compiler happy
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsNullA0S : PCondIsNullA0
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol consequentName;
        public readonly int consequentOffset;

        protected PCondIsNullA0S (PrimitiveIsNullA0 predicate, StaticVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveIsNullA0 predicate, StaticVariable consequent, SCode alternative)
        {
            return
                //: (alternative is LexicalVariable) ? PCondIsNullASL.Make (predicate, consequent, (LexicalVariable) alternative)
                //(alternative is Quotation) ? PCondIsNullA1XQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsNullA0S (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullA0S");
#endif
            if (environment.Argument0Value == null) {
                if (environment.StaticValue (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsNullA0S";
#endif
                expression = this.alternative;
                answer = null; // keep c# compiler happy
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsNullA0XA : PCondIsNullA0
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int alternativeOffset;
        protected PCondIsNullA0XA (PrimitiveIsNullA0 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveIsNullA0 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCondIsNullA0XA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCondIsNullA0XA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCondIsNullA0XA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullA0XA");
#endif
            if (environment.Argument0Value == null) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsNullA0XA";
#endif
                expression = this.consequent;
                answer = null; // keep c# compiler happy
                return true;
            }
            else {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsNullA0XA0 : PCondIsNullA0XA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsNullA0XA0 (PrimitiveIsNullA0 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsNullA0 predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCondIsNullA0XA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullA0XA0");
#endif
            object ev0 = environment.Argument0Value;

            if (ev0 == null) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsNullA0XA0";
#endif
                expression = this.consequent;
                answer = null; // keep c# compiler happy
                return true;
            }
            else {
                answer = ev0;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsNullA0XA1 : PCondIsNullA0XA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsNullA0XA1 (PrimitiveIsNullA0 predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsNullA0 predicate, SCode consequent, Argument1 alternative)
        {
            return
                new PCondIsNullA0XA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullA0XA1");
#endif
            if (environment.Argument0Value == null) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsNullA0XA1";
#endif
                expression = this.consequent;
                answer = null; // keep c# compiler happy
                return true;
            }
            else {
                answer = environment.Argument1Value;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsNullA0XQ : PCondIsNullA0
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCondIsNullA0XQ (PrimitiveIsNullA0 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveIsNullA0 predicate, SCode consequent, Quotation alternative)
        {
            return
                //: (alternative is LexicalVariable) ? PCondIsNullASL.Make (predicate, consequent, (LexicalVariable) alternative)
                //(alternative is Quotation) ? PCondIsNullA1XQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsNullA0XQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullA0XQ");
#endif
            if (environment.Argument0Value == null) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsNullA0XQ";
#endif
                expression = this.consequent;
                answer = null; // keep c# compiler happy
                return true;
            }
            else {
                answer = this.alternativeValue;
                return false;

            }
        }
    }

    [Serializable]
    class PCondIsNullA0XS : PCondIsNullA0
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol alternativeName;
        public readonly int alternativeOffset;
        protected PCondIsNullA0XS (PrimitiveIsNullA0 predicate, SCode consequent, StaticVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveIsNullA0 predicate, SCode consequent, StaticVariable alternative)
        {
            return
                //: (alternative is LexicalVariable) ? PCondIsNullASL.Make (predicate, consequent, (LexicalVariable) alternative)
                //(alternative is Quotation) ? PCondIsNullA1XQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsNullA0XS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullA0XS");
#endif
            if (environment.Argument0Value == null) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsNullA0XQ";
#endif
                expression = this.consequent;
                answer = null; // keep c# compiler happy
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
    class PCondIsNullA1 : PCondIsNullA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsNullA1 (PrimitiveIsNullA1 predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsNullA1 predicate, SCode consequent, SCode alternative)
        {
            return
                (consequent is Argument) ? PCondIsNullA1A.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? PCondIsNullA1Q.Make (predicate, (Quotation) consequent, alternative) :
                (alternative is Argument) ? PCondIsNullA1XA.Make (predicate, consequent, (Argument) alternative) :
                (alternative is Quotation) ? PCondIsNullA1XQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsNullA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullA1");
#endif
            if (environment.Argument1Value == null) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsNullA1";
#endif
                expression = this.consequent;
                answer = null; // keep c# compiler happy
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsNullA1";
#endif
                expression = this.alternative;
                answer = null; // keep c# compiler happy
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsNullA1A : PCondIsNullA1
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int consequentOffset;
        protected PCondIsNullA1A (PrimitiveIsNullA1 predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveIsNullA1 predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondIsNullA1A0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? PCondIsNullA1A1.Make (predicate, (Argument1) consequent, alternative) :
                //: (alternative is LexicalVariable) ? PCondIsNullASL.Make (predicate, consequent, (LexicalVariable) alternative)
                //(alternative is Quotation) ? PCondIsNullA1XQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsNullA1A (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullA1A");
#endif
            if (environment.Argument1Value == null) {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsNullA1A";
#endif
                expression = this.alternative;
                answer = null; // keep c# compiler happy
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsNullA1A0 : PCondIsNullA1A
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsNullA1A0 (PrimitiveIsNullA1 predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsNullA1 predicate, Argument0 consequent, SCode alternative)
        {
            return

                //: (alternative is LexicalVariable) ? PCondIsNullASL.Make (predicate, consequent, (LexicalVariable) alternative)
                //(alternative is Quotation) ? PCondIsNullA1XQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsNullA1A0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullA1A0");
#endif
            if (environment.Argument1Value == null) {
                answer = environment.Argument0Value;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsNullA1A0";
#endif
                expression = this.alternative;
                answer = null; // keep c# compiler happy
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsNullA1A1 : PCondIsNullA1A
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsNullA1A1 (PrimitiveIsNullA1 predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsNullA1 predicate, Argument1 consequent, SCode alternative)
        {
            return

                //: (alternative is LexicalVariable) ? PCondIsNullASL.Make (predicate, consequent, (LexicalVariable) alternative)
                //(alternative is Quotation) ? PCondIsNullA1XQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsNullA1A1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullA1A1");
#endif
            object ev0 = environment.Argument1Value;

            if (ev0 == null) {
                answer = ev0;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsNullA1A1";
#endif
                expression = this.alternative;
                answer = null; // keep c# compiler happy
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsNullA1Q : PCondIsNullA1
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;
        protected PCondIsNullA1Q (PrimitiveIsNullA1 predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveIsNullA1 predicate, Quotation consequent, SCode alternative)
        {
            return
                //: (alternative is LexicalVariable) ? PCondIsNullASL.Make (predicate, consequent, (LexicalVariable) alternative)
                //(alternative is Quotation) ? PCondIsNullA1XQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsNullA1Q (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullA1Q");
#endif
            if (environment.Argument1Value == null) {
                answer = this.consequentValue;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsNullA1";
#endif
                expression = this.alternative;
                answer = null; // keep c# compiler happy
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsNullA1XA : PCondIsNullA1
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int alternativeOffset;
        protected PCondIsNullA1XA (PrimitiveIsNullA1 predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveIsNullA1 predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCondIsNullA1XA0.Make (predicate, consequent, (Argument0) alternative) :
                (alternative is Argument1) ? PCondIsNullA1XA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCondIsNullA1XA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullA1XA");
#endif
            if (environment.Argument1Value == null) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsNullA1XA";
#endif
                expression = this.consequent;
                answer = null; // keep c# compiler happy
                return true;
            }
            else {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsNullA1XA0 : PCondIsNullA1XA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsNullA1XA0 (PrimitiveIsNullA1 predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsNullA1 predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCondIsNullA1XA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullA1XA0");
#endif
            if (environment.Argument1Value == null) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsNullA1XA0";
#endif
                expression = this.consequent;
                answer = null; // keep c# compiler happy
                return true;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsNullA1XA1 : PCondIsNullA1XA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsNullA1XA1 (PrimitiveIsNullA1 predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsNullA1 predicate, SCode consequent, Argument1 alternative)
        {
            return
                new PCondIsNullA1XA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullA1XA1");
#endif
            object ev0 = environment.Argument1Value;

            if (ev0 == null) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsNullA1XA1";
#endif
                expression = this.consequent;
                answer = null; // keep c# compiler happy
                return true;
            }
            else {
                answer = ev0;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsNullA1XQ : PCondIsNullA1
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCondIsNullA1XQ (PrimitiveIsNullA1 predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveIsNullA1 predicate, SCode consequent, Quotation alternative)
        {
            return
                //: (alternative is LexicalVariable) ? PCondIsNullASL.Make (predicate, consequent, (LexicalVariable) alternative)
                //(alternative is Quotation) ? PCondIsNullA1XQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsNullA1XQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullA1XQ");
#endif
            if (environment.Argument1Value == null) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsNullA1XQ";
#endif
                expression = this.consequent;
                answer = null; // keep c# compiler happy
                return true;
            }
            else {
                answer = this.alternativeValue;
                return false;

            }
        }
    }

    [Serializable]
    class PCondIsNullAA : PCondIsNullA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int consequentOffset;

        protected PCondIsNullAA (PrimitiveIsNullA predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveIsNullA predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondIsNullAA0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? PCondIsNullAA1.Make (predicate, (Argument1) consequent, alternative) :
                new PCondIsNullAA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullAA");
#endif
            if (environment.ArgumentValue (this.predicateArgumentOffset) == null) {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsNullAA";
#endif
                expression = this.alternative;
                answer = null; // keep c# compiler happy
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsNullAA0 : PCondIsNullAA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsNullAA0 (PrimitiveIsNullA predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsNullA predicate, Argument0 consequent, SCode alternative)
        {
            return
                new PCondIsNullAA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullAA0");
#endif
            if (environment.ArgumentValue (this.predicateArgumentOffset) == null) {
                answer = environment.Argument0Value;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsNullAA0";
#endif
                expression = this.alternative;
                answer = null; // keep c# compiler happy
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsNullAA1 : PCondIsNullAA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsNullAA1 (PrimitiveIsNullA predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsNullA predicate, Argument1 consequent, SCode alternative)
        {
            return
                new PCondIsNullAA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullAA1");
#endif
            if (environment.ArgumentValue (this.predicateArgumentOffset) == null) {
                answer = environment.Argument1Value;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsNullAA1";
#endif
                expression = this.alternative;
                answer = null; // keep c# compiler happy
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsNullAQ : PCondIsNullA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;

        protected PCondIsNullAQ (PrimitiveIsNullA predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveIsNullA predicate, Quotation consequent, SCode alternative)
        {
            return
                //: (alternative is LexicalVariable) ? PCondIsNullASL.Make (predicate, consequent, (LexicalVariable) alternative)
                //: (alternative is Quotation) ? PCondIsNullASQ.Make (predicate, consequent, (Quotation) alternative)
                new PCondIsNullAQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullAQ");
#endif
            if (environment.ArgumentValue (this.predicateArgumentOffset) == null) {
                answer = this.consequentValue;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsNullAQ";
#endif
                expression = this.alternative;
                answer = null; // keep c# compiler happy
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsNullAXQ : PCondIsNullA
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;

        protected PCondIsNullAXQ (PrimitiveIsNullA predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveIsNullA predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondIsNullAXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullAXQ");
#endif
            if (environment.ArgumentValue (this.predicateArgumentOffset) == null) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsNullAXQ";
#endif
                expression = this.consequent;
                answer = null; // keep c# compiler happy
                return true;
            }
            else {
                answer = this.alternativeValue;
                return false;

            }
        }
    }

    [Serializable]
    class PCondIsNullQ : PCondIsNull
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object predicateArgumentValue;

        protected PCondIsNullQ (PrimitiveIsNullQ predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.predicateArgumentValue = predicate.randValue;
        }

        public static SCode Make (PrimitiveIsNullQ predicate, SCode consequent, SCode alternative)
        {
            return
                //(predicate is PrimitiveIsNullA0) ? PCondIsNullA0.Make ((PrimitiveIsNullA0) predicate, consequent, alternative) :
                //(predicate is PrimitiveIsNullA1) ? PCondIsNullA1.Make ((PrimitiveIsNullA1) predicate, consequent, alternative) :
                //: (consequent is LexicalVariable) ? PCondIsNullAL.Make (predicate, (LexicalVariable) consequent, alternative)
                //: (consequent is Quotation) ? PCondIsNullAQ.Make (predicate, (Quotation) consequent, alternative)
                //: (alternative is LexicalVariable) ? PCondIsNullASL.Make (predicate, consequent, (LexicalVariable) alternative)
                //: (alternative is Quotation) ? PCondIsNullASQ.Make (predicate, consequent, (Quotation) alternative)
                new PCondIsNullQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
            //#if DEBUG
            //            Warm ("-");
            //            SCode.location = "PCondIsNullA";
            //#endif
            //            if (environment.ArgumentValue (this.predicateArgumentOffset) == null) {
            //#if DEBUG
            //                SCode.location = "-";
            //                NoteCalls (this.alternative);
            //                alternativeTypeHistogram.Note (this.alternativeType);
            //                SCode.location = "PCondIsNullA";
            //#endif
            //                expression = this.alternative;
            //	        answer = null; // keep c# compiler happy
            //                return true;
            //            }
            //            else {
            //#if DEBUG
            //                SCode.location = "-";
            //                NoteCalls (this.consequent);
            //                consequentTypeHistogram.Note (this.consequentType);
            //                SCode.location = "PCondIsNullA";
            //#endif
            //                expression = this.consequent;
            //	        answer = null; // keep c# compiler happy
            //                return true;
            //            }
        }
    }

    [Serializable]
    class PCondIsNullS : PCondIsNull
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol predicateRandName;
        public readonly int predicateRandOffset;

        protected PCondIsNullS (PrimitiveIsNullS predicate, SCode consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.predicateRandName = predicate.rand0Name;
            this.predicateRandOffset = predicate.rand0Offset;
        }

        public static SCode Make (PrimitiveIsNullS predicate, SCode consequent, SCode alternative)
        {
            return
                //(predicate is PrimitiveIsNullA0) ? PCondIsNullA0.Make ((PrimitiveIsNullA0) predicate, consequent, alternative) :
                //(predicate is PrimitiveIsNullA1) ? PCondIsNullA1.Make ((PrimitiveIsNullA1) predicate, consequent, alternative) :
                (consequent is Argument) ? PCondIsNullSA.Make (predicate, (Argument) consequent, alternative) :
                (consequent is Quotation) ? PCondIsNullSQ.Make (predicate, (Quotation) consequent, alternative) :
                (consequent is StaticVariable) ? PCondIsNullSS.Make (predicate, (StaticVariable) consequent, alternative) :
                //: (alternative is LexicalVariable) ? PCondIsNullASL.Make (predicate, consequent, (LexicalVariable) alternative)
                (alternative is Quotation) ? PCondIsNullSXQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsNullS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullS");
#endif
            object ev0;
            if (environment.StaticValue (out ev0, this.predicateRandName, this.predicateRandOffset))
                throw new NotImplementedException ();

            if (ev0 == null) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsNullS";
#endif
                expression = this.consequent;
                answer = null; // keep c# compiler happy
                return true;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsNullS";
#endif
                expression = this.alternative;
                answer = null; // keep c# compiler happy
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsNullSA : PCondIsNullS
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int consequentOffset;

        protected PCondIsNullSA (PrimitiveIsNullS predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveIsNullS predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondIsNullSA0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? PCondIsNullSA1.Make (predicate, (Argument1) consequent, alternative) :
                new PCondIsNullSA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullSA");
#endif
            object ev0;
            if (environment.StaticValue (out ev0, this.predicateRandName, this.predicateRandOffset))
                throw new NotImplementedException ();

            if (ev0 == null) {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsNullSA";
#endif
                expression = this.alternative;
                answer = null; // keep c# compiler happy
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsNullSA0 : PCondIsNullSA
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsNullSA0 (PrimitiveIsNullS predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsNullS predicate, Argument0 consequent, SCode alternative)
        {
            return
                new PCondIsNullSA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullSA0");
#endif
            object ev0;
            if (environment.StaticValue (out ev0, this.predicateRandName, this.predicateRandOffset))
                throw new NotImplementedException ();

            if (ev0 == null) {
                answer = environment.Argument0Value;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsNullSA0";
#endif
                expression = this.alternative;
                answer = null; // keep c# compiler happy
                return true;
            }
        }
    }

    class PCondIsNullSA1 : PCondIsNullSA
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsNullSA1 (PrimitiveIsNullS predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsNullS predicate, Argument1 consequent, SCode alternative)
        {
            return
                new PCondIsNullSA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullSA1");
#endif
            object ev0;
            if (environment.StaticValue (out ev0, this.predicateRandName, this.predicateRandOffset))
                throw new NotImplementedException ();

            if (ev0 == null) {
                answer = environment.Argument1Value;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsNullSA1";
#endif
                expression = this.alternative;
                answer = null; // keep c# compiler happy
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsNullSQ : PCondIsNullS
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;

        protected PCondIsNullSQ (PrimitiveIsNullS predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveIsNullS predicate, Quotation consequent, SCode alternative)
        {
            return
                //: (alternative is LexicalVariable) ? PCondIsNullASL.Make (predicate, consequent, (LexicalVariable) alternative)
                //(alternative is Quotation) ? PCondIsNullSXQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsNullSQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullSQ");
#endif
            object ev0;
            if (environment.StaticValue (out ev0, this.predicateRandName, this.predicateRandOffset))
                throw new NotImplementedException ();

            if (ev0 == null) {
                answer = this.consequentValue;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsNullSQ";
#endif
                expression = this.alternative;
                answer = null; // keep c# compiler happy
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsNullSS : PCondIsNullS
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol consequentName;
        public readonly int consequentOffset;

        protected PCondIsNullSS (PrimitiveIsNullS predicate, StaticVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveIsNullS predicate, StaticVariable consequent, SCode alternative)
        {
            return
                //: (alternative is LexicalVariable) ? PCondIsNullASL.Make (predicate, consequent, (LexicalVariable) alternative)
                //(alternative is Quotation) ? PCondIsNullSXQ.Make (predicate, consequent, (Quotation) alternative) :
                new PCondIsNullSS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullSS");
#endif
            object ev0;
            if (environment.StaticValue (out ev0, this.predicateRandName, this.predicateRandOffset))
                throw new NotImplementedException ();

            if (ev0 == null) {
                if (environment.StaticValue (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsNullSS";
#endif
                expression = this.alternative;
                answer = null; // keep c# compiler happy
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsNullSXQ : PCondIsNullS
    {
#if DEBUG
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;

        protected PCondIsNullSXQ (PrimitiveIsNullS predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveIsNullS predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondIsNullSXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PCondIsNullSXQ");
#endif
            object ev0;
            if (environment.StaticValue (out ev0, this.predicateRandName, this.predicateRandOffset))
                throw new NotImplementedException ();

            if (ev0 == null) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsNullSXQ";
#endif
                expression = this.consequent;
                answer = null; // keep c# compiler happy
                return true;
            }
            else {
                answer = this.alternativeValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsNullXA : PCondIsNull
    {
#if DEBUG
        static Histogram<Type> arg0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int consequentOffset;

        protected PCondIsNullXA (PrimitiveIsNull predicate, Argument consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveIsNull predicate, Argument consequent, SCode alternative)
        {
            return
                (consequent is Argument0) ? PCondIsNullXA0.Make (predicate, (Argument0) consequent, alternative) :
                (consequent is Argument1) ? PCondIsNullXA1.Make (predicate, (Argument1) consequent, alternative) :
                new PCondIsNullXA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            arg0TypeHistogram.Note (this.arg0Type);
            SCode.location = "PCondIsNullXA";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsNullXA";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveIsNullFrame0 (this, closureEnvironment));
                //answer = Interpreter.Unwind;
                //closureEnvironment = env;
                //return false;
            }

            if (ev0 == null) {
                answer = environment.ArgumentValue (this.consequentOffset);
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsNullXA";
#endif
                expression = this.alternative;
                answer = null; // keep c# compiler happy
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsNullXA0 : PCondIsNullXA
    {
#if DEBUG
        static Histogram<Type> arg0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsNullXA0 (PrimitiveIsNull predicate, Argument0 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsNull predicate, Argument0 consequent, SCode alternative)
        {
            return
                new PCondIsNullXA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            arg0TypeHistogram.Note (this.arg0Type);
            SCode.location = "PCondIsNullXA0";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsNullXA0";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveIsNullFrame0 (this, closureEnvironment));
                //answer = Interpreter.Unwind;
                //closureEnvironment = env;
                //return false;
            }

            if (ev0 == null) {
                answer = environment.Argument0Value;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsNullXA0";
#endif
                expression = this.alternative;
                answer = null; // keep c# compiler happy
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsNullXA1 : PCondIsNullXA
    {
#if DEBUG
        static Histogram<Type> arg0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsNullXA1 (PrimitiveIsNull predicate, Argument1 consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsNull predicate, Argument1 consequent, SCode alternative)
        {
            return
                new PCondIsNullXA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            arg0TypeHistogram.Note (this.arg0Type);
            SCode.location = "PCondIsNullXA1";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsNullXA1";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveIsNullFrame0 (this, closureEnvironment));
                //answer = Interpreter.Unwind;
                //closureEnvironment = env;
                //return false;
            }

            if (ev0 == null) {
                answer = environment.Argument1Value;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsNullXA1";
#endif
                expression = this.alternative;
                answer = null; // keep c# compiler happy
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsNullXQ : PCondIsNull
    {
#if DEBUG
        static Histogram<Type> arg0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object consequentValue;
        protected PCondIsNullXQ (PrimitiveIsNull predicate, Quotation consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentValue = consequent.Quoted;
        }

        public static SCode Make (PrimitiveIsNull predicate, Quotation consequent, SCode alternative)
        {
            return
                new PCondIsNullXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            arg0TypeHistogram.Note (this.arg0Type);
            SCode.location = "PCondIsNullXQ";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsNullXQ";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveIsNullFrame0 (this, closureEnvironment));
                //answer = Interpreter.Unwind;
                //closureEnvironment = env;
                //return false;
            }

            if (ev0 == null) {
                answer = this.consequentValue;
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsNullXQ";
#endif
                expression = this.alternative;
                answer = null; // keep c# compiler happy
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsNullXS : PCondIsNull
    {
#if DEBUG
        static Histogram<Type> arg0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol consequentName;
        public readonly int consequentOffset;

        protected PCondIsNullXS (PrimitiveIsNull predicate, StaticVariable consequent, SCode alternative)
            : base (predicate, consequent, alternative)
        {
            this.consequentName = consequent.Name;
            this.consequentOffset = consequent.Offset;
        }

        public static SCode Make (PrimitiveIsNull predicate, StaticVariable consequent, SCode alternative)
        {
            return
                new PCondIsNullXS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            arg0TypeHistogram.Note (this.arg0Type);
            SCode.location = "PCondIsNullXS";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsNullXS";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveIsNullFrame0 (this, closureEnvironment));
                //answer = Interpreter.Unwind;
                //closureEnvironment = env;
                //return false;
            }

            if (ev0 == null) {
                if (environment.StaticValue (out answer, this.consequentName, this.consequentOffset))
                    throw new NotImplementedException ();
                return false;
            }
            else {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.alternative);
                alternativeTypeHistogram.Note (this.alternativeType);
                SCode.location = "PCondIsNullXS";
#endif
                expression = this.alternative;
                answer = null; // keep c# compiler happy
                return true;
            }
        }
    }

    [Serializable]
    class PCondIsNullXXA : PCondIsNull
    {
#if DEBUG
        static Histogram<Type> arg0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int alternativeOffset;

        protected PCondIsNullXXA (PrimitiveIsNull predicate, SCode consequent, Argument alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveIsNull predicate, SCode consequent, Argument alternative)
        {
            return
                (alternative is Argument0) ? PCondIsNullXXA0.Make (predicate, consequent, (Argument0) alternative) :
                 (alternative is Argument1) ? PCondIsNullXXA1.Make (predicate, consequent, (Argument1) alternative) :
                new PCondIsNullXXA (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            arg0TypeHistogram.Note (this.arg0Type);
            SCode.location = "PCondIsNullXXA";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsNullXXA";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveIsNullFrame0 (this, closureEnvironment));
                //answer = Interpreter.Unwind;
                //closureEnvironment = env;
                //return false;
            }

            if (ev0 == null) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsNullXXA";
#endif
                expression = this.consequent;
                answer = null; // keep c# compiler happy
                return true;
            }
            else {
                answer = environment.ArgumentValue (this.alternativeOffset);
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsNullXXA0 : PCondIsNullXXA
    {
#if DEBUG
        static Histogram<Type> arg0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly int alternativeOffset;

        protected PCondIsNullXXA0 (PrimitiveIsNull predicate, SCode consequent, Argument0 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsNull predicate, SCode consequent, Argument0 alternative)
        {
            return
                new PCondIsNullXXA0 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            arg0TypeHistogram.Note (this.arg0Type);
            SCode.location = "PCondIsNullXXA0";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsNullXXA0";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveIsNullFrame0 (this, closureEnvironment));
                //answer = Interpreter.Unwind;
                //closureEnvironment = env;
                //return false;
            }

            if (ev0 == null) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsNullXXA0";
#endif
                expression = this.consequent;
                answer = null; // keep c# compiler happy
                return true;
            }
            else {
                answer = environment.Argument0Value;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsNullXXA1 : PCondIsNullXXA
    {
#if DEBUG
        static Histogram<Type> arg0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        protected PCondIsNullXXA1 (PrimitiveIsNull predicate, SCode consequent, Argument1 alternative)
            : base (predicate, consequent, alternative)
        {
        }

        public static SCode Make (PrimitiveIsNull predicate, SCode consequent, Argument1 alternative)
        {
            return
                new PCondIsNullXXA1 (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            arg0TypeHistogram.Note (this.arg0Type);
            SCode.location = "PCondIsNullXXA1";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsNullXXA1";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveIsNullFrame0 (this, closureEnvironment));
                //answer = Interpreter.Unwind;
                //closureEnvironment = env;
                //return false;
            }

            if (ev0 == null) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsNullXXA1";
#endif
                expression = this.consequent;
                answer = null; // keep c# compiler happy
                return true;
            }
            else {
                answer = environment.Argument1Value;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsNullXXQ : PCondIsNull
    {
#if DEBUG
        static Histogram<Type> arg0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly object alternativeValue;
        protected PCondIsNullXXQ (PrimitiveIsNull predicate, SCode consequent, Quotation alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeValue = alternative.Quoted;
        }

        public static SCode Make (PrimitiveIsNull predicate, SCode consequent, Quotation alternative)
        {
            return
                new PCondIsNullXXQ (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            arg0TypeHistogram.Note (this.arg0Type);
            SCode.location = "PCondIsNullXXQ";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsNullXXQ";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveIsNullFrame0 (this, closureEnvironment));
                //answer = Interpreter.Unwind;
                //closureEnvironment = env;
                //return false;
            }

            if (ev0 == null) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsNullXXQ";
#endif
                expression = this.consequent;
                answer = null; // keep c# compiler happy
                return true;
            }
            else {
                answer = this.alternativeValue;
                return false;
            }
        }
    }

    [Serializable]
    class PCondIsNullXXS : PCondIsNull
    {
#if DEBUG
        static Histogram<Type> arg0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> consequentTypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol alternativeName;
        public readonly int alternativeOffset;

        protected PCondIsNullXXS (PrimitiveIsNull predicate, SCode consequent, StaticVariable alternative)
            : base (predicate, consequent, alternative)
        {
            this.alternativeName = alternative.Name;
            this.alternativeOffset = alternative.Offset;
        }

        public static SCode Make (PrimitiveIsNull predicate, SCode consequent, StaticVariable alternative)
        {
            return
                new PCondIsNullXXS (predicate, consequent, alternative);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            arg0TypeHistogram.Note (this.arg0Type);
            SCode.location = "PCondIsNullXXS";
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
#if DEBUG
            SCode.location = "PCondIsNullXXS";
#endif
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new PrimitiveIsNullFrame0 (this, closureEnvironment));
                //answer = Interpreter.Unwind;
                //closureEnvironment = env;
                //return false;
            }

            if (ev0 == null) {
#if DEBUG
                SCode.location = "-";
                NoteCalls (this.consequent);
                consequentTypeHistogram.Note (this.consequentType);
                SCode.location = "PCondIsNullXXS";
#endif
                expression = this.consequent;
                answer = null; // keep c# compiler happy
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


using System;
using System.Diagnostics;

namespace Microcode
{
    [Serializable]
    class POrIsType<SType> : POr1
    {
#if DEBUG
        static Histogram<Type> arg0TypeHistogram = new Histogram<Type>();
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type>();
#endif

        protected POrIsType(PrimitiveIsType<SType> predicate, SCode alternative)
            : base(predicate, alternative)
        {
        }

        public static SCode Make(PrimitiveIsType<SType> predicate, SCode alternative)
        {
            return
                (predicate is PrimitiveIsTypeA<SType>) ? POrIsTypeA<SType>.Make((PrimitiveIsTypeA<SType>)predicate, alternative) :
                 new POrIsType<SType>(predicate, alternative);
        }

        public override bool EvalStep(out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("POrIsType");
            NoteCalls(this.arg0);
            arg0TypeHistogram.Note(this.arg0Type);
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0;
            while (unev0.EvalStep(out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack)
            {
                throw new NotImplementedException();
                //((UnwinderState) env).AddFrame (new PrimitiveIsTypeFrame0 (this, environment));
                //answer = Interpreter.UnwindStack;
                //environment = env;
                //return false;
            }
#if DEBUG
            SCode.location = "POrIsType";
#endif
            if (ev0 is SType)
            {
                answer = Constant.sharpT;
                return false;
            }
            else
            {
#if DEBUG
                NoteCalls(this.alternative);
                alternativeTypeHistogram.Note(this.alternativeType);
#endif
                expression = this.alternative;
                answer = null;
                return true;
            }
        }
    }

    [Serializable]
    class POrIsTypeA<SType> : POrIsType<SType>
    {
        public readonly int predicateOffset;
        protected POrIsTypeA(PrimitiveIsTypeA<SType> predicate, SCode alternative)
            : base(predicate, alternative)
        {
            this.predicateOffset = predicate.offset;
        }

        public static SCode Make(PrimitiveIsTypeA<SType> predicate, SCode alternative)
        {
            return
                (predicate is PrimitiveIsTypeA0<SType>) ? POrIsTypeA0<SType>.Make((PrimitiveIsTypeA0<SType>)predicate, alternative) :
                new POrIsTypeA<SType>(predicate, alternative);
        }

        public override bool EvalStep(out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("POrIsTypeA");
#endif
            if (environment.ArgumentValue(this.predicateOffset) is SType)
            {
                answer = Constant.sharpT;
                return false;
            }
            else
            {
                expression = this.alternative;
                answer = null;
#if DEBUG
                NoteCalls((SCode)expression);
#endif
                return true;
            }
        }
    }

    [Serializable]
    class POrIsTypeA0<SType> : POrIsTypeA<SType>
    {
#if DEBUG
        static Histogram<Type> alternativeTypeHistogram = new Histogram<Type>();
#endif
        protected POrIsTypeA0(PrimitiveIsTypeA0<SType> predicate, SCode alternative)
            : base(predicate, alternative)
        {
        }

        internal static SCode Make(PrimitiveIsTypeA0<SType> predicate, SCode alternative)
        {
            return
               new POrIsTypeA0<SType>(predicate, alternative);
        }

        public override bool EvalStep(out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm("POrIsTypeA0");
#endif
            if (environment.Argument0Value is SType)
            {
                answer = Constant.sharpT;
                return false;
            }
            else
            {
#if DEBUG
                SCode.location = "-";
                NoteCalls((SCode)expression);
                alternativeTypeHistogram.Note(this.alternativeType);
                SCode.location = "POrIsTypeA0";
#endif
                expression = this.alternative;
                answer = null;   
                return true;
            }
        }
    }
}

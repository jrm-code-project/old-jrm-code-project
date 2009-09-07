using System;
using System.Diagnostics;

namespace Microcode
{
    [Serializable]
    class Promise : SchemeObject, ISystemPair
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public override TC TypeCode { get { return TC.DELAYED; } }

        object bodyOrValue;
        object environmentOrTrue;

        public Promise (SCode body, object environment)
        {
            this.bodyOrValue = body;
            this.environmentOrTrue = environment;
        }

        public bool IsForced
        {
            get
            {
                return ((this.environmentOrTrue is bool) && (bool) (this.environmentOrTrue));
            }
        }

        public SCode Expression
        {
            get
            {
                return (SCode) this.bodyOrValue;
            }
            set
            {
                this.bodyOrValue = value;
            }
        }

        public Environment Environment
        {
            get
            {
                return (Environment) this.environmentOrTrue;
            }
            set
            {
                this.environmentOrTrue = value;
            }
        }

        public object Value
        {
            get
            {
                return this.bodyOrValue;
            }

            set
            {
                    this.bodyOrValue = value;
                    this.environmentOrTrue = true;
            }
        }

        public object SystemPairCar
        {
            get
            {
                return this.environmentOrTrue;
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        public object SystemPairCdr
        {
            get
            {
                return this.bodyOrValue;
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        [SchemePrimitive ("DELAYED?", 1, true)]
        public static bool IsDelayed (out object answer, object arg)
        {
            answer = arg is Promise;
            return false;
        }

        [SchemePrimitive ("FORCE", 1, false)]
        public static bool Force (out object answer, object arg)
        {
            Promise p = (Promise) arg;
            if (p.IsForced)
                answer = p.bodyOrValue;
            else {
                object value = null;
                Control expression = (SCode) p.bodyOrValue;
                Environment env = (Environment) p.environmentOrTrue;
                while (expression.EvalStep (out value, ref expression, ref env)) { };
                if (value == Interpreter.UnwindStack) throw new NotImplementedException ();
                p.bodyOrValue = value;
                p.environmentOrTrue = true;
                answer = value;
            }
            return false;
        }
    }
}

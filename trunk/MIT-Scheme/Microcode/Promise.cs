using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    class Promise : ISystemPair
    {
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

        [SchemePrimitive ("DELAYED?", 1)]
        public static object IsDelayed (Interpreter interpreter, object arg)
        {
            return interpreter.Return (arg is Promise);
        }

        [SchemePrimitive ("FORCE", 1)]
        public static object Force (Interpreter interpreter, object arg)
        {
            Promise p = (Promise) (arg);
            if (p.IsForced)
                return interpreter.Return (p.Value);
            else
                return interpreter.EvalNewSubproblem (p.Expression, p.Environment, new MemoizePromise (interpreter.Continuation, p));
        }

        sealed class MemoizePromise : Continuation
        {
            readonly Promise p;

            public MemoizePromise (Continuation next, Promise p)
                : base (next)
            {
                this.p = p;
            }
 
            internal override object Invoke (Interpreter interpreter, object value)
            {
                if (p.IsForced)
                    return interpreter.Return (p.Value);
                else
                {
                    p.Value = value;
                    return interpreter.Return (value);
                }
            }

            public override int FrameSize
            {
                get { throw new NotImplementedException (); }
            }
        }




    }
}

using System;
using System.Collections.Generic;
using System.Diagnostics;

namespace Microcode
{
    class Entity : SCode, ISystemPair
    {
        object first;
        object second;

        public Entity (object first, object second)
        {
            this.first = first;
            this.second = second;
        }

        internal override object EvalStep (Interpreter interpreter, object etc)
        {
            object [] arguments = interpreter.Arguments;
            object [] newArglist = new object [arguments.Length + 1];
            newArglist [0] = this.second;
            for (int i = 0; i < arguments.Length; i++)
                newArglist [i + 1] = arguments [i];
            return interpreter.Apply (this.first, newArglist);
        }

        #region ISystemPair Members

        public object SystemPairCar
        {
            get
            {
                return this.first ;
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
                return this.second;
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        #endregion

        [SchemePrimitive ("ENTITY?", 1)]
        public static object IsEntity (Interpreter interpreter, object arg)
        {
            return interpreter.Return (arg is Entity);
        }
    }
}

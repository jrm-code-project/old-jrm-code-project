using System;
using System.Collections.Generic;
using System.Diagnostics;

namespace Microcode
{
    sealed class Entity : SchemeObject, IApplicable, ISystemPair
    {
#if DEBUG
        static long applicationCount = 0;
#endif
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        object first;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        object second;

        public Entity (object first, object second)
            : base (TC.ENTITY)
        {
            this.first = first;
            this.second = second;
        }

        //[SchemePrimitive ("ENTITY?", 1)]
        //public static PartialResult IsEntity (object arg)
        //{
        //    return new PartialResult (arg is Entity);
        //}

        #region IApplicable Members

        public bool Apply (out object answer, ref SCode expression, ref Environment environment, object [] args)
        {
#if DEBUG
            Entity.applicationCount += 1;
#endif
            object [] adjustedArgs = new object [args.Length + 1];
            Array.Copy (args, 0, adjustedArgs, 1, args.Length);
            adjustedArgs [0] = this.first;
            return Interpreter.Apply (out answer, ref expression, ref environment, this.first, adjustedArgs);
        }

        public bool Call (out object answer, ref SCode expression, ref Environment environment)
        {
#if DEBUG
            Entity.applicationCount += 1;
#endif
            return Interpreter.Apply (out answer, ref expression, ref environment, this.first, new object [] { this.first, this });
        }

        public bool Call (out object answer, ref SCode expression, ref Environment environment, object arg0)
        {
#if DEBUG
            Entity.applicationCount += 1;
#endif
            return Interpreter.Apply (out answer, ref expression, ref environment, this.first, new object [] { this.first, this, arg0 });
        }

        public bool Call (out object answer, ref SCode expression, ref Environment environment, object arg0, object arg1)
        {
#if DEBUG
            Entity.applicationCount += 1;
#endif
            return Interpreter.Apply (out answer, ref expression, ref environment, this.first, new object [] { this.first, this, arg0, arg1 });
        }

        #endregion

        #region ISystemPair Members

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemPairCar
        {
            [DebuggerStepThrough]
            get
            {
                return this.first ;
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemPairCdr
        {
            [DebuggerStepThrough]
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
    }
}

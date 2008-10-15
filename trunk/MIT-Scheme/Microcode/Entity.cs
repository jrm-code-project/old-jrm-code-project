using System;
using System.Collections.Generic;
using System.Diagnostics;

namespace Microcode
{
    [Serializable]
    sealed class Entity : SchemeObject, IApplicable, ISystemPair
    {
#if DEBUG
        [NonSerialized]
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

        [SchemePrimitive ("ENTITY?", 1, true)]
        public static bool IsEntity (out object answer, object arg)
        {
            answer = arg is Entity ;
            return false;
        }

        #region IApplicable Members

        public bool Apply (out object answer, ref Control expression, ref Environment environment, object [] args)
        {
#if DEBUG
            Entity.applicationCount += 1;
#endif
            object [] adjustedArgs = new object [args.Length + 1];
            Array.Copy (args, 0, adjustedArgs, 1, args.Length);
            adjustedArgs [0] = this.first;
            return Interpreter.Apply (out answer, ref expression, ref environment, this.first, adjustedArgs);
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Entity.applicationCount += 1;
#endif
            return Interpreter.Call (out answer, ref expression, ref environment, this.first,  this );
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0)
        {
#if DEBUG
            Entity.applicationCount += 1;
#endif
            return Interpreter.Call (out answer, ref expression, ref environment, this.first, this, arg0);
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1)
        {
#if DEBUG
            Entity.applicationCount += 1;
#endif
            return Interpreter.Call (out answer, ref expression, ref environment, this.first, this, arg0, arg1);
        }


        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2)
        {
#if DEBUG
            Entity.applicationCount += 1;
#endif
            return Interpreter.Call (out answer, ref expression, ref environment, this.first, this, arg0, arg1, arg2);

        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3)
        {
#if DEBUG
            Entity.applicationCount += 1;
#endif
            return Interpreter.Call (out answer, ref expression, ref environment, this.first, this, arg0, arg1, arg2, arg3);
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3, object arg4)
        {
            throw new NotImplementedException ();
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3, object arg4, object arg5)
        {
            throw new NotImplementedException ();
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

        #region IApplicable Members

        #endregion
    }
}

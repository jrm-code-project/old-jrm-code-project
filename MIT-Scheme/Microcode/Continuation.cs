using System;
using System.Diagnostics;

namespace Microcode
{
    abstract class Continuation : ISystemVector
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly Continuation parent;

        [DebuggerStepThrough]
        internal Continuation (Continuation parent)
        {
            this.parent = parent;
        }

        public Continuation Parent
        {
            [DebuggerStepThrough]
            get
            {
                return this.parent;
            }
        }

        public abstract int FrameSize
        {
            get;
        }


        public virtual object FrameRef (int offset)
        {
            throw new NotImplementedException ();
        }

        internal abstract object Invoke (Interpreter interpreter, object value);

        #region ISystemVector Members
        public virtual int SystemVectorSize
        {
            get
            {
                return FrameSize + Parent.SystemVectorSize;
            }
        }

        public object SystemVectorRef (int offset)
        {
            if (offset < FrameSize)
                return FrameRef (offset);
            else
                return Parent.SystemVectorRef (offset - FrameSize);
        }

        public object SystemVectorSet (int offset, object newValue)
        {
            throw new NotImplementedException ();
        }

        #endregion
    }

    abstract class Subproblem<T> : Continuation
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly T expression;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly Environment environment;

        [DebuggerStepThrough]
        public Subproblem (Continuation next, T expression, Environment environment)
            : base (next)
        {
            this.expression = expression;
            this.environment = environment;
        }

        public T Expression
        {
            [DebuggerStepThrough]
            get
            {
                return this.expression;
            }
        }

        public Environment Environment
        {
            [DebuggerStepThrough]
            get
            {
                return this.environment;
            }
        }
    }
}

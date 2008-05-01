using System;
using System.Diagnostics;

namespace Microcode
{
    abstract class Continuation
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly Continuation parent;

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

        internal abstract object Invoke (Interpreter interpreter, object value);
    }

    abstract class Subproblem<T> : Continuation
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly T expression;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly Environment environment;

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

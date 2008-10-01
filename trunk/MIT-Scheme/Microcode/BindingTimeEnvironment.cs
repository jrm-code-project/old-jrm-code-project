using System;
using System.Diagnostics;
using System.Collections.Generic;
using System.Text;

namespace Microcode
{
 /// <summary>
 /// Simulates the effect of looking up a variable.  We can often find shortcuts
 /// to the full deep search and thus speed up the interpreter.
 /// </summary>
    public abstract class BindingTimeEnvironment
    {
        protected BindingTimeEnvironment ()
        {
        }

        internal abstract Variable BindLexical (string name, int depth);

        internal abstract Variable BindVariable (string name);

        internal abstract Variable BindShadow (string name, int depth, int shadowingFrame);

        internal BindingTimeEnvironment Extend (SimpleLambda lambda)
        {
            return new SimpleLambdaBindingEnvironment (lambda, this);
        }

        internal BindingTimeEnvironment Extend (StandardLambda lambda)
        {
            return new StandardLambdaBindingEnvironment (lambda, this);
        }

        internal BindingTimeEnvironment Extend (StaticLambda lambda)
        {
            return new StaticLambdaBindingEnvironment (lambda, this);
        }

        internal BindingTimeEnvironment Extend (ExtendedLambda lambda)
        {
            return new ExtendedLambdaBindingEnvironment (lambda, this);
        }
    }

    sealed public class RootBindingEnvironment : BindingTimeEnvironment
    {
        readonly Environment bindingEnvironment;

        public RootBindingEnvironment (Environment bindingEnvironment)
        {
            this.bindingEnvironment = bindingEnvironment;
        }

        internal override Variable BindVariable (string name)
        {
            ClosureBase closure = this.bindingEnvironment.Closure;

            if (closure == null)
                // We know it can only be in the global environment.
                return GlobalVariable.Make (name, this.bindingEnvironment);
            else {
                int offset = this.bindingEnvironment.Closure.FormalOffset (name);
                // If it isn't bound in the binding time environment, then we
                // call it `Free' and search the incrementals and then the environment.
                return (offset != -1)
                    ? Argument.Make (name, offset)
                    : FreeVariable.Make (name, bindingEnvironment);
            }
        }

        internal override Variable BindShadow (string name, int depth, int shadowingFrame)
        {
            ClosureBase closure = this.bindingEnvironment.Closure;
            if (closure == null) {
                // Same as free.  We have no clue where the variable is.
                // Hope it shows up before we need it, though.
                return DangerousFreeVariable.Make (name, shadowingFrame, depth);
            }
            else {
                int offset = closure.FormalOffset (name);
                return (offset == -1)
                    ? DangerousFreeVariable.Make (name, shadowingFrame, depth)
                    : DangerousLexicalVariable.Make (name, shadowingFrame, depth, offset);
            }
        }

        internal override Variable BindLexical (string name, int depth)
        {
            ClosureBase closure = this.bindingEnvironment.Closure;
            if (closure == null) {
                // Root environment and cannot be shadowed.
                return GlobalVariable.Make (name, this.bindingEnvironment);
            }
            else {
                int offset = closure.FormalOffset (name);
                return (offset == -1)
                    ? FreeVariable.Make (name, this.bindingEnvironment)
                    : TopLevelVariable.Make (name, this.bindingEnvironment.GetValueCell (name));
            }
        }
    }

    abstract class LambdaBindingEnvironment : BindingTimeEnvironment
    {
        protected readonly Lambda lambda;
        protected readonly BindingTimeEnvironment parent;

        public LambdaBindingEnvironment (Lambda lambda, BindingTimeEnvironment parent)
        {
            this.lambda = lambda;
            this.parent = parent;
        }
    }

    abstract class SafeLambdaBindingEnvironment : LambdaBindingEnvironment
    {
        protected SafeLambdaBindingEnvironment (Lambda lambda, BindingTimeEnvironment parent)
            : base (lambda, parent)
        { }

        internal override Variable BindVariable (string name)
        {
            int offset = this.lambda.LexicalOffset (name);
            return (offset == -1)
                ? parent.BindLexical (name, 1)  // Search deeper.
                : Argument.Make (name, offset);
        }

        internal override Variable BindShadow (string name, int depth, int shadowingFrame)
        {
            int offset = this.lambda.LexicalOffset (name);
            return (offset == -1)
                ? parent.BindShadow (name, depth + 1, shadowingFrame)
                : DangerousLexicalVariable.Make (name, shadowingFrame, depth, offset);
        }

        internal override Variable BindLexical (string name, int depth)
        {
            int offset = this.lambda.LexicalOffset (name);
            return (offset == -1)
              ? parent.BindLexical (name, depth + 1)  // Search deeper.
              : LexicalVariable.Make (name, depth, offset);
        }
    }

    sealed class SimpleLambdaBindingEnvironment : SafeLambdaBindingEnvironment
    {
        public SimpleLambdaBindingEnvironment (SimpleLambda lambda, BindingTimeEnvironment parent)
            : base (lambda, parent)
        {
        }
    }

    sealed class StaticLambdaBindingEnvironment : SafeLambdaBindingEnvironment
    {
        public StaticLambdaBindingEnvironment (StaticLambda lambda, BindingTimeEnvironment parent)
            : base (lambda, parent)
        {
        }
    }

    sealed class StandardLambdaBindingEnvironment : LambdaBindingEnvironment
    {
        public StandardLambdaBindingEnvironment (StandardLambda lambda, BindingTimeEnvironment parent)
            : base (lambda, parent)
        {
        }

        internal override Variable BindLexical (string name, int depth)
        {
            int offset = this.lambda.LexicalOffset (name);
            return (offset == -1)
                ? parent.BindShadow (name, depth + 1, depth) // ***
                : LexicalVariable.Make (name, depth, offset);
        }

        internal override Variable BindShadow (string name, int depth, int shadowingFrame)
        {
            int offset = this.lambda.LexicalOffset (name);
            return (offset == -1)
                ? parent.BindShadow (name, depth + 1, shadowingFrame)
                : DangerousLexicalVariable.Make (name, shadowingFrame, depth, offset);
        }

        internal override Variable BindVariable (string name)
        {
            int offset = this.lambda.LexicalOffset (name);
            return (offset == -1)
                ? parent.BindShadow (name, 1, 0)
                : Argument.Make (name, offset);
        }
    }

    sealed class ExtendedLambdaBindingEnvironment : BindingTimeEnvironment
    {
        readonly ExtendedLambda lambda;
        readonly BindingTimeEnvironment parent;

        public ExtendedLambdaBindingEnvironment (ExtendedLambda lambda, BindingTimeEnvironment parent)
        {
            this.lambda = lambda;
            this.parent = parent;
        }
 
        internal override Variable BindLexical (string name, int depth)
        {
            int offset = this.lambda.LexicalOffset (name);
            return (offset == -1)
                ? parent.BindShadow (name, depth + 1, depth)
                : LexicalVariable.Make (name, depth, offset);
        }

        internal override Variable BindShadow (string name, int depth, int shadowingFrame)
        {
            int offset = this.lambda.LexicalOffset (name);
            return (offset == -1)
                ? parent.BindShadow (name, depth + 1, shadowingFrame)
                : DangerousLexicalVariable.Make (name, shadowingFrame, depth, offset);
        }

        internal override Variable BindVariable (string name)
        {
            int offset = this.lambda.LexicalOffset (name);
            return (offset == -1)
                ? parent.BindShadow (name, 1, 0)
                : Argument.Make (name, offset); 
        }
    }
}

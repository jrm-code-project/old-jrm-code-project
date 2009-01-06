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
 /// 
    public abstract class LexicalMap
    {
        internal LexicalMap Extend (LambdaBase lambda)
        {
            return new LambdaLexicalMap (lambda, this);
        }
        internal abstract LexicalMap AddSubstitution (Symbol name, BoundVariable value);
        public  static LexicalMap Make (Environment environment)
        {
            return (environment is GlobalEnvironment)
                ? (LexicalMap) new GlobalLexicalMap (environment)
                : (LexicalMap) new TopLevelLexicalMap (environment);
        }
        internal abstract BoundVariable Bind (Symbol name);
        //internal abstract BoundVariable SimulateDangerousLookup (object lambdaName, int safeDepth, int randDepth);
        //internal abstract BoundVariable SimulateLookup (object lambdaName);
        //internal abstract BoundVariable SimulateStaticLookup (object lambdaName, int randDepth);
    }

    /// <summary>
    /// A lexical map derived from a real live environment.  We can simply
    /// ask where the variable is.
    /// </summary>
    sealed class GlobalLexicalMap : LexicalMap
    {
        readonly Environment bindingEnvironment;
        Dictionary<Symbol,BoundVariable> bindingCache = new Dictionary<Symbol, BoundVariable> ();

        internal GlobalLexicalMap (Environment bindingEnvironment)
        {
            this.bindingEnvironment = bindingEnvironment;
        }


        //internal override BoundVariable SimulateLookup (object lambdaName)
        //{
        //    return bindingEnvironment.SimulateLookup (lambdaName);
        //}

        //internal override BoundVariable SimulateStaticLookup (object lambdaName, int randDepth)
        //{
        //    return bindingEnvironment.SimulateStaticLookup (lambdaName, randDepth);
        //}

        //internal override BoundVariable SimulateDangerousLookup (object lambdaName, int safeDepth, int randDepth)
        //{
        //    return bindingEnvironment.SimulateDangerousLookup (lambdaName, safeDepth, randDepth);
        //}

        internal override BoundVariable Bind (Symbol name)
        {
            BoundVariable answer;
            if (bindingCache.TryGetValue (name, out answer))
                return answer;

            if (Configuration.EnableGlobalBinding) {
                answer = new GlobalVariable (name, this.bindingEnvironment);
                bindingCache.Add (name, answer);
                return answer;
            }
            else {
                answer = new FreeVariable (name);
                bindingCache.Add (name, answer);
                return answer;
            }
                
        }

        internal override LexicalMap AddSubstitution (Symbol name, BoundVariable value)
        {
            throw new NotImplementedException ();
        }
    }


    /// <summary>
    /// A lexical map derived from a real live environment.  We can simply
    /// ask where the variable is.
    /// </summary>
    sealed class TopLevelLexicalMap : LexicalMap
    {
        readonly Environment bindingEnvironment;
        Dictionary<object,BoundVariable> bindingCache = new Dictionary<object, BoundVariable> ();

           internal TopLevelLexicalMap (Environment bindingEnvironment)
            {
                this.bindingEnvironment = bindingEnvironment;
            }


           //internal override BoundVariable SimulateLookup (object lambdaName)
           //{
           //    return bindingEnvironment.SimulateLookup (lambdaName);
           //}

           //internal override BoundVariable SimulateStaticLookup (object lambdaName, int randDepth)
           //{
           //    return bindingEnvironment.SimulateStaticLookup (lambdaName, randDepth);
           //}

           //internal override BoundVariable SimulateDangerousLookup (object lambdaName, int safeDepth, int randDepth)
           //{
           //    return bindingEnvironment.SimulateDangerousLookup (lambdaName, safeDepth, randDepth);
           //}

           internal override BoundVariable Bind (Symbol name)
           {
               BoundVariable answer;
               if (bindingCache.TryGetValue (name, out answer))
                   return answer;

               int offset = this.bindingEnvironment.closure.FormalOffset (name);
               if (offset != -1) {
                   answer = new TopLevelVariable (name, this.bindingEnvironment.GetValueCell (name));
                   bindingCache.Add (name, answer);
                   return answer;
               }
               answer = new DeepVariable (name, this.bindingEnvironment);
               bindingCache.Add (name, answer);
               return answer;

           }

           internal override LexicalMap AddSubstitution (Symbol name, BoundVariable value)
           {
               throw new NotImplementedException ();
           }
    }

    /// <summary>
    /// A lexical map formed by simulating lambda application.
    /// We know where the variable is modulo the incrementals, etc.
    /// </summary>
    sealed class LambdaLexicalMap : LexicalMap
    {
        readonly LambdaBase lambda;
        readonly LexicalMap parent;
        Dictionary<Symbol,BoundVariable> bindingCache = new Dictionary<Symbol, BoundVariable> ();
        internal LambdaLexicalMap (LambdaBase lambda, LexicalMap parent)
        {
            this.lambda = lambda;
            this.parent = parent;
        }


        //internal override BoundVariable SimulateLookup (object lambdaName)
        //{
        //    return this.lambda.SimulateLookup (lambdaName, this.parent);
        //}

        //internal override BoundVariable SimulateStaticLookup (object lambdaName, int randDepth)
        //{
        //    return this.lambda.SimulateStaticLookup (lambdaName, this.parent, randDepth);
        //}

        //internal override BoundVariable SimulateDangerousLookup (object lambdaName, int safeDepth, int randDepth)
        //{
        //    return this.lambda.SimulateDangerousLookup (lambdaName, this.parent, safeDepth, randDepth);
        //}

        internal override BoundVariable Bind (Symbol name)
        {
            BoundVariable answer;
            if (bindingCache.TryGetValue (name, out answer))
                return answer;

            if (Configuration.EnableArgumentBinding) {
                int offset = this.lambda.LexicalOffset (name);
                if (offset != -1) {
                    answer = Argument.Make (name, this.lambda, offset);
                    bindingCache.Add (name, answer);
                    return answer;
                }
            }
            if (Configuration.EnableLexicalAddressing) {
                int offset = this.lambda.LexicalOffset (name);
                if (offset != -1) {
                    answer = LexicalVariable.Make (name, this.lambda, 0, offset);
                    bindingCache.Add (name, answer);
                    return answer;
                }
                else {
                    BoundVariable outer = parent.Bind (name);
                    answer = this.lambda.IncreaseLexicalDepth (outer);
                    bindingCache.Add (name, answer);
                    return answer;
                }
            }
            answer = new FreeVariable (name);
            bindingCache.Add (name, answer);
            return answer;
        }

        internal override LexicalMap AddSubstitution (Symbol name, BoundVariable value)
        {
            return new SubstitutionLexicalMap (name, value, this);
        }
    }

    class SubstitutionLexicalMap : LexicalMap
    {
        protected readonly Symbol name;
        protected readonly BoundVariable value;
        protected readonly LexicalMap parent;

        public SubstitutionLexicalMap (Symbol name, BoundVariable value, LexicalMap parent)
        {
            this.name = name;
            this.value = value;
            this.parent = parent;
        }

        internal override LexicalMap AddSubstitution (Symbol name, BoundVariable value)
        {
            return new SubstitutionLexicalMap (name, value, this);
        }

        internal override BoundVariable Bind (Symbol name)
        {
            if (name == this.name)
                return this.value;
            return parent.Bind (name);
        }
    }
    //public abstract class LexicalMap
    //{
    //    protected LexicalMap ()
    //    {
    //    }

    //    internal abstract Variable BindLexical (object ratorName, int randDepth);

    //    internal abstract Variable BindVariable (object ratorName);

    //    internal abstract Variable BindShadow (object ratorName, int randDepth, int shadowingFrame);

    //    //internal LexicalMap Extend (SimpleLambda lambda)
    //    //{
    //    //    return new SimpleLambdaBindingEnvironment (lambda, this);
    //    //}

    //    internal LexicalMap Extend (StandardLambda lambda)
    //    {
    //        return new StandardLambdaBindingEnvironment (lambda, this);
    //    }

    //    //internal LexicalMap Extend (StaticLambda lambda)
    //    //{
    //    //    return new StaticLambdaBindingEnvironment (lambda, this);
    //    //}

    //    internal LexicalMap Extend (ExtendedLambda lambda)
    //    {
    //        return new ExtendedLambdaBindingEnvironment (lambda, this);
    //    }
    //}

    //sealed public class RootBindingEnvironment : LexicalMap
    //{
    //    readonly Environment bindingEnvironment;

    //    public RootBindingEnvironment (Environment bindingEnvironment)
    //    {
    //        this.bindingEnvironment = bindingEnvironment;
    //    }

    //    internal override Variable BindVariable (object ratorName)
    //    {
    //        ClosureBase closure = this.bindingEnvironment.Closure;

    //        if (closure == null)
    //            // We know it can only be in the global environment.
    //            return GlobalVariable.Make (ratorName, this.bindingEnvironment);
    //        else {
    //            int randOffset = this.bindingEnvironment.Closure.FormalOffset (ratorName);
    //            // If it isn't bound in the binding time environment, then we
    //            // call it `Free' and search the incrementals and then the environment.
    //            return (randOffset != -1)
    //                ? Argument.Make (ratorName, randOffset)
    //                : FreeVariable.Make (ratorName, bindingEnvironment);
    //        }
    //    }

    //    internal override Variable BindShadow (object ratorName, int randDepth, int shadowingFrame)
    //    {
    //        ClosureBase closure = this.bindingEnvironment.Closure;
    //        if (closure == null) {
    //            // Same as free.  We have no clue where the variable is.
    //            // Hope it shows up before we need it, though.
    //            return DangerousFreeVariable.Make (ratorName, shadowingFrame, randDepth);
    //        }
    //        else {
    //            int randOffset = closure.FormalOffset (ratorName);
    //            return (randOffset == -1)
    //                ? DangerousFreeVariable.Make (ratorName, shadowingFrame, randDepth)
    //                : DangerousLexicalVariable.Make (ratorName, shadowingFrame, randDepth, randOffset);
    //        }
    //    }

    //    internal override Variable BindLexical (object ratorName, int randDepth)
    //    {
    //        ClosureBase closure = this.bindingEnvironment.Closure;
    //        if (closure == null) {
    //            // Root environment and cannot be shadowed.
    //            return GlobalVariable.Make (ratorName, this.bindingEnvironment);
    //        }
    //        else {
    //            int randOffset = closure.FormalOffset (ratorName);
    //            return (randOffset == -1)
    //                ? FreeVariable.Make (ratorName, this.bindingEnvironment)
    //                : TopLevelVariable.Make (ratorName, this.bindingEnvironment.GetValueCell (ratorName));
    //        }
    //    }
    //}

    //abstract class LambdaBindingEnvironment : LexicalMap
    //{
    //    protected readonly Lambda lambda;
    //    protected readonly LexicalMap parent;

    //    public LambdaBindingEnvironment (Lambda lambda, LexicalMap parent)
    //    {
    //        this.lambda = lambda;
    //        this.parent = parent;
    //    }
    //}

    //abstract class SafeLambdaBindingEnvironment : LambdaBindingEnvironment
    //{
    //    protected SafeLambdaBindingEnvironment (Lambda lambda, LexicalMap parent)
    //        : base (lambda, parent)
    //    { }

    //    internal override Variable BindVariable (object ratorName)
    //    {
    //        int randOffset = this.lambda.LexicalOffset (ratorName);
    //        return (randOffset == -1)
    //            ? parent.BindLexical (ratorName, 1)  // Search deeper.
    //            : Argument.Make (ratorName, randOffset);
    //    }

    //    internal override Variable BindShadow (object ratorName, int randDepth, int shadowingFrame)
    //    {
    //        int randOffset = this.lambda.LexicalOffset (ratorName);
    //        return (randOffset == -1)
    //            ? parent.BindShadow (ratorName, randDepth + 1, shadowingFrame)
    //            : DangerousLexicalVariable.Make (ratorName, shadowingFrame, randDepth, randOffset);
    //    }

    //    internal override Variable BindLexical (object ratorName, int randDepth)
    //    {
    //        int randOffset = this.lambda.LexicalOffset (ratorName);
    //        return (randOffset == -1)
    //          ? parent.BindLexical (ratorName, randDepth + 1)  // Search deeper.
    //          : LexicalVariable.Make (ratorName, randDepth, randOffset);
    //    }
    //}

    ////sealed class SimpleLambdaBindingEnvironment : SafeLambdaBindingEnvironment
    ////{
    ////    public SimpleLambdaBindingEnvironment (SimpleLambda lambda, LexicalMap parent)
    ////        : base (lambda, parent)
    ////    {
    ////    }
    ////}

    ////sealed class StaticLambdaBindingEnvironment : SafeLambdaBindingEnvironment
    ////{
    ////    public StaticLambdaBindingEnvironment (StaticLambda lambda, LexicalMap parent)
    ////        : base (lambda, parent)
    ////    {
    ////    }
    ////}

    //sealed class StandardLambdaBindingEnvironment : LambdaBindingEnvironment
    //{
    //    public StandardLambdaBindingEnvironment (StandardLambda lambda, LexicalMap parent)
    //        : base (lambda, parent)
    //    {
    //    }

    //    internal override Variable BindLexical (object ratorName, int randDepth)
    //    {
    //        int randOffset = this.lambda.LexicalOffset (ratorName);
    //        return (randOffset == -1)
    //            ? parent.BindShadow (ratorName, randDepth + 1, randDepth) // ***
    //            : LexicalVariable.Make (ratorName, randDepth, randOffset);
    //    }

    //    internal override Variable BindShadow (object ratorName, int randDepth, int shadowingFrame)
    //    {
    //        int randOffset = this.lambda.LexicalOffset (ratorName);
    //        return (randOffset == -1)
    //            ? parent.BindShadow (ratorName, randDepth + 1, shadowingFrame)
    //            : DangerousLexicalVariable.Make (ratorName, shadowingFrame, randDepth, randOffset);
    //    }

    //    internal override Variable BindVariable (object ratorName)
    //    {
    //        int randOffset = this.lambda.LexicalOffset (ratorName);
    //        return (randOffset == -1)
    //            ? parent.BindShadow (ratorName, 1, 0)
    //            : Argument.Make (ratorName, randOffset);
    //    }
    //}

    //sealed class ExtendedLambdaBindingEnvironment : LexicalMap
    //{
    //    readonly ExtendedLambda lambda;
    //    readonly LexicalMap parent;

    //    public ExtendedLambdaBindingEnvironment (ExtendedLambda lambda, LexicalMap parent)
    //    {
    //        this.lambda = lambda;
    //        this.parent = parent;
    //    }
 
    //    internal override Variable BindLexical (object ratorName, int randDepth)
    //    {
    //        int randOffset = this.lambda.LexicalOffset (ratorName);
    //        return (randOffset == -1)
    //            ? parent.BindShadow (ratorName, randDepth + 1, randDepth)
    //            : LexicalVariable.Make (ratorName, randDepth, randOffset);
    //    }

    //    internal override Variable BindShadow (object ratorName, int randDepth, int shadowingFrame)
    //    {
    //        int randOffset = this.lambda.LexicalOffset (ratorName);
    //        return (randOffset == -1)
    //            ? parent.BindShadow (ratorName, randDepth + 1, shadowingFrame)
    //            : DangerousLexicalVariable.Make (ratorName, shadowingFrame, randDepth, randOffset);
    //    }

    //    internal override Variable BindVariable (object ratorName)
    //    {
    //        int randOffset = this.lambda.LexicalOffset (ratorName);
    //        return (randOffset == -1)
    //            ? parent.BindShadow (ratorName, 1, 0)
    //            : Argument.Make (ratorName, randOffset); 
    //    }
    //}
}

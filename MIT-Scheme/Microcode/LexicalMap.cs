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
        internal abstract LexicalMap AddSubstitution (object name, BoundVariable value);
        public  static LexicalMap Make (Environment environment)
        {
            return (environment is GlobalEnvironment)
                ? (LexicalMap) new GlobalLexicalMap (environment)
                : (LexicalMap) new TopLevelLexicalMap (environment);
        }
        internal abstract BoundVariable Bind (object name);
        //internal abstract BoundVariable SimulateDangerousLookup (object lambdaName, int safeDepth, int argDepth);
        //internal abstract BoundVariable SimulateLookup (object lambdaName);
        //internal abstract BoundVariable SimulateStaticLookup (object lambdaName, int argDepth);
    }

    /// <summary>
    /// A lexical map derived from a real live environment.  We can simply
    /// ask where the variable is.
    /// </summary>
    sealed class GlobalLexicalMap : LexicalMap
    {
        readonly Environment bindingEnvironment;
        Dictionary<object,BoundVariable> bindingCache = new Dictionary<object, BoundVariable> ();

        internal GlobalLexicalMap (Environment bindingEnvironment)
        {
            this.bindingEnvironment = bindingEnvironment;
        }


        //internal override BoundVariable SimulateLookup (object lambdaName)
        //{
        //    return bindingEnvironment.SimulateLookup (lambdaName);
        //}

        //internal override BoundVariable SimulateStaticLookup (object lambdaName, int argDepth)
        //{
        //    return bindingEnvironment.SimulateStaticLookup (lambdaName, argDepth);
        //}

        //internal override BoundVariable SimulateDangerousLookup (object lambdaName, int safeDepth, int argDepth)
        //{
        //    return bindingEnvironment.SimulateDangerousLookup (lambdaName, safeDepth, argDepth);
        //}

        internal override BoundVariable Bind (object name)
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

        internal override LexicalMap AddSubstitution (object name, BoundVariable value)
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

           //internal override BoundVariable SimulateStaticLookup (object lambdaName, int argDepth)
           //{
           //    return bindingEnvironment.SimulateStaticLookup (lambdaName, argDepth);
           //}

           //internal override BoundVariable SimulateDangerousLookup (object lambdaName, int safeDepth, int argDepth)
           //{
           //    return bindingEnvironment.SimulateDangerousLookup (lambdaName, safeDepth, argDepth);
           //}

           internal override BoundVariable Bind (object name)
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

           internal override LexicalMap AddSubstitution (object name, BoundVariable value)
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
        Dictionary<object,BoundVariable> bindingCache = new Dictionary<object, BoundVariable> ();
        internal LambdaLexicalMap (LambdaBase lambda, LexicalMap parent)
        {
            this.lambda = lambda;
            this.parent = parent;
        }


        //internal override BoundVariable SimulateLookup (object lambdaName)
        //{
        //    return this.lambda.SimulateLookup (lambdaName, this.parent);
        //}

        //internal override BoundVariable SimulateStaticLookup (object lambdaName, int argDepth)
        //{
        //    return this.lambda.SimulateStaticLookup (lambdaName, this.parent, argDepth);
        //}

        //internal override BoundVariable SimulateDangerousLookup (object lambdaName, int safeDepth, int argDepth)
        //{
        //    return this.lambda.SimulateDangerousLookup (lambdaName, this.parent, safeDepth, argDepth);
        //}

        internal override BoundVariable Bind (object name)
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

        internal override LexicalMap AddSubstitution (object name, BoundVariable value)
        {
            return new SubstitutionLexicalMap (name, value, this);
        }
    }

    class SubstitutionLexicalMap : LexicalMap
    {
        protected readonly object name;
        protected readonly BoundVariable value;
        protected readonly LexicalMap parent;

        public SubstitutionLexicalMap (object name, BoundVariable value, LexicalMap parent)
        {
            this.name = name;
            this.value = value;
            this.parent = parent;
        }

        internal override LexicalMap AddSubstitution (object name, BoundVariable value)
        {
            return new SubstitutionLexicalMap (name, value, this);
        }

        internal override BoundVariable Bind (object name)
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

    //    internal abstract Variable BindLexical (object varname, int argDepth);

    //    internal abstract Variable BindVariable (object varname);

    //    internal abstract Variable BindShadow (object varname, int argDepth, int shadowingFrame);

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

    //    internal override Variable BindVariable (object varname)
    //    {
    //        ClosureBase closure = this.bindingEnvironment.Closure;

    //        if (closure == null)
    //            // We know it can only be in the global environment.
    //            return GlobalVariable.Make (varname, this.bindingEnvironment);
    //        else {
    //            int argOffset = this.bindingEnvironment.Closure.FormalOffset (varname);
    //            // If it isn't bound in the binding time environment, then we
    //            // call it `Free' and search the incrementals and then the environment.
    //            return (argOffset != -1)
    //                ? Argument.Make (varname, argOffset)
    //                : FreeVariable.Make (varname, bindingEnvironment);
    //        }
    //    }

    //    internal override Variable BindShadow (object varname, int argDepth, int shadowingFrame)
    //    {
    //        ClosureBase closure = this.bindingEnvironment.Closure;
    //        if (closure == null) {
    //            // Same as free.  We have no clue where the variable is.
    //            // Hope it shows up before we need it, though.
    //            return DangerousFreeVariable.Make (varname, shadowingFrame, argDepth);
    //        }
    //        else {
    //            int argOffset = closure.FormalOffset (varname);
    //            return (argOffset == -1)
    //                ? DangerousFreeVariable.Make (varname, shadowingFrame, argDepth)
    //                : DangerousLexicalVariable.Make (varname, shadowingFrame, argDepth, argOffset);
    //        }
    //    }

    //    internal override Variable BindLexical (object varname, int argDepth)
    //    {
    //        ClosureBase closure = this.bindingEnvironment.Closure;
    //        if (closure == null) {
    //            // Root environment and cannot be shadowed.
    //            return GlobalVariable.Make (varname, this.bindingEnvironment);
    //        }
    //        else {
    //            int argOffset = closure.FormalOffset (varname);
    //            return (argOffset == -1)
    //                ? FreeVariable.Make (varname, this.bindingEnvironment)
    //                : TopLevelVariable.Make (varname, this.bindingEnvironment.GetValueCell (varname));
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

    //    internal override Variable BindVariable (object varname)
    //    {
    //        int argOffset = this.lambda.LexicalOffset (varname);
    //        return (argOffset == -1)
    //            ? parent.BindLexical (varname, 1)  // Search deeper.
    //            : Argument.Make (varname, argOffset);
    //    }

    //    internal override Variable BindShadow (object varname, int argDepth, int shadowingFrame)
    //    {
    //        int argOffset = this.lambda.LexicalOffset (varname);
    //        return (argOffset == -1)
    //            ? parent.BindShadow (varname, argDepth + 1, shadowingFrame)
    //            : DangerousLexicalVariable.Make (varname, shadowingFrame, argDepth, argOffset);
    //    }

    //    internal override Variable BindLexical (object varname, int argDepth)
    //    {
    //        int argOffset = this.lambda.LexicalOffset (varname);
    //        return (argOffset == -1)
    //          ? parent.BindLexical (varname, argDepth + 1)  // Search deeper.
    //          : LexicalVariable.Make (varname, argDepth, argOffset);
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

    //    internal override Variable BindLexical (object varname, int argDepth)
    //    {
    //        int argOffset = this.lambda.LexicalOffset (varname);
    //        return (argOffset == -1)
    //            ? parent.BindShadow (varname, argDepth + 1, argDepth) // ***
    //            : LexicalVariable.Make (varname, argDepth, argOffset);
    //    }

    //    internal override Variable BindShadow (object varname, int argDepth, int shadowingFrame)
    //    {
    //        int argOffset = this.lambda.LexicalOffset (varname);
    //        return (argOffset == -1)
    //            ? parent.BindShadow (varname, argDepth + 1, shadowingFrame)
    //            : DangerousLexicalVariable.Make (varname, shadowingFrame, argDepth, argOffset);
    //    }

    //    internal override Variable BindVariable (object varname)
    //    {
    //        int argOffset = this.lambda.LexicalOffset (varname);
    //        return (argOffset == -1)
    //            ? parent.BindShadow (varname, 1, 0)
    //            : Argument.Make (varname, argOffset);
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
 
    //    internal override Variable BindLexical (object varname, int argDepth)
    //    {
    //        int argOffset = this.lambda.LexicalOffset (varname);
    //        return (argOffset == -1)
    //            ? parent.BindShadow (varname, argDepth + 1, argDepth)
    //            : LexicalVariable.Make (varname, argDepth, argOffset);
    //    }

    //    internal override Variable BindShadow (object varname, int argDepth, int shadowingFrame)
    //    {
    //        int argOffset = this.lambda.LexicalOffset (varname);
    //        return (argOffset == -1)
    //            ? parent.BindShadow (varname, argDepth + 1, shadowingFrame)
    //            : DangerousLexicalVariable.Make (varname, shadowingFrame, argDepth, argOffset);
    //    }

    //    internal override Variable BindVariable (object varname)
    //    {
    //        int argOffset = this.lambda.LexicalOffset (varname);
    //        return (argOffset == -1)
    //            ? parent.BindShadow (varname, 1, 0)
    //            : Argument.Make (varname, argOffset); 
    //    }
    //}
}

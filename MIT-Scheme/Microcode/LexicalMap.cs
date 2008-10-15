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
        internal abstract LexicalMap Extend (LambdaBase lambda);
        public  static LexicalMap Make (Environment environment)
        {
            return (environment is GlobalEnvironment)
                ? (LexicalMap) new GlobalLexicalMap (environment)
                : (LexicalMap) new TopLevelLexicalMap (environment);
        }
        internal abstract BoundVariable Bind (object name);
        //internal abstract BoundVariable SimulateDangerousLookup (object name, int safeDepth, int depth);
        //internal abstract BoundVariable SimulateLookup (object name);
        //internal abstract BoundVariable SimulateStaticLookup (object name, int depth);
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

        internal override LexicalMap Extend (LambdaBase lambda)
        {
            return new LambdaLexicalMap (lambda, this);
        }

        //internal override BoundVariable SimulateLookup (object name)
        //{
        //    return bindingEnvironment.SimulateLookup (name);
        //}

        //internal override BoundVariable SimulateStaticLookup (object name, int depth)
        //{
        //    return bindingEnvironment.SimulateStaticLookup (name, depth);
        //}

        //internal override BoundVariable SimulateDangerousLookup (object name, int safeDepth, int depth)
        //{
        //    return bindingEnvironment.SimulateDangerousLookup (name, safeDepth, depth);
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

           internal override LexicalMap Extend (LambdaBase lambda)
           {
               return new LambdaLexicalMap (lambda, this);
           }

           //internal override BoundVariable SimulateLookup (object name)
           //{
           //    return bindingEnvironment.SimulateLookup (name);
           //}

           //internal override BoundVariable SimulateStaticLookup (object name, int depth)
           //{
           //    return bindingEnvironment.SimulateStaticLookup (name, depth);
           //}

           //internal override BoundVariable SimulateDangerousLookup (object name, int safeDepth, int depth)
           //{
           //    return bindingEnvironment.SimulateDangerousLookup (name, safeDepth, depth);
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

        internal override LexicalMap Extend (LambdaBase lambda)
        {
            return new LambdaLexicalMap (lambda, this);
        }

        //internal override BoundVariable SimulateLookup (object name)
        //{
        //    return this.lambda.SimulateLookup (name, this.parent);
        //}

        //internal override BoundVariable SimulateStaticLookup (object name, int depth)
        //{
        //    return this.lambda.SimulateStaticLookup (name, this.parent, depth);
        //}

        //internal override BoundVariable SimulateDangerousLookup (object name, int safeDepth, int depth)
        //{
        //    return this.lambda.SimulateDangerousLookup (name, this.parent, safeDepth, depth);
        //}

        internal override BoundVariable Bind (object name)
        {
            BoundVariable answer;
            if (bindingCache.TryGetValue (name, out answer))
                return answer;

            if (Configuration.EnableArgumentBinding) {
                int offset = this.lambda.LexicalOffset (name);
                if (offset != -1) {
                    answer = Argument.Make (name, offset);
                    bindingCache.Add (name, answer);
                    return answer;
                }
            }
            if (Configuration.EnableLexicalAddressing) {
                int offset = this.lambda.LexicalOffset (name);
                if (offset != -1) {
                    answer = LexicalVariable.Make (name, 0, offset);
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
    }


    //public abstract class LexicalMap
    //{
    //    protected LexicalMap ()
    //    {
    //    }

    //    internal abstract Variable BindLexical (object varname, int depth);

    //    internal abstract Variable BindVariable (object varname);

    //    internal abstract Variable BindShadow (object varname, int depth, int shadowingFrame);

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
    //            int offset = this.bindingEnvironment.Closure.FormalOffset (varname);
    //            // If it isn't bound in the binding time environment, then we
    //            // call it `Free' and search the incrementals and then the environment.
    //            return (offset != -1)
    //                ? Argument.Make (varname, offset)
    //                : FreeVariable.Make (varname, bindingEnvironment);
    //        }
    //    }

    //    internal override Variable BindShadow (object varname, int depth, int shadowingFrame)
    //    {
    //        ClosureBase closure = this.bindingEnvironment.Closure;
    //        if (closure == null) {
    //            // Same as free.  We have no clue where the variable is.
    //            // Hope it shows up before we need it, though.
    //            return DangerousFreeVariable.Make (varname, shadowingFrame, depth);
    //        }
    //        else {
    //            int offset = closure.FormalOffset (varname);
    //            return (offset == -1)
    //                ? DangerousFreeVariable.Make (varname, shadowingFrame, depth)
    //                : DangerousLexicalVariable.Make (varname, shadowingFrame, depth, offset);
    //        }
    //    }

    //    internal override Variable BindLexical (object varname, int depth)
    //    {
    //        ClosureBase closure = this.bindingEnvironment.Closure;
    //        if (closure == null) {
    //            // Root environment and cannot be shadowed.
    //            return GlobalVariable.Make (varname, this.bindingEnvironment);
    //        }
    //        else {
    //            int offset = closure.FormalOffset (varname);
    //            return (offset == -1)
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
    //        int offset = this.lambda.LexicalOffset (varname);
    //        return (offset == -1)
    //            ? parent.BindLexical (varname, 1)  // Search deeper.
    //            : Argument.Make (varname, offset);
    //    }

    //    internal override Variable BindShadow (object varname, int depth, int shadowingFrame)
    //    {
    //        int offset = this.lambda.LexicalOffset (varname);
    //        return (offset == -1)
    //            ? parent.BindShadow (varname, depth + 1, shadowingFrame)
    //            : DangerousLexicalVariable.Make (varname, shadowingFrame, depth, offset);
    //    }

    //    internal override Variable BindLexical (object varname, int depth)
    //    {
    //        int offset = this.lambda.LexicalOffset (varname);
    //        return (offset == -1)
    //          ? parent.BindLexical (varname, depth + 1)  // Search deeper.
    //          : LexicalVariable.Make (varname, depth, offset);
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

    //    internal override Variable BindLexical (object varname, int depth)
    //    {
    //        int offset = this.lambda.LexicalOffset (varname);
    //        return (offset == -1)
    //            ? parent.BindShadow (varname, depth + 1, depth) // ***
    //            : LexicalVariable.Make (varname, depth, offset);
    //    }

    //    internal override Variable BindShadow (object varname, int depth, int shadowingFrame)
    //    {
    //        int offset = this.lambda.LexicalOffset (varname);
    //        return (offset == -1)
    //            ? parent.BindShadow (varname, depth + 1, shadowingFrame)
    //            : DangerousLexicalVariable.Make (varname, shadowingFrame, depth, offset);
    //    }

    //    internal override Variable BindVariable (object varname)
    //    {
    //        int offset = this.lambda.LexicalOffset (varname);
    //        return (offset == -1)
    //            ? parent.BindShadow (varname, 1, 0)
    //            : Argument.Make (varname, offset);
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
 
    //    internal override Variable BindLexical (object varname, int depth)
    //    {
    //        int offset = this.lambda.LexicalOffset (varname);
    //        return (offset == -1)
    //            ? parent.BindShadow (varname, depth + 1, depth)
    //            : LexicalVariable.Make (varname, depth, offset);
    //    }

    //    internal override Variable BindShadow (object varname, int depth, int shadowingFrame)
    //    {
    //        int offset = this.lambda.LexicalOffset (varname);
    //        return (offset == -1)
    //            ? parent.BindShadow (varname, depth + 1, shadowingFrame)
    //            : DangerousLexicalVariable.Make (varname, shadowingFrame, depth, offset);
    //    }

    //    internal override Variable BindVariable (object varname)
    //    {
    //        int offset = this.lambda.LexicalOffset (varname);
    //        return (offset == -1)
    //            ? parent.BindShadow (varname, 1, 0)
    //            : Argument.Make (varname, offset); 
    //    }
    //}
}

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;

namespace Microcode
{
    public struct LexicalAddress
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly short depth;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly short offset;

        public LexicalAddress (short depth, short offset)
        {
            this.depth = depth;
            this.offset = offset;
        }

        public short Depth { [DebuggerStepThrough] get { return this.depth; } }
        public short Offset { [DebuggerStepThrough] get { return this.offset; } }
    }

    class LexicalBinding
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly LambdaBase binder;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly LexicalAddress address;

        public LexicalBinding (LambdaBase binder, LexicalAddress address)
        {
            this.binder = binder;
            this.address = address;
        }

        public LambdaBase Binder { [DebuggerStepThrough] get { return this.binder; } }
        public LexicalAddress Address { [DebuggerStepThrough] get { return this.address; } }

        public LexicalBinding IncreaseLexicalDepth ()
        {
            return new LexicalBinding (this.binder, new LexicalAddress ((short)(this.address.Depth + 1), this.address.Offset));
        }
    }

    abstract class LexicalMap
    {
        Dictionary<Symbol,SCode> cache;
        protected LexicalMap () {
            cache = new Dictionary<Symbol,SCode>();
        }

        protected static LexicalMap NotImplemented () {
            throw new NotImplementedException("LexicalMap");
        }

        public static LexicalMap Make (Environment env) {
            return
                (env is GlobalEnvironment) ? GlobalLexicalMap.Make((GlobalEnvironment) env) :
                (env is StandardEnvironment) ? StandardLexicalMap.Make ((StandardEnvironment) env) :
                NotImplemented();
        }

        public LexicalMap Extend (LambdaBase lambda)
        {
            return
                lambda.CallsTheEnvironment () ? FirstClassLexicalMap.Make (this, lambda) :
                StaticLexicalMap.Make (this, lambda);
        }

        internal abstract SCode LookupVariableUncached (Symbol variable,
                                                Func<int, int, SCode> ifLexicalVariable,
                                                Func<int, int, SCode> ifShadowableLexicalVariable,
                                                Func<StandardEnvironment, SCode> ifAuxVariable,
                                                Func<StandardEnvironment, SCode> ifShadowableAuxVariable,
                                                Func<StandardEnvironment, SCode> ifFreeVariable,
                                                Func<StandardEnvironment, SCode> ifShadowableFreeVariable,
                                                Func<GlobalEnvironment, ValueCell, SCode> ifGlobalVariable,
                                                Func<GlobalEnvironment, ValueCell, SCode> ifShadowableGlobalVariable);

        internal SCode LookupVariable (Symbol variable,
                                                Func<int, int, SCode> ifLexicalVariable,
                                                Func<int, int, SCode> ifShadowableLexicalVariable,
                                                Func<StandardEnvironment, SCode> ifAuxVariable,
                                                Func<StandardEnvironment, SCode> ifShadowableAuxVariable,
                                                Func<StandardEnvironment, SCode> ifFreeVariable,
                                                Func<StandardEnvironment, SCode> ifShadowableFreeVariable,
                                                Func<GlobalEnvironment, ValueCell, SCode> ifGlobalVariable,
                                                Func<GlobalEnvironment, ValueCell, SCode> ifShadowableGlobalVariable)
        {
            SCode answer;
            if (this.cache.TryGetValue (variable, out answer))
                return answer;
            answer = LookupVariableUncached (variable,
                ifLexicalVariable,
                ifShadowableLexicalVariable,
                ifAuxVariable,
                ifShadowableAuxVariable,
                ifFreeVariable,
                ifShadowableFreeVariable,
                ifGlobalVariable,
                ifShadowableGlobalVariable);
            cache.Add (variable, answer);
            return answer;
        }
    }

    abstract class ExtendedLexicalMap : LexicalMap
    {
        protected readonly LexicalMap parent;

        protected ExtendedLexicalMap (LexicalMap parent)
            : base ()
        {
            this.parent = parent;
        }
    }

    class FirstClassLexicalMap : ExtendedLexicalMap
    {
        LambdaBase lambda;
        FirstClassLexicalMap (LexicalMap parent, LambdaBase lambda)
            : base (parent)
        {
            this.lambda = lambda;
        }

        public static LexicalMap Make (LexicalMap parent, LambdaBase lambda)
        {
            return new FirstClassLexicalMap (parent, lambda);
        }

        //public override SCode LookupVariableUncached (Symbol variable, 
        //                                      Func<int, Argument> ifArgument,
        //                                      Func<SCode, SCode> ifLexical1,
        //                                      Func<SCode, SCode> ifShadowableLexical1,
        //                                      Func<SCode, SCode> ifLexicalVariable,
        //                                      Func<SCode, SCode> ifShadowableLexicalVariable,
        //                                      Func<SCode, SCode> ifGlobalVariable,
        //                                      Func<SCode, SCode> ifShadowableGlobalVariable)
        //    SCode answer;
        //    if (this.cache.TryGetValue (variable, out answer))
        //        return answer;
        //    int offset = this.lambda.LexicalOffset (variable);
        //    answer = 
        //        (offset == -1) ? this.parent.LookupVariableUncached (variable,
        //             delegate (Argument arg) { return ifShadowableLexical1 (arg); },
        //             delegate (LexicalVariable1 lv1) { return ifShadowableLexicalVariable (lv1); },
        //             delegate (LexicalVariable lv) { return ifShadowableLexicalVariable (lv); },
        //             delegate (GlobalVariable gv) { return ifShadowableGlobalVariable (gv); }
        //             ) :
        //             ifArgument(offset);
        //    this.cache.Add (variable, answer);
        //    return answer;
        //}

        SCode UnderConstruction (SCode thing)
        {
            throw new NotImplementedException ();
        }
        internal override SCode LookupVariableUncached (Symbol variable,  
                                                    Func<int, int, SCode> ifLexicalVariable, 
                                                    Func<int, int, SCode> ifShadowableLexicalVariable,
                                                    Func<StandardEnvironment, SCode> ifAuxVariable,
                                                    Func<StandardEnvironment, SCode> ifShadowableAuxVariable,
                                                    Func<StandardEnvironment, SCode> ifFreeVariable,
                                                    Func<StandardEnvironment, SCode> ifShadowableFreeVariable,
                                                    Func<GlobalEnvironment, ValueCell, SCode> ifGlobalVariable, 
                                                    Func<GlobalEnvironment, ValueCell, SCode> ifShadowableGlobalVariable)
        {
            int offset = this.lambda.LexicalOffset (variable);
            return
                (offset == -1) ? this.parent.LookupVariableUncached (variable,
                     delegate (int depth, int o) { return ifShadowableLexicalVariable (depth + 1, o); },
                     delegate (int depth, int o) { return ifShadowableLexicalVariable (depth + 1, o); },
                     ifShadowableAuxVariable,
                     ifShadowableAuxVariable,
                     ifShadowableFreeVariable,
                     ifShadowableFreeVariable,
                     ifShadowableGlobalVariable,
                     ifShadowableGlobalVariable
                     ) :
                     ifLexicalVariable (0, offset);
        }
    }

    class StaticLexicalMap : ExtendedLexicalMap
    {
        LambdaBase lambda;
        StaticLexicalMap (LexicalMap parent, LambdaBase lambda)
            : base (parent)
        {
            this.lambda = lambda;
        }
        public static LexicalMap Make (LexicalMap parent, LambdaBase lambda)
        {
            return new StaticLexicalMap (parent, lambda);
        }

        SCode UnderConstruction (SCode thing)
        {
            throw new NotImplementedException ();
        }

        internal override SCode LookupVariableUncached (Symbol variable,  
            Func<int, int, SCode> ifLexicalVariable, 
            Func<int, int, SCode> ifShadowableLexicalVariable,
                        Func<StandardEnvironment, SCode> ifAuxVariable,
            Func<StandardEnvironment, SCode> ifShadowableAuxVariable,
            Func<StandardEnvironment, SCode> ifFreeVariable,
            Func<StandardEnvironment, SCode> ifShadowableFreeVariable,
            Func<GlobalEnvironment, ValueCell, SCode> ifGlobalVariable, 
            Func<GlobalEnvironment, ValueCell, SCode> ifShadowableGlobalVariable)
        {
            int offset = this.lambda.LexicalOffset (variable);
            return
                (offset == -1) ? this.parent.LookupVariableUncached (variable,
                     delegate (int depth, int o) { return ifLexicalVariable (depth + 1, o); },
                     delegate (int depth, int o) { return ifShadowableLexicalVariable (depth+1, o); },
                     ifAuxVariable,
                     ifShadowableAuxVariable,
                     ifFreeVariable,
                     ifShadowableFreeVariable,
                     ifGlobalVariable,
                     ifShadowableGlobalVariable
                     ) :
                     ifLexicalVariable (0, offset);
        }
    }

    class GlobalLexicalMap : LexicalMap
    {
        GlobalEnvironment environment;
        GlobalLexicalMap (GlobalEnvironment env) 
            : base () {
            this.environment = env;
        }
        public static LexicalMap Make (GlobalEnvironment env) {
            return new GlobalLexicalMap (env);
        }

        internal override SCode LookupVariableUncached (Symbol variable,  
            Func<int, int, SCode> ifLexicalVariable, 
            Func<int, int, SCode> ifShadowableLexicalVariable,
            Func<StandardEnvironment, SCode> ifAuxVariable,
            Func<StandardEnvironment, SCode> ifShadowableAuxVariable,
            Func<StandardEnvironment, SCode> ifFreeVariable,
            Func<StandardEnvironment, SCode> ifShadowableFreeVariable,
            Func<GlobalEnvironment, ValueCell, SCode> ifGlobalVariable, 
            Func<GlobalEnvironment, ValueCell, SCode> ifShadowableGlobalVariable)
        {
            throw new NotImplementedException ();
            //return this.environment.LocateVariable<SCode> (variable,
            //    delegate () { return ifGlobalVariable (this.environment, null); },
            //    delegate (ValueCell cell) { return ifGlobalVariable (this.environment, cell); },
            //    delegate (LexicalEnvironment env, int depth, int o) { throw new NotImplementedException (); },
            //    delegate (StandardEnvironment env, int depth) { throw new NotImplementedException (); });
        }
    }

    class StandardLexicalMap : LexicalMap
    {
        StandardEnvironment environment;
        StandardLexicalMap (StandardEnvironment env)
            : base ()
        {
            this.environment = env;
        }
        internal static LexicalMap Make (StandardEnvironment env)
        {
            return new StandardLexicalMap (env);
        }

        internal override SCode LookupVariableUncached (Symbol variable,
            Func<int, int, SCode> ifLexicalVariable,
            Func<int, int, SCode> ifShadowableLexicalVariable,
            Func<StandardEnvironment, SCode> ifAuxVariable,
            Func<StandardEnvironment, SCode> ifShadowableAuxVariable,
            Func<StandardEnvironment, SCode> ifFreeVariable,
            Func<StandardEnvironment, SCode> ifShadowableFreeVariable,
            Func<GlobalEnvironment, ValueCell, SCode> ifGlobalVariable,
            Func<GlobalEnvironment, ValueCell, SCode> ifShadowableGlobalVariable)
        {
            throw new NotImplementedException ();
        //    this.environment.LocateVariable<SCode> (variable,
        //        delegate () { throw new NotImplementedException (); },
        //        delegate (ValueCell cell) { throw new NotImplementedException (); },
        //        delegate (LexicalEnvironment env, int depth, int o) {
        //            if (depth == 0) {
        //                return ifLexicalVariable (depth, o);
        //            }
        //            else {
        //                return ifShadowableLexicalVariable (depth, o);
        //            }
        //        },
        //        delegate (StandardEnvironment env, int depth) {
        //            if (depth == 0) {
        //                return ifAuxVariable (env);
        //            }
        //            else {
        //                return ifShadowableAuxVariable (env);
        //            }
        //        });

        //    int offset = this.environment.SearchFormals (variable);
        //    if (offset == -1) {
        //        ValueCell cell = this.environment.SearchIncrementals(variable);
        //        if (cell == null)
        //            return ifFreeVariable (this.environment);
        //        return ifAuxVariable (this.environment);
        //    }
        //    return ifLexicalVariable (0, offset);
        }
    }

    // class TopLevelLexicalMap 
    //{
    //    LambdaBase lambda;
    //    Environment closureEnvironment;

    //    public LexicalMap (LambdaBase lambda, Environment closureEnvironment)
    //    {
    //        this.lambda = lambda;
    //        this.closureEnvironment = closureEnvironment;
    //    }

    //    public LexicalMap (LambdaBase lambdaBase)
    //    {
    //        table = new Dictionary<Symbol, LexicalBinding> ();
    //        if (Configuration.EnableArgumentBinding)
    //            for (short offset = 0; offset < lambdaBase.Formals.Length; offset++) {
    //                table.Add (lambdaBase.Formals [offset],
    //                    new LexicalBinding (lambdaBase,
    //                        new LexicalAddress (0, offset)));
    //            }
    //        if (lambdaBase is StaticLambdaBase) {
    //            depth = 1;
    //        }
    //    }

    //    public LexicalMap (LambdaBase lambdaBase, LexicalMap parent)
    //    {
    //        table = new Dictionary<Symbol, LexicalBinding> ();
    //        //for (int offset = 0; offset < lambdaBase.Formals.Length; offset++) {
    //        //    if (Configuration.EnableArgumentBinding)
    //        //        table.Add (lambdaBase.Formals [offset], Argument.Make (lambdaBase.Formals [offset], lambdaBase, offset));
    //        //    else
    //        //        table.Add (lambdaBase.Formals [offset], Variable.Make (lambdaBase.Formals [offset]));
    //        //}
    //        foreach (KeyValuePair<Symbol, LexicalBinding> kvp in parent.table.AsEnumerable ()) {
    //            if (!table.ContainsKey (kvp.Key)) {
    //                table.Add (kvp.Key, kvp.Value.IncreaseLexicalDepth ());
    //            }
    //        }
    //        if (lambdaBase is StaticLambdaBase)
    //            depth = parent.StaticDepth + 1;
    //    }

    //    public override SCode LookupVariableUncached (Symbol variable, Func<int,SCode> ifArgument)
    //    {
    //        int offset = this.lambda.LexicalOffset (variable);
    //        if (offset != -1) {
    //            return ifArgument (offset);
    //        }
    //        throw new NotImplementedException ("Working on it.");
    //    }

    //    public int StaticDepth { get { return this.depth; } }
    //}
}

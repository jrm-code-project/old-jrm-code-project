using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;

namespace Microcode
{
    abstract class PartialEnvironment
    {
        internal abstract StaticMapping [] GetStaticMapping (ICollection<Symbol> freeVariables);
        internal abstract Environment BaseEnvironment { get ;}
        internal abstract Dictionary<Symbol, ValueCell> TopLevelVariables { get; }

        /// <summary>
        /// The list of symbols that can be used in deeper lexical environments.
        /// Includes the lambda-bound variables and any imported statics.
        /// </summary>
        internal abstract IList<Symbol> ExportedStatics
        {
            get;
        }

        internal static PartialEnvironment Make (ITopLevelEnvironment env)
        {
            return new PartialTopLevelEnvironment (env);
        }

        internal abstract TRet LocateVariable<TRet> (object name,
            Func<int, TRet> ifArgument,
            Func<int, TRet> ifStatic,
            Func<ValueCell, TRet> ifTopLevel,
            Func<GlobalEnvironment, TRet> ifGlobal,
            Func<TRet> ifNotFound);
    }

    sealed class PartialTopLevelEnvironment : PartialEnvironment
    {
        readonly ITopLevelEnvironment environment;

        public PartialTopLevelEnvironment (ITopLevelEnvironment environment)
        {
            this.environment = environment;
        }

        // Top level environments export Top Level variables, so they don't
        // have statics.
        static readonly StaticMapping [] noStaticMapping = new StaticMapping[0];
        internal override StaticMapping [] GetStaticMapping (ICollection<Symbol> freeVariables)
        {
            return noStaticMapping;
        }

        static readonly List<Symbol> noExportedStatics = new List<Symbol> ();
        internal override IList<Symbol> ExportedStatics
        {
            get { return noExportedStatics; }
        }

        internal override Environment BaseEnvironment
        {
            get { throw new NotImplementedException (); }
        }

        internal override Dictionary<Symbol, ValueCell> TopLevelVariables
        {
            get { return environment.TopLevelVariables; }
        }

        internal override TRet LocateVariable<TRet> (object name, Func<int, TRet> ifArgument, Func<int, TRet> ifStatic, Func<ValueCell, TRet> ifTopLevel, Func<GlobalEnvironment, TRet> ifGlobal, Func<TRet> ifNotFound)
        {
            ValueCell cell;
            if (this.environment.TopLevelVariables.TryGetValue ((Symbol) name, out cell))
                return ifTopLevel (cell);
            else
                return this.environment.LocateVariable<TRet> (name, ifGlobal, ifNotFound);
        }
    }

    sealed class PartialGlobalEnvironment : PartialEnvironment
    {
        internal override StaticMapping [] GetStaticMapping (ICollection<Symbol> freeVariables)
        {
            throw new NotImplementedException ();
        }

        internal override Environment BaseEnvironment
        {
            get { throw new NotImplementedException (); }
        }

        internal override Dictionary<Symbol, ValueCell> TopLevelVariables
        {
            get { throw new NotImplementedException (); }
        }

        internal override IList<Symbol> ExportedStatics
        {
            get { throw new NotImplementedException (); }
        }

        internal override TRet LocateVariable<TRet> (object name, Func<int, TRet> ifArgument, Func<int, TRet> ifStatic, Func<ValueCell, TRet> ifTopLevel, Func<GlobalEnvironment, TRet> ifGlobal, Func<TRet> ifNotFound)
        {
            throw new NotImplementedException ();
        }
    }

    abstract class PartialLexicalEnvironment<LType> : PartialEnvironment where LType : LambdaBase
    {
        protected PartialClosure<LType> envClosure;

        protected Dictionary<Symbol, ValueCell> topLevelVariables;
        protected IList<Symbol> importedStaticVariables;

        // This is the paranoid way of constructing these.
        // If we always check the TopLevelVariables last, we
        // shouldn't need to remove the bindings at each step.
        static Dictionary<Symbol, ValueCell> ComputeTopLevelVariables (Dictionary<Symbol, ValueCell> incoming, Symbol [] formals)
        {
            for (int i = 0; i < formals.Length; i++)
                if (incoming.ContainsKey (formals [i])) {
                    // at least one formal is shadowed, make a new dictionary
                    Dictionary<Symbol, ValueCell> answer = new Dictionary<Symbol, ValueCell> (incoming);
                    // remove the shadowed entries.
                    foreach (KeyValuePair<Symbol, ValueCell> kvp in incoming) {
                        if (Array.IndexOf<Symbol> (formals, kvp.Key) != -1)
                            answer.Remove (kvp.Key);
                    }
                    return answer;
                }
            // nothing was shadowed, just re-use the incoming
            return incoming;
        }

        protected PartialLexicalEnvironment (PartialClosure<LType> closure)
            : base ()
        {
            this.envClosure = closure;

            Symbol [] boundVariables = closure.BoundVariables;
            ICollection<Symbol> freeVariables = closure.FreeVariables;
            this.topLevelVariables = ComputeTopLevelVariables (closure.Environment.TopLevelVariables, boundVariables);
            this.importedStaticVariables = closure.ImportedStaticVariables;

        }

        internal override TRet LocateVariable<TRet> (object name,
            Func<int, TRet> ifArgument,
            Func<int, TRet> ifStatic,
            Func<ValueCell, TRet> ifTopLevel,
            Func<GlobalEnvironment, TRet> ifGlobal,
            Func<TRet> ifNotFound)
        {
            int argOffset = this.envClosure.FormalOffset (name);
            if (argOffset != -1) {
                return ifArgument (argOffset);
            }

            int staticOffset = this.importedStaticVariables.IndexOf ((Symbol) name);
            if (staticOffset != -1) {
                return ifStatic (staticOffset);
            }
            // Must be last so that bindings and statics will shadow!
            ValueCell topLevelCell;
            if (this.topLevelVariables.TryGetValue ((Symbol) name, out topLevelCell))
                return ifTopLevel (topLevelCell);
            return ifNotFound ();
        }

   }

    class PartialStandardEnvironment : PartialLexicalEnvironment<StandardLambda>
    {
        public PartialStandardEnvironment (PartialClosure<StandardLambda> closure)
            : base (closure)
        {
        }

        internal override StaticMapping [] GetStaticMapping (ICollection<Symbol> freeVariables)
        {
            Symbol [] formals = this.envClosure.Lambda.Formals;

            // determine how many mappings we need.
            int count = 0;
            for (int index = 0; index < formals.Length; index++)
                if (freeVariables.Contains (formals [index])) {
                    count += 1;
                }
            // Don't pass on the static mapping because we might shadow them.
            //foreach (Symbol stat in this.importedStaticVariables) {
            //    if (freeVariables.Contains (stat)) {
            //        count += 1;
            //    }
            //}

            StaticMapping [] names = new StaticMapping [count];
            int mapptr = 0;
            // Fill in the arguments
            for (int index = 0; index < formals.Length; index++)
                if (freeVariables.Contains (formals [index])) {
                    names [mapptr] = new StaticMapping (formals [index], (-index) - 1);
                    mapptr += 1;
                }

            //for (int index = 0; index < this.importedStaticVariables.Count; index++) {
            //    Symbol stat = this.importedStaticVariables [index];
            //    if (freeVariables.Contains (stat)) {
            //        names [mapptr] = new StaticMapping (stat, index);
            //        mapptr += 1;
            //    }
            //}

            StaticMapping.ValidateStaticMapping (names);
            return names;
        }

        //internal override PartialEnvironment BaseEnvironment
        //{
        //    get
        //    {
        //        return this;
        //    }
        //}

        // A standard environment
        static Dictionary<Symbol,ValueCell> noTopLevelVariables = new Dictionary<Symbol, ValueCell> ();
        internal override Dictionary<Symbol, ValueCell> TopLevelVariables
        {
            get { return noTopLevelVariables; }
        }

        internal override IList<Symbol> ExportedStatics
        {
            get
            {
                IList<Symbol> answer = new List<Symbol> ();
                foreach (Symbol symbol in this.envClosure.BoundVariables)
                    answer.Add (symbol);
                return answer;
            }
        }

        internal override Environment BaseEnvironment
        {
            get { throw new NotImplementedException (); }
        }
    }

    class PartialStandardExtendedEnvironment : PartialLexicalEnvironment<StandardExtendedLambda>
    {

        public PartialStandardExtendedEnvironment (PartialClosure<StandardExtendedLambda> closure)
            : base (closure)
        {
        }

        internal override StaticMapping [] GetStaticMapping (ICollection<Symbol> freeVariables)
        {
            Symbol [] formals = this.envClosure.Lambda.Formals;

            // determine how many mappings we need.
            int count = 0;
            for (int index = 0; index < formals.Length; index++)
                if (freeVariables.Contains (formals [index])) {
                    count += 1;
                }
            // Don't pass on the static mapping because we might shadow them.
            //foreach (Symbol stat in this.importedStaticVariables) {
            //    if (freeVariables.Contains (stat)) {
            //        count += 1;
            //    }
            //}

            StaticMapping [] names = new StaticMapping [count];
            int mapptr = 0;
            // Fill in the arguments
            for (int index = 0; index < formals.Length; index++)
                if (freeVariables.Contains (formals [index])) {
                    names [mapptr] = new StaticMapping (formals [index], (-index) - 1);
                    mapptr += 1;
                }

            //for (int index = 0; index < this.importedStaticVariables.Count; index++) {
            //    Symbol stat = this.importedStaticVariables [index];
            //    if (freeVariables.Contains (stat)) {
            //        names [mapptr] = new StaticMapping (stat, index);
            //        mapptr += 1;
            //    }
            //}

            StaticMapping.ValidateStaticMapping (names);
            return names;
        }

        //internal override Environment BaseEnvironment
        //{
        //    get
        //    {
        //        return this;
        //    }
        //}

        static Dictionary<Symbol,ValueCell> noTopLevelVariables = new Dictionary<Symbol, ValueCell> ();
        internal override Dictionary<Symbol, ValueCell> TopLevelVariables
        {
            get { return noTopLevelVariables; }
        }

        internal override IList<Symbol> ExportedStatics
        {
            get
            {
                IList<Symbol> answer = new List<Symbol> ();
                foreach (Symbol symbol in this.envClosure.BoundVariables)
                    answer.Add (symbol);
                return answer;
            }
        }

        internal override Environment BaseEnvironment
        {
            get { throw new NotImplementedException (); }
        }
    }

    class PartialStaticEnvironment : PartialLexicalEnvironment<StaticLambda>
    {
        public PartialStaticEnvironment (PartialClosure<StaticLambda> closure)
            : base (closure)
        {
            //this.topLevelVariables = new Dictionary<Symbol, ValueCell> ();
            //Symbol [] formals = closure.Lambda.Formals;
            //foreach (KeyValuePair<Symbol,ValueCell> kvp in closure.Environment.TopLevelVariables) {
            //    bool found = false;
            //    foreach (Symbol formal in formals) {
            //        if (kvp.Key.Equals (formal))
            //            found = true;
            //    }
            //    if (!found)
            //        this.topLevelVariables.Add (kvp.Key, kvp.Value);
            //}
        }


        internal override StaticMapping [] GetStaticMapping (ICollection<Symbol> freeVariables)
        {
            Symbol [] formals = this.envClosure.Lambda.Formals;

            // determine how many mappings we need.
            int count = 0;
            for (int index = 0; index < formals.Length; index++)
                if (freeVariables.Contains (formals [index])) {
                    count += 1;
                }
            foreach (Symbol stat in this.importedStaticVariables) {
                if (freeVariables.Contains (stat)) {
                    count += 1;
                }
            }

            StaticMapping [] names = new StaticMapping [count];
            int mapptr = 0;
            // Fill in the arguments
            for (int index = 0; index < formals.Length; index++)
                if (freeVariables.Contains (formals [index])) {
                    names [mapptr] = new StaticMapping (formals [index], (-index) - 1);
                    mapptr += 1;
                }

            for (int index = 0; index < this.importedStaticVariables.Count; index++) {
                Symbol stat = this.importedStaticVariables [index];
                if (freeVariables.Contains (stat)) {
                    names [mapptr] = new StaticMapping (stat, index);
                    mapptr += 1;
                }
            }

            StaticMapping.ValidateStaticMapping (names);
            return names;
        }

        //internal override Environment BaseEnvironment
        //{
        //    get
        //    {
        //        return this.envClosure.Environment;
        //    }
        //}

        internal override IList<Symbol> ExportedStatics
        {
            get
            {
                IList<Symbol> answer = new List<Symbol> ();
                foreach (Symbol symbol in this.envClosure.BoundVariables)
                    answer.Add (symbol);
                foreach (Symbol symbol in this.importedStaticVariables)
                    answer.Add (symbol);
                return answer;
            }
        }

        internal override Dictionary<Symbol, ValueCell> TopLevelVariables
        {
            [DebuggerStepThrough]
            get { return this.topLevelVariables; }
        }

        internal override Environment BaseEnvironment
        {
            get { throw new NotImplementedException (); }
        }
    }

    class PartialSimpleEnvironment : PartialLexicalEnvironment<SimpleLambda>
    {

        public PartialSimpleEnvironment (PartialClosure<SimpleLambda> closure)
            : base (closure)
        {
            //this.topLevelVariables = new Dictionary<Symbol, ValueCell> ();
            //Symbol [] formals = closure.Lambda.Formals;
            //foreach (KeyValuePair<Symbol,ValueCell> kvp in closure.Environment.TopLevelVariables) {
            //    bool found = false;
            //    foreach (Symbol formal in formals) {
            //        if (kvp.Key.Equals (formal))
            //            found = true;
            //    }
            //    if (!found)
            //        this.topLevelVariables.Add (kvp.Key, kvp.Value);
            //}
        }

        internal override StaticMapping [] GetStaticMapping (ICollection<Symbol> freeVariables)
        {
            Symbol [] formals = this.envClosure.Lambda.Formals;

            // determine how many mappings we need.
            int count = 0;
            for (int index = 0; index < formals.Length; index++)
                if (freeVariables.Contains (formals [index])) {
                    count += 1;
                }
            foreach (Symbol stat in this.importedStaticVariables) {
                if (freeVariables.Contains (stat)) {
                    count += 1;
                }
            }

            StaticMapping [] names = new StaticMapping [count];
            int mapptr = 0;
            // Fill in the arguments
            for (int index = 0; index < formals.Length; index++)
                if (freeVariables.Contains (formals [index])) {
                    names [mapptr] = new StaticMapping (formals [index], (-index) - 1);
                    mapptr += 1;
                }

            for (int index = 0; index < this.importedStaticVariables.Count; index++) {
                Symbol stat = this.importedStaticVariables [index];
                if (freeVariables.Contains (stat)) {
                    names [mapptr] = new StaticMapping (stat, index);
                    mapptr += 1;
                }
            }

            StaticMapping.ValidateStaticMapping (names);
            return names;
        }


        //internal override Environment BaseEnvironment
        //{
        //    get
        //    {
        //        return this.Closure.Environment;
        //    }
        //}


        internal override IList<Symbol> ExportedStatics
        {
            get
            {
                IList<Symbol> answer = new List<Symbol> ();
                foreach (Symbol symbol in this.envClosure.BoundVariables)
                    answer.Add (symbol);
                foreach (Symbol symbol in this.importedStaticVariables)
                    answer.Add (symbol);
                return answer;
            }
        }

        internal override Dictionary<Symbol, ValueCell> TopLevelVariables
        {
            [DebuggerStepThrough]
            get { return this.topLevelVariables; }
        }

        internal override Environment BaseEnvironment
        {
            get { throw new NotImplementedException (); }
        }
    }

}

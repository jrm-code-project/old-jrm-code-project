using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;

namespace Microcode
{
    abstract class PartialEnvironment
    {
        protected IDictionary<Symbol, ValueCell> importedTopLevelVariables;

        internal abstract StaticMapping [] GetStaticMapping (ICollection<Symbol> freeVariables);
        internal IDictionary<Symbol, ValueCell> TopLevelVariables { get { return this.importedTopLevelVariables; } }

        protected PartialEnvironment (IDictionary<Symbol, ValueCell> importedTopLevelVariables)
        {
            this.importedTopLevelVariables = importedTopLevelVariables;
        }

        internal static PartialEnvironment Make (ITopLevelEnvironment env)
        {
            return new PartialTopLevelEnvironment (env);
        }

        /// <summary>
        /// The list of symbols that can be used in deeper lexical environments.
        /// Includes the lambda-bound variables and any imported statics.
        /// </summary>
        internal abstract IList<Symbol> ExportedStatics
        {
            get;
        }

        internal abstract SCode LocateVariable (Variable variable);
    }

    sealed class PartialTopLevelEnvironment : PartialEnvironment
    {
        readonly ITopLevelEnvironment environment;

        public PartialTopLevelEnvironment (ITopLevelEnvironment environment)
            : base (environment.ExportedTopLevelVariables)
        {
            this.environment = environment;
        }

        // Top level environments export their variables as ExportedTopLevelVariables, so they don't
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

        internal override SCode LocateVariable (Variable variable)
        {
            ValueCell cell;
            if (this.importedTopLevelVariables.TryGetValue (variable.Name, out cell))
                return variable.MakeTopLevel (cell);
            else
                return this.environment.SpecializeVariable (variable);
        }
    }

    abstract class PartialLexicalEnvironment<LType> : PartialEnvironment where LType : LambdaBase
    {
        protected PartialClosure<LType> envClosure;

        protected IList<Symbol> importedStaticVariables;

        protected PartialLexicalEnvironment (PartialClosure<LType> closure)
            : base (closure.ExportedTopLevelVariables)
        {
            this.envClosure = closure;

            Symbol [] boundVariables = closure.BoundVariables;
            ICollection<Symbol> freeVariables = closure.FreeVariables;

            this.importedStaticVariables = closure.ImportedStaticVariables;

        }

        internal override SCode LocateVariable (Variable variable)
        {
            int argOffset = this.envClosure.FormalOffset (variable.Name);
            if (argOffset != -1) {
                return variable.MakeArgument (argOffset);
            }

            int staticOffset = this.importedStaticVariables.IndexOf (variable.Name);
            if (staticOffset != -1) {
                return variable.MakeStatic (staticOffset);
            }
            //// Must be last so that bindings and statics will shadow!
            ValueCell topLevelCell;
            if (this.importedTopLevelVariables.TryGetValue (variable.Name, out topLevelCell))
                return variable.MakeTopLevel (topLevelCell);
            return variable.MakeFree ();
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
    }

    class PartialStaticEnvironment : PartialLexicalEnvironment<StaticLambda>
    {
        public PartialStaticEnvironment (PartialClosure<StaticLambda> closure)
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
    }

    sealed class PartialSimpleEnvironment : PartialLexicalEnvironment<SimpleLambda>
    {
        public PartialSimpleEnvironment (PartialClosure<SimpleLambda> closure)
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
    }
}

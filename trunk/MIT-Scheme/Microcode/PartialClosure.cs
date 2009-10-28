using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;

namespace Microcode
{
    sealed class PartialClosure<LType> where LType : LambdaBase
    {
        readonly LType lambda;
        readonly PartialEnvironment environment;
        readonly IDictionary<Symbol, ValueCell> exportedTopLevelVariables;

        readonly StaticMapping staticMapping;

        static IDictionary<Symbol, ValueCell> ComputeUnshadowedTopLevelVariables (IDictionary<Symbol, ValueCell> topLevelVariables, Symbol [] shadowingSymbols)
        {
            for (int i = 0; i < shadowingSymbols.Length; i++)
                if (topLevelVariables.ContainsKey (shadowingSymbols [i])) {
                    // at least one formal is shadowed, make a new dictionary
                    IDictionary<Symbol, ValueCell> answer = new Dictionary<Symbol, ValueCell> (topLevelVariables);
                    // remove the shadowed entries.
                    foreach (KeyValuePair<Symbol, ValueCell> kvp in topLevelVariables) {
                        if (Array. IndexOf<Symbol> (shadowingSymbols, kvp.Key) != -1)
                            answer.Remove (kvp.Key);
                    }
                    return answer;
                }
            // nothing was shadowed, just re-use the incoming
            return topLevelVariables;
        }

        static readonly IDictionary<Symbol, ValueCell> noUnshadowedTopLevelVariables = new Dictionary<Symbol, ValueCell> (0);

        public PartialClosure (LType lambda, PartialEnvironment environment)
        {
            this.lambda = lambda;
            this.environment = environment;
            this.staticMapping = environment.GetStaticMapping (lambda.FreeVariables);
            this.exportedTopLevelVariables = lambda.CallsTheEnvironment()
                ? noUnshadowedTopLevelVariables
                : ComputeUnshadowedTopLevelVariables (environment.TopLevelVariables, lambda.Formals);
        }

        public LType Lambda { [DebuggerStepThrough] get { return this.lambda; } }
        public PartialEnvironment Environment { [DebuggerStepThrough] get { return this.environment; } }
        public IDictionary<Symbol, ValueCell> ExportedTopLevelVariables { [DebuggerStepThrough] get { return this.exportedTopLevelVariables; } }

        /// <summary>
        /// Returns the array of symbols bound when applying this closure.
        /// </summary>
        public Symbol [] BoundVariables { [DebuggerStepThrough] get { return this.lambda.Formals; } }

        /// <summary>
        /// Returns the collection of variables used by this closure.
        /// The bound variables are not in the collection.
        /// </summary>
        public ICollection<Symbol> FreeVariables
        {
            [DebuggerStepThrough]
            get
            {
                return this.lambda.FreeVariables;
            }
        }

        public StaticMapping StaticMap
        {
            get
            {
                return this.staticMapping;
            }
        }

        public Symbol [] ImportedStaticVariables
        {
            [DebuggerStepThrough]
            get
            {
                return this.staticMapping.Names;
            }
        }

        internal int FormalOffset (object name)
        {
            return this.lambda.LexicalOffset (name);
        }

        public StaticMapping ExportedStaticMapping (ICollection<Symbol> freeVariables)
        {
            Symbol [] formals = this.Lambda.Formals;
            int formalCount = formals.Length;

            // determine how many mappings we need.
            int count = 0;
            for (int index = 0; index < formals.Length; index++)
                if (freeVariables.Contains (formals [index])) {
                    count += 1;
                }

            // If we don't have shadowing variables, we can pass on the static ones.
            if (!this.lambda.CallsTheEnvironment ()) {
                foreach (Symbol stat in this.staticMapping.Names) {
                    if (freeVariables.Contains (stat)) {
                        count += 1;
                    }
                }
            }

            Symbol [] exportedNames = new Symbol [count];
            int [] exportedOffsets = new int [count];
            int mapptr = 0;
            // Fill in the arguments
            for (int index = 0; index < formals.Length; index++)
                if (freeVariables.Contains (formals [index])) {
                    exportedNames [mapptr] = formals [index];
                    exportedOffsets [mapptr] = index;
                    mapptr += 1;
                }

            if (!this.lambda.CallsTheEnvironment ()) {
                Symbol [] importedNames = this.staticMapping.Names;
                for (int index = 0; index < importedNames.Length; index++) {
                    Symbol stat = importedNames [index];
                    if (freeVariables.Contains (stat)) {
                        exportedNames [mapptr] = stat;
                        exportedOffsets [mapptr] = index + formalCount;
                        mapptr += 1;
                    }
                }
            }

            return new StaticMapping (exportedNames, exportedOffsets);
        }

    }
}

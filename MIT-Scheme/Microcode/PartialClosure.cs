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

        StaticMapping [] staticMapping;
        IList<Symbol> importedStaticVariables;

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

            this.exportedTopLevelVariables = lambda.CallsTheEnvironment()
                ? noUnshadowedTopLevelVariables
                : ComputeUnshadowedTopLevelVariables (environment.TopLevelVariables, lambda.Formals);

            this.staticMapping = environment.GetStaticMapping (lambda.FreeVariables);
            this.importedStaticVariables = new List<Symbol> ();

            // get the exported statics
            IList<Symbol> exportedStatics = environment.ExportedStatics;
            foreach (Symbol stat in exportedStatics) {
                int pos = Array.IndexOf<Symbol> (lambda.Formals, stat);
                bool inFree = lambda.FreeVariables.Contains (stat);
                if (inFree == true &&
                    pos == -1) {
                    this.importedStaticVariables.Add (stat);
                }
                else if (inFree == false &&
                         pos == -1) {
                }
                else if (inFree == false) {
                    //Debugger.Break ();
                }
                else
                    throw new NotImplementedException ();
            }
        }

        public LType Lambda { [DebuggerStepThrough] get { return this.lambda; } }
        public PartialEnvironment Environment { [DebuggerStepThrough] get { return this.environment; } }
        public IDictionary<Symbol, ValueCell> ExportedTopLevelVariables { [DebuggerStepThrough] get { return this.exportedTopLevelVariables; } }

        /// <summary>
        /// Returns the array of symbols bound when applying this closure.
        /// </summary>
        public Symbol [] BoundVariables
        {
            [DebuggerStepThrough]
            get
            {
                return this.lambda.Formals;
            }
        }

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

        public StaticMapping [] StaticMapping
        {
            get
            {
                return this.staticMapping;
            }
        }

        public IList<Symbol> ImportedStaticVariables
        {
            [DebuggerStepThrough]
            get
            {
                return this.importedStaticVariables;
            }
        }

        internal int FormalOffset (object name)
        {
            return this.lambda.LexicalOffset (name);
        }
    }
}

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;

namespace Microcode
{

    sealed class PartialClosure<LType> where LType : LambdaBase
    //: ClosureBase<LType> 
    {
        readonly LType lambda;
        readonly PartialEnvironment environment;
        StaticMapping [] staticMapping;
        IList<Symbol> importedStaticVariables;

        public PartialClosure (LType lambda, PartialEnvironment environment)
        //: base (lambda, environment, null)
        {
            this.lambda = lambda;
            this.environment = environment;
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

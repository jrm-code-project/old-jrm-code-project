using System;
using System.Collections.Generic;
using System.Diagnostics;

namespace Microcode
{
    /// <summary>
    /// Represents the evaluation environment.  Must be public.
    /// </summary>
    [Serializable]
    public abstract class Environment : SchemeObject, ISystemVector
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public override TC TypeCode { get { return TC.ENVIRONMENT; } }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        static readonly GlobalEnvironment systemGlobalEnvironment = new GlobalEnvironment ();

        public static GlobalEnvironment Global
        {
            [DebuggerStepThrough]
            get
            {
                return systemGlobalEnvironment;
            }
        }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected static readonly IDictionary<Symbol, ValueCell> noExportedTopLevelVariables = new Dictionary<Symbol, ValueCell> (0);

        // This is to capture stupid calls to `ToEnvironment' when we
        // already know it is an environmnet.
        static public Environment ToEnvironment (Environment env)
        {
            throw new NotImplementedException ();
        }

        static public Environment ToEnvironment (object env)
        {
            Environment eenv = env as Environment;
            if (eenv == null) {
                // MIT Scheme allows #F to mean the global environment.
                if (env is bool && (bool) env == false)
                    return systemGlobalEnvironment;
                else
                    throw new NotImplementedException ();
            }
            else
                return eenv;
        }

#if DEBUG
        [NonSerialized]
        static protected long [] foundInAuxes = new long [128];
        [NonSerialized]
        static protected long [] foundInGlobal = new long [128];
        [NonSerialized]
        static internal long [] extendedBy = new long [1024];
        [NonSerialized]
        static internal long [] valueCellsCopied = new long [128];
        [NonSerialized]
        static internal Histogram<int[]> staticMappings = new Histogram<int[]> ();
#endif

        // Abstract functions on environments

        // Grab the variable at randOffset in frame.  Must be int.
        public abstract object ArgumentValue (int offset);
        public abstract object Argument0Value { get; }
        public abstract object Argument1Value { get; }

        // Deep search for variable location and smash the value.
        public abstract bool Assign (out object oldValue, object name, object newValue);
        internal abstract bool AssignArgument (out object oldValue, int offset, object newValue);
        internal abstract bool AssignStatic (out object oldValue, int offset, object newValue);

        // Define a variable in the topmost frame.  This will shadow
        // other definitions.  Returns false on success, true if there
        // is a problem.
        public abstract bool Define (object name, object value);

        // Deep search the lexical environment for the variable.
        // returns false on success, true if there is a problem.
        public abstract bool DeepSearch (out object value, object name);
        // internal call tracks randDepth.
        internal abstract bool DeepSearch (out object value, object name, uint depth);
        internal abstract bool DeepSearchType (out object value, object name);

        // Similar to deep search, but returns a ValueCell.
        public abstract bool FreeReference (out ValueCell value, object name);
        internal abstract bool FreeReference (out ValueCell value, object name, int depth);

        // Used to link variables.
        internal abstract ValueCell GetValueCell (object name);
        internal abstract Environment BaseEnvironment { get; }

        internal abstract bool StaticValue (out object value, object name, int staticOffset);

        //internal abstract ValueCell [] GetValueCells (StaticMapping mapping);
        internal abstract object[] GetValueCells (StaticMapping mapping);

        // Implementation of primitive.
        internal abstract bool SafeDeepSearch (out object value, object name);

        // Used to link variables.
        internal abstract bool SetValueCell (object name, ValueCell newCell);
        internal abstract bool IsUnbound (object name);
        internal abstract bool IsUnreferenceable (object name);

        internal abstract bool UnbindVariable (out object answer, object name);

        [SchemePrimitive ("LEXICAL-UNBOUND?", 2, false)]
        public static bool IsLexicalUnbound (out object answer, object env, object name)
        {
            answer = ToEnvironment (env).IsUnbound ((Symbol) name);
            return false; // copacetic
        }

        [SchemePrimitive ("LEXICAL-UNREFERENCEABLE?", 2, false)]
        public static bool IsLexicalUnreferenceable (out object answer, object env, object name)
        {
            answer = ToEnvironment (env).IsUnreferenceable ((Symbol) name);
            return false; // copacetic
        }

        // Same as assigning in the interpreter.
        [SchemePrimitive ("LEXICAL-ASSIGNMENT", 3, false)]
        public static bool LexicalAssignment (out object answer, object aenv, object aname, object value)
        {
            object oldValue;
            if (ToEnvironment (aenv).Assign (out oldValue, (Symbol) aname, value))
                throw new NotImplementedException ("error during assignment");
            answer = oldValue;
            return false;
        }

        // Same as evaluating variable.
        [SchemePrimitive ("LEXICAL-REFERENCE", 2, false)]
        public static bool LexicalReference (out object answer, object env, object name)
        {
            if (ToEnvironment (env).DeepSearch (out answer, (Symbol) name))
                throw new NotImplementedException ("Error during lexical-reference.");
            return false;
        }

        // Looks up variable, tells whether it is a macro, unassigned, etc.
        [SchemePrimitive ("LEXICAL-REFERENCE-TYPE", 2, false)]
        public static bool LexicalReferenceType (out object answer, object env, object name)
        {
            if (ToEnvironment (env).DeepSearchType (out answer, (Symbol) name))
                throw new NotImplementedException ("Error during lexical-reference-type.");
            return false;
        }

        [SchemePrimitive ("SAFE-LEXICAL-REFERENCE", 2, false)]
        public static bool SafeLexicalReference (out object answer, object env, object name)
        {
            if (ToEnvironment (env).SafeDeepSearch (out answer, (Symbol) name))
                throw new NotImplementedException ("Error during lexical-reference.");
            return false;
        }

        [SchemePrimitive ("LINK-VARIABLES", 4, false)]
        public static bool LinkVariables (out object answer, object [] arglist)
        {
            Environment target_env = ToEnvironment (arglist [0]);

            Symbol target_name = (Symbol) arglist [1];
            Environment source_env = ToEnvironment (arglist [2]);
            Symbol source_name = (Symbol) arglist [3];
            if (target_env.SetValueCell (target_name, source_env.GetValueCell (source_name)))
                throw new NotImplementedException ("Error during link-variables.");
            else {
                answer = null;
                return false;
            }
        }

        // This has the same effect as a DEFINE in the interpreter.
        [SchemePrimitive ("LOCAL-ASSIGNMENT", 3, false)]
        public static bool LocalAssignment (out object answer, object env, object aname, object value)
        {
            if (ToEnvironment (env).Define ((Symbol) aname, value)) throw new NotImplementedException ();

            answer = aname;
            return false;
        }

        [SchemePrimitive ("ENVIRONMENT?", 1, true)]
        public static bool IsEnvironment (out object answer, object arg)
        {
            answer = arg is Environment || (arg is bool && (bool) arg == false);
            return false;
        }

        [SchemePrimitive ("UNBIND-VARIABLE", 2, false)]
        public static bool UnbindVariable (out object answer, object env, object name)
        {
            if (ToEnvironment (env).UnbindVariable (out answer, (Symbol) name))
                throw new NotImplementedException ("Error during lexical-reference-type.");
            return false;
        }

        #region ISystemVector Members

        public int SystemVectorSize
        {
            get { throw new NotImplementedException (); }
        }

        public virtual object SystemVectorRef (int index)
        {
            throw new NotImplementedException ();
        }

        public object SystemVectorSet (int index, object newValue)
        {
            throw new NotImplementedException ();
        }

        #endregion
    }

    [Serializable]
    class NullEnvironment : Environment, ITopLevelEnvironment
    {
        public NullEnvironment ()
            : base ()
        {
        }

        public override object ArgumentValue (int offset)
        {
            throw new NotImplementedException ();
        }

        public override bool Assign (out object oldValue, object name, object newValue)
        {
            throw new NotImplementedException ();
        }

        internal override bool AssignArgument (out object oldValue, int offset, object newValue)
        {
            throw new NotImplementedException ();
        }

        internal override bool AssignStatic (out object oldValue, int offset, object newValue)
        {
            throw new NotImplementedException ();
        }

        public override bool DeepSearch (out object value, object name)
        {
            throw new NotImplementedException ();
        }

        internal override bool DeepSearch (out object value, object name, uint depth)
        {
            throw new NotImplementedException ();
        }

        public override bool Define (object name, object value)
        {
            throw new NotImplementedException ();
        }

        internal override ValueCell GetValueCell (object name)
        {
            throw new NotImplementedException ();
        }

        internal override bool SetValueCell (object name, ValueCell newCell)
        {
            throw new NotImplementedException ();
        }

        internal override bool IsUnreferenceable (object name)
        {
            throw new NotImplementedException ();
        }

        internal override bool SafeDeepSearch (out object value, object name)
        {
            throw new NotImplementedException ();
        }

        internal override bool DeepSearchType (out object value, object name)
        {
            throw new NotImplementedException ();
        }

        internal override bool IsUnbound (object name)
        {
            throw new NotImplementedException ();
        }

        public override object Argument0Value
        {
            get
            {
                throw new NotImplementedException ();
            }
        }
        public override object Argument1Value
        {
            get
            {
                throw new NotImplementedException ();
            }
        }

        internal override bool UnbindVariable (out object answer, object name)
        {
            throw new NotImplementedException ();
        }



        internal override bool StaticValue (out object value, object name, int staticOffset)
        {
            throw new NotImplementedException ();
        }

        internal override Environment BaseEnvironment
        {
            get
            {
                throw new NotImplementedException ();
            }
        }

        internal override object [] GetValueCells (StaticMapping mapping)
        {
            throw new NotImplementedException ();
        }

        public override bool FreeReference (out ValueCell value, object name)
        {
            throw new NotImplementedException ();
        }

        internal override bool FreeReference (out ValueCell value, object name, int depth)
        {
            throw new NotImplementedException ();
        }

        #region ITopLevelEnvironment Members

        public IDictionary<Symbol, ValueCell> ExportedTopLevelVariables
        {
            get { return noExportedTopLevelVariables; }
        }

        public SCode SpecializeVariable (IVariableSpecializer variable)
        {
            return variable.MakeFree ();
        }

        #endregion
    }

    [Serializable]
    public sealed class GlobalEnvironment : Environment, ITopLevelEnvironment
    {
        // Object -> value cell so we have EQ semantics
        // rather than string= semantics.
        readonly Dictionary<object, ValueCell> globalBindings = new Dictionary<object, ValueCell> ();

        public GlobalEnvironment ()
            : base ()
        {
        }

        public override object ArgumentValue (int offset)
        {
            throw new NotImplementedException ();
        }


        internal override bool AssignArgument (out object oldValue, int offset, object newValue)
        {
            throw new NotImplementedException ();
        }

        internal override bool AssignStatic (out object oldValue, int offset, object newValue)
        {
            throw new NotImplementedException ();
        }

        public override bool Assign (out object oldValue, object name, object newValue)
        {
            ValueCell vcell = null;
            if (this.globalBindings.TryGetValue (name, out vcell)) {
                if (vcell.Assign (out oldValue, newValue))
                    throw new NotImplementedException ("Error in Assign.");
                return false;
            }
            throw new NotImplementedException ("IsUnbound variable in Assign.");
        }

        public override bool Define (object name, object value)
        {
            ValueCell cell = null;
            if (this.globalBindings.TryGetValue (name, out cell) == false) {
                cell = new ValueCell (name, value);
                this.globalBindings.Add (name, cell);
            }
            else {
                object oldValue;
                Debug.WriteLine ("Redefining " + name);
                cell.Assign (out oldValue, value);
            }
            return false; // no problems
        }

        public override bool DeepSearch (out object value, object name)
        {
            return DeepSearch (out value, name, 0);
        }

        internal override bool DeepSearch (out object value, object name, uint depth)
        {
            ValueCell cell;
            if (this.globalBindings.TryGetValue (name, out cell)) {
#if DEBUG
                foundInGlobal [depth] += 1;
#endif
                return cell.GetValue (out value);
            }
            value = false;
            return true;
        }

        internal override ValueCell GetValueCell (object name)
        {
            ValueCell cell = null;
            this.globalBindings.TryGetValue (name, out cell);
            return cell;
        }

        internal override bool SafeDeepSearch (out object value, object name)
        {
            ValueCell cell = null;
            if (this.globalBindings.TryGetValue (name, out cell)) {

                if (cell.SafeGetValue (out value))
                    throw new NotImplementedException ("Error getting value from cell");
                return false;
            }
            throw new NotImplementedException ("Variable not bound");
        }

        internal override bool DeepSearchType (out object value, object name)
        {
            ValueCell cell = null;
            if (this.globalBindings.TryGetValue (name, out cell)) {

                if (cell.GetType (out value))
                    throw new NotImplementedException ("Error getting value from cell");
                return false;
            }
            // if it is unbound, the reference type is 0
            value = 0;
            return false;
        }

        internal override bool SetValueCell (object name, ValueCell newCell)
        {
            ValueCell cell = null;
            if (this.globalBindings.TryGetValue (name, out cell)) {
                if (cell != newCell)
                    throw new NotImplementedException ("Value cell already exists.");
                else
                    // No effect to just do it again, even if it is weird.
                    return false;
            }
            this.globalBindings.Add (name, newCell);
            return false; // copacetic
        }

        internal override bool IsUnreferenceable (object name)
        {
            ValueCell vcell;
            if (this.globalBindings.TryGetValue (name, out vcell)) {
                return vcell.Unreferenceable ();
            }
            else return true;
        }

        internal override bool IsUnbound (object name)
        {
            throw new NotImplementedException ();
        }

        public override object Argument0Value
        {
            get
            {
                throw new NotImplementedException ();
            }
        }

        public override object Argument1Value
        {
            get
            {
                throw new NotImplementedException ();
            }
        }

        internal override bool UnbindVariable (out object answer, object name)
        {
            throw new NotImplementedException ();
        }

        internal override bool StaticValue (out object value, object name, int staticOffset)
        {
            throw new NotImplementedException ();
        }

        internal override Environment BaseEnvironment
        {
            get
            {
                return this;
            }
        }

        object [] noValueCells = new ValueCell [0];
        internal override object [] GetValueCells (StaticMapping mapping)
        {
#if DEBUG
            valueCellsCopied [0] += 1;
#endif
            if (mapping == null)
                throw new ArgumentNullException ("mapping");
            if (mapping.Size != 0)
                throw new NotImplementedException ();
            else return noValueCells;
        }

        public override bool FreeReference (out ValueCell value, object name)
        {
            return this.FreeReference (out value, name, 0);
        }

        internal override bool FreeReference (out ValueCell value, object name, int depth)
        {
            if (this.globalBindings.TryGetValue (name, out value)) {
#if DEBUG
                foundInGlobal [depth] += 1;
#endif
                return false;
            }
            value = null;
            return true;
        }


        #region ITopLevelEnvironment Members

        public IDictionary<Symbol, ValueCell> ExportedTopLevelVariables
        {
            get { return noExportedTopLevelVariables; }
        }

        public SCode SpecializeVariable (IVariableSpecializer variable)
        {
            return variable.MakeGlobal (this);
        }

        #endregion
    }

    [Serializable]
    abstract class LexicalEnvironment<LType> : Environment where LType:LambdaBase
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly ClosureBase<LType> envClosure;

        protected LexicalEnvironment (ClosureBase<LType> closure)
            : base ()
        {
            this.envClosure = closure;
        }

        public ClosureBase<LType> Closure
        {
            [DebuggerStepThrough]
            get
            {
                return this.envClosure;
            }
        }

        public override bool Assign (out object oldValue, object name, object newValue)
        {
            throw new NotImplementedException ();
        }

        public override bool Define (object name, object value)
        {
            throw new NotImplementedException ();
        }

        public override bool DeepSearch (out object value, object name)
        {
            throw new NotImplementedException ();
        }

        internal override bool DeepSearch (out object value, object name, uint depth)
        {
            throw new NotImplementedException ();
        }

        internal override bool DeepSearchType (out object value, object name)
        {
            throw new NotImplementedException ();
        }

        public override bool FreeReference (out ValueCell value, object name)
        {
            throw new NotImplementedException ();
        }

        internal override bool FreeReference (out ValueCell value, object name, int depth)
        {
            throw new NotImplementedException ();
        }

        internal override ValueCell GetValueCell (object name)
        {
            throw new NotImplementedException ();
        }

        internal override Environment BaseEnvironment
        {
            get
            {
                throw new NotImplementedException ();
            }
        }

        internal override bool StaticValue (out object value, object name, int staticOffset)
        {
            return this.Closure.StaticValue (out value, name, staticOffset);
        }

        //internal override object [] GetValueCells (StaticMapping mapping)
        //{
        //    throw new NotImplementedException ();
        //}

        internal override bool SafeDeepSearch (out object value, object name)
        {
            throw new NotImplementedException ();
        }

        internal override bool SetValueCell (object name, ValueCell newCell)
        {
            throw new NotImplementedException ();
        }

        internal override bool IsUnbound (object name)
        {
            throw new NotImplementedException ();
        }

        internal override bool IsUnreferenceable (object name)
        {
            throw new NotImplementedException ();
        }

        internal override bool UnbindVariable (out object answer, object name)
        {
            throw new NotImplementedException ();
        }
    }

    /// <summary>
    /// A StandardEnvironment supports sharable bindingValues and incremental
    /// definition.
    /// </summary>
    [Serializable]
    sealed class StandardEnvironment<LType, CType> : LexicalEnvironment<LType>, ITopLevelEnvironment where CType: ClosureBase<LType> where LType: LambdaBase
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly ValueCell [] bindings;
        Dictionary <object,ValueCell> incrementals;

        static ValueCell [] noBindings = new ValueCell [0];
        internal StandardEnvironment (CType closure)
            : base (closure)
        {
            this.bindings = noBindings;
#if DEBUG
            extendedBy [0] += 1;
            // sanity check.
            object [] formals = closure.Lambda.Formals;
            if (formals.Length != 0)
                throw new NotImplementedException ();
#endif
        }

        internal StandardEnvironment (CType closure, object [] initialValues)
            : base (closure)
        {
            object [] formals = closure.Lambda.Formals;
            this.bindings = new ValueCell [initialValues.Length];
            for (int i = 0; i < initialValues.Length; i++)
                this.bindings [i] = new ValueCell (formals [i], initialValues [i]);
#if DEBUG
            extendedBy [initialValues.Length] += 1;
            // sanity check
            if (formals.Length != initialValues.Length)
                throw new NotImplementedException ();
#endif
        }

        public override object ArgumentValue (int offset)
        {
            object answer;
            if (bindings [offset].GetValue (out answer)) throw new NotImplementedException ();
            return answer;
        }


        internal override bool AssignArgument (out object oldValue, int offset, object newValue)
        {
            return bindings [offset].Assign (out oldValue, newValue);
        }

        internal override bool AssignStatic (out object oldValue, int offset, object newValue)
        {
            return this.envClosure.SetStaticValue (out oldValue, null, newValue, offset);
        }

        public override object Argument0Value
        {
            get
            {
                object answer;
                if (bindings [0].GetValue (out answer)) throw new NotImplementedException ();
                return answer;
            }
        }

        public override object Argument1Value
        {
            get
            {
                object answer;
                if (bindings [1].GetValue (out answer)) throw new NotImplementedException ();
                return answer;
            }
        }

        public override bool Assign (out object oldValue, object name, object newValue)
        {
            int offset = this.envClosure.FormalOffset (name);
            if (offset == -1) {
                ValueCell vcell;
                if (this.incrementals != null
                    && this.incrementals.TryGetValue (name, out vcell))
                    return vcell.Assign (out oldValue, newValue);
                else
                    return envClosure.Environment.Assign (out oldValue, name, newValue);
            }
            return bindings [offset].Assign (out oldValue, newValue);
        }

        public override bool Define (object name, object value)
        {
            int offset = this.envClosure.FormalOffset (name);
            if (offset == -1) {
                if (this.incrementals == null)
                    this.incrementals = new Dictionary<object, ValueCell> ();
                ValueCell vcell;
                if (this.incrementals.TryGetValue (name, out vcell)) {
                    // cell exists.
                    object oldValue = null;
                    if (vcell.Assign (out oldValue, value))
                        throw new NotImplementedException ("Error during incremental definition.");
                    return false; // copacetic
                }
                else {
                    this.incrementals.Add (name, new ValueCell (name, value));
                    return false;
                }
            }
            else {
                object oldValue = null;
                if (this.bindings [offset].Assign (out oldValue, value))
                    throw new NotImplementedException ("Error during redefinition.");
                return false;
            }
        }

        public override bool DeepSearch (out object value, object name)
        {
            return DeepSearch (out value, name, 0);
        }

        internal override bool DeepSearch (out object value, object name, uint depth)
        {
            int offset = this.envClosure.FormalOffset (name);
            if (offset == -1) {
                ValueCell vcell;
                if (this.incrementals != null
                    && this.incrementals.TryGetValue (name, out vcell)) {
#if DEBUG
                    foundInAuxes [depth] += 1;
#endif
                    return vcell.GetValue (out value);
                }
                else
                    return envClosure.Environment.DeepSearch (out value, name, depth + 1);
            }
            else
                return bindings [offset].GetValue (out value);
        }

        internal override ValueCell GetValueCell (object name)
        {
            int offset = this.envClosure.FormalOffset (name);
            if (offset == -1) {
                ValueCell vcell;
                if (this.incrementals != null
                    && this.incrementals.TryGetValue (name, out vcell)) {
                    return vcell;
                }
                else
                    return envClosure.Environment.GetValueCell (name);
            }
            return bindings [offset];
        }

        internal override bool SetValueCell (object name, ValueCell newCell)
        {
            int offset = this.envClosure.FormalOffset (name);
            if (offset == -1) {
                ValueCell vcell;
                if (incrementals != null
                    && incrementals.TryGetValue (name, out vcell)) {
                    incrementals.Remove (name);
                    incrementals.Add (name, newCell);
                    return false;
                    //throw new NotImplementedException ("Existing value cell in incrementals.");
                }
                else {
                    // no existing value cell in incrementals.
                    // add one.
                    // Shadow any existing value cell.
                    incrementals.Add (name, newCell);
                    return false; //ok
                }
            }
            // A value cell exists in the formal parameters.
            // Replace it.
            this.bindings [offset] = newCell;
            return false;
        }

        internal override bool SafeDeepSearch (out object value, object name)
        {
            int offset = this.envClosure.FormalOffset (name);
            if (offset == -1) {
                ValueCell vcell;
                if (this.incrementals != null
                    && this.incrementals.TryGetValue (name, out vcell)) {
                    return vcell.SafeGetValue (out value);
                }
                else
                    return envClosure.Environment.SafeDeepSearch (out value, name);
            }
            return bindings [offset].SafeGetValue (out value);
        }

        internal override bool DeepSearchType (out object value, object name)
        {
            int offset = this.envClosure.FormalOffset (name);
            if (offset == -1) {
                ValueCell vcell;
                if (this.incrementals != null
                    && this.incrementals.TryGetValue (name, out vcell)) {
                    return vcell.GetType (out value);
                }
                else
                    return envClosure.Environment.DeepSearchType (out value, name);
            }
            return bindings [offset].GetType (out value);
        }

        internal override bool IsUnreferenceable (object name)
        {
            int offset = this.envClosure.FormalOffset (name);
            if (offset == -1) {
                ValueCell vcell;
                if (incrementals != null
                    && incrementals.TryGetValue (name, out vcell))
                    return vcell.Unreferenceable ();
                else
                    return this.envClosure.Environment.IsUnreferenceable (name);
            }
            return bindings [offset].Unreferenceable ();
        }

        internal override bool IsUnbound (object name)
        {
            int offset = this.envClosure.FormalOffset (name);
            if (offset == -1) {
                ValueCell vcell;
                if (incrementals != null
                    && incrementals.TryGetValue (name, out vcell))
                    return vcell.Unbound ();
                else
                    return this.envClosure.Environment.IsUnbound (name);
            }
            return bindings [offset].Unbound ();
        }

        public override object SystemVectorRef (int index)
        {
            if (index == 0)
                return this.envClosure;
            else {
                object answer;
                if (this.bindings [index - 1].GetValue (out answer))
                    throw new NotImplementedException ();
                return answer;
            }
        }

        internal override bool UnbindVariable (out object answer, object name)
        {
            int offset = this.envClosure.FormalOffset (name);
            if (offset == -1) {
                ValueCell vcell;
                if (incrementals != null
                    && incrementals.TryGetValue (name, out vcell)) {
                    // Ugh!  Delete the cell!
                    incrementals.Remove (name);
                    answer = Constant.sharpT;
                    return false;
                }

                else
                    return this.envClosure.Environment.UnbindVariable (out answer, name);
            }
            throw new NotImplementedException ("Found in bindings.");
        }

        internal override bool StaticValue (out object value, object name, int staticOffset)
        {
            // We might have a local that shadows the binding.
            ValueCell vcell;
            if (incrementals != null
                && incrementals.TryGetValue (name, out vcell)) {
                return vcell.GetValue (out value);
            }
            return this.Closure.StaticValue (out value, name, staticOffset);
        }

        internal override Environment BaseEnvironment
        {
            get
            {
                return this;
            }
        }

        internal override object [] GetValueCells (StaticMapping mapping)
        {
#if DEBUG
            SCode.location = "StandardEnvironment.GetValueCells";
            valueCellsCopied [mapping.Size] += 1;
            //staticMappings.Note (mapping);
#endif
            int count = mapping.Size;
            object [] newCells = new object [count];
            for (int index = 0; index < count; index++) {
                int o = mapping.GetOffset (index);
                newCells [index] = this.bindings [o];
            }
            return newCells;
        }

        public override bool FreeReference (out ValueCell value, object name)
        {
            return this.FreeReference (out value, name, 0);
//            // Unrolled by one.  We shouldn't need to search our own frame
//            // because that would have been turned into an argument reference
//            // (I hope).
//            ValueCell vcell;
//            if (this.incrementals != null &&
//                this.incrementals.TryGetValue (name, out vcell)) {
//#if DEBUG
//                foundInAuxes [0] += 1;
//#endif
//                value = vcell;
//                return false;
//            }
//            return envClosure.Environment.FreeReference (out value, name, 1);
        }

        internal override bool FreeReference (out ValueCell value, object name, int depth)
        {
            int offset = envClosure.FormalOffset (name);
            if (offset == -1) {
                ValueCell vcell;
                if (this.incrementals != null &&
                    this.incrementals.TryGetValue (name, out vcell)) {
#if DEBUG
                    foundInAuxes [0] += 1;
#endif
                    value = vcell;
                    return false;
                }
                return envClosure.Environment.FreeReference (out value, name, depth+1);
            }
            else {
                value = bindings [offset];
                return false;
            }

        }

        #region ITopLevelEnvironment Members

        // We'll need these if we ever try to load or eval
        // in this environment, and if we try once, we'll
        // probably try again.
        IDictionary<Symbol, ValueCell> exportedTopLevelVariables;

        IDictionary<Symbol, ValueCell> ComputeExportedTopLevelVariables ()
        {
            Symbol [] names = this.envClosure.BoundVariables;
            Dictionary<Symbol, ValueCell> answer = new Dictionary<Symbol, ValueCell> (names.Length);
            for (int i = 0; i < names.Length; i++) {
                answer.Add (names [i], this.bindings [i]);
            }
            return answer;
        }

        public IDictionary<Symbol, ValueCell> ExportedTopLevelVariables
        {
            get {
                if (this.exportedTopLevelVariables == null) {
                    this.exportedTopLevelVariables = ComputeExportedTopLevelVariables();
                }
                return this.exportedTopLevelVariables;
            }
        }

        public SCode SpecializeVariable (IVariableSpecializer variable)
        {
            return variable.MakeFree();
        }

        #endregion
    }

    /// <summary>
    /// A StaticEnvironment supports sharable bindingValues.
    /// </summary>
    [Serializable]
    sealed class StaticEnvironment : LexicalEnvironment<StaticLambda>
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly ValueCell [] bindings;

        static ValueCell [] noBindings = new ValueCell [0];
        internal StaticEnvironment (StaticClosure closure)
            : base (closure)
        {
            this.bindings = noBindings;
#if DEBUG
            extendedBy [0] += 1;
            // sanity check.
            object [] formals = closure.Lambda.Formals;
            if (formals.Length != 0)
                throw new NotImplementedException ();
#endif
        }

        internal StaticEnvironment (ClosureBase<StaticLambda> closure, object [] initialValues)
            : base (closure)
        {
            object [] formals = closure.Lambda.Formals;
            this.bindings = new ValueCell [initialValues.Length];
            for (int i = 0; i < initialValues.Length; i++)
                this.bindings [i] = new ValueCell (formals [i], initialValues [i]);
#if DEBUG
            extendedBy [initialValues.Length] += 1;
            // sanity check
            if (formals.Length != initialValues.Length)
                throw new NotImplementedException ();
#endif
        }

        public override object ArgumentValue (int offset)
        {
            object answer;
            if (bindings [offset].GetValue (out answer)) throw new NotImplementedException ();
            return answer;
        }


        internal override bool AssignArgument (out object oldValue, int offset, object newValue)
        {
            return bindings [offset].Assign (out oldValue, newValue);
        }

        internal override bool AssignStatic (out object oldValue, int offset, object newValue)
        {
            return this.envClosure.SetStaticValue (out oldValue, null, newValue, offset);
        }

        public override object Argument0Value
        {
            get
            {
                object answer;
                if (bindings [0].GetValue (out answer)) throw new NotImplementedException ();
                return answer;
            }
        }

        public override object Argument1Value
        {
            get
            {
                object answer;
                if (bindings [1].GetValue (out answer)) throw new NotImplementedException ();
                return answer;
            }
        }

        public override bool Assign (out object oldValue, object name, object newValue)
        {
            int offset = this.envClosure.FormalOffset (name);
            if (offset != -1) return bindings [offset].Assign (out oldValue, newValue);
            int soffset = this.envClosure.StaticOffset (name);
            if (offset != -1) throw new NotImplementedException ("assign to static in static environment");
            return envClosure.Environment.Assign (out oldValue, name, newValue);
        }

        public override bool Define (object name, object value)
        {
            int offset = this.envClosure.FormalOffset (name);
            if (offset == -1) {
                throw new NotImplementedException ("No incremental definition in static environments.");
            }
            else {
                object oldValue = null;
                if (this.bindings [offset].Assign (out oldValue, value))
                    throw new NotImplementedException ("Error during redefinition.");
                return false;
            }
        }

        public override bool DeepSearch (out object value, object name)
        {
            return DeepSearch (out value, name, 0);
        }

        internal override bool DeepSearch (out object value, object name, uint depth)
        {
            int offset = this.envClosure.FormalOffset (name);
            if (offset == -1) {
               return envClosure.Environment.DeepSearch (out value, name, depth + 1);
            }
            return bindings [offset].GetValue (out value);
        }

        internal override ValueCell GetValueCell (object name)
        {
            int offset = this.envClosure.FormalOffset (name);
            if (offset == -1) {
                    return envClosure.Environment.GetValueCell (name);
            }
            return bindings [offset];
        }

        internal override bool SetValueCell (object name, ValueCell newCell)
        {
            int offset = this.envClosure.FormalOffset (name);
            if (offset == -1) {
                throw new NotImplementedException ("Cannot set incremental value cell in StaticEnvironment.");
            }
            // A value cell exists in the formal parameters.
            // Replace it.
            this.bindings [offset] = newCell;
            return false;
        }

        internal override bool SafeDeepSearch (out object value, object name)
        {
            int offset = this.envClosure.FormalOffset (name);
            if (offset == -1) {
                return envClosure.Environment.SafeDeepSearch (out value, name);
            }
            return bindings [offset].SafeGetValue (out value);
        }

        internal override bool DeepSearchType (out object value, object name)
        {
            int offset = this.envClosure.FormalOffset (name);
            if (offset == -1) {
                    return envClosure.Environment.DeepSearchType (out value, name);
            }
            return bindings [offset].GetType (out value);
        }

        internal override bool IsUnreferenceable (object name)
        {
            int offset = this.envClosure.FormalOffset (name);
            if (offset == -1) {
                    return this.envClosure.Environment.IsUnreferenceable (name);
            }
            return bindings [offset].Unreferenceable ();
        }

        internal override bool IsUnbound (object name)
        {
            int offset = this.envClosure.FormalOffset (name);
            if (offset == -1) {
                    return this.envClosure.Environment.IsUnbound (name);
            }
            return bindings [offset].Unbound ();
        }

        public override object SystemVectorRef (int index)
        {
            if (index == 0)
                return this.envClosure;
            else {
                object answer;
                if (this.bindings [index - 1].GetValue (out answer))
                    throw new NotImplementedException ();
                return answer;
            }
        }

        internal override bool UnbindVariable (out object answer, object name)
        {
            int offset = this.envClosure.FormalOffset (name);
            if (offset == -1) {
                    return this.envClosure.Environment.UnbindVariable (out answer, name);
            }
            throw new NotImplementedException ("Found in bindings.");
        }

        internal override Environment BaseEnvironment
        {
            get
            {
                return this.envClosure.Environment;
            }
        }

        internal override object [] GetValueCells (StaticMapping mapping)
        {
#if DEBUG
            SCode.location = "StaticEnvironment.GetValueCells";
            valueCellsCopied [mapping.Size] += 1;
            //staticMappings.Note (mapping);
#endif
            int count = mapping.Size;
            int formalCount = this.bindings.Length;
            object[] oldCells = this.Closure.StaticCells;
            object [] newCells = new object [count];
            for (int index = 0; index < count; index++) {
                int o = mapping.GetOffset(index);
                if (o < formalCount)
                    newCells [index] = this.bindings [o];
                else
                    newCells [index] = oldCells[o - formalCount];
            }
            return newCells;
        }

        public override bool FreeReference (out ValueCell value, object name)
        {
            return envClosure.Environment.FreeReference (out value, name, 1);
        }

        internal override bool FreeReference (out ValueCell value, object name, int depth)
        {
            int offset = envClosure.FormalOffset (name);
            if (offset == -1) {
                return envClosure.Environment.FreeReference (out value, name, depth + 1);
            }
            else {
                value = bindings [offset];
                return false;
            }
        }
    }

    /// <summary>
    /// A SimpleEnvironment cannot have assignments or incrementals.
    /// It doesn't use value cells, but stores the object directly.
    /// </summary>
    [Serializable]
    sealed class SimpleEnvironment : LexicalEnvironment<SimpleLambda>
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly object [] bindings;

        static object [] noBindings = new object [0];
        internal SimpleEnvironment (SimpleClosure closure)
            : base (closure)
        {
            this.bindings = noBindings;
#if DEBUG
            extendedBy [0] += 1;
            // sanity check.
            object [] formals = closure.Lambda.Formals;
            if (formals.Length != 0)
                throw new NotImplementedException ();
#endif
        }

        internal SimpleEnvironment (ClosureBase<SimpleLambda> closure, object [] initialValues)
            : base (closure)
        {
            object [] formals = closure.Lambda.Formals;
            this.bindings = initialValues;
#if DEBUG
            extendedBy [initialValues.Length] += 1;
            // sanity check
            if (formals.Length != initialValues.Length)
                throw new NotImplementedException ();
#endif
        }

        public override object ArgumentValue (int offset)
        {
            return bindings [offset];
        }


        internal override bool AssignArgument (out object oldValue, int offset, object newValue)
        {
            throw new NotImplementedException ();
        }

        internal override bool AssignStatic (out object oldValue, int offset, object newValue)
        {
            return this.envClosure.SetStaticValue (out oldValue, null, newValue, offset);
        }

        public override object Argument0Value
        {
            get
            {
                return bindings [0];
            }
        }

        public override object Argument1Value
        {
            get
            {
                return bindings [1];
            }
        }

        public override bool Assign (out object oldValue, object name, object newValue)
        {
            int offset = this.envClosure.FormalOffset (name);
            if (offset == -1) {
                return envClosure.Environment.Assign (out oldValue, name, newValue);
            }
            throw new NotImplementedException ("Assignment in Simple Environment.");
        }

        public override bool Define (object name, object value)
        {
            throw new NotImplementedException ("No assignment or definition.");
        }

        public override bool DeepSearch (out object value, object name)
        {
            return DeepSearch (out value, name, 0);
        }

        internal override bool DeepSearch (out object value, object name, uint depth)
        {
            int offset = this.envClosure.FormalOffset (name);
            if (offset == -1) {
                return envClosure.Environment.DeepSearch (out value, name, depth + 1);
            }
            value = bindings [offset];
            return false;
        }

        internal override ValueCell GetValueCell (object name)
        {
            throw new NotImplementedException ("No value cells in simple environment.");
        }

        internal override bool SetValueCell (object name, ValueCell newCell)
        {
            throw new NotImplementedException ("Cannot set value cell in SimpleEnvironment.");
        }

        internal override bool SafeDeepSearch (out object value, object name)
        {
            int offset = this.envClosure.FormalOffset (name);
            if (offset == -1) {
                return envClosure.Environment.SafeDeepSearch (out value, name);
            }
            value = bindings [offset];
            return false;
        }

        internal override bool DeepSearchType (out object value, object name)
        {
            int offset = this.envClosure.FormalOffset (name);
            if (offset == -1) {
                return envClosure.Environment.DeepSearchType (out value, name);
            }
            throw new NotImplementedException ("deepSearchType in simple environment");
        }

        internal override bool IsUnreferenceable (object name)
        {
            int offset = this.envClosure.FormalOffset (name);
            if (offset == -1) {
                return this.envClosure.Environment.IsUnreferenceable (name);
            }
            return false;
        }

        internal override bool IsUnbound (object name)
        {
            int offset = this.envClosure.FormalOffset (name);
            if (offset == -1) {
                return this.envClosure.Environment.IsUnbound (name);
            }
            return false;
        }

        public override object SystemVectorRef (int index)
        {
            if (index == 0)
                return this.envClosure;
            else {
                return this.bindings [index - 1];
            }
        }

        internal override bool UnbindVariable (out object answer, object name)
        {
            int offset = this.envClosure.FormalOffset (name);
            if (offset == -1) {
                return this.envClosure.Environment.UnbindVariable (out answer, name);
            }
            throw new NotImplementedException ("Cannot unbind in simple environment.");
        }

        internal override bool StaticValue (out object value, object name, int staticOffset)
        {
            return this.Closure.StaticValue (out value, name, staticOffset);
        }

        internal override Environment BaseEnvironment
        {
            get
            {
                return this.envClosure.Environment;
            }
        }

        internal override object [] GetValueCells (StaticMapping mapping)
        {
#if DEBUG
            SCode.location = "Simple.GetValueCells";
            valueCellsCopied [mapping.Size] += 1;
            staticMappings.Note (mapping.Offsets);
#endif
            int count = mapping.Size;
            int formalCount = this.bindings.Length;
            object [] oldCells = this.Closure.StaticCells;
            object [] cells = new object [count];
            for (int index = 0; index < count; index++) {
                int o = mapping.GetOffset(index);
                cells [index] = (o < formalCount) ?
                    //new ValueCell (this.Closure.Lambda.Formals[o], this.bindings [o]) :
                    this.bindings [o] :
                    oldCells [o - formalCount];
                    //this.Closure.StaticCell (o - formalCount);
            }
            return cells;
        }

        public override bool FreeReference (out ValueCell value, object name)
        {
            return envClosure.Environment.FreeReference (out value, name, 1);
        }

        internal override bool FreeReference (out ValueCell value, object name, int depth)
        {
            return envClosure.Environment.FreeReference (out value, name, depth + 1);
        }
    }

    [Serializable]
    sealed class SmallEnvironment0 : LexicalEnvironment<SimpleLambda>
    {
        internal SmallEnvironment0 (ClosureBase<SimpleLambda> closure)
            : base (closure)
        {
#if DEBUG
            extendedBy [0] += 1;
#endif
        }

        public override object ArgumentValue (int offset)
        {
            throw new NotImplementedException ("Bad offset");
        }


        internal override bool AssignArgument (out object oldValue, int offset, object newValue)
        {
            throw new NotImplementedException ();
        }

        internal override bool AssignStatic (out object oldValue, int offset, object newValue)
        {
            return this.envClosure.SetStaticValue (out oldValue, null, newValue, offset);
        }

        public override object Argument0Value
        {
            get
            {
                throw new NotImplementedException ("No argument 0");
            }
        }

        public override object Argument1Value
        {
            get
            {
                throw new NotImplementedException ("Argument1Value in SimpleEnvironment1");
            }
        }

        public override bool Assign (out object oldValue, object name, object newValue)
        {
            int offset = this.envClosure.FormalOffset (name);
            if (offset != -1) throw new NotImplementedException ("Assignment in Simple Environment.");
            int soffset = this.envClosure.StaticOffset (name);
            if (soffset != -1) return this.envClosure.SetStaticValue (out oldValue, name, newValue, soffset);
            return this.envClosure.Environment.Assign (out oldValue, name, newValue);
        }

        public override bool Define (object name, object value)
        {
            throw new NotImplementedException ("No assignment or definition.");
        }

        public override bool DeepSearch (out object value, object name)
        {
            return DeepSearch (out value, name, 0);
        }

        internal override bool DeepSearch (out object value, object name, uint depth)
        {

                return envClosure.Environment.DeepSearch (out value, name, depth + 1);
        }

        internal override ValueCell GetValueCell (object name)
        {
            throw new NotImplementedException ("No value cells in simple environment.");
        }

        internal override bool SetValueCell (object name, ValueCell newCell)
        {
            throw new NotImplementedException ("Cannot set value cell in SimpleEnvironment.");
        }

        internal override bool SafeDeepSearch (out object value, object name)
        {
                return envClosure.Environment.SafeDeepSearch (out value, name);
        }

        internal override bool DeepSearchType (out object value, object name)
        {
                return envClosure.Environment.DeepSearchType (out value, name);
        }

        internal override bool IsUnreferenceable (object name)
        {
                return this.envClosure.Environment.IsUnreferenceable (name);
        }

        internal override bool IsUnbound (object name)
        {
                return this.envClosure.Environment.IsUnbound (name);
        }

        public override object SystemVectorRef (int index)
        {
            if (index == 0)
                return this.envClosure;
            else
                throw new NotImplementedException ("Out of range");
        }

        internal override bool UnbindVariable (out object answer, object name)
        {
                return this.envClosure.Environment.UnbindVariable (out answer, name);
        }

        internal override bool StaticValue (out object value, object name, int staticOffset)
        {
            return this.Closure.StaticValue (out value, name, staticOffset);
        }

        internal override Environment BaseEnvironment
        {
            get
            {
                return this.envClosure.Environment;
            }
        }
        static int[] codes = new int[64];
        internal override object [] GetValueCells (StaticMapping mapping)
        {
#if DEBUG
            SCode.location = "SmallEnvironment0.GetValueCells";
            valueCellsCopied [mapping.Size] += 1;
            staticMappings.Note (mapping.Offsets);
            int code = mapping.OffsetCode;
            if (code >= 0 &&
                code < 64)
                codes[code] += 1;
#endif
            int count = mapping.Size;
            int [] offsets = mapping.Offsets;
            object [] newCells = new object [count];
            object [] oldCells = this.Closure.StaticCells;
            for (int index = 0; index < count; index++) {
                newCells [index] = oldCells[offsets[index]];
            }
            return newCells;
        }

        public override bool FreeReference (out ValueCell value, object name)
        {
            return envClosure.Environment.FreeReference (out value, name, 1);
        }

        internal override bool FreeReference (out ValueCell value, object name, int depth)
        {
            return envClosure.Environment.FreeReference (out value, name, depth + 1);
        }
    }

    [Serializable]
    sealed class SmallEnvironment1 : LexicalEnvironment<SimpleLambda>
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly object binding0;

        internal SmallEnvironment1 (ClosureBase<SimpleLambda> closure, object binding0Value)
            : base (closure)
        {
            this.binding0 = binding0Value;
#if DEBUG
            extendedBy [1] += 1;
#endif
        }

        public override object ArgumentValue (int offset)
        {
            if (offset == 0) return this.binding0;
            else throw new NotImplementedException ("Bad offset");
        }


        internal override bool AssignArgument (out object oldValue, int offset, object newValue)
        {
            throw new NotImplementedException ();
        }

        internal override bool AssignStatic (out object oldValue, int offset, object newValue)
        {
            return this.envClosure.SetStaticValue (out oldValue, null, newValue, offset);
        }

        public override object Argument0Value
        {
            get
            {
                return this.binding0;
            }
        }

        public override object Argument1Value
        {
            get
            {
                throw new NotImplementedException ("Argument1Value in SimpleEnvironment1");
            }
        }

        public override bool Assign (out object oldValue, object name, object newValue)
        {
            int offset = this.envClosure.FormalOffset (name);
            if (offset != -1) throw new NotImplementedException ("Assignment in Simple Environment.");
            int soffset = this.envClosure.StaticOffset (name);
            if (soffset != -1) return this.envClosure.SetStaticValue (out oldValue, name, newValue, soffset);
            return this.envClosure.Environment.Assign (out oldValue, name, newValue);
        }

        public override bool Define (object name, object value)
        {
            throw new NotImplementedException ("No assignment or definition.");
        }

        public override bool DeepSearch (out object value, object name)
        {
            return DeepSearch (out value, name, 0);
        }

        internal override bool DeepSearch (out object value, object name, uint depth)
        {
            int offset = this.envClosure.FormalOffset (name);
            if (offset == -1) {
                return envClosure.Environment.DeepSearch (out value, name, depth + 1);
            }
            if (offset == 0)
                value = this.binding0;
            else
                throw new NotImplementedException ();
            return false;
        }

        internal override ValueCell GetValueCell (object name)
        {
            throw new NotImplementedException ("No value cells in simple environment.");
        }

        internal override bool SetValueCell (object name, ValueCell newCell)
        {
            throw new NotImplementedException ("Cannot set value cell in SimpleEnvironment.");
        }

        internal override bool SafeDeepSearch (out object value, object name)
        {
            int offset = this.envClosure.FormalOffset (name);
            if (offset == -1) {
                return envClosure.Environment.SafeDeepSearch (out value, name);
            }
            if (offset == 0)
                value = binding0;
            else
                throw new NotImplementedException ();
            return false;
        }

        internal override bool DeepSearchType (out object value, object name)
        {
            int offset = this.envClosure.FormalOffset (name);
            if (offset == -1) {
                return envClosure.Environment.DeepSearchType (out value, name);
            }
            throw new NotImplementedException ("deepSearchType in simple environment");
        }

        internal override bool IsUnreferenceable (object name)
        {
            int offset = this.envClosure.FormalOffset (name);
            if (offset == -1) {
                return this.envClosure.Environment.IsUnreferenceable (name);
            }
            return false;
        }

        internal override bool IsUnbound (object name)
        {
            int offset = this.envClosure.FormalOffset (name);
            if (offset == -1) {
                return this.envClosure.Environment.IsUnbound (name);
            }
            return false;
        }

        public override object SystemVectorRef (int index)
        {
            if (index == 0)
                return this.envClosure;
            else if (index == 1)
                return this.binding0;
            else
                throw new NotImplementedException ("Out of range");
        }

        internal override bool UnbindVariable (out object answer, object name)
        {
            int offset = this.envClosure.FormalOffset (name);
            if (offset == -1) {
                return this.envClosure.Environment.UnbindVariable (out answer, name);
            }
            throw new NotImplementedException ("Cannot unbind in simple environment.");
        }

        internal override bool StaticValue (out object value, object name, int staticOffset)
        {
            return this.Closure.StaticValue (out value, name, staticOffset);
        }

        internal override Environment BaseEnvironment
        {
            get
            {
                return this.envClosure.Environment;
            }
        }

        static int [] codes = new int [64];

        internal override object [] GetValueCells (StaticMapping mapping)
        {
#if DEBUG
            SCode.location = "SmallEnvironment1.GetValueCells";
            valueCellsCopied [mapping.Size] += 1;
            staticMappings.Note (mapping.Offsets);
            int code = mapping.OffsetCode;
            if (code >= 0 &&
                code < 64)
                codes[code] += 1;
#endif
            int count = mapping.Size;
            int [] offsets = mapping.Offsets;
            object [] incomingCells = this.envClosure.StaticCells;
            object [] cells = new object [count];
            for (int index = 0; index < count; index++) {
                int offset = offsets [index];
                if (offset == 0)
                    //cells [index] = new ValueCell (this.Closure.Lambda.Formals [0], this.binding0);
                    cells [index] = this.binding0;
                else
                    cells [index] = incomingCells [offset - 1];
            }
            //if (count > 0) {
            //    if (offsets[0] == -1) {
            //        cells[0] = new ValueCell (this.Closure.Lambda.Formals[0], this.binding0);
            //        for (int index = 1; index < count; index++) {
            //            cells [index] = incomingCells [offsets [index]];
            //        }
            //    }
            //    else {
            //        for (int index = 0; index < count; index++) {
            //            cells [index] = incomingCells [offsets [index]];
            //        }
            //    }
            //}
            return cells;
        }

        public override bool FreeReference (out ValueCell value, object name)
        {
            return envClosure.Environment.FreeReference (out value, name, 1);
        }

        internal override bool FreeReference (out ValueCell value, object name, int depth)
        {
            return envClosure.Environment.FreeReference (out value, name, depth + 1);
        }

    }

    [Serializable]
    sealed class SmallEnvironment2 : LexicalEnvironment<SimpleLambda>
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly object binding0;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly object binding1;

        internal SmallEnvironment2 (ClosureBase<SimpleLambda> closure, object binding0Value, object binding1Value)
            : base (closure)
        {
            this.binding0 = binding0Value;
            this.binding1 = binding1Value;
#if DEBUG
            extendedBy [2] += 1;
#endif
        }

        internal override bool AssignArgument (out object oldValue, int offset, object newValue)
        {
            throw new NotImplementedException ();
        }

        internal override bool AssignStatic (out object oldValue, int offset, object newValue)
        {
            return this.envClosure.SetStaticValue (out oldValue, null, newValue, offset);
        }

        public override object ArgumentValue (int offset)
        {
            if (offset == 0)
                return this.binding0;
            else if (offset == 1)
                return this.binding1;
            else throw new NotImplementedException ("Bad offset");
        }

        public override object Argument0Value
        {
            get
            {
                return this.binding0;
            }
        }

        public override object Argument1Value
        {
            get
            {
                return this.binding1;
            }
        }

        public override bool Assign (out object oldValue, object name, object newValue)
        {
            int offset = this.envClosure.FormalOffset (name);
            if (offset != -1) throw new NotImplementedException ("Assignment in Simple Environment.");
            int soffset = this.envClosure.StaticOffset (name);
            if (soffset != -1) return this.envClosure.SetStaticValue (out oldValue, name, newValue, soffset);
            return envClosure.Environment.Assign (out oldValue, name, newValue);
        }

        public override bool Define (object name, object value)
        {
            throw new NotImplementedException ("No assignment or definition.");
        }

        public override bool DeepSearch (out object value, object name)
        {
            return DeepSearch (out value, name, 0);
        }

        internal override bool DeepSearch (out object value, object name, uint depth)
        {
            switch (this.envClosure.FormalOffset (name)) {
                case -1:
                    return envClosure.Environment.DeepSearch (out value, name, depth + 1);

                case 0:
                    value = this.binding0;
                    break;

                case 1:
                    value = this.binding1;
                    break;

                default:
                    throw new NotImplementedException ();
            }
            return false;
        }

        internal override ValueCell GetValueCell (object name)
        {
            throw new NotImplementedException ("No value cells in simple environment.");
        }

        internal override bool SetValueCell (object name, ValueCell newCell)
        {
            throw new NotImplementedException ("Cannot set value cell in SimpleEnvironment.");
        }

        internal override bool SafeDeepSearch (out object value, object name)
        {
            int offset = this.envClosure.FormalOffset (name);
            if (offset == -1) {
                return envClosure.Environment.SafeDeepSearch (out value, name);
            }
            if (offset == 0)
                value = binding0;
            else if (offset == 1)
                value = binding1;
            else
                throw new NotImplementedException ();
            return false;
        }

        internal override bool DeepSearchType (out object value, object name)
        {
            int offset = this.envClosure.FormalOffset (name);
            if (offset == -1) {
                return envClosure.Environment.DeepSearchType (out value, name);
            }
            throw new NotImplementedException ("deepSearchType in simple environment");
        }

        internal override bool IsUnreferenceable (object name)
        {
            int offset = this.envClosure.FormalOffset (name);
            if (offset == -1) {
                return this.envClosure.Environment.IsUnreferenceable (name);
            }
            return false;
        }

        internal override bool IsUnbound (object name)
        {
            int offset = this.envClosure.FormalOffset (name);
            if (offset == -1) {
                return this.envClosure.Environment.IsUnbound (name);
            }
            return false;
        }

        public override object SystemVectorRef (int index)
        {
            if (index == 0)
                return this.envClosure;
            else if (index == 1)
                return this.binding0;
            else if (index == 2)
                return this.binding0;
            else
                throw new NotImplementedException ("Out of range");
        }

        internal override bool UnbindVariable (out object answer, object name)
        {
            int offset = this.envClosure.FormalOffset (name);
            if (offset == -1) {
                return this.envClosure.Environment.UnbindVariable (out answer, name);
            }
            throw new NotImplementedException ("Cannot unbind in simple environment.");
        }

        internal override bool StaticValue (out object value, object name, int staticOffset)
        {
            return this.Closure.StaticValue (out value, name, staticOffset);
        }

        internal override Environment BaseEnvironment
        {
            get
            {
                return this.envClosure.Environment;
            }
        }

        static int[] codes = new int[64];
        internal override object [] GetValueCells (StaticMapping mapping)
        {
#if DEBUG
            SCode.location = "-";
            valueCellsCopied [mapping.Size] += 1;
            staticMappings.Note (mapping.Offsets);
            SCode.location = "SmallEnvironment2.GetValueCells";
            int code = mapping.OffsetCode;
            if (code >= 0 &&
                code < 64)
                codes[code] += 1;
#endif
            Symbol[] mappingNames = this.Closure.Lambda.Formals;
            object [] oldCells = this.Closure.StaticCells;
            object [] cells;
            switch (mapping.OffsetCode) {

                case 1:
                    cells = new object [1];
                    cells[0] = this.binding0;
                    return cells;

                case 2:
                    cells = new object [1];
                    cells[0] = this.binding1;
                    return cells;

                case 3:
                    cells = new object[2];
                    cells[0] =  this.binding0;
                    cells[1] =  this.binding1;
                    return cells;

               case 23: 
                    cells = new object[4];
                    cells[0] = this.binding0;
                    cells[1] = this.binding1;
                    cells [2] = oldCells [0];
                    cells [3] = oldCells [2];
                //    cells[0] = new ValueCell(mappingNames[1], this.binding1);
                //    cells[1] = this.Closure.StaticCell(0);
                //    cells[2] = this.Closure.StaticCell(1);
                //    cells[3] = this.Closure.StaticCell(2);
                    return cells;

                default:
                    //if (mapping.OffsetCode == 23) Debugger.Break();
                    int count = mapping.Size;
                    cells = new object [count];
                    int[] offsets = mapping.Offsets;
                    for (int index = 0; index < count; index++) {
                        int o = offsets[index];
                        if (o == 0)
                            cells[index] = this.binding0;
                        //cells [index] = this.binding0;
                        else if (o == 1)
                            cells[index] = this.binding1;
                        //cells [index] = this.binding1;
                        else
                            cells [index] = oldCells [o - 2];
                    }
                    return cells;
            }
        }

        public override bool FreeReference (out ValueCell value, object name)
        {
            return envClosure.Environment.FreeReference (out value, name, 1);
        }

        internal override bool FreeReference (out ValueCell value, object name, int depth)
        {
            return envClosure.Environment.FreeReference (out value, name, depth + 1);
        }

    }

    [Serializable]
    sealed class SmallEnvironment3 : LexicalEnvironment<SimpleLambda>
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly object binding0;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly object binding1;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly object binding2;

        internal SmallEnvironment3 (ClosureBase<SimpleLambda> closure, object binding0Value, object binding1Value, object binding2Value)
            : base (closure)
        {
            this.binding0 = binding0Value;
            this.binding1 = binding1Value;
            this.binding2 = binding2Value;
#if DEBUG
            extendedBy [3] += 1;
#endif
        }

        internal override bool AssignArgument (out object oldValue, int offset, object newValue)
        {
            throw new NotImplementedException ();
        }

        internal override bool AssignStatic (out object oldValue, int offset, object newValue)
        {
            return this.envClosure.SetStaticValue (out oldValue, null, newValue, offset);
        }

        public override object ArgumentValue (int offset)
        {
            if (offset == 0)
                return this.binding0;
            else if (offset == 1)
                return this.binding1;
            else if (offset == 2)
                return this.binding2;
            else throw new NotImplementedException ("Bad offset");
        }

        public override object Argument0Value
        {
            get
            {
                return this.binding0;
            }
        }

        public override object Argument1Value
        {
            get
            {
                return this.binding1;
            }
        }

        public override bool Assign (out object oldValue, object name, object newValue)
        {
            int offset = this.envClosure.FormalOffset (name);
            if (offset != -1) throw new NotImplementedException ("Assignment in Simple Environment.");
            int soffset = this.envClosure.StaticOffset (name);
            if (soffset != -1) return this.envClosure.SetStaticValue (out oldValue, name, newValue, soffset);
            return envClosure.Environment.Assign (out oldValue, name, newValue);
        }

        public override bool Define (object name, object value)
        {
            throw new NotImplementedException ("No assignment or definition.");
        }

        public override bool DeepSearch (out object value, object name)
        {
            return DeepSearch (out value, name, 0);
        }

        internal override bool DeepSearch (out object value, object name, uint depth)
        {
            switch (this.envClosure.FormalOffset (name)) {
                case -1:
                    return envClosure.Environment.DeepSearch (out value, name, depth + 1);

                case 0:
                    value = this.binding0;
                    break;

                case 1:
                    value = this.binding1;
                    break;

                case 2:
                    value = this.binding2;
                    break;

                default:
                    throw new NotImplementedException ();
            }
            return false;
        }

        internal override ValueCell GetValueCell (object name)
        {
            throw new NotImplementedException ("No value cells in simple environment.");
        }

        internal override bool SetValueCell (object name, ValueCell newCell)
        {
            throw new NotImplementedException ("Cannot set value cell in SimpleEnvironment.");
        }

        internal override bool SafeDeepSearch (out object value, object name)
        {
            int offset = this.envClosure.FormalOffset (name);
            if (offset == -1) {
                return envClosure.Environment.SafeDeepSearch (out value, name);
            }
            if (offset == 0)
                value = this.binding0;
            else if (offset == 1)
                value = this.binding1;
            else if (offset == 2)
                value = this.binding2;
            else
                throw new NotImplementedException ();
            return false;
        }

        internal override bool DeepSearchType (out object value, object name)
        {
            int offset = this.envClosure.FormalOffset (name);
            if (offset == -1) {
                return envClosure.Environment.DeepSearchType (out value, name);
            }
            throw new NotImplementedException ("deepSearchType in simple environment");
        }

        internal override bool IsUnreferenceable (object name)
        {
            int offset = this.envClosure.FormalOffset (name);
            if (offset == -1) {
                return this.envClosure.Environment.IsUnreferenceable (name);
            }
            return false;
        }

        internal override bool IsUnbound (object name)
        {
            int offset = this.envClosure.FormalOffset (name);
            if (offset == -1) {
                return this.envClosure.Environment.IsUnbound (name);
            }
            return false;
        }

        public override object SystemVectorRef (int index)
        {
            if (index == 0)
                return this.envClosure;
            else if (index == 1)
                return this.binding0;
            else if (index == 2)
                return this.binding1;
            else if (index == 3)
                return this.binding2;
            else
                throw new NotImplementedException ("Out of range");
        }

        internal override bool UnbindVariable (out object answer, object name)
        {
            int offset = this.envClosure.FormalOffset (name);
            if (offset == -1) {
                return this.envClosure.Environment.UnbindVariable (out answer, name);
            }
            throw new NotImplementedException ("Cannot unbind in simple environment.");
        }

        internal override bool StaticValue (out object value, object name, int staticOffset)
        {
            return this.Closure.StaticValue (out value, name, staticOffset);
        }

        internal override Environment BaseEnvironment
        {
            get
            {
                return this.envClosure.Environment;
            }
        }

        static int[] codes = new int[64];
        internal override object [] GetValueCells (StaticMapping mapping)
        {
#if DEBUG
            SCode.location = "SmallEnvironment2.GetValueCells";
            valueCellsCopied [mapping.Size] += 1;
            staticMappings.Note (mapping.Offsets);
            int code = mapping.OffsetCode;
            if (code >= 0 &&
                code < 64)
                codes[code] += 1;
#endif
            Symbol[] mappingNames = this.Closure.Lambda.Formals;
            int count = mapping.Size;
            int [] offsets = mapping.Offsets;
            object [] cells = new object [count];
            object [] oldCells = this.Closure.StaticCells;
            for (int index = 0; index < count; index++) {
                int o = offsets [index];
                if (o == 0)
                    cells [index] =  this.binding0;
                else if (o == 1)
                    cells [index] = this.binding1;
                else if (o == 2)
                    cells [index] =  this.binding2;
                else
                    cells [index] = oldCells [o-3];
            }
            return cells;
        }

        public override bool FreeReference (out ValueCell value, object name)
        {
            return envClosure.Environment.FreeReference (out value, name, 1);
        }

        internal override bool FreeReference (out ValueCell value, object name, int depth)
        {
            return envClosure.Environment.FreeReference (out value, name, depth + 1);
        }

    }

    //These are magic structures that get stuffed in the
    //closureEnvironment register when unwinding the stack.  They are
    //not actually environments, but need to be of that type.
    abstract class FakeEnvironment : Environment
    {
        protected FakeEnvironment ()
            : base ()
        { }

        public override object ArgumentValue (int offset)
        {
            throw new NotImplementedException ();
        }

        internal override bool AssignArgument (out object oldValue, int offset, object newValue)
        {
            throw new NotImplementedException ();
        }

        public override object Argument0Value
        {
            get { throw new NotImplementedException (); }
        }

        public override object Argument1Value
        {
            get { throw new NotImplementedException (); }
        }

        public override bool Assign (out object oldValue, object name, object newValue)
        {
            throw new NotImplementedException ();
        }

        internal override bool AssignStatic (out object oldValue, int offset, object newValue)
        {
            throw new NotImplementedException ();
        }

        public override bool Define (object name, object value)
        {
            throw new NotImplementedException ();
        }

        public override bool DeepSearch (out object value, object name)
        {
            throw new NotImplementedException ();
        }

        internal override bool DeepSearch (out object value, object name, uint depth)
        {
            throw new NotImplementedException ();
        }

        internal override bool DeepSearchType (out object value, object name)
        {
            throw new NotImplementedException ();
        }

        public override bool FreeReference (out ValueCell value, object name)
        {
            throw new NotImplementedException ();
        }

        internal override bool FreeReference (out ValueCell value, object name, int depth)
        {
            throw new NotImplementedException ();
        }

        internal override ValueCell GetValueCell (object name)
        {
            throw new NotImplementedException ();
        }

        internal override Environment BaseEnvironment
        {
            get
            {
                throw new NotImplementedException ();
            }
        }

        internal override bool StaticValue (out object value, object name, int staticOffset)
        {
            throw new NotImplementedException ();
        }

        internal override object [] GetValueCells (StaticMapping mapping)
        {
            throw new NotImplementedException ();
        }

        internal override bool SafeDeepSearch (out object value, object name)
        {
            throw new NotImplementedException ();
        }

        internal override bool SetValueCell (object name, ValueCell newCell)
        {
            throw new NotImplementedException ();
        }

        internal override bool IsUnbound (object name)
        {
            throw new NotImplementedException ();
        }

        internal override bool IsUnreferenceable (object name)
        {
            throw new NotImplementedException ();
        }

        internal override bool UnbindVariable (out object answer, object name)
        {
            throw new NotImplementedException ();
        }
    }

    class WithinControlPoint : FakeEnvironment
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        ControlPoint controlPoint;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        ClosureBase<StandardLambda> thunk;

        public WithinControlPoint (ControlPoint controlPoint, ClosureBase<StandardLambda> thunk)
        {
            this.controlPoint = controlPoint;
            this.thunk = thunk;
        }
    }

    // not really an closureEnvironment at all, but rather
    // a structure that holds the reified stack as we
    // unwind it.
    class UnwinderState : FakeEnvironment
    {
        bool isExit;
        object exitValue;

        public bool IsExit { get { return this.isExit; } }
        public object ExitValue { get { return this.exitValue; } set { this.exitValue = value; this.isExit = true; } }

        /// <summary>
        /// When we get back to the outermost loop, we either want
        /// to reload the continuation we just saved (if we were
        /// performing a call-with-current-continuation) or we want
        /// to blow away the continuation and use a new one (if we
        /// were performing a within-continuation).  If this field
        /// is empty, we use the saved continuation, otherwise we
        /// use this one.
        /// </summary>
        ControlPoint newContinuation;

        /// <summary>
        /// A list of frames that have not yet been fully assembled
        /// into the continuation.  Each frame has saved its own state.
        /// </summary>
        ContinuationFrameList newFrames;

        /// <summary>
        /// A list of frames that already have been assembled into the continuation.
        /// When unloading the stack, we need not unload these frames.
        /// </summary>
        ContinuationFrameList oldFrames;

        Control receiver;

        /// <summary>
        /// Constructor that takes a new continuation to be installed.
        /// Stack is destroyed as we unwind and we reload the new one.
        /// </summary>
        /// <param ratorName="newContinuation"></param>
        /// <param ratorName="receiver"></param>
        internal UnwinderState (ControlPoint newContinuation, Control receiver)
        {
            this.newContinuation = newContinuation;
            this.receiver = receiver;
        }

        /// <summary>
        /// Constructor that does not take a new continuation to be
        /// installed.  Stack is reloaded with the continuation that
        /// has been read.
        /// </summary>
        /// <param ratorName="receiver"></param>
        internal UnwinderState (Control receiver)
        {
            this.receiver = receiver;
        }

        internal ControlPoint NewContinuation
        {
            get
            {
                return this.newContinuation;
            }
        }

        internal Control Receiver
        {
            get
            {
                return this.receiver;
            }
        }

        /// <summary>
        /// Push a newly created heap frame onto the list of frames that need
        /// to be assembled into the continuation.  This should be done in the
        /// handler that protects the initial subroutine call.
        /// </summary>
        internal void AddFrame (ContinuationFrame extension)
        {
            // If we are moving to a new continuation, we don't need
            // the old one.  Just discard it.
            if (this.newContinuation == null) {
                this.newFrames = new ContinuationFrameList (extension, this.newFrames);
            }
        }

        /// <summary>
        /// Append the tail of the current continuation to the exception
        /// object so that the handler can assemble the new frames onto it.
        /// </summary>
        internal void AppendContinuationFrames (ContinuationFrameList oldFrames)
        {
            // if we are moving to a new continuation, discard the
            // old frames.
            if (this.newContinuation == null) {
                // only grab them if we don't know what they are
                // this makes it safe to put this in the reload
                // path without protective logic.
                if (this.oldFrames == null)
                    this.oldFrames = oldFrames;
            }
        }

        /// <summary>
        /// Create or return a control point to be loaded.
        /// </summary>
        /// <returns></returns>
        public ControlPoint ToControlPoint ()
        {
            return this.newContinuation == null
                ? new ControlPoint (newFrames, oldFrames)
                : this.newContinuation;
        }
    }

    // Not an closureEnvironment, but the state needed to restore the
    // continuation.
    class RewindState : FakeEnvironment
    {
        ContinuationFrameList reversedFrames;
        Control receiver;
        ControlPoint cp;

        internal RewindState (ControlPoint cp, Control receiver)
        {
            this.cp = cp;
            this.receiver = receiver;
            this.reversedFrames = ContinuationFrameList.reverse (cp.FrameList);
        }

        internal ControlPoint ControlPoint
        {
            get
            {
                return this.cp;
            }
        }

        internal ContinuationFrameList GetFrameList ()
        {
            return this.reversedFrames;
        }

        internal Control PopFrame ()
        {
            if (reversedFrames == null) {
                return this.receiver;
            }
            else {
                ContinuationFrame frame = reversedFrames.first;
                reversedFrames = reversedFrames.rest;
                return frame;
            }
        }
    }
}


using System;
using System.Collections.Generic;
using System.Diagnostics;

namespace Microcode
{
    [Serializable]
    public abstract class Environment : SchemeObject, ISystemVector
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public static GlobalEnvironment systemGlobalEnvironment;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        internal readonly ClosureBase closure;

        internal Environment (ClosureBase closure)
            : base (TC.ENVIRONMENT)
        {
            this.closure = closure;
        }

        // Gets the closure that created this environment
        internal ClosureBase Closure { [DebuggerStepThrough] get { return this.closure; } }

        public static GlobalEnvironment Global
        {
            [DebuggerStepThrough]
            get
            {
                if (systemGlobalEnvironment == null)
                    systemGlobalEnvironment = new GlobalEnvironment ();
                return systemGlobalEnvironment;
            }

            [DebuggerStepThrough]
            set
            {
                systemGlobalEnvironment = value;
            }
        }

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
                if (env is bool && (bool) env == false)
                    return Environment.Global;
                else
                    throw new NotImplementedException ();
            }
            else
                return eenv;
        }

#if DEBUG
        [NonSerialized]
        static protected long [] foundAtDepth = new long [128];
        [NonSerialized]
        static protected long [] foundInAuxes = new long [128];
        [NonSerialized]
        static protected long [] foundInGlobal = new long [128];
        [NonSerialized]
        static protected long [] extendedBy = new long [1024];
#endif

        // Abstract functions on environments

        // Grab the variable at offset in frame.
        public abstract object ArgumentValue (int offset);
        public abstract object Argument0Value { get; }
        public abstract object Argument1Value { get; }

        // Deep search for variable location and smash the value.
        public abstract bool Assign (out object oldValue, object name, object newValue);


        // Define a variable in the topmost frame.  This will shadow
        // other definitions.  Returns false on success, true if there
        // is a problem.
        public abstract bool Define (object name, object value);

        // Deep search the lexical environment for the variable.
        // returns false on success, true if there is a problem.
        public abstract bool DeepSearch (out object value, object name);
        // internal call tracks depth.
        internal abstract bool DeepSearch (out object value, object name, int depth);
        internal abstract bool DeepSearchType (out object value, object name);

        // Used to link variables.
        internal abstract ValueCell GetValueCell (object name);

        internal abstract bool FreeRef (out object value, object name);
        internal abstract bool FreeRef (out object value, object name, int depth);
        internal abstract bool DangerousFreeRef (out object value, object name, int shadowDepth, int depth);
        internal abstract bool DangerousLexicalRef (out object value, object name, int shadowDepth, int depth, int offset);
        internal abstract bool LexicalRef (out object value, object name, int depth, int offset);
        // Fast lexical ref does not check the incrementals.
        internal abstract bool FastLexicalRef (out object value, object name, int depth, int offset);
        internal abstract bool FastLexicalRef1 (out object value, object name, int offset);
        // walk up the chain.  Zero steps means stay here.
        internal abstract Environment GetAncestorEnvironment (int depth);

        internal abstract bool SafeDeepSearch (out object value, object name);
        // Used to link variables.
        internal abstract bool SetValueCell (object name, ValueCell newCell);
        //internal abstract BoundVariable SimulateDangerousLookup (object name, int safeDepth, int depth);
        //internal abstract BoundVariable SimulateLookup (object name);
        //internal abstract BoundVariable SimulateStaticLookup (object name, int depth);
        internal abstract bool IsUnbound (object name);
        internal abstract bool IsUnreferenceable (object name);

        [SchemePrimitive ("LEXICAL-UNBOUND?", 2, false)]
        public static bool IsLexicalUnbound (out object answer, object env, object name)
        {
            answer = ToEnvironment (env).IsUnbound ((string) name);
            return false; // copacetic
        }

        [SchemePrimitive ("LEXICAL-UNREFERENCEABLE?", 2, false)]
        public static bool IsLexicalUnreferenceable (out object answer, object env, object name)
        {
            answer = ToEnvironment (env).IsUnreferenceable ((string) name);
            return false; // copacetic
        }

        // Same as assigning in the interpreter.
        [SchemePrimitive ("LEXICAL-ASSIGNMENT", 3, false)]
        public static bool LexicalAssignment (out object answer, object aenv, object aname, object value)
        {
            Environment env = ToEnvironment (aenv);
            object name = (string) aname;
            object oldValue;
            if (env.Assign (out oldValue, name, value))
                throw new NotImplementedException ("error during assignment");
            answer = oldValue;
            return false;
        }

        // Same as evaluating variable.
        [SchemePrimitive ("LEXICAL-REFERENCE", 2, false)]
        public static bool LexicalReference (out object answer, object env, object name)
        {
            if (ToEnvironment (env).DeepSearch (out answer, (string) name))
                throw new NotImplementedException ("Error during lexical-reference.");
            return false;
        }

        // Looks up variable, tells whether it is a macro, unassigned, etc.
        [SchemePrimitive ("LEXICAL-REFERENCE-TYPE", 2, false)]
        public static bool LexicalReferenceType (out object answer, object env, object name)
        {
            if (ToEnvironment (env).DeepSearchType (out answer, (string) name))
                throw new NotImplementedException ("Error during lexical-reference-type.");
            return false;
        }


        [SchemePrimitive ("SAFE-LEXICAL-REFERENCE", 2, false)]
        public static bool SafeLexicalReference (out object answer, object env, object name)
        {
            if (ToEnvironment (env).SafeDeepSearch (out answer, (string) name))
                throw new NotImplementedException ("Error during lexical-reference.");
            return false;
        }

        [SchemePrimitive ("LINK-VARIABLES", 4, false)]
        public static bool LinkVariables (out object answer, object [] arglist)
        {
            Environment target_env = ToEnvironment (arglist [0]);

            string target_name = (string) arglist [1];
            Environment source_env = ToEnvironment (arglist [2]);
            string source_name = (string) arglist [3];
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
            if (ToEnvironment (env).Define ((string) aname, value)) throw new NotImplementedException ();

            answer = aname;
            return false;
        }

        [SchemePrimitive ("ENVIRONMENT?", 1, true)]
        public static bool IsEnvironment (out object answer, object arg)
        {
            answer = arg is Environment || (arg is bool && (bool) arg == false);
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

    // There are four different kinds of environment.  
    // The null environment doesn't respond to anything yet.
    // A Root environment has no parent and no frame.  It is
    // used for the systemGlobalEnvironment environment.
    // A `Top Level' environment has a frame and incrementals.
    // it can be extended, searched, etc.  A full-feature environment.

    [Serializable]
    class NullEnvironment : Environment
    {
        public NullEnvironment (ClosureBase closure)
            : base (closure)
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

        public override bool Define (object name, object value)
        {
            throw new NotImplementedException ();
        }

        public override bool DeepSearch (out object value, object name)
        {
            throw new NotImplementedException ();
        }

        internal override bool DeepSearch (out object value, object name, int depth)
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

        internal override bool FastLexicalRef (out object value, object name, int depth, int offset)
        {
            throw new NotImplementedException ();
        }

        internal override Environment GetAncestorEnvironment (int depth)
        {
            if (depth == 0) return this;
            throw new NotImplementedException ();
        }

        //internal override bool LexicalRef (out object value, object varname, int depth, int offset)
        //{
        //    throw new NotImplementedException ();
        //}

        internal override bool DangerousLexicalRef (out object value, object name, int shadowDepth, int depth, int offset)
        {
            throw new NotImplementedException ();
        }

        internal override bool LexicalRef (out object value, object name, int depth, int offset)
        {
            throw new NotImplementedException ();
        }

        internal override bool FreeRef (out object value, object name)
        {
            throw new NotImplementedException ();
        }

        internal override bool DangerousFreeRef (out object value, object name, int shadowDepth, int depth)
        {
            throw new NotImplementedException ();
        }

        internal override bool FreeRef (out object value, object name, int depth)
        {
            throw new NotImplementedException ();
        }

        //internal override BoundVariable SimulateLookup (object name)
        //{
        //    throw new NotImplementedException ();
        //}

        //internal override BoundVariable SimulateStaticLookup (object name, int depth)
        //{
        //    throw new NotImplementedException ();
        //}

        //internal override BoundVariable SimulateDangerousLookup (object name, int safeDepth, int depth)
        //{
        //    throw new NotImplementedException ();
        //}

        internal override bool FastLexicalRef1 (out object value, object name, int offset)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    public sealed class GlobalEnvironment : Environment
    {
        // Object -> value cell so we have EQ semantics
        // rather than string= semantics.
        readonly Dictionary<object, ValueCell> globalBindings = new Dictionary<object, ValueCell> ();

        public GlobalEnvironment ()
            : base (null)
        {
        }

        public override object ArgumentValue (int offset)
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

        internal override bool DeepSearch (out object value, object name, int depth)
        {
            ValueCell cell = null;
            if (this.globalBindings.TryGetValue (name, out cell)) {
#if DEBUG
                foundInGlobal [depth] += 1;
#endif
                if (cell.GetValue (out value))
                    throw new NotImplementedException ("Error getting value from cell");
                return false;
            }
            throw new NotImplementedException ("Variable not bound: " + name);
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
                throw new NotImplementedException ("Value cell already exists.");
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

        internal override bool FastLexicalRef (out object value, object name, int depth, int offset)
        {
            throw new NotImplementedException ();
        }

        internal override Environment GetAncestorEnvironment (int depth)
        {
            if (depth == 0) return this;
            else throw new NotImplementedException ("Global environments have no ancestor.");
        }

        //internal override bool LexicalRef (out object value, object varname, int depth, int offset)
        //{
        //    throw new NotImplementedException ();
        //}

        internal override bool DangerousLexicalRef (out object value, object name, int shadowDepth, int depth, int offset)
        {
            throw new NotImplementedException ();
        }

        internal override bool LexicalRef (out object value, object name, int depth, int offset)
        {
            throw new NotImplementedException ();
        }

        internal override bool FreeRef (out object value, object name)
        {
            throw new NotImplementedException ();
        }

        internal override bool DangerousFreeRef (out object value, object name, int shadowDepth, int depth)
        {
            throw new NotImplementedException ();
        }

        internal override bool FreeRef (out object value, object name, int depth)
        {
            ValueCell cell = null;
            if (this.globalBindings.TryGetValue (name, out cell)) {
                if (cell.GetValue (out value))
                    throw new NotImplementedException ("Error getting value from cell");
                return false;
            }
            throw new NotImplementedException ("Variable not bound: " + name); 
        }

        //internal override BoundVariable SimulateLookup (object name)
        //{
        //    return GlobalVariable.Make (name, this);
        //}

        //internal override BoundVariable SimulateStaticLookup (object name, int depth)
        //{
        //    // The variable is known not to be shadowed in the lexical
        //    // environment, so we can treat it as a global.
        //    return GlobalVariable.Make (name, this);
        //}

        //internal override BoundVariable SimulateDangerousLookup (object name, int safeDepth, int depth)
        //{
        //    return DangerousGlobalVariable.Make (name, this, safeDepth, depth);
        //}

        internal override bool FastLexicalRef1 (out object value, object name, int offset)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    abstract class LexicalEnvironment : Environment
    {
#if DEBUG
        static long lexicalCacheHits;
#endif
        long transitCount;
        Environment [] lexicalCache;

        protected LexicalEnvironment (ClosureBase closure)
            : base (closure)
        { }

        internal override bool FastLexicalRef (out object value, object name, int depth, int offset)
        {
            if (depth == 1) {
                value = this.closure.Environment.ArgumentValue (offset);
            }
            else {
                Environment env = GetAncestorEnvironment (depth);
                value = env.ArgumentValue (offset);
            }
            return false;
        }


        internal override Environment GetAncestorEnvironment (int depth)
        {
            if (depth == 0) return this;
            else if (depth == 1) return this.closure.Environment;
                // 5  858
                // 7  889
                // 8 
                // 9  869
                // 10 875
                // 11 876
                // 12 871
            else {
                if (this.lexicalCache == null) {
                    if (transitCount++ < 5)
                        return this.Closure.Environment.GetAncestorEnvironment (depth - 1);
                    this.lexicalCache = new Environment [depth + 1];
                }
                else if (this.lexicalCache.Length <= depth) {
                    Environment [] newCache = new Environment [depth + 1];
                    Array.Copy (this.lexicalCache, newCache, this.lexicalCache.Length);
                    this.lexicalCache = newCache;
                }

                Environment env = this.lexicalCache [depth];
                if (env == null) {
#if DEBUG
                    lexicalCacheHits -= 1;
#endif
                    env = this.Closure.Environment.GetAncestorEnvironment (depth-1);
                    this.lexicalCache [depth] = env;
                }
#if DEBUG
                lexicalCacheHits += 1;
#endif
                return env;
            }
        }


        public override object ArgumentValue (int offset)
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

        public override bool Define (object name, object value)
        {
            throw new NotImplementedException ();
        }

        public override bool DeepSearch (out object value, object name)
        {
            throw new NotImplementedException ();
        }

        internal override bool DeepSearch (out object value, object name, int depth)
        {
            throw new NotImplementedException ();
        }

        internal override bool DeepSearchType (out object value, object name)
        {
            throw new NotImplementedException ();
        }

        internal override ValueCell GetValueCell (object name)
        {
            throw new NotImplementedException ();
        }

        internal override bool FreeRef (out object value, object name)
        {
            throw new NotImplementedException ();
        }

        internal override bool FreeRef (out object value, object name, int depth)
        {
            throw new NotImplementedException ();
        }

        internal override bool DangerousFreeRef (out object value, object name, int shadowDepth, int depth)
        {
            throw new NotImplementedException ();
        }

        internal override bool DangerousLexicalRef (out object value, object name, int shadowDepth, int depth, int offset)
        {
            throw new NotImplementedException ();
        }

        internal override bool LexicalRef (out object value, object name, int depth, int offset)
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
    }


    /// <summary>
    /// A StandardEnvironment supports sharable bindings and incremental
    /// definition.
    /// </summary>
    [Serializable]
    sealed class StandardEnvironment : LexicalEnvironment
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly ValueCell [] bindings;
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        Dictionary <object,ValueCell> incrementals;

        internal StandardEnvironment (ClosureBase closure)
            : base (closure)
        {
#if DEBUG
            extendedBy [0] += 1;
            // sanity check.
            object [] formals = closure.Lambda.Formals;
            if (formals.Length != 0)
                throw new NotImplementedException ();
#endif
        }

        internal StandardEnvironment (ClosureBase closure, object [] initialValues)
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
            int offset = this.closure.FormalOffset (name);
            if (offset == -1) {
                ValueCell vcell;
                if (this.incrementals != null
                    && this.incrementals.TryGetValue (name, out vcell))
                    return vcell.Assign (out oldValue, newValue);
                else
                    return closure.Environment.Assign (out oldValue, name, newValue);
            }
            return bindings [offset].Assign (out oldValue, newValue);
        }

        public override bool Define (object name, object value)
        {
            int offset = this.closure.FormalOffset (name);
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

        internal override bool DeepSearch (out object value, object name, int depth)
        {
            int offset = this.closure.FormalOffset (name);
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
                    return closure.Environment.DeepSearch (out value, name, depth + 1);
            }
#if DEBUG
            foundAtDepth [depth] += 1;
#endif
            return bindings [offset].GetValue (out value);
        }

        internal override bool FastLexicalRef (out object value, object name, int depth, int offset)
        {
            throw new NotImplementedException ();
        //    if (depth == 0) return bindings [offset].GetValue (out value);
        //    ValueCell vcell;
        //    if (this.incrementals != null
        //        && this.incrementals.TryGetValue (varname, out vcell)) {
        //        return vcell.GetValue (out value);
        //    }

        //    return closure.Environment.FastLexicalRef (out value, varname, depth - 1, offset);
        }

        internal override ValueCell GetValueCell (object name)
        {
            int offset = this.closure.FormalOffset (name);
            if (offset == -1) {
                ValueCell vcell;
                if (this.incrementals != null
                    && this.incrementals.TryGetValue (name, out vcell)) {
                    return vcell;
                }
                else
                    return closure.Environment.GetValueCell (name);
            }
            return bindings [offset];
        }

        internal override bool SetValueCell (object name, ValueCell newCell)
        {
            int offset = this.closure.FormalOffset (name);
            if (offset == -1) {
                ValueCell vcell;
                if (incrementals != null
                    && incrementals.TryGetValue (name, out vcell)) {
                    throw new NotImplementedException ("Existing value cell in incrementals.");
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
            int offset = this.closure.FormalOffset (name);
            if (offset == -1) {
                ValueCell vcell;
                if (this.incrementals != null
                    && this.incrementals.TryGetValue (name, out vcell)) {
                    return vcell.SafeGetValue (out value);
                }
                else
                    return closure.Environment.SafeDeepSearch (out value, name);
            }
            return bindings [offset].SafeGetValue (out value);
        }

        internal override bool DeepSearchType (out object value, object name)
        {
            int offset = this.closure.FormalOffset (name);
            if (offset == -1) {
                ValueCell vcell;
                if (this.incrementals != null
                    && this.incrementals.TryGetValue (name, out vcell)) {
                    return vcell.GetType (out value);
                }
                else
                    return closure.Environment.DeepSearchType (out value, name);
            }
            return bindings [offset].GetType (out value);
        }

        internal override bool IsUnreferenceable (object name)
        {
            int offset = this.closure.FormalOffset (name);
            if (offset == -1) {
                ValueCell vcell;
                if (incrementals != null
                    && incrementals.TryGetValue (name, out vcell))
                    return vcell.Unreferenceable ();
                else
                    return this.closure.Environment.IsUnreferenceable (name);
            }
            return bindings [offset].Unreferenceable ();
        }


        internal override bool IsUnbound (object name)
        {
            int offset = this.closure.FormalOffset (name);
            if (offset == -1) {
                ValueCell vcell;
                if (incrementals != null
                    && incrementals.TryGetValue (name, out vcell))
                    return vcell.Unbound ();
                else
                    return this.closure.Environment.IsUnbound (name);
            }
            return bindings [offset].Unbound ();
        }

        internal override bool LexicalRef (out object value, object name, int depth, int offset)
        {
            if (depth == 0) return bindings [offset].GetValue (out value);
            ValueCell vcell;
            if (this.incrementals != null
                && this.incrementals.TryGetValue (name, out vcell)) {
                return vcell.GetValue (out value);
            }

            return closure.Environment.LexicalRef (out value, name, depth - 1, offset);
        }

        internal override bool DangerousLexicalRef (out object value, object name, int shadowDepth, int depth, int offset)
        {
            return GetAncestorEnvironment (shadowDepth).LexicalRef (out value, name, depth +1 , offset);
        }

        internal override bool FreeRef (out object value, object name)
        {
            // we know it isn't lexical
            ValueCell vcell;
            if (this.incrementals != null
                && this.incrementals.TryGetValue (name, out vcell)) {
                return vcell.GetValue (out value);
            }
            return closure.Environment.DeepSearch (out value, name);
        }

        internal override bool DangerousFreeRef (out object value, object name, int shadowDepth, int depth)
        {
            return GetAncestorEnvironment (shadowDepth).FreeRef (out value, name, depth - shadowDepth);
        }

        internal override bool FreeRef (out object value, object name, int depth)
        {
            // We know it isn't in our argument vector, but we could be
            // shadowing it.
            // we know it isn't lexical
            ValueCell vcell;
            if (this.incrementals != null
                && this.incrementals.TryGetValue (name, out vcell)) {
                return vcell.GetValue (out value);
            }
            if (depth == 1)
                return closure.Environment.DeepSearch (out value, name);
            else
                return closure.Environment.FreeRef (out value, name, depth - 1);
        }

        public override object SystemVectorRef (int index)
        {
            if (index == 0)
                return this.Closure;
            else {
                object answer;
                if (this.bindings [index - 1].GetValue (out answer))
                    throw new NotImplementedException ();
                return answer;
            }
        }

        //internal override BoundVariable SimulateLookup (object name)
        //{
        //    int offset = this.closure.FormalOffset (name);
        //    if (offset == -1) {
        //        if (this.incrementals == null)
        //            return FreeVariable.Make (name, this);
        //        else {
        //            ValueCell vcell;
        //            if (this.incrementals.TryGetValue (name, out vcell))
        //                return TopLevelVariable.Make (name, vcell);
        //            else
        //                return FreeVariable.Make (name, this);
        //        }
        //    }
        //    return TopLevelVariable.Make (name, this.bindings [offset]);
            
        //}

        //internal override BoundVariable SimulateStaticLookup (object name, int depth)
        //{
        //    int offset = this.closure.FormalOffset (name);
        //    if (offset == -1) {
        //        if (this.incrementals == null)
        //            return FreeVariable.Make (name, this);
        //        else {
        //            ValueCell vcell;
        //            if (this.incrementals.TryGetValue (name, out vcell))
        //                return TopLevelVariable.Make (name, vcell);
        //            else
        //                return FreeVariable.Make (name, this);
        //        }
        //    }
        //    return TopLevelVariable.Make (name, this.bindings [offset]); 
        //}

        //internal override BoundVariable SimulateDangerousLookup (object name, int safeDepth, int depth)
        //{
        //    int offset = this.closure.FormalOffset (name);
        //    if (offset == -1) {
        //        if (this.incrementals == null)
        //            return DangerousFreeVariable.Make (name, this, safeDepth);
        //        else {
        //            ValueCell vcell;
        //            if (this.incrementals.TryGetValue (name, out vcell))
        //                return DangerousTopLevelVariable.Make (name, vcell, safeDepth);
        //            else
        //                return DangerousFreeVariable.Make (name, this, safeDepth);
        //        }
        //    }
        //    return DangerousTopLevelVariable.Make (name, this.bindings [offset], safeDepth); 
  
        //}

        internal override bool FastLexicalRef1 (out object value, object name, int offset)
        {
            throw new NotImplementedException ();
        }
    }

    /// <summary>
    /// A StaticEnvironment prohibits sharable bindings and incremental
    /// definition, but allows assignment and redefinition.
    /// </summary>
    [Serializable]
    sealed class StaticEnvironment : LexicalEnvironment
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly object [] bindings;

        internal StaticEnvironment (ClosureBase closure)
            : base (closure)
        {
#if DEBUG
            extendedBy [0] += 1;
            // sanity check.
            object [] formals = closure.Lambda.Formals;
            if (formals.Length != 0)
                throw new NotImplementedException ();
#endif
        }

        internal StaticEnvironment (ClosureBase closure, object [] initialValues)
            : base (closure)
        {
            this.bindings = initialValues;
#if DEBUG
            extendedBy [initialValues.Length] += 1;
            // sanity check
            if (closure.Lambda.Formals.Length != initialValues.Length)
                throw new NotImplementedException ();
#endif
        }

        public override object ArgumentValue (int offset)
        {
            object answer = bindings [offset];
            if (answer is ReferenceTrap) throw new NotImplementedException ();
            return answer;
        }

        public override object Argument0Value
        {
            get
            {
                object answer = bindings [0];
                if (answer is ReferenceTrap) throw new NotImplementedException ();
                return answer;
            }
        }

        public override object Argument1Value
        {
            get
            {
                object answer = bindings [1];
                if (answer is ReferenceTrap) throw new NotImplementedException ();
                return answer;
            }
        }

        public override bool Assign (out object oldValue, object varname, object newValue)
        {
            int offset = this.closure.FormalOffset (varname);
            if (offset == -1)
                return closure.Environment.Assign (out oldValue, varname, newValue);
            object temp = bindings [offset];
            bindings [offset] = (newValue == Constant.ExternalUnassigned)
                ? ReferenceTrap.Unassigned
                : newValue;
            oldValue = (temp == ReferenceTrap.Unassigned) ? Constant.ExternalUnassigned : temp;
            return false;
        }

        public override bool Define (object varname, object value)
        {
            int offset = this.closure.FormalOffset (varname);
            if (offset == -1)
                throw new NotImplementedException ("Incremental define not supported in static environment.");
            object temp = bindings [offset];
            bindings [offset] = (value == Constant.ExternalUnassigned)
                ? ReferenceTrap.Unassigned
                : value;
            return false;
        }

        public override bool DeepSearch (out object value, object varname)
        {
            return DeepSearch (out value, varname, 0);
        }

        internal override bool DeepSearch (out object value, object varname, int depth)
        {
            if (Configuration.EnableLexicalAddressing)
                throw new NotImplementedException ("Deep search should not be necessary");
            else {
                int offset = this.closure.FormalOffset (varname);
                if (offset == -1)
                    return this.closure.Environment.DeepSearch (out value, varname, depth + 1);
                else {
                    value = bindings [offset];
                    return false;
                }
            }
        }

        internal override ValueCell GetValueCell (object varname)
        {
            throw new NotImplementedException ();
        }

        internal override bool SetValueCell (object varname, ValueCell newCell)
        {
            throw new NotImplementedException ();
        }

        internal override bool SafeDeepSearch (out object value, object varname)
        {
            throw new NotImplementedException ();
        }

        internal override bool DeepSearchType (out object value, object varname)
        {
            throw new NotImplementedException ();
        }

        internal override bool IsUnreferenceable (object varname)
        {
            throw new NotImplementedException ();
        }

        internal override bool IsUnbound (object varname)
        {
            throw new NotImplementedException ();
        }

        internal override bool DangerousLexicalRef (out object value, object varname, int shadowDepth, int depth, int offset)
        {
            return GetAncestorEnvironment (shadowDepth).LexicalRef (out value, varname, depth + 1, offset);
        }

        internal override bool LexicalRef (out object value, object varname, int depth, int offset)
        {
            if (depth == 0) {
                value = bindings [offset];
                return false;
            }

            return this.closure.Environment.LexicalRef (out value, varname, depth - 1, offset);
        }
        
        internal override bool FastLexicalRef1 (out object value, object name, int offset)
        {
            value = this.closure.Environment.ArgumentValue (offset);
            return false;
        }

        internal override bool FreeRef (out object value, object varname)
        {
            return this.closure.Environment.DeepSearch (out value, varname);
        }

        internal override bool DangerousFreeRef (out object value, object varname, int shadowDepth, int depth)
        {
            return GetAncestorEnvironment (shadowDepth).FreeRef (out value, varname, depth - shadowDepth);
        }

        internal override bool FreeRef (out object value, object varname, int depth)
        {
            if (depth == 1)
                return this.Closure.Environment.DeepSearch (out value, varname);
            else
                return this.Closure.Environment.FreeRef (out value, varname, depth - 1);
        }

        //internal override BoundVariable SimulateLookup (object name)
        //{
        //    throw new NotImplementedException ();
        //}

        //internal override BoundVariable SimulateStaticLookup (object name, int depth)
        //{
        //    throw new NotImplementedException ();
        //}

        //internal override BoundVariable SimulateDangerousLookup (object name, int safeDepth, int depth)
        //{
        //    throw new NotImplementedException ();
        //}

    }

    /// <summary>
    /// A SimpleEnvironment prohibits sharable bindings and incremental
    /// definition, assignment, and redefinition.
    /// </summary>

    [Serializable]
    sealed class SimpleEnvironment : LexicalEnvironment
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly object [] bindings;


        internal SimpleEnvironment (ClosureBase closure)
            : base (closure)
        {
#if DEBUG
            extendedBy [0] += 1;
            // sanity check.
            object [] formals = closure.Lambda.Formals;
            if (formals.Length != 0)
                throw new NotImplementedException ();
#endif
        }

        internal SimpleEnvironment (ClosureBase closure, object [] initialValues)
            : base (closure)
        {
            this.bindings = initialValues;
#if DEBUG
            extendedBy [initialValues.Length] += 1;
            // sanity check
            if (closure.Lambda.Formals.Length != initialValues.Length)
                throw new NotImplementedException ();
#endif
        }

        public override object ArgumentValue (int offset)
        {
            return this.bindings [offset];
        }

        public override object Argument0Value
        {
            get
            {
                return this.bindings [0];
            }
        }

        public override object Argument1Value
        {
            get
            {
                return this.bindings [1];
            }
        }

        public override bool Assign (out object oldValue, object varname, object newValue)
        {
            int offset = this.closure.FormalOffset (varname);
            if (offset == -1)
                return closure.Environment.Assign (out oldValue, varname, newValue);
            throw new NotImplementedException ();
        }

        public override bool Define (object varname, object value)
        {
            throw new NotImplementedException ();
        }

        public override bool DeepSearch (out object value, object varname)
        {
            return DeepSearch (out value, varname, 0);
        }

        internal override bool DeepSearch (out object value, object varname, int depth)
        {
            int offset = this.closure.FormalOffset (varname);
            if (offset == -1)
                return this.closure.Environment.DeepSearch (out value, varname, depth + 1);
            value = this.bindings [offset];
            return value is ReferenceTrap;
        }

        internal override ValueCell GetValueCell (object varname)
        {
            throw new NotImplementedException ();
        }

        internal override bool SetValueCell (object varname, ValueCell newCell)
        {
            throw new NotImplementedException ();
        }

        internal override bool SafeDeepSearch (out object value, object varname)
        {
            throw new NotImplementedException ();
        }

        internal override bool DeepSearchType (out object value, object varname)
        {
            throw new NotImplementedException ();
        }

        internal override bool IsUnreferenceable (object varname)
        {
            throw new NotImplementedException ();
        }

        internal override bool IsUnbound (object varname)
        {
            throw new NotImplementedException ();
        }

        internal override bool DangerousLexicalRef (out object value, object varname, int shadowDepth, int depth, int offset)
        {
            throw new NotImplementedException ();
        }

        internal override bool LexicalRef (out object value, object varname, int depth, int offset)
        {
            if (depth == 0) {
                value = this.bindings [offset];
                return value is ReferenceTrap;
            }
            else
                return this.closure.Environment.LexicalRef (out value, varname, depth - 1, offset);
        }

        internal override bool FastLexicalRef1 (out object value, object name, int offset)
        {
            value = this.closure.Environment.ArgumentValue (offset);
            return false;
        }

        internal override bool FreeRef (out object value, object varname)
        {
            throw new NotImplementedException ();
        }

        internal override bool DangerousFreeRef (out object value, object varname, int shadowDepth, int depth)
        {
            return GetAncestorEnvironment (shadowDepth).FreeRef (out value, varname, depth - shadowDepth);
        }

        internal override bool FreeRef (out object value, object varname, int depth)
        {
            if (depth == 1)
                return this.closure.Environment.DeepSearch (out value, varname);
            else
                return this.closure.Environment.FreeRef (out value, varname, depth - 1);
        }

        //internal override BoundVariable SimulateLookup (object name)
        //{
        //    throw new NotImplementedException ();
        //}

        //internal override BoundVariable SimulateStaticLookup (object name, int depth)
        //{
        //    throw new NotImplementedException ();
        //}

        //internal override BoundVariable SimulateDangerousLookup (object name, int safeDepth, int depth)
        //{
        //    throw new NotImplementedException ();
        //}

    }

    /// <summary>
    /// A SmallEnvironment needs no value cells because
    /// the bindings are never mutated.
    /// </summary>
    [Serializable]
    sealed class SmallEnvironment1 : LexicalEnvironment
    {
        readonly object value0;

        internal SmallEnvironment1 (ClosureBase closure, object value0)
            : base (closure)
        {
#if DEBUG
            if (value0 is ReferenceTrap) throw new NotImplementedException ();
#endif
            this.value0 = value0;
        }

        public override object ArgumentValue (int offset)
        {
            if (offset == 0) return this.value0;
            else throw new NotImplementedException ();
        }

        public override object Argument0Value
        {
            get
            {
                return this.value0;
            }
        }

        public override object Argument1Value
        {
            get
            {
                throw new NotImplementedException ();
            }
        }

        public override bool Assign (out object oldValue, object varname, object newValue)
        {
            int offset = this.closure.FormalOffset (varname);
            if (offset == -1) {
                return closure.Environment.Assign (out oldValue, varname, newValue);
            }
            throw new NotImplementedException ();
        }

        public override bool Define (object varname, object value)
        {
            throw new NotImplementedException ();
        }

        public override bool DeepSearch (out object value, object varname)
        {
            return DeepSearch (out value, varname, 0);
        }

        internal override bool DeepSearch (out object value, object varname, int depth)
        {
            int offset = this.closure.FormalOffset (varname);
            if (offset == -1) {
                return closure.Environment.DeepSearch (out value, varname, depth + 1);
            }
#if DEBUG
            foundAtDepth [depth] += 1;
#endif
            value = value0;
            return false;
        }

        internal override bool DeepSearchType (out object value, object varname)
        {
            throw new NotImplementedException ();
        }

        internal override ValueCell GetValueCell (object varname)
        {
            throw new NotImplementedException ();
        }

        internal override bool SafeDeepSearch (out object value, object varname)
        {
            throw new NotImplementedException ();
        }

        internal override bool SetValueCell (object varname, ValueCell newCell)
        {
            throw new NotImplementedException ();
        }

        internal override bool IsUnbound (object varname)
        {
            throw new NotImplementedException ();
        }

        internal override bool IsUnreferenceable (object varname)
        {
            throw new NotImplementedException ();
        }

        internal override bool DangerousLexicalRef (out object value, object varname, int shadowDepth, int depth, int offset)
        {
            return GetAncestorEnvironment (shadowDepth).LexicalRef (out value, varname, depth - shadowDepth, offset);
        }

        internal override bool LexicalRef (out object value, object varname, int depth, int offset)
        {
            if (depth == 0) {
                if (offset == 0) {
                    value = value0;
                    return false;
                }
                else
                    throw new NotImplementedException ();
            }
            return this.Closure.Environment.LexicalRef (out value, varname, depth - 1, offset);
        }

        internal override bool FastLexicalRef1 (out object value, object name, int offset)
        {
            value = this.closure.Environment.ArgumentValue (offset);
            return false;
        }

        internal override bool FreeRef (out object value, object varname)
        {
            throw new NotImplementedException ();
        }

        internal override bool DangerousFreeRef (out object value, object varname, int shadowDepth, int depth)
        {
            return GetAncestorEnvironment (shadowDepth).FreeRef (out value, varname, depth);
        }

        internal override bool FreeRef (out object value, object varname, int depth)
        {
            if (depth == 1)
                return this.Closure.Environment.DeepSearch (out value, varname);
            else
                return this.Closure.Environment.FreeRef (out value, varname, depth - 1); 
        }

        //internal override BoundVariable SimulateLookup (object name)
        //{
        //    throw new NotImplementedException ();
        //}

        //internal override BoundVariable SimulateStaticLookup (object name, int depth)
        //{
        //    throw new NotImplementedException ();
        //}

        //internal override BoundVariable SimulateDangerousLookup (object name, int safeDepth, int depth)
        //{
        //    throw new NotImplementedException ();
        //}

    }

    [Serializable]
    sealed class SmallEnvironment2 : LexicalEnvironment
    {
        readonly object value0;
        readonly object value1;


        internal SmallEnvironment2 (ClosureBase closure, object value0, object value1)
            : base (closure)
        {
#if DEBUG
            if (value0 is ReferenceTrap) throw new NotImplementedException ();
            if (value1 is ReferenceTrap) throw new NotImplementedException ();
#endif
            this.value0 = value0;
            this.value1 = value1;
        }

        public override object ArgumentValue (int offset)
        {
            if (offset == 1) return this.value1;
            else return this.value0;
        }

        public override object Argument0Value
        {
            get
            {
                return this.value0;
            }
        }

        public override object Argument1Value
        {
            get
            {
                return this.value1;
            }
        }

        public override bool Assign (out object oldValue, object varname, object newValue)
        {
            int offset = this.closure.FormalOffset (varname);
            if (offset == -1) {
                return closure.Environment.Assign (out oldValue, varname, newValue);
            }
            throw new NotImplementedException ();
        }

        public override bool Define (object varname, object value)
        {
            throw new NotImplementedException ();
        }

        public override bool DeepSearch (out object value, object varname)
        {
            return DeepSearch (out value, varname, 0);
        }

        internal override bool DeepSearch (out object value, object varname, int depth)
        {
            int offset = this.closure.FormalOffset (varname);
            if (offset == -1) {
                return closure.Environment.DeepSearch (out value, varname, depth + 1);
            }
#if DEBUG
            foundAtDepth [depth] += 1;
#endif
            value = (offset == 1) ? value1 : value0;
            return false;
        }

        internal override bool DeepSearchType (out object value, object varname)
        {
            throw new NotImplementedException ();
        }

        internal override ValueCell GetValueCell (object varname)
        {
            throw new NotImplementedException ();
        }

        internal override bool SafeDeepSearch (out object value, object varname)
        {
            throw new NotImplementedException ();
        }

        internal override bool SetValueCell (object varname, ValueCell newCell)
        {
            throw new NotImplementedException ();
        }

        internal override bool IsUnbound (object varname)
        {
            throw new NotImplementedException ();
        }

        internal override bool IsUnreferenceable (object varname)
        {
            throw new NotImplementedException ();
        }

        internal override bool DangerousLexicalRef (out object value, object varname, int shadowDepth, int depth, int offset)
        {
            return GetAncestorEnvironment (shadowDepth).LexicalRef (out value, varname, depth - shadowDepth, offset);
        }

        internal override bool LexicalRef (out object value, object varname, int depth, int offset)
        {
            if (depth == 0) {
                if (offset == 0) {
                    value = value0;
                    return false;
                }
                else if (offset == 1) {
                    value = value1;
                    return false;
                }
                else
                    throw new NotImplementedException ();
            }
            else
                return this.Closure.Environment.LexicalRef (out value, varname, depth - 1, offset);
        }

        internal override bool FastLexicalRef1 (out object value, object name, int offset)
        {
            value = this.closure.Environment.ArgumentValue (offset);
            return false;
        }

        internal override bool FreeRef (out object value, object varname)
        {
            throw new NotImplementedException ();
        }

        internal override bool DangerousFreeRef (out object value, object varname, int shadowDepth, int depth)
        {
            return GetAncestorEnvironment (shadowDepth).FreeRef (out value, varname, depth - shadowDepth);
        }

        internal override bool FreeRef (out object value, object varname, int depth)
        {
            if (depth == 1)
                return this.Closure.Environment.DeepSearch (out value, varname);
            else
                return this.Closure.Environment.FreeRef (out value, varname, depth - 1); 
        }

        //internal override BoundVariable SimulateLookup (object name)
        //{
        //    throw new NotImplementedException ();
        //}

        //internal override BoundVariable SimulateStaticLookup (object name, int depth)
        //{
        //    throw new NotImplementedException ();
        //}

        //internal override BoundVariable SimulateDangerousLookup (object name, int safeDepth, int depth)
        //{
        //    throw new NotImplementedException ();
        //}

    }

    [Serializable]
    sealed class SmallEnvironment3 : LexicalEnvironment
    {
        readonly object value0;
        readonly object value1;
        readonly object value2;

        internal SmallEnvironment3 (ClosureBase closure, object value0, object value1, object value2)
            : base (closure)
        {
#if DEBUG
            if (value0 is ReferenceTrap) throw new NotImplementedException ();
            if (value1 is ReferenceTrap) throw new NotImplementedException ();
            if (value2 is ReferenceTrap) throw new NotImplementedException ();
#endif
            this.value0 = value0;
            this.value1 = value1;
            this.value2 = value2;
        }

        public override object ArgumentValue (int offset)
        {
            switch (offset) {
                case 0: return this.value0;
                case 1: return this.value1;
                case 2: return this.value2;
                default:
                    throw new NotImplementedException ();
            }
        }

        public override object Argument0Value
        {
            get
            {
                return this.value0;
            }
        }

        public override object Argument1Value
        {
            get
            {
                return this.value1;
            }
        }

        public override bool Assign (out object oldValue, object varname, object newValue)
        {
            int offset = this.closure.FormalOffset (varname);
            if (offset == -1) {
                return closure.Environment.Assign (out oldValue, varname, newValue);
            }
            throw new NotImplementedException ();
        }

        public override bool Define (object varname, object value)
        {
            throw new NotImplementedException ();
        }

        public override bool DeepSearch (out object value, object varname)
        {
            return DeepSearch (out value, varname, 0);
        }

        internal override bool DeepSearch (out object value, object varname, int depth)
        {
            int offset = this.closure.FormalOffset (varname);
            if (offset == -1) {
                return closure.Environment.DeepSearch (out value, varname, depth + 1);
            }
#if DEBUG
            foundAtDepth [depth] += 1;
#endif
            value = (offset == 1) ? value1 : value0;
            return false;
        }

        internal override bool DeepSearchType (out object value, object varname)
        {
            throw new NotImplementedException ();
        }

        internal override ValueCell GetValueCell (object varname)
        {
            throw new NotImplementedException ();
        }

        internal override bool SafeDeepSearch (out object value, object varname)
        {
            throw new NotImplementedException ();
        }

        internal override bool SetValueCell (object varname, ValueCell newCell)
        {
            throw new NotImplementedException ();
        }

        internal override bool IsUnbound (object varname)
        {
            throw new NotImplementedException ();
        }

        internal override bool IsUnreferenceable (object varname)
        {
            throw new NotImplementedException ();
        }

        internal override bool DangerousLexicalRef (out object value, object varname, int shadowDepth, int depth, int offset)
        {
            return GetAncestorEnvironment (shadowDepth).LexicalRef (out value, varname, depth - shadowDepth, offset);
        }

        internal override bool LexicalRef (out object value, object varname, int depth, int offset)
        {
            if (depth == 0) {
                value = ArgumentValue (offset);
                return false;
            }
            else
                return this.closure.Environment.LexicalRef (out value, varname, depth - 1, offset);
        }

        internal override bool FastLexicalRef1 (out object value, object name, int offset)
        {
            value = this.closure.Environment.ArgumentValue (offset);
            return false;
        }

        internal override bool FreeRef (out object value, object varname)
        {
            throw new NotImplementedException ();
        }

        internal override bool DangerousFreeRef (out object value, object varname, int shadowDepth, int depth)
        {
            return GetAncestorEnvironment (shadowDepth).FreeRef (out value, varname, depth - shadowDepth);
        }

        internal override bool FreeRef (out object value, object varname, int depth)
        {
            if (depth == 0)
                throw new NotImplementedException ();
            else
                return this.closure.Environment.FreeRef (out value, varname, depth - 1);
        }

        //internal override BoundVariable SimulateLookup (object name)
        //{
        //    throw new NotImplementedException ();
        //}

        //internal override BoundVariable SimulateStaticLookup (object name, int depth)
        //{
        //    throw new NotImplementedException ();
        //}

        //internal override BoundVariable SimulateDangerousLookup (object name, int safeDepth, int depth)
        //{
        //    throw new NotImplementedException ();
        //}

    }

    [Serializable]
    sealed class SmallEnvironment4 : LexicalEnvironment
    {
        object value0;
        object value1;
        object value2;
        object value3;

        internal SmallEnvironment4 (ClosureBase closure, object value0, object value1, object value2, object value3)
            : base (closure)
        {
#if DEBUG
            if (value0 is ReferenceTrap) throw new NotImplementedException ();
            if (value1 is ReferenceTrap) throw new NotImplementedException ();
            if (value2 is ReferenceTrap) throw new NotImplementedException ();
            if (value3 is ReferenceTrap) throw new NotImplementedException ();
#endif
            this.value0 = value0;
            this.value1 = value1;
            this.value2 = value2;
            this.value3 = value3;
        }

        public override object ArgumentValue (int offset)
        {
            switch (offset) {
                case 0: return this.value0;
                case 1: return this.value1;
                case 2: return this.value2;
                case 3: return this.value3;
                default:
                    throw new NotImplementedException ();
            }
        }

        public override object Argument0Value
        {
            get
            {
                return this.value0;
            }
        }

        public override object Argument1Value
        {
            get
            {
                return this.value1;
            }
        }

        public override bool Assign (out object oldValue, object varname, object newValue)
        {
            int offset = this.closure.FormalOffset (varname);
            if (offset == -1) {
                return closure.Environment.Assign (out oldValue, varname, newValue);
            }
            throw new NotImplementedException ();
        }

        public override bool Define (object varname, object value)
        {
            throw new NotImplementedException ();
        }

        public override bool DeepSearch (out object value, object varname)
        {
            return DeepSearch (out value, varname, 0);
        }

        internal override bool DeepSearch (out object value, object varname, int depth)
        {
            int offset = this.closure.FormalOffset (varname);
            if (offset == -1) {
                return closure.Environment.DeepSearch (out value, varname, depth + 1);
            }
#if DEBUG
            foundAtDepth [depth] += 1;
#endif
            value = (offset == 1) ? value1 : value0;
            return false;
        }

        internal override bool DeepSearchType (out object value, object varname)
        {
            throw new NotImplementedException ();
        }

        internal override ValueCell GetValueCell (object varname)
        {
            throw new NotImplementedException ();
        }

        internal override bool SafeDeepSearch (out object value, object varname)
        {
            throw new NotImplementedException ();
        }

        internal override bool SetValueCell (object varname, ValueCell newCell)
        {
            throw new NotImplementedException ();
        }

        internal override bool IsUnbound (object varname)
        {
            throw new NotImplementedException ();
        }

        internal override bool IsUnreferenceable (object varname)
        {
            throw new NotImplementedException ();
        }

        internal override bool DangerousLexicalRef (out object value, object varname, int shadowDepth, int depth, int offset)
        {
            throw new NotImplementedException ();
        }

        internal override bool LexicalRef (out object value, object varname, int depth, int offset)
        {
            if (depth == 0) {
                value = ArgumentValue (offset);
                return false;
            }
            else
                return this.closure.Environment.LexicalRef (out value, varname, depth - 1, offset);

        }
        internal override bool FastLexicalRef1 (out object value, object name, int offset)
        {
            value = this.closure.Environment.ArgumentValue (offset);
            return false;
        }

        internal override bool FreeRef (out object value, object varname)
        {
            throw new NotImplementedException ();
        }

        internal override bool DangerousFreeRef (out object value, object varname, int shadowDepth, int depth)
        {
            return GetAncestorEnvironment (shadowDepth).FreeRef (out value, varname, depth - shadowDepth);
        }

        internal override bool FreeRef (out object value, object varname, int depth)
        {
            if (depth == 0)
                throw new NotImplementedException ();
            else
                return this.closure.Environment.FreeRef (out value, varname, depth - 1);
        }

        //internal override BoundVariable SimulateLookup (object name)
        //{
        //    throw new NotImplementedException ();
        //}

        //internal override BoundVariable SimulateStaticLookup (object name, int depth)
        //{
        //    throw new NotImplementedException ();
        //}

        //internal override BoundVariable SimulateDangerousLookup (object name, int safeDepth, int depth)
        //{
        //    throw new NotImplementedException ();
        //}
    }

     //These are magic structures that get stuffed in the
     //environment register when unwinding the stack.  They are
     //not actually environments, but need to be of that type.
    abstract class FakeEnvironment : Environment
    {
        protected FakeEnvironment ()
            : base (null)
        { }

        public override object ArgumentValue (int offset)
        {
            throw new NotImplementedException ();
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

        internal override bool DeepSearch (out object value, object name, int depth)
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

        internal override bool SafeDeepSearch (out object answer, object name)
        {
            throw new NotImplementedException ();
        }

        internal override bool DeepSearchType (out object answer, object name)
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

        internal override bool FastLexicalRef (out object value, object name, int depth, int offset)
        {
            throw new NotImplementedException ();
        }

        internal override Environment GetAncestorEnvironment (int depth)
        {
            throw new NotImplementedException ();
        }

        internal override bool DangerousLexicalRef (out object value, object name, int shadowDepth, int depth, int offset)
        {
            throw new NotImplementedException ();
        }

        internal override bool LexicalRef (out object value, object name, int depth, int offset)
        {
            throw new NotImplementedException ();
        }

        internal override bool FreeRef (out object value, object name)
        {
            throw new NotImplementedException ();
        }

        internal override bool DangerousFreeRef (out object value, object name, int shadowDepth, int depth)
        {
            throw new NotImplementedException ();
        }

        internal override bool FreeRef (out object value, object name, int depth)
        {
            throw new NotImplementedException ();
        }

        //internal override BoundVariable SimulateLookup (object name)
        //{
        //    throw new NotImplementedException ();
        //}

        //internal override BoundVariable SimulateStaticLookup (object name, int depth)
        //{
        //    throw new NotImplementedException ();
        //}

        //internal override BoundVariable SimulateDangerousLookup (object name, int safeDepth, int depth)
        //{
        //    throw new NotImplementedException ();
        //}

        internal override bool FastLexicalRef1 (out object value, object name, int offset)
        {
            throw new NotImplementedException ();
        }
    }

    class WithinControlPoint : FakeEnvironment
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        ControlPoint controlPoint;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        ClosureBase thunk;

        public WithinControlPoint (ControlPoint controlPoint, ClosureBase thunk)
        {
            this.controlPoint = controlPoint;
            this.thunk = thunk;
        }
    }

    // not really an environment at all, but rather
    // a structure that holds the reified stack as we
    // unwind it.
    class UnwinderState : FakeEnvironment
    {
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
        /// <param varname="newContinuation"></param>
        /// <param varname="receiver"></param>
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
        /// <param varname="receiver"></param>
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

    // Not an environment, but the state needed to restore the
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

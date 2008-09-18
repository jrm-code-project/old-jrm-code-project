using System;
using System.Collections.Generic;
using System.Diagnostics;

namespace Microcode
{
    public enum LookupDisposition
    {
        OK,
        Unbound,
        Unassigned,
        Macro
    };

    enum ReferenceType
    {
        Unbound,
        Unassigned,
        Normal,
        Macro
    };

    [Serializable]
    public abstract class Environment : SchemeObject
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public static GlobalEnvironment systemGlobalEnvironment;

        protected Environment ()
            : base (TC.ENVIRONMENT)
        {
        }

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
        static protected int [] foundAtDepth = new int [128];
        [NonSerialized]
        static protected int [] foundInAuxes = new int [128];
        [NonSerialized]
        static protected int [] foundInGlobal = new int [128];
        [NonSerialized]
        static protected int [] extendedBy = new int [1024];
#endif

        // Abstract functions on environments

        // Grab the variable at offset in frame.
        public abstract object ArgumentValue (int offset);
        public abstract object Argument0Value {get;}
        public abstract object Argument1Value {get;}


        // Deep search for variable location and smash the value.
        public abstract bool Assign (out object oldValue, string name, object newValue);

        // Gets the closure that created this environment
        public abstract IClosure Closure {get;}

        // Define a variable in the topmost frame.  This will shadow
        // other definitions.  Returns false on success, true if there
        // is a problem.
        public abstract bool Define (string name, object value);

        // Deep search the lexical environment for the variable.
        // returns false on success, true if there is a problem.
        public abstract bool DeepSearch (out object value, string name);
        // internal call tracks depth.
        internal abstract bool DeepSearch ( out object value, string name, int depth);
        internal abstract bool DeepSearchType (out object value, string name);

        // Used to link variables.
        internal abstract ValueCell GetValueCell (string name);

        internal abstract bool LexicalRef (out object value, string name, int depth, int offset);
        // special case only one level deep.
        internal abstract bool LexicalRef1 (out object value, string name, int offset);

        internal abstract bool SafeDeepSearch (out object value, string name);
        // Used to link variables.
        internal abstract bool SetValueCell (string name, ValueCell newCell);

        internal abstract Dictionary<object, ValueCell> Incrementals { get; }

        internal abstract bool IsUnbound (string name);
        internal abstract bool IsUnreferenceable (string name);

        [SchemePrimitive ("LEXICAL-UNBOUND?", 2, false)]
        public static bool IsLexicalUnbound (out object answer, object env, object name)
        {
            answer = ToEnvironment (env).IsUnbound ((string) name);
            return false; // copacetic
        }

        [SchemePrimitive ("LEXICAL-UNREFERENCEABLE?", 2, false)]
        public static bool IsLexicalUnreferenceable (out object answer, object env, object name)
        {
            answer = ToEnvironment(env).IsUnreferenceable ((string) name);
            return false; // copacetic
        }

        // Same as assigning in the interpreter.
        [SchemePrimitive ("LEXICAL-ASSIGNMENT", 3, false)]
        public static bool LexicalAssignment (out object answer, object aenv, object aname, object value)
        {
            Environment env = ToEnvironment(aenv);
            string name = (string) aname;
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
            if (ToEnvironment(env).DeepSearch (out answer, (string) name))
                throw new NotImplementedException ("Error during lexical-reference.");
            return false;
        }

        // Looks up variable, tells whether it is a macro, unassigned, etc.
        [SchemePrimitive ("LEXICAL-REFERENCE-TYPE", 2, false)]
        public static bool LexicalReferenceType (out object answer, object env, object name)
        {
            if (ToEnvironment(env).DeepSearchType (out answer, (string) name))
                throw new NotImplementedException ("Error during lexical-reference-type.");
            return false;
        }


        [SchemePrimitive ("SAFE-LEXICAL-REFERENCE", 2, false)]
        public static bool SafeLexicalReference (out object answer, object env, object name)
        {
            if (ToEnvironment(env).SafeDeepSearch (out answer, (string) name))
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
            if (source_name == "deferred-unparser-methods"
                || target_name == "deferred-unparser-methods")
                Debugger.Break ();
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
            if ((string)aname == "deferred-unparser-methods")
                Debugger.Break ();
            if (ToEnvironment(env).Define ((string) aname, value)) throw new NotImplementedException();
            
            answer = aname;
            return false;
         }

        [SchemePrimitive ("ENVIRONMENT?", 1, true)]
        public static bool IsEnvironment (out object answer, object arg)
        {
            answer = arg is Environment || (arg is bool && (bool) arg == false);
            return false;
        }
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
        public override object ArgumentValue (int offset)
        {
            throw new NotImplementedException ();
        }
        
        public override bool Define (string name, object value)
        {
            throw new NotImplementedException ();
        }

        public override bool DeepSearch (out object value, string name )
        {
            throw new NotImplementedException ();
        }

        internal override bool DeepSearch (out object value, string name, int depth)
        {
            throw new NotImplementedException ();
        }

        public override bool Assign (out object oldValue, string name, object newValue)
        {
            throw new NotImplementedException ();
        }

        internal override ValueCell GetValueCell (string name)
        {
            throw new NotImplementedException ();
        }

        internal override bool SetValueCell (string name, ValueCell newCell)
        {
            throw new NotImplementedException ();
        }

        internal override bool IsUnreferenceable (string name)
        {
            throw new NotImplementedException ();
        }

        internal override bool SafeDeepSearch ( out object value, string name)
        {
            throw new NotImplementedException ();
        }

        internal override bool DeepSearchType (out object value, string name)
        {
            throw new NotImplementedException ();
        }

        internal override bool IsUnbound (string name)
        {
            throw new NotImplementedException ();
        }

        public override IClosure Closure
        {
            get { throw new NotImplementedException (); }
        }

        internal override Dictionary<object, ValueCell> Incrementals
        {
            get { throw new NotImplementedException (); }
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

        internal override bool LexicalRef (out object value, string name, int depth, int offset)
        {
            throw new NotImplementedException ();
        }

        internal override bool LexicalRef1 (out object value, string name, int offset)
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
        public override object ArgumentValue (int offset)
        {
            throw new NotImplementedException ();
        }

        public override bool Define (string name, object value)
        {
            ValueCell cell = null;
            if (this.globalBindings.TryGetValue (name, out cell) == false) {
                cell = new ValueCell (name, value);
                this.globalBindings.Add (name, cell);
            }
            else {
                object oldValue;
                cell.Assign (out oldValue, value);
            }
            return false; // no problems
        }

        public override bool DeepSearch (out object value, string name)
        {
            return DeepSearch (out value, name, 0);
        }

        internal override bool DeepSearch (out object value, string name, int depth)
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
            throw new NotImplementedException ("Variable not bound");
        }

         public override bool Assign (out object oldValue, string name, object newValue)
        {
            ValueCell vcell = null;
            if (this.globalBindings.TryGetValue (name, out vcell)) {
                if (vcell.Assign (out oldValue, newValue))
                    throw new NotImplementedException ("Error in Assign.");
                return false;
            }
            throw new NotImplementedException ("IsUnbound variable in Assign.");
        }

        internal override ValueCell GetValueCell (string name)
        {
             throw new NotImplementedException();
        }


        internal override bool SafeDeepSearch (out object value, string name)
        {
            ValueCell cell = null;
            if (this.globalBindings.TryGetValue (name, out cell)) {

                if (cell.SafeGetValue (out value))
                    throw new NotImplementedException ("Error getting value from cell");
                return false;
            }
            throw new NotImplementedException ("Variable not bound");
        }

        internal override bool DeepSearchType (out object value, string name)
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

        internal override bool SetValueCell (string name, ValueCell newCell)
        {                        
            ValueCell cell = null;
            if (this.globalBindings.TryGetValue (name, out cell)) {
                throw new NotImplementedException ("Value cell already exists.");
            }
            this.globalBindings.Add (name, newCell);
            return false; // copacetic
        }

        internal override bool IsUnreferenceable (string name)
        {
            ValueCell vcell;
            if (this.globalBindings.TryGetValue (name, out vcell)) {
                return vcell.Unreferenceable ();
            }
            else return true;
        }

        internal override bool IsUnbound (string name)
        {
            throw new NotImplementedException ();
        }

        public override IClosure Closure
        {
            get { return null; }
        }

        internal override Dictionary<object, ValueCell> Incrementals
        {
            get { throw new NotImplementedException (); }
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

        internal override bool LexicalRef (out object value, string name, int depth, int offset)
        {
            throw new NotImplementedException ();
        }

        internal override bool LexicalRef1 (out object value, string name, int offset)
        {
            throw new NotImplementedException ();
        }
    }

    /// <summary>
    /// A StandardEnvironment supports sharable bindings and incremental
    /// definition.
    /// </summary>
    [Serializable]
    sealed class StandardEnvironment : Environment
    {
#if DEBUG
        static long creationCount;
#endif
                [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly IClosure closure;
                [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly ValueCell [] bindings;
                [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        Dictionary <object,ValueCell> incrementals;

        internal StandardEnvironment (IClosure closure)
        {  
            this.closure = closure;
#if DEBUG
            StandardEnvironment.creationCount += 1;
            extendedBy [0] += 1;
            // sanity check.
            String [] formals = closure.Lambda.Formals;
            if (formals.Length != 0)
                throw new NotImplementedException ();
 #endif
       }

        internal StandardEnvironment (IClosure closure, object [] initialValues)
        {
            this.closure = closure;
            String [] formals = closure.Lambda.Formals;  
            this.bindings = new ValueCell [initialValues.Length];
            for (int i = 0; i < initialValues.Length; i++)
                this.bindings [i] = new ValueCell (formals[i], initialValues [i]);
#if DEBUG
            StandardEnvironment.creationCount += 1;
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
        
        public override bool Assign (out object oldValue, string name, object newValue)
        {
            //if (name == "deferred-unparser-methods")
            //    Debugger.Break ();
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
        
        public override bool Define (string name, object value)
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

        public override bool DeepSearch (out object value, string name)
        {
            return DeepSearch (out value, name, 0);
        }

        internal override bool DeepSearch (out object value, string name, int depth)
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

        internal override bool LexicalRef (out object value, string name, int depth, int offset)
        {
            if (depth == 0) return bindings [offset].GetValue (out value);
            ValueCell vcell;
            if (this.incrementals != null
                && this.incrementals.TryGetValue (name, out vcell)) {
                return vcell.GetValue (out value);
            }
            return closure.Environment.LexicalRef (out value, name, depth - 1, offset);
        }

        internal override bool LexicalRef1 (out object value, string name, int offset)
        {
            ValueCell vcell;
            if (this.incrementals != null
                && this.incrementals.TryGetValue (name, out vcell)) {
                return vcell.GetValue (out value);
            }
            value = closure.Environment.ArgumentValue (offset);
            return false;
        }

        internal override ValueCell GetValueCell (string name)
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

        internal override bool SetValueCell (string name, ValueCell newCell)
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

        internal override bool SafeDeepSearch (out object value, string name)
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

        internal override bool DeepSearchType (out object value, string name)
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
        
        
        internal override bool IsUnreferenceable (string name)
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


        internal override bool IsUnbound (string name)
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

        public override IClosure Closure
        {
            get { return this.closure; }
        }

        internal override Dictionary<object, ValueCell> Incrementals
        {
            get
            {
                if (this.incrementals == null)
                    this.incrementals = new Dictionary<object, ValueCell> ();
                return this.incrementals;
            }

        }
    }

    [Serializable]
    sealed class SimpleEnvironment : Environment
    {
#if DEBUG
        static long creationCount;
#endif
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly IClosure closure;
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly object [] bindings;

        internal SimpleEnvironment (IClosure closure)
        {
            this.closure = closure;
#if DEBUG
            SimpleEnvironment.creationCount += 1;
            extendedBy [0] += 1;
            // sanity checks.
            String [] formals = closure.Lambda.Formals;
            if (formals.Length != 0)
                throw new NotImplementedException ();
#endif
        }

        internal SimpleEnvironment (IClosure closure, object [] initialValues)
        {
            this.closure = closure;
            String [] formals = closure.Lambda.Formals;
            this.bindings = initialValues;
#if DEBUG
            SimpleEnvironment.creationCount += 1;
            extendedBy [initialValues.Length] += 1;
            // sanity check
            if (formals.Length != initialValues.Length)
                throw new NotImplementedException ();
            foreach (object init in initialValues)
                if (init is ReferenceTrap) throw new NotImplementedException ();
#endif
        }

        public override object ArgumentValue (int offset)
        {
            return bindings [offset];
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

        public override bool Assign (out object oldValue, string name, object newValue)
        {
            //if (name == "deferred-unparser-methods")
            //    Debugger.Break ();
            int offset = this.closure.FormalOffset (name);
            if (offset == -1) {
                    return closure.Environment.Assign (out oldValue, name, newValue);
            }
            oldValue = bindings [offset];
            bindings [offset] = newValue;
            return false;
        }

        public override bool Define (string name, object value)
        {
            int offset = this.closure.FormalOffset (name);
            if (offset == -1) {
                throw new NotImplementedException ();
            }
            else {
                bindings [offset] = value;
                return false;
            }
        }

        public override bool DeepSearch (out object value, string name)
        {
            return DeepSearch (out value, name, 0);
        }

        internal override bool DeepSearch (out object value, string name, int depth)
        {
            int offset = this.closure.FormalOffset (name);
            if (offset == -1) {
                    return closure.Environment.DeepSearch (out value, name, depth + 1);
            }
#if DEBUG
            foundAtDepth [depth] += 1;
#endif
            value = bindings [offset];
            return false;
        }

        internal override bool LexicalRef (out object value, string name, int depth, int offset)
        {
            if (depth == 0) {
                value = bindings [offset];
                return false;
            }
            else return closure.Environment.LexicalRef (out value, name, depth - 1, offset);
        }

        internal override bool LexicalRef1 (out object value, string name, int offset)
        {
           value = closure.Environment.ArgumentValue (offset);
           return false;
        }

        internal override ValueCell GetValueCell (string name)
        {
            throw new NotImplementedException ();
        }

        internal override bool SetValueCell (string name, ValueCell newCell)
        {
            throw new NotImplementedException ();
        }

        internal override bool SafeDeepSearch (out object value, string name)
        {
            int offset = this.closure.FormalOffset (name);
            if (offset == -1) {
                    return closure.Environment.SafeDeepSearch (out value, name);
            }
            value = bindings [offset];
            return false;
        }

        internal override bool DeepSearchType (out object value, string name)
        {
            throw new NotImplementedException ();
        }


        internal override bool IsUnreferenceable (string name)
        {
            int offset = this.closure.FormalOffset (name);
            if (offset == -1) {
                    return this.closure.Environment.IsUnreferenceable (name);
            }
            return false;
        }


        internal override bool IsUnbound (string name)
        {
            int offset = this.closure.FormalOffset (name);
            if (offset == -1) {
                    return this.closure.Environment.IsUnbound (name);
            }
            return false;
        }

        public override IClosure Closure
        {
            get { return this.closure; }
        }

        internal override Dictionary<object, ValueCell> Incrementals
        {
            get { throw new NotImplementedException (); }
        }
    }

    /// <summary>
    /// A SmallEnvironment needs no value cells because
    /// the bindings are never mutated.
    /// </summary>
    [Serializable]
    sealed class SmallEnvironment : Environment
    {
#if DEBUG
        static long creationCount;
#endif
        IClosure closure;
        object value0;
        object value1;

        internal SmallEnvironment (IClosure closure, object value0)
        {
#if DEBUG
            SmallEnvironment.creationCount += 1;
            if (value0 is ReferenceTrap) throw new NotImplementedException ();
#endif
            this.closure = closure;
            this.value0 = value0;
        }

        internal SmallEnvironment (IClosure closure, object value0, object value1)
        {
#if DEBUG
            SmallEnvironment.creationCount += 1;
            if (value0 is ReferenceTrap) throw new NotImplementedException ();
            if (value1 is ReferenceTrap) throw new NotImplementedException ();
#endif
            this.closure = closure;
            this.value0 = value0;
            this.value1 = value1;

        }

        public override object ArgumentValue (int offset)
        {
            if (offset == 1) return this.value1;
            else return this.value0;
        }

        public override object Argument0Value { get
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

        public override bool Assign (out object oldValue, string name, object newValue)
        {
            int offset = this.closure.FormalOffset (name);
            if (offset == -1) {
                return closure.Environment.Assign (out oldValue, name, newValue);
            }
            throw new NotImplementedException ();
        }

        public override IClosure Closure
        {
            get { return this.closure; }
        }

        public override bool Define (string name, object value)
        {
            throw new NotImplementedException ();
        }

        public override bool DeepSearch (out object value, string name)
        {
            return DeepSearch (out value, name, 0);
        }

        internal override bool DeepSearch (out object value, string name, int depth)
        {
            int offset = this.closure.FormalOffset (name);
            if (offset == -1) {
                return closure.Environment.DeepSearch (out value, name, depth + 1);
            }
#if DEBUG
            foundAtDepth [depth] += 1;
#endif
            value = (offset == 1) ? value1: value0;
            return false;
        }

        internal override bool DeepSearchType (out object value, string name)
        {
            throw new NotImplementedException ();
        }
        internal override bool LexicalRef (out object value, string name, int depth, int offset)
        {
            if (depth == 0) {
                value = (offset == 1) ? value1 : value0;
                return false;
            }
            else
                return closure.Environment.LexicalRef (out value, name, depth - 1, offset);
        }

        internal override bool LexicalRef1 (out object value, string name, int offset)
        {
            value = closure.Environment.ArgumentValue (offset);
            return false;
        }

        internal override ValueCell GetValueCell (string name)
        {
            throw new NotImplementedException ();
        }

        internal override bool SafeDeepSearch (out object value, string name)
        {
            throw new NotImplementedException ();
        }

        internal override bool SetValueCell (string name, ValueCell newCell)
        {
            throw new NotImplementedException ();
        }

        internal override Dictionary<object, ValueCell> Incrementals
        {
            get { throw new NotImplementedException (); }
        }

        internal override bool IsUnbound (string name)
        {
            throw new NotImplementedException ();
        }

        internal override bool IsUnreferenceable (string name)
        {
            throw new NotImplementedException ();
        }
    }

    // These are magic structures that get stuffed in the
    // environment register when unwinding the stack.  They are
    // not actually environments, but need to be of that type.
    class WithinControlPoint : Environment
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        ControlPoint controlPoint;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        IClosure thunk;

        public WithinControlPoint (ControlPoint controlPoint, IClosure thunk)
        {
            this.controlPoint = controlPoint;
            this.thunk = thunk;
        }


        public override object ArgumentValue (int offset)
        {
            throw new NotImplementedException ();
        }

        public override bool Define (string name, object value)
        {
            throw new NotImplementedException ();
        }

        public override bool DeepSearch (out object value, string name)
        {
            throw new NotImplementedException ();
        }

        internal override bool DeepSearch (out object value, string name, int depth)
        {
            throw new NotImplementedException ();
        }

        public override bool Assign (out object oldValue, string name, object newValue)
        {
            throw new NotImplementedException ();
        }

        internal override ValueCell GetValueCell (string name)
        {
            throw new NotImplementedException ();
        }

        internal override bool SetValueCell (string name, ValueCell newCell)
        {
            throw new NotImplementedException ();
        }

        internal override bool IsUnreferenceable (string name)
        {
            throw new NotImplementedException ();
        }

        internal override bool SafeDeepSearch (out object answer, string name)
        {
            throw new NotImplementedException ();
        }

        internal override bool DeepSearchType (out object answer, string name)
        {
            throw new NotImplementedException ();
        }

        internal override bool IsUnbound (string name)
        {
            throw new NotImplementedException ();
        }

        public override IClosure Closure
        {
            get { throw new NotImplementedException (); }
        }

        internal override Dictionary<object, ValueCell> Incrementals
        {
            get { throw new NotImplementedException (); }
        }

        public override object Argument0Value { get 
        {
            throw new NotImplementedException ();
        }
        }

        public override object Argument1Value { get 
        {
            throw new NotImplementedException ();
        }
        }

        internal override bool LexicalRef (out object value, string name, int depth, int offset)
        {
            throw new NotImplementedException ();
        }

        internal override bool LexicalRef1 (out object value, string name, int offset)
        {
            throw new NotImplementedException ();
        }
    }


    // not really an environment at all, but rather
    // a structure that holds the reified stack as we
    // unwind it.
    class UnwinderState : Environment
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
        /// <param name="newContinuation"></param>
        /// <param name="receiver"></param>
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
        /// <param name="receiver"></param>
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
            get {
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

        // These make us look like an environment.

        public override object ArgumentValue ( int offset)
        {
            throw new NotImplementedException ();
        }

        public override bool Assign (out object oldValue, string name, object newValue)
        {
            throw new NotImplementedException ();
        }

        public override bool Define (string name, object value)
        {
            throw new NotImplementedException ();
        }

        public override bool DeepSearch (out object answer, string name)
        {
            throw new NotImplementedException ();
        }

        internal override bool DeepSearch (out object answer, string name, int depth)
        {
            throw new NotImplementedException ();
        }

        internal override ValueCell GetValueCell (string name)
        {
            throw new NotImplementedException ();
        }

        internal override bool SetValueCell (string name, ValueCell newCell)
        {
            throw new NotImplementedException ();
        }

        internal override bool IsUnreferenceable (string name)
        {
            throw new NotImplementedException ();
        }

        internal override bool SafeDeepSearch (out object answer, string name)
        {
            throw new NotImplementedException ();
        }

        internal override bool DeepSearchType (out object answer, string name)
        {
            throw new NotImplementedException ();
        }

        internal override bool IsUnbound (string name)
        {
            throw new NotImplementedException ();
        }

        public override IClosure Closure
        {
            get { throw new NotImplementedException (); }
        }

        internal override Dictionary<object, ValueCell> Incrementals
        {
            get { throw new NotImplementedException (); }
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

        internal override bool LexicalRef (out object value, string name, int depth, int offset)
        {
            throw new NotImplementedException ();
        }

        internal override bool LexicalRef1 (out object value, string name, int offset)
        {
            throw new NotImplementedException ();
        }
    }

    // Not an environment, but the state needed to restore the
    // continuation.
    class RewindState : Environment
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

        // We pretend to be an environment!
        public override object ArgumentValue (int offset)
        {
            throw new NotImplementedException ();
        }

        public override bool Assign (out object oldValue, string name, object newValue)
        {
            throw new NotImplementedException ();
        }

        public override bool Define (string name, object value)
        {
            throw new NotImplementedException ();
        }

        public override bool DeepSearch (out object value, string name)
        {
            throw new NotImplementedException ();
        }

        internal override bool DeepSearch (out object value, string name, int depth)
        {
            throw new NotImplementedException ();
        }

        internal override ValueCell GetValueCell (string name)
        {
            throw new NotImplementedException ();
        }

        internal override bool SetValueCell (string name, ValueCell newCell)
        {
            throw new NotImplementedException ();
        }

        internal override bool IsUnreferenceable (string name)
        {
            throw new NotImplementedException ();
        }

        internal override bool SafeDeepSearch (out object answer, string name)
        {
            throw new NotImplementedException ();
        }

        internal override bool DeepSearchType (out object answer, string name)
        {
            throw new NotImplementedException ();
        }

        internal override bool IsUnbound (string name)
        {
            throw new NotImplementedException ();
        }

        public override IClosure Closure
        {
            get { throw new NotImplementedException (); }
        }

        internal override Dictionary<object, ValueCell> Incrementals
        {
            get { throw new NotImplementedException (); }
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

        internal override bool LexicalRef (out object value, string name, int depth, int offset)
        {
            throw new NotImplementedException ();
        }

        internal override bool LexicalRef1 (out object value, string name, int offset)
        {
            throw new NotImplementedException ();
        }
    }
}

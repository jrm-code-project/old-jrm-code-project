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

    public abstract class Environment : SchemeObject
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        static Environment global;

        protected Environment ()
            : base (TC.ENVIRONMENT)
        {
        }

        public static Environment Global
        {
            [DebuggerStepThrough]
            get
            {
                if (global == null)
                    global = new GlobalEnvironment ();
                return global;
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
        static protected int [] foundAtDepth = new int [128];
        static protected int [] foundInAuxes = new int [128];
        static protected int [] foundInGlobal = new int [128];
        static protected int [] extendedBy = new int [256];
#endif

        // Abstract functions on environments

        // Grab the variable at offset in frame.
        public abstract bool ArgumentValue (out object value, int offset);

        // Deep search for variable location and smash the value.
        public abstract bool Assign (out object oldValue, string name, object newValue);

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

        internal abstract Environment Extend (IClosure closure);
        // Have to use vector because we can't discriminate one arg
        // that is a vector from one arg only.
        internal abstract Environment Extend (IClosure closure, object [] args);

        // Used to link variables.
        internal abstract ValueCell GetValueCell (string name);

        internal abstract bool SafeDeepSearch (out object value, string name);
        // Used to link variables.
        internal abstract bool SetValueCell (string name, ValueCell newCell);

        internal abstract bool Unreferenceable (string name);

        [SchemePrimitive ("LEXICAL-UNREFERENCEABLE?", 2)]
        public static bool IsLexicalUnreferenceable (out object answer, object env, object name)
        {
            answer = ((Environment) env).Unreferenceable ((string) name);
            return false; // copacetic
        }

        // Same as assigning in the interpreter.
        [SchemePrimitive ("LEXICAL-ASSIGNMENT", 3)]
        public static bool LexicalAssignment (out object answer, object aenv, object aname, object value)
        {
            Environment env = (Environment) aenv;
            string name = (string) aname;
            object oldValue;
            if (env.Assign (out oldValue, name, value))
                throw new NotImplementedException ("error during assignment");
            answer = oldValue;
            return false;
        }

        // Same as evaluating variable.
        [SchemePrimitive ("LEXICAL-REFERENCE", 2)]
        public static bool LexicalReference (out object answer, object env, object name)
        {
            if (((Environment) env).DeepSearch (out answer, (string) name))
                throw new NotImplementedException ("Error during lexical-reference.");
            return false;
        }

        // Looks up variable, tells whether it is a macro, unassigned, etc.
        [SchemePrimitive ("LEXICAL-REFERENCE-TYPE", 2)]
        public static bool LexicalReferenceType (out object answer, object env, object name)
        {
            if (((Environment) env).DeepSearchType (out answer, (string) name))
                throw new NotImplementedException ("Error during lexical-reference-type.");
            return false;
        }


        [SchemePrimitive ("SAFE-LEXICAL-REFERENCE", 2)]
        public static bool SafeLexicalReference (out object answer, object env, object name)
        {
            if (((Environment) env).SafeDeepSearch (out answer, (string) name))
                throw new NotImplementedException ("Error during lexical-reference.");
            return false;
        }

        [SchemePrimitive ("LINK-VARIABLES", 4)]
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
        [SchemePrimitive ("LOCAL-ASSIGNMENT", 3)]
        public static bool LocalAssignment (out object answer, object env, object aname, object value)
        {
            if ((string)aname == "deferred-unparser-methods")
                Debugger.Break ();
            if (ToEnvironment(env).Define ((string) aname, value)) throw new NotImplementedException();
            
            answer = aname;
            return false;
         }

        [SchemePrimitive ("ENVIRONMENT?", 1)]
        public static bool IsEnvironment (out object answer, object arg)
        {
            answer = arg is Environment || (arg is bool && (bool) arg == false);
            return false;
        }
    }

    // There are four different kinds of environment.  
    // The null environment doesn't respond to anything yet.
    // A Root environment has no parent and no frame.  It is
    // used for the global environment.
    // A `Top Level' environment has a frame and incrementals.
    // it can be extended, searched, etc.  A full-feature environment.

    class NullEnvironment : Environment
    {
        public override bool ArgumentValue (out object value, int offset)
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

        internal override Environment Extend (IClosure closure)
        {
            throw new NotImplementedException ();
        }

        internal override Environment Extend (IClosure closure, object [] args)
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

        internal override bool Unreferenceable (string name)
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
    }

    class GlobalEnvironment : Environment
    {
        public override bool ArgumentValue (out object value, int offset)
        {
            throw new NotImplementedException ();
        }

        Dictionary<string, ValueCell> bindings = new Dictionary<string, ValueCell> ();

        public override bool Define (string name, object value)
        {
            ValueCell cell = null;
            if (this.bindings.TryGetValue (name, out cell) == false) {
                cell = new ValueCell (name, value);
                this.bindings.Add (name, cell);
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
            if (this.bindings.TryGetValue (name, out cell)) {
#if DEBUG
                foundInGlobal [depth] += 1;
#endif
                if (cell.GetValue (out value)) 
                    throw new NotImplementedException ("Error getting value from cell");
                return false;
            }
            throw new NotImplementedException ("Variable not bound");
        }

        internal override Environment Extend (IClosure closure)
        {
#if DEBUG
            extendedBy [closure.Lambda.Formals.Length] += 1;
#endif
            return new TopLevelEnvironment (closure);
        }

        internal override Environment Extend (IClosure closure, object [] args)
        {
#if DEBUG
            extendedBy [args.Length] += 1;
#endif
            return new TopLevelEnvironment (closure, args);
        }

        public override bool Assign (out object oldValue, string name, object newValue)
        {
            ValueCell vcell = null;
            if (this.bindings.TryGetValue (name, out vcell)) {
                if (vcell.Assign (out oldValue, newValue))
                    throw new NotImplementedException ("Error in Assign.");
                return false;
            }
            throw new NotImplementedException ("Unbound variable in Assign.");
        }

        internal override ValueCell GetValueCell (string name)
        {
             throw new NotImplementedException();
        }


        internal override bool SafeDeepSearch (out object value, string name)
        {
            ValueCell cell = null;
            if (this.bindings.TryGetValue (name, out cell)) {

                if (cell.SafeGetValue (out value))
                    throw new NotImplementedException ("Error getting value from cell");
                return false;
            }
            throw new NotImplementedException ("Variable not bound");
        }

        internal override bool DeepSearchType (out object value, string name)
        {
            ValueCell cell = null;
            if (this.bindings.TryGetValue (name, out cell)) {

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
            if (this.bindings.TryGetValue (name, out cell)) {
                throw new NotImplementedException ("Value cell already exists.");
            }
            this.bindings.Add (name, newCell);
            return false; // copacetic
        }

        internal override bool Unreferenceable (string name)
        {
            ValueCell vcell;
            if (this.bindings.TryGetValue (name, out vcell)) {
                return vcell.Unreferenceable ();
            }
            else return true;
        }
    }

    sealed class TopLevelEnvironment : Environment
    {
        IClosure closure;
        ValueCell [] bindings;
        Dictionary <string,ValueCell> incrementals;

        internal TopLevelEnvironment (IClosure closure)
        {
            this.closure = closure;
        }

        internal TopLevelEnvironment (IClosure closure, object [] initialValues)
        {
            this.closure = closure;
            this.bindings = new ValueCell [initialValues.Length];
            String [] formals = closure.Lambda.Formals;
            for (int i = 0; i < initialValues.Length; i++)
                this.bindings [i] = new ValueCell (formals[i], initialValues [i]);
            //incrementals = new Dictionary<string, ValueCell> ();
        }


        public override bool ArgumentValue (out object value, int offset)
        {
            return bindings [offset].GetValue (out value);
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
                    this.incrementals = new Dictionary<string, ValueCell> ();
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

        internal override Environment Extend (IClosure closure)
        {
            throw new NotImplementedException ();
        }

        internal override Environment Extend (IClosure closure, object [] args)
        {
#if DEBUG
            extendedBy [args.Length] += 1;
#endif
            return new TopLevelEnvironment (closure, args);
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
        
        
        internal override bool Unreferenceable (string name)
        {
            int offset = this.closure.FormalOffset (name);
            if (offset == -1) {
                ValueCell vcell;
                if (incrementals != null
                    && incrementals.TryGetValue (name, out vcell))
                    return vcell.Unreferenceable ();
                else
                    return this.closure.Environment.Unreferenceable (name);
            }
            return bindings [offset].Unreferenceable ();
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


        public override bool ArgumentValue (out object value, int offset)
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

        internal override Environment Extend (IClosure closure)
        {
            throw new NotImplementedException ();
        }

        internal override Environment Extend (IClosure closure, object [] args)
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

        internal override bool Unreferenceable (string name)
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

        SCode receiver;

        /// <summary>
        /// Constructor that takes a new continuation to be installed.
        /// Stack is destroyed as we unwind and we reload the new one.
        /// </summary>
        /// <param name="newContinuation"></param>
        /// <param name="receiver"></param>
        internal UnwinderState (ControlPoint newContinuation, SCode receiver)
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
        internal UnwinderState (SCode receiver)
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

        internal SCode Receiver
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

        public override bool ArgumentValue (out object value, int offset)
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

        internal override Environment Extend (IClosure closure)
        {
            throw new NotImplementedException ();
        }

        internal override Environment Extend (IClosure closure, object [] args)
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

        internal override bool Unreferenceable (string name)
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
    }

    // Not an environment, but the state needed to restore the
    // continuation.
    class RewindState : Environment
    {
        ContinuationFrameList reversedFrames;
        SCode receiver;
        ControlPoint cp;

        internal RewindState (ControlPoint cp, SCode receiver)
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

        internal SCode PopFrame ()
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
        public override bool ArgumentValue (out object value, int offset)
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

        internal override Environment Extend (IClosure closure)
        {
            throw new NotImplementedException ();
        }

        internal override Environment Extend (IClosure closure, object [] args)
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

        internal override bool Unreferenceable (string name)
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
    }
}

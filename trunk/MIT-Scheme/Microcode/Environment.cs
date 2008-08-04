using System;
using System.Collections.Generic;
using System.Diagnostics;

namespace Microcode
{
    enum LookupDisposition
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

    abstract class Environment
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        static Environment global;

        public static Environment Global
        {
            [DebuggerStepThrough]
            get
            {
                if (global == null)
                    global = new TopLevelEnvironment ();
                return global;
            }
        }

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

        static protected int [] foundAtDepth = new int [128];
        static protected int [] foundInAuxes = new int [128];

        public abstract void AddBinding (string name);
        public abstract LookupDisposition AssignVariable (string name, object value, out object oldValue);
        public abstract LookupDisposition DefineVariable (string name, object value);
        public abstract object SafeLookup (string name);
        public abstract LookupDisposition LookupVariable (int depth, string name, out object value);
        public abstract object LookupType (string name);
        public abstract object Argument (int offset);
        public abstract ValueCell LexicalLoop (string name, int frame, int offset);
        public abstract object Lexical (string name, int frame, int offset);
        public abstract ValueCell ValueCell (string name);
        public abstract void LinkVariable (string name, ValueCell cell);
        //public bool IsUnassigned (string name)
        //{
        //    ValueCell vcell = ValueCell (name);
        //    return !vcell.isAssigned;
        //}

        //public bool IsUnbound (string name)
        //{
        //    ValueCell vcell = ValueCell (name);
        //    return vcell == null;
        //}

        [SchemePrimitive ("LEXICAL-UNREFERENCEABLE?", 2)]
        public static object IsLexicalUnreferenceable (Interpreter interpreter, object env, object name)
        {
            object dummy;
            switch (((Environment) env).LookupVariable (0, (string) name, out dummy)) {
                case LookupDisposition.OK:
                    return interpreter.Return (false);
                case LookupDisposition.Unassigned:
                case LookupDisposition.Unbound:
                    return interpreter.Return (true);
                default:
                    throw new NotImplementedException ();
            }
        }

        [SchemePrimitive ("LEXICAL-ASSIGNMENT", 3)]
        public static object LexicalAssignment (Interpreter interpreter, object aenv, object aname, object value)
        {
            Environment env = (Environment) aenv;
            string name = (string) aname;
            object oldValue;
            LookupDisposition disp = env.AssignVariable (name, value, out oldValue);
            if (disp == LookupDisposition.OK)
                return interpreter.Return (oldValue);
            throw new NotImplementedException();
        }

        [SchemePrimitive ("LEXICAL-UNASSIGNED?", 2)]
        public static object LexicalUnassigned (Interpreter interpreter, object envobj, object nameobj)
        {
            throw new NotImplementedException ();
            //Environment env = (Environment) envobj;
            //string name = (string) nameobj;
            ////bool unassignedp = env.IsUnassigned (name);

            //return interpreter.Return (unassignedp);
        }

        [SchemePrimitive ("LEXICAL-UNBOUND?", 2)]
        public static object LexicalBound (Interpreter interpreter, object env, object name)
        {
            throw new NotImplementedException ();
        }

        [SchemePrimitive ("LEXICAL-REFERENCE", 2)]
        public static object LexicalReference (Interpreter interpreter, object env, object name)
        {
            object value;
            LookupDisposition disp = ((Environment) env).LookupVariable (0, (string) name, out value);
            if (disp != LookupDisposition.OK)
                throw new NotImplementedException();
            return interpreter.Return (value);
        }

        [SchemePrimitive("SAFE-LEXICAL-REFERENCE", 2)]
        public static object SafeLexicalReference(Interpreter interpreter, object env, object name)
        {
            object value;
            LookupDisposition disp = ToEnvironment (env).LookupVariable (0, (string) name, out value);
            if (disp == LookupDisposition.Unassigned)
                return interpreter.Return (Constant.ExternalUnassigned);
            else
                return interpreter.Return (value);
        }

        [SchemePrimitive ("LEXICAL-REFERENCE-TYPE", 2)]
        public static object LexicalReferenceType (Interpreter interpreter, object env, object name)
        {
            object dummy;
            switch (ToEnvironment(env).LookupVariable (0, (string) name, out dummy)) {
                case LookupDisposition.OK:
                    return interpreter.Return (2);
                case LookupDisposition.Macro:
                    return interpreter.Return (3);
                case LookupDisposition.Unassigned:
                    return interpreter.Return (1);
                case LookupDisposition.Unbound:
                    return interpreter.Return (0);
                default:
                    throw new NotImplementedException ();
            }
        }

        //static bool hacked = false;
        [SchemePrimitive ("LINK-VARIABLES", 4)]
        public static object LinkVariables (Interpreter interpreter, object [] arglist)
        {
            Environment target_env;
            if (arglist [0] is bool && (bool) arglist [0] == false)
                target_env = Environment.Global;
            else
                target_env = (Environment) arglist [0];
            

            string target_name = (string) arglist [1];
            Environment source_env = (Environment) arglist [2];
            string source_name = (string) arglist [3];
            target_env.LinkVariable (target_name, source_env.ValueCell (source_name));
            return interpreter.Return (null);
        }

        [SchemePrimitive ("LOCAL-ASSIGNMENT", 3)]
        public static object LocalAssignment (Interpreter interpreter, object aenv, object aname, object value)
        {
            Environment env;
            if (aenv is bool && (bool) aenv == false)
                env = Environment.Global;
            else
                env = (Environment) aenv;
            string name = (string) aname;
            LookupDisposition disp = env.DefineVariable (name, value);
            if (disp != LookupDisposition.OK)
                throw new NotImplementedException ();
            return interpreter.Return (name);
        }

        [SchemePrimitive ("ENVIRONMENT?", 1)]
        public static object IsEnvironment (Interpreter interpreter, object arg)
        {
            return interpreter.Return (arg is Environment || (arg is bool && (bool) arg == false));
        }
    }

    class NullEnvironment : Environment
    {
        public override void AddBinding (string name)
        {
            throw new NotImplementedException ();
        }

        public override LookupDisposition AssignVariable (string name, object value, out object oldValue)
        {
            throw new NotImplementedException ();
        }

        public override LookupDisposition DefineVariable (string name, object value)
        {
            throw new NotImplementedException ();
        }

        public override LookupDisposition LookupVariable (int depth, string name, out object value)
        {
            throw new NotImplementedException ();
        }

        public override object LookupType (string name)
        {
            throw new NotImplementedException ();
        }

        public override object SafeLookup (string name)
        {
            throw new NotImplementedException ();
        }

        public override object Argument (int offset)
        {
            throw new NotImplementedException ();
        }

        public override ValueCell LexicalLoop (string name, int frame, int offset)
        {
            throw new NotImplementedException ();
        }

        public override object Lexical (string name, int frame, int offset)
        {
            throw new NotImplementedException ();
        }

        public override ValueCell ValueCell (string name)
        {
            throw new NotImplementedException ();
        }

        public override void LinkVariable (string name, ValueCell cell)
        {
            throw new NotImplementedException ();
        }
    }

    class TopLevelEnvironment : Environment
    {
        Dictionary<string, ValueCell> bindings = new Dictionary<string, ValueCell> ();

        public override void AddBinding (string name)
        {
            ValueCell cell = null;
            if (this.bindings.TryGetValue (name, out cell) == true)
                return;
            else
                bindings.Add (name, new ValueCell ());
        }

        public override ValueCell ValueCell (string name)
        {
            ValueCell cell = null;
            this.bindings.TryGetValue (name, out cell);
            return cell;
        }

        public override void LinkVariable (string name, ValueCell cell)
        {
            bindings.Add (name, cell);
        }

        public override LookupDisposition AssignVariable (string name, object value, out object oldValue)
        {
            ValueCell cell = null;
            if (this.bindings.TryGetValue (name, out cell) == true) {
                oldValue = cell.Assign (value);
                return LookupDisposition.OK;
             
            }
            else
                throw new NotImplementedException ();

            //    if (cell.isAssigned) {
            //        oldValue = cell.Assign (value);
            //        return LookupDisposition.OK;
            //    }
            //    else
            //        throw new NotImplementedException ();
            //}
            //else
            //    throw new NotImplementedException ();
        }

        public override LookupDisposition DefineVariable (string name, object value)
        {
            ValueCell cell = null;
            if (this.bindings.TryGetValue (name, out cell) == false)
                {
                cell = new ValueCell ();
                this.bindings.Add (name, cell);

            }        
            cell.Assign (value);
            return LookupDisposition.OK;
        }


        public override LookupDisposition LookupVariable (int depth, string name, out object value)
        {
            ValueCell cell = null;
            if (this.bindings.TryGetValue (name, out cell) == true) {
                foundAtDepth [depth] += 1;
                object temp = cell.Value;
                switch (ReferenceTrap.GetTrapKind (temp)) {
                    case TrapKind.NON_TRAP_KIND:
                        value = temp;
                        return LookupDisposition.OK;

                    case TrapKind.TRAP_UNASSIGNED:
                        value = null;
                        return LookupDisposition.Unassigned;

                    case TrapKind.TRAP_UNBOUND:
                        value = null;
                        return LookupDisposition.Unbound;

                    case TrapKind.TRAP_MACRO:
                        value = temp;
                        return LookupDisposition.Macro;

                    case TrapKind.TRAP_COMPILER_CACHED:
                        throw new NotImplementedException ();

                    default:
                        throw new NotImplementedException ();
                }


            }
            else {
                value = null;
                return LookupDisposition.Unbound;
            }
        }

        public override object SafeLookup (string name)
        {
            ValueCell cell = null;
            if (this.bindings.TryGetValue (name, out cell) == true)
                return cell.Value;
            throw new NotImplementedException ();
        }

        public override object LookupType (string name)
        {
            throw new NotImplementedException ();
            //ValueCell cell = null;
            //if (this.bindings.TryGetValue (name, out cell) == true)
            //{
            //    if (cell.isAssigned) {
 
            //            return ReferenceType.Normal;
            //    }
            //    else
            //        return ReferenceType.Unassigned;
            //}
            //else
            //    return ReferenceType.Unbound;

        }

        public override object Argument (int offset)
        {
            throw new NotImplementedException ();
        }

        public override ValueCell LexicalLoop (string name, int frame, int offset)
        {
            throw new NotImplementedException ();
        }

        public override object Lexical (string name, int frame, int offset)
        {
            throw new NotImplementedException ();
        }
    }

    class InterpreterEnvironment : Environment
    {
        IClosure closure;
        ValueCell [] framevector;
        Dictionary<string, ValueCell> incrementals;

        public InterpreterEnvironment (IClosure closure, object [] framevector)
        {
            this.closure = closure;
            this.framevector = new ValueCell [framevector.Length];

            for (int i = 0; i < framevector.Length; i++)
            {
                this.framevector [i] = new ValueCell (framevector [i]);
            }
        }

        public InterpreterEnvironment (object [] framevector)
        {
            this.closure = (Closure) (framevector [0]);
            this.framevector = new ValueCell [framevector.Length - 1];

            for (int i = 0; i < framevector.Length - 1; i++)
            {
                this.framevector [i] = new ValueCell ();
                this.framevector [i].Assign (framevector [i + 1]);
            }
        }

        public override ValueCell ValueCell (string name)
        {
            int offset = this.closure.FormalOffset (name);
            if (offset == -1)
            {
                ValueCell vcell = null;
                if (incrementals != null && incrementals.TryGetValue (name, out vcell))
                {
                    return vcell;
                }
                return Environment.ToEnvironment(this.Parent).ValueCell (name);
            }
            return this.framevector [offset];
        }

        public override void AddBinding (string name)
        {
            int offset = this.closure.FormalOffset (name);
            if (offset == -1)
            {
                if (incrementals == null)
                    incrementals = new Dictionary<string, ValueCell> ();
                if (!incrementals.ContainsKey (name))
                    incrementals.Add (name, new ValueCell ());
            }
        }

        public override void LinkVariable (string name, ValueCell cell)
        {
            int offset = this.closure.FormalOffset (name);
            if (offset == -1)
            {
                if (incrementals == null)
                    incrementals = new Dictionary<string, ValueCell> ();
                incrementals.Add (name, cell);
            }
            else
                this.framevector [offset] = cell;
        }

        public override LookupDisposition AssignVariable (string name, object value, out object oldValue)
        {
            int offset = this.closure.FormalOffset (name);
            if (offset == -1)
            {
                ValueCell vcell = null;
                if (incrementals != null && incrementals.TryGetValue (name, out vcell))
                {
                    oldValue = vcell.Assign (value);
                    return LookupDisposition.OK;
                }
                else
                    return Environment.ToEnvironment(this.Parent).AssignVariable (name, value, out oldValue);
            }
            ValueCell avcell = framevector [offset];
            oldValue = avcell.Assign (value);
            return LookupDisposition.OK;
        }

        public override LookupDisposition DefineVariable (string name, object value)
        {
            int offset = this.closure.FormalOffset (name);
            if (offset == -1) {
                ValueCell vcell = null;
                if (incrementals != null && incrementals.TryGetValue (name, out vcell)) {
                    vcell.Assign (value);
                }
                else {
                    vcell = new ValueCell ();
                    if (incrementals == null)
                        incrementals = new Dictionary<string, ValueCell> ();
                    incrementals.Add (name, vcell);
                    vcell.Assign (value);
                }
            }
            else framevector [offset].Assign (value);
            return LookupDisposition.OK;
        }

        public override LookupDisposition LookupVariable (int depth, string name, out object value)
        {
            int offset = this.closure.FormalOffset (name);
            if (offset == -1) {
                ValueCell vcell = null;
                if (incrementals != null && incrementals.TryGetValue (name, out vcell)) {
                    foundInAuxes [depth] += 1;
                    object temp = vcell.Value;
                    switch (ReferenceTrap.GetTrapKind (temp)) {
                        case TrapKind.NON_TRAP_KIND:
                            value = temp;
                            return LookupDisposition.OK;

                        case TrapKind.TRAP_UNASSIGNED:
                            value = null;
                            throw new NotImplementedException ();

                        case TrapKind.TRAP_UNBOUND:
                            value = null;
                            throw new NotImplementedException ();

                        case TrapKind.TRAP_MACRO:
                            value = temp;
                            throw new NotImplementedException ();

                        case TrapKind.TRAP_COMPILER_CACHED:
                            throw new NotImplementedException ();

                        default:
                            throw new NotImplementedException ();
                    }
                }
                else
                    return Environment.ToEnvironment (this.Parent).LookupVariable (depth + 1, name, out value);
            }
            else {
                foundAtDepth [depth] += 1;
                ValueCell vcell = framevector [offset];
                object temp = vcell.Value;
                switch (ReferenceTrap.GetTrapKind (temp)) {
                    case TrapKind.NON_TRAP_KIND:
                        value = temp;
                        return LookupDisposition.OK;

                    case TrapKind.TRAP_UNASSIGNED:
                        value = null;
                        return LookupDisposition.Unassigned;

                    case TrapKind.TRAP_UNBOUND:
                        value = null;
                        return LookupDisposition.Unbound;

                    case TrapKind.TRAP_MACRO:
                        value = temp;
                        return LookupDisposition.Macro;

                    case TrapKind.TRAP_COMPILER_CACHED:
                        throw new NotImplementedException ();

                    default:
                        throw new NotImplementedException ();
                }
            }
        }

        public override object SafeLookup (string name)
        {
            int offset = this.closure.FormalOffset (name);
            if (offset == -1) {
                ValueCell vcell = null;
                if (incrementals != null && incrementals.TryGetValue (name, out vcell))
                    return vcell.Value;
                else
                    return Environment.ToEnvironment (this.Parent).SafeLookup (name);
            }
            else
                return framevector [offset].Value;
        }

        public override object LookupType (string name)
        {
            throw new NotImplementedException ();
            //int offset = this.closure.FormalOffset (name);
            //if (offset == -1)
            //{
            //    ValueCell vcell = null;
            //    if (incrementals != null && incrementals.TryGetValue (name, out vcell))
            //    {
            //        if (vcell.isAssigned)
            //            return (int) ReferenceType.Normal;
            //        else
            //            return (int) ReferenceType.Unassigned;
            //    }
            //    else
            //        return Environment.ToEnvironment(this.Parent).LookupType (name);
            //}
            //else
            //    return (int) ReferenceType.Normal;
        }

        // Variant of Lookup.  If variable is bound in the nearest environment,
        // it cannot be shadowed.  Just grab it.
        public override object Argument (int offset)
        {
            return framevector [offset].Value;
        }

        public override ValueCell LexicalLoop (string name, int frame, int offset)
        {
            if (frame == 0)
                return framevector [offset];
            ValueCell vcell = null;
            if (incrementals != null &&
                incrementals.TryGetValue (name, out vcell))
                return vcell;
            else
                return ToEnvironment(this.Parent).LexicalLoop (name, frame - 1, offset);
        }

        // Variant of Lookup.  If variable is *not* bound in the nearest environment
        // no need to scan the nearest.
        public override object Lexical (string name, int frame, int offset)
        {
            ValueCell vcell = LexicalLoop (name, frame, offset);
            return vcell.Value;
        }

        public object Parent
        {
            [DebuggerStepThrough]
            get
            {
                return this.closure.Environment;
            }
        }
    }
}

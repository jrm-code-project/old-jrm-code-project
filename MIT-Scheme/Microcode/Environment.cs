using System;
using System.Collections.Generic;
using System.Diagnostics;

namespace Microcode
{
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

        static public Environment ToEnvironment (object env)
        {
            if (env is bool && (bool) env == false)
                return Environment.Global;
            else
                return (Environment) env;
        }

        public abstract void AddBinding (string name);
        public abstract object Assign (string name, object value);
        public abstract object LocalAssign (string name, object value);
        public abstract object Lookup (string name);
        public abstract object LookupType (string name);
        public abstract object Argument (int offset);
        public abstract ValueCell LexicalLoop (string name, int frame, int offset);
        public abstract object Lexical (string name, int frame, int offset);
        public abstract ValueCell ValueCell (string name);
        public abstract void LinkVariable (string name, ValueCell cell);
        public bool IsUnassigned (string name)
        {
            ValueCell vcell = ValueCell (name);
            return !vcell.isAssigned;
        }

        public bool IsUnbound (string name)
        {
            ValueCell vcell = ValueCell (name);
            return vcell == null;
        }

        [SchemePrimitive ("LEXICAL-UNREFERENCEABLE?", 2)]
        public static object IsLexicalUnreferenceable (Interpreter interpreter, object env, object name)
        {
            ValueCell vcell = ((Environment) env).ValueCell ((string) name);
            return interpreter.Return (vcell == null || !vcell.isAssigned);
        }

        [SchemePrimitive ("LEXICAL-ASSIGNMENT", 3)]
        public static object LexicalAssignment (Interpreter interpreter, object aenv, object aname, object value)
        {
            Environment env = (Environment) aenv;
            string name = (string) aname;
            return interpreter.Return (env.Assign (name, value));
        }

        [SchemePrimitive ("LEXICAL-UNASSIGNED?", 2)]
        public static object LexicalUnassigned (Interpreter interpreter, object envobj, object nameobj)
        {
            Environment env = (Environment) envobj;
            string name = (string) nameobj;
            bool unassignedp = env.IsUnassigned (name);

            return interpreter.Return (unassignedp);
        }

        [SchemePrimitive ("LEXICAL-UNBOUND?", 2)]
        public static object LexicalBound (Interpreter interpreter, object env, object name)
        {
            throw new NotImplementedException ();
        }

        [SchemePrimitive ("LEXICAL-REFERENCE", 2)]
        public static object LexicalReference (Interpreter interpreter, object env, object name)
        {
            return interpreter.Return (((Environment) env).Lookup ((string) name));
        }

        [SchemePrimitive ("LEXICAL-REFERENCE-TYPE", 2)]
        public static object LexicalReferenceType (Interpreter interpreter, object env, object name)
        {
            return interpreter.Return (((Environment) env).LookupType ((string) name));
        }

        static bool hacked = false;
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
            return interpreter.Return (env.LocalAssign (name, value));
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

        public override object Assign (string name, object value)
        {
            throw new NotImplementedException ();
        }

        public override object LocalAssign (string name, object value)
        {
            throw new NotImplementedException ();
        }

        public override object Lookup (string name)
        {
            throw new NotImplementedException ();
        }

        public override object LookupType (string name)
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

        public override object Assign (string name, object value)
        {
            ValueCell cell = null;
            if (this.bindings.TryGetValue (name, out cell) == true)
                return cell.Assign (value);
            throw new NotImplementedException ();
        }

        public override object LocalAssign (string name, object value)
        {
            ValueCell cell = null;
            if (this.bindings.TryGetValue (name, out cell) == true)
                return cell.Assign (value);
            else
            {
                cell = new ValueCell ();
                this.bindings.Add (name, cell);
                return cell.Assign (value);
            }
        }

        public override object Lookup (string name)
        {
            ValueCell cell = null;
            if (this.bindings.TryGetValue (name, out cell) == true)
                return cell.Value;
            throw new NotImplementedException ();
        }

        public override object LookupType (string name)
        {
            ValueCell cell = null;
            if (this.bindings.TryGetValue (name, out cell) == true)
            {
                if (cell.isAssigned)
                    return ReferenceType.Normal;
                else
                    return ReferenceType.Unassigned;
            }
            else
                return ReferenceType.Unbound;

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
        Closure closure;
        ValueCell [] framevector;
        Dictionary<string, ValueCell> incrementals;

        public InterpreterEnvironment (Closure closure, object [] framevector)
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

        public override object Assign (string name, object value)
        {
            int offset = this.closure.FormalOffset (name);
            if (offset == -1)
            {
                ValueCell vcell = null;
                if (incrementals != null && incrementals.TryGetValue (name, out vcell))
                {
                    return vcell.Assign (value);
                }
                else
                    return Environment.ToEnvironment(this.Parent).Assign (name, value);
            }
            return framevector [offset].Assign (value);
        }

        public override object LocalAssign (string name, object value)
        {
            int offset = this.closure.FormalOffset (name);
            if (offset == -1)
            {
                ValueCell vcell = null;
                if (incrementals != null && incrementals.TryGetValue (name, out vcell))
                {
                    return vcell.Assign (value);
                }
                else
                {
                    vcell = new ValueCell ();
                    if (incrementals == null)
                        incrementals = new Dictionary<string, ValueCell> ();
                    incrementals.Add (name, vcell);
                    return vcell.Assign (value);
                }
            }
            return framevector [offset].Assign (value);
        }

        public override object Lookup (string name)
        {
            int offset = this.closure.FormalOffset (name);
            if (offset == -1)
            {
                ValueCell vcell = null;
                if (incrementals != null && incrementals.TryGetValue (name, out vcell))
                    return vcell.Value;
                else
                    return Environment.ToEnvironment(this.Parent).Lookup (name);
            }
            else
                return framevector [offset].Value;
        }

        public override object LookupType (string name)
        {
            int offset = this.closure.FormalOffset (name);
            if (offset == -1)
            {
                ValueCell vcell = null;
                if (incrementals != null && incrementals.TryGetValue (name, out vcell))
                {
                    if (vcell.isAssigned)
                        return (int) ReferenceType.Normal;
                    else
                        return (int) ReferenceType.Unassigned;
                }
                else
                    return Environment.ToEnvironment(this.Parent).LookupType (name);
            }
            else
                return (int) ReferenceType.Normal;
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

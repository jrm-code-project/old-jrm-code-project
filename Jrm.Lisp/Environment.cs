using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Lisp
{
    class Environment
    {
        public enum Namespace
        {
            Function,
            Variable
        }

        readonly Dictionary<Symbol, ILocation<Delegate>> functions = new Dictionary<Symbol, ILocation<Delegate>> ();
        readonly Dictionary<Symbol, ILocation<object>> variables = new Dictionary<Symbol, ILocation<object>> ();
        static Environment defaultEnvironment = new Environment ();

        public static Environment Default
        {
            get
            {
                return defaultEnvironment;
            }
        }

        public void Bind (Symbol id, Namespace nspace, ILocation<Object> location) {
            switch (nspace)
            {
                case Namespace.Variable:
                    variables.Add(id, location);
                    break;
                default:
                    throw new NotImplementedException ();
            }
        }
        
        public bool IsBound (Symbol id)
        {
            return variables.ContainsKey (id);
        }

        public bool IsFbound (Symbol id)
        {
            return functions.ContainsKey (id);
        }

        public object Setq (Symbol id, object value)
        {
            ILocation<object> binding;
            if (!variables.TryGetValue (id, out binding))
                Bind (id, Namespace.Variable, new ValueCell<object> (value));
            else
                binding.Value = value;
            return value;
        }

        public object SymbolValue (Symbol id)
        {
            ILocation<object> binding;
            if (variables.TryGetValue (id, out binding))
                return binding.Value;
            else
                throw new NotImplementedException ();
        }

        public Delegate SymbolFunction (Symbol id)
        {
            ILocation<Delegate> binding;
            if (functions.TryGetValue (id, out binding))
                return binding.Value;
            if (id.NamesDotnetMethod ()) {
                Delegate gf = (Delegate) CLOS.EnsureGenericFunction (id, KW.Environment, this);
                binding = new ValueCell<Delegate> (gf);
                return binding.Value;
            }
            else
                throw new NotImplementedException ();
        }

    }
}

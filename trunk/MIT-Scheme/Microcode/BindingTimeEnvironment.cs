using System;
using System.Collections.Generic;
using System.Text;

namespace Microcode
{
    public class VariableInfo
    {
    }

    public class FreeVariableInfo : VariableInfo
    {
        Environment bindingEnvironment;

        public FreeVariableInfo (Environment bindingEnvironment)
        {
            this.bindingEnvironment = bindingEnvironment;
        }

        public Environment Environment
        {
            get
            {
                return this.bindingEnvironment;
            }
        }
    }

    public class GlobalVariableInfo : VariableInfo
    {
        Environment bindingEnvironment;

        public GlobalVariableInfo (Environment bindingEnvironment)
        {
            this.bindingEnvironment = bindingEnvironment;
        }

        public Environment Environment
        {
            get
            {
                return this.bindingEnvironment;
            }
        }
    }

    public class TopLevelVariableInfo : VariableInfo
    {
        ValueCell cell;

        public TopLevelVariableInfo (ValueCell binding)
        {
            this.cell = binding;
        }

        public ValueCell ValueCell
        {
            get
            {
                return this.cell;
            }
        }
    }

    public class LexicalVariableInfo : VariableInfo
    {
        int depth;
        int offset;
        ILambda binder;
        public LexicalVariableInfo (int depth, int offset, ILambda binder)
        {
            this.depth = depth;
            this.offset = offset;
            this.binder = binder;
        }

        public int Depth { get { return this.depth; } }
        public int Offset { get { return this.offset; } }
        public ILambda Binder { get { return this.binder; } }
    }

    public abstract class BindingTimeEnvironment
    {
        public BindingTimeEnvironment Extend (ILambda lambda)
        {
            return new LambdaBindingEnvironment (lambda, this);
        }

        public abstract VariableInfo Lookup (string name);
        public abstract VariableInfo Lookup (string name, int depth);
    }

    public class RootBindingEnvironment : BindingTimeEnvironment
    {
        Environment bindingEnvironment;
        Dictionary<object, VariableInfo> infoCache;

        public RootBindingEnvironment (Environment bindingEnvironment)
        {
            this.bindingEnvironment = bindingEnvironment;
            this.infoCache = new Dictionary<object, VariableInfo> ();
        }

        public override VariableInfo Lookup (string name)
        {
            return Lookup (name, 0);
        }

        public override VariableInfo Lookup (string name, int depth)
        {
            VariableInfo answer;
            if (infoCache.TryGetValue (name, out answer)) return answer;
            IClosure closure = this.bindingEnvironment.Closure;
            if (closure == null) // global environment
                answer = new GlobalVariableInfo (bindingEnvironment);
            else {
                int offset = closure.FormalOffset (name);
                if (offset == -1)
                    answer = new FreeVariableInfo (bindingEnvironment);
                else
                    answer = new TopLevelVariableInfo (bindingEnvironment.GetValueCell (name));
            }
            infoCache.Add (name, answer);
            return answer;
        }
    }

    public class LambdaBindingEnvironment : BindingTimeEnvironment
    {
        ILambda lambda;
        string [] formals;
        BindingTimeEnvironment parent;

        public LambdaBindingEnvironment (ILambda lambda, BindingTimeEnvironment parent)
        {
            this.lambda = lambda;
            this.formals = lambda.Formals;
            this.parent = parent;
        }

        public override VariableInfo Lookup (string name)
        {
            return Lookup (name, 0);
        }

        public override VariableInfo Lookup (string name, int depth)
        {
            VariableInfo answer;
            int offset = this.lambda.LexicalOffset (name);
            if (offset == -1) {
                answer = parent.Lookup (name, depth + 1);
            }
            else
                answer = new LexicalVariableInfo (depth, offset, this.lambda);
            return answer;
        }
    }
}

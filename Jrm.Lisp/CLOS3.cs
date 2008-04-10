using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Reflection;

namespace Lisp
{
    delegate bool MethodCompare3 (StandardInstance left, StandardInstance right, Cons arglist);
    public delegate object MethodStep (NextMethodFunction callNextMethod, params object [] arguments);
    public delegate object NextMethodFunction (params object [] arguments);
    public delegate object SlotChangingFunction (object currentValue);
    public delegate T SlotChangingFunction<T> (T currentValue);

    class EffectiveMethodCache
    {
        Cons table;

        static bool Equal (Cons left, Cons right)
        {
            if ((left == null) && (right == null))
                return true;
            if (left == null)
                return false;
            if (right == null)
                return false;
            if (left.Car.Equals (right.Car))
                return Equal ((Cons) left.Cdr, (Cons) right.Cdr);
            else
                return false;
        }

        static Cons Assoc (Cons key, Cons table)
        {
            if (table == null)
                return null;
            Cons entry = (Cons) table.Car;
            if (Equal (key, (Cons) entry.Car))
                return entry;
            else {
                return Assoc (key, (Cons) table.Cdr);
            }
        }

        static Cons standinForNull = new Cons (null, null);
        public EffectiveMethodCache () {
        }

        internal bool TryGetValue (Cons key, out NextMethodFunction value)
        {
            Cons probe = Assoc (key == null ? standinForNull : key, table);
            if (probe == null) {
                value = null;
                return false;
            }
            else {
                value = (NextMethodFunction) probe.Cdr;
                return true;
            }
        }

        internal void Add (object key, NextMethodFunction value)
        {
            // will shadow
            table = new Cons (new Cons (key, value), table);
        }

        internal bool ContainsKey (object key)
        {
            throw new NotImplementedException ("MethodCache::ContainsKey");
        }
    }

    class GroundMethodFunction
    {
        Delegate del;

        GroundMethodFunction (Delegate del)
        {
            this.del = del;
        }

        object Funcall1r (params object [] arguments)
        {
           return del.DynamicInvoke (null, arguments);
        }

        object Funcall2 (params object [] arguments)
        {
            if (arguments.Length != 1)
                throw new NotImplementedException ();
            return this.del.DynamicInvoke (null, arguments [0]);
        }

        object Funcall2r (params object [] arguments)
        {
            object [] newArguments = new object [arguments.Length - 1];
            Array.Copy (arguments, 1, newArguments, 0, newArguments.Length);
            return this.del.DynamicInvoke (null, arguments [0], newArguments);
        }

        object Funcall3 (params object [] arguments)
        {
            if (arguments.Length != 2)
                throw new NotImplementedException ();
            return this.del.DynamicInvoke (null, arguments [0], arguments [1]);
        }

        static MethodInfo GetFuncallMethod (Delegate del)
        {
            ParameterInfo [] parameters = del.Method.GetParameters ();
            ParameterInfo lastParameter = parameters [parameters.Length - 1];
            object [] attr = lastParameter.GetCustomAttributes (typeof (System.ParamArrayAttribute), false);
            string funcall = "Funcall" + (parameters.Length - ((attr.Length > 0) ? 1 : 0)).ToString () + ((attr.Length > 0) ? "r" : "");
            MethodInfo mi =
            typeof (GroundMethodFunction)
                .GetMethod (funcall, BindingFlags.NonPublic | BindingFlags.Instance);
            if (mi == null)
                throw new NotImplementedException (funcall);
            return mi;
        }

        static public NextMethodFunction Create (Delegate del)
        {
            GroundMethodFunction gmf = new GroundMethodFunction (del);
            return (NextMethodFunction) Delegate.CreateDelegate (typeof (NextMethodFunction), gmf, GetFuncallMethod(del));
        }
    }

    class MethodStepper
    {
        StandardInstance generic;

        public MethodStepper (StandardInstance generic)
        {
            this.generic = generic;
        }

        public NextMethodFunction Step (Cons primaries, object [] arguments)
        {
            return MethodStepper1.Create (this, generic, primaries, arguments);
        }
    }

    class MethodStepper1
    {
        MethodStepper outer;
        StandardInstance generic;
        Cons primaries;
        object [] args;

        MethodStepper1 (MethodStepper outer, StandardInstance generic, Cons primaries, object [] args)
        {
            this.outer = outer;
            this.generic = generic;
            this.primaries = primaries;
            this.args = args;
        }

        public object Funcall (params object [] newargs)
        {
            object [] nextArgs = newargs.Length == 0 ? this.args : newargs;
            Delegate meth = (Delegate) CL.Cdar (this.primaries);
            NextMethodFunction nmf =
                (primaries.Cdr == null)
                ? NoNextMethod.Create (this.generic, CL.Caar (this.primaries), nextArgs)
                : outer.Step ((Cons) primaries.Cdr, nextArgs);
            return meth.DynamicInvoke (nmf, nextArgs);
        }

        static public NextMethodFunction Create (MethodStepper outer, StandardInstance generic, Cons primaries, object [] args)
        {
            return new NextMethodFunction (new MethodStepper1 (outer, generic, primaries, args).Funcall);
        }
    }

    public class MethodStepWrapper
    {

        Delegate del;

        public MethodStepWrapper (Delegate del)
        {
            this.del = del;
        }

        object Funcall2 (NextMethodFunction nmf, object [] arguments)
        {
            return this.del.DynamicInvoke (nmf, arguments [0]);
        }

        object Funcall2r (NextMethodFunction nmf, object [] arguments)
        {
            object [] remaining = new object [arguments.Length - 1];
            Array.Copy(arguments,1,remaining,0,arguments.Length-1);
            return this.del.DynamicInvoke (nmf, arguments [0], remaining);
        }

        object Funcall3 (NextMethodFunction nmf, object [] arguments)
        {
            if (arguments.Length != 2)
                throw new NotImplementedException ("wrong number of arguments");
            return this.del.DynamicInvoke (nmf, arguments [0], arguments [1]);
        }

        object Funcall3r (NextMethodFunction nmf, object [] arguments)
        {
            object [] remaining = new object [arguments.Length - 2];
            Array.Copy (arguments, 2, remaining, 0, arguments.Length - 2);
            return this.del.DynamicInvoke (nmf, arguments [0], arguments [1], remaining);
        }

        static MethodInfo GetFuncallMethod (Delegate del)
        {
            ParameterInfo [] parameters = del.Method.GetParameters ();
            ParameterInfo lastParameter = parameters [parameters.Length - 1];
            object [] attr = lastParameter.GetCustomAttributes (typeof (System.ParamArrayAttribute), false);
            string funcall = "Funcall" + (parameters.Length - ((attr.Length > 0) ? 1 : 0)).ToString () + ((attr.Length > 0) ? "r" : "");
            MethodInfo mi =
            typeof (MethodStepWrapper)
                .GetMethod (funcall, BindingFlags.NonPublic | BindingFlags.Instance);
            if (mi == null)
                throw new NotImplementedException (funcall);
            return mi;
        }

        public static MethodStep Create (Delegate del)
        {
            return (MethodStep)
                Delegate.CreateDelegate (typeof (MethodStep),
                                            new MethodStepWrapper (del),
                                            GetFuncallMethod (del)
                                            );
        }
    }

    class NoNextMethod
    {
        StandardInstance generic;
        object info;
        object [] args;

        NoNextMethod (StandardInstance generic, object info, object [] args)
        {
            this.generic = generic;
            this.info = info;
            this.args = args;
        }

        object Funcall (params object [] args)
        {
            throw new NotImplementedException ("No Next Method");
        }

        static public NextMethodFunction Create (StandardInstance generic, object info, object [] args)
        {
            NoNextMethod nnm = new NoNextMethod (generic, info, args);
            return (NextMethodFunction) Delegate.CreateDelegate (typeof (NextMethodFunction), nnm, typeof (NoNextMethod)
                .GetMethod ("Funcall", BindingFlags.NonPublic | BindingFlags.Instance));
        }
    }

    class StandardDiscriminatingFunction
    {
        StandardInstance genericFunction;

        StandardDiscriminatingFunction (StandardInstance gf)
        {
            this.genericFunction = gf;
        }

        static object last (object l)
        {
            Cons lp = l as Cons;
            return (lp.Cdr == null)
                ? lp.Car
                : last (lp.Cdr);
        }

        static NextMethodFunction computeCallable (bool isGroundCase, StandardInstance generic, EffectiveMethodCache cache, Cons keys, object [] arguments)
        {
            NextMethodFunction answer;
            if (isGroundCase) {
                Delegate m = (Delegate) CLOS.MethodFunction ((StandardInstance) last (CLOS.GenericFunctionMethods (generic)));
                answer = (NextMethodFunction) GroundMethodFunction.Create (m);
            }
            else {
                object [] newArguments = new object [arguments.Length + 1];
                arguments.CopyTo (newArguments, 1);
                newArguments [0] = generic;
                answer = (NextMethodFunction) CLOS.ComputeEffectiveMethod (generic, CLOS.ComputeApplicableMethods (newArguments));
            }
            cache.Add (keys, answer);
            return answer;
        }

        static Cons getKeysLoop (object [] arguments, int argindex, ConsList<ICollection<object>> singletons, Cons keys)
        {
            if ((singletons == null) || (argindex >= arguments.Length))
                return keys;
            ICollection<object> thisTable = singletons.Car;
            object thisArg = arguments [argindex];
            object thisKey;
            if (thisTable != null && thisTable.Contains (thisArg))
                thisKey = thisArg;
            else
                thisKey = CLOS.ClassOf (thisArg).SerialNumber ();
            return getKeysLoop (arguments, argindex + 1, singletons.Cdr, new Cons (thisKey, keys));
        }

        static Cons getKeys (object [] arguments, ConsList<ICollection<object>> singletons)
        {
            return (Cons) CL.Reverse (getKeysLoop (arguments, 0, singletons, null));
        }

        object Funcall (StandardInstance self, object [] arguments)
        {
            ConsList<ICollection<object>> singletons = (ConsList<ICollection<object>>) CLOS.genericFunctionSingletonsList (genericFunction);
            Cons keys = getKeys (arguments, singletons);

            Cons appCache   = (Cons) CLOS.genericFunctionApplicationCache (genericFunction);
            // Cons lambdaList = (Cons) CLOS.GenericFunctionLambdaList (genericFunction);
            // should do arity check here

            // check if we need to flush the cache
            if (!object.ReferenceEquals (appCache.Car, CLOS.GenericApplicationCacheTag)) {
                appCache = new Cons (CLOS.GenericApplicationCacheTag, new EffectiveMethodCache ());
                CLOS.setGenericFunctionApplicationCache (self, appCache);
            }

            EffectiveMethodCache cache = (EffectiveMethodCache) appCache.Cdr;
            NextMethodFunction ah;
            if (!cache.TryGetValue (keys, out ah)) {
                bool isGroundCase = CLOS.GenericInvocationGenerics.Contains (self)
                                    && arguments.Length > 0
                                    && CLOS.GenericInvocationGenerics.Contains ((StandardInstance) arguments [0]);
                ah = computeCallable (isGroundCase, self, cache, keys, arguments);
                }

            return ah.Invoke (arguments);
        }

        public static FuncallHandler Create (StandardInstance gf)
        {
            StandardDiscriminatingFunction self = new StandardDiscriminatingFunction (gf);
            return self.Funcall;
        }
    }

    static class ClassForNull
    {
    }

    public static class CLOS
    {
        // temporary to force initialization
        static object UnboundSlotValue = UnboundSlot.Value;

        public static object Init ()
        {
            initializeClos ();
            return UnboundSlotValue;
        }

        #region StandardObjectExtensionMethods

        // Extension methods for StandardInstance
        [DebuggerStepThrough]
        static StandardInstance Class (this StandardInstance obj)
        {
            return ((ManifestInstance) obj.Target).Class;
        }

        static object SetClass (this StandardInstance obj, StandardInstance closClass)
        {
            ((ManifestInstance) obj.Target).Class = closClass;
            return closClass;
        }

        static object InstanceRef (this StandardInstance obj, int index)
        {
            return ((ManifestInstance) obj.Target).Slots [index];
        }

        static object InstanceSet (this StandardInstance obj, int index, object newValue)
        {
            ((ManifestInstance) obj.Target).Slots [index] = newValue;
            return newValue;
        }

        static T InstanceSet<T> (this StandardInstance obj, int index, T newValue)
        {
            ((ManifestInstance) obj.Target).Slots [index] = newValue;
            return newValue;
        }

        static object InstanceChange (this StandardInstance obj, int index, SlotChangingFunction change)
        {
            object [] slots = ((ManifestInstance) obj.Target).Slots;
            object newValue = change.Invoke (slots [index]);
            slots [index] = newValue;
            return newValue;
        }

        static T InstanceChange<T> (this StandardInstance obj, int index, SlotChangingFunction<T> change)
        {
            object [] slots = ((ManifestInstance) obj.Target).Slots;
            T newValue = change.Invoke ((T) slots [index]);
            slots [index] = newValue;
            return newValue;
        }

        public static object StandardInstanceAccess (StandardInstance instance, int location)
        {
            return instance.InstanceRef (location);
        }

        public static object SetStandardInstanceAccess (StandardInstance instance, int location, object newValue)
        {
            return instance.InstanceSet (location, newValue);
        }

        public static T SetStandardInstanceAccess<T> (StandardInstance instance, int location, T newValue)
        {
            return instance.InstanceSet<T> (location, newValue);
        }

        public static object StandardInstanceChange (StandardInstance instance, int location, SlotChangingFunction change)
        {
            return instance.InstanceChange (location, change);
        }

        public static T StandardInstanceChange<T> (StandardInstance instance, int location, SlotChangingFunction<T> change)
        {
            return instance.InstanceChange<T> (location, change);
        }

        static internal int SerialNumber (this StandardInstance obj)
        {
            return ((ManifestInstance) obj.Target).SerialNumber;
        }

        // Not an extension, but Instance-specific.
        static object SetSlots (this StandardInstance instance, object [] slots)
        {
            ((ManifestInstance) instance.Target).Slots = slots;
            return slots;
        }

        static object setFuncallableInstanceFunction (StandardInstance funcallableInstance, FuncallHandler handler)
        {
            ((ManifestInstance) funcallableInstance.Target).OnFuncall = handler;
            return handler;
        }

        //static object setFuncallableInstanceFunction (StandardInstance funcallableInstance, MethodInfo handler)
        //{
        //    ((ManifestInstance) funcallableInstance.Target).OnFuncall = FuncallHandlerWrapper.Create (handler);
        //    return handler;
        //}

        #endregion StandardObjectExtensionMethods

        #region ExportedGenerics
        public static StandardInstance AccessorMethodSlotDefinition
        {
            [DebuggerStepThrough]
            get
            {
                return accessorMethodSlotDefinition;
            }
        }

        public static StandardInstance AddDependent
        {
            [DebuggerStepThrough]
            get
            {
                return addDependent;
            }
        }

        public static StandardInstance AddDirectMethod
        {
            [DebuggerStepThrough]
            get
            {
                return addDirectMethod;
            }
        }

        public static StandardInstance AddDirectSubclass
        {
            [DebuggerStepThrough]
            get
            {
                return addDirectSubclass;
            }
        }

        public static StandardInstance AddMethod
        {
            [DebuggerStepThrough]
            get
            {
                return addMethod;
            }
        }

        public static StandardInstance AllocateInstance
        {
            [DebuggerStepThrough]
            get
            {
                return allocateInstance;
            }
        }

        public static StandardInstance BuiltInClass
        {
            [DebuggerStepThrough]
            get
            {
                return builtInClass;
            }
        }

        public static StandardInstance ChangeClass
        {
            [DebuggerStepThrough]
            get
            {
                return changeClass;
            }
        }

        public static StandardInstance ClassDefaultInitargs
        {
            [DebuggerStepThrough]
            get
            {
                return classDefaultInitargs;
            }
        }

        public static StandardInstance ClassDirectDefaultInitargs
        {
            [DebuggerStepThrough]
            get
            {
                return classDirectDefaultInitargs;
            }
        }

        public static StandardInstance ClassDirectSlots
        {
            [DebuggerStepThrough]
            get
            {
                return classDirectSlots;
            }
        }

        public static StandardInstance ClassDirectSubclasses
        {
            [DebuggerStepThrough]
            get
            {
                return classDirectSubclasses;
            }
        }

        public static StandardInstance ClassDirectSuperclasses
        {
            [DebuggerStepThrough]
            get
            {
                return classDirectSuperclasses;
            }
        }

        public static StandardInstance ClassName
        {
            [DebuggerStepThrough]
            get
            {
                return className;
            }
        }

        public static StandardInstance ClassPrecedenceList
        {
            [DebuggerStepThrough]
            get
            {
                return classPrecedenceList;
            }
        }

        public static StandardInstance ClassPrototype
        {
            [DebuggerStepThrough]
            get
            {
                return classPrototype;
            }
        }

        public static StandardInstance ClassSlots
        {
            [DebuggerStepThrough]
            get
            {
                return classSlots;
            }
        }

        public static StandardInstance ClosClass
        {
            [DebuggerStepThrough]
            get
            {
                return closClass;
            }
        }

        public static StandardInstance ComputeApplicableMethods
        {
            [DebuggerStepThrough]
            get
            {
                return computeApplicableMethods;
            }
        }

        public static StandardInstance ComputeApplicableMethodsUsingClasses
        {
            [DebuggerStepThrough]
            get
            {
                return computeApplicableMethodsUsingClasses;
            }
        }

        public static StandardInstance ComputeClassPrecedenceList
        {
            [DebuggerStepThrough]
            get
            {
                return computeClassPrecedenceList;
            }
        }

        public static StandardInstance ComputeDefaultInitargs
        {
            [DebuggerStepThrough]
            get
            {
                return computeDefaultInitargs;
            }
        }

        public static StandardInstance ComputeDiscriminatingFunction
        {
            [DebuggerStepThrough]
            get
            {
                return computeDiscriminatingFunction;
            }
        }

        public static StandardInstance ComputeEffectiveMethod
        {
            [DebuggerStepThrough]
            get
            {
                return computeEffectiveMethod;
            }
        }

        public static StandardInstance ComputeEffectiveSlotDefinition
        {
            [DebuggerStepThrough]
            get
            {
                return computeEffectiveSlotDefinition;
            }
        }

        public static StandardInstance ComputeSlots
        {
            [DebuggerStepThrough]
            get
            {
                return computeSlots;
            }
        }

        public static StandardInstance ComputeMethodMoreSpecific
        {
            [DebuggerStepThrough]
            get
            {
                return computeMethodMoreSpecific;
            }
        }

        public static StandardInstance DefaultInitargs
        {
            [DebuggerStepThrough]
            get
            {
                return defaultInitargs;
            }
        }

        public static StandardInstance DirectSlotDefinition
        {
            [DebuggerStepThrough]
            get
            {
                return directSlotDefinition;
            }
        }

        public static StandardInstance DirectSlotDefinitionClass
        {
            [DebuggerStepThrough]
            get
            {
                return directSlotDefinitionClass;
            }
        }

        public static StandardInstance DotnetGenericFunction
        {
            [DebuggerStepThrough]
            get
            {
                return dotnetGenericFunction;
            }
        }

        public static StandardInstance EffectiveSlotDefinition
        {
            [DebuggerStepThrough]
            get
            {
                return effectiveSlotDefinition;
            }
        }

        public static StandardInstance EffectiveSlotDefinitionClass
        {
            [DebuggerStepThrough]
            get
            {
                return effectiveSlotDefinitionClass;
            }
        }

        public static StandardInstance EnsureClass
        {
            [DebuggerStepThrough]
            get
            {
                return ensureClass;
            }
        }

        public static StandardInstance EnsureClassUsingClass
        {
            [DebuggerStepThrough]
            get
            {
                return ensureClassUsingClass;
            }
        }

        public static StandardInstance EnsureGenericFunction
        {
            [DebuggerStepThrough]
            get
            {
                return ensureGenericFunction;
            }
        }

        public static StandardInstance EnsureGenericFunctionUsingClass
        {
            [DebuggerStepThrough]
            get
            {
                return ensureGenericFunctionUsingClass;
            }
        }

        public static StandardInstance EqlSpecializer
        {
            [DebuggerStepThrough]
            get
            {
                return eqlSpecializer;
            }
        }

        public static StandardInstance EqlSpecializerObject
        {
            [DebuggerStepThrough]
            get
            {
                return eqlSpecializerObject;
            }
        }

        public static StandardInstance ExtractLambdaList
        {
            [DebuggerStepThrough]
            get
            {
                return extractLambdaList;
            }
        }

        public static StandardInstance ExtractSpecializerNames
        {
            [DebuggerStepThrough]
            get
            {
                return extractSpecializerNames;
            }
        }

        public static StandardInstance FinalizeInheritance
        {
            [DebuggerStepThrough]
            get
            {
                return finalizeInheritance;
            }
        }

        public static StandardInstance FindClass
        {
            [DebuggerStepThrough]
            get
            {
                return findClass;
            }
        }

        public static StandardInstance FindMethod
        {
            [DebuggerStepThrough]
            get
            {
                return findMethod;
            }
        }

        public static StandardInstance FindMethodCombination
        {
            [DebuggerStepThrough]
            get
            {
                return findMethodCombination;
            }
        }

        public static StandardInstance ForwardReferencedClass
        {
            [DebuggerStepThrough]
            get
            {
                return forwardReferencedClass;
            }
        }

        public static StandardInstance FuncallableStandardClass
        {
            [DebuggerStepThrough]
            get
            {
                return funcallableStandardClass;
            }
        }

        public static StandardInstance FuncallableStandardObject
        {
            [DebuggerStepThrough]
            get
            {
                return funcallableStandardObject;
            }
        }

        public static StandardInstance FunctionKeywords
        {
            [DebuggerStepThrough]
            get
            {
                return functionKeywords;
            }
        }

        public static StandardInstance GenericFunction
        {
            [DebuggerStepThrough]
            get
            {
                return genericFunction;
            }
        }

        public static StandardInstance GenericFunctionArgumentPrecedenceOrder
        {
            [DebuggerStepThrough]
            get
            {
                return genericFunctionArgumentPrecedenceOrder;
            }
        }

        public static StandardInstance GenericFunctionDeclarations
        {
            [DebuggerStepThrough]
            get
            {
                return genericFunctionDeclarations;
            }
        }

        public static StandardInstance GenericFunctionLambdaList
        {
            [DebuggerStepThrough]
            get
            {
                return genericFunctionLambdaList;
            }
        }

        public static StandardInstance GenericFunctionMethodClass
        {
            [DebuggerStepThrough]
            get
            {
                return genericFunctionMethodClass;
            }
        }

        public static StandardInstance GenericFunctionMethodCombination
        {
            [DebuggerStepThrough]
            get
            {
                return genericFunctionMethodCombination;
            }
        }

        public static StandardInstance GenericFunctionMethods
        {
            [DebuggerStepThrough]
            get
            {
                return genericFunctionMethods;
            }
        }

        public static StandardInstance GenericFunctionName
        {
            [DebuggerStepThrough]
            get
            {
                return genericFunctionName;
            }
        }

        public static StandardInstance InitializeInstance
        {
            [DebuggerStepThrough]
            get
            {
                return initializeInstance;
            }
        }

        public static StandardInstance InternEqlSpecializer
        {
            [DebuggerStepThrough]
            get
            {
                return internEqlSpecializer;
            }
        }

        public static StandardInstance IsClassFinalized
        {
            [DebuggerStepThrough]
            get
            {
                return isClassFinalized;
            }
        }

        public static StandardInstance MakeInstance
        {
            [DebuggerStepThrough]
            get
            {
                return makeInstance;
            }
        }

        public static StandardInstance MakeInstancesObsolete
        {
            [DebuggerStepThrough]
            get
            {
                return makeInstancesObsolete;
            }
        }

        public static StandardInstance MakeLoadForm
        {
            [DebuggerStepThrough]
            get
            {
                return makeLoadForm;
            }
        }

        public static StandardInstance MakeMethodLambda
        {
            [DebuggerStepThrough]
            get
            {
                return makeMethodLambda;
            }
        }

        public static StandardInstance MapDependents
        {
            [DebuggerStepThrough]
            get
            {
                return mapDependents;
            }
        }

        public static StandardInstance Metaobject
        {
            [DebuggerStepThrough]
            get
            {
                return metaobject;
            }
        }

        public static StandardInstance Method
        {
            [DebuggerStepThrough]
            get
            {
                return method;
            }
        }

        public static StandardInstance MethodCombination
        {
            [DebuggerStepThrough]
            get
            {
                return methodCombination;
            }
        }

        public static StandardInstance MethodFunction
        {
            [DebuggerStepThrough]
            get
            {
                return methodFunction;
            }
        }

        public static StandardInstance MethodGenericFunction
        {
            [DebuggerStepThrough]
            get
            {
                return methodGenericFunction;
            }
        }

        public static StandardInstance MethodLambdaList
        {
            [DebuggerStepThrough]
            get
            {
                return methodLambdaList;
            }
        }

        public static StandardInstance MethodSpecializers
        {
            [DebuggerStepThrough]
            get
            {
                return methodSpecializers;
            }
        }

        public static StandardInstance MethodQualifier
        {
            [DebuggerStepThrough]
            get
            {
                return methodQualifier;
            }
        }

        public static StandardInstance NoApplicableMethod
        {
            [DebuggerStepThrough]
            get
            {
                return noApplicableMethod;
            }
        }

        public static StandardInstance NoNextMethod
        {
            [DebuggerStepThrough]
            get
            {
                return noNextMethod;
            }
        }

        public static StandardInstance ReaderMethodClass
        {
            [DebuggerStepThrough]
            get
            {
                return readerMethodClass;
            }
        }

        public static StandardInstance ReinitializeInstance
        {
            [DebuggerStepThrough]
            get
            {
                return reinitializeInstance;
            }
        }

        public static StandardInstance RemoveDependents
        {
            [DebuggerStepThrough]
            get
            {
                return removeDependents;
            }
        }

        public static StandardInstance RemoveDirectMethod
        {
            [DebuggerStepThrough]
            get
            {
                return removeDirectMethod;
            }
        }

        public static StandardInstance RemoveDirectSubclass
        {
            [DebuggerStepThrough]
            get
            {
                return removeDirectSubclass;
            }
        }

        public static StandardInstance RemoveMethod
        {
            [DebuggerStepThrough]
            get
            {
                return removeMethod;
            }
        }

        public static StandardInstance SharedInitialize
        {
            [DebuggerStepThrough]
            get
            {
                return sharedInitialize;
            }
        }

        public static StandardInstance SetClassName
        {
            [DebuggerStepThrough]
            get
            {
                return setClassName;
            }
        }

        public static StandardInstance SetGenericFunctionName
        {
            [DebuggerStepThrough]
            get
            {
                return setGenericFunctionName;
            }
        }

        public static StandardInstance SetSlotValueUsingClass
        {
            [DebuggerStepThrough]
            get
            {
                return setSlotValueUsingClass;
            }
        }

        public static StandardInstance IsSlotBoundUsingClass
        {
            [DebuggerStepThrough]
            get
            {
                return isSlotBoundUsingClass;
            }
        }

        public static StandardInstance SlotDefinition
        {
            [DebuggerStepThrough]
            get
            {
                return slotDefinition;
            }
        }

        public static StandardInstance SlotDefinitionAllocation
        {
            [DebuggerStepThrough]
            get
            {
                return slotDefinitionAllocation;
            }
        }

        public static StandardInstance SlotDefinitionInitargs
        {
            [DebuggerStepThrough]
            get
            {
                return slotDefinitionInitargs;
            }
        }

        public static StandardInstance SlotDefinitionInitform
        {
            [DebuggerStepThrough]
            get
            {
                return slotDefinitionInitform;
            }
        }

        public static StandardInstance SlotDefinitionInitfunction
        {
            [DebuggerStepThrough]
            get
            {
                return slotDefinitionInitfunction;
            }
        }

        public static StandardInstance SlotDefinitionLocation
        {
            [DebuggerStepThrough]
            get
            {
                return slotDefinitionLocation;
            }
        }

        public static StandardInstance SlotDefinitionName
        {
            [DebuggerStepThrough]
            get
            {
                return slotDefinitionName;
            }
        }

        public static StandardInstance SlotDefinitionType
        {
            [DebuggerStepThrough]
            get
            {
                return slotDefinitionType;
            }
        }

        public static StandardInstance SlotDefinitionReaders
        {
            [DebuggerStepThrough]
            get
            {
                return slotDefinitionReaders;
            }
        }

        public static StandardInstance SlotDefinitionWriters
        {
            [DebuggerStepThrough]
            get
            {
                return slotDefinitionWriters;
            }
        }

        public static StandardInstance SlotMakunboundUsingClass
        {
            [DebuggerStepThrough]
            get
            {
                return slotMakunboundUsingClass;
            }
        }

        public static StandardInstance SlotMissing
        {
            [DebuggerStepThrough]
            get
            {
                return slotMissing;
            }
        }

        public static StandardInstance SlotUnbound
        {
            [DebuggerStepThrough]
            get
            {
                return slotUnbound;
            }
        }

        public static StandardInstance SlotValueUsingClass
        {
            [DebuggerStepThrough]
            get
            {
                return slotValueUsingClass;
            }
        }

        public static StandardInstance Specializer
        {
            [DebuggerStepThrough]
            get
            {
                return specializer;
            }
        }

        public static StandardInstance SpecializerDirectGenericFunctions
        {
            [DebuggerStepThrough]
            get
            {
                return specializerDirectGenericFunctions;
            }
        }

        public static StandardInstance SpecializerDirectMethods
        {
            [DebuggerStepThrough]
            get
            {
                return specializerDirectMethods;
            }
        }

        public static StandardInstance StandardAccessorMethod
        {
            [DebuggerStepThrough]
            get
            {
                return standardAccessorMethod;
            }
        }

        public static StandardInstance StandardAccessorMethodClass
        {
            [DebuggerStepThrough]
            get
            {
                return standardAccessorMethodClass;
            }
        }

        public static StandardInstance StandardClass
        {
            [DebuggerStepThrough]
            get
            {
                return standardClass;
            }
        }

        public static StandardInstance StandardDirectSlotDefinition
        {
            [DebuggerStepThrough]
            get
            {
                return standardDirectSlotDefinition;
            }
        }

        public static StandardInstance StandardEffectiveSlotDefinition
        {
            [DebuggerStepThrough]
            get
            {
                return standardEffectiveSlotDefinition;
            }
        }

        public static StandardInstance StandardGenericFunction
        {
            [DebuggerStepThrough]
            get
            {
                return standardGenericFunction;
            }
        }

        public static StandardInstance StandardMethod
        {
            [DebuggerStepThrough]
            get
            {
                return standardMethod;
            }
        }

        public static StandardInstance StandardObjectClass
        {
            [DebuggerStepThrough]
            get
            {
                return standardObjectClass;
            }
        }

        public static StandardInstance StandardReaderMethod
        {
            [DebuggerStepThrough]
            get
            {
                return standardReaderMethod;
            }
        }

        public static StandardInstance StandardSlotDefinition
        {
            [DebuggerStepThrough]
            get
            {
                return standardSlotDefinition;
            }
        }

        public static StandardInstance StandardWriterMethod
        {
            [DebuggerStepThrough]
            get
            {
                return standardWriterMethod;
            }
        }

        public static StandardInstance Top
        {
            [DebuggerStepThrough]
            get
            {
                return top;
            }
        }

        public static StandardInstance UpdateDependent
        {
            [DebuggerStepThrough]
            get
            {
                return updateDependent;
            }
        }

        public static StandardInstance UpdateInstanceForDifferentClass
        {
            [DebuggerStepThrough]
            get
            {
                return updateInstanceForDifferentClass;
            }
        }

        public static StandardInstance UpdateInstanceForRedefinedClass
        {
            [DebuggerStepThrough]
            get
            {
                return updateInstanceForRedefinedClass;
            }
        }

        public static StandardInstance ValidateSuperclass
        {
            [DebuggerStepThrough]
            get
            {
                return validateSuperclass;
            }
        }

        public static StandardInstance WriterMethodClass
        {
            [DebuggerStepThrough]
            get
            {
                return writerMethodClass;
            }
        }

        public static bool IsSlotBound (StandardInstance instance, Symbol slotName)
        {
            throw new NotImplementedException ();
        }

        public static bool IsSlotExists (StandardInstance instance, Symbol slotName)
        {
            throw new NotImplementedException ();
        }

        public static StandardInstance SlotMakunbound (StandardInstance instance, Symbol slotName)
        {
            throw new NotImplementedException ();
        }

        public static object SlotValue (StandardInstance instance, Symbol slotName)
        {
            StandardInstance closClass = instance.Class();
            return SlotValueUsingClass (closClass, instance, lookupSlot (closClass, slotName));
        }

        public static object SetSlotValue (object newValue, StandardInstance instance, Symbol slotName)
        {
            StandardInstance closClass = instance.Class();
            return SetSlotValueUsingClass (newValue, closClass, instance, lookupSlot (closClass, slotName));
        }

        public static T SetSlotValue<T> (T newValue, StandardInstance instance, Symbol slotName)
        {
            throw new NotImplementedException ();
        }

        #endregion ExportedGenerics

        #region Symbols

        static readonly Package closPackage = Package.Clos;

        // static readonly Symbol QuoteAccessorMethodSlotDefinition = closSymbol ("AccessorMethodSlotDefinition");
        // static readonly Symbol QuoteAddDependent = closSymbol ("AddDependent");
        // static readonly Symbol QuoteAddDirectMethod = closSymbol ("AddDirectMethod");
        // static readonly Symbol QuoteAddDirectSubclass = closSymbol ("AddDirectSubclass");
        // static readonly Symbol QuoteAddMethod = Package.CommonLisp.FindSymbol ("ADD-METHOD");
        static readonly Symbol QuoteAllocateInstance = Package.CommonLisp.FindSymbol ("ALLOCATE-INSTANCE");
        static readonly Symbol QuoteAllocation = closSymbol ("ALLOCATION");
        static readonly Symbol QuoteAndAllowOtherKeys = Package.CommonLisp.FindSymbol ("&ALLOW-OTHER-KEYS");
        static readonly Symbol QuoteAndKey = Package.CommonLisp.FindSymbol ("&KEY");
        static readonly Symbol QuoteAndRest = Package.CommonLisp.FindSymbol ("&REST");
        // static readonly Symbol QuoteAnonymous = Package.CommonLisp.FindSymbol ("[unnamed]");
        static readonly Symbol QuoteApplicationCache = closSymbol ("APPLICATION-CACHE");
        static readonly Symbol QuoteArgumentPrecedenceOrder = closSymbol ("ARGUMENT-PRECEDENCE-ORDER");
        static readonly Symbol QuoteArguments = closSymbol ("ARGUMENTS");
        // static readonly Symbol QuoteArity = closSymbol ("ARITY");
        static readonly Symbol QuoteBuiltInClass = closSymbol ("BUILT-IN-CLASS");
        static readonly Symbol QuoteBuiltInEffectiveSlotDefinition = closSymbol ("BUILT-IN-EFFECTIVE-SLOT-DEFINITION");
        static readonly Symbol QuoteBuiltInSlotDefinition = closSymbol ("BUILT-IN-SLOT-DEFINITION");
        // static readonly Symbol QuoteChangeClass = closSymbol ("ChangeClass");
        static readonly Symbol QuoteClass = closSymbol ("CLASS");
        // static readonly Symbol QuoteClassDefaultInitargs = closSymbol ("ClassDefaultInitargs");
        // static readonly Symbol QuoteClassDirectDefaultInitargs = closSymbol ("ClassDirectDefaultInitargs");
        // static readonly Symbol QuoteClassDirectSlots = closSymbol ("CLASS-DIRECT-SLOTS");
        // static readonly Symbol QuoteClassDirectSubclasses = closSymbol ("ClassDirectSubclasses");
        // static readonly Symbol QuoteClassDirectSuperclasses = closSymbol ("ClassDirectSuperclasses");
        // static readonly Symbol QuoteClassInitializers = closSymbol ("CLASS-INITIALIZERS");
        // static readonly Symbol QuoteClassName = closSymbol ("CLASS-NAME");
        static readonly Symbol QuoteClassPrecedenceList = closSymbol ("CLASS-PRECEDENCE-LIST");
        // static readonly Symbol QuoteClassPrototype = closSymbol ("CLASS-PROTOTYPE");
        static readonly Symbol QuoteClassSlots = closSymbol ("CLASS-SLOTS");
        static readonly Symbol QuoteComputeApplicableMethods = closSymbol ("COMPUTE-APPLICABLE-METHODS");
        // static readonly Symbol QuoteComputeApplicableMethodsUsingClasses = closSymbol ("ComputeApplicableMethodsUsingClasses");
        // static readonly Symbol QuoteComputeClassPrecedenceList = closSymbol ("COMPUTE-CLASS-PRECEDENCE-LIST");
        // static readonly Symbol QuoteComputeDefaultInitargs = closSymbol ("ComputeDefaultInitargs");
        static readonly Symbol QuoteComputeDiscriminatingFunction = closSymbol ("COMPUTE-DISCRIMINATING-FUNCTION");
        static readonly Symbol QuoteComputeEffectiveMethod = closSymbol ("COMPUTE-EFFECTIVE-METHOD");
        // static readonly Symbol QuoteComputeEffectiveSlotDefinition = closSymbol ("ComputeEffectiveSlotDefinition");
        static readonly Symbol QuoteComputeMethodMoreSpecific = closSymbol ("COMPUTE-METHOD-MORE-SPECIFIC");
        // static readonly Symbol QuoteComputeSlots = closSymbol ("COMPUTE-SLOTS");
        // static readonly Symbol QuoteCons = closSymbol ("CONS");
        static readonly Symbol QuoteDeclarations = closSymbol ("DECLARATIONS");
        static readonly Symbol QuoteDefaultInitargs = closSymbol ("DEFAULT-INITARGS");
        static readonly Symbol QuoteDirectDefaultInitargs = closSymbol ("DIRECT-DEFAULT-INITARGS");
        static readonly Symbol QuoteDirectMethods = closSymbol ("DIRECT-METHODS");
        static readonly Symbol QuoteDirectSlotDefinition = closSymbol ("DIRECT-SLOT-DEFINITION");
        // static readonly Symbol QuoteDirectSlotDefinitionClass = closSymbol ("DirectSlotDefinitionClass");
        static readonly Symbol QuoteDirectSlots = closSymbol ("DIRECT-SLOTS");
        static readonly Symbol QuoteDirectSubclasses = closSymbol ("DIRECT-SUBCLASSES");
        static readonly Symbol QuoteDirectSuperclasses = closSymbol ("DIRECT-SUPERCLASSES");
        static readonly Symbol QuoteDocumentation = closSymbol ("DOCUMENTATION");
        static readonly Symbol QuoteDotnetGenericFunction = closSymbol ("DOTNET-GENERIC-FUNCTION");
        static readonly Symbol QuoteDotnetType = closSymbol ("DOTNET-TYPE");
        static readonly Symbol QuoteEffectiveSlotDefinition = closSymbol ("Effective-Slot-Definition");
        // static readonly Symbol QuoteEffectiveSlotDefinitionClass = closSymbol ("EffectiveSlotDefinitionClass");
        // static readonly Symbol QuoteEffectiveSlots = closSymbol ("EFFECTIVE-SLOTS");
        // static readonly Symbol QuoteEnsureClass = closSymbol ("ENSURE-CLASS");
        // static readonly Symbol QuoteEnsureClassUsingClass = closSymbol ("EnsureClassUsingClass");
        static readonly Symbol QuoteEnsureGenericFunction = Package.CommonLisp.FindSymbol ("ENSURE-GENERIC-FUNCTION");
        static readonly Symbol QuoteEnsureGenericFunctionUsingClass = closSymbol ("ENSURE-GENERIC-FUNCTION-USING-CLASS");
        static readonly Symbol QuoteEqlSpecializer = closSymbol ("EQL-SPECIALIZER");
        // static readonly Symbol QuoteEqlSpecializerObject = closSymbol ("EqlSpecializerObject");
        // static readonly Symbol QuoteExtractLambdaList = closSymbol ("ExtractLambdaList");
        // static readonly Symbol QuoteExtractSpecializerNames = closSymbol ("ExtractSpecializerNames");
        // static readonly Symbol QuoteFinalizeInheritance = closSymbol ("FinalizeInheritance");
        // static readonly Symbol QuoteFinalizedP = closSymbol ("FINALIZEDP");
        // static readonly Symbol QuoteFindClass = closSymbol ("FindClass");
        // static readonly Symbol QuoteFindMethod = closSymbol ("FindMethod");
        // static readonly Symbol QuoteFindMethodCombination = closSymbol ("FindMethodCombination");
        static readonly Symbol QuoteForwardReferencedClass = closSymbol ("FORWARD-REFERENCED-CLASS");
        static readonly Symbol QuoteFuncallableStandardClass = closSymbol ("FUNCALLABLE-STANDARD-CLASS");
        static readonly Symbol QuoteFuncallableStandardObject = closSymbol ("FUNCALLABLE-STANDARD-OBJECT");
        static readonly Symbol QuoteFunction = closSymbol ("FUNCTION");
        // static readonly Symbol QuoteFunctionKeywords = closSymbol ("FunctionKeywords");
        static readonly Symbol QuoteFunctionName = closSymbol ("FUNCTION-NAME");
        static readonly Symbol QuoteGenericFunction = closSymbol ("GENERIC-FUNCTION");
        // static readonly Symbol QuoteGenericFunctionArgumentPrecedenceOrder = closSymbol ("GenericFunctionArgumentPrecedenceOrder");
        static readonly Symbol QuoteGenericFunctionApplicationCache = closSymbol ("GENERIC-FUNCTION-APPLICATION-CACHE");
        static readonly Symbol QuoteGenericFunctionClass = closSymbol ("GENERIC-FUNCTION-CLASS");
        // static readonly Symbol QuoteGenericFunctionDeclarations = closSymbol ("GenericFunctionDeclarations");
        static readonly Symbol QuoteGenericFunctionLambdaList = closSymbol ("GENERIC-FUNCTION-LAMBDA-LIST");
        // static readonly Symbol QuoteGenericFunctionMethodClass = closSymbol ("GENERIC-FUNCTION-METHOD-CLASS");
        // static readonly Symbol QuoteGenericFunctionMethodCombination = closSymbol ("GenericFunctionMethodCombination");
        static readonly Symbol QuoteGenericFunctionMethods = closSymbol ("GENERIC-FUNCTION-METHODS");
        // static readonly Symbol QuoteGenericFunctionName = closSymbol ("GENERIC-FUNCTION-NAME");
        static readonly Symbol QuoteGenericFunctionSingletonsList = closSymbol ("GENERIC-FUNCTION-SINGELTONS-LIST");
        // static readonly Symbol QuoteGettersAndSetters = closSymbol ("GETTERS-AND-SETTERS");
        static readonly Symbol QuoteInitargs = closSymbol ("INITARGS");
        // static readonly Symbol QuoteInitform = closSymbol ("INITFORM");
        // static readonly Symbol QuoteInitfunction = closSymbol ("INITFUNCTION");
        static readonly Symbol QuoteInitializeInstance = closSymbol ("INITIALIZE-INSTANCE");
        static readonly Symbol QuoteInitializer = closSymbol ("INITIALIZER");
        // static readonly Symbol QuoteInitializers = closSymbol ("INITIALIZERS");
        static readonly Symbol QuoteInstance = closSymbol ("INSTANCE");
        // static readonly Symbol QuoteInternEqlSpecializer = closSymbol ("InternEqlSpecializer");
        // static readonly Symbol QuoteIsClassFinalized = closSymbol ("IsClassFinalized");
        // static readonly Symbol QuoteIsSlotBoundUsingClass = closSymbol ("IsSlotBoundUsingClass");
        static readonly Symbol QuoteLambdaList = closSymbol ("LAMBDA-LIST");
        static readonly Symbol QuoteLeft = closSymbol ("LEFT");
        // static readonly Symbol QuoteList = Package.CommonLisp.FindSymbol ("LIST");
        static readonly Symbol QuoteLocation = closSymbol ("LOCATION");
        static readonly Symbol QuoteMakeInstance = Package.CommonLisp.FindSymbol ("MAKE-INSTANCE");
        // static readonly Symbol QuoteMakeInstancesObsolete = closSymbol ("MAKE-INSTANCES-OBSOLETE");
        // static readonly Symbol QuoteMakeLoadForm = closSymbol ("MakeLoadForm");
        // static readonly Symbol QuoteMakeMethodLambda = closSymbol ("MakeMethodLambda");
        // static readonly Symbol QuoteMapDependents = closSymbol ("MapDependents");
        static readonly Symbol QuoteMetaobject = closSymbol ("METAOBJECT");
        static readonly Symbol QuoteMethod = closSymbol ("METHOD");
        static readonly Symbol QuoteMethodAllocateInstance = closSymbol ("METHOD:allocate-instance");
        static readonly Symbol QuoteMethodClass = closSymbol ("METHOD-CLASS");
        static readonly Symbol QuoteMethodClassPrecedenceList = closSymbol ("METHOD:class-precedence-list");
        static readonly Symbol QuoteMethodClassSlots = closSymbol ("METHOD:class-slots");
        static readonly Symbol QuoteMethodCombination = closSymbol ("METHOD-COMBINATION");
        // static readonly Symbol QuoteMethodComputeApplicableMethods = closSymbol ("METHOD:compute-applicable-methods");
        static readonly Symbol QuoteMethodComputeDiscriminatingFunction = closSymbol ("METHOD:compute-discriminating-function");
        // static readonly Symbol QuoteMethodComputeEffectiveMethod = closSymbol ("METHOD:compute-effective-method");
        // static readonly Symbol QuoteMethodComputeMethodMoreSpecific = closSymbol ("METHOD:compute-method-more-specific");
        // static readonly Symbol QuoteMethodDefaultInitargs = closSymbol ("METHOD:default-initargs");
        static readonly Symbol QuoteMethodEnsureGenericFunction = closSymbol ("METHOD:ensure-generic-function");
        static readonly Symbol QuoteMethodEnsureGenericFunctionUsingClass = closSymbol ("METHOD:ensure-generic-function-using-class");
        static readonly Symbol QuoteMethodFunction = closSymbol ("MethodFunction");
        // static readonly Symbol QuoteMethodGenericFunction = closSymbol ("MethodGenericFunction");
        static readonly Symbol QuoteMethodInitializeInstance = closSymbol ("METHOD:initialize-instance");
        // static readonly Symbol QuoteMethodLambdaList = closSymbol ("MethodLambdaList");
        static readonly Symbol QuoteMethodMakeInstance = closSymbol ("METHOD:make-instance");
        // static readonly Symbol QuoteMethodMethodQualifier = closSymbol ("METHOD:method-qualifier");
        // static readonly Symbol QuoteMethodMethodSpecializers = closSymbol ("METHOD:method-specializers");
        static readonly Symbol QuoteMethodQualifier = closSymbol ("METHOD-QUALIFIER");
        static readonly Symbol QuoteMethodSharedInitialize = closSymbol ("Method:shared-initialize");
        static readonly Symbol QuoteMethodSpecializers = closSymbol ("METHOD-SPECIALIZERS");
        static readonly Symbol QuoteMethods = closSymbol ("METHODS");
        static readonly Symbol QuoteName = closSymbol ("NAME");
        // static readonly Symbol QuoteNewMakeInstance = closSymbol ("NEW-MAKE-INSTANCE");
        // static readonly Symbol QuoteNewMakeInstanceMethod = closSymbol ("NEW-MAKE-INSTANCE-METHOD");
        static readonly Symbol QuoteNewValue = closSymbol ("NEW-VALUE");
        // static readonly Symbol QuoteNoApplicableMethod = closSymbol ("NoApplicableMethod");
        // static readonly Symbol QuoteNoNextMethod = closSymbol ("NoNextMethod");
        static readonly Symbol QuoteNull = closSymbol ("NULL");
        static readonly Symbol QuotePrecedenceList = closSymbol ("PRECEDENCE-LIST");
        // static readonly Symbol QuoteProcedure = closSymbol ("PROCEDURE");
        static readonly Symbol QuotePrototype = closSymbol ("PROTOTYPE");
        static readonly Symbol QuoteQualifier = closSymbol ("QUALIFIER");
        // static readonly Symbol QuoteReaderMethodClass = closSymbol ("ReaderMethodClass");
        // static readonly Symbol QuoteReaders = closSymbol ("READERS");
        // static readonly Symbol QuoteReinitializeInstance = closSymbol ("REINITIALIZE-INSTANCE");
        // static readonly Symbol QuoteRemoveDependents = closSymbol ("RemoveDependents");
        // static readonly Symbol QuoteRemoveDirectMethod = closSymbol ("RemoveDirectMethod");
        // static readonly Symbol QuoteRemoveDirectSubclass = closSymbol ("RemoveDirectSubclass");
        // static readonly Symbol QuoteRemoveMethod = closSymbol ("RemoveMethod");
        static readonly Symbol QuoteRight = closSymbol ("RIGHT");
        static readonly Symbol QuoteRuntimeType = closSymbol ("RUNTIME-TYPE");
        // static readonly Symbol QuoteSetClassDirectSlots = closSymbol ("SET-CLASS-DIRECT-SLOTS");
        // static readonly Symbol QuoteSetClassDirectSuperclasses = closSymbol ("SET-CLASS-DIRECT-SUPERCLASSES");
        // static readonly Symbol QuoteSetClassName = closSymbol ("SetClassName");
        // static readonly Symbol QuoteSetClassSlots = closSymbol ("SET-CLASS-SLOTS");
        static readonly Symbol QuoteSetGenericFunctionApplicationCache = closSymbol ("SET-GENERIC-FUNCTION-APPLICATION-CACHE");
        // static readonly Symbol QuoteSetGenericFunctionName = closSymbol ("SetGenericFunctionName");
        // static readonly Symbol QuoteSetSlotValue = closSymbol ("SetSlotValue");
        static readonly Symbol QuoteSetSlotValueUsingClass = closSymbol ("SET-SLOT-VALUE-USING-CLASS");
        static readonly Symbol QuoteSharedInitialize = closSymbol ("SHARED-INITIALIZE");
        static readonly Symbol QuoteSingletonsList = closSymbol ("SINGLETONS-LIST");
        static readonly Symbol QuoteSlotDefinition = closSymbol ("SLOT-DEFINITION");
        // static readonly Symbol QuoteSlotDefinitionAllocation = closSymbol ("SlotDefinitionAllocation");
        // static readonly Symbol QuoteSlotDefinitionInitargs = closSymbol ("SlotDefinitionInitargs");
        // static readonly Symbol QuoteSlotDefinitionInitform = closSymbol ("SlotDefinitionInitform");
        // static readonly Symbol QuoteSlotDefinitionInitfunction = closSymbol ("SlotDefinitionInitfunction");
        // static readonly Symbol QuoteSlotDefinitionInitializer = closSymbol ("SLOT-DEFINITION-INITIALIZER");
        static readonly Symbol QuoteSlotDefinitionLocation = closSymbol ("SlotDefinitionLocation");
        static readonly Symbol QuoteSlotDefinitionName = closSymbol ("SlotDefinitionName");
        // static readonly Symbol QuoteSlotDefinitionReaders = closSymbol ("SlotDefinitionReaders");
        // static readonly Symbol QuoteSlotDefinitionType = closSymbol ("SlotDefinitionType");
        // static readonly Symbol QuoteSlotDefinitionWriters = closSymbol ("SlotDefinitionWriters");
        // static readonly Symbol QuoteSlotInitializers = closSymbol ("SLOT-INITIALIZERS");
        // static readonly Symbol QuoteSlotMakunboundUsingClass = closSymbol ("SlotMakunboundUsingClass");
        // static readonly Symbol QuoteSlotMissing = closSymbol ("SlotMissing");
        // static readonly Symbol QuoteSlotNames = closSymbol ("SLOT-NAMES");
        // static readonly Symbol QuoteSlotUnbound = closSymbol ("SlotUnbound");
        // static readonly Symbol QuoteSlotValue = closSymbol ("SLOT-VALUE");
        static readonly Symbol QuoteSlotValueUsingClass = closSymbol ("SLOT-VALUE-USING-CLASS");
        static readonly Symbol QuoteSlot = closSymbol ("SLOT");
        static readonly Symbol QuoteSlots = closSymbol ("SLOTS");
        static readonly Symbol QuoteSpecializer = closSymbol ("SPECIALIZER");
        // static readonly Symbol QuoteSpecializerDirectGenericFunctions = closSymbol ("SpecializerDirectGenericFunctions");
        // static readonly Symbol QuoteSpecializerDirectMethods = closSymbol ("SpecializerDirectMethods");
        static readonly Symbol QuoteSpecializers = closSymbol ("SPECIALIZERS");
        static readonly Symbol QuoteStandardAccessorMethod = closSymbol ("STANDARD-ACCESSOR-METHOD");
        static readonly Symbol QuoteStandardClass = closSymbol ("STANDARD-CLASS");
        static readonly Symbol QuoteStandardDirectSlotDefinition = closSymbol ("STANDARD-DIRECT-SLOT-DEFINITION");
        static readonly Symbol QuoteStandardEffectiveSlotDefinition = closSymbol ("STANDARD-EFFECTIVE-SLOT-DEFINITION");
        static readonly Symbol QuoteStandardGenericFunction = closSymbol ("STANDARD-GENERIC-FUNCTION");
        static readonly Symbol QuoteStandardMethod = Package.CommonLisp.FindSymbol ("STANDARD-METHOD");
        static readonly Symbol QuoteStandardObject = closSymbol ("STANDARD-OBJECT");
        static readonly Symbol QuoteStandardReaderMethod = closSymbol ("STANDARD-READER-METHOD");
        static readonly Symbol QuoteStandardSlotDefinition = closSymbol ("STANDARD-SLOT-DEFINITION");
        static readonly Symbol QuoteStandardWriterMethod = closSymbol ("STANDARD-WRITER-METHOD");
        static readonly Symbol QuoteStudlyName = closSymbol ("StudlyName");
        // static readonly Symbol QuoteSuperclass = closSymbol ("SUPERCLASS");
        // static readonly Symbol QuoteSuppliedInitargs = closSymbol ("SUPPLIED-INITARGS");
        static readonly Symbol QuoteSymbol = closSymbol ("SYMBOL");
        static readonly Symbol QuoteTop = closSymbol ("TOP");
        static readonly Symbol QuoteType = closSymbol ("TYPE");
        // static readonly Symbol QuoteUpdateDependent = closSymbol ("UpdateDependent");
        // static readonly Symbol QuoteUpdateInstanceForDifferentClass = closSymbol ("UpdateInstanceForDifferentClass");
        // static readonly Symbol QuoteUpdateInstanceForRedefinedClass = closSymbol ("UpdateInstanceForRedefinedClass");
        // static readonly Symbol QuoteValidateSuperclass = closSymbol ("VALIDATE-SUPERCLASS");
        // static readonly Symbol QuoteWriterMethodClass = closSymbol ("WriterMethodClass");
        // static readonly Symbol QuoteWriters = closSymbol ("WRITERS");

        #endregion Symbols

        #region Allocation
        // CLOS is highly circular, so I'm defining everything from the get-go as
        // empty objects.  I'll fill them in later.

        // The MOP classes.

        static readonly StandardInstance builtInClass = ManifestInstance.CreateInstance ();
        static readonly StandardInstance builtInEffectiveSlotDefinition = ManifestInstance.CreateInstance ();
        static readonly StandardInstance builtInSlotDefinition = ManifestInstance.CreateInstance ();
        static readonly StandardInstance closClass = ManifestInstance.CreateInstance ();
        static readonly StandardInstance directSlotDefinition = ManifestInstance.CreateInstance ();
        static readonly StandardInstance dotnetGenericFunction = ManifestInstance.CreateInstance ();
        static readonly StandardInstance effectiveSlotDefinition = ManifestInstance.CreateInstance ();
        static readonly StandardInstance eqlSpecializer = ManifestInstance.CreateInstance ();
        static readonly StandardInstance forwardReferencedClass = ManifestInstance.CreateInstance ();
        static readonly StandardInstance funcallableStandardClass = ManifestInstance.CreateInstance ();
        static readonly StandardInstance funcallableStandardObject = ManifestInstance.CreateInstance ();
        static readonly StandardInstance function = ManifestInstance.CreateInstance ();
        static readonly StandardInstance genericFunction = ManifestInstance.CreateInstance ();
        static readonly StandardInstance metaobject = ManifestInstance.CreateInstance ();
        static readonly StandardInstance method = ManifestInstance.CreateInstance ();
        static readonly StandardInstance methodCombinationClass = ManifestInstance.CreateInstance ();
        static readonly StandardInstance nullClass = ManifestInstance.CreateInstance ();
        static readonly StandardInstance runtimeType = ManifestInstance.CreateInstance();
        static readonly StandardInstance slotDefinition = ManifestInstance.CreateInstance ();
        static readonly StandardInstance specializer = ManifestInstance.CreateInstance ();
        static readonly StandardInstance standardAccessorMethodClass = ManifestInstance.CreateInstance ();
        static readonly StandardInstance standardClass = ManifestInstance.CreateInstance ();
        static readonly StandardInstance standardDirectSlotDefinition = ManifestInstance.CreateInstance ();
        static readonly StandardInstance standardEffectiveSlotDefinition = ManifestInstance.CreateInstance ();
        static readonly StandardInstance standardGenericFunction = ManifestInstance.CreateInstance ();
        static readonly StandardInstance standardMethod = ManifestInstance.CreateInstance ();
        static readonly StandardInstance standardObjectClass = ManifestInstance.CreateInstance ();
        static readonly StandardInstance standardReaderMethod = ManifestInstance.CreateInstance ();
        static readonly StandardInstance standardSlotDefinition = ManifestInstance.CreateInstance ();
        static readonly StandardInstance standardWriterMethod = ManifestInstance.CreateInstance ();
        static readonly StandardInstance symbolClass = ManifestInstance.CreateInstance ();
        static readonly StandardInstance top = ManifestInstance.CreateInstance ();

        // The MOP generic functions.

        static readonly StandardInstance accessorMethodSlotDefinition = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance addDependent = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance addDirectMethod = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance addDirectSubclass = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance addMethod = ManifestInstance.CreateFuncallableInstance ();

        delegate object AllocateInstanceMethodSignature (NextMethodFunction callNextMethod, StandardInstance closClass, params object [] initargs);
        static readonly StandardInstance allocateInstance = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance changeClass = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance classDefaultInitargs = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance classDirectDefaultInitargs = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance classDirectSlots = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance classDirectSubclasses = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance classDirectSuperclasses = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance className = ManifestInstance.CreateFuncallableInstance ();
        delegate ConsList<StandardInstance> ClassPrecedenceListMethodSignature (NextMethodFunction callNextMethod, StandardInstance closClass);
        static readonly StandardInstance classPrecedenceList = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance classPrototype = ManifestInstance.CreateFuncallableInstance ();
        delegate ICollection<StandardInstance> ClassSlotsMethodSignature (NextMethodFunction callNextMethod, StandardInstance closClass);
        static readonly StandardInstance classSlots = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance computeApplicableMethods = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance computeApplicableMethodsUsingClasses = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance computeClassPrecedenceList = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance computeDefaultInitargs = ManifestInstance.CreateFuncallableInstance ();

        delegate FuncallHandler ComputeDiscriminatingFunctionMethodSignature (NextMethodFunction callNextMethod, StandardInstance genericFunction);
        static readonly StandardInstance computeDiscriminatingFunction = ManifestInstance.CreateFuncallableInstance ();

        static readonly StandardInstance computeEffectiveMethod = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance computeEffectiveSlotDefinition = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance computeSlots = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance computeMethodMoreSpecific = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance defaultInitargs = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance directSlotDefinitionClass = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance effectiveSlotDefinitionClass = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance ensureClass = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance ensureClassUsingClass = ManifestInstance.CreateFuncallableInstance ();

        delegate StandardInstance EnsureGenericFunctionMethodSignature (NextMethodFunction callNextMethod, object functionName, params object [] arguments); 
        static readonly StandardInstance ensureGenericFunction = ManifestInstance.CreateFuncallableInstance ();
        delegate StandardInstance EnsureGenericFunctionUsingClassMethodSignature (NextMethodFunction callNextMethod, object existingGF, object functionName, params object [] arguments); 
        static readonly StandardInstance ensureGenericFunctionUsingClass = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance eqlSpecializerObject = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance extractLambdaList = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance extractSpecializerNames = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance finalizeInheritance = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance findClass = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance findMethod = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance findMethodCombination = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance functionKeywords = ManifestInstance.CreateFuncallableInstance ();
        static public readonly StandardInstance genericFunctionApplicationCache = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance genericFunctionArgumentPrecedenceOrder = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance genericFunctionDeclarations = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance genericFunctionLambdaList = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance genericFunctionMethodClass = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance genericFunctionMethodCombination = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance genericFunctionMethods = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance genericFunctionName = ManifestInstance.CreateFuncallableInstance ();
        static public readonly StandardInstance genericFunctionSingletonsList = ManifestInstance.CreateFuncallableInstance ();

        delegate object InitializeInstanceMethodSignature (NextMethodFunction callNextMethod, StandardInstance instance, params object [] initargs);
        static readonly StandardInstance initializeInstance = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance internEqlSpecializer = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance isClassFinalized = ManifestInstance.CreateFuncallableInstance ();

        delegate object MakeInstanceMethodSignature (NextMethodFunction callNextMethod, StandardInstance closClass, params object [] initargs);
        static readonly StandardInstance makeInstance = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance makeInstancesObsolete = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance makeLoadForm = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance makeMethodLambda = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance mapDependents = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance methodCombination = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance methodFunction = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance methodGenericFunction = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance methodLambdaList = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance methodSpecializers = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance methodQualifier = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance noApplicableMethod = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance noNextMethod = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance readerMethodClass = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance reinitializeInstance = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance removeDependents = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance removeDirectMethod = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance removeDirectSubclass = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance removeMethod = ManifestInstance.CreateFuncallableInstance ();

        delegate object SharedInitializeMethodSignature (NextMethodFunction callNextMethod, StandardInstance instance, object slotNames, params object [] initargs);
        static readonly StandardInstance sharedInitialize = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance setClassDirectSlots = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance setClassDirectSuperclasses = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance setClassName = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance setClassSlots = ManifestInstance.CreateFuncallableInstance ();
        static internal readonly StandardInstance setGenericFunctionApplicationCache = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance setGenericFunctionName = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance setSlotValueUsingClass = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance isSlotBoundUsingClass = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance slotDefinitionAllocation = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance slotDefinitionInitargs = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance slotDefinitionInitform = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance slotDefinitionInitfunction = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance slotDefinitionLocation = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance slotDefinitionName = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance slotDefinitionType = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance slotDefinitionReaders = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance slotDefinitionWriters = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance slotMakunboundUsingClass = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance slotMissing = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance slotUnbound = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance slotValueUsingClass = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance specializerDirectGenericFunctions = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance specializerDirectMethods = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance standardAccessorMethod = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance updateDependent = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance updateInstanceForDifferentClass = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance updateInstanceForRedefinedClass = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance validateSuperclass = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardInstance writerMethodClass = ManifestInstance.CreateFuncallableInstance ();

        static readonly StandardInstance standardClassEqlSpecializer = ManifestInstance.CreateInstance ();

        static readonly StandardInstance method0ForAllocateInstance = ManifestInstance.CreateInstance();
        static readonly StandardInstance method0ForClassPrecedenceList = ManifestInstance.CreateInstance ();
        static readonly StandardInstance method1ForClassPrecedenceList = ManifestInstance.CreateInstance ();
        static readonly StandardInstance method0ForClassSlots = ManifestInstance.CreateInstance ();
        static readonly StandardInstance method1ForClassSlots = ManifestInstance.CreateInstance ();
        static readonly StandardInstance method2ForClassSlots = ManifestInstance.CreateInstance ();
        static readonly StandardInstance method3ForClassSlots = ManifestInstance.CreateInstance ();
        static readonly StandardInstance methodForComputeDiscriminatingFunction = ManifestInstance.CreateInstance ();
        static readonly StandardInstance method0ForEnsureGenericFunction = ManifestInstance.CreateInstance();
        static readonly StandardInstance method1ForEnsureGenericFunction = ManifestInstance.CreateInstance();
        static readonly StandardInstance method0ForEnsureGenericFunctionUsingClass = ManifestInstance.CreateInstance();
        static readonly StandardInstance method0ForInitializeInstance = ManifestInstance.CreateInstance();
        static readonly StandardInstance method0ForMakeInstance = ManifestInstance.CreateInstance();
        static readonly StandardInstance method0ForSharedInitialize = ManifestInstance.CreateInstance();

        #endregion Allocation

        static internal readonly List<StandardInstance> GenericInvocationGenerics =
            new List<StandardInstance> {computeDiscriminatingFunction,
                                      computeApplicableMethods,
                                      computeEffectiveMethod,
                                      computeMethodMoreSpecific};
        static internal Cons GenericApplicationCacheTag = new Cons (null, null);

        static readonly Dictionary<Symbol, StandardInstance> allNamedClasses = new Dictionary<Symbol, StandardInstance> ();
        static readonly Dictionary<Symbol, StandardInstance> allNamedGenericFunctions = new Dictionary<Symbol, StandardInstance> ();

        static readonly Dictionary<Type, StandardInstance> builtInClassMapping = new Dictionary<Type, StandardInstance> ();

        static Type standinForNull = typeof(ClassForNull);

        static StandardInstance FindDotnetClass (Type dotnetType)
        {
            StandardInstance closClass = null;            
            if (builtInClassMapping.TryGetValue (dotnetType == null ? standinForNull : dotnetType, out closClass))
                return closClass;
            StandardInstance metaclass = FindDotnetClass (dotnetType.GetType());
            StandardInstance superclass = FindDotnetClass (dotnetType.BaseType);
            StandardInstance builtIn = ManifestInstance.CreateInstance();
            bootstrapInitializeBuiltInClass (metaclass, builtIn,
                                             "",
                                             closSymbol (dotnetType.Name),
                                             dotnetType.FullName,
                                             null,
                                             null, 
                                             CL.List<StandardInstance>(superclass),
                                             new ConsList<StandardInstance>(builtIn, (ConsList<StandardInstance>) ClassPrecedenceList (superclass)),
                                             dotnetType);
           return builtIn;
        }

        public static StandardInstance ClassOf (object instance)
        {
            if (instance == null)
                return nullClass;
            StandardInstance soInstance = instance as StandardInstance;
            return (soInstance == null)
                ? FindDotnetClass (instance.GetType())
                : soInstance.Class();
        }

        static bool IsInstanceOf (object item, StandardInstance closClass)
        {
            if (closClass.Equals (Top))
                return true;
            StandardInstance cx = ClassOf (item);
            if (cx.Equals (closClass))
                return true;
            else {
                return CL.Memq<StandardInstance> (closClass, (ConsList<StandardInstance>) CLOS.ClassPrecedenceList (cx)) != null;
            }
        }

        static bool InstancesOf (Cons items, ConsList<StandardInstance> classes)
        {
            return (items == null)
                || (classes == null)
                || (IsInstanceOf (items.Car, classes.Car) 
                    && InstancesOf ((Cons) items.Cdr, classes.Cdr));
        }

        delegate bool MethodCompare2 (StandardInstance left, StandardInstance right);
        delegate bool MethodCompare3 (StandardInstance left, StandardInstance right, Cons arglist);

        static ConsList<StandardInstance> MergeMethods (ConsList<StandardInstance> left, ConsList<StandardInstance> right, MethodCompare2 comp)
        {
            ConsList<StandardInstance> result = null;
            while (true) {
                if (left == null) {
                    if (right == null)
                        break;
                    else {
                        result = new ConsList<StandardInstance> (right.Car, result);
                        right =  right.Cdr;
                    }
                }
                else {
                    if (right == null) {
                        result = new ConsList<StandardInstance> (left.Car, result);
                        left =  left.Cdr;
                    }
                    else if (comp (left.Car, right.Car)) {
                        result = new ConsList<StandardInstance> (left.Car, result);
                        left =  left.Cdr;
                    }
                    else {
                        result = new ConsList<StandardInstance> (right.Car, result);
                        right =  right.Cdr;
                    }
                }
            }

            return CL.Reverse (result);
        }

        static ConsList<StandardInstance> SortMethods (ConsList<StandardInstance> methodList, MethodCompare2 comp)
        {
            if (methodList == null)
                return null;
            if (methodList.Cdr == null)
                return methodList;
            ConsList<StandardInstance> left = null;
            ConsList<StandardInstance> right = null;
            while (methodList != null) {
                ConsList<StandardInstance> temp = new ConsList<StandardInstance> (methodList.Car, left);
                left = right;
                right = temp;
                methodList = methodList.Cdr;
            }
            return MergeMethods (SortMethods (left, comp), SortMethods (right, comp), comp);
        }

        static MethodCompare2 MakeSortPredicate (MethodCompare3 internalCompare, Cons arglist)
        {
            return new MethodCompare2 ((left, right) => internalCompare (left, right, arglist));
        }

        delegate bool MethodPredicate (StandardInstance method);

        static ConsList<StandardInstance> FilterMethods (MethodPredicate mp, ConsList<StandardInstance> methodList)
        {
            if (methodList == null)
                return null;
            StandardInstance thisMethod =  methodList.Car;
            if (mp (thisMethod))
                return new ConsList<StandardInstance> (thisMethod, FilterMethods (mp, methodList.Cdr));
            else
                return FilterMethods (mp, methodList.Cdr);
        }

        static ConsList<StandardInstance> methodImplForComputeApplicableMethods (NextMethodFunction callNextMethod,
                                                           StandardInstance genericFunction,
                                                           params object [] arguments)
        {
            Cons arglist = Cons.SubvectorToList (arguments, 0, arguments.Length);
            ConsList<StandardInstance> methods = (ConsList<StandardInstance>) GenericFunctionMethods (genericFunction);
            ConsList<StandardInstance> filteredMethods = FilterMethods ((m) => InstancesOf (arglist, (ConsList<StandardInstance>) MethodSpecializers (m)), methods);
            ConsList<StandardInstance> sortedMethods = SortMethods (filteredMethods,
                                              MakeSortPredicate ((MethodCompare3) ComputeMethodMoreSpecific (genericFunction), arglist));
            return sortedMethods;
        }

        static NextMethodFunction oneAroundStep (Cons methods, Cons args)
        {
            throw new NotImplementedException ();
        }

        static NextMethodFunction methodImplForComputeEffectiveMethod (NextMethodFunction callNextMethod, StandardInstance generic, ConsList<StandardInstance> methodList)
        {
            Cons primaryMethods = null;
            Cons aroundMethods = null;
            ConsList<StandardInstance> beforeMethods = null;
            ConsList<StandardInstance> afterMethods = null;

            MethodStepper oneStep = new MethodStepper (generic);

            while (methodList != null) {
                StandardInstance method = methodList.Car;
                methodList = methodList.Cdr;
                Symbol q = (Symbol) MethodQualifier (method);
                if (q.Equals (KW.Primary)) {
                    primaryMethods = new Cons (new Cons (method, MethodStepWrapper.Create ((Delegate) MethodFunction (method))), primaryMethods);
                }
                else
                    throw new NotImplementedException ();
            }
            primaryMethods = CL.Reverse (primaryMethods);

            if (primaryMethods == null)
                throw new NotImplementedException ("no applicable method");
            else if (beforeMethods == null
                && afterMethods == null
                && aroundMethods == null)
                return oneStep.Step (primaryMethods, null);
            else
                return oneAroundStep (aroundMethods, null);
        }

        static bool IsMoreSpecific (StandardInstance left, StandardInstance right, object arg)
        {
            ConsList<StandardInstance> cc1 = CL.Memq<StandardInstance> (left, (ConsList<StandardInstance>) ClassPrecedenceList (ClassOf (arg)));
            return (cc1 != null)
                && CL.Memq (right, cc1.Cdr) != null;
        }

        static bool computeMethodMoreSpecificLoop (StandardInstance generic, StandardInstance left, StandardInstance right, Cons arglist)
        {
            Cons speclsLeft = (Cons) MethodSpecializers (left);
            Cons speclsRight = (Cons) MethodSpecializers (right);
            while (true) {
                if ((speclsLeft == null) && (speclsRight == null)) {
                    if (CL.Eq (MethodQualifier (left),
                               MethodQualifier (right)))
                        throw new NotImplementedException ("equally specific");
                    else
                        return false;
                }
                else if (arglist == null) {
                    throw new NotImplementedException ("fewer args than specializers");
                }
                else if (speclsLeft == null) {
                    if (CL.Eq (CL.Car (speclsRight), Top)) {
                        speclsRight = (Cons) CL.Cdr (speclsRight);
                        arglist = (Cons) CL.Cdr (arglist);
                        continue;
                    }
                    return false;
                }
                else if (speclsRight == null) {
                    if (CL.Eq (CL.Car (speclsLeft), Top)) {
                        speclsLeft = (Cons) CL.Cdr (speclsLeft);
                        arglist = (Cons) CL.Cdr (arglist);
                        continue;
                    }
                    return true;
                }
                else {
                    StandardInstance c1 = (StandardInstance) CL.Car (speclsLeft);
                    StandardInstance c2 = (StandardInstance) CL.Car (speclsRight);
                    if (CL.Eq (c1, c2)) {
                        speclsLeft = (Cons) CL.Cdr (speclsLeft);
                        speclsRight = (Cons) CL.Cdr (speclsRight);
                        arglist = (Cons) CL.Cdr (arglist);
                        continue;
                    }
                    else {
                        return IsMoreSpecific (c1, c2, CL.Car (arglist));
                    }
                }
            }
        }

        static MethodCompare3 methodImplForComputeMethodMoreSpecific (NextMethodFunction callNextMethod, StandardInstance generic)
        {
            return new MethodCompare3 ((left, right, arglist) => computeMethodMoreSpecificLoop (generic, left, right, arglist));
        }

        static StandardInstance bootstrapInitializeBuiltInClass (StandardInstance metaclass,
                                                               StandardInstance closClass,
                                                               string documentation,
                                                               Symbol name,
                                                               string studlyName,
                                                               ConsList<StandardInstance> directMethods,
                                                               ConsList<StandardInstance> directSubclasses,
                                                               ConsList<StandardInstance> directSuperclasses,
                                                               ConsList<StandardInstance> precedenceList,
                                                               Type dotnetType)
        {
            closClass.SetClass (metaclass);
            closClass.SetSlots (
                new object [] {
                    documentation,
                    name,
                    studlyName,
                    directMethods,
                    directSubclasses,
                    directSuperclasses,
                    precedenceList,
                    dotnetType
                    });
            builtInClassMapping.Add (dotnetType == null ? standinForNull : dotnetType, closClass);
            allNamedClasses.Add (name, closClass);
            return closClass;
        }

        static StandardInstance bootstrapInitializeEqlSpecializer (StandardInstance specializer,
                                                                 StandardInstance closClass)
        {
            specializer.SetClass (eqlSpecializer);
            specializer.SetSlots (
                new object [] {
                    "An EQL specializer.",
                    QuoteEqlSpecializer,
                    "AnEqlSpecializer",
                    closClass});
            return specializer;
        }

        static StandardInstance bootstrapInitializeStandardClass (StandardInstance closClass,
                                                                string documentation,
                                                                Symbol name,
                                                                string studlyName,
                                                                ConsList<StandardInstance> directMethods,
                                                                ConsList<StandardInstance> directSubclasses,
                                                                ConsList<StandardInstance> directSuperclasses,
                                                                ConsList<StandardInstance> precedenceList,
                                                                Cons defaultInitargs,
                                                                Cons directDefaultInitargs,
                                                                ConsList<StandardInstance> directSlots,
                                                                StandardInstance prototype,
                                                                StandardInstance [] effectiveSlots)
        {
            closClass.SetClass (standardClass);
            closClass.SetSlots (
                new object [] {
                    documentation,
                    name,
                    studlyName,
                    directMethods,
                    directSubclasses,
                    directSuperclasses,
                    precedenceList,
                    defaultInitargs,
                    directDefaultInitargs,
                    directSlots,
                    prototype,
                    effectiveSlots
                    });
            allNamedClasses.Add (name, closClass);
            return closClass;
        }

        static StandardInstance bootstrapInitializeFuncallableStandardClass (StandardInstance closClass,
                                                                           string documentation,
                                                                           Symbol name,
                                                                           string studlyName,
                                                                           ConsList<StandardInstance> directMethods,
                                                                           ConsList<StandardInstance> directSubclasses,
                                                                           ConsList<StandardInstance> directSuperclasses,
                                                                           ConsList<StandardInstance> precedenceList,
                                                                           Cons defaultInitargs,
                                                                           Cons directDefaultInitargs,
                                                                           ConsList<StandardInstance> directSlots,
                                                                           StandardInstance prototype,
                                                                           StandardInstance [] effectiveSlots)
        {
            closClass.SetClass (funcallableStandardClass);
            closClass.SetSlots (
                new object [] {
                    documentation,
                    name,
                    studlyName,
                    directMethods,
                    directSubclasses,
                    directSuperclasses,
                    precedenceList,
                    defaultInitargs,
                    directDefaultInitargs,
                    directSlots,
                    prototype,
                    effectiveSlots
                    });
            allNamedClasses.Add (name, closClass);
            return closClass;
        }

        static Cons bootstrapGenericFunctionApplicationCache (StandardInstance self, object [] arguments)
        {
            return (Cons) CLOS.SlotValue ((StandardInstance) arguments [0], QuoteApplicationCache);
        }

        static Cons bootstrapSetGenericFunctionApplicationCache (StandardInstance self, object [] arguments)
        {
            return (Cons) CLOS.SetSlotValue (arguments [1], (StandardInstance) arguments[0], QuoteApplicationCache);
        }

        static Cons bootstrapGenericFunctionLambdaList (StandardInstance self, object [] arguments)
        {
            return (Cons) CLOS.SlotValue ((StandardInstance) arguments [0], QuoteLambdaList);
        }

        static ConsList<StandardInstance> bootstrapGenericFunctionMethods (StandardInstance self, object [] arguments)
        {
            return (ConsList<StandardInstance>) CLOS.SlotValue ((StandardInstance) arguments [0], QuoteMethods);
        }

        static ConsList<ICollection<object>> bootstrapGenericFunctionSingletonsList (StandardInstance self, object [] arguments)
        {
            return (ConsList<ICollection<object>>) CLOS.SlotValue ((StandardInstance) arguments [0], QuoteSingletonsList);
        }

        static Delegate bootstrapMethodFunction (StandardInstance self, object [] arguments)
        {
            return (Delegate) CLOS.SlotValue ((StandardInstance) arguments [0], QuoteFunction);
        }

        static Symbol bootstrapMethodQualifier (StandardInstance self, object [] arguments)
        {
            return (Symbol) CLOS.SlotValue ((StandardInstance) arguments [0], QuoteQualifier);
        }

        static ConsList<StandardInstance> bootstrapMethodSpecializers (StandardInstance self, object [] arguments)
        {
            return (ConsList<StandardInstance>) CLOS.SlotValue ((StandardInstance) arguments [0], QuoteSpecializers);
        }

        static StandardInstance bootstrapInitializeStandardMethod (StandardInstance method, 
                                                                 string documentation,
                                                                 Symbol name,
                                                                 string studlyName,
                                                                 Delegate function,
                                                                 Symbol qualifier,
                                                                 ConsList<StandardInstance> specializers)
        {
            method.SetClass (StandardMethod);
            method.SetSlots (
                new object [] {
                   documentation,
                   name,
                   studlyName,
                   function,
                   qualifier,
                   specializers
                   });
            return method;
        }

        static StandardInstance bootstrapInitializeStandardGenericFunction (StandardInstance genericFunction,
                                                                          string documentation,
                                                                          Symbol name,
                                                                          string studlyName,
                                                                          ConsList<Symbol> lambdaList,
                                                                          ConsList<StandardInstance> methods,
                                                                          ConsList<ICollection<object>> singletons)
        {
            genericFunction.SetClass (StandardGenericFunction);
            genericFunction.SetSlots (
                new object [] {
                    documentation,
                    name,
                    studlyName,
                    new Cons (null, new EffectiveMethodCache()),
                    lambdaList,
                    methods, // methods
                    singletons  // singletons
                    });
            allNamedGenericFunctions.Add (name, genericFunction);
            return genericFunction;
        }

        static object bootstrapClassPrecedenceList (StandardInstance self, params object [] arguments)
        {
            StandardInstance closClass = (StandardInstance)arguments[0];
            return SlotValue (closClass, QuotePrecedenceList);
        }

        static object bootstrapClassSlots (StandardInstance self, params object [] arguments)
        {
            StandardInstance closClass = (StandardInstance) arguments [0];
            if (closClass == standardClass)
                return classSlotsMethod0Impl (null, closClass);
            if (IsInstanceOf (closClass, funcallableStandardClass))
                return SlotValue (closClass, QuoteSlots);
            if (IsInstanceOf (closClass, standardClass))
                return SlotValue (closClass, QuoteSlots);
            if (IsInstanceOf (closClass, builtInClass))
                return classSlotsMethod2Impl (null, closClass);
            throw new NotImplementedException ("classSlots");
        }

        static object bootstrapComputeApplicableMethods (StandardInstance self, object [] arguments)
        {
            object [] newArguments = new object [arguments.Length - 1];
            Array.Copy (arguments, 1, newArguments, 0, newArguments.Length);
            return methodImplForComputeApplicableMethods (null, (StandardInstance) arguments[0], newArguments);
        }

        static object bootstrapComputeDiscriminatingFunction (StandardInstance self, object [] arguments)
        {
            // self is the GenericFunction `computeDiscriminatingFunction'
            // the first argument is the generic function we are going to compute
            // the discriminating function for (eventually)
            // we first compute the discriminating function for `computeDiscriminatingFunction'
            // then we install it, and finally invoke it on the argument.
            FuncallHandler discriminatingFunction = computeDiscriminatingFunctionMethodImpl (null, self);
            setFuncallableInstanceFunction (self, discriminatingFunction);
            return discriminatingFunction.Invoke (self, arguments);
        }

        static object computeDiscriminatingFunctionTrampoline (StandardInstance gf, object [] arguments)
        {
            FuncallHandler discriminatingFunction = (FuncallHandler) ComputeDiscriminatingFunction (gf);
            setFuncallableInstanceFunction (gf, discriminatingFunction);
            return discriminatingFunction.Invoke (gf, arguments);
        }

        static object bootstrapComputeMethodMoreSpecific (StandardInstance self, object [] arguments)
        {
            return methodImplForComputeMethodMoreSpecific (null, (StandardInstance) arguments[0]);
        }

        static object bootstrapComputeEffectiveMethod (StandardInstance self, object [] arguments)
        {
            return methodImplForComputeEffectiveMethod (null, (StandardInstance) arguments[0], (ConsList<StandardInstance>) arguments[1]);
        }

        static StandardInstance bootstrapCreateBuiltInEffectiveSlot (FieldInfo dotnetField)
        {
            return ManifestInstance.CreateInstance (builtInEffectiveSlotDefinition,
                                                     new object [] {
                                                         "A built in slot.",
                                                         closSymbol (dotnetField.Name),
                                                         dotnetField.Name, 
                                                         null,
                                                         null,
                                                         null,
                                                         dotnetField });
        }

        static StandardInstance bootstrapCreateBuiltInEffectiveSlot (PropertyInfo dotnetProperty)
        {
            return ManifestInstance.CreateInstance (builtInEffectiveSlotDefinition,
                                                     new object [] {
                                                         "A built in slot.",
                                                         closSymbol (dotnetProperty.Name),
                                                         dotnetProperty.Name, 
                                                         null,
                                                         null,
                                                         null,
                                                         dotnetProperty });
        }

        static StandardInstance bootstrapCreateEffectiveSlot (string documentation,
                                                            Symbol name,
                                                            string studlyName,
                                                            Keyword allocation,
                                                            ConsList<Symbol> initargs,
                                                            object type,
                                                            SlotInitializer initializer,
                                                            int location)
        {
            if (initializer == null)
               throw new NotImplementedException ("initializer is null");
            return ManifestInstance.CreateInstance (standardEffectiveSlotDefinition,
                                                    new object [] {
                                                      documentation,
                                                      name,
                                                      studlyName,        
                                                      allocation,
                                                      initargs,
                                                      type,
                                                      initializer,
                                                      location});
        }

        static StandardInstance bootstrapCreateDocumentationSlot ()
        {
            return bootstrapCreateEffectiveSlot ("The documentation slot.",
                                                 QuoteDocumentation,
                                                 "Documentation",
                                                 KW.Instance,
                                                 CL.List<Symbol> (KW.Documentation),
                                                 null,
                                                 constantSlotInitializer (""),
                                                 0);
        }

        static StandardInstance bootstrapCreateNameSlot ()
        {
            return bootstrapCreateEffectiveSlot ("The name slot.",
                                                 QuoteName,
                                                 "Name",
                                                 KW.Instance,
                                                 CL.List<Symbol> (KW.Name),
                                                 null,
                                                 keywordSlotInitializer(CL.List<Symbol>(KW.Name)),
                                                 1);
        }

        static StandardInstance bootstrapCreateStudlyNameSlot ()
        {
            return bootstrapCreateEffectiveSlot ("The StudlyName slot.",
                                                 QuoteStudlyName,
                                                 "StudlyName",
                                                 KW.Instance,
                                                 CL.List<Symbol> (KW.StudlyName),
                                                 null,
                                                 keywordSlotInitializer(CL.List<Symbol>(KW.StudlyName)),
                                                 2);
        }

        static StandardInstance bootstrapCreateDirectMethodsSlot ()
        {
            return bootstrapCreateEffectiveSlot ("The direct methods slot.",
                                                 QuoteDirectMethods,
                                                 "DirectMethodsSlot",
                                                 KW.Instance,
                                                 null,  // CL.List<Symbol> (),
                                                 null,
                                                 constantSlotInitializer(null),
                                                 3);
        }

        static StandardInstance bootstrapCreateDirectSubclassesSlot ()
        {
            return bootstrapCreateEffectiveSlot ("",
                                                 null,
                                                 "DirectSubclassesSlot",
                                                 KW.Instance,
                                                 null,  // CL.List<Symbol> (),
                                                 null,
                                                 constantSlotInitializer(null),
                                                 4);
        }

        static StandardInstance bootstrapCreateDirectSuperclassesSlot ()
        {
            return bootstrapCreateEffectiveSlot ("The direct superclasses slot.",
                                                 QuoteDirectSuperclasses,
                                                 "DirectSuperclassesSlot",
                                                 KW.Instance,
                                                 CL.List<Symbol> (KW.DirectSuperclasses),
                                                 null,
                                                 keywordSlotInitializer(CL.List<Symbol>(KW.DirectSuperclasses)),
                                                 5);
        }

        static StandardInstance bootstrapCreatePrecedenceListSlot ()
        {
            return bootstrapCreateEffectiveSlot ("The class precedence list slot.",
                                                 QuotePrecedenceList,
                                                 "PrecedenceListSlot",
                                                 KW.Instance,
                                                 null, // null, // CL.List<Symbol> (),
                                                 null,
                                                 unboundSlotInitializer(),
                                                 6);
        }

        static StandardInstance bootstrapCreateDefaultInitargsSlot ()
        {
            return bootstrapCreateEffectiveSlot ("The direct methods slot.",
                                                 QuoteDirectMethods,
                                                 "DirectMethodsSlot",
                                                 KW.Instance,
                                                 null,  // CL.List<Symbol> (),
                                                 null,
                                                 unboundSlotInitializer(),
                                                 7);
        }

        static StandardInstance bootstrapCreateDirectDefaultInitargsSlot ()
        {
            return bootstrapCreateEffectiveSlot ("The direct methods slot.",
                                                 QuoteDirectMethods,
                                                 "DirectMethodsSlot",
                                                 KW.Instance,
                                                 null, // null, // CL.List<Symbol> (),
                                                 null,
                                                 unboundSlotInitializer(),
                                                 8);
        }

        static StandardInstance bootstrapCreateDirectSlotsSlot ()
        {
            return bootstrapCreateEffectiveSlot ("The direct methods slot.",
                                                 QuoteDirectMethods,
                                                 "DirectMethodsSlot",
                                                 KW.Instance,
                                                 null, // null, // CL.List<Symbol> (),
                                                 null,
                                                 keywordSlotInitializer(CL.List<Symbol>(KW.DirectSlots)),
                                                 9);
        }

        static StandardInstance bootstrapCreatePrototypeSlot ()
        {
            return bootstrapCreateEffectiveSlot ("The direct methods slot.",
                                                 QuoteDirectMethods,
                                                 "DirectMethodsSlot",
                                                 KW.Instance,
                                                 null,  // CL.List<Symbol> (),
                                                 null,
                                                 unboundSlotInitializer(),
                                                 10);
        }

        static StandardInstance bootstrapCreateApplicationCacheSlot ()
        {
            return bootstrapCreateEffectiveSlot ("The application cache slot.",
                                                 QuoteApplicationCache,
                                                 "ApplicationCache",
                                                 KW.Instance,
                                                 null, // null, // CL.List<Symbol> (),
                                                 null,
                                                 unboundSlotInitializer(),
                                                 3);
        }

        static StandardInstance bootstrapCreateLambdaListSlot ()
        {
            return bootstrapCreateEffectiveSlot ("The lambda list slot.",
                                                 QuoteLambdaList,
                                                 "LambdaList",
                                                 KW.Instance,
                                                 null, // CL.List<Symbol> (),
                                                 null,
                                                 keywordSlotInitializer(CL.List<Symbol>(KW.LambdaList)),
                                                 4);
        }

        static StandardInstance bootstrapCreateMethodsSlot ()
        {
            return bootstrapCreateEffectiveSlot ("The methods slot.",
                                                 QuoteMethods,
                                                 "Methods",
                                                 KW.Instance,
                                                 null, // CL.List<Symbol> (),
                                                 null,
                                                 constantSlotInitializer(null),
                                                 5);
        }

        static StandardInstance bootstrapCreateSingletonsListSlot ()
        {
            return bootstrapCreateEffectiveSlot ("The singletons list slot.",
                                                 QuoteSingletonsList,
                                                 "SingletonsList",
                                                 KW.Instance,
                                                 null, // CL.List<Symbol> (),
                                                 null,
                                                 constantSlotInitializer(null),
                                                 6);
        }

        static StandardInstance bootstrapCreateFunctionSlot ()
        {
            return bootstrapCreateEffectiveSlot ("The function slot.",
                                                 QuoteFunction,
                                                 "FunctionSlot",
                                                 KW.Instance,
                                                 null, // CL.List<Symbol> (),
                                                 null,
                                                 unboundSlotInitializer(),
                                                 3);
        }

        static StandardInstance bootstrapCreateQualifierSlot ()
        {
            return bootstrapCreateEffectiveSlot ("The qualifiers slot.",
                                                 QuoteQualifier,
                                                 "Qualifier",
                                                 KW.Instance,
                                                 null, // CL.List<Symbol> (),
                                                 null,
                                                 keywordSlotInitializer(CL.List<Symbol>(KW.Qualifier)),
                                                 4);
        }

        static StandardInstance bootstrapCreateSpecializersSlot ()
        {
            return bootstrapCreateEffectiveSlot ("The specializers slot.",
                                                 QuoteSpecializers,
                                                 "SpecializerSlot",
                                                 KW.Instance,
                                                 null, // CL.List<Symbol> (),
                                                 null,
                                                 keywordSlotInitializer (CL.List<Symbol>(KW.Specializers)),
                                                 5);
        }

        static Symbol bootstrapSlotDefinitionName (StandardInstance self, object [] arguments)
        {
            StandardInstance slotDefinition = (StandardInstance) arguments [0];
            return (slotDefinition.Class() == standardEffectiveSlotDefinition)
                   ? (Symbol) StandardInstanceAccess (slotDefinition, 1)
                   : (Symbol) SlotValue (slotDefinition, QuoteName);
        }

        static object bootstrapSlotDefinitionLocation (StandardInstance self, object [] arguments)
        {
            StandardInstance slotDefinition = (StandardInstance) arguments [0];
            return
                (slotDefinition.Class() == standardEffectiveSlotDefinition)
                ? StandardInstanceAccess (slotDefinition, 7)
                : SlotValue (slotDefinition, QuoteLocation);
        }

        static object bootstrapSlotValueUsingClass (StandardInstance self, object [] arguments)
        {
            StandardInstance instance = (StandardInstance) arguments [1];
            StandardInstance effectiveSlot = (StandardInstance) arguments [2];
            return StandardInstanceAccess (instance, (int) slotDefinitionLocation (effectiveSlot));
        }

        static object bootstrapSetSlotValueUsingClass (StandardInstance self, object [] arguments)
        {
            object newValue = arguments[0];
            StandardInstance instance = (StandardInstance) arguments [2];
            StandardInstance effectiveSlot = (StandardInstance) arguments [3];
            return SetStandardInstanceAccess (instance, (int) SlotDefinitionLocation (effectiveSlot), newValue);
        }

        static StandardInstance [] theEffectiveSlotsOfBuiltInClass = 
            new StandardInstance [] {
                bootstrapCreateDocumentationSlot (),
                bootstrapCreateNameSlot (),
                bootstrapCreateStudlyNameSlot (),
                bootstrapCreateDirectMethodsSlot(),
                bootstrapCreateDirectSubclassesSlot(),
                bootstrapCreateDirectSuperclassesSlot(),
                bootstrapCreatePrecedenceListSlot(),
                bootstrapCreateEffectiveSlot ("The dotnet type slot",
                                             QuoteDotnetType,
                                             "DotnetType",
                                             KW.Instance,
                                             null,
                                             null,
                                             unboundSlotInitializer(),
                                             7
                                             )};

        static StandardInstance [] theEffectiveSlotsOfEqlSpecializer =
            new StandardInstance [] {
                bootstrapCreateDocumentationSlot (),
                bootstrapCreateNameSlot (),
                bootstrapCreateStudlyNameSlot (),
                bootstrapCreateEffectiveSlot ("The object slot",
                                             QuoteDirectMethods,
                                             "DirectMethods",
                                             KW.Instance,
                                             null,
                                             null,
                                             unboundSlotInitializer(),
                                             3
                                             )
                };

        static StandardInstance [] theEffectiveSlotsOfStandardClass =
            new StandardInstance [] {
                bootstrapCreateDocumentationSlot (),
                bootstrapCreateNameSlot (),
                bootstrapCreateStudlyNameSlot (),
                bootstrapCreateDirectMethodsSlot(),
                bootstrapCreateDirectSubclassesSlot(),
                bootstrapCreateDirectSuperclassesSlot(),
                bootstrapCreatePrecedenceListSlot(),
                bootstrapCreateEffectiveSlot ("The direct methods slot",
                                             QuoteDirectMethods,
                                             "DirectMethods",
                                             KW.Instance,
                                             null,
                                             null,
                                             constantSlotInitializer(null),
                                             3
                                             ),
                bootstrapCreateEffectiveSlot ("The direct subclasses slot",
                                             QuoteDirectSubclasses,
                                             "DirectSubclasses",
                                             KW.Instance,
                                             null,
                                             null,
                                             constantSlotInitializer(null),
                                             4
                                             ),
                bootstrapCreateEffectiveSlot ("The direct superclasses slot",
                                             QuoteDirectMethods,
                                             "DirectSuperclasses",
                                             KW.Instance,
                                             null,
                                             null,
                                             keywordSlotInitializer(CL.List<Symbol>(KW.DirectSuperclasses)),
                                             5
                                             ),
                bootstrapCreateEffectiveSlot ("The precedence list slot",
                                             QuotePrecedenceList,
                                             "PrecedenceList",
                                             KW.Instance,
                                             null,
                                             null,
                                             unboundSlotInitializer(),
                                             6
                                             ),
                bootstrapCreateEffectiveSlot ("The default initargs slot",
                                             QuoteDefaultInitargs,
                                             "DefaultInitargs",
                                             KW.Instance,
                                             null,
                                             null,
                                             unboundSlotInitializer(),
                                             7
                                             ),
                bootstrapCreateEffectiveSlot ("The direct default initargs slot",
                                             QuoteDirectDefaultInitargs,
                                             "DirectDefaultInitargs",
                                             KW.Instance,
                                             null,
                                             null,
                                             unboundSlotInitializer(),
                                             8
                                             ),

                bootstrapCreateEffectiveSlot ("The direct slots slot",
                                             QuoteDirectSlots,
                                             "DirectSlots",
                                             KW.Instance,
                                             null,
                                             null,
                                             keywordSlotInitializer(CL.List<Symbol>(KW.DirectSlots)),
                                             9
                                             ),

                bootstrapCreateEffectiveSlot ("The prototype slot",
                                             QuotePrototype,
                                             "Prototype",
                                             KW.Instance,
                                             null,
                                             null,
                                             unboundSlotInitializer(),
                                             10
                                             ),
                bootstrapCreateEffectiveSlot ("The effective slots slot",
                                             QuoteSlots,
                                             "Slots",
                                             KW.Instance,
                                             null,
                                             null,
                                             unboundSlotInitializer(),
                                             11
                                             )
                };          // effectiveSlots


        static StandardInstance [] theEffectiveSlotsOfStandardMethod =
               new StandardInstance [] {
                bootstrapCreateDocumentationSlot (),
                bootstrapCreateNameSlot (),
                bootstrapCreateStudlyNameSlot (),
                bootstrapCreateEffectiveSlot ("The function slot",
                                              QuoteFunction,
                                              "Function",
                                              KW.Instance,
                                              null,
                                              null,
                                              unboundSlotInitializer(),
                                              3),
                bootstrapCreateEffectiveSlot ("The qualifier slot",
                                              QuoteQualifier,
                                              "Qualifier",
                                              KW.Instance,
                                              null,
                                              null,
                                              keywordSlotInitializer(CL.List<Symbol>(KW.Qualifier)),
                                              4),
                bootstrapCreateEffectiveSlot ("The specializers slot",
                                              QuoteSpecializers,
                                              "Specializers",
                                              KW.Instance,
                                              null,
                                              null,
                                              keywordSlotInitializer(CL.List<Symbol>(KW.Specializers)),
                                              5)
               
                 };          // effectiveSlots


        static public bool initializeClos ()
        {

             // Fill in the above.
            
            // First, stuff in some bootstrap code so we can call a few of these
            // generic functions.
            setFuncallableInstanceFunction (classPrecedenceList, bootstrapClassPrecedenceList);
            setFuncallableInstanceFunction (classSlots, bootstrapClassSlots);
            setFuncallableInstanceFunction (computeApplicableMethods, bootstrapComputeApplicableMethods); 
            setFuncallableInstanceFunction (computeDiscriminatingFunction, bootstrapComputeDiscriminatingFunction);
            setFuncallableInstanceFunction (computeEffectiveMethod, bootstrapComputeEffectiveMethod);
            setFuncallableInstanceFunction (computeMethodMoreSpecific, bootstrapComputeMethodMoreSpecific);
            setFuncallableInstanceFunction (genericFunctionApplicationCache, bootstrapGenericFunctionApplicationCache);
            setFuncallableInstanceFunction (genericFunctionLambdaList, bootstrapGenericFunctionLambdaList);
            setFuncallableInstanceFunction (genericFunctionMethods, bootstrapGenericFunctionMethods);
            setFuncallableInstanceFunction (genericFunctionSingletonsList, bootstrapGenericFunctionSingletonsList);
            setFuncallableInstanceFunction (methodFunction, bootstrapMethodFunction);
            setFuncallableInstanceFunction (methodSpecializers, bootstrapMethodSpecializers);
            setFuncallableInstanceFunction (methodQualifier, bootstrapMethodQualifier);
            setFuncallableInstanceFunction (setGenericFunctionApplicationCache, bootstrapSetGenericFunctionApplicationCache);
            setFuncallableInstanceFunction (setSlotValueUsingClass, bootstrapSetSlotValueUsingClass);
            setFuncallableInstanceFunction (slotDefinitionLocation, bootstrapSlotDefinitionLocation);
            setFuncallableInstanceFunction (slotDefinitionName, bootstrapSlotDefinitionName);
            setFuncallableInstanceFunction (slotValueUsingClass, bootstrapSlotValueUsingClass);

            bootstrapInitializeStandardClass (builtInClass,
                "The superclass of all non CLOS classes.",
                QuoteBuiltInClass,
                "BuiltInClass",

                null,  // CL.List<StandardInstance> (),          // directMethods
                null, // null, // CL.List<StandardInstance> (),          // directSubclasses
                null, //CL.List<StandardInstance> (closClass), // directSuperclasses
                CL.List<StandardInstance> (builtInClass, closClass, specializer, metaobject, standardObjectClass, top),       // precedenceList

                null,                                // defaultInitargs
                null,                                // directDefaultInitargs
                null,  // CL.List<StandardInstance> (),          // directSlots
                ManifestInstance.CreateInstance (),  // prototype
                theEffectiveSlotsOfBuiltInClass
                );

           bootstrapInitializeStandardClass (builtInEffectiveSlotDefinition,
                "Class of dotnet-defined effective slot definitions.",
                QuoteBuiltInEffectiveSlotDefinition,
                "BuiltInEffectiveSlotDefinition",
                null,  // CL.List<StandardInstance> (),                               // directMethods
                null,  // CL.List<StandardInstance> (), // directSubclasses
                CL.List<StandardInstance> (builtInEffectiveSlotDefinition),                 // directSuperclasses
                CL.List<StandardInstance> (builtInEffectiveSlotDefinition, builtInSlotDefinition, slotDefinition, metaobject, standardObjectClass, top), // precedence list
                null,  // defaultInitargs
                null,  // directDefaultInitargs
                null, // null, // CL.List<StandardInstance> (),          // directSlots
                ManifestInstance.CreateInstance (),  // prototype
                new StandardInstance [] {
                    bootstrapCreateDocumentationSlot (),
                    bootstrapCreateNameSlot (),
                    bootstrapCreateStudlyNameSlot (),
                    }
                );


            bootstrapInitializeStandardClass (builtInSlotDefinition,
                "Superclass of dotnet-defined slot definitions.",
                QuoteBuiltInSlotDefinition,
                "BuiltInSlotDefinition",
                null,  // CL.List<StandardInstance> (),                               // directMethods
                CL.List<StandardInstance> (builtInEffectiveSlotDefinition), // directSubclasses
                CL.List<StandardInstance> (slotDefinition),                 // directSuperclasses
                CL.List<StandardInstance> (builtInSlotDefinition, slotDefinition, metaobject, standardObjectClass, top), // precedence list
                null,  // defaultInitargs
                null,  // directDefaultInitargs
                null, // null, // CL.List<StandardInstance> (),         // directSlots
                ManifestInstance.CreateInstance (), // prototype
                new StandardInstance [] {
                    bootstrapCreateDocumentationSlot (),
                    bootstrapCreateNameSlot (),
                    bootstrapCreateStudlyNameSlot (),
                    }
                );

            bootstrapInitializeStandardClass (closClass,
                "The superclass of all class metaobjects.",
                QuoteClass,
                "Class",

                null,  // CL.List<StandardInstance> (), // directMethods
                CL.List<StandardInstance> (builtInClass, forwardReferencedClass, funcallableStandardClass, standardClass), // directSubclasses
                CL.List<StandardInstance> (specializer),         // directSuperclasses
                CL.List<StandardInstance> (closClass, specializer, metaobject, standardObjectClass, top),         // precedenceList

                null,                              // defaultInitargs
                null,                              // directDefaultInitargs
                null, //CL.List<StandardInstance> (
                // direct subclasses
                // direct superclasses
                // precedence list
                 //    ),      // directSlots
                ManifestInstance.CreateInstance (), // prototype
                new StandardInstance [] {
                    bootstrapCreateDocumentationSlot (),
                    bootstrapCreateNameSlot (),
                    bootstrapCreateStudlyNameSlot (),
                    }
                );

            bootstrapInitializeStandardClass (directSlotDefinition,
                "The superclass of all direct slot definition metaobjects.",
                QuoteDirectSlotDefinition,
                "DirectSlotDefinition",

                null,  // CL.List<StandardInstance> (),
                CL.List<StandardInstance> (standardDirectSlotDefinition), // directSubclasses
                CL.List<StandardInstance> (slotDefinition),               // directSuperclasses
                CL.List<StandardInstance> (directSlotDefinition, slotDefinition, metaobject, standardObjectClass, top),         // precedenceList

                null,  // defaultInitargs
                null,  // directDefaultInitargs
                null, //CL.List<StandardInstance> (
                // readers
                // writers
                //   ),         // directSlots
                ManifestInstance.CreateInstance (), // prototype
                new StandardInstance [] {
                    bootstrapCreateDocumentationSlot (),
                    bootstrapCreateNameSlot (),
                    bootstrapCreateStudlyNameSlot (),
                    }
                );

            bootstrapInitializeFuncallableStandardClass (dotnetGenericFunction,
                "Class of dotnet-defined generic functions.",
                QuoteDotnetGenericFunction,
                "DotnetGenericFunction",
                null,  // CL.List<StandardInstance> (), // directMethods
                null,  // CL.List<StandardInstance> (), // directSubclasses
                CL.List<StandardInstance> (genericFunction),                 // directSuperclasses
                CL.List<StandardInstance> (dotnetGenericFunction, genericFunction, metaobject, funcallableStandardObject, standardObjectClass, function, top),         // precedenceList
                null,  // defaultInitargs
                null,  // directDefaultInitargs
                null, //CL.List<StandardInstance> (
                // application cache
                // lambda list
                // singletons list
                //    ),         // directSlots
                ManifestInstance.CreateInstance (), // prototype
                new StandardInstance [] {
                    bootstrapCreateDocumentationSlot (),
                    bootstrapCreateNameSlot (),
                    bootstrapCreateStudlyNameSlot (),
                    }
        );

            bootstrapInitializeStandardClass (effectiveSlotDefinition,
                "The superclass of all effective slot definition metaobjects.",
                QuoteEffectiveSlotDefinition,
                "EffectiveSlotDefinition",

                null, // null, // CL.List<StandardInstance> (),
                CL.List<StandardInstance> (standardEffectiveSlotDefinition), // directSubclasses
                CL.List<StandardInstance> (slotDefinition),         // directSuperclasses
                CL.List<StandardInstance> (effectiveSlotDefinition, slotDefinition, metaobject, standardObjectClass, top),         // precedenceList

                null,  // defaultInitargs
                null,  // directDefaultInitargs
               null, // CL.List<StandardInstance> (
                // initializer
                // location
                //    ),         // directSlots
                ManifestInstance.CreateInstance (), // prototype
                new StandardInstance [] {
                    bootstrapCreateDocumentationSlot (),
                    bootstrapCreateNameSlot (),
                    bootstrapCreateStudlyNameSlot (),
                    }
                );

            bootstrapInitializeStandardClass (eqlSpecializer,
                "The class of EQL specializers.",
                QuoteEqlSpecializer,
                "EqlSpecializer",

                null,  // CL.List<StandardInstance> (),

                null,  // CL.List<StandardInstance> (),         // directSubclasses
                CL.List<StandardInstance> (specializer),         // directSuperclasses
                CL.List<StandardInstance> (eqlSpecializer, specializer, metaobject, standardObjectClass, top),         // precedenceList

                null,  // defaultInitargs
                null,  // directDefaultInitargs
                null, // null, // CL.List<StandardInstance> (),         // directSlots
                ManifestInstance.CreateInstance (), // prototype
                new StandardInstance [] {
                    bootstrapCreateDocumentationSlot (),
                    bootstrapCreateNameSlot (),
                    bootstrapCreateStudlyNameSlot (),
                    }
                );

            bootstrapInitializeStandardClass (forwardReferencedClass,
                "The superclass of all classes that are forward referenced.",
                QuoteForwardReferencedClass,
                "ForwardReferencedClass",
                null,  // CL.List<StandardInstance> (),         // directmethods
                null,  // CL.List<StandardInstance> (),         // directSubclasses
                CL.List<StandardInstance> (closClass),         // directSuperclasses
                CL.List<StandardInstance> (forwardReferencedClass, closClass, specializer, metaobject, standardObjectClass, top),         // precedenceList

                null,  // defaultInitargs
                null,  // directDefaultInitargs
                null,  // CL.List<StandardInstance> (),         // directSlots
                ManifestInstance.CreateInstance (), // prototype
                new StandardInstance [] {
                    bootstrapCreateDocumentationSlot (),
                    bootstrapCreateNameSlot (),
                    bootstrapCreateStudlyNameSlot (),
                    }
                );

            bootstrapInitializeStandardClass (funcallableStandardClass,
                "The superclass of all classes that represent funcallable instances.",
                QuoteFuncallableStandardClass,
                "FuncallableStandardClass",
                null,  // CL.List<StandardInstance> (),         // directMethods
                null,  // CL.List<StandardInstance> (),         // directSubclasses
                CL.List<StandardInstance> (closClass), // directSuperclasses
                CL.List<StandardInstance> (funcallableStandardClass, closClass, specializer, metaobject, standardObjectClass, top),         // precedenceList
                null,  // defaultInitargs
                null,  // directDefaultInitargs
                null, // CL.List<StandardInstance> (
                // default initargs
                // direct default initargs
                // direct slots
                // prototype
                // slots

                 //   ),         // directSlots
                ManifestInstance.CreateInstance (), // prototype
                new StandardInstance [] {
                    bootstrapCreateDocumentationSlot (),
                    bootstrapCreateNameSlot (),
                    bootstrapCreateStudlyNameSlot (),
                    bootstrapCreateDirectMethodsSlot(),
                    bootstrapCreateDirectSubclassesSlot(),
                    bootstrapCreateDirectSuperclassesSlot(),
                    bootstrapCreatePrecedenceListSlot(),
                    bootstrapCreateDefaultInitargsSlot(),
                    bootstrapCreateDirectDefaultInitargsSlot(),
                    bootstrapCreateDirectSlotsSlot(),
                    bootstrapCreatePrototypeSlot(),
                    bootstrapCreateEffectiveSlot ("The effective slots slot",
                                             QuoteSlots,
                                             "Slots",
                                             KW.Instance,
                                             null,
                                             null,
                                             unboundSlotInitializer(),
                                             11
                                             )
                    }
                 );

            bootstrapInitializeStandardClass (funcallableStandardObject,
                "The superclass of CLOS objects that can be invoked on arguments.",
                QuoteFuncallableStandardObject,
                "FuncallableStandardObject",
                null,  // CL.List<StandardInstance> (),
                CL.List<StandardInstance> (genericFunction),         // directSubclasses
                CL.List<StandardInstance> (standardObjectClass, function),         // directSuperclasses
                CL.List<StandardInstance> (funcallableStandardObject, standardObjectClass, function, top),         // precedenceList
                null,  // defaultInitargs
                null,  // directDefaultInitargs
                null, // null, // CL.List<StandardInstance> (),         // directSlots
                ManifestInstance.CreateInstance (), // prototype
                new StandardInstance [] {
                    }
                );

            bootstrapInitializeBuiltInClass (builtInClass, function,
                "Built-in class representing functions.",
                QuoteFunction,
                "Function",
                null,  // CL.List<StandardInstance> (),
                CL.List<StandardInstance> (funcallableStandardObject),
                CL.List<StandardInstance> (top),
                CL.List<StandardInstance> (function, top),
                typeof(Delegate)
                );

            bootstrapInitializeFuncallableStandardClass (genericFunction,
                "Superclass of generic functions.",
                QuoteGenericFunction,
                "GenericFunction",
                null,  // CL.List<StandardInstance> (),         // directMethods
                CL.List<StandardInstance> (dotnetGenericFunction, standardGenericFunction),                 // directSubclasses
                CL.List<StandardInstance> (metaobject, funcallableStandardObject),   // directSuperclasses
                CL.List<StandardInstance> (genericFunction, metaobject, funcallableStandardObject, standardObjectClass, function, top), // precedenceList
                null,  // defaultInitargs
                null,  // directDefaultInitargs
                null, // null, // CL.List<StandardInstance> (),         // directSlots
                ManifestInstance.CreateInstance (), // prototype
                new StandardInstance [] {
                    bootstrapCreateDocumentationSlot (),
                    bootstrapCreateNameSlot (),
                    bootstrapCreateStudlyNameSlot (),
                    }
                );

            bootstrapInitializeStandardClass (metaobject,
                "Superclass of all CLOS metaobjects.",
                QuoteMetaobject,
                "Metaobject",
                null,  // CL.List<StandardInstance> (), //directMethods
                CL.List<StandardInstance> (genericFunction, method, methodCombination, specializer, slotDefinition), // directSubclasses
                CL.List<StandardInstance> (standardObjectClass),         // directSuperclasses
                CL.List<StandardInstance> (metaobject, standardObjectClass, top),         // precedenceList
                null,  // defaultInitargs
                null,  // directDefaultInitargs
                null, //CL.List<StandardInstance> (
                // name
                // documentation
                // studlyname

                //       ),         // directSlots
                ManifestInstance.CreateInstance (), // prototype
                new StandardInstance [] {
                    bootstrapCreateDocumentationSlot (),
                    bootstrapCreateNameSlot (),
                    bootstrapCreateStudlyNameSlot (),
                    }
                );

            bootstrapInitializeStandardClass (method,
                "Superclass of all method classes.",
                QuoteMethod,
                "Method",
                null,  // CL.List<StandardInstance> (),         // directMethods
                CL.List<StandardInstance> (standardMethod), // directSubclasses
                CL.List<StandardInstance> (metaobject),     // directSuperclasses
                CL.List<StandardInstance> (method, metaobject, standardObjectClass, top),         // precedenceList
                null,  // defaultInitargs
                null,  // directDefaultInitargs
                null,  // CL.List<StandardInstance> (),         // directSlots
                ManifestInstance.CreateInstance (), // prototype
                new StandardInstance [] {
                    bootstrapCreateDocumentationSlot (),
                    bootstrapCreateNameSlot (),
                    bootstrapCreateStudlyNameSlot (),
                    }
                );

            bootstrapInitializeStandardClass (methodCombinationClass,
                "Class of method combination objects.",
                QuoteMethodCombination,
                "MethodCombination",
                null,  // CL.List<StandardInstance> (),
                null,  // CL.List<StandardInstance> (),         // directSubclasses
                CL.List<StandardInstance> (metaobject),         // directSuperclasses
                CL.List<StandardInstance> (methodCombinationClass, metaobject, standardObjectClass, top),         // precedenceList
                null,  // defaultInitargs
                null,  // directDefaultInitargs
                null,  // CL.List<StandardInstance> (),         // directSlots
                ManifestInstance.CreateInstance (), // prototype
                new StandardInstance [] {
                    bootstrapCreateDocumentationSlot (),
                    bootstrapCreateNameSlot (),
                    bootstrapCreateStudlyNameSlot (),
                    }
                );

            bootstrapInitializeBuiltInClass (builtInClass, nullClass,
                "The class of null.",
                QuoteNull,
                "Null",
                null,  // CL.List<StandardInstance> (), // directMethods
                null,  // CL.List<StandardInstance> (), // directSubclasses
                CL.List<StandardInstance> (symbolClass), // directSuperclasses
                CL.List<StandardInstance> (nullClass, symbolClass, top),
                null); // no affiliated dotnet type object

            bootstrapInitializeStandardClass (slotDefinition,
                "Superclass of all slot definition classes.",
                QuoteSlotDefinition,
                "SlotDefinition",
                null, // CL.List<StandardInstance> (),
                CL.List<StandardInstance> (directSlotDefinition, effectiveSlotDefinition, standardSlotDefinition),         // directSubclasses
                CL.List<StandardInstance> (metaobject),         // directSuperclasses
                CL.List<StandardInstance> (slotDefinition, metaobject, standardObjectClass, top),         // precedenceList
                null,  // defaultInitargs
                null,  // directDefaultInitargs
                null, //CL.List<StandardInstance> (
                // allocation
                // initargs
                // type
                 //      ),         // directSlots
                ManifestInstance.CreateInstance (),
                new StandardInstance [] {
                    bootstrapCreateDocumentationSlot (),
                    bootstrapCreateNameSlot (),
                    bootstrapCreateStudlyNameSlot (),
                    }
                );

            bootstrapInitializeStandardClass (specializer,
                "Superclass of all objects that can be specialized upon.",
                QuoteSpecializer,
                "Specializer",

            null, // CL.List<StandardInstance> (),
                CL.List<StandardInstance> (closClass, eqlSpecializer),         // directSubclasses
                CL.List<StandardInstance> (metaobject),         // directSuperclasses
                CL.List<StandardInstance> (specializer, metaobject, standardObjectClass, top),         // precedenceList
                null,  // defaultInitargs
                null,  // directDefaultInitargs
                null, //CL.List<StandardInstance> (
                // direct methods
                 //     ),         // directSlots
                ManifestInstance.CreateInstance (), // prototype
                new StandardInstance [] {
                    bootstrapCreateDocumentationSlot (),
                    bootstrapCreateNameSlot (),
                    bootstrapCreateStudlyNameSlot (),
                    }
                );

            bootstrapInitializeStandardClass (standardAccessorMethodClass,
                "Superclass of all methods that access slots.",
                QuoteStandardAccessorMethod,
                "StandardAccessorMethod",
                null, // CL.List<StandardInstance> (),         // directSlots
                CL.List<StandardInstance> (standardReaderMethod, standardWriterMethod), // directSubclasses
                CL.List<StandardInstance> (standardMethod),                             // directSuperclasses
                CL.List<StandardInstance> (standardAccessorMethodClass, standardMethod, method, metaobject, standardObjectClass, top),         // precedenceList

                null,  // defaultInitargs
                null,  // directDefaultInitargs
                null, // CL.List<StandardInstance> (),         // directSlots
                ManifestInstance.CreateInstance (), // prototype
                new StandardInstance [] {
                    bootstrapCreateDocumentationSlot (),
                    bootstrapCreateNameSlot (),
                    bootstrapCreateStudlyNameSlot (),
                    }
                );

            bootstrapInitializeStandardClass (standardClass,
                "Superclass of CLOS-defined classes.",
                 QuoteStandardClass,
                "StandardClass",
                null, // CL.List<StandardInstance> (),         // directMethods
                null, // CL.List<StandardInstance> (),         // directSubclasses
                CL.List<StandardInstance> (closClass),         // directSuperclasses
                CL.List<StandardInstance> (standardClass, closClass, specializer, metaobject, standardObjectClass, top),         // precedenceList

                null,  // defaultInitargs
                null,  // directDefaultInitargs
                null, // CL.List<StandardInstance> (
                // default initargs
                // direct default initargs
                // direct slots
                // prototype
                // slots
                 //   ),         // directSlots
                ManifestInstance.CreateInstance (), // prototype
                new StandardInstance [] {
                    bootstrapCreateDocumentationSlot (),
                    bootstrapCreateNameSlot (),
                    bootstrapCreateStudlyNameSlot (),
                bootstrapCreateDirectMethodsSlot(),
                bootstrapCreateDirectSubclassesSlot(),
                bootstrapCreateDirectSuperclassesSlot(),
                bootstrapCreatePrecedenceListSlot(),

                    }

                );

            bootstrapInitializeStandardClass (standardDirectSlotDefinition,
                "Class of direct slot definitions for standard classes.",
                QuoteStandardDirectSlotDefinition,
                "StandardDirectSlotDefinition",
                null, // CL.List<StandardInstance> (),         // directMethods
                null, // CL.List<StandardInstance> (),         // directSubclasses
                CL.List<StandardInstance> (standardSlotDefinition, directSlotDefinition),         // directSuperclasses
                CL.List<StandardInstance> (standardDirectSlotDefinition, directSlotDefinition, standardSlotDefinition, slotDefinition, metaobject, standardObjectClass, top),         // precedenceList
                null,  // defaultInitargs
                null,  // directDefaultInitargs
                null, // CL.List<StandardInstance> (),         // directSlots
                ManifestInstance.CreateInstance (), // prototype
                new StandardInstance [] {
                    bootstrapCreateDocumentationSlot (),
                    bootstrapCreateNameSlot (),
                    bootstrapCreateStudlyNameSlot (),
                    }
                );

            bootstrapInitializeStandardClass (standardEffectiveSlotDefinition,
                "Class of effective slot definitions for standard classes.",
                QuoteStandardEffectiveSlotDefinition,
                "StandardEffectiveSlotDefinition",
                null, // CL.List<StandardInstance> (),         // directMethods
                null, // CL.List<StandardInstance> (),         // directSubclasses
                CL.List<StandardInstance> (standardSlotDefinition, effectiveSlotDefinition),         // directSuperclasses
                CL.List<StandardInstance> (standardEffectiveSlotDefinition, effectiveSlotDefinition, standardSlotDefinition, slotDefinition, metaobject, standardObjectClass, top),         // precedenceList
                null,  // defaultInitargs
                null,  // directDefaultInitargs
                null, // CL.List<StandardInstance> (),         // directSlots
                ManifestInstance.CreateInstance (), // prototype
                new StandardInstance [] {
                    bootstrapCreateDocumentationSlot (),
                    bootstrapCreateNameSlot (),
                    bootstrapCreateStudlyNameSlot (),
                bootstrapCreateEffectiveSlot ("The allocation slot",
                                              QuoteAllocation,
                                              "Allocation",
                                              KW.Instance,
                                              null,
                                              null,
                                              constantSlotInitializer(KW.Instance),
                                              3),
                bootstrapCreateEffectiveSlot ("The initargs slot",
                                              QuoteInitargs,
                                              "Initargs",
                                              KW.Instance,
                                              null,
                                              null,
                                              keywordSlotInitializer(CL.List<Symbol>(KW.Initarg)),
                                              4),
                bootstrapCreateEffectiveSlot ("The type slot",
                                              QuoteType,
                                              "Type",
                                              KW.Instance,
                                              null,
                                              null,
                                              unboundSlotInitializer(),
                                              5),
                bootstrapCreateEffectiveSlot ("The initializer slot",
                                              QuoteInitializer,
                                              "Initializer",
                                              KW.Instance,
                                              null,
                                              null,
                                              unboundSlotInitializer(),
                                              6),
                bootstrapCreateEffectiveSlot ("The location slot",
                                              QuoteLocation,
                                              "Location",
                                              KW.Instance,
                                              null,
                                              null,
                                              unboundSlotInitializer(),
                                              7)}
                );

            bootstrapInitializeFuncallableStandardClass (standardGenericFunction,
                "Class of CLOS-defined generic functions.",
                QuoteStandardGenericFunction,
                "StandardGenericFunction",
                null, // CL.List<StandardInstance> (), // directMethods
                null, // CL.List<StandardInstance> (), // directSubclasses
                CL.List<StandardInstance> (genericFunction),                 // directSuperclasses
                CL.List<StandardInstance> (standardGenericFunction, genericFunction, metaobject, funcallableStandardObject, standardObjectClass, function, top),         // precedenceList
                null,  // defaultInitargs
                null,  // directDefaultInitargs
                null, //CL.List<StandardInstance> (
                // application cache
                // lambda list
                // singletons list
                //   ),         // directSlots
                ManifestInstance.CreateInstance (), // prototype
                new StandardInstance [] {
                    bootstrapCreateDocumentationSlot (),
                    bootstrapCreateNameSlot (),
                    bootstrapCreateStudlyNameSlot (),
                    bootstrapCreateApplicationCacheSlot(),
                    bootstrapCreateLambdaListSlot(),
                    bootstrapCreateMethodsSlot(),
                    bootstrapCreateSingletonsListSlot()
                    }

          // effectiveSlots
        );

            bootstrapInitializeStandardClass (standardMethod,
                "Class of CLOS-defined methods.",
                QuoteStandardMethod,
                "StandardMethod",
                null, //CL.List<StandardInstance> (
                       // function
                       // specializers
                       // qualifier
                        //),         // directMethods
                CL.List<StandardInstance> (standardAccessorMethod), // directSubclasses
                CL.List<StandardInstance> (method),                 // directSuperclasses
                CL.List<StandardInstance> (standardMethod, method, metaobject, standardObjectClass, top),         // precedenceList
                null,  // defaultInitargs
                null,  // directDefaultInitargs
                null, // CL.List<StandardInstance> (),         // directSlots
                ManifestInstance.CreateInstance (), // prototype
                new StandardInstance [] {
                    bootstrapCreateDocumentationSlot (),
                    bootstrapCreateNameSlot (),
                    bootstrapCreateStudlyNameSlot (),
                    bootstrapCreateFunctionSlot(),
                    bootstrapCreateQualifierSlot(),
                    bootstrapCreateSpecializersSlot()
                    }
                );

            // All StandardObjects will have classes that inherit from this.
            // Since all objects need effective slot descriptors, this class provides that
            // slot.
            bootstrapInitializeStandardClass (standardObjectClass,
                "Superclass of CLOS-defined objects.",
                QuoteStandardObject,
                "StandardObjectClass",
                null, // CL.List<StandardInstance> (),           // directMethods
                CL.List<StandardInstance> (funcallableStandardObject, metaobject), // directSubclasses
                CL.List<StandardInstance> (top),        // directSuperclasses
                CL.List<StandardInstance> (standardObjectClass, top),           // precedenceList
                null,  // defaultInitargs
                null,  // directDefaultInitargs
                null, // CL.List<StandardInstance> (),           // directSlots
                ManifestInstance.CreateInstance (),   // prototype
                new StandardInstance [] {
                    bootstrapCreateDocumentationSlot (),
                    bootstrapCreateNameSlot (),
                    bootstrapCreateStudlyNameSlot (),
                    }
                );

            bootstrapInitializeStandardClass (standardReaderMethod,
                "Superclass of slot reader methods.",
                QuoteStandardReaderMethod,
                "StandardReaderMethod",
                null, // CL.List<StandardInstance> (),         // directMethods
                null, // CL.List<StandardInstance> (),         // directSubclasses
                CL.List<StandardInstance> (standardAccessorMethodClass),         // directSuperclasses
                CL.List<StandardInstance> (standardReaderMethod, standardAccessorMethodClass, standardMethod, method, metaobject, standardObjectClass, top),         // precedenceList
                null,  // defaultInitargs
                null,  // directDefaultInitargs
                null, // CL.List<StandardInstance> (),         // directSlots
                ManifestInstance.CreateInstance (), // prototype
                new StandardInstance [] {
                    bootstrapCreateDocumentationSlot (),
                    bootstrapCreateNameSlot (),
                    bootstrapCreateStudlyNameSlot (),
                    }
                );

            bootstrapInitializeStandardClass (standardSlotDefinition,
                "Superclass of CLOS-defined slot definitions.",
                QuoteStandardSlotDefinition,
                "StandardSlotDefinition",
                null, // CL.List<StandardInstance> (),         // directMethods
                CL.List<StandardInstance> (standardDirectSlotDefinition, standardEffectiveSlotDefinition), // directSubclasses
                CL.List<StandardInstance> (standardSlotDefinition, slotDefinition, metaobject, standardObjectClass, top),         // directSuperclasses
                CL.List<StandardInstance> (top),         // precedenceList
                null,  // defaultInitargs
                null,  // directDefaultInitargs
                null, // CL.List<StandardInstance> (),         // directSlots
                ManifestInstance.CreateInstance (), // prototype
                new StandardInstance [] {
                    bootstrapCreateDocumentationSlot (),
                    bootstrapCreateNameSlot (),
                    bootstrapCreateStudlyNameSlot (),
                    }
                );

            bootstrapInitializeStandardClass (standardWriterMethod,
            "Superclass of CLOS-defined writer methods.",
                QuoteStandardWriterMethod,
                "StandardWriterMethod",
                null, // CL.List<StandardInstance> (),         // directMethods
                null, // CL.List<StandardInstance> (),         // directSubclasses
                CL.List<StandardInstance> (standardAccessorMethodClass),         // directSuperclasses
                CL.List<StandardInstance> (standardWriterMethod, standardAccessorMethodClass, standardMethod, method, metaobject, standardObjectClass, top),         // precedenceList
                null,  // defaultInitargs
                null,  // directDefaultInitargs
                null, // CL.List<StandardInstance> (),         // directSlots
                ManifestInstance.CreateInstance (), // prototype
                new StandardInstance [] {
                    bootstrapCreateDocumentationSlot (),
                    bootstrapCreateNameSlot (),
                    bootstrapCreateStudlyNameSlot (),
                    }
                );

            bootstrapInitializeBuiltInClass (builtInClass, symbolClass,
                "Class of lisp symbols.",
                QuoteSymbol,
                "Symbol",
                null, // CL.List<StandardInstance> (), // direct methods
                null, // CL.List<StandardInstance> (), // direct subclasses
                CL.List<StandardInstance> (top),  //direct superclasses
                CL.List<StandardInstance> (symbolClass, top), // precedence list
                typeof(Lisp.Symbol)
                );

            bootstrapInitializeBuiltInClass (builtInClass, top,
                "Root of the class hierarchy.",
                QuoteTop,
                "Top",
                null, // CL.List<StandardInstance> (), // direct methods
                CL.List<StandardInstance> (function, standardObjectClass), // direct subclasses
                null, // CL.List<StandardInstance> (),  //direct superclasses
                CL.List<StandardInstance> (top), // precedence list
                typeof(object)
                );

            bootstrapInitializeBuiltInClass(builtInClass, runtimeType,
                "Reflected dotnet class type.",
                QuoteRuntimeType,
                "RuntimeType",
                null, // CL.List<StandardInstance> (), // direct methods
                null, // CL.List<StandardInstance> (),
                CL.List<StandardInstance> (runtimeType, builtInClass),  //direct superclasses
                CL.List<StandardInstance> (runtimeType, builtInClass, closClass, specializer, metaobject, standardObjectClass, top),
                typeof(object).GetType()
                );

            // The generic functions

            //accessorMethodSlotDefinition.SetClass (StandardGenericFunction);
            //accessorMethodSlotDefinition.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteAccessorMethodSlotDefinition,
            //    "AccessorMethodSlotDefinition" });
            //allNamedGenericFunctions.Add (QuoteAccessorMethodSlotDefinition, accessorMethodSlotDefinition);

            //addDependent.SetClass (StandardGenericFunction);
            //addDependent.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteAddDependent,
            //    "AddDependent" });
            //allNamedGenericFunctions.Add (QuoteAddDependent, addDependent);

            //addDirectMethod.SetClass (StandardGenericFunction);
            //addDirectMethod.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteAddDirectMethod,
            //    "AddDirectMethod" });
            //allNamedGenericFunctions.Add (QuoteAddDirectMethod, addDirectMethod);

            //addDirectSubclass.SetClass (StandardGenericFunction);
            //addDirectSubclass.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteAddDirectSubclass,
            //    "AddDirectSubclass" });
            //allNamedGenericFunctions.Add (QuoteAddDirectSubclass, addDirectSubclass);

            //addMethod.SetClass (StandardGenericFunction);
            //addMethod.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteAddMethod,
            //    "AddMethod" });
            //allNamedGenericFunctions.Add (QuoteAddMethod, addMethod);

            bootstrapInitializeStandardMethod (method0ForAllocateInstance,
                                               "Allocate an instance",
                                               QuoteMethodAllocateInstance,
                                               "Method:AllocateInstance",
                                               (AllocateInstanceMethodSignature) allocateInstanceMethod0Impl,
                                               KW.Primary,
                                               CL.List (funcallableStandardClass)
                                               );

            bootstrapInitializeStandardGenericFunction (allocateInstance,
                                                        "Creates an instance of a class.",
                                                        QuoteAllocateInstance,
                                                        "AllocateInstance",
                                                        CL.List(QuoteClass, QuoteAndRest, QuoteInitargs),
                                                        CL.List (method0ForAllocateInstance),
                                                        CL.List<ICollection<object>>(null));
            setFuncallableInstanceFunction (allocateInstance, computeDiscriminatingFunctionTrampoline);

            //changeClass.SetClass (StandardGenericFunction);
            //changeClass.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteChangeClass,
            //    "ChangeClass" });
            //allNamedGenericFunctions.Add (QuoteChangeClass, changeClass);

            //classDefaultInitargs.SetClass (StandardGenericFunction);
            //classDefaultInitargs.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteClassDefaultInitargs,
            //    "ClassDefaultInitargs" });
            //allNamedGenericFunctions.Add (QuoteClassDefaultInitargs, classDefaultInitargs);

            //classDirectDefaultInitargs.SetClass (StandardGenericFunction);
            //classDirectDefaultInitargs.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteClassDirectDefaultInitargs,
            //    "ClassDirectDefaultInitargs" });
            //allNamedGenericFunctions.Add (QuoteClassDirectDefaultInitargs, classDirectDefaultInitargs);

            //classDirectSlots.SetClass (StandardGenericFunction);
            //classDirectSlots.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteClassDirectSlots,
            //    "ClassDirectSlots" });
            //allNamedGenericFunctions.Add (QuoteClassDirectSlots, classDirectSlots);

            //classDirectSubclasses.SetClass (StandardGenericFunction);
            //classDirectSubclasses.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteClassDirectSubclasses,
            //    "ClassDirectSubclasses" });
            //allNamedGenericFunctions.Add (QuoteClassDirectSubclasses, classDirectSubclasses);

            //classDirectSuperclasses.SetClass (StandardGenericFunction);
            //classDirectSuperclasses.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteClassDirectSuperclasses,
            //    "ClassDirectSuperclasses" });
            //allNamedGenericFunctions.Add (QuoteClassDirectSuperclasses, classDirectSuperclasses);

            //className.SetClass (StandardGenericFunction);
            //className.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteClassName,
            //    "ClassName" });
            //allNamedGenericFunctions.Add (QuoteClassName, className);

            bootstrapInitializeStandardMethod (method0ForClassPrecedenceList,
                                               "Returns the precedence list.",
                                               QuoteMethodClassPrecedenceList,
                                               "Method:ClassPrecedenceList",
                                               (ClassPrecedenceListMethodSignature) classPrecedenceListMethod0Impl,
                                               KW.Primary,
                                               CL.List (closClass));

            bootstrapInitializeStandardMethod (method1ForClassPrecedenceList,
                                               "Returns the precedence list.",
                                               QuoteMethodClassPrecedenceList,
                                               "Method:ClassPrecedenceList",
                                               (ClassPrecedenceListMethodSignature) classPrecedenceListMethod1Impl,
                                               KW.Primary,
                                               CL.List (builtInClass));

            bootstrapInitializeStandardGenericFunction (classPrecedenceList,
                                                        "Returns the precedence list of a class.",
                                                        QuoteClassPrecedenceList,
                                                        "ClassPrecedenceList",
                                                        CL.List(QuoteClass),
                                                        CL.List(method0ForClassPrecedenceList),
                                                        CL.List<ICollection<object>>(null));

            //classPrototype.SetClass (StandardGenericFunction);
            //classPrototype.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteClassPrototype,
            //    "ClassPrototype" });
            //allNamedGenericFunctions.Add (QuoteClassPrototype, classPrototype);

            bootstrapInitializeEqlSpecializer (standardClassEqlSpecializer, standardClass);

            bootstrapInitializeStandardMethod (method0ForClassSlots,
                                               "Returns the slots of a class.",
                                               QuoteMethodClassSlots,
                                               "Method:ClassSlots",
                                               (ClassSlotsMethodSignature) classSlotsMethod0Impl,
                                               KW.Primary,
                                               CL.List (standardClassEqlSpecializer));

            bootstrapInitializeStandardMethod (method1ForClassSlots,
                                               "Returns the slots of a class.",
                                               QuoteMethodClassSlots,
                                               "Method:ClassSlots",
                                               (ClassSlotsMethodSignature) classSlotsMethod1Impl,
                                               KW.Primary,
                                               CL.List (funcallableStandardClass));

            bootstrapInitializeStandardMethod (method2ForClassSlots,
                                               "Returns the slots of a class.",
                                               QuoteMethodClassSlots,
                                               "Method:ClassSlots",
                                               (ClassSlotsMethodSignature) classSlotsMethod2Impl,
                                               KW.Primary,
                                               CL.List (builtInClass));

            bootstrapInitializeStandardMethod (method3ForClassSlots,
                                               "Returns the slots of a class.",
                                               QuoteMethodClassSlots,
                                               "Method:ClassSlots",
                                               (ClassSlotsMethodSignature) classSlotsMethod3Impl,
                                               KW.Primary,
                                               CL.List (closClass));

            bootstrapInitializeStandardGenericFunction (classSlots,
                                                        "Returns the effective slots of a class.",
                                                        QuoteClassSlots,
                                                        "ClassSlots",
                                                        CL.List(QuoteGenericFunction),
                                                        CL.List(method0ForClassSlots,
                                                                // method1ForClassSlots,
                                                                method2ForClassSlots,
                                                                method3ForClassSlots),
                                                        CL.List<ICollection<object>> (CL.List<object>(standardClass))
                                                                );

            bootstrapInitializeStandardGenericFunction (computeApplicableMethods,
                                                        "Gets the applicable methods for a generic function call.",
                                                        QuoteComputeApplicableMethods,
                                                        "ComputeApplicableMethods",
                                                        CL.List(QuoteGenericFunction, QuoteAndRest, QuoteArguments),
                                                        null,
                                                        CL.List<ICollection<object>>(null));

            //computeApplicableMethodsUsingClasses.SetClass (StandardGenericFunction);
            //computeApplicableMethodsUsingClasses.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteComputeApplicableMethodsUsingClasses,
            //    "ComputeApplicableMethodsUsingClasses" });
            //allNamedGenericFunctions.Add (QuoteComputeApplicableMethodsUsingClasses, computeApplicableMethodsUsingClasses);

            //computeClassPrecedenceList.SetClass (StandardGenericFunction);
            //computeClassPrecedenceList.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteComputeClassPrecedenceList,
            //    "ComputeClassPrecedenceList" });
            //allNamedGenericFunctions.Add (QuoteComputeClassPrecedenceList, computeClassPrecedenceList);

            //computeDefaultInitargs.SetClass (StandardGenericFunction);
            //computeDefaultInitargs.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteComputeDefaultInitargs,
            //    "ComputeDefaultInitargs" });
            //allNamedGenericFunctions.Add (QuoteComputeDefaultInitargs, computeDefaultInitargs);

            bootstrapInitializeStandardMethod (methodForComputeDiscriminatingFunction,
                                               "Computes the discriminating function.",
                                               QuoteMethodComputeDiscriminatingFunction,
                                               "Method:ComputeDiscriminatingFunction",
                                               (ComputeDiscriminatingFunctionMethodSignature) computeDiscriminatingFunctionMethodImpl,
                                               KW.Primary,
                                               CL.List (StandardGenericFunction)
                                               );

            bootstrapInitializeStandardGenericFunction (computeDiscriminatingFunction,
                                                        "Computes the dispatch function of a generic function.",
                                                        QuoteComputeDiscriminatingFunction,
                                                        "ComputeDiscriminatingFunction",
                                                        CL.List(QuoteGenericFunction),
                                                        CL.List(methodForComputeDiscriminatingFunction),
                                                        CL.List<ICollection<object>>(null));

            bootstrapInitializeStandardGenericFunction (computeEffectiveMethod,
                                                        "Computes the effective method of a particular invokation.",
                                                        QuoteComputeEffectiveMethod,
                                                        "ComputeEffectiveMethod",
                                                        CL.List(QuoteGenericFunction, QuoteAndRest, QuoteArguments),
                                                        null,
                                                        CL.List<ICollection<object>>(null));

            //computeEffectiveSlotDefinition.SetClass (StandardGenericFunction);
            //computeEffectiveSlotDefinition.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteComputeEffectiveSlotDefinition,
            //    "ComputeEffectiveSlotDefinition" });
            //allNamedGenericFunctions.Add (QuoteComputeEffectiveSlotDefinition, computeEffectiveSlotDefinition);

            //computeSlots.SetClass (StandardGenericFunction);
            //computeSlots.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteComputeSlots,
            //    "ComputeSlots" });
            //allNamedGenericFunctions.Add (QuoteComputeSlots, computeSlots);

            bootstrapInitializeStandardGenericFunction (computeMethodMoreSpecific,
                                                        "Compares two methods to determine which is more specific.",
                                                         QuoteComputeMethodMoreSpecific,
                                                        "ComputeMethodMoreSpecific",
                                                        CL.List(QuoteLeft, QuoteRight), 
                                                        null,
                                                        CL.List<ICollection<object>>(null, null));

            //defaultInitargs.SetClass (StandardGenericFunction);
            //defaultInitargs.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteDefaultInitargs,
            //    "DefaultInitargs" });
            //allNamedGenericFunctions.Add (QuoteDefaultInitargs, defaultInitargs);

            //directSlotDefinitionClass.SetClass (StandardGenericFunction);
            //directSlotDefinitionClass.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteDirectSlotDefinitionClass,
            //    "DirectSlotDefinitionClass" });
            //allNamedGenericFunctions.Add (QuoteDirectSlotDefinitionClass, directSlotDefinitionClass);

            //effectiveSlotDefinitionClass.SetClass (StandardGenericFunction);
            //effectiveSlotDefinitionClass.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteEffectiveSlotDefinitionClass,
            //    "EffectiveSlotDefinitionClass" });
            //allNamedGenericFunctions.Add (QuoteEffectiveSlotDefinitionClass, effectiveSlotDefinitionClass);

            //ensureClass.SetClass (StandardGenericFunction);
            //ensureClass.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteEnsureClass,
            //    "EnsureClass" });
            //allNamedGenericFunctions.Add (QuoteEnsureClass, ensureClass);

            //ensureClassUsingClass.SetClass (StandardGenericFunction);
            //ensureClassUsingClass.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteEnsureClassUsingClass,
            //    "EnsureClassUsingClass" });
            //allNamedGenericFunctions.Add (QuoteEnsureClassUsingClass, ensureClassUsingClass);

            bootstrapInitializeStandardMethod (method0ForEnsureGenericFunction,
                                               "Ensures a generic function exists.",
                                               QuoteMethodEnsureGenericFunction,
                                               "Method:EnsureGenericFunction",
                                               (EnsureGenericFunctionMethodSignature) ensureGenericFunctionMethod0Impl,
                                               KW.Primary,
                                               CL.List (symbolClass)
                                               );

            bootstrapInitializeStandardMethod (method1ForEnsureGenericFunction,
                                               "Ensures a generic function exists.",
                                               QuoteMethodEnsureGenericFunction,
                                               "Method:EnsureGenericFunction",
                                               (EnsureGenericFunctionMethodSignature) ensureGenericFunctionMethod1Impl,
                                               KW.Primary,
                                               CL.List (FindDotnetClass (typeof (Cons)))
                                               );

            bootstrapInitializeStandardGenericFunction (ensureGenericFunction,
                                                        "Creates or reinitializes a generic function.",
                                                        QuoteEnsureGenericFunction,
                                                        "EnsureGenericFunction",
                                                    CL.List (
                                                          QuoteFunctionName,
                                                          QuoteAndKey,
                                                          QuoteArgumentPrecedenceOrder,
                                                          QuoteDeclarations,
                                                          QuoteDocumentation,
                                                          QuoteGenericFunctionClass,
                                                          QuoteLambdaList,
                                                          QuoteMethodClass,
                                                          QuoteMethodCombination,
                                                          QuoteName,
                                                          QuoteAndAllowOtherKeys),
                                                        CL.List (method0ForEnsureGenericFunction,
                                                                 method1ForEnsureGenericFunction),
                                                        CL.List<ICollection<object>>(null));
            setFuncallableInstanceFunction (ensureGenericFunction, computeDiscriminatingFunctionTrampoline);

            bootstrapInitializeStandardMethod (method0ForEnsureGenericFunctionUsingClass,
                                               "Ensures a generic function exists.",
                                               QuoteMethodEnsureGenericFunctionUsingClass,
                                               "Method:EnsureGenericFunctionUsingClass",
                                               (EnsureGenericFunctionUsingClassMethodSignature) ensureGenericFunctionUsingClassMethod0Impl,
                                               KW.Primary,
                                               CL.List (nullClass, symbolClass)
                                               );

            bootstrapInitializeStandardGenericFunction (ensureGenericFunctionUsingClass,
                                                        "Creates or reinitializes a generic function.",
                                                        QuoteEnsureGenericFunctionUsingClass,
                                                        "EnsureGenericFunctionUsingClass",
                                                    CL.List (
                                                          QuoteGenericFunction,
                                                          QuoteFunctionName,
                                                          QuoteAndKey,
                                                          QuoteArgumentPrecedenceOrder,
                                                          QuoteDeclarations,
                                                          QuoteDocumentation,
                                                          QuoteGenericFunctionClass,
                                                          QuoteLambdaList,
                                                          QuoteMethodClass,
                                                          QuoteMethodCombination,
                                                          QuoteName,
                                                          QuoteAndAllowOtherKeys),
                                                        CL.List (method0ForEnsureGenericFunctionUsingClass),
                                                        CL.List<ICollection<object>>(null));
            setFuncallableInstanceFunction (ensureGenericFunctionUsingClass, computeDiscriminatingFunctionTrampoline);

            //eqlSpecializerObject.SetClass (StandardGenericFunction);
            //eqlSpecializerObject.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteEqlSpecializerObject,
            //    "EqlSpecializerObject" });
            //allNamedGenericFunctions.Add (QuoteEqlSpecializerObject, eqlSpecializerObject);

            //extractLambdaList.SetClass (StandardGenericFunction);
            //extractLambdaList.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteExtractLambdaList,
            //    "ExtractLambdaList" });
            //allNamedGenericFunctions.Add (QuoteExtractLambdaList, extractLambdaList);

            //extractSpecializerNames.SetClass (StandardGenericFunction);
            //extractSpecializerNames.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteExtractSpecializerNames,
            //    "ExtractSpecializerNames" });
            //allNamedGenericFunctions.Add (QuoteExtractSpecializerNames, extractSpecializerNames);

            //finalizeInheritance.SetClass (StandardGenericFunction);
            //finalizeInheritance.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteFinalizeInheritance,
            //    "FinalizeInheritance" });
            //allNamedGenericFunctions.Add (QuoteFinalizeInheritance, finalizeInheritance);

            //findClass.SetClass (StandardGenericFunction);
            //findClass.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteFindClass,
            //    "FindClass" });
            //allNamedGenericFunctions.Add (QuoteFindClass, findClass);

            //findMethod.SetClass (StandardGenericFunction);
            //findMethod.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteFindMethod,
            //    "FindMethod" });
            //allNamedGenericFunctions.Add (QuoteFindMethod, findMethod);

            //findMethodCombination.SetClass (StandardGenericFunction);
            //findMethodCombination.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteFindMethodCombination,
            //    "FindMethodCombination" });
            //allNamedGenericFunctions.Add (QuoteFindMethodCombination, findMethodCombination);

            //functionKeywords.SetClass (StandardGenericFunction);
            //functionKeywords.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteFunctionKeywords,
            //    "FunctionKeywords" });
            //allNamedGenericFunctions.Add (QuoteFunctionKeywords, functionKeywords);

            bootstrapInitializeStandardGenericFunction (genericFunctionApplicationCache,
                                                        "Returns the application cache of a generic function.",
                                                        QuoteGenericFunctionApplicationCache,
                                                        "GenericFunctionApplicationCache",
                                                        CL.List (QuoteGenericFunction),
                                                        null,
                                                        CL.List<ICollection<object>>(null));

            //genericFunctionArgumentPrecedenceOrder.SetClass (StandardGenericFunction);
            //genericFunctionArgumentPrecedenceOrder.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteGenericFunctionArgumentPrecedenceOrder,
            //    "GenericFunctionArgumentPrecedenceOrder" });
            //allNamedGenericFunctions.Add (QuoteGenericFunctionArgumentPrecedenceOrder, genericFunctionArgumentPrecedenceOrder);

            //genericFunctionDeclarations.SetClass (StandardGenericFunction);
            //genericFunctionDeclarations.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteGenericFunctionDeclarations,
            //    "GenericFunctionDeclarations" });
            //allNamedGenericFunctions.Add (QuoteGenericFunctionDeclarations, genericFunctionDeclarations);

            bootstrapInitializeStandardGenericFunction (genericFunctionLambdaList,
                                        "Returns the lambda list of a generic function.",
                                            QuoteGenericFunctionLambdaList,
                                            "GenericFunctionLambdaList",
                                            CL.List (QuoteGenericFunction),
                                            null,
                                                      CL.List<ICollection<object>>(null));

            //genericFunctionMethodClass.SetClass (StandardGenericFunction);
            //genericFunctionMethodClass.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteGenericFunctionMethodClass,
            //    "GenericFunctionMethodClass" });
            //allNamedGenericFunctions.Add (QuoteGenericFunctionMethodClass, genericFunctionMethodClass);

            //genericFunctionMethodCombination.SetClass (StandardGenericFunction);
            //genericFunctionMethodCombination.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteGenericFunctionMethodCombination,
            //    "GenericFunctionMethodCombination" });
            //allNamedGenericFunctions.Add (QuoteGenericFunctionMethodCombination, genericFunctionMethodCombination);

            bootstrapInitializeStandardGenericFunction (genericFunctionMethods,
                                                        "Returns the methods of a generic function.",
                                                        QuoteGenericFunctionMethods,
                                                        "GenericFunctionMethods",
                                                        CL.List(QuoteGenericFunction),
                                                        null,
                                                        CL.List<ICollection<object>>(null));

            //genericFunctionName.SetClass (StandardGenericFunction);
            //genericFunctionName.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteGenericFunctionName,
            //    "GenericFunctionName" });
            //allNamedGenericFunctions.Add (QuoteGenericFunctionName, genericFunctionName);

            bootstrapInitializeStandardGenericFunction (genericFunctionSingletonsList,
                            "Returns the singletons list of a generic function.",
                                QuoteGenericFunctionSingletonsList,
                                "GenericFunctionLambdaList",
                                CL.List(QuoteGenericFunction),
                                null,
                                                     CL.List<ICollection<object>>(null));
 
            bootstrapInitializeStandardMethod (method0ForInitializeInstance,
                                               "Initialize an instance",
                                               QuoteMethodInitializeInstance,
                                               "Method:InitializeInstance",
                                               (InitializeInstanceMethodSignature) initializeInstanceMethod0Impl,
                                               KW.Primary,
                                               null
                                               );


            bootstrapInitializeStandardGenericFunction (initializeInstance,
                                                        "Initializes an instance of a class.",
                                                        QuoteInitializeInstance,
                                                        "InitializeInstance",
                                                        CL.List(QuoteInstance, QuoteAndRest, QuoteInitargs),
                                                        CL.List (method0ForInitializeInstance),
                                                        CL.List<ICollection<object>>(null));
            setFuncallableInstanceFunction (initializeInstance, computeDiscriminatingFunctionTrampoline);


            //internEqlSpecializer.SetClass (StandardGenericFunction);
            //internEqlSpecializer.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteInternEqlSpecializer,
            //    "InternEqlSpecializer" });
            //allNamedGenericFunctions.Add (QuoteInternEqlSpecializer, internEqlSpecializer);

            //isClassFinalized.SetClass (StandardGenericFunction);
            //isClassFinalized.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteIsClassFinalized,
            //    "IsClassFinalized" });
            //allNamedGenericFunctions.Add (QuoteIsClassFinalized, isClassFinalized);

            bootstrapInitializeStandardMethod (method0ForMakeInstance,
                                               "Create an instance",
                                               QuoteMethodMakeInstance,
                                               "Method:MakeInstance",
                                               (MakeInstanceMethodSignature) makeInstanceMethod0Impl,
                                               KW.Primary,
                                               CL.List (funcallableStandardClass)
                                               );

            bootstrapInitializeStandardGenericFunction (makeInstance,
                                                        "Creates an instance of a class.",
                                                        QuoteMakeInstance,
                                                        "MakeInstance",
                                                        CL.List(QuoteClass, QuoteAndRest, QuoteInitargs),
                                                        CL.List (method0ForMakeInstance),
                                                        CL.List<ICollection<object>>(null));
            setFuncallableInstanceFunction (makeInstance, computeDiscriminatingFunctionTrampoline);

            //makeInstancesObsolete.SetClass (StandardGenericFunction);
            //makeInstancesObsolete.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteMakeInstancesObsolete,
            //    "MakeInstancesObsolete" });
            //allNamedGenericFunctions.Add (QuoteMakeInstancesObsolete, makeInstancesObsolete);

            //makeLoadForm.SetClass (StandardGenericFunction);
            //makeLoadForm.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteMakeLoadForm,
            //    "MakeLoadForm" });
            //allNamedGenericFunctions.Add (QuoteMakeLoadForm, makeLoadForm);

            //makeMethodLambda.SetClass (StandardGenericFunction);
            //makeMethodLambda.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteMakeMethodLambda,
            //    "MakeMethodLambda" });
            //allNamedGenericFunctions.Add (QuoteMakeMethodLambda, makeMethodLambda);

            //mapDependents.SetClass (StandardGenericFunction);
            //mapDependents.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteMapDependents,
            //    "MapDependents" });
            //allNamedGenericFunctions.Add (QuoteMapDependents, mapDependents);

            //methodCombination.SetClass (StandardGenericFunction);
            //methodCombination.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteMethodCombination,
            //    "MethodCombination" });
            //allNamedGenericFunctions.Add (QuoteMethodCombination, methodCombination);

            bootstrapInitializeStandardGenericFunction (methodFunction,
                                                        "Returns the function of a method.",
                                                        QuoteMethodFunction,
                                                        "MethodFunction",
                                                        CL.List (QuoteMethod),
                                                        null,
                                                        CL.List<ICollection<object>>(null));

            //methodGenericFunction.SetClass (StandardGenericFunction);
            //methodGenericFunction.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteMethodGenericFunction,
            //    "MethodGenericFunction" });
            //allNamedGenericFunctions.Add (QuoteMethodGenericFunction, methodGenericFunction);

            //methodLambdaList.SetClass (StandardGenericFunction);
            //methodLambdaList.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteMethodLambdaList,
            //    "MethodLambdaList" });
            //allNamedGenericFunctions.Add (QuoteMethodLambdaList, methodLambdaList);

            bootstrapInitializeStandardGenericFunction (methodSpecializers,
                                                        "Returns the specializers of a method.",
                                                        QuoteMethodSpecializers,
                                                        "MethodSpecializers",
                                                        CL.List (QuoteMethod),
                                                        null,
                                                        CL.List<ICollection<object>>(null));

            bootstrapInitializeStandardGenericFunction (methodQualifier,
                                                        "Returns the qualifier of a method.",
                                                        QuoteMethodQualifier,
                                                        "MethodQualifier",
                                                        CL.List (QuoteMethod),
                                                        null,
                                                        CL.List<ICollection<object>>(null));

            //noApplicableMethod.SetClass (StandardGenericFunction);
            //noApplicableMethod.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteNoApplicableMethod,
            //    "NoApplicableMethod" });
            //allNamedGenericFunctions.Add (QuoteNoApplicableMethod, noApplicableMethod);

            //noNextMethod.SetClass (StandardGenericFunction);
            //noNextMethod.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteNoNextMethod,
            //    "NoNextMethod" });
            //allNamedGenericFunctions.Add (QuoteNoNextMethod, noNextMethod);

            //readerMethodClass.SetClass (StandardGenericFunction);
            //readerMethodClass.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteReaderMethodClass,
            //    "ReaderMethodClass" });
            //allNamedGenericFunctions.Add (QuoteReaderMethodClass, readerMethodClass);

            //reinitializeInstance.SetClass (StandardGenericFunction);
            //reinitializeInstance.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteReinitializeInstance,
            //    "ReinitializeInstance" });
            //allNamedGenericFunctions.Add (QuoteReinitializeInstance, reinitializeInstance);

            //removeDependents.SetClass (StandardGenericFunction);
            //removeDependents.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteRemoveDependents,
            //    "RemoveDependents" });
            //allNamedGenericFunctions.Add (QuoteRemoveDependents, removeDependents);

            //removeDirectMethod.SetClass (StandardGenericFunction);
            //removeDirectMethod.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteRemoveDirectMethod,
            //    "RemoveDirectMethod" });
            //allNamedGenericFunctions.Add (QuoteRemoveDirectMethod, removeDirectMethod);

            //removeDirectSubclass.SetClass (StandardGenericFunction);
            //removeDirectSubclass.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteRemoveDirectSubclass,
            //    "RemoveDirectSubclass" });
            //allNamedGenericFunctions.Add (QuoteRemoveDirectSubclass, removeDirectSubclass);

            //removeMethod.SetClass (StandardGenericFunction);
            //removeMethod.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteRemoveMethod,
            //    "RemoveMethod" });
            //allNamedGenericFunctions.Add (QuoteRemoveMethod, removeMethod);

            bootstrapInitializeStandardMethod (method0ForSharedInitialize,
                                               "Initialize an instance",
                                               QuoteMethodSharedInitialize,
                                               "Method:SharedInitialize",
                                               (SharedInitializeMethodSignature) sharedInitializeMethod0Impl,
                                               KW.Primary,
                                               null
                                               );


            bootstrapInitializeStandardGenericFunction (sharedInitialize,
                                                        "Initializes an instance of a class.",
                                                        QuoteSharedInitialize,
                                                        "SharedInitialize",
                                                        CL.List(QuoteInstance, QuoteSlots, QuoteAndRest, QuoteInitargs),
                                                        CL.List (method0ForSharedInitialize),
                                                        CL.List<ICollection<object>>(null));
            setFuncallableInstanceFunction (sharedInitialize, computeDiscriminatingFunctionTrampoline);

            //setClassDirectSlots.SetClass (StandardGenericFunction);
            //setClassDirectSlots.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteSetClassDirectSlots,
            //    "SetClassDirectSlots" });
            //allNamedGenericFunctions.Add (QuoteSetClassDirectSlots, setClassDirectSlots);

            //setClassDirectSuperclasses.SetClass (StandardGenericFunction);
            //setClassDirectSuperclasses.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteSetClassDirectSuperclasses,
            //    "SetClassDirectSuperclasses" });
            //allNamedGenericFunctions.Add (QuoteSetClassDirectSuperclasses, setClassDirectSuperclasses);

            //setClassName.SetClass (StandardGenericFunction);
            //setClassName.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteSetClassName,
            //    "SetClassName" });
            //allNamedGenericFunctions.Add (QuoteSetClassName, setClassName);

            //setClassSlots.SetClass (StandardGenericFunction);
            //setClassSlots.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteSetClassSlots,
            //    "SetClassSlots" });
            //allNamedGenericFunctions.Add (QuoteSetClassSlots, setClassSlots);

            bootstrapInitializeStandardGenericFunction (setGenericFunctionApplicationCache,
                            "Mutates the application cache of a generic function.",
                                QuoteSetGenericFunctionApplicationCache,
                                "SetGenericFunctionApplicationCache",
                                CL.List(QuoteGenericFunction),
                                null,
                                                         CL.List<ICollection<object>>(null,null)) ;
 

            //setGenericFunctionName.SetClass (StandardGenericFunction);
            //setGenericFunctionName.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteSetGenericFunctionName,
            //    "SetGenericFunctionName" });
            //allNamedGenericFunctions.Add (QuoteSetGenericFunctionName, setGenericFunctionName);

            bootstrapInitializeStandardGenericFunction (setSlotValueUsingClass,
                                                        "Implementation of SetSlotValue",
                                                        QuoteSetSlotValueUsingClass,
                                                        "SetSlotValueUsingClass",
                                                        CL.List(QuoteNewValue, QuoteClass,QuoteInstance,QuoteSlotDefinition),
                                                        null,
                                                        CL.List<ICollection<object>>(null,null,null,null)
                                                     );

            //isSlotBoundUsingClass.SetClass (StandardGenericFunction);
            //isSlotBoundUsingClass.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteIsSlotBoundUsingClass,
            //    "IsSlotBoundUsingClass" });
            //allNamedGenericFunctions.Add (QuoteIsSlotBoundUsingClass, isSlotBoundUsingClass);

            //slotDefinitionAllocation.SetClass (StandardGenericFunction);
            //slotDefinitionAllocation.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteSlotDefinitionAllocation,
            //    "SlotDefinitionAllocation" });
            //allNamedGenericFunctions.Add (QuoteSlotDefinitionAllocation, slotDefinitionAllocation);

            //slotDefinitionInitargs.SetClass (StandardGenericFunction);
            //slotDefinitionInitargs.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteSlotDefinitionInitargs,
            //    "SlotDefinitionInitargs" });
            //allNamedGenericFunctions.Add (QuoteSlotDefinitionInitargs, slotDefinitionInitargs);

            //slotDefinitionInitform.SetClass (StandardGenericFunction);
            //slotDefinitionInitform.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteSlotDefinitionInitform,
            //    "SlotDefinitionInitform" });
            //allNamedGenericFunctions.Add (QuoteSlotDefinitionInitform, slotDefinitionInitform);

            //slotDefinitionInitfunction.SetClass (StandardGenericFunction);
            //slotDefinitionInitfunction.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteSlotDefinitionInitfunction,
            //    "SlotDefinitionInitfunction" });
            //allNamedGenericFunctions.Add (QuoteSlotDefinitionInitfunction, slotDefinitionInitfunction);

            bootstrapInitializeStandardGenericFunction (slotDefinitionLocation,
                                                        "Returns the locatio of the slot.",
                                                         QuoteSlotDefinitionLocation,
                                                         "SlotDefinitionLocation",
                                                         CL.List(QuoteSlotDefinition),
                                                         null,
                                                        CL.List<ICollection<object>>(null));

            bootstrapInitializeStandardGenericFunction (slotDefinitionName,
                                                        "Returns the name of the slot.",
                                                         QuoteSlotDefinitionName,
                                                         "SlotDefinitionName",
                                                         CL.List(QuoteSlotDefinition),
                                                         null,
                                                        CL.List<ICollection<object>>(null));

            //slotDefinitionType.SetClass (StandardGenericFunction);
            //slotDefinitionType.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteSlotDefinitionType,
            //    "SlotDefinitionType" });
            //allNamedGenericFunctions.Add (QuoteSlotDefinitionType, slotDefinitionType);

            //slotDefinitionReaders.SetClass (StandardGenericFunction);
            //slotDefinitionReaders.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteSlotDefinitionReaders,
            //    "SlotDefinitionReaders" });
            //allNamedGenericFunctions.Add (QuoteSlotDefinitionReaders, slotDefinitionReaders);

            //slotDefinitionWriters.SetClass (StandardGenericFunction);
            //slotDefinitionWriters.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteSlotDefinitionWriters,
            //    "SlotDefinitionWriters" });
            //allNamedGenericFunctions.Add (QuoteSlotDefinitionWriters, slotDefinitionWriters);

            //slotMakunboundUsingClass.SetClass (StandardGenericFunction);
            //slotMakunboundUsingClass.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteSlotMakunboundUsingClass,
            //    "SlotMakunboundUsingClass" });
            //allNamedGenericFunctions.Add (QuoteSlotMakunboundUsingClass, slotMakunboundUsingClass);

            //slotMissing.SetClass (StandardGenericFunction);
            //slotMissing.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteSlotMissing,
            //    "SlotMissing" });
            //allNamedGenericFunctions.Add (QuoteSlotMissing, slotMissing);

            //slotUnbound.SetClass (StandardGenericFunction);
            //slotUnbound.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteSlotUnbound,
            //    "SlotUnbound" });
            //allNamedGenericFunctions.Add (QuoteSlotUnbound, slotUnbound);

            bootstrapInitializeStandardGenericFunction (slotValueUsingClass,
                                                        "Implementation of SlotValue",
                                                        QuoteSlotValueUsingClass,
                                                        "SlotValueUsingClass",
                                                        CL.List(QuoteClass,QuoteInstance,QuoteSlotDefinition),
                                                        null,
                                                        CL.List<ICollection<object>>(null,null,null));

            //specializerDirectGenericFunctions.SetClass (StandardGenericFunction);
            //specializerDirectGenericFunctions.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteSpecializerDirectGenericFunctions,
            //    "SpecializerDirectGenericFunctions" });
            //allNamedGenericFunctions.Add (QuoteSpecializerDirectGenericFunctions, specializerDirectGenericFunctions);

            //specializerDirectMethods.SetClass (StandardGenericFunction);
            //specializerDirectMethods.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteSpecializerDirectMethods,
            //    "SpecializerDirectMethods" });
            //allNamedGenericFunctions.Add (QuoteSpecializerDirectMethods, specializerDirectMethods);

            //standardAccessorMethod.SetClass (StandardGenericFunction);
            //standardAccessorMethod.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteStandardAccessorMethod,
            //    "StandardAccessorMethod" });
            //allNamedGenericFunctions.Add (QuoteStandardAccessorMethod, standardAccessorMethod);

            //updateDependent.SetClass (StandardGenericFunction);
            //updateDependent.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteUpdateDependent,
            //    "UpdateDependent" });
            //allNamedGenericFunctions.Add (QuoteUpdateDependent, updateDependent);

            //updateInstanceForDifferentClass.SetClass (StandardGenericFunction);
            //updateInstanceForDifferentClass.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteUpdateInstanceForDifferentClass,
            //    "UpdateInstanceForDifferentClass" });
            //allNamedGenericFunctions.Add (QuoteUpdateInstanceForDifferentClass, updateInstanceForDifferentClass);

            //updateInstanceForRedefinedClass.SetClass (StandardGenericFunction);
            //updateInstanceForRedefinedClass.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteUpdateInstanceForRedefinedClass,
            //    "UpdateInstanceForRedefinedClass" });
            //allNamedGenericFunctions.Add (QuoteUpdateInstanceForRedefinedClass, updateInstanceForRedefinedClass);

            //validateSuperclass.SetClass (StandardGenericFunction);
            //validateSuperclass.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteValidateSuperclass,
            //    "ValidateSuperclass" });
            //allNamedGenericFunctions.Add (QuoteValidateSuperclass, validateSuperclass);

            //writerMethodClass.SetClass (StandardGenericFunction);
            //writerMethodClass.SetSlots (new object [] {
            //    "missing documentation string",
            //    QuoteWriterMethodClass,
            //    "WriterMethodClass" });
            //allNamedGenericFunctions.Add (QuoteWriterMethodClass, writerMethodClass);

            return true;
        }

        // closClass is a funcallableStandardClass
        static object allocateInstanceMethod0Impl (NextMethodFunction callNextMethod, StandardInstance closClass, params object [] initargs)
        {
            return ManifestInstance.CreateFuncallableInstance (closClass, ((ICollection<StandardInstance>) ClassSlots (closClass)).Count);
        }

        static ConsList<StandardInstance> classPrecedenceListMethod0Impl (NextMethodFunction callNextMethod, StandardInstance closClass)
        {
            return (ConsList<StandardInstance>) SlotValue (closClass, QuotePrecedenceList);
        }

        static ConsList<StandardInstance> classPrecedenceListMethod1Impl (NextMethodFunction callNextMethod, StandardInstance closClass)
        {
            throw new NotImplementedException();
        }

        // closClass is standardClass
        static StandardInstance [] classSlotsMethod0Impl (NextMethodFunction callNextMethod, StandardInstance closClass)
        {
            // sanity check
            if (closClass != standardClass)
                throw new NotImplementedException ();
            return theEffectiveSlotsOfStandardClass;
        }

        static StandardInstance [] classSlotsMethod1Impl (NextMethodFunction callNextMethod, StandardInstance closClass)
        {
            throw new NotImplementedException();
        }

        // closClass is a built-in class
        static StandardInstance [] classSlotsMethod2Impl (NextMethodFunction callNextMethod, StandardInstance closClass)
        {
            Type dotnetClass = (Type) SlotValue (closClass, QuoteDotnetType);
            MemberInfo [] dotnetSlots = dotnetClass.GetMembers (BindingFlags.GetField | BindingFlags.GetProperty | BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static);
            ConsList<StandardInstance> slots = ConsList<StandardInstance>.VectorToList (theEffectiveSlotsOfBuiltInClass);
            foreach (MemberInfo dotnetSlot in dotnetSlots) {
                if (dotnetSlot.MemberType == MemberTypes.Field) {
                    FieldInfo dotnetField = dotnetSlot as FieldInfo;
                    StandardInstance slot = bootstrapCreateBuiltInEffectiveSlot (dotnetField);
                    slots = new ConsList<StandardInstance> (slot, slots);
                }
                else if (dotnetSlot.MemberType == MemberTypes.Property) {
                    PropertyInfo dotnetProperty = dotnetSlot as PropertyInfo;
                    StandardInstance slot = bootstrapCreateBuiltInEffectiveSlot (dotnetProperty);
                    slots = new ConsList<StandardInstance> (slot, slots);
                }
                // otherwise ignore it
            }

            throw new NotImplementedException();
        }

        static StandardInstance [] classSlotsMethod3Impl (NextMethodFunction callNextMethod, StandardInstance closClass)
        {
            return (StandardInstance []) SlotValue (closClass, QuoteSlots);
        }

        static FuncallHandler computeDiscriminatingFunctionMethodImpl (NextMethodFunction callNextMethod, StandardInstance genericFunction)
        {
            return StandardDiscriminatingFunction.Create (genericFunction);
        }

        // functionName is a symbol
        static StandardInstance ensureGenericFunctionMethod0Impl (NextMethodFunction callNextMethod, object functionName, params object [] arguments)
        {
            StandardInstance existingGf = null;
            allNamedGenericFunctions.TryGetValue ((Symbol) functionName, out existingGf);
            return (StandardInstance) CL.Apply (ensureGenericFunctionUsingClass, existingGf, functionName, Cons.VectorToList (arguments));
        }

        static StandardInstance ensureGenericFunctionMethod1Impl (NextMethodFunction callNextMethod, object functionName, params object [] arguments)
        {
           throw new NotImplementedException();
        }

        // existingGf is null
        // functionName is a symbol
        static StandardInstance ensureGenericFunctionUsingClassMethod0Impl (NextMethodFunction callNextMethod, object existingGf, object functionName, params object [] arguments)
        {
            KeywordArgument<StandardInstance> genericFunctionClass = new KeywordArgument<StandardInstance> (KW.GenericFunctionClass);
            KeywordArgumentBase.ProcessKeywordArguments (new KeywordArgumentBase [] { genericFunctionClass }, arguments, true); // allowOtherKeys
            StandardInstance instance = 
                (StandardInstance)
                CL.Apply (makeInstance, 
                          genericFunctionClass.Supplied ? genericFunctionClass.Value : standardGenericFunction,
                          CL.Cons (KW.Name, CL.Cons (functionName, Utility.RemArgs (arguments, KW.GenericFunctionClass))));
            return instance;
        }

        static object initializeInstanceMethod0Impl (NextMethodFunction callNextMethod, StandardInstance instance, params object [] initargs)
        {
            return CL.Apply (sharedInitialize, instance, true, Cons.VectorToList (initargs));
        }

        static object makeInstanceMethod0Impl (NextMethodFunction callNextMethod, StandardInstance closClass, params object [] initargs)
        {
            Cons initargsList = Cons.VectorToList (initargs);
            StandardInstance instance = (StandardInstance) CL.Apply (allocateInstance, closClass, initargsList);
            CL.Apply (initializeInstance, instance, initargsList);
            return instance;
        }

        delegate object SlotInitializer (StandardInstance instance, StandardInstance effectiveSlot, Cons initargs);

        static SlotInitializer unboundSlotInitializer ()
        {
            return (SlotInitializer)
                delegate (StandardInstance instance, StandardInstance effectiveSlot, Cons initargs)
                {
                    return SetStandardInstanceAccess (instance, (int) slotDefinitionLocation(effectiveSlot), UnboundSlotValue);
                };
        }

        static SlotInitializer constantSlotInitializer (object initialValue)
        {
            return (SlotInitializer)
                delegate (StandardInstance instance, StandardInstance effectiveSlot, Cons initargs)
                {
                    return SetStandardInstanceAccess (instance, (int) slotDefinitionLocation(effectiveSlot), initialValue);
                };
        }

        static SlotInitializer keywordSlotInitializer (ConsList<Symbol> keywords)
        {
            return (SlotInitializer)
                delegate (StandardInstance instance, StandardInstance effectiveSlot, Cons initargs)
                {
                    object value = Utility.GetArgStar (initargs, keywords, null);
                    if (value == null)
                        throw new NotImplementedException ();
                    return SetStandardInstanceAccess (instance, (int) slotDefinitionLocation (effectiveSlot), value);
                };
        }

        static SlotInitializer slotDefinitionInitializer (StandardInstance effectiveSlot)
        {
            object value = SlotValue (effectiveSlot, QuoteInitializer);
            return (SlotInitializer) value;
        }

        static object sharedInitializeMethod0Impl (NextMethodFunction callNextMethod, StandardInstance instance, object slots, params object [] initargs)
        {
            Cons initarglist = Cons.VectorToList (initargs);
            StandardInstance [] effectiveSlots = (StandardInstance []) classSlots (ClassOf (instance));
            foreach (StandardInstance slot in effectiveSlots) {
                slotDefinitionInitializer (slot) (instance, slot, initarglist);
            }
            return instance;
        }

        //
        static StandardInstance lookupSlot (StandardInstance closClass, Symbol slotName)
        {
            StandardInstance [] slots = (StandardInstance []) classSlots (closClass);
            foreach (StandardInstance effectiveSlot in slots) {
                Symbol name = (Symbol) slotDefinitionName (effectiveSlot);
                if (slotName == name)
                    return effectiveSlot;
                }
            throw new NotImplementedException ("Slot lookup failed for " + slotName.Name);
        }

        // Various and sundry utilities.

        static Symbol closSymbol (string name)
        {
            return closPackage.Intern (name);
        }

        public static object [] Subvector (object [] source, int start, int limit)
        {
            object [] answer = new object [limit - start];
            Array.Copy (source, start, answer, 0, limit - start);
            return answer;
        }

        public static string StandardObjectName (StandardInstance so)
        {
            // all metaobjects have their StudlyName
            // in slot 2.
            return (string) so.InstanceRef (2);
        }

        public static bool StopHere ()
        {
            Debug.Assert (false);
            return true;
        }
    }
}

using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Reflection;

namespace Lisp
{
    delegate bool MethodCompare3 (StandardObject left, StandardObject right, Cons arglist);
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
        StandardObject generic;

        public MethodStepper (StandardObject generic)
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
        StandardObject generic;
        Cons primaries;
        object [] args;

        MethodStepper1 (MethodStepper outer, StandardObject generic, Cons primaries, object [] args)
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

        static public NextMethodFunction Create (MethodStepper outer, StandardObject generic, Cons primaries, object [] args)
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
        StandardObject generic;
        object info;
        object [] args;

        NoNextMethod (StandardObject generic, object info, object [] args)
        {
            this.generic = generic;
            this.info = info;
            this.args = args;
        }

        object Funcall (params object [] args)
        {
            throw new NotImplementedException ("No Next Method");
        }

        static public NextMethodFunction Create (StandardObject generic, object info, object [] args)
        {
            NoNextMethod nnm = new NoNextMethod (generic, info, args);
            return (NextMethodFunction) Delegate.CreateDelegate (typeof (NextMethodFunction), nnm, typeof (NoNextMethod)
                .GetMethod ("Funcall", BindingFlags.NonPublic | BindingFlags.Instance));
        }
    }

    class StandardDiscriminatingFunction
    {
        StandardObject genericFunction;

        StandardDiscriminatingFunction (StandardObject gf)
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

        static NextMethodFunction computeCallable (bool isGroundCase, StandardObject generic, EffectiveMethodCache cache, Cons keys, object [] arguments)
        {
            NextMethodFunction answer;
            if (isGroundCase) {
                Delegate m = (Delegate) CLOS.MethodFunction ((StandardObject) last (CLOS.InternalGenericFunctionMethods (generic)));
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

        object Funcall (StandardObject self, object [] arguments)
        {
            ConsList<ICollection<object>> singletons = CLOS.InternalGenericFunctionSingletonsList (genericFunction);
            Cons keys = getKeys (arguments, singletons);

            Cons appCache   = (Cons) CLOS.InternalGenericFunctionApplicationCache (genericFunction);
            // Cons lambdaList = (Cons) CLOS.InternalGenericFunctionLambdaList (genericFunction);
            // should do arity check here

            // check if we need to flush the cache
            if (!object.ReferenceEquals (appCache.Car, CLOS.GenericApplicationCacheTag)) {
                appCache = new Cons (CLOS.GenericApplicationCacheTag, new EffectiveMethodCache ());
                CLOS.InternalSetGenericFunctionApplicationCache (self, appCache);
            }

            EffectiveMethodCache cache = (EffectiveMethodCache) appCache.Cdr;
            NextMethodFunction ah;
            if (!cache.TryGetValue (keys, out ah)) {
                bool isGroundCase = CLOS.GenericInvocationGenerics.Contains (self)
                                    && arguments.Length > 0
                                    && CLOS.GenericInvocationGenerics.Contains ((StandardObject) arguments [0]);
                ah = computeCallable (isGroundCase, self, cache, keys, arguments);
                }

            return ah.Invoke (arguments);
        }

        public static FuncallHandler Create (StandardObject gf)
        {
            StandardDiscriminatingFunction self = new StandardDiscriminatingFunction (gf);
            return self.Funcall;
        }
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

        // Extension methods for StandardObject
        [DebuggerStepThrough]
        static StandardObject Class (this StandardObject obj)
        {
            return ((ManifestInstance) obj.Target).Class;
        }

        static object SetClass (this StandardObject obj, StandardObject closClass)
        {
            ((ManifestInstance) obj.Target).Class = closClass;
            return closClass;
        }

        static object InstanceRef (this StandardObject obj, int index)
        {
            return ((ManifestInstance) obj.Target).Slots [index];
        }

        static object InstanceSet (this StandardObject obj, int index, object newValue)
        {
            ((ManifestInstance) obj.Target).Slots [index] = newValue;
            return newValue;
        }

        static T InstanceSet<T> (this StandardObject obj, int index, T newValue)
        {
            ((ManifestInstance) obj.Target).Slots [index] = newValue;
            return newValue;
        }

        static object InstanceChange (this StandardObject obj, int index, SlotChangingFunction change)
        {
            object [] slots = ((ManifestInstance) obj.Target).Slots;
            object newValue = change.Invoke (slots [index]);
            slots [index] = newValue;
            return newValue;
        }

        static T InstanceChange<T> (this StandardObject obj, int index, SlotChangingFunction<T> change)
        {
            object [] slots = ((ManifestInstance) obj.Target).Slots;
            T newValue = change.Invoke ((T) slots [index]);
            slots [index] = newValue;
            return newValue;
        }

        public static object StandardInstanceAccess (StandardObject instance, int location)
        {
            return instance.InstanceRef (location);
        }

        public static object SetStandardInstanceAccess (StandardObject instance, int location, object newValue)
        {
            return instance.InstanceSet (location, newValue);
        }

        public static T SetStandardInstanceAccess<T> (StandardObject instance, int location, T newValue)
        {
            return instance.InstanceSet<T> (location, newValue);
        }

        public static object StandardInstanceChange (StandardObject instance, int location, SlotChangingFunction change)
        {
            return instance.InstanceChange (location, change);
        }

        public static T StandardInstanceChange<T> (StandardObject instance, int location, SlotChangingFunction<T> change)
        {
            return instance.InstanceChange<T> (location, change);
        }

        static internal int SerialNumber (this StandardObject obj)
        {
            return ((ManifestInstance) obj.Target).SerialNumber;
        }

        // Not an extension, but Instance-specific.
        static object SetSlots (this StandardObject instance, object [] slots)
        {
            ((ManifestInstance) instance.Target).Slots = slots;
            return slots;
        }

        static object setFuncallableInstanceFunction (StandardObject funcallableInstance, FuncallHandler handler)
        {
            ((ManifestInstance) funcallableInstance.Target).OnFuncall = handler;
            return handler;
        }

        //static object setFuncallableInstanceFunction (StandardObject funcallableInstance, MethodInfo handler)
        //{
        //    ((ManifestInstance) funcallableInstance.Target).OnFuncall = FuncallHandlerWrapper.Create (handler);
        //    return handler;
        //}

        #endregion StandardObjectExtensionMethods

        #region ExportedGenerics
        public static StandardObject AccessorMethodSlotDefinition
        {
            [DebuggerStepThrough]
            get
            {
                return accessorMethodSlotDefinition;
            }
        }

        public static StandardObject AddDependent
        {
            [DebuggerStepThrough]
            get
            {
                return addDependent;
            }
        }

        public static StandardObject AddDirectMethod
        {
            [DebuggerStepThrough]
            get
            {
                return addDirectMethod;
            }
        }

        public static StandardObject AddDirectSubclass
        {
            [DebuggerStepThrough]
            get
            {
                return addDirectSubclass;
            }
        }

        public static StandardObject AddMethod
        {
            [DebuggerStepThrough]
            get
            {
                return addMethod;
            }
        }

        public static StandardObject AllocateInstance
        {
            [DebuggerStepThrough]
            get
            {
                return allocateInstance;
            }
        }

        public static StandardObject BuiltInClass
        {
            [DebuggerStepThrough]
            get
            {
                return builtInClass;
            }
        }

        public static StandardObject ChangeClass
        {
            [DebuggerStepThrough]
            get
            {
                return changeClass;
            }
        }

        public static StandardObject ClassDefaultInitargs
        {
            [DebuggerStepThrough]
            get
            {
                return classDefaultInitargs;
            }
        }

        public static StandardObject ClassDirectDefaultInitargs
        {
            [DebuggerStepThrough]
            get
            {
                return classDirectDefaultInitargs;
            }
        }

        public static StandardObject ClassDirectSlots
        {
            [DebuggerStepThrough]
            get
            {
                return classDirectSlots;
            }
        }

        public static StandardObject ClassDirectSubclasses
        {
            [DebuggerStepThrough]
            get
            {
                return classDirectSubclasses;
            }
        }

        public static StandardObject ClassDirectSuperclasses
        {
            [DebuggerStepThrough]
            get
            {
                return classDirectSuperclasses;
            }
        }

        public static StandardObject ClassName
        {
            [DebuggerStepThrough]
            get
            {
                return className;
            }
        }

        public static StandardObject ClassPrecedenceList
        {
            [DebuggerStepThrough]
            get
            {
                return classPrecedenceList;
            }
        }

        public static StandardObject ClassPrototype
        {
            [DebuggerStepThrough]
            get
            {
                return classPrototype;
            }
        }

        public static StandardObject ClassSlots
        {
            [DebuggerStepThrough]
            get
            {
                return classSlots;
            }
        }

        public static StandardObject ClosClass
        {
            [DebuggerStepThrough]
            get
            {
                return closClass;
            }
        }

        public static StandardObject ComputeApplicableMethods
        {
            [DebuggerStepThrough]
            get
            {
                return computeApplicableMethods;
            }
        }

        public static StandardObject ComputeApplicableMethodsUsingClasses
        {
            [DebuggerStepThrough]
            get
            {
                return computeApplicableMethodsUsingClasses;
            }
        }

        public static StandardObject ComputeClassPrecedenceList
        {
            [DebuggerStepThrough]
            get
            {
                return computeClassPrecedenceList;
            }
        }

        public static StandardObject ComputeDefaultInitargs
        {
            [DebuggerStepThrough]
            get
            {
                return computeDefaultInitargs;
            }
        }

        public static StandardObject ComputeDiscriminatingFunction
        {
            [DebuggerStepThrough]
            get
            {
                return computeDiscriminatingFunction;
            }
        }

        public static StandardObject ComputeEffectiveMethod
        {
            [DebuggerStepThrough]
            get
            {
                return computeEffectiveMethod;
            }
        }

        public static StandardObject ComputeEffectiveSlotDefinition
        {
            [DebuggerStepThrough]
            get
            {
                return computeEffectiveSlotDefinition;
            }
        }

        public static StandardObject ComputeSlots
        {
            [DebuggerStepThrough]
            get
            {
                return computeSlots;
            }
        }

        public static StandardObject ComputeMethodMoreSpecific
        {
            [DebuggerStepThrough]
            get
            {
                return computeMethodMoreSpecific;
            }
        }

        public static StandardObject DefaultInitargs
        {
            [DebuggerStepThrough]
            get
            {
                return defaultInitargs;
            }
        }

        public static StandardObject DirectSlotDefinition
        {
            [DebuggerStepThrough]
            get
            {
                return directSlotDefinition;
            }
        }

        public static StandardObject DirectSlotDefinitionClass
        {
            [DebuggerStepThrough]
            get
            {
                return directSlotDefinitionClass;
            }
        }

        public static StandardObject EffectiveSlotDefinition
        {
            [DebuggerStepThrough]
            get
            {
                return effectiveSlotDefinition;
            }
        }

        public static StandardObject EffectiveSlotDefinitionClass
        {
            [DebuggerStepThrough]
            get
            {
                return effectiveSlotDefinitionClass;
            }
        }

        public static StandardObject EnsureClass
        {
            [DebuggerStepThrough]
            get
            {
                return ensureClass;
            }
        }

        public static StandardObject EnsureClassUsingClass
        {
            [DebuggerStepThrough]
            get
            {
                return ensureClassUsingClass;
            }
        }

        public static StandardObject EnsureGenericFunction
        {
            [DebuggerStepThrough]
            get
            {
                return ensureGenericFunction;
            }
        }

        public static StandardObject EnsureGenericFunctionUsingClass
        {
            [DebuggerStepThrough]
            get
            {
                return ensureGenericFunctionUsingClass;
            }
        }

        public static StandardObject EqlSpecializer
        {
            [DebuggerStepThrough]
            get
            {
                return eqlSpecializer;
            }
        }

        public static StandardObject EqlSpecializerObject
        {
            [DebuggerStepThrough]
            get
            {
                return eqlSpecializerObject;
            }
        }

        public static StandardObject ExtractLambdaList
        {
            [DebuggerStepThrough]
            get
            {
                return extractLambdaList;
            }
        }

        public static StandardObject ExtractSpecializerNames
        {
            [DebuggerStepThrough]
            get
            {
                return extractSpecializerNames;
            }
        }

        public static StandardObject FinalizeInheritance
        {
            [DebuggerStepThrough]
            get
            {
                return finalizeInheritance;
            }
        }

        public static StandardObject FindClass
        {
            [DebuggerStepThrough]
            get
            {
                return findClass;
            }
        }

        public static StandardObject FindMethod
        {
            [DebuggerStepThrough]
            get
            {
                return findMethod;
            }
        }

        public static StandardObject FindMethodCombination
        {
            [DebuggerStepThrough]
            get
            {
                return findMethodCombination;
            }
        }

        public static StandardObject ForwardReferencedClass
        {
            [DebuggerStepThrough]
            get
            {
                return forwardReferencedClass;
            }
        }

        public static StandardObject FuncallableStandardClass
        {
            [DebuggerStepThrough]
            get
            {
                return funcallableStandardClass;
            }
        }

        public static StandardObject FuncallableStandardObject
        {
            [DebuggerStepThrough]
            get
            {
                return funcallableStandardObject;
            }
        }

        public static StandardObject FunctionKeywords
        {
            [DebuggerStepThrough]
            get
            {
                return functionKeywords;
            }
        }

        public static StandardObject GenericFunction
        {
            [DebuggerStepThrough]
            get
            {
                return genericFunction;
            }
        }

        public static StandardObject GenericFunctionArgumentPrecedenceOrder
        {
            [DebuggerStepThrough]
            get
            {
                return genericFunctionArgumentPrecedenceOrder;
            }
        }

        public static StandardObject GenericFunctionDeclarations
        {
            [DebuggerStepThrough]
            get
            {
                return genericFunctionDeclarations;
            }
        }

        public static StandardObject GenericFunctionLambdaList
        {
            [DebuggerStepThrough]
            get
            {
                return genericFunctionLambdaList;
            }
        }

        public static StandardObject GenericFunctionMethodClass
        {
            [DebuggerStepThrough]
            get
            {
                return genericFunctionMethodClass;
            }
        }

        public static StandardObject GenericFunctionMethodCombination
        {
            [DebuggerStepThrough]
            get
            {
                return genericFunctionMethodCombination;
            }
        }

        public static StandardObject GenericFunctionMethods
        {
            [DebuggerStepThrough]
            get
            {
                return genericFunctionMethods;
            }
        }

        public static StandardObject GenericFunctionName
        {
            [DebuggerStepThrough]
            get
            {
                return genericFunctionName;
            }
        }

        public static StandardObject InitializeInstance
        {
            [DebuggerStepThrough]
            get
            {
                return initializeInstance;
            }
        }

        public static StandardObject InternEqlSpecializer
        {
            [DebuggerStepThrough]
            get
            {
                return internEqlSpecializer;
            }
        }

        public static StandardObject IsClassFinalized
        {
            [DebuggerStepThrough]
            get
            {
                return isClassFinalized;
            }
        }

        public static StandardObject MakeInstance
        {
            [DebuggerStepThrough]
            get
            {
                return makeInstance;
            }
        }

        public static StandardObject MakeInstancesObsolete
        {
            [DebuggerStepThrough]
            get
            {
                return makeInstancesObsolete;
            }
        }

        public static StandardObject MakeLoadForm
        {
            [DebuggerStepThrough]
            get
            {
                return makeLoadForm;
            }
        }

        public static StandardObject MakeMethodLambda
        {
            [DebuggerStepThrough]
            get
            {
                return makeMethodLambda;
            }
        }

        public static StandardObject MapDependents
        {
            [DebuggerStepThrough]
            get
            {
                return mapDependents;
            }
        }

        public static StandardObject Metaobject
        {
            [DebuggerStepThrough]
            get
            {
                return metaobject;
            }
        }

        public static StandardObject Method
        {
            [DebuggerStepThrough]
            get
            {
                return method;
            }
        }

        public static StandardObject MethodCombination
        {
            [DebuggerStepThrough]
            get
            {
                return methodCombination;
            }
        }

        public static StandardObject MethodFunction
        {
            [DebuggerStepThrough]
            get
            {
                return methodFunction;
            }
        }

        public static StandardObject MethodGenericFunction
        {
            [DebuggerStepThrough]
            get
            {
                return methodGenericFunction;
            }
        }

        public static StandardObject MethodLambdaList
        {
            [DebuggerStepThrough]
            get
            {
                return methodLambdaList;
            }
        }

        public static StandardObject MethodSpecializers
        {
            [DebuggerStepThrough]
            get
            {
                return methodSpecializers;
            }
        }

        public static StandardObject MethodQualifier
        {
            [DebuggerStepThrough]
            get
            {
                return methodQualifier;
            }
        }

        public static StandardObject NoApplicableMethod
        {
            [DebuggerStepThrough]
            get
            {
                return noApplicableMethod;
            }
        }

        public static StandardObject NoNextMethod
        {
            [DebuggerStepThrough]
            get
            {
                return noNextMethod;
            }
        }

        public static StandardObject ReaderMethodClass
        {
            [DebuggerStepThrough]
            get
            {
                return readerMethodClass;
            }
        }

        public static StandardObject ReinitializeInstance
        {
            [DebuggerStepThrough]
            get
            {
                return reinitializeInstance;
            }
        }

        public static StandardObject RemoveDependents
        {
            [DebuggerStepThrough]
            get
            {
                return removeDependents;
            }
        }

        public static StandardObject RemoveDirectMethod
        {
            [DebuggerStepThrough]
            get
            {
                return removeDirectMethod;
            }
        }

        public static StandardObject RemoveDirectSubclass
        {
            [DebuggerStepThrough]
            get
            {
                return removeDirectSubclass;
            }
        }

        public static StandardObject RemoveMethod
        {
            [DebuggerStepThrough]
            get
            {
                return removeMethod;
            }
        }

        public static StandardObject SharedInitialize
        {
            [DebuggerStepThrough]
            get
            {
                return sharedInitialize;
            }
        }

        public static StandardObject SetClassName
        {
            [DebuggerStepThrough]
            get
            {
                return setClassName;
            }
        }

        public static StandardObject SetGenericFunctionName
        {
            [DebuggerStepThrough]
            get
            {
                return setGenericFunctionName;
            }
        }

        public static StandardObject SetSlotValueUsingClass
        {
            [DebuggerStepThrough]
            get
            {
                return setSlotValueUsingClass;
            }
        }

        public static StandardObject IsSlotBoundUsingClass
        {
            [DebuggerStepThrough]
            get
            {
                return isSlotBoundUsingClass;
            }
        }

        public static StandardObject SlotDefinition
        {
            [DebuggerStepThrough]
            get
            {
                return slotDefinition;
            }
        }

        public static StandardObject SlotDefinitionAllocation
        {
            [DebuggerStepThrough]
            get
            {
                return slotDefinitionAllocation;
            }
        }

        public static StandardObject SlotDefinitionInitargs
        {
            [DebuggerStepThrough]
            get
            {
                return slotDefinitionInitargs;
            }
        }

        public static StandardObject SlotDefinitionInitform
        {
            [DebuggerStepThrough]
            get
            {
                return slotDefinitionInitform;
            }
        }

        public static StandardObject SlotDefinitionInitfunction
        {
            [DebuggerStepThrough]
            get
            {
                return slotDefinitionInitfunction;
            }
        }

        public static StandardObject SlotDefinitionLocation
        {
            [DebuggerStepThrough]
            get
            {
                return slotDefinitionLocation;
            }
        }

        public static StandardObject SlotDefinitionName
        {
            [DebuggerStepThrough]
            get
            {
                return slotDefinitionName;
            }
        }

        public static StandardObject SlotDefinitionType
        {
            [DebuggerStepThrough]
            get
            {
                return slotDefinitionType;
            }
        }

        public static StandardObject SlotDefinitionReaders
        {
            [DebuggerStepThrough]
            get
            {
                return slotDefinitionReaders;
            }
        }

        public static StandardObject SlotDefinitionWriters
        {
            [DebuggerStepThrough]
            get
            {
                return slotDefinitionWriters;
            }
        }

        public static StandardObject SlotMakunboundUsingClass
        {
            [DebuggerStepThrough]
            get
            {
                return slotMakunboundUsingClass;
            }
        }

        public static StandardObject SlotMissing
        {
            [DebuggerStepThrough]
            get
            {
                return slotMissing;
            }
        }

        public static StandardObject SlotUnbound
        {
            [DebuggerStepThrough]
            get
            {
                return slotUnbound;
            }
        }

        public static StandardObject SlotValueUsingClass
        {
            [DebuggerStepThrough]
            get
            {
                return slotValueUsingClass;
            }
        }

        public static StandardObject Specializer
        {
            [DebuggerStepThrough]
            get
            {
                return specializer;
            }
        }

        public static StandardObject SpecializerDirectGenericFunctions
        {
            [DebuggerStepThrough]
            get
            {
                return specializerDirectGenericFunctions;
            }
        }

        public static StandardObject SpecializerDirectMethods
        {
            [DebuggerStepThrough]
            get
            {
                return specializerDirectMethods;
            }
        }

        public static StandardObject StandardAccessorMethod
        {
            [DebuggerStepThrough]
            get
            {
                return standardAccessorMethod;
            }
        }

        public static StandardObject StandardAccessorMethodClass
        {
            [DebuggerStepThrough]
            get
            {
                return standardAccessorMethodClass;
            }
        }

        public static StandardObject StandardClass
        {
            [DebuggerStepThrough]
            get
            {
                return standardClass;
            }
        }

        public static StandardObject StandardDirectSlotDefinition
        {
            [DebuggerStepThrough]
            get
            {
                return standardDirectSlotDefinition;
            }
        }

        public static StandardObject StandardEffectiveSlotDefinition
        {
            [DebuggerStepThrough]
            get
            {
                return standardEffectiveSlotDefinition;
            }
        }

        public static StandardObject StandardGenericFunction
        {
            [DebuggerStepThrough]
            get
            {
                return standardGenericFunction;
            }
        }

        public static StandardObject StandardMethod
        {
            [DebuggerStepThrough]
            get
            {
                return standardMethod;
            }
        }

        public static StandardObject StandardObjectClass
        {
            [DebuggerStepThrough]
            get
            {
                return standardObjectClass;
            }
        }

        public static StandardObject StandardReaderMethod
        {
            [DebuggerStepThrough]
            get
            {
                return standardReaderMethod;
            }
        }

        public static StandardObject StandardSlotDefinition
        {
            [DebuggerStepThrough]
            get
            {
                return standardSlotDefinition;
            }
        }

        public static StandardObject StandardWriterMethod
        {
            [DebuggerStepThrough]
            get
            {
                return standardWriterMethod;
            }
        }

        public static StandardObject Top
        {
            [DebuggerStepThrough]
            get
            {
                return top;
            }
        }

        public static StandardObject UpdateDependent
        {
            [DebuggerStepThrough]
            get
            {
                return updateDependent;
            }
        }

        public static StandardObject UpdateInstanceForDifferentClass
        {
            [DebuggerStepThrough]
            get
            {
                return updateInstanceForDifferentClass;
            }
        }

        public static StandardObject UpdateInstanceForRedefinedClass
        {
            [DebuggerStepThrough]
            get
            {
                return updateInstanceForRedefinedClass;
            }
        }

        public static StandardObject ValidateSuperclass
        {
            [DebuggerStepThrough]
            get
            {
                return validateSuperclass;
            }
        }

        public static StandardObject WriterMethodClass
        {
            [DebuggerStepThrough]
            get
            {
                return writerMethodClass;
            }
        }

        public static bool IsSlotBound (StandardObject instance, Symbol slotName)
        {
            throw new NotImplementedException ();
        }

        public static bool IsSlotExists (StandardObject instance, Symbol slotName)
        {
            throw new NotImplementedException ();
        }

        public static StandardObject SlotMakunbound (StandardObject instance, Symbol slotName)
        {
            throw new NotImplementedException ();
        }

        public static object SlotValue (StandardObject instance, Symbol slotName)
        {
            StandardObject closClass = instance.Class();
            return SlotValueUsingClass (closClass, instance, lookupSlot (closClass, slotName));
        }

        public static object SetSlotValue (object newValue, StandardObject instance, Symbol slotName)
        {
            StandardObject closClass = instance.Class();
            return SetSlotValueUsingClass (newValue, closClass, instance, lookupSlot (closClass, slotName));
        }

        public static T SetSlotValue<T> (T newValue, StandardObject instance, Symbol slotName)
        {
            throw new NotImplementedException ();
        }

        #endregion ExportedGenerics

        #region Symbols

        static readonly Package closPackage = Package.Clos;

        static readonly Symbol QuoteAccessorMethodSlotDefinition = closSymbol ("AccessorMethodSlotDefinition");
        static readonly Symbol QuoteAddDependent = closSymbol ("AddDependent");
        static readonly Symbol QuoteAddDirectMethod = closSymbol ("AddDirectMethod");
        static readonly Symbol QuoteAddDirectSubclass = closSymbol ("AddDirectSubclass");
        static readonly Symbol QuoteAddMethod = Package.CommonLisp.FindSymbol ("ADD-METHOD");
        static readonly Symbol QuoteAllocateInstance = Package.CommonLisp.FindSymbol ("ALLOCATE-INSTANCE");
        static readonly Symbol QuoteAllocation = closSymbol ("ALLOCATION");
        static readonly Symbol QuoteAndAllowOtherKeys = Package.CommonLisp.FindSymbol ("&ALLOW-OTHER-KEYS");
        static readonly Symbol QuoteAndKey = Package.CommonLisp.FindSymbol ("&KEY");
        static readonly Symbol QuoteAndRest = Package.CommonLisp.FindSymbol ("&REST");
        static readonly Symbol QuoteAnonymous = Package.CommonLisp.FindSymbol ("[unnamed]");
        static readonly Symbol QuoteApplicationCache = closSymbol ("APPLICATION-CACHE");
        static readonly Symbol QuoteArgumentPrecedenceOrder = closSymbol ("ARGUMENT-PRECEDENCE-ORDER");
        static readonly Symbol QuoteArguments = closSymbol ("ARGUMENTS");
        static readonly Symbol QuoteArity = closSymbol ("ARITY");
        static readonly Symbol QuoteBuiltInClass = closSymbol ("BUILT-IN-CLASS");
        static readonly Symbol QuoteBuiltInEffectiveSlotDefinition = closSymbol ("BUILT-IN-EFFECTIVE-SLOT-DEFINITION");
        static readonly Symbol QuoteBuiltInSlotDefinition = closSymbol ("BUILT-IN-SLOT-DEFINITION");
        static readonly Symbol QuoteChangeClass = closSymbol ("ChangeClass");
        static readonly Symbol QuoteClass = closSymbol ("CLASS");
        static readonly Symbol QuoteClassDefaultInitargs = closSymbol ("ClassDefaultInitargs");
        static readonly Symbol QuoteClassDirectDefaultInitargs = closSymbol ("ClassDirectDefaultInitargs");
        static readonly Symbol QuoteClassDirectSlots = closSymbol ("CLASS-DIRECT-SLOTS");
        static readonly Symbol QuoteClassDirectSubclasses = closSymbol ("ClassDirectSubclasses");
        static readonly Symbol QuoteClassDirectSuperclasses = closSymbol ("ClassDirectSuperclasses");
        static readonly Symbol QuoteClassInitializers = closSymbol ("CLASS-INITIALIZERS");
        static readonly Symbol QuoteClassName = closSymbol ("CLASS-NAME");
        static readonly Symbol QuoteClassPrecedenceList = closSymbol ("CLASS-PRECEDENCE-LIST");
        static readonly Symbol QuoteClassPrototype = closSymbol ("CLASS-PROTOTYPE");
        static readonly Symbol QuoteClassSlots = closSymbol ("CLASS-SLOTS");
        static readonly Symbol QuoteComputeApplicableMethods = closSymbol ("COMPUTE-APPLICABLE-METHODS");
        static readonly Symbol QuoteComputeApplicableMethodsUsingClasses = closSymbol ("ComputeApplicableMethodsUsingClasses");
        static readonly Symbol QuoteComputeClassPrecedenceList = closSymbol ("COMPUTE-CLASS-PRECEDENCE-LIST");
        static readonly Symbol QuoteComputeDefaultInitargs = closSymbol ("ComputeDefaultInitargs");
        static readonly Symbol QuoteComputeDiscriminatingFunction = closSymbol ("COMPUTE-DISCRIMINATING-FUNCTION");
        static readonly Symbol QuoteComputeEffectiveMethod = closSymbol ("COMPUTE-EFFECTIVE-METHOD");
        static readonly Symbol QuoteComputeEffectiveSlotDefinition = closSymbol ("ComputeEffectiveSlotDefinition");
        static readonly Symbol QuoteComputeMethodMoreSpecific = closSymbol ("COMPUTE-METHOD-MORE-SPECIFIC");
        static readonly Symbol QuoteComputeSlots = closSymbol ("COMPUTE-SLOTS");
        static readonly Symbol QuoteCons = closSymbol ("CONS");
        static readonly Symbol QuoteDeclarations = closSymbol ("DECLARATIONS");
        static readonly Symbol QuoteDefaultInitargs = closSymbol ("DEFAULT-INITARGS");
        static readonly Symbol QuoteDirectDefaultInitargs = closSymbol ("DIRECT-DEFAULT-INITARGS");
        static readonly Symbol QuoteDirectMethods = closSymbol ("DIRECT-METHODS");
        static readonly Symbol QuoteDirectSlotDefinition = closSymbol ("DIRECT-SLOT-DEFINITION");
        static readonly Symbol QuoteDirectSlotDefinitionClass = closSymbol ("DirectSlotDefinitionClass");
        static readonly Symbol QuoteDirectSlots = closSymbol ("DIRECT-SLOTS");
        static readonly Symbol QuoteDirectSubclasses = closSymbol ("DIRECT-SUBCLASSES");
        static readonly Symbol QuoteDirectSuperclasses = closSymbol ("DIRECT-SUPERCLASSES");
        static readonly Symbol QuoteDocumentation = closSymbol ("DOCUMENTATION");
        static readonly Symbol QuoteDotnetType = closSymbol ("DOTNET-TYPE");
        static readonly Symbol QuoteEffectiveSlotDefinition = closSymbol ("Effective-Slot-Definition");
        static readonly Symbol QuoteEffectiveSlotDefinitionClass = closSymbol ("EffectiveSlotDefinitionClass");
        static readonly Symbol QuoteEffectiveSlots = closSymbol ("EFFECTIVE-SLOTS");
        static readonly Symbol QuoteEnsureClass = closSymbol ("ENSURE-CLASS");
        static readonly Symbol QuoteEnsureClassUsingClass = closSymbol ("EnsureClassUsingClass");
        static readonly Symbol QuoteEnsureGenericFunction = Package.CommonLisp.FindSymbol ("ENSURE-GENERIC-FUNCTION");
        static readonly Symbol QuoteEnsureGenericFunctionUsingClass = closSymbol ("ENSURE-GENERIC-FUNCTION-USING-CLASS");
        static readonly Symbol QuoteEqlSpecializer = closSymbol ("EQL-SPECIALIZER");
        static readonly Symbol QuoteEqlSpecializerObject = closSymbol ("EqlSpecializerObject");
        static readonly Symbol QuoteExtractLambdaList = closSymbol ("ExtractLambdaList");
        static readonly Symbol QuoteExtractSpecializerNames = closSymbol ("ExtractSpecializerNames");
        static readonly Symbol QuoteFinalizeInheritance = closSymbol ("FinalizeInheritance");
        static readonly Symbol QuoteFinalizedP = closSymbol ("FINALIZEDP");
        static readonly Symbol QuoteFindClass = closSymbol ("FindClass");
        static readonly Symbol QuoteFindMethod = closSymbol ("FindMethod");
        static readonly Symbol QuoteFindMethodCombination = closSymbol ("FindMethodCombination");
        static readonly Symbol QuoteForwardReferencedClass = closSymbol ("FORWARD-REFERENCED-CLASS");
        static readonly Symbol QuoteFuncallableStandardClass = closSymbol ("FUNCALLABLE-STANDARD-CLASS");
        static readonly Symbol QuoteFuncallableStandardObject = closSymbol ("FUNCALLABLE-STANDARD-OBJECT");
        static readonly Symbol QuoteFunction = closSymbol ("FUNCTION");
        static readonly Symbol QuoteFunctionKeywords = closSymbol ("FunctionKeywords");
        static readonly Symbol QuoteFunctionName = closSymbol ("FUNCTION-NAME");
        static readonly Symbol QuoteGenericFunction = closSymbol ("GENERIC-FUNCTION");
        static readonly Symbol QuoteGenericFunctionArgumentPrecedenceOrder = closSymbol ("GenericFunctionArgumentPrecedenceOrder");
        static readonly Symbol QuoteGenericFunctionApplicationCache = closSymbol ("GENERIC-FUNCTION-APPLICATION-CACHE");
        static readonly Symbol QuoteGenericFunctionClass = closSymbol ("GENERIC-FUNCTION-CLASS");
        static readonly Symbol QuoteGenericFunctionDeclarations = closSymbol ("GenericFunctionDeclarations");
        static readonly Symbol QuoteGenericFunctionLambdaList = closSymbol ("GENERIC-FUNCTION-LAMBDA-LIST");
        static readonly Symbol QuoteGenericFunctionMethodClass = closSymbol ("GENERIC-FUNCTION-METHOD-CLASS");
        static readonly Symbol QuoteGenericFunctionMethodCombination = closSymbol ("GenericFunctionMethodCombination");
        static readonly Symbol QuoteGenericFunctionMethods = closSymbol ("GENERIC-FUNCTION-METHODS");
        static readonly Symbol QuoteGenericFunctionName = closSymbol ("GENERIC-FUNCTION-NAME");
        static readonly Symbol QuoteGenericFunctionSingletonsList = closSymbol ("GENERIC-FUNCTION-SINGELTONS-LIST");
        static readonly Symbol QuoteGettersAndSetters = closSymbol ("GETTERS-AND-SETTERS");
        static readonly Symbol QuoteInitargs = closSymbol ("INITARGS");
        static readonly Symbol QuoteInitform = closSymbol ("INITFORM");
        static readonly Symbol QuoteInitfunction = closSymbol ("INITFUNCTION");
        static readonly Symbol QuoteInitializeInstance = closSymbol ("INITIALIZE-INSTANCE");
        static readonly Symbol QuoteInitializer = closSymbol ("INITIALIZER");
        static readonly Symbol QuoteInitializers = closSymbol ("INITIALIZERS");
        static readonly Symbol QuoteInstance = closSymbol ("INSTANCE");
        static readonly Symbol QuoteInternEqlSpecializer = closSymbol ("InternEqlSpecializer");
        static readonly Symbol QuoteIsClassFinalized = closSymbol ("IsClassFinalized");
        static readonly Symbol QuoteIsSlotBoundUsingClass = closSymbol ("IsSlotBoundUsingClass");
        static readonly Symbol QuoteLambdaList = closSymbol ("LAMBDA-LIST");
        static readonly Symbol QuoteLeft = closSymbol ("LEFT");
        static readonly Symbol QuoteList = Package.CommonLisp.FindSymbol ("LIST");
        static readonly Symbol QuoteLocation = closSymbol ("LOCATION");
        static readonly Symbol QuoteMakeInstance = Package.CommonLisp.FindSymbol ("MAKE-INSTANCE");
        static readonly Symbol QuoteMakeInstancesObsolete = closSymbol ("MakeInstancesObsolete");
        static readonly Symbol QuoteMakeLoadForm = closSymbol ("MakeLoadForm");
        static readonly Symbol QuoteMakeMethodLambda = closSymbol ("MakeMethodLambda");
        static readonly Symbol QuoteMapDependents = closSymbol ("MapDependents");
        static readonly Symbol QuoteMetaobject = closSymbol ("METAOBJECT");
        static readonly Symbol QuoteMethod = closSymbol ("METHOD");
        static readonly Symbol QuoteMethodAllocateInstance = closSymbol ("METHOD:allocate-instance");
        static readonly Symbol QuoteMethodClass = closSymbol ("METHOD-CLASS");
        static readonly Symbol QuoteMethodClassPrecedenceList = closSymbol ("METHOD:class-precedence-list");
        static readonly Symbol QuoteMethodClassSlots = closSymbol ("METHOD:class-slots");
        static readonly Symbol QuoteMethodCombination = closSymbol ("METHOD-COMBINATION");
        static readonly Symbol QuoteMethodComputeApplicableMethods = closSymbol ("METHOD:compute-applicable-methods");
        static readonly Symbol QuoteMethodComputeDiscriminatingFunction = closSymbol ("METHOD:compute-discriminating-function");
        static readonly Symbol QuoteMethodComputeEffectiveMethod = closSymbol ("METHOD:compute-effective-method");
        static readonly Symbol QuoteMethodComputeMethodMoreSpecific = closSymbol ("METHOD:compute-method-more-specific");
        static readonly Symbol QuoteMethodDefaultInitargs = closSymbol ("METHOD:default-initargs");
        static readonly Symbol QuoteMethodEnsureGenericFunction = closSymbol ("METHOD:ensure-generic-function");
        static readonly Symbol QuoteMethodFunction = closSymbol ("MethodFunction");
        static readonly Symbol QuoteMethodGenericFunction = closSymbol ("MethodGenericFunction");
        static readonly Symbol QuoteMethodInitializeInstance = closSymbol ("METHOD:initialize-instance");
        static readonly Symbol QuoteMethodLambdaList = closSymbol ("MethodLambdaList");
        static readonly Symbol QuoteMethodMethodQualifier = closSymbol ("METHOD:method-qualifier");
        static readonly Symbol QuoteMethodMethodSpecializers = closSymbol ("METHOD:method-specializers");
        static readonly Symbol QuoteMethodQualifier = closSymbol ("METHOD-QUALIFIER");
        static readonly Symbol QuoteMethodSpecializers = closSymbol ("METHOD-SPECIALIZERS");
        static readonly Symbol QuoteMethods = closSymbol ("METHODS");
        static readonly Symbol QuoteName = closSymbol ("NAME");
        static readonly Symbol QuoteNewMakeInstance = closSymbol ("NEW-MAKE-INSTANCE");
        static readonly Symbol QuoteNewMakeInstanceMethod = closSymbol ("NEW-MAKE-INSTANCE-METHOD");
        static readonly Symbol QuoteNewValue = closSymbol ("NEW-VALUE");
        static readonly Symbol QuoteNoApplicableMethod = closSymbol ("NoApplicableMethod");
        static readonly Symbol QuoteNoNextMethod = closSymbol ("NoNextMethod");
        static readonly Symbol QuotePrecedenceList = closSymbol ("PRECEDENCE-LIST");
        static readonly Symbol QuoteProcedure = closSymbol ("PROCEDURE");
        static readonly Symbol QuotePrototype = closSymbol ("PROTOTYPE");
        static readonly Symbol QuoteQualifier = closSymbol ("QUALIFIER");
        static readonly Symbol QuoteReaderMethodClass = closSymbol ("ReaderMethodClass");
        static readonly Symbol QuoteReaders = closSymbol ("READERS");
        static readonly Symbol QuoteReinitializeInstance = closSymbol ("ReinitializeInstance");
        static readonly Symbol QuoteRemoveDependents = closSymbol ("RemoveDependents");
        static readonly Symbol QuoteRemoveDirectMethod = closSymbol ("RemoveDirectMethod");
        static readonly Symbol QuoteRemoveDirectSubclass = closSymbol ("RemoveDirectSubclass");
        static readonly Symbol QuoteRemoveMethod = closSymbol ("RemoveMethod");
        static readonly Symbol QuoteRight = closSymbol ("RIGHT");
        static readonly Symbol QuoteRuntimeType = closSymbol ("RUNTIME-TYPE");
        static readonly Symbol QuoteSetClassDirectSlots = closSymbol ("SET-CLASS-DIRECT-SLOTS");
        static readonly Symbol QuoteSetClassDirectSuperclasses = closSymbol ("SET-CLASS-DIRECT-SUPERCLASSES");
        static readonly Symbol QuoteSetClassName = closSymbol ("SetClassName");
        static readonly Symbol QuoteSetClassSlots = closSymbol ("SET-CLASS-SLOTS");
        static readonly Symbol QuoteSetGenericFunctionApplicationCache = closSymbol ("SET-GENERIC-FUNCTION-APPLICATION-CACHE");
        static readonly Symbol QuoteSetGenericFunctionName = closSymbol ("SetGenericFunctionName");
        static readonly Symbol QuoteSetSlotValue = closSymbol ("SetSlotValue");
        static readonly Symbol QuoteSetSlotValueUsingClass = closSymbol ("SET-SLOT-VALUE-USING-CLASS");
        static readonly Symbol QuoteSharedInitialize = closSymbol ("SHARED-INITIALIZE");
        static readonly Symbol QuoteSingletonsList = closSymbol ("SINGLETONS-LIST");
        static readonly Symbol QuoteSlotDefinition = closSymbol ("SLOT-DEFINITION");
        static readonly Symbol QuoteSlotDefinitionAllocation = closSymbol ("SlotDefinitionAllocation");
        static readonly Symbol QuoteSlotDefinitionInitargs = closSymbol ("SlotDefinitionInitargs");
        static readonly Symbol QuoteSlotDefinitionInitform = closSymbol ("SlotDefinitionInitform");
        static readonly Symbol QuoteSlotDefinitionInitfunction = closSymbol ("SlotDefinitionInitfunction");
        static readonly Symbol QuoteSlotDefinitionInitializer = closSymbol ("SLOT-DEFINITION-INITIALIZER");
        static readonly Symbol QuoteSlotDefinitionLocation = closSymbol ("SlotDefinitionLocation");
        static readonly Symbol QuoteSlotDefinitionName = closSymbol ("SlotDefinitionName");
        static readonly Symbol QuoteSlotDefinitionReaders = closSymbol ("SlotDefinitionReaders");
        static readonly Symbol QuoteSlotDefinitionType = closSymbol ("SlotDefinitionType");
        static readonly Symbol QuoteSlotDefinitionWriters = closSymbol ("SlotDefinitionWriters");
        static readonly Symbol QuoteSlotInitializers = closSymbol ("SLOT-INITIALIZERS");
        static readonly Symbol QuoteSlotMakunboundUsingClass = closSymbol ("SlotMakunboundUsingClass");
        static readonly Symbol QuoteSlotMissing = closSymbol ("SlotMissing");
        static readonly Symbol QuoteSlotNames = closSymbol ("SLOT-NAMES");
        static readonly Symbol QuoteSlotUnbound = closSymbol ("SlotUnbound");
        static readonly Symbol QuoteSlotValue = closSymbol ("SLOT-VALUE");
        static readonly Symbol QuoteSlotValueUsingClass = closSymbol ("SLOT-VALUE-USING-CLASS");
        static readonly Symbol QuoteSlot = closSymbol ("SLOT");
        static readonly Symbol QuoteSlots = closSymbol ("SLOTS");
        static readonly Symbol QuoteSpecializer = closSymbol ("SPECIALIZER");
        static readonly Symbol QuoteSpecializerDirectGenericFunctions = closSymbol ("SpecializerDirectGenericFunctions");
        static readonly Symbol QuoteSpecializerDirectMethods = closSymbol ("SpecializerDirectMethods");
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
        static readonly Symbol QuoteSuperclass = closSymbol ("SUPERCLASS");
        static readonly Symbol QuoteSuppliedInitargs = closSymbol ("SUPPLIED-INITARGS");
        static readonly Symbol QuoteSymbol = closSymbol ("SYMBOL");
        static readonly Symbol QuoteTop = closSymbol ("TOP");
        static readonly Symbol QuoteType = closSymbol ("TYPE");
        static readonly Symbol QuoteUpdateDependent = closSymbol ("UpdateDependent");
        static readonly Symbol QuoteUpdateInstanceForDifferentClass = closSymbol ("UpdateInstanceForDifferentClass");
        static readonly Symbol QuoteUpdateInstanceForRedefinedClass = closSymbol ("UpdateInstanceForRedefinedClass");
        static readonly Symbol QuoteValidateSuperclass = closSymbol ("VALIDATE-SUPERCLASS");
        static readonly Symbol QuoteWriterMethodClass = closSymbol ("WriterMethodClass");
        static readonly Symbol QuoteWriters = closSymbol ("WRITERS");

        #endregion Symbols

        #region Allocation
        // CLOS is highly circular, so I'm defining everything from the get-go as
        // empty objects.  I'll fill them in later.

        // The MOP classes.

        static readonly StandardObject builtInClass = ManifestInstance.CreateInstance ();
        static readonly StandardObject builtInEffectiveSlotDefinition = ManifestInstance.CreateInstance ();
        static readonly StandardObject builtInSlotDefinition = ManifestInstance.CreateInstance ();
        static readonly StandardObject closClass = ManifestInstance.CreateInstance ();
        static readonly StandardObject directSlotDefinition = ManifestInstance.CreateInstance ();
        static readonly StandardObject effectiveSlotDefinition = ManifestInstance.CreateInstance ();
        static readonly StandardObject eqlSpecializer = ManifestInstance.CreateInstance ();
        static readonly StandardObject forwardReferencedClass = ManifestInstance.CreateInstance ();
        static readonly StandardObject funcallableStandardClass = ManifestInstance.CreateInstance ();
        static readonly StandardObject funcallableStandardObject = ManifestInstance.CreateInstance ();
        static readonly StandardObject function = ManifestInstance.CreateInstance ();
        static readonly StandardObject genericFunction = ManifestInstance.CreateInstance ();
        static readonly StandardObject metaobject = ManifestInstance.CreateInstance ();
        static readonly StandardObject method = ManifestInstance.CreateInstance ();
        static readonly StandardObject methodCombinationClass = ManifestInstance.CreateInstance ();
        static readonly StandardObject runtimeType = ManifestInstance.CreateInstance();
        static readonly StandardObject slotDefinition = ManifestInstance.CreateInstance ();
        static readonly StandardObject specializer = ManifestInstance.CreateInstance ();
        static readonly StandardObject standardAccessorMethodClass = ManifestInstance.CreateInstance ();
        static readonly StandardObject standardClass = ManifestInstance.CreateInstance ();
        static readonly StandardObject standardDirectSlotDefinition = ManifestInstance.CreateInstance ();
        static readonly StandardObject standardEffectiveSlotDefinition = ManifestInstance.CreateInstance ();
        static readonly StandardObject standardGenericFunction = ManifestInstance.CreateInstance ();
        static readonly StandardObject standardMethod = ManifestInstance.CreateInstance ();
        static readonly StandardObject standardObjectClass = ManifestInstance.CreateInstance ();
        static readonly StandardObject standardReaderMethod = ManifestInstance.CreateInstance ();
        static readonly StandardObject standardSlotDefinition = ManifestInstance.CreateInstance ();
        static readonly StandardObject standardWriterMethod = ManifestInstance.CreateInstance ();
        static readonly StandardObject top = ManifestInstance.CreateInstance ();

        // The MOP generic functions.

        static readonly StandardObject accessorMethodSlotDefinition = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject addDependent = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject addDirectMethod = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject addDirectSubclass = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject addMethod = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject allocateInstance = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject changeClass = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject classDefaultInitargs = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject classDirectDefaultInitargs = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject classDirectSlots = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject classDirectSubclasses = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject classDirectSuperclasses = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject className = ManifestInstance.CreateFuncallableInstance ();
        delegate ConsList<StandardObject> ClassPrecedenceListMethodSignature (NextMethodFunction callNextMethod, StandardObject closClass);
        static readonly StandardObject classPrecedenceList = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject classPrototype = ManifestInstance.CreateFuncallableInstance ();
        delegate ICollection<StandardObject> ClassSlotsMethodSignature (NextMethodFunction callNextMethod, StandardObject closClass);
        static readonly StandardObject classSlots = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject computeApplicableMethods = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject computeApplicableMethodsUsingClasses = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject computeClassPrecedenceList = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject computeDefaultInitargs = ManifestInstance.CreateFuncallableInstance ();

        delegate FuncallHandler ComputeDiscriminatingFunctionMethodSignature (NextMethodFunction callNextMethod, StandardObject genericFunction);
        static readonly StandardObject computeDiscriminatingFunction = ManifestInstance.CreateFuncallableInstance ();

        static readonly StandardObject computeEffectiveMethod = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject computeEffectiveSlotDefinition = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject computeSlots = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject computeMethodMoreSpecific = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject defaultInitargs = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject directSlotDefinitionClass = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject effectiveSlotDefinitionClass = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject ensureClass = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject ensureClassUsingClass = ManifestInstance.CreateFuncallableInstance ();

        delegate StandardObject EnsureGenericFunctionMethodSignature (NextMethodFunction callNextMethod, object functionName, params object [] arguments); 
        static readonly StandardObject ensureGenericFunction = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject ensureGenericFunctionUsingClass = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject eqlSpecializerObject = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject extractLambdaList = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject extractSpecializerNames = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject finalizeInheritance = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject findClass = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject findMethod = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject findMethodCombination = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject functionKeywords = ManifestInstance.CreateFuncallableInstance ();
        static public readonly StandardObject genericFunctionApplicationCache = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject genericFunctionArgumentPrecedenceOrder = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject genericFunctionDeclarations = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject genericFunctionLambdaList = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject genericFunctionMethodClass = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject genericFunctionMethodCombination = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject genericFunctionMethods = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject genericFunctionName = ManifestInstance.CreateFuncallableInstance ();
        static public readonly StandardObject genericFunctionSingletonsList = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject initializeInstance = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject internEqlSpecializer = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject isClassFinalized = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject makeInstance = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject makeInstancesObsolete = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject makeLoadForm = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject makeMethodLambda = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject mapDependents = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject methodCombination = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject methodFunction = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject methodGenericFunction = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject methodLambdaList = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject methodSpecializers = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject methodQualifier = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject noApplicableMethod = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject noNextMethod = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject readerMethodClass = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject reinitializeInstance = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject removeDependents = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject removeDirectMethod = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject removeDirectSubclass = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject removeMethod = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject sharedInitialize = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject setClassDirectSlots = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject setClassDirectSuperclasses = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject setClassName = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject setClassSlots = ManifestInstance.CreateFuncallableInstance ();
        static internal readonly StandardObject setGenericFunctionApplicationCache = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject setGenericFunctionName = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject setSlotValueUsingClass = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject isSlotBoundUsingClass = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject slotDefinitionAllocation = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject slotDefinitionInitargs = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject slotDefinitionInitform = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject slotDefinitionInitfunction = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject slotDefinitionLocation = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject slotDefinitionName = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject slotDefinitionType = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject slotDefinitionReaders = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject slotDefinitionWriters = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject slotMakunboundUsingClass = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject slotMissing = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject slotUnbound = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject slotValueUsingClass = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject specializerDirectGenericFunctions = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject specializerDirectMethods = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject standardAccessorMethod = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject updateDependent = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject updateInstanceForDifferentClass = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject updateInstanceForRedefinedClass = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject validateSuperclass = ManifestInstance.CreateFuncallableInstance ();
        static readonly StandardObject writerMethodClass = ManifestInstance.CreateFuncallableInstance ();

        static readonly StandardObject standardClassEqlSpecializer = ManifestInstance.CreateInstance ();

        static readonly StandardObject method0ForClassPrecedenceList = ManifestInstance.CreateInstance ();
        static readonly StandardObject method1ForClassPrecedenceList = ManifestInstance.CreateInstance ();
        static readonly StandardObject method0ForClassSlots = ManifestInstance.CreateInstance ();
        static readonly StandardObject method1ForClassSlots = ManifestInstance.CreateInstance ();
        static readonly StandardObject method2ForClassSlots = ManifestInstance.CreateInstance ();
        static readonly StandardObject method3ForClassSlots = ManifestInstance.CreateInstance ();
        static readonly StandardObject methodForComputeDiscriminatingFunction = ManifestInstance.CreateInstance ();
        static readonly StandardObject method0ForEnsureGenericFunction = ManifestInstance.CreateInstance();
        static readonly StandardObject method1ForEnsureGenericFunction = ManifestInstance.CreateInstance();

        #endregion Allocation

        static internal readonly List<StandardObject> GenericInvocationGenerics =
            new List<StandardObject> {computeDiscriminatingFunction,
                                      computeApplicableMethods,
                                      computeEffectiveMethod,
                                      computeMethodMoreSpecific};
        static internal Cons GenericApplicationCacheTag = new Cons (null, null);

        static readonly Dictionary<Symbol, StandardObject> allNamedClasses = new Dictionary<Symbol, StandardObject> ();
        static readonly Dictionary<Symbol, StandardObject> allNamedGenericFunctions = new Dictionary<Symbol, StandardObject> ();

        static readonly Dictionary<Type, StandardObject> builtInClassMapping = new Dictionary<Type, StandardObject> ();

        static StandardObject FindDotnetClass (Type dotnetType)
        {
            StandardObject closClass = null;            
            if (builtInClassMapping.TryGetValue (dotnetType, out closClass))
                return closClass;
            StandardObject metaclass = FindDotnetClass (dotnetType.GetType());
            StandardObject superclass = FindDotnetClass (dotnetType.BaseType);
            StandardObject builtIn = ManifestInstance.CreateInstance();
            bootstrapInitializeBuiltInClass (metaclass, builtIn,
                                             "",
                                             closSymbol (dotnetType.Name),
                                             dotnetType.FullName,
                                             null,
                                             null, 
                                             CL.List<StandardObject>(superclass),
                                             new ConsList<StandardObject>(builtIn, (ConsList<StandardObject>) InternalClassPrecedenceList (superclass)),
                                             dotnetType);
           return builtIn;
        }

        public static StandardObject ClassOf (object instance)
        {
            StandardObject soInstance = instance as StandardObject;
            return (soInstance == null)
                ? FindDotnetClass (instance.GetType())
                : soInstance.Class();
        }

        static bool IsInstanceOf (object item, StandardObject closClass)
        {
            if (closClass.Equals (Top))
                return true;
            StandardObject cx = ClassOf (item);
            if (cx.Equals (closClass))
                return true;
            else {
                return CL.Memq<StandardObject> (closClass, CLOS.InternalClassPrecedenceList (cx)) != null;
            }
        }

        static bool InstancesOf (Cons items, Cons classes)
        {
            return (items == null)
                || (classes == null)
                || (IsInstanceOf (items.Car, (StandardObject) classes.Car) 
                    && InstancesOf ((Cons) items.Cdr, (Cons) classes.Cdr));
        }

        delegate bool MethodCompare2 (StandardObject left, StandardObject right);
        delegate bool MethodCompare3 (StandardObject left, StandardObject right, Cons arglist);

        static Cons MergeMethods (Cons left, Cons right, MethodCompare2 comp)
        {
            Cons result = null;
            while (true) {
                if (left == null) {
                    if (right == null)
                        break;
                    else {
                        result = CL.Cons (CL.Car (right), result);
                        right = (Cons) CL.Cdr (right);
                    }
                }
                else {
                    if (right == null) {
                        result = CL.Cons (CL.Car (left), result);
                        left = (Cons) CL.Cdr (left);
                    }
                    else if (comp ((StandardObject) CL.Car (left),
                                   (StandardObject) CL.Car (right))) {
                        result = CL.Cons (CL.Car (left), result);
                        left = (Cons) CL.Cdr (left);
                    }
                    else {
                        result = CL.Cons (CL.Car (right), result);
                        right = (Cons) CL.Cdr (right);
                    }
                }
            }

            return CL.Reverse (result);
        }

        static Cons SortMethods (Cons methodList, MethodCompare2 comp)
        {
            if (methodList == null)
                return null;
            if (methodList.Cdr == null)
                return methodList;
            Cons left = null;
            Cons right = null;
            while (methodList != null) {
                Cons temp = CL.Cons (CL.Car (methodList), left);
                left = right;
                right = temp;
                methodList = (Cons) CL.Cdr (methodList);
            }
            return MergeMethods (SortMethods (left, comp), SortMethods (right, comp), comp);
        }

        static MethodCompare2 MakeSortPredicate (MethodCompare3 internalCompare, Cons arglist)
        {
            return new MethodCompare2 ((left, right) => internalCompare (left, right, arglist));
        }

        delegate bool MethodPredicate (StandardObject method);

        static Cons FilterMethods (MethodPredicate mp, Cons methodList)
        {
            if (methodList == null)
                return null;
            StandardObject thisMethod = (StandardObject) methodList.Car;
            if (mp (thisMethod))
                return new Cons (thisMethod, FilterMethods (mp, (Cons) methodList.Cdr));
            else
                return FilterMethods (mp, (Cons) methodList.Cdr);
        }

        static Cons methodImplForComputeApplicableMethods (NextMethodFunction callNextMethod,
                                                           StandardObject genericFunction,
                                                           params object [] arguments)
        {
            Cons arglist = Cons.SubvectorToList (arguments, 0, arguments.Length);
            Cons methods = (Cons) InternalGenericFunctionMethods (genericFunction);
            Cons filteredMethods = FilterMethods ((m) => InstancesOf (arglist, (Cons) InternalMethodSpecializers (m)), methods);
            Cons sortedMethods = SortMethods (filteredMethods,
                                              MakeSortPredicate ((MethodCompare3) ComputeMethodMoreSpecific (genericFunction), arglist));
            return sortedMethods;
        }

        static NextMethodFunction oneAroundStep (Cons methods, Cons args)
        {
            throw new NotImplementedException ();
        }

        static NextMethodFunction methodImplForComputeEffectiveMethod (NextMethodFunction callNextMethod, StandardObject generic, Cons methodList)
        {
            Cons primaryMethods = null;
            Cons aroundMethods = null;
            Cons beforeMethods = null;
            Cons afterMethods = null;

            MethodStepper oneStep = new MethodStepper (generic);

            while (methodList != null) {
                StandardObject method = (StandardObject) methodList.Car;
                methodList = (Cons) methodList.Cdr;
                Symbol q = (Symbol) InternalMethodQualifier (method);
                if (q.Equals (KW.Primary)) {
                    primaryMethods = new Cons (new Cons (method, MethodStepWrapper.Create ((Delegate) InternalMethodFunction (method))), primaryMethods);
                }
                else
                    throw new NotImplementedException ();
            }
            primaryMethods = (Cons) CL.Reverse (primaryMethods);

            if (primaryMethods == null)
                throw new NotImplementedException ("no applicable method");
            else if (beforeMethods == null
                && afterMethods == null
                && aroundMethods == null)
                return oneStep.Step (primaryMethods, null);
            else
                return oneAroundStep (aroundMethods, null);
        }

        static bool IsMoreSpecific (StandardObject left, StandardObject right, object arg)
        {
            ConsList<StandardObject> cc1 = CL.Memq<StandardObject> (left, InternalClassPrecedenceList (ClassOf (arg)));
            return (cc1 != null)
                && CL.Memq (right, cc1.Cdr) != null;
        }

        static bool computeMethodMoreSpecificLoop (StandardObject generic, StandardObject left, StandardObject right, Cons arglist)
        {
            Cons speclsLeft = (Cons) InternalMethodSpecializers (left);
            Cons speclsRight = (Cons) InternalMethodSpecializers (right);
            while (true) {
                if ((speclsLeft == null) && (speclsRight == null)) {
                    if (CL.Eq (InternalMethodQualifier (left),
                               InternalMethodQualifier (right)))
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
                    StandardObject c1 = (StandardObject) CL.Car (speclsLeft);
                    StandardObject c2 = (StandardObject) CL.Car (speclsRight);
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

        static MethodCompare3 methodImplForComputeMethodMoreSpecific (NextMethodFunction callNextMethod, StandardObject generic)
        {
            return new MethodCompare3 ((left, right, arglist) => computeMethodMoreSpecificLoop (generic, left, right, arglist));
        }

        static StandardObject bootstrapInitializeBuiltInClass (StandardObject metaclass,
                                                               StandardObject closClass,
                                                               string documentation,
                                                               Symbol name,
                                                               string studlyName,
                                                               ConsList<StandardObject> directMethods,
                                                               ConsList<StandardObject> directSubclasses,
                                                               ConsList<StandardObject> directSuperclasses,
                                                               ConsList<StandardObject> precedenceList,
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
            builtInClassMapping.Add (dotnetType, closClass);
            allNamedClasses.Add (name, closClass);
            return closClass;
        }

        static StandardObject bootstrapInitializeEqlSpecializer (StandardObject specializer,
                                                                 StandardObject closClass)
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

        static StandardObject bootstrapInitializeStandardClass (StandardObject closClass,
                                                                string documentation,
                                                                Symbol name,
                                                                string studlyName,
                                                                ConsList<StandardObject> directMethods,
                                                                ConsList<StandardObject> directSubclasses,
                                                                ConsList<StandardObject> directSuperclasses,
                                                                ConsList<StandardObject> precedenceList,
                                                                Cons defaultInitargs,
                                                                Cons directDefaultInitargs,
                                                                ConsList<StandardObject> directSlots,
                                                                StandardObject prototype,
                                                                ICollection<StandardObject> slots)
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
                    slots
                    });
            allNamedClasses.Add (name, closClass);
            return closClass;
        }

        static StandardObject bootstrapInitializeFuncallableStandardClass (StandardObject closClass,
                                                                string documentation,
                                                                Symbol name,
                                                                string studlyName,
                                                                ConsList<StandardObject> directMethods,
                                                                ConsList<StandardObject> directSubclasses,
                                                                ConsList<StandardObject> directSuperclasses,
                                                                ConsList<StandardObject> precedenceList,
                                                                Cons defaultInitargs,
                                                                Cons directDefaultInitargs,
                                                                ConsList<StandardObject> directSlots,
                                                                StandardObject prototype,
                                                                ICollection<StandardObject> slots)
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
                    slots
                    });
            allNamedClasses.Add (name, closClass);
            return closClass;
        }

        static Cons bootstrapGenericFunctionApplicationCache (StandardObject self, object [] arguments)
        {
            return (Cons) CLOS.SlotValue ((StandardObject) arguments [0], QuoteApplicationCache);
        }

        static Cons bootstrapSetGenericFunctionApplicationCache (StandardObject self, object [] arguments)
        {
            return (Cons) CLOS.SetSlotValue (arguments [1], (StandardObject) arguments[0], QuoteApplicationCache);
        }

        static Cons bootstrapGenericFunctionLambdaList (StandardObject self, object [] arguments)
        {
            return (Cons) CLOS.SlotValue ((StandardObject) arguments [0], QuoteLambdaList);
        }

        static Cons bootstrapGenericFunctionMethods (StandardObject self, object [] arguments)
        {
            return (Cons) CLOS.SlotValue ((StandardObject) arguments [0], QuoteMethods);
        }

        static ConsList<ICollection<object>> bootstrapGenericFunctionSingletonsList (StandardObject self, object [] arguments)
        {
            return (ConsList<ICollection<object>>) CLOS.SlotValue ((StandardObject) arguments [0], QuoteSingletonsList);
        }

        static Delegate bootstrapMethodFunction (StandardObject self, object [] arguments)
        {
            return (Delegate) CLOS.SlotValue ((StandardObject) arguments [0], QuoteFunction);
        }

        static Symbol bootstrapMethodQualifier (StandardObject self, object [] arguments)
        {
            return (Symbol) CLOS.SlotValue ((StandardObject) arguments [0], QuoteQualifier);
        }

        static Cons bootstrapMethodSpecializers (StandardObject self, object [] arguments)
        {
            return (Cons) CLOS.SlotValue ((StandardObject) arguments [0], QuoteSpecializers);
        }

        static StandardObject bootstrapInitializeStandardMethod (StandardObject method, 
                                                                 string documentation,
                                                                 Symbol name,
                                                                 string studlyName,
                                                                 Delegate function,
                                                                 Symbol qualifier,
                                                                 Cons specializers)
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

        static StandardObject bootstrapInitializeStandardGenericFunction (StandardObject genericFunction,
                                                                          string documentation,
                                                                          Symbol name,
                                                                          string studlyName,
                                                                          Cons lambdaList,
                                                                          Cons methods,
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
            setFuncallableInstanceFunction (genericFunction, computeDiscriminatingFunctionTrampoline);
            allNamedGenericFunctions.Add (name, genericFunction);
            return genericFunction;
        }

        static object bootstrapComputeApplicableMethods (StandardObject self, object [] arguments)
        {
            object [] newArguments = new object [arguments.Length - 1];
            Array.Copy (arguments, 1, newArguments, 0, newArguments.Length);
            return methodImplForComputeApplicableMethods (null, (StandardObject) arguments[0], newArguments);
        }

        static object bootstrapComputeDiscriminatingFunction (StandardObject self, object [] arguments)
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

        static object computeDiscriminatingFunctionTrampoline (StandardObject gf, object [] arguments)
        {
            FuncallHandler discriminatingFunction = (FuncallHandler) ComputeDiscriminatingFunction (gf);
            setFuncallableInstanceFunction (gf, discriminatingFunction);
            return discriminatingFunction.Invoke (gf, arguments);
        }

        static object bootstrapComputeMethodMoreSpecific (StandardObject self, object [] arguments)
        {
            return methodImplForComputeMethodMoreSpecific (null, (StandardObject) arguments[0]);
        }

        static object bootstrapComputeEffectiveMethod (StandardObject self, object [] arguments)
        {
            return methodImplForComputeEffectiveMethod (null, (StandardObject) arguments[0], (Cons) arguments[1]);
        }

        static StandardObject bootstrapCreateBuiltInEffectiveSlot (FieldInfo dotnetField)
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

        static StandardObject bootstrapCreateBuiltInEffectiveSlot (PropertyInfo dotnetProperty)
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

        static StandardObject bootstrapCreateEffectiveSlot (string documentation,
                                                            Symbol name,
                                                            string studlyName,
                                                            Keyword allocation,
                                                            ConsList<Symbol> initargs,
                                                            object type,
                                                            object initializer,
                                                            int location)
        {
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

        static StandardObject bootstrapCreateDocumentationSlot ()
        {
            return bootstrapCreateEffectiveSlot ("The documentation slot.",
                                                 QuoteDocumentation,
                                                 "Documentation",
                                                 KW.Instance,
                                                 CL.List<Symbol> (KW.Documentation),
                                                 null,
                                                 null,
                                                 0);
        }

        static StandardObject bootstrapCreateNameSlot ()
        {
            return bootstrapCreateEffectiveSlot ("The name slot.",
                                                 QuoteName,
                                                 "Name",
                                                 KW.Instance,
                                                 CL.List<Symbol> (KW.Name),
                                                 null,
                                                 null,
                                                 1);
        }

        static StandardObject bootstrapCreateStudlyNameSlot ()
        {
            return bootstrapCreateEffectiveSlot ("The StudlyName slot.",
                                                 QuoteStudlyName,
                                                 "StudlyName",
                                                 KW.Instance,
                                                 CL.List<Symbol> (KW.StudlyName),
                                                 null,
                                                 null,
                                                 2);
        }

        static Symbol bootstrapSlotDefinitionName (StandardObject self, object [] arguments)
        {
            StandardObject slotDefinition = (StandardObject) arguments [0];
            return (slotDefinition.Class() == standardEffectiveSlotDefinition)
                   ? (Symbol) StandardInstanceAccess (slotDefinition, 1)
                   : (Symbol) SlotValue (slotDefinition, QuoteName);
        }

        static object bootstrapSlotDefinitionLocation (StandardObject self, object [] arguments)
        {
            StandardObject slotDefinition = (StandardObject) arguments [0];
            return
                (slotDefinition.Class() == standardEffectiveSlotDefinition)
                ? StandardInstanceAccess (slotDefinition, 7)
                : SlotValue (slotDefinition, QuoteLocation);
        }

        static object bootstrapSlotValueUsingClass (StandardObject self, object [] arguments)
        {
            StandardObject instance = (StandardObject) arguments [1];
            StandardObject effectiveSlot = (StandardObject) arguments [2];
            return StandardInstanceAccess (instance, (int) SlotDefinitionLocation (effectiveSlot));
        }

        static object bootstrapSetSlotValueUsingClass (StandardObject self, object [] arguments)
        {
            object newValue = arguments[0];
            StandardObject instance = (StandardObject) arguments [2];
            StandardObject effectiveSlot = (StandardObject) arguments [3];
            return SetStandardInstanceAccess (instance, (int) SlotDefinitionLocation (effectiveSlot), newValue);
        }

        static ICollection<StandardObject> theEffectiveSlotsOfBuiltInClass = 
            CL.List<StandardObject> (
                bootstrapCreateDocumentationSlot (),
                bootstrapCreateNameSlot (),
                bootstrapCreateStudlyNameSlot (),
                bootstrapCreateEffectiveSlot ("The direct methods slot",
                                             QuoteDirectMethods,
                                             "DirectMethods",
                                             KW.Instance,
                                             null,
                                             null,
                                             null,
                                             3
                                             ),
                bootstrapCreateEffectiveSlot ("The direct subclasses slot",
                                             QuoteDirectSubclasses,
                                             "DirectSubclasses",
                                             KW.Instance,
                                             null,
                                             null,
                                             null,
                                             4
                                             ),
                bootstrapCreateEffectiveSlot ("The direct superclasses slot",
                                             QuoteDirectMethods,
                                             "DirectSuperclasses",
                                             KW.Instance,
                                             null,
                                             null,
                                             null,
                                             5
                                             ),
                bootstrapCreateEffectiveSlot ("The precedence list slot",
                                             QuotePrecedenceList,
                                             "PrecedenceList",
                                             KW.Instance,
                                             null,
                                             null,
                                             null,
                                             6
                                             ),
                bootstrapCreateEffectiveSlot ("The dotnet type slot",
                                             QuoteDotnetType,
                                             "DotnetType",
                                             KW.Instance,
                                             null,
                                             null,
                                             null,
                                             7
                                             ));


        static ICollection<StandardObject> theEffectiveSlotsOfStandardClass =
            CL.List<StandardObject> (
                bootstrapCreateDocumentationSlot (),
                bootstrapCreateNameSlot (),
                bootstrapCreateStudlyNameSlot (),
                bootstrapCreateEffectiveSlot ("The direct methods slot",
                                             QuoteDirectMethods,
                                             "DirectMethods",
                                             KW.Instance,
                                             null,
                                             null,
                                             null,
                                             3
                                             ),
                bootstrapCreateEffectiveSlot ("The direct subclasses slot",
                                             QuoteDirectSubclasses,
                                             "DirectSubclasses",
                                             KW.Instance,
                                             null,
                                             null,
                                             null,
                                             4
                                             ),
                bootstrapCreateEffectiveSlot ("The direct superclasses slot",
                                             QuoteDirectMethods,
                                             "DirectSuperclasses",
                                             KW.Instance,
                                             null,
                                             null,
                                             null,
                                             5
                                             ),
                bootstrapCreateEffectiveSlot ("The precedence list slot",
                                             QuotePrecedenceList,
                                             "PrecedenceList",
                                             KW.Instance,
                                             null,
                                             null,
                                             null,
                                             6
                                             ),
                bootstrapCreateEffectiveSlot ("The default initargs slot",
                                             QuoteDefaultInitargs,
                                             "DefaultInitargs",
                                             KW.Instance,
                                             null,
                                             null,
                                             null,
                                             7
                                             ),
                bootstrapCreateEffectiveSlot ("The direct default initargs slot",
                                             QuoteDirectDefaultInitargs,
                                             "DirectDefaultInitargs",
                                             KW.Instance,
                                             null,
                                             null,
                                             null,
                                             8
                                             ),

                bootstrapCreateEffectiveSlot ("The direct slots slot",
                                             QuoteDirectSlots,
                                             "DirectSlots",
                                             KW.Instance,
                                             null,
                                             null,
                                             null,
                                             9
                                             ),

                bootstrapCreateEffectiveSlot ("The prototype slot",
                                             QuotePrototype,
                                             "Prototype",
                                             KW.Instance,
                                             null,
                                             null,
                                             null,
                                             10
                                             ),

                bootstrapCreateEffectiveSlot ("The slots slot",
                                             QuoteSlots,
                                             "Slots",
                                             KW.Instance,
                                             null,
                                             null,
                                             null,
                                             11
                                             )
                );          // effectiveSlots

        static ICollection<StandardObject> theEffectiveSlotsOfStandardGenericFunction =
                CL.List<StandardObject> (
                bootstrapCreateDocumentationSlot (),
                bootstrapCreateNameSlot (),
                bootstrapCreateStudlyNameSlot (),
                bootstrapCreateEffectiveSlot ("The application cache slot",
                                             QuoteApplicationCache,
                                             "ApplicationCache",
                                             KW.Instance,
                                             null,
                                             null,
                                             null,
                                             3
                                             ),
               bootstrapCreateEffectiveSlot ("The lambda-list cache slot",
                                             QuoteLambdaList,
                                             "LambdaList",
                                             KW.Instance,
                                             null,
                                             null,
                                             null,
                                             4
                                             ),
              bootstrapCreateEffectiveSlot ("The methods slot",
                                            QuoteMethods,
                                            "Methods",
                                            KW.Instance,
                                            null,
                                            null,
                                            null,
                                            5
                                            ),
              bootstrapCreateEffectiveSlot ("The singletons list slot",
                                             QuoteSingletonsList,
                                             "SingletonsList",
                                             KW.Instance,
                                             null,
                                             null,
                                             null,
                                             6
                                             )
        );          // effectiveSlots

        static ICollection<StandardObject> theEffectiveSlotsOfStandardMethod =
                CL.List<StandardObject> (
                bootstrapCreateDocumentationSlot (),
                bootstrapCreateNameSlot (),
                bootstrapCreateStudlyNameSlot (),
                bootstrapCreateEffectiveSlot ("The function slot",
                                              QuoteFunction,
                                              "Function",
                                              KW.Instance,
                                              null,
                                              null,
                                              null,
                                              3),
                bootstrapCreateEffectiveSlot ("The qualifier slot",
                                              QuoteQualifier,
                                              "Qualifier",
                                              KW.Instance,
                                              null,
                                              null,
                                              null,
                                              4),
                bootstrapCreateEffectiveSlot ("The specializers slot",
                                              QuoteSpecializers,
                                              "Specializers",
                                              KW.Instance,
                                              null,
                                              null,
                                              null,
                                              5)
               
                 );          // effectiveSlots

        static public bool initializeClos ()
        {
            // Fill in the above.
            bootstrapInitializeStandardClass (builtInClass,
                "The superclass of all non CLOS classes.",
                QuoteBuiltInClass,
                "BuiltInClass",

                CL.List<StandardObject> (),          // directMethods
                CL.List<StandardObject> (),          // directSubclasses
                CL.List<StandardObject> (closClass), // directSuperclasses
                CL.List<StandardObject> (builtInClass, closClass, specializer, metaobject, standardObjectClass, top),       // precedenceList

                null,                                // defaultInitargs
                null,                                // directDefaultInitargs
                CL.List<StandardObject> (),          // directSlots
                ManifestInstance.CreateInstance (),  // prototype
                theEffectiveSlotsOfBuiltInClass
                );

           bootstrapInitializeStandardClass (builtInEffectiveSlotDefinition,
                "Class of dotnet-defined effective slot definitions.",
                QuoteBuiltInEffectiveSlotDefinition,
                "BuiltInEffectiveSlotDefinition",
                CL.List<StandardObject> (),                               // directMethods
                CL.List<StandardObject> (), // directSubclasses
                CL.List<StandardObject> (builtInEffectiveSlotDefinition),                 // directSuperclasses
                CL.List<StandardObject> (builtInEffectiveSlotDefinition, builtInSlotDefinition, slotDefinition, metaobject, standardObjectClass, top), // precedence list
                null,  // defaultInitargs
                null,  // directDefaultInitargs
                CL.List<StandardObject> (),         // directSlots
                ManifestInstance.CreateInstance (), // prototype
                CL.List<StandardObject> (
                bootstrapCreateDocumentationSlot (),
                bootstrapCreateNameSlot (),
                bootstrapCreateStudlyNameSlot ()
                // allocation
                // initargs
                // type
                 )          // effectiveSlots
                );


            bootstrapInitializeStandardClass (builtInSlotDefinition,
                "Superclass of dotnet-defined slot definitions.",
                QuoteBuiltInSlotDefinition,
                "BuiltInSlotDefinition",
                CL.List<StandardObject> (),                               // directMethods
                CL.List<StandardObject> (builtInEffectiveSlotDefinition), // directSubclasses
                CL.List<StandardObject> (slotDefinition),                 // directSuperclasses
                CL.List<StandardObject> (builtInSlotDefinition, slotDefinition, metaobject, standardObjectClass, top), // precedence list
                null,  // defaultInitargs
                null,  // directDefaultInitargs
                CL.List<StandardObject> (),         // directSlots
                ManifestInstance.CreateInstance (), // prototype
                CL.List<StandardObject> (
                bootstrapCreateDocumentationSlot (),
                bootstrapCreateNameSlot (),
                bootstrapCreateStudlyNameSlot ()
                // allocation
                // initargs
                // type
                 )          // effectiveSlots
                );

            bootstrapInitializeStandardClass (closClass,
                "The superclass of all class metaobjects.",
                QuoteClass,
                "Class",

                CL.List<StandardObject> (), // directMethods
                CL.List<StandardObject> (builtInClass, forwardReferencedClass, funcallableStandardClass, standardClass), // directSubclasses
                CL.List<StandardObject> (specializer),         // directSuperclasses
                CL.List<StandardObject> (closClass, specializer, metaobject, standardObjectClass, top),         // precedenceList

                null,                              // defaultInitargs
                null,                              // directDefaultInitargs
                CL.List<StandardObject> (
                // direct subclasses
                // direct superclasses
                // precedence list
                     ),      // directSlots
                ManifestInstance.CreateInstance (), // prototype
                CL.List<StandardObject> (
                bootstrapCreateDocumentationSlot (),
                bootstrapCreateNameSlot (),
                bootstrapCreateStudlyNameSlot ()
                // direct methods
                // direct subclasses
                // direct superclasses
                // precedence list
                // default initargs
                // direct default initargs
                // direct slots
                // prototype
                // slots
            )          // effectiveSlots
                );

            bootstrapInitializeStandardClass (directSlotDefinition,
                "The superclass of all direct slot definition metaobjects.",
                QuoteDirectSlotDefinition,
                "DirectSlotDefinition",

                CL.List<StandardObject> (),
                CL.List<StandardObject> (standardDirectSlotDefinition), // directSubclasses
                CL.List<StandardObject> (slotDefinition),               // directSuperclasses
                CL.List<StandardObject> (directSlotDefinition, slotDefinition, metaobject, standardObjectClass, top),         // precedenceList

                null,  // defaultInitargs
                null,  // directDefaultInitargs
                CL.List<StandardObject> (
                // readers
                // writers
                   ),         // directSlots
                ManifestInstance.CreateInstance (), // prototype
                CL.List<StandardObject> (
                bootstrapCreateDocumentationSlot (),
                bootstrapCreateNameSlot (),
                bootstrapCreateStudlyNameSlot ()

                // documentation
                // name
                // studlyname
                // allocation
                // initargs
                // type
                // readers
                // writers
                     )          // effectiveSlots
                );

            bootstrapInitializeStandardClass (effectiveSlotDefinition,
                "The superclass of all effective slot definition metaobjects.",
                QuoteEffectiveSlotDefinition,
                "EffectiveSlotDefinition",

                CL.List<StandardObject> (),
                CL.List<StandardObject> (standardEffectiveSlotDefinition), // directSubclasses
                CL.List<StandardObject> (slotDefinition),         // directSuperclasses
                CL.List<StandardObject> (effectiveSlotDefinition, slotDefinition, metaobject, standardObjectClass, top),         // precedenceList

                null,  // defaultInitargs
                null,  // directDefaultInitargs
                CL.List<StandardObject> (
                // initializer
                // location
                    ),         // directSlots
                ManifestInstance.CreateInstance (), // prototype
                CL.List<StandardObject> (
                bootstrapCreateDocumentationSlot (),
                bootstrapCreateNameSlot (),
                bootstrapCreateStudlyNameSlot ()
                // documentation
                // name
                // studlyname
                // allocation
                // initargs
                // type
                // initializer
                // location
                     )          // effectiveSlots
                );

            bootstrapInitializeStandardClass (eqlSpecializer,
                "The class of EQL specializers.",
                QuoteEqlSpecializer,
                "EqlSpecializer",

                CL.List<StandardObject> (),

                CL.List<StandardObject> (),         // directSubclasses
                CL.List<StandardObject> (specializer),         // directSuperclasses
                CL.List<StandardObject> (eqlSpecializer, specializer, metaobject, standardObjectClass, top),         // precedenceList

                null,  // defaultInitargs
                null,  // directDefaultInitargs
                CL.List<StandardObject> (),         // directSlots
                ManifestInstance.CreateInstance (), // prototype
                CL.List<StandardObject> (
                bootstrapCreateDocumentationSlot (),
                bootstrapCreateNameSlot (),
                bootstrapCreateStudlyNameSlot ()
                // documentation
                // name
                // studlyname
                    )          // effectiveSlots
                );

            bootstrapInitializeStandardClass (forwardReferencedClass,
                "The superclass of all classes that are forward referenced.",
                QuoteForwardReferencedClass,
                "ForwardReferencedClass",
                CL.List<StandardObject> (),         // directmethods
                CL.List<StandardObject> (),         // directSubclasses
                CL.List<StandardObject> (closClass),         // directSuperclasses
                CL.List<StandardObject> (forwardReferencedClass, closClass, specializer, metaobject, standardObjectClass, top),         // precedenceList

                null,  // defaultInitargs
                null,  // directDefaultInitargs
                CL.List<StandardObject> (),         // directSlots
                ManifestInstance.CreateInstance (), // prototype
                CL.List<StandardObject> (
                bootstrapCreateDocumentationSlot (),
                bootstrapCreateNameSlot (),
                bootstrapCreateStudlyNameSlot ()
                // documentation
                // name
                // studlyname
                // direct-methods
                     )          // effectiveSlots
                );

            bootstrapInitializeStandardClass (funcallableStandardClass,
                "The superclass of all classes that represent funcallable instances.",
                QuoteFuncallableStandardClass,
                "FuncallableStandardClass",
                CL.List<StandardObject> (),         // directMethods
                CL.List<StandardObject> (),         // directSubclasses
                CL.List<StandardObject> (closClass), // directSuperclasses
                CL.List<StandardObject> (funcallableStandardClass, closClass, specializer, metaobject, standardObjectClass, top),         // precedenceList
                null,  // defaultInitargs
                null,  // directDefaultInitargs
                CL.List<StandardObject> (
                // default initargs
                // direct default initargs
                // direct slots
                // prototype
                // slots

                    ),         // directSlots
                ManifestInstance.CreateInstance (), // prototype
            CL.List<StandardObject> (
                bootstrapCreateDocumentationSlot (),
                bootstrapCreateNameSlot (),
                bootstrapCreateStudlyNameSlot (),
                bootstrapCreateEffectiveSlot ("The direct methods slot",
                                             QuoteDirectMethods,
                                             "DirectMethods",
                                             KW.Instance,
                                             null,
                                             null,
                                             null,
                                             3
                                             ),
                bootstrapCreateEffectiveSlot ("The direct subclasses slot",
                                             QuoteDirectSubclasses,
                                             "DirectSubclasses",
                                             KW.Instance,
                                             null,
                                             null,
                                             null,
                                             4
                                             ),
                bootstrapCreateEffectiveSlot ("The direct superclasses slot",
                                             QuoteDirectMethods,
                                             "DirectSuperclasses",
                                             KW.Instance,
                                             null,
                                             null,
                                             null,
                                             5
                                             ),
                bootstrapCreateEffectiveSlot ("The precedence list slot",
                                             QuotePrecedenceList,
                                             "PrecedenceList",
                                             KW.Instance,
                                             null,
                                             null,
                                             null,
                                             6
                                             ),
                bootstrapCreateEffectiveSlot ("The default initargs slot",
                                             QuoteDefaultInitargs,
                                             "DefaultInitargs",
                                             KW.Instance,
                                             null,
                                             null,
                                             null,
                                             7
                                             ),
                bootstrapCreateEffectiveSlot ("The direct default initargs slot",
                                             QuoteDirectDefaultInitargs,
                                             "DirectDefaultInitargs",
                                             KW.Instance,
                                             null,
                                             null,
                                             null,
                                             8
                                             ),

                bootstrapCreateEffectiveSlot ("The direct slots slot",
                                             QuoteDirectSlots,
                                             "DirectSlots",
                                             KW.Instance,
                                             null,
                                             null,
                                             null,
                                             9
                                             ),

                bootstrapCreateEffectiveSlot ("The prototype slot",
                                             QuotePrototype,
                                             "Prototype",
                                             KW.Instance,
                                             null,
                                             null,
                                             null,
                                             10
                                             ),

                bootstrapCreateEffectiveSlot ("The slots slot",
                                             QuoteSlots,
                                             "Slots",
                                             KW.Instance,
                                             null,
                                             null,
                                             null,
                                             11
                                             )
                ) // effective slots
                 );

            bootstrapInitializeStandardClass (funcallableStandardObject,
                "The superclass of CLOS objects that can be invoked on arguments.",
                QuoteFuncallableStandardObject,
                "FuncallableStandardObject",
                CL.List<StandardObject> (),
                CL.List<StandardObject> (genericFunction),         // directSubclasses
                CL.List<StandardObject> (standardObjectClass, function),         // directSuperclasses
                CL.List<StandardObject> (funcallableStandardObject, standardObjectClass, function, top),         // precedenceList
                null,  // defaultInitargs
                null,  // directDefaultInitargs
                CL.List<StandardObject> (),         // directSlots
                ManifestInstance.CreateInstance (), // prototype
                CL.List<StandardObject> ()          // effectiveSlots
                );

            bootstrapInitializeBuiltInClass (builtInClass, function,
                "Built-in class representing functions.",
                QuoteFunction,
                "Function",
                CL.List<StandardObject> (),
                CL.List<StandardObject> (funcallableStandardObject),
                CL.List<StandardObject> (top),
                CL.List<StandardObject> (function, top),
                typeof(Delegate)
                );

            bootstrapInitializeFuncallableStandardClass (genericFunction,
                "Superclass of generic functions.",
                QuoteGenericFunction,
                "GenericFunction",
                CL.List<StandardObject> (),         // directMethods
                CL.List<StandardObject> (standardGenericFunction),                 // directSubclasses
                CL.List<StandardObject> (metaobject, funcallableStandardObject),   // directSuperclasses
                CL.List<StandardObject> (genericFunction, metaobject, funcallableStandardObject, standardObjectClass, function, top), // precedenceList
                null,  // defaultInitargs
                null,  // directDefaultInitargs
                CL.List<StandardObject> (),         // directSlots
                ManifestInstance.CreateInstance (), // prototype
                CL.List<StandardObject> (
                bootstrapCreateDocumentationSlot (),
                bootstrapCreateNameSlot (),
                bootstrapCreateStudlyNameSlot ()
                // documentation
                // name
                // studlyname
                      )          // effectiveSlots
                );

            bootstrapInitializeStandardClass (metaobject,
                "Superclass of all CLOS metaobjects.",
                QuoteMetaobject,
                "Metaobject",
            CL.List<StandardObject> (), //directMethods
                CL.List<StandardObject> (genericFunction, method, methodCombination, specializer, slotDefinition), // directSubclasses
                CL.List<StandardObject> (standardObjectClass),         // directSuperclasses
                CL.List<StandardObject> (metaobject, standardObjectClass, top),         // precedenceList
                null,  // defaultInitargs
                null,  // directDefaultInitargs
                CL.List<StandardObject> (

                // documentation
                // name
                // studlyname

                       ),         // directSlots
                ManifestInstance.CreateInstance (), // prototype
                CL.List<StandardObject> (
                bootstrapCreateDocumentationSlot (),
                bootstrapCreateNameSlot (),
                bootstrapCreateStudlyNameSlot ()
                // direct methods
                // direct subclasses
                // direct superclasses
                // precedence list

                      )          // effectiveSlots
                );

            bootstrapInitializeStandardClass (method,
                "Superclass of all method classes.",
                QuoteMethod,
                "Method",
                CL.List<StandardObject> (),         // directMethods
                CL.List<StandardObject> (standardMethod), // directSubclasses
                CL.List<StandardObject> (metaobject),     // directSuperclasses
                CL.List<StandardObject> (method, metaobject, standardObjectClass, top),         // precedenceList
                null,  // defaultInitargs
                null,  // directDefaultInitargs
                CL.List<StandardObject> (),         // directSlots
                ManifestInstance.CreateInstance (), // prototype
                CL.List<StandardObject> (
                bootstrapCreateDocumentationSlot (),
                bootstrapCreateNameSlot (),
                bootstrapCreateStudlyNameSlot ()
                     )          // effectiveSlots
                );

            bootstrapInitializeStandardClass (methodCombinationClass,
                "Class of method combination objects.",
                QuoteMethodCombination,
                "MethodCombination",
                CL.List<StandardObject> (),
                CL.List<StandardObject> (),         // directSubclasses
                CL.List<StandardObject> (metaobject),         // directSuperclasses
                CL.List<StandardObject> (methodCombinationClass, metaobject, standardObjectClass, top),         // precedenceList
                null,  // defaultInitargs
                null,  // directDefaultInitargs
                CL.List<StandardObject> (),         // directSlots
                ManifestInstance.CreateInstance (), // prototype
                CL.List<StandardObject> (
                bootstrapCreateDocumentationSlot (),
                bootstrapCreateNameSlot (),
                bootstrapCreateStudlyNameSlot ()
                // documentation
                // name
                // studlyname
                )          // effectiveSlots
                );

            bootstrapInitializeStandardClass (slotDefinition,
                "Superclass of all slot definition classes.",
                QuoteSlotDefinition,
                "SlotDefinition",
                CL.List<StandardObject> (),
                CL.List<StandardObject> (directSlotDefinition, effectiveSlotDefinition, standardSlotDefinition),         // directSubclasses
                CL.List<StandardObject> (metaobject),         // directSuperclasses
                CL.List<StandardObject> (slotDefinition, metaobject, standardObjectClass, top),         // precedenceList
                null,  // defaultInitargs
                null,  // directDefaultInitargs
                CL.List<StandardObject> (
                // allocation
                // initargs
                // type
                       ),         // directSlots
                ManifestInstance.CreateInstance (), // prototype
                CL.List<StandardObject> (
                bootstrapCreateDocumentationSlot (),
                bootstrapCreateNameSlot (),
                bootstrapCreateStudlyNameSlot ()
                // allocation
                // initargs
                // type
                   )          // effectiveSlots
                );

            bootstrapInitializeStandardClass (specializer,
                "Superclass of all objects that can be specialized upon.",
                QuoteSpecializer,
                "Specializer",

            CL.List<StandardObject> (),
                CL.List<StandardObject> (closClass, eqlSpecializer),         // directSubclasses
                CL.List<StandardObject> (metaobject),         // directSuperclasses
                CL.List<StandardObject> (specializer, metaobject, standardObjectClass, top),         // precedenceList
                null,  // defaultInitargs
                null,  // directDefaultInitargs
                CL.List<StandardObject> (
                // direct methods
                      ),         // directSlots
                ManifestInstance.CreateInstance (), // prototype
                CL.List<StandardObject> (
                bootstrapCreateDocumentationSlot (),
                bootstrapCreateNameSlot (),
                bootstrapCreateStudlyNameSlot ()
                // documentation
                // name
                // studlyname
                // direct-methods
                // direct subclasses
                // direct superclasses
                // precedence list

                )          // effectiveSlots
                );

            bootstrapInitializeStandardClass (standardAccessorMethodClass,
                "Superclass of all methods that access slots.",
                QuoteStandardAccessorMethod,
                "StandardAccessorMethod",
                CL.List<StandardObject> (),         // directSlots
                CL.List<StandardObject> (standardReaderMethod, standardWriterMethod), // directSubclasses
                CL.List<StandardObject> (standardMethod),                             // directSuperclasses
                CL.List<StandardObject> (standardAccessorMethodClass, standardMethod, method, metaobject, standardObjectClass, top),         // precedenceList

                null,  // defaultInitargs
                null,  // directDefaultInitargs
                CL.List<StandardObject> (),         // directSlots
                ManifestInstance.CreateInstance (), // prototype
                CL.List<StandardObject> (
                bootstrapCreateDocumentationSlot (),
                bootstrapCreateNameSlot (),
                bootstrapCreateStudlyNameSlot (),
                bootstrapCreateEffectiveSlot ("The function slot",
                                              QuoteFunction,
                                              "Function",
                                              KW.Instance,
                                              null,
                                              null,
                                              null,
                                              3),
                bootstrapCreateEffectiveSlot ("The qualifier slot",
                                              QuoteQualifier,
                                              "Qualifier",
                                              KW.Instance,
                                              null,
                                              null,
                                              null,
                                              4),
                bootstrapCreateEffectiveSlot ("The specializers slot",
                                              QuoteSpecializers,
                                              "Specializers",
                                              KW.Instance,
                                              null,
                                              null,
                                              null,
                                              5) 
                 )          // effectiveSlots
                );

            bootstrapInitializeStandardClass (standardClass,
                "Superclass of CLOS-defined classes.",
                 QuoteStandardClass,
                "StandardClass",
                CL.List<StandardObject> (),         // directMethods
                CL.List<StandardObject> (),         // directSubclasses
                CL.List<StandardObject> (closClass),         // directSuperclasses
                CL.List<StandardObject> (standardClass, closClass, specializer, metaobject, standardObjectClass, top),         // precedenceList

                null,  // defaultInitargs
                null,  // directDefaultInitargs
                CL.List<StandardObject> (
                // default initargs
                // direct default initargs
                // direct slots
                // prototype
                // slots
                    ),         // directSlots
                ManifestInstance.CreateInstance (), // prototype
                theEffectiveSlotsOfStandardClass // effectiveSlots
                );

            bootstrapInitializeStandardClass (standardDirectSlotDefinition,
                "Class of direct slot definitions for standard classes.",
                QuoteStandardDirectSlotDefinition,
                "StandardDirectSlotDefinition",
                CL.List<StandardObject> (),         // directMethods
                CL.List<StandardObject> (),         // directSubclasses
                CL.List<StandardObject> (standardSlotDefinition, directSlotDefinition),         // directSuperclasses
                CL.List<StandardObject> (standardDirectSlotDefinition, directSlotDefinition, standardSlotDefinition, slotDefinition, metaobject, standardObjectClass, top),         // precedenceList
                null,  // defaultInitargs
                null,  // directDefaultInitargs
                CL.List<StandardObject> (),         // directSlots
                ManifestInstance.CreateInstance (), // prototype
                CL.List<StandardObject> (
                bootstrapCreateDocumentationSlot (),
                bootstrapCreateNameSlot (),
                bootstrapCreateStudlyNameSlot ()
                // documentation
                // name
                // studlyname
                // allocation
                // initargs
                // type
                // readers
                // writers
                   )          // effectiveSlots
                );

            bootstrapInitializeStandardClass (standardEffectiveSlotDefinition,
                "Class of effective slot definitions for standard classes.",
                QuoteStandardEffectiveSlotDefinition,
                "StandardEffectiveSlotDefinition",
                CL.List<StandardObject> (),         // directMethods
                CL.List<StandardObject> (),         // directSubclasses
                CL.List<StandardObject> (standardSlotDefinition, effectiveSlotDefinition),         // directSuperclasses
                CL.List<StandardObject> (standardEffectiveSlotDefinition, effectiveSlotDefinition, standardSlotDefinition, slotDefinition, metaobject, standardObjectClass, top),         // precedenceList
                null,  // defaultInitargs
                null,  // directDefaultInitargs
                CL.List<StandardObject> (),         // directSlots
                ManifestInstance.CreateInstance (), // prototype
                CL.List<StandardObject> (
                bootstrapCreateDocumentationSlot (),
                bootstrapCreateNameSlot (),
                bootstrapCreateStudlyNameSlot ()
                // documentation
                // name
                // studlyname
                // allocation
                // initargs
                // type
                // initializer
                // location
                     )          // effectiveSlots
                );

            bootstrapInitializeFuncallableStandardClass (standardGenericFunction,
                "Class of CLOS-defined generic functions.",
                QuoteStandardGenericFunction,
                "StandardGenericFunction",
                CL.List<StandardObject> (), // directMethods
                CL.List<StandardObject> (), // directSubclasses
                CL.List<StandardObject> (genericFunction),                 // directSuperclasses
                CL.List<StandardObject> (standardGenericFunction, genericFunction, metaobject, funcallableStandardObject, standardObjectClass, function, top),         // precedenceList
                null,  // defaultInitargs
                null,  // directDefaultInitargs
                CL.List<StandardObject> (
                // application cache
                // lambda list
                // singletons list
                    ),         // directSlots
                ManifestInstance.CreateInstance (), // prototype
                theEffectiveSlotsOfStandardGenericFunction
          // effectiveSlots
        );

            bootstrapInitializeStandardClass (standardMethod,
                "Class of CLOS-defined methods.",
                QuoteStandardMethod,
                "StandardMethod",
                CL.List<StandardObject> (
                       // function
                       // specializers
                       // qualifier
                        ),         // directMethods
                CL.List<StandardObject> (standardAccessorMethod), // directSubclasses
                CL.List<StandardObject> (method),                 // directSuperclasses
                CL.List<StandardObject> (standardMethod, method, metaobject, standardObjectClass, top),         // precedenceList
                null,  // defaultInitargs
                null,  // directDefaultInitargs
                CL.List<StandardObject> (),         // directSlots
                ManifestInstance.CreateInstance (), // prototype
                theEffectiveSlotsOfStandardMethod
                );

            bootstrapInitializeStandardClass (standardObjectClass,
            "Superclass of CLOS-defined objects.",
                QuoteStandardObject,
                "StandardObjectClass",
                CL.List<StandardObject> (),           // directMethods
                CL.List<StandardObject> (funcallableStandardObject, metaobject), // directSubclasses
                CL.List<StandardObject> (top),        // directSuperclasses
                CL.List<StandardObject> (standardObjectClass, top),           // precedenceList
                null,  // defaultInitargs
                null,  // directDefaultInitargs
                CL.List<StandardObject> (),           // directSlots
                ManifestInstance.CreateInstance (),   // prototype
                CL.List<StandardObject> (
                bootstrapCreateDocumentationSlot (),
                bootstrapCreateNameSlot (),
                bootstrapCreateStudlyNameSlot ()
                // documentation
                // name
                // studlyname
                   )            // effectiveSlots
                );

            bootstrapInitializeStandardClass (standardReaderMethod,
                "Superclass of slot reader methods.",
                QuoteStandardReaderMethod,
                "StandardReaderMethod",
                CL.List<StandardObject> (),         // directMethods
                CL.List<StandardObject> (),         // directSubclasses
                CL.List<StandardObject> (standardAccessorMethodClass),         // directSuperclasses
                CL.List<StandardObject> (standardReaderMethod, standardAccessorMethodClass, standardMethod, method, metaobject, standardObjectClass, top),         // precedenceList
                null,  // defaultInitargs
                null,  // directDefaultInitargs
                CL.List<StandardObject> (),         // directSlots
                ManifestInstance.CreateInstance (), // prototype
                CL.List<StandardObject> (
                bootstrapCreateDocumentationSlot (),
                bootstrapCreateNameSlot (),
                bootstrapCreateStudlyNameSlot (),
                bootstrapCreateEffectiveSlot ("The function slot",
                                              QuoteFunction,
                                              "Function",
                                              KW.Instance,
                                              null,
                                              null,
                                              null,
                                              3),
                bootstrapCreateEffectiveSlot ("The qualifier slot",
                                              QuoteQualifier,
                                              "Qualifier",
                                              KW.Instance,
                                              null,
                                              null,
                                              null,
                                              4),
                bootstrapCreateEffectiveSlot ("The specializers slot",
                                              QuoteSpecializers,
                                              "Specializers",
                                              KW.Instance,
                                              null,
                                              null,
                                              null,
                                              5)
                )          // effectiveSlots
                );

            bootstrapInitializeStandardClass (standardSlotDefinition,
                "Superclass of CLOS-defined slot definitions.",
                QuoteStandardSlotDefinition,
                "StandardSlotDefinition",
                CL.List<StandardObject> (),         // directMethods
                CL.List<StandardObject> (standardDirectSlotDefinition, standardEffectiveSlotDefinition), // directSubclasses
                CL.List<StandardObject> (standardSlotDefinition, slotDefinition, metaobject, standardObjectClass, top),         // directSuperclasses
                CL.List<StandardObject> (top),         // precedenceList
                null,  // defaultInitargs
                null,  // directDefaultInitargs
                CL.List<StandardObject> (),         // directSlots
                ManifestInstance.CreateInstance (), // prototype
                CL.List<StandardObject> (
                bootstrapCreateDocumentationSlot (),
                bootstrapCreateNameSlot (),
                bootstrapCreateStudlyNameSlot ()
                // allocation
                // initargs
                // type
                 )          // effectiveSlots
                );

            bootstrapInitializeStandardClass (standardWriterMethod,
            "Superclass of CLOS-defined writer methods.",
                QuoteStandardWriterMethod,
                "StandardWriterMethod",
                CL.List<StandardObject> (),         // directMethods
                CL.List<StandardObject> (),         // directSubclasses
                CL.List<StandardObject> (standardAccessorMethodClass),         // directSuperclasses
                CL.List<StandardObject> (standardWriterMethod, standardAccessorMethodClass, standardMethod, method, metaobject, standardObjectClass, top),         // precedenceList
                null,  // defaultInitargs
                null,  // directDefaultInitargs
                CL.List<StandardObject> (),         // directSlots
                ManifestInstance.CreateInstance (), // prototype
                CL.List<StandardObject> (
                bootstrapCreateDocumentationSlot (),
                bootstrapCreateNameSlot (),
                bootstrapCreateStudlyNameSlot (),
                bootstrapCreateEffectiveSlot ("The function slot",
                                              QuoteFunction,
                                              "Function",
                                              KW.Instance,
                                              null,
                                              null,
                                              null,
                                              3),
                bootstrapCreateEffectiveSlot ("The qualifier slot",
                                              QuoteQualifier,
                                              "Qualifier",
                                              KW.Instance,
                                              null,
                                              null,
                                              null,
                                              4),
                bootstrapCreateEffectiveSlot ("The specializers slot",
                                              QuoteSpecializers,
                                              "Specializers",
                                              KW.Instance,
                                              null,
                                              null,
                                              null,
                                              5)
                     )          // effectiveSlots
                );

            bootstrapInitializeBuiltInClass (builtInClass, top,
                "Root of the class hierarchy.",
                QuoteTop,
                "Top",
                CL.List<StandardObject> (), // direct methods
                CL.List<StandardObject> (function, standardObjectClass), // direct subclasses
                CL.List<StandardObject> (),  //direct superclasses
                CL.List<StandardObject> (top), // precedence list
                typeof(object)
                );

            bootstrapInitializeBuiltInClass(builtInClass, runtimeType,
                "Reflected dotnet class type.",
                QuoteRuntimeType,
                "RuntimeType",
                CL.List<StandardObject> (), // direct methods
                CL.List<StandardObject> (),
                CL.List<StandardObject> (runtimeType, builtInClass),  //direct superclasses
                CL.List<StandardObject> (runtimeType, builtInClass, closClass, specializer, metaobject, standardObjectClass, top),
                typeof(object).GetType()
                );

            // The generic functions

            accessorMethodSlotDefinition.SetClass (StandardGenericFunction);
            accessorMethodSlotDefinition.SetSlots (new object [] {
                "missing documentation string",
                QuoteAccessorMethodSlotDefinition,
                "AccessorMethodSlotDefinition" });
            allNamedGenericFunctions.Add (QuoteAccessorMethodSlotDefinition, accessorMethodSlotDefinition);

            addDependent.SetClass (StandardGenericFunction);
            addDependent.SetSlots (new object [] {
                "missing documentation string",
                QuoteAddDependent,
                "AddDependent" });
            allNamedGenericFunctions.Add (QuoteAddDependent, addDependent);

            addDirectMethod.SetClass (StandardGenericFunction);
            addDirectMethod.SetSlots (new object [] {
                "missing documentation string",
                QuoteAddDirectMethod,
                "AddDirectMethod" });
            allNamedGenericFunctions.Add (QuoteAddDirectMethod, addDirectMethod);

            addDirectSubclass.SetClass (StandardGenericFunction);
            addDirectSubclass.SetSlots (new object [] {
                "missing documentation string",
                QuoteAddDirectSubclass,
                "AddDirectSubclass" });
            allNamedGenericFunctions.Add (QuoteAddDirectSubclass, addDirectSubclass);

            addMethod.SetClass (StandardGenericFunction);
            addMethod.SetSlots (new object [] {
                "missing documentation string",
                QuoteAddMethod,
                "AddMethod" });
            allNamedGenericFunctions.Add (QuoteAddMethod, addMethod);

            allocateInstance.SetClass (StandardGenericFunction);
            allocateInstance.SetSlots (new object [] {
                "missing documentation string",
                QuoteAllocateInstance,
                "AllocateInstance" });
            allNamedGenericFunctions.Add (QuoteAllocateInstance, allocateInstance);

            changeClass.SetClass (StandardGenericFunction);
            changeClass.SetSlots (new object [] {
                "missing documentation string",
                QuoteChangeClass,
                "ChangeClass" });
            allNamedGenericFunctions.Add (QuoteChangeClass, changeClass);

            classDefaultInitargs.SetClass (StandardGenericFunction);
            classDefaultInitargs.SetSlots (new object [] {
                "missing documentation string",
                QuoteClassDefaultInitargs,
                "ClassDefaultInitargs" });
            allNamedGenericFunctions.Add (QuoteClassDefaultInitargs, classDefaultInitargs);

            classDirectDefaultInitargs.SetClass (StandardGenericFunction);
            classDirectDefaultInitargs.SetSlots (new object [] {
                "missing documentation string",
                QuoteClassDirectDefaultInitargs,
                "ClassDirectDefaultInitargs" });
            allNamedGenericFunctions.Add (QuoteClassDirectDefaultInitargs, classDirectDefaultInitargs);

            classDirectSlots.SetClass (StandardGenericFunction);
            classDirectSlots.SetSlots (new object [] {
                "missing documentation string",
                QuoteClassDirectSlots,
                "ClassDirectSlots" });
            allNamedGenericFunctions.Add (QuoteClassDirectSlots, classDirectSlots);

            classDirectSubclasses.SetClass (StandardGenericFunction);
            classDirectSubclasses.SetSlots (new object [] {
                "missing documentation string",
                QuoteClassDirectSubclasses,
                "ClassDirectSubclasses" });
            allNamedGenericFunctions.Add (QuoteClassDirectSubclasses, classDirectSubclasses);

            classDirectSuperclasses.SetClass (StandardGenericFunction);
            classDirectSuperclasses.SetSlots (new object [] {
                "missing documentation string",
                QuoteClassDirectSuperclasses,
                "ClassDirectSuperclasses" });
            allNamedGenericFunctions.Add (QuoteClassDirectSuperclasses, classDirectSuperclasses);

            className.SetClass (StandardGenericFunction);
            className.SetSlots (new object [] {
                "missing documentation string",
                QuoteClassName,
                "ClassName" });
            allNamedGenericFunctions.Add (QuoteClassName, className);

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

            classPrototype.SetClass (StandardGenericFunction);
            classPrototype.SetSlots (new object [] {
                "missing documentation string",
                QuoteClassPrototype,
                "ClassPrototype" });
            allNamedGenericFunctions.Add (QuoteClassPrototype, classPrototype);

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
                                                                method1ForClassSlots,
                                                                method2ForClassSlots,
                                                                method3ForClassSlots),
                                                        CL.List<ICollection<object>> (CL.List<object>(standardClass, funcallableStandardClass))
                                                                );

            bootstrapInitializeStandardGenericFunction (computeApplicableMethods,
                                                        "Gets the applicable methods for a generic function call.",
                                                        QuoteComputeApplicableMethods,
                                                        "ComputeApplicableMethods",
                                                        CL.List(QuoteGenericFunction, QuoteAndRest, QuoteArguments),
                                                        null,
                                                        CL.List<ICollection<object>>(null));
            setFuncallableInstanceFunction (computeApplicableMethods, bootstrapComputeApplicableMethods); 

            computeApplicableMethodsUsingClasses.SetClass (StandardGenericFunction);
            computeApplicableMethodsUsingClasses.SetSlots (new object [] {
                "missing documentation string",
                QuoteComputeApplicableMethodsUsingClasses,
                "ComputeApplicableMethodsUsingClasses" });
            allNamedGenericFunctions.Add (QuoteComputeApplicableMethodsUsingClasses, computeApplicableMethodsUsingClasses);

            computeClassPrecedenceList.SetClass (StandardGenericFunction);
            computeClassPrecedenceList.SetSlots (new object [] {
                "missing documentation string",
                QuoteComputeClassPrecedenceList,
                "ComputeClassPrecedenceList" });
            allNamedGenericFunctions.Add (QuoteComputeClassPrecedenceList, computeClassPrecedenceList);

            computeDefaultInitargs.SetClass (StandardGenericFunction);
            computeDefaultInitargs.SetSlots (new object [] {
                "missing documentation string",
                QuoteComputeDefaultInitargs,
                "ComputeDefaultInitargs" });
            allNamedGenericFunctions.Add (QuoteComputeDefaultInitargs, computeDefaultInitargs);

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
            setFuncallableInstanceFunction (computeDiscriminatingFunction, bootstrapComputeDiscriminatingFunction);

            bootstrapInitializeStandardGenericFunction (computeEffectiveMethod,
                                                        "Computes the effective method of a particular invokation.",
                                                        QuoteComputeEffectiveMethod,
                                                        "ComputeEffectiveMethod",
                                                        CL.List(QuoteGenericFunction, QuoteAndRest, QuoteArguments),
                                                        null,
                                                        CL.List<ICollection<object>>(null));
            setFuncallableInstanceFunction (computeEffectiveMethod, bootstrapComputeEffectiveMethod);

            computeEffectiveSlotDefinition.SetClass (StandardGenericFunction);
            computeEffectiveSlotDefinition.SetSlots (new object [] {
                "missing documentation string",
                QuoteComputeEffectiveSlotDefinition,
                "ComputeEffectiveSlotDefinition" });
            allNamedGenericFunctions.Add (QuoteComputeEffectiveSlotDefinition, computeEffectiveSlotDefinition);

            computeSlots.SetClass (StandardGenericFunction);
            computeSlots.SetSlots (new object [] {
                "missing documentation string",
                QuoteComputeSlots,
                "ComputeSlots" });
            allNamedGenericFunctions.Add (QuoteComputeSlots, computeSlots);

            bootstrapInitializeStandardGenericFunction (computeMethodMoreSpecific,
                                                        "Compares two methods to determine which is more specific.",
                                                         QuoteComputeMethodMoreSpecific,
                                                        "ComputeMethodMoreSpecific",
                                                        CL.List(QuoteLeft, QuoteRight), 
                                                        null,
                                                        CL.List<ICollection<object>>(null, null));
            setFuncallableInstanceFunction (computeMethodMoreSpecific, bootstrapComputeMethodMoreSpecific);

            defaultInitargs.SetClass (StandardGenericFunction);
            defaultInitargs.SetSlots (new object [] {
                "missing documentation string",
                QuoteDefaultInitargs,
                "DefaultInitargs" });
            allNamedGenericFunctions.Add (QuoteDefaultInitargs, defaultInitargs);

            directSlotDefinitionClass.SetClass (StandardGenericFunction);
            directSlotDefinitionClass.SetSlots (new object [] {
                "missing documentation string",
                QuoteDirectSlotDefinitionClass,
                "DirectSlotDefinitionClass" });
            allNamedGenericFunctions.Add (QuoteDirectSlotDefinitionClass, directSlotDefinitionClass);

            effectiveSlotDefinitionClass.SetClass (StandardGenericFunction);
            effectiveSlotDefinitionClass.SetSlots (new object [] {
                "missing documentation string",
                QuoteEffectiveSlotDefinitionClass,
                "EffectiveSlotDefinitionClass" });
            allNamedGenericFunctions.Add (QuoteEffectiveSlotDefinitionClass, effectiveSlotDefinitionClass);

            ensureClass.SetClass (StandardGenericFunction);
            ensureClass.SetSlots (new object [] {
                "missing documentation string",
                QuoteEnsureClass,
                "EnsureClass" });
            allNamedGenericFunctions.Add (QuoteEnsureClass, ensureClass);

            ensureClassUsingClass.SetClass (StandardGenericFunction);
            ensureClassUsingClass.SetSlots (new object [] {
                "missing documentation string",
                QuoteEnsureClassUsingClass,
                "EnsureClassUsingClass" });
            allNamedGenericFunctions.Add (QuoteEnsureClassUsingClass, ensureClassUsingClass);

            bootstrapInitializeStandardMethod (method0ForEnsureGenericFunction,
                                               "Ensures a generic function exists.",
                                               QuoteMethodEnsureGenericFunction,
                                               "Method:EnsureGenericFunction",
                                               (EnsureGenericFunctionMethodSignature) ensureGenericFunctionMethod0Impl,
                                               KW.Primary,
                                               CL.List (FindDotnetClass (typeof (Symbol)))
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

            ensureGenericFunctionUsingClass.SetClass (StandardGenericFunction);
            ensureGenericFunctionUsingClass.SetSlots (new object [] {
                "missing documentation string",
                QuoteEnsureGenericFunctionUsingClass,
                "EnsureGenericFunctionUsingClass" });
            allNamedGenericFunctions.Add (QuoteEnsureGenericFunctionUsingClass, ensureGenericFunctionUsingClass);

            eqlSpecializerObject.SetClass (StandardGenericFunction);
            eqlSpecializerObject.SetSlots (new object [] {
                "missing documentation string",
                QuoteEqlSpecializerObject,
                "EqlSpecializerObject" });
            allNamedGenericFunctions.Add (QuoteEqlSpecializerObject, eqlSpecializerObject);

            extractLambdaList.SetClass (StandardGenericFunction);
            extractLambdaList.SetSlots (new object [] {
                "missing documentation string",
                QuoteExtractLambdaList,
                "ExtractLambdaList" });
            allNamedGenericFunctions.Add (QuoteExtractLambdaList, extractLambdaList);

            extractSpecializerNames.SetClass (StandardGenericFunction);
            extractSpecializerNames.SetSlots (new object [] {
                "missing documentation string",
                QuoteExtractSpecializerNames,
                "ExtractSpecializerNames" });
            allNamedGenericFunctions.Add (QuoteExtractSpecializerNames, extractSpecializerNames);

            finalizeInheritance.SetClass (StandardGenericFunction);
            finalizeInheritance.SetSlots (new object [] {
                "missing documentation string",
                QuoteFinalizeInheritance,
                "FinalizeInheritance" });
            allNamedGenericFunctions.Add (QuoteFinalizeInheritance, finalizeInheritance);

            findClass.SetClass (StandardGenericFunction);
            findClass.SetSlots (new object [] {
                "missing documentation string",
                QuoteFindClass,
                "FindClass" });
            allNamedGenericFunctions.Add (QuoteFindClass, findClass);

            findMethod.SetClass (StandardGenericFunction);
            findMethod.SetSlots (new object [] {
                "missing documentation string",
                QuoteFindMethod,
                "FindMethod" });
            allNamedGenericFunctions.Add (QuoteFindMethod, findMethod);

            findMethodCombination.SetClass (StandardGenericFunction);
            findMethodCombination.SetSlots (new object [] {
                "missing documentation string",
                QuoteFindMethodCombination,
                "FindMethodCombination" });
            allNamedGenericFunctions.Add (QuoteFindMethodCombination, findMethodCombination);

            functionKeywords.SetClass (StandardGenericFunction);
            functionKeywords.SetSlots (new object [] {
                "missing documentation string",
                QuoteFunctionKeywords,
                "FunctionKeywords" });
            allNamedGenericFunctions.Add (QuoteFunctionKeywords, functionKeywords);

            bootstrapInitializeStandardGenericFunction (genericFunctionApplicationCache,
                                                        "Returns the application cache of a generic function.",
                                                        QuoteGenericFunctionApplicationCache,
                                                        "GenericFunctionApplicationCache",
                                                        CL.List (QuoteGenericFunction),
                                                        null,
                                                        CL.List<ICollection<object>>(null));
            setFuncallableInstanceFunction (genericFunctionApplicationCache, bootstrapGenericFunctionApplicationCache);

            genericFunctionArgumentPrecedenceOrder.SetClass (StandardGenericFunction);
            genericFunctionArgumentPrecedenceOrder.SetSlots (new object [] {
                "missing documentation string",
                QuoteGenericFunctionArgumentPrecedenceOrder,
                "GenericFunctionArgumentPrecedenceOrder" });
            allNamedGenericFunctions.Add (QuoteGenericFunctionArgumentPrecedenceOrder, genericFunctionArgumentPrecedenceOrder);

            genericFunctionDeclarations.SetClass (StandardGenericFunction);
            genericFunctionDeclarations.SetSlots (new object [] {
                "missing documentation string",
                QuoteGenericFunctionDeclarations,
                "GenericFunctionDeclarations" });
            allNamedGenericFunctions.Add (QuoteGenericFunctionDeclarations, genericFunctionDeclarations);

            bootstrapInitializeStandardGenericFunction (genericFunctionLambdaList,
                                        "Returns the lambda list of a generic function.",
                                            QuoteGenericFunctionLambdaList,
                                            "GenericFunctionLambdaList",
                                            CL.List (QuoteGenericFunction),
                                            null,
                                                      CL.List<ICollection<object>>(null));
            setFuncallableInstanceFunction (genericFunctionLambdaList, bootstrapGenericFunctionLambdaList);

            genericFunctionMethodClass.SetClass (StandardGenericFunction);
            genericFunctionMethodClass.SetSlots (new object [] {
                "missing documentation string",
                QuoteGenericFunctionMethodClass,
                "GenericFunctionMethodClass" });
            allNamedGenericFunctions.Add (QuoteGenericFunctionMethodClass, genericFunctionMethodClass);

            genericFunctionMethodCombination.SetClass (StandardGenericFunction);
            genericFunctionMethodCombination.SetSlots (new object [] {
                "missing documentation string",
                QuoteGenericFunctionMethodCombination,
                "GenericFunctionMethodCombination" });
            allNamedGenericFunctions.Add (QuoteGenericFunctionMethodCombination, genericFunctionMethodCombination);

            bootstrapInitializeStandardGenericFunction (genericFunctionMethods,
                                                        "Returns the methods of a generic function.",
                                                        QuoteGenericFunctionMethods,
                                                        "GenericFunctionMethods",
                                                        CL.List(QuoteGenericFunction),
                                                        null,
                                                        CL.List<ICollection<object>>(null));
            setFuncallableInstanceFunction (genericFunctionMethods, bootstrapGenericFunctionMethods);

            genericFunctionName.SetClass (StandardGenericFunction);
            genericFunctionName.SetSlots (new object [] {
                "missing documentation string",
                QuoteGenericFunctionName,
                "GenericFunctionName" });
            allNamedGenericFunctions.Add (QuoteGenericFunctionName, genericFunctionName);

            bootstrapInitializeStandardGenericFunction (genericFunctionSingletonsList,
                            "Returns the singletons list of a generic function.",
                                QuoteGenericFunctionSingletonsList,
                                "GenericFunctionLambdaList",
                                CL.List(QuoteGenericFunction),
                                null,
                                                     CL.List<ICollection<object>>(null));
            setFuncallableInstanceFunction (genericFunctionSingletonsList, bootstrapGenericFunctionSingletonsList);
 
            initializeInstance.SetClass (StandardGenericFunction);
            initializeInstance.SetSlots (new object [] {
                "missing documentation string",
                QuoteInitializeInstance,
                "InitializeInstance" });
            allNamedGenericFunctions.Add (QuoteInitializeInstance, initializeInstance);

            internEqlSpecializer.SetClass (StandardGenericFunction);
            internEqlSpecializer.SetSlots (new object [] {
                "missing documentation string",
                QuoteInternEqlSpecializer,
                "InternEqlSpecializer" });
            allNamedGenericFunctions.Add (QuoteInternEqlSpecializer, internEqlSpecializer);

            isClassFinalized.SetClass (StandardGenericFunction);
            isClassFinalized.SetSlots (new object [] {
                "missing documentation string",
                QuoteIsClassFinalized,
                "IsClassFinalized" });
            allNamedGenericFunctions.Add (QuoteIsClassFinalized, isClassFinalized);

            makeInstance.SetClass (StandardGenericFunction);
            makeInstance.SetSlots (new object [] {
                "missing documentation string",
                QuoteMakeInstance,
                "MakeInstance" });
            allNamedGenericFunctions.Add (QuoteMakeInstance, makeInstance);

            makeInstancesObsolete.SetClass (StandardGenericFunction);
            makeInstancesObsolete.SetSlots (new object [] {
                "missing documentation string",
                QuoteMakeInstancesObsolete,
                "MakeInstancesObsolete" });
            allNamedGenericFunctions.Add (QuoteMakeInstancesObsolete, makeInstancesObsolete);

            makeLoadForm.SetClass (StandardGenericFunction);
            makeLoadForm.SetSlots (new object [] {
                "missing documentation string",
                QuoteMakeLoadForm,
                "MakeLoadForm" });
            allNamedGenericFunctions.Add (QuoteMakeLoadForm, makeLoadForm);

            makeMethodLambda.SetClass (StandardGenericFunction);
            makeMethodLambda.SetSlots (new object [] {
                "missing documentation string",
                QuoteMakeMethodLambda,
                "MakeMethodLambda" });
            allNamedGenericFunctions.Add (QuoteMakeMethodLambda, makeMethodLambda);

            mapDependents.SetClass (StandardGenericFunction);
            mapDependents.SetSlots (new object [] {
                "missing documentation string",
                QuoteMapDependents,
                "MapDependents" });
            allNamedGenericFunctions.Add (QuoteMapDependents, mapDependents);

            methodCombination.SetClass (StandardGenericFunction);
            methodCombination.SetSlots (new object [] {
                "missing documentation string",
                QuoteMethodCombination,
                "MethodCombination" });
            allNamedGenericFunctions.Add (QuoteMethodCombination, methodCombination);

            bootstrapInitializeStandardGenericFunction (methodFunction,
                                                        "Returns the function of a method.",
                                                        QuoteMethodFunction,
                                                        "MethodFunction",
                                                        CL.List (QuoteMethod),
                                                        null,
                                                        CL.List<ICollection<object>>(null));
            setFuncallableInstanceFunction (methodFunction, bootstrapMethodFunction);

            methodGenericFunction.SetClass (StandardGenericFunction);
            methodGenericFunction.SetSlots (new object [] {
                "missing documentation string",
                QuoteMethodGenericFunction,
                "MethodGenericFunction" });
            allNamedGenericFunctions.Add (QuoteMethodGenericFunction, methodGenericFunction);

            methodLambdaList.SetClass (StandardGenericFunction);
            methodLambdaList.SetSlots (new object [] {
                "missing documentation string",
                QuoteMethodLambdaList,
                "MethodLambdaList" });
            allNamedGenericFunctions.Add (QuoteMethodLambdaList, methodLambdaList);

            bootstrapInitializeStandardGenericFunction (methodSpecializers,
                                                        "Returns the specializers of a method.",
                                                        QuoteMethodSpecializers,
                                                        "MethodSpecializers",
                                                        CL.List (QuoteMethod),
                                                        null,
                                                        CL.List<ICollection<object>>(null));
            setFuncallableInstanceFunction (methodSpecializers, bootstrapMethodSpecializers);

            bootstrapInitializeStandardGenericFunction (methodQualifier,
                                                        "Returns the qualifier of a method.",
                                                        QuoteMethodQualifier,
                                                        "MethodQualifier",
                                                        CL.List (QuoteMethod),
                                                        null,
                                                        CL.List<ICollection<object>>(null));
            setFuncallableInstanceFunction (methodQualifier, bootstrapMethodQualifier);

            noApplicableMethod.SetClass (StandardGenericFunction);
            noApplicableMethod.SetSlots (new object [] {
                "missing documentation string",
                QuoteNoApplicableMethod,
                "NoApplicableMethod" });
            allNamedGenericFunctions.Add (QuoteNoApplicableMethod, noApplicableMethod);

            noNextMethod.SetClass (StandardGenericFunction);
            noNextMethod.SetSlots (new object [] {
                "missing documentation string",
                QuoteNoNextMethod,
                "NoNextMethod" });
            allNamedGenericFunctions.Add (QuoteNoNextMethod, noNextMethod);

            readerMethodClass.SetClass (StandardGenericFunction);
            readerMethodClass.SetSlots (new object [] {
                "missing documentation string",
                QuoteReaderMethodClass,
                "ReaderMethodClass" });
            allNamedGenericFunctions.Add (QuoteReaderMethodClass, readerMethodClass);

            reinitializeInstance.SetClass (StandardGenericFunction);
            reinitializeInstance.SetSlots (new object [] {
                "missing documentation string",
                QuoteReinitializeInstance,
                "ReinitializeInstance" });
            allNamedGenericFunctions.Add (QuoteReinitializeInstance, reinitializeInstance);

            removeDependents.SetClass (StandardGenericFunction);
            removeDependents.SetSlots (new object [] {
                "missing documentation string",
                QuoteRemoveDependents,
                "RemoveDependents" });
            allNamedGenericFunctions.Add (QuoteRemoveDependents, removeDependents);

            removeDirectMethod.SetClass (StandardGenericFunction);
            removeDirectMethod.SetSlots (new object [] {
                "missing documentation string",
                QuoteRemoveDirectMethod,
                "RemoveDirectMethod" });
            allNamedGenericFunctions.Add (QuoteRemoveDirectMethod, removeDirectMethod);

            removeDirectSubclass.SetClass (StandardGenericFunction);
            removeDirectSubclass.SetSlots (new object [] {
                "missing documentation string",
                QuoteRemoveDirectSubclass,
                "RemoveDirectSubclass" });
            allNamedGenericFunctions.Add (QuoteRemoveDirectSubclass, removeDirectSubclass);

            removeMethod.SetClass (StandardGenericFunction);
            removeMethod.SetSlots (new object [] {
                "missing documentation string",
                QuoteRemoveMethod,
                "RemoveMethod" });
            allNamedGenericFunctions.Add (QuoteRemoveMethod, removeMethod);

            sharedInitialize.SetClass (StandardGenericFunction);
            sharedInitialize.SetSlots (new object [] {
                "missing documentation string",
                QuoteSharedInitialize,
                "SharedInitialize" });
            allNamedGenericFunctions.Add (QuoteSharedInitialize, sharedInitialize);

            setClassDirectSlots.SetClass (StandardGenericFunction);
            setClassDirectSlots.SetSlots (new object [] {
                "missing documentation string",
                QuoteSetClassDirectSlots,
                "SetClassDirectSlots" });
            allNamedGenericFunctions.Add (QuoteSetClassDirectSlots, setClassDirectSlots);

            setClassDirectSuperclasses.SetClass (StandardGenericFunction);
            setClassDirectSuperclasses.SetSlots (new object [] {
                "missing documentation string",
                QuoteSetClassDirectSuperclasses,
                "SetClassDirectSuperclasses" });
            allNamedGenericFunctions.Add (QuoteSetClassDirectSuperclasses, setClassDirectSuperclasses);

            setClassName.SetClass (StandardGenericFunction);
            setClassName.SetSlots (new object [] {
                "missing documentation string",
                QuoteSetClassName,
                "SetClassName" });
            allNamedGenericFunctions.Add (QuoteSetClassName, setClassName);

            setClassSlots.SetClass (StandardGenericFunction);
            setClassSlots.SetSlots (new object [] {
                "missing documentation string",
                QuoteSetClassSlots,
                "SetClassSlots" });
            allNamedGenericFunctions.Add (QuoteSetClassSlots, setClassSlots);

            bootstrapInitializeStandardGenericFunction (setGenericFunctionApplicationCache,
                            "Mutates the application cache of a generic function.",
                                QuoteSetGenericFunctionApplicationCache,
                                "SetGenericFunctionApplicationCache",
                                CL.List(QuoteGenericFunction),
                                null,
                                                         CL.List<ICollection<object>>(null,null)) ;
            setFuncallableInstanceFunction (setGenericFunctionApplicationCache, bootstrapSetGenericFunctionApplicationCache);
 

            setGenericFunctionName.SetClass (StandardGenericFunction);
            setGenericFunctionName.SetSlots (new object [] {
                "missing documentation string",
                QuoteSetGenericFunctionName,
                "SetGenericFunctionName" });
            allNamedGenericFunctions.Add (QuoteSetGenericFunctionName, setGenericFunctionName);

            bootstrapInitializeStandardGenericFunction (setSlotValueUsingClass,
                                                        "Implementation of SetSlotValue",
                                                        QuoteSetSlotValueUsingClass,
                                                        "SetSlotValueUsingClass",
                                                        CL.List(QuoteNewValue, QuoteClass,QuoteInstance,QuoteSlotDefinition),
                                                        null,
                                                        CL.List<ICollection<object>>(null,null,null,null)
                                                     );
            setFuncallableInstanceFunction (setSlotValueUsingClass, bootstrapSetSlotValueUsingClass);

            isSlotBoundUsingClass.SetClass (StandardGenericFunction);
            isSlotBoundUsingClass.SetSlots (new object [] {
                "missing documentation string",
                QuoteIsSlotBoundUsingClass,
                "IsSlotBoundUsingClass" });
            allNamedGenericFunctions.Add (QuoteIsSlotBoundUsingClass, isSlotBoundUsingClass);

            slotDefinitionAllocation.SetClass (StandardGenericFunction);
            slotDefinitionAllocation.SetSlots (new object [] {
                "missing documentation string",
                QuoteSlotDefinitionAllocation,
                "SlotDefinitionAllocation" });
            allNamedGenericFunctions.Add (QuoteSlotDefinitionAllocation, slotDefinitionAllocation);

            slotDefinitionInitargs.SetClass (StandardGenericFunction);
            slotDefinitionInitargs.SetSlots (new object [] {
                "missing documentation string",
                QuoteSlotDefinitionInitargs,
                "SlotDefinitionInitargs" });
            allNamedGenericFunctions.Add (QuoteSlotDefinitionInitargs, slotDefinitionInitargs);

            slotDefinitionInitform.SetClass (StandardGenericFunction);
            slotDefinitionInitform.SetSlots (new object [] {
                "missing documentation string",
                QuoteSlotDefinitionInitform,
                "SlotDefinitionInitform" });
            allNamedGenericFunctions.Add (QuoteSlotDefinitionInitform, slotDefinitionInitform);

            slotDefinitionInitfunction.SetClass (StandardGenericFunction);
            slotDefinitionInitfunction.SetSlots (new object [] {
                "missing documentation string",
                QuoteSlotDefinitionInitfunction,
                "SlotDefinitionInitfunction" });
            allNamedGenericFunctions.Add (QuoteSlotDefinitionInitfunction, slotDefinitionInitfunction);

            bootstrapInitializeStandardGenericFunction (slotDefinitionLocation,
                                                        "Returns the locatio of the slot.",
                                                         QuoteSlotDefinitionLocation,
                                                         "SlotDefinitionLocation",
                                                         CL.List(QuoteSlotDefinition),
                                                         null,
                                                        CL.List<ICollection<object>>(null));
            setFuncallableInstanceFunction (slotDefinitionLocation, bootstrapSlotDefinitionLocation);

            bootstrapInitializeStandardGenericFunction (slotDefinitionName,
                                                        "Returns the name of the slot.",
                                                         QuoteSlotDefinitionName,
                                                         "SlotDefinitionName",
                                                         CL.List(QuoteSlotDefinition),
                                                         null,
                                                        CL.List<ICollection<object>>(null));
            setFuncallableInstanceFunction (slotDefinitionName, bootstrapSlotDefinitionName);

            slotDefinitionType.SetClass (StandardGenericFunction);
            slotDefinitionType.SetSlots (new object [] {
                "missing documentation string",
                QuoteSlotDefinitionType,
                "SlotDefinitionType" });
            allNamedGenericFunctions.Add (QuoteSlotDefinitionType, slotDefinitionType);

            slotDefinitionReaders.SetClass (StandardGenericFunction);
            slotDefinitionReaders.SetSlots (new object [] {
                "missing documentation string",
                QuoteSlotDefinitionReaders,
                "SlotDefinitionReaders" });
            allNamedGenericFunctions.Add (QuoteSlotDefinitionReaders, slotDefinitionReaders);

            slotDefinitionWriters.SetClass (StandardGenericFunction);
            slotDefinitionWriters.SetSlots (new object [] {
                "missing documentation string",
                QuoteSlotDefinitionWriters,
                "SlotDefinitionWriters" });
            allNamedGenericFunctions.Add (QuoteSlotDefinitionWriters, slotDefinitionWriters);

            slotMakunboundUsingClass.SetClass (StandardGenericFunction);
            slotMakunboundUsingClass.SetSlots (new object [] {
                "missing documentation string",
                QuoteSlotMakunboundUsingClass,
                "SlotMakunboundUsingClass" });
            allNamedGenericFunctions.Add (QuoteSlotMakunboundUsingClass, slotMakunboundUsingClass);

            slotMissing.SetClass (StandardGenericFunction);
            slotMissing.SetSlots (new object [] {
                "missing documentation string",
                QuoteSlotMissing,
                "SlotMissing" });
            allNamedGenericFunctions.Add (QuoteSlotMissing, slotMissing);

            slotUnbound.SetClass (StandardGenericFunction);
            slotUnbound.SetSlots (new object [] {
                "missing documentation string",
                QuoteSlotUnbound,
                "SlotUnbound" });
            allNamedGenericFunctions.Add (QuoteSlotUnbound, slotUnbound);

            bootstrapInitializeStandardGenericFunction (slotValueUsingClass,
                                                        "Implementation of SlotValue",
                                                        QuoteSlotValueUsingClass,
                                                        "SlotValueUsingClass",
                                                        CL.List(QuoteClass,QuoteInstance,QuoteSlotDefinition),
                                                        null,
                                                        CL.List<ICollection<object>>(null,null,null));
            setFuncallableInstanceFunction (slotValueUsingClass, bootstrapSlotValueUsingClass);

            specializerDirectGenericFunctions.SetClass (StandardGenericFunction);
            specializerDirectGenericFunctions.SetSlots (new object [] {
                "missing documentation string",
                QuoteSpecializerDirectGenericFunctions,
                "SpecializerDirectGenericFunctions" });
            allNamedGenericFunctions.Add (QuoteSpecializerDirectGenericFunctions, specializerDirectGenericFunctions);

            specializerDirectMethods.SetClass (StandardGenericFunction);
            specializerDirectMethods.SetSlots (new object [] {
                "missing documentation string",
                QuoteSpecializerDirectMethods,
                "SpecializerDirectMethods" });
            allNamedGenericFunctions.Add (QuoteSpecializerDirectMethods, specializerDirectMethods);

            standardAccessorMethod.SetClass (StandardGenericFunction);
            standardAccessorMethod.SetSlots (new object [] {
                "missing documentation string",
                QuoteStandardAccessorMethod,
                "StandardAccessorMethod" });
            allNamedGenericFunctions.Add (QuoteStandardAccessorMethod, standardAccessorMethod);

            updateDependent.SetClass (StandardGenericFunction);
            updateDependent.SetSlots (new object [] {
                "missing documentation string",
                QuoteUpdateDependent,
                "UpdateDependent" });
            allNamedGenericFunctions.Add (QuoteUpdateDependent, updateDependent);

            updateInstanceForDifferentClass.SetClass (StandardGenericFunction);
            updateInstanceForDifferentClass.SetSlots (new object [] {
                "missing documentation string",
                QuoteUpdateInstanceForDifferentClass,
                "UpdateInstanceForDifferentClass" });
            allNamedGenericFunctions.Add (QuoteUpdateInstanceForDifferentClass, updateInstanceForDifferentClass);

            updateInstanceForRedefinedClass.SetClass (StandardGenericFunction);
            updateInstanceForRedefinedClass.SetSlots (new object [] {
                "missing documentation string",
                QuoteUpdateInstanceForRedefinedClass,
                "UpdateInstanceForRedefinedClass" });
            allNamedGenericFunctions.Add (QuoteUpdateInstanceForRedefinedClass, updateInstanceForRedefinedClass);

            validateSuperclass.SetClass (StandardGenericFunction);
            validateSuperclass.SetSlots (new object [] {
                "missing documentation string",
                QuoteValidateSuperclass,
                "ValidateSuperclass" });
            allNamedGenericFunctions.Add (QuoteValidateSuperclass, validateSuperclass);

            writerMethodClass.SetClass (StandardGenericFunction);
            writerMethodClass.SetSlots (new object [] {
                "missing documentation string",
                QuoteWriterMethodClass,
                "WriterMethodClass" });
            allNamedGenericFunctions.Add (QuoteWriterMethodClass, writerMethodClass);

            return true;
        }

        static ConsList<StandardObject> classPrecedenceListMethod0Impl (NextMethodFunction callNextMethod, StandardObject closClass)
        {
            return (ConsList<StandardObject>) SlotValue (closClass, QuotePrecedenceList);
        }

        static ConsList<StandardObject> classPrecedenceListMethod1Impl (NextMethodFunction callNextMethod, StandardObject closClass)
        {
            throw new NotImplementedException();
        }

        // closClass is standardClass
        static ICollection<StandardObject> classSlotsMethod0Impl (NextMethodFunction callNextMethod, StandardObject closClass)
        {
            if (closClass != standardClass)
                throw new NotImplementedException ();
            return theEffectiveSlotsOfStandardClass;
        }

        static ConsList<StandardObject> classSlotsMethod1Impl (NextMethodFunction callNextMethod, StandardObject closClass)
        {
            throw new NotImplementedException();
        }

        // closClass is a built-in class
        static ICollection<StandardObject> classSlotsMethod2Impl (NextMethodFunction callNextMethod, StandardObject closClass)
        {
            Type dotnetClass = (Type) SlotValue (closClass, QuoteDotnetType);
            MemberInfo [] dotnetSlots = dotnetClass.GetMembers (BindingFlags.GetField | BindingFlags.GetProperty | BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static);
            ConsList<StandardObject> slots = (ConsList<StandardObject>) theEffectiveSlotsOfBuiltInClass;
            foreach (MemberInfo dotnetSlot in dotnetSlots) {
                if (dotnetSlot.MemberType == MemberTypes.Field) {
                    FieldInfo dotnetField = dotnetSlot as FieldInfo;
                    StandardObject slot = bootstrapCreateBuiltInEffectiveSlot (dotnetField);
                    slots = new ConsList<StandardObject> (slot, slots);
                }
                else if (dotnetSlot.MemberType == MemberTypes.Property) {
                    PropertyInfo dotnetProperty = dotnetSlot as PropertyInfo;
                    StandardObject slot = bootstrapCreateBuiltInEffectiveSlot (dotnetProperty);
                    slots = new ConsList<StandardObject> (slot, slots);
                }
                // otherwise ignore it
            }

            return (ICollection<StandardObject>) slots;
        }

        static ConsList<StandardObject> classSlotsMethod3Impl (NextMethodFunction callNextMethod, StandardObject closClass)
        {
            return (ConsList<StandardObject>) SlotValue (closClass, QuoteSlots);
        }

        static FuncallHandler computeDiscriminatingFunctionMethodImpl (NextMethodFunction callNextMethod, StandardObject genericFunction)
        {
            return StandardDiscriminatingFunction.Create (genericFunction);
        }

        static StandardObject ensureGenericFunctionMethod0Impl (NextMethodFunction callNextMethod, object functionName, params object [] arguments)
        {
           throw new NotImplementedException();
        }

        static StandardObject ensureGenericFunctionMethod1Impl (NextMethodFunction callNextMethod, object functionName, params object [] arguments)
        {
           throw new NotImplementedException();
        }

        //
        static internal ConsList<StandardObject> InternalClassPrecedenceList (StandardObject closClass)
        {
            return 
                 (closClass.Class() == builtInClass)  ? (ConsList<StandardObject>) StandardInstanceAccess (closClass, 6)
               : (closClass.Class() == standardClass) ? (ConsList<StandardObject>) StandardInstanceAccess (closClass, 6)
               : (ConsList<StandardObject>) classPrecedenceList (closClass);
        }

        static internal Cons InternalGenericFunctionApplicationCache (StandardObject genericFunction)
        {
            return (genericFunction.Class() == standardGenericFunction)
                ? (Cons) StandardInstanceAccess (genericFunction, 3)
                : (Cons) genericFunctionApplicationCache (genericFunction);
        }

        static internal Cons InternalSetGenericFunctionApplicationCache (StandardObject genericFunction, object newValue)
        {
            return (genericFunction.Class() == standardGenericFunction)
                ? (Cons) SetStandardInstanceAccess (genericFunction, 3, newValue)
                : (Cons) setGenericFunctionApplicationCache (genericFunction, newValue);
        }

        static internal Cons InternalGenericFunctionLambdaList (StandardObject genericFunction)
        {
            return (genericFunction.Class() == standardGenericFunction)
                ? (Cons) StandardInstanceAccess (genericFunction, 4)
                : (Cons) GenericFunctionLambdaList (genericFunction);
        }

        static internal Cons InternalGenericFunctionMethods (StandardObject genericFunction)
        {
            return (genericFunction.Class() == standardGenericFunction)
                ? (Cons) StandardInstanceAccess (genericFunction, 5)
                : (Cons) GenericFunctionMethods (genericFunction);
        }

        static internal ConsList<ICollection<object>> InternalGenericFunctionSingletonsList (StandardObject genericFunction)
        {
            return (genericFunction.Class() == standardGenericFunction)
                ? (ConsList<ICollection<object>>) StandardInstanceAccess (genericFunction, 6)
                : (ConsList<ICollection<object>>) genericFunctionSingletonsList (genericFunction);
        }

        static internal Delegate InternalMethodFunction (StandardObject method)
        {
            return (method.Class() == standardMethod)
               ? (Delegate) StandardInstanceAccess (method, 3)
               : (Delegate) methodFunction (method);
        }

        static internal Symbol InternalMethodQualifier (StandardObject method)
        {
            return (method.Class() == standardMethod)
               ? (Symbol) StandardInstanceAccess (method, 4)
               : (Symbol) methodQualifier (method);
        }

        static internal Cons InternalMethodSpecializers (StandardObject method)
        {
            return (method.Class() == standardMethod)
               ? (Cons) StandardInstanceAccess (method, 5)
               : (Cons) methodSpecializers (method);
        }

        static StandardObject lookupSlot (StandardObject closClass, Symbol slotName)
        {
            ICollection<StandardObject> slots = 
                (closClass == builtInClass) ? theEffectiveSlotsOfBuiltInClass 
                : (closClass == standardClass) ? theEffectiveSlotsOfStandardClass
                : (closClass == standardGenericFunction) ? theEffectiveSlotsOfStandardGenericFunction
                : (closClass == standardMethod) ? theEffectiveSlotsOfStandardMethod
                : (ICollection<StandardObject>) ClassSlots (closClass);

            if (slots != null)
                foreach (StandardObject effectiveSlot in slots) {
                   Symbol name =
                       (effectiveSlot.Class() == standardEffectiveSlotDefinition)
                       ? (Symbol) StandardInstanceAccess (effectiveSlot, 1)
                       : (Symbol) SlotValue (effectiveSlot, QuoteName);
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

        public static object [] Subvector (object [] src, int start, int limit)
        {
            object [] answer = new object [limit - start];
            Array.Copy (src, start, answer, 0, limit - start);
            return answer;
        }

        public static string StandardObjectName (StandardObject so)
        {
            return (string) so.InstanceRef (2);
        }

        public static bool StopHere ()
        {
            Debug.Assert (false);
            return true;
        }
    }
}

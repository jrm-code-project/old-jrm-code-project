using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Reflection;

namespace Lisp
{
    public delegate object NextMethodFunction (params object [] arguments);

    public class MethodCache
    {
        Cons table;

        static bool Equal (Cons left, Cons right) {
            if ((left == null) && (right == null))
                return true;
            if (left == null)
                return false;
            if (right == null)
                return false;
            if (left.Car.Equals(right.Car))
                return Equal ((Cons) left.Cdr, (Cons) right.Cdr);
            else
                return false;
        }

        static Cons Assoc (Cons key, Cons table) {
            if (table == null)
                return null;
            Cons entry = (Cons) table.Car;
            if (Equal (key, (Cons) entry.Car))
                return entry;
            else
        {

                return Assoc (key, (Cons) table.Cdr);
        }
        }

        static Cons standinForNull = new Cons (null, null);
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


    public delegate object MethodStep (NextMethodFunction callNextMethod, params object [] arguments);

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

    class ClassInitializer
    {
        public ClassInitializer ()
        {
        }

        public void Initialize (StandardObject instance, Cons initargs)
        {
            throw new NotImplementedException ();
        }
    }

    delegate object SlotInitFunction (Cons initargs);

    class SlotInitializer
    {
        SlotInitFunction init;
        public SlotInitializer (SlotInitFunction init)
        {
            this.init = init;
        }

        public void Initialize (StandardObject instance, int slot, Cons initargs)
        {
            ((ManifestInstance) instance.Target).Slots [slot] = init(initargs);
        }
    }

    public delegate object SlotReader (StandardObject obj);
    delegate T SlotReader<T> (StandardObject obj);

    public static class CLOS
    {
        static object UnboundSlotValue = UnboundSlot.Value;

        public static object Init ()
        {
            return UnboundSlotValue;
        }

        static StandardObject Class (this StandardObject obj)
        {
            return ((ManifestInstance) obj.Target).Class;
        }

        // Extension methods for StandardObject
        static object InstanceRef (this StandardObject obj, int index)
        {
            object result = ((ManifestInstance) obj.Target).Slots [index];
            if (result == UnboundSlotValue)
                throw new NotImplementedException ("UnboundSlot");
            return result;
        }

        static T InstanceRef<T> (this StandardObject obj, int index)
        {
            return (T) ((ManifestInstance) obj.Target).Slots [index];
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

        static int SerialNumber (this StandardObject obj)
        {
            return ((ManifestInstance) obj.Target).SerialNumber;
        }

        //// End of extension methods

        // Not an extension, but Instance-specific.
        static void setFuncallableInstanceFunction (StandardObject funcallableInstance, FuncallHandler handler)
        {
            ((ManifestInstance) funcallableInstance.Target).OnFuncall = handler;
        }

        static readonly Package closPackage = Package.Clos;

        #region Symbols
        static readonly Symbol QuoteAndAllowOtherKeys = Package.CommonLisp.FindSymbol ("&ALLOW-OTHER-KEYS");
        static readonly Symbol QuoteAndKey = Package.CommonLisp.FindSymbol ("&KEY");
        static readonly Symbol QuoteAndRest = Package.CommonLisp.FindSymbol ("&REST");
        static readonly Symbol QuoteAnonymous = Package.CommonLisp.FindSymbol ("[unnamed]");
        static readonly Symbol QuoteAddMethod = Package.CommonLisp.FindSymbol ("ADD-METHOD");
        static readonly Symbol QuoteAllocateInstance = Package.CommonLisp.FindSymbol ("ALLOCATE-INSTANCE");
        static readonly Symbol QuoteApplicationCache = closSymbol ("APPLICATION-CACHE");
        static readonly Symbol QuoteArguments = closSymbol ("ARGUMENTS");
        static readonly Symbol QuoteArgumentPrecedenceOrder = closSymbol ("ARGUMENT-PRECEDENCE-ORDER");
        static readonly Symbol QuoteArity = closSymbol ("ARITY");
        static readonly Symbol QuoteBuiltInClass = closSymbol ("BUILT-IN-CLASS");
        static readonly Symbol QuoteClassPrecedenceList = closSymbol ("CLASS-PRECEDENCE-LIST");
        static readonly Symbol QuoteClassPrototype = closSymbol ("CLASS-PROTOTYPE");

        static readonly Symbol QuoteClass = closSymbol ("CLASS");
        static readonly Symbol QuoteClassInitializers = closSymbol ("CLASS-INITIALIZERS");
        static readonly Symbol QuoteComputeApplicableMethods = closSymbol ("COMPUTE-APPLICABLE-METHODS");
        static readonly Symbol QuoteComputeClassPrecedenceList = closSymbol ("COMPUTE-CLASS-PRECEDENCE-LIST");
        static readonly Symbol QuoteComputeDiscriminatingFunction = closSymbol ("COMPUTE-DISCRIMINATING-FUNCTION");
        static readonly Symbol QuoteComputeEffectiveMethod = closSymbol ("COMPUTE-EFFECTIVE-METHOD");
        static readonly Symbol QuoteComputeMethodMoreSpecific = closSymbol ("COMPUTE-METHOD-MORE-SPECIFIC");
        static readonly Symbol QuoteComputeSlots = closSymbol ("COMPUTE-SLOTS");
        static readonly Symbol QuoteDeclarations = closSymbol ("DECLARATIONS");
        static readonly Symbol QuoteDefaultInitargs = closSymbol ("DEFAULT-INITARGS");
        static readonly Symbol QuoteDirectDefaultInitargs = closSymbol ("DIRECT-DEFAULT-INITARGS");
        static readonly Symbol QuoteDirectSlots = closSymbol ("DIRECT-SLOTS");
        static readonly Symbol QuoteDirectSubclasses = closSymbol ("DIRECT-SUBCLASSES");
        static readonly Symbol QuoteDirectSuperclasses = closSymbol ("DIRECT-SUPERCLASSES");
        static readonly Symbol QuoteDocumentation = closSymbol ("DOCUMENTATION");
        static readonly Symbol QuoteDotnetType = closSymbol ("DOTNET-TYPE");
        static readonly Symbol QuoteEffectiveSlots = closSymbol ("EFFECTIVE-SLOTS");
        static readonly Symbol QuoteEnsureGenericFunction = Package.CommonLisp.FindSymbol ("ENSURE-GENERIC-FUNCTION");
        static readonly Symbol QuoteFinalizedP = closSymbol ("FINALIZEDP");
        static readonly Symbol QuoteFuncallableStandardClass = closSymbol ("FUNCALLABLE-STANDARD-CLASS");
        static readonly Symbol QuoteFuncallableStandardObject = closSymbol ("FUNCALLABLE-STANDARD-OBJECT");
        static readonly Symbol QuoteFunction = closSymbol ("FUNCTION");
        static readonly Symbol QuoteFunctionName = closSymbol ("FUNCTION-NAME");
        static readonly Symbol QuoteGenericFunction = closSymbol ("GENERIC-FUNCTION");
        static readonly Symbol QuoteGenericFunctionClass = closSymbol ("GENERIC-FUNCTION-CLASS");
        static readonly Symbol QuoteGenericFunctionMethodClass = closSymbol ("GENERIC-FUNCTION-METHOD-CLASS");
        static readonly Symbol QuoteGettersAndSetters = closSymbol ("GETTERS-AND-SETTERS");
        static readonly Symbol QuoteInstance = closSymbol ("INSTANCE");
        static readonly Symbol QuoteInitargs = closSymbol ("INITARGS");
        static readonly Symbol QuoteInitializers = closSymbol ("INITIALIZERS");
        static readonly Symbol QuoteInitializeInstance = closSymbol ("INITIALIZE-INSTANCE");
        static readonly Symbol QuoteLambdaList = closSymbol ("LAMBDA-LIST");
        static readonly Symbol QuoteList = Package.CommonLisp.FindSymbol ("LIST");
        static readonly Symbol QuoteLocation = closSymbol ("LOCATION");
        static readonly Symbol QuoteMakeInstance = Package.CommonLisp.FindSymbol ("MAKE-INSTANCE");
        static readonly Symbol QuoteMetaobject = closSymbol ("METAOBJECT");
        static readonly Symbol QuoteMethod = closSymbol ("METHOD");
        static readonly Symbol QuoteMethodAllocateInstance = closSymbol ("METHOD:allocate-instance");
        static readonly Symbol QuoteMethodDefaultInitargs = closSymbol ("METHOD:default-initargs");
        static readonly Symbol QuoteMethodComputeApplicableMethods = closSymbol ("METHOD:compute-applicable-methods");
        static readonly Symbol QuoteMethodComputeDiscriminatingFunction = closSymbol ("METHOD:compute-discriminating-function");
        static readonly Symbol QuoteMethodComputeEffectiveMethod = closSymbol ("METHOD:compute-effective-method");
        static readonly Symbol QuoteMethodComputeMethodMoreSpecific = closSymbol ("METHOD:compute-method-more-specific");
        static readonly Symbol QuoteMethods = closSymbol ("METHODS");
        static readonly Symbol QuoteMethodClass = closSymbol ("METHOD-CLASS");
        static readonly Symbol QuoteMethodCombination = closSymbol ("METHOD-COMBINATION");
        static readonly Symbol QuoteMethodEnsureGenericFunction = closSymbol ("METHOD:ensure-generic-function");
        static readonly Symbol QuoteMethodInitializeInstance = closSymbol ("METHOD:initialize-instance");
        static readonly Symbol QuoteMethodMethodQualifier = closSymbol ("METHOD:method-qualifier");
        static readonly Symbol QuoteMethodMethodSpecializers = closSymbol ("METHOD:method-specializers");
        static readonly Symbol QuoteMethodQualifier = closSymbol ("METHOD-QUALIFIER");
        static readonly Symbol QuoteMethodSpecializers = closSymbol ("METHOD-SPECIALIZERS");
        static readonly Symbol QuoteName = closSymbol ("NAME");
        static readonly Symbol QuoteNewMakeInstance = closSymbol ("NEW-MAKE-INSTANCE");
        static readonly Symbol QuoteNewMakeInstanceMethod = closSymbol ("NEW-MAKE-INSTANCE-METHOD");
        static readonly Symbol QuoteProcedure = closSymbol ("PROCEDURE");
        static readonly Symbol QuoteQualifier = closSymbol ("QUALIFIER");
        static readonly Symbol QuoteSingletonsList = closSymbol ("SINGLETONS-LIST");
        static readonly Symbol QuoteSlotInitializers = closSymbol ("SLOT-INITIALIZERS");
        static readonly Symbol QuoteSlotNames = closSymbol ("SLOT-NAMES");
        static readonly Symbol QuoteSpecializer = closSymbol ("SPECIALIZER");
        static readonly Symbol QuoteSpecializers = closSymbol ("SPECIALIZERS");
        static readonly Symbol QuoteStandardAccessorMethod = closSymbol ("STANDARD-ACCESSOR-METHOD");
        static readonly Symbol QuoteStandardReaderMethod = closSymbol ("STANDARD-READER-METHOD");
        static readonly Symbol QuoteStandardWriterMethod = closSymbol ("STANDARD-WRITER-METHOD");
        static readonly Symbol QuoteStandardClass = closSymbol ("STANDARD-CLASS");   
        static readonly Symbol QuoteStandardGenericFunction = closSymbol ("STANDARD-GENERIC-FUNCTION");
        static readonly Symbol QuoteStandardMethod = Package.CommonLisp.FindSymbol ("STANDARD-METHOD");
        static readonly Symbol QuoteStandardObject = closSymbol ("STANDARD-OBJECT");
        static readonly Symbol QuoteStudlyName = closSymbol ("StudlyName");
        static readonly Symbol QuoteSuperclass = closSymbol ("SUPERCLASS");
        static readonly Symbol QuoteSuppliedInitargs = closSymbol ("SUPPLIED-INITARGS");
        static readonly Symbol QuoteTop = closSymbol ("TOP");
        static readonly Symbol QuoteValidateSuperclass = closSymbol ("VALIDATE-SUPERCLASS");
        #endregion Symbols


        delegate object SlotWriter (StandardObject obj, object newValue);
        delegate T SlotWriter<T> (StandardObject obj, T newValue);
        delegate object SlotChangingFunction (object currentValue);
        delegate object SlotChanger (StandardObject obj, SlotChangingFunction func);

        delegate T SlotChangingFunction<T> (T currentValue);
        delegate T SlotChanger<T> (StandardObject obj, SlotChangingFunction<T> func);

                                   

        // ClassOf
        delegate StandardObject ClassOfFunction (object obj);

        static ClassOfFunction classOf =
            new ClassOfFunction (delegate (object instance)
        {
            StandardObject probe = instance as StandardObject;
            return (probe == null) ? Top : probe.Class();
        });

        static ClassOfFunction ClassOf
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return classOf;
            }
        }

        delegate object amf (Object obj);
        static ConsList<T> AppendMap<T> (amf del, Cons list)
        {
            ConsList<T> answer = null;
            while (true) {
                if (list == null)
                    break;
                answer =  CL.Append<T> ((ConsList<T>) del (list.Car), answer);
                list = (Cons) list.Cdr;
            }

            return answer;

        }

        static Cons bootstrapComputeClassPrecedenceList (Cons superclasses, Cons soFar)
        {
            if (superclasses == null)
                return (Cons) CL.Reverse (soFar);
            return bootstrapComputeClassPrecedenceList ((Cons) CL.Append (superclasses.Cdr, InternalClassDirectSuperclasses ((StandardObject) superclasses.Car)),
                                                        (CL.Memq (superclasses.Car, soFar) == null)
                                                        ? CL.Cons (superclasses.Car, soFar)
                                                            : soFar);
        }

        delegate Cons Allocator (StandardObject initargs);

        static object unspecificInitializer ()
        {
            throw new NotImplementedException ();
        }

        static SlotInitFunction MakeSlotInitFunction (StandardObject slotSpec)
        {
            object initvalue = UnboundSlotValue;
            ConsList<Keyword> initkeys = null;

            //Cons tail = (Cons) slotSpec.Cdr;
            //while (true) {
            //    if (tail == null)
            //        break;

            //    if (tail.Car == KW.Initvalue) {
            //        initvalue = CL.Cadr (tail);
            //    }
            //    else if (tail.Car == KW.Initarg) {
            //        initkeys = new ConsList<Keyword> ((Keyword) CL.Cadr (tail), initkeys);
            //    }

            //    tail = (Cons) tail.Cdr;
            //    tail = (Cons) tail.Cdr;
            //}
            return new SlotInitFunction(
                delegate (Cons initargList) {
                    while (true) {
                        if (initargList == null)
                            break;
                        if (initkeys != null)
                        foreach (Keyword initkey in initkeys) {
                            if (initkey == initargList.Car)
                                return CL.Cadr (initargList);
                        }
                        initargList = (Cons) CL.Cdr (initargList);
                        initargList = (Cons) CL.Cdr (initargList);
                    }
                    return initvalue;
                });

 
         }

        static StandardObject bootstrapProcessDirectSlot (object directSlotSpecs)
        {
            return (StandardObject) CL.Apply (MakeInstance, StandardDirectSlotDefinition, ((Cons) directSlotSpecs));
        }

        static StandardObject bootstrapProcessEffectiveSlot (object directSlot)
        {
            return (StandardObject) CL.MakeInstance (StandardEffectiveSlotDefinition, directSlot);
        }

        static ConsList<StandardObject> bootstrapComputeEffectiveSlots (ConsList<StandardObject> effectiveSlots)
        {
            int index = 0;
            if (effectiveSlots != null) {
                foreach (StandardObject effectiveSlot in effectiveSlots) {
                    internalSetSlotDefinitionLocation (effectiveSlot, index);
                    index += 1;
                }
            }
            return effectiveSlots;
        }

        static object bootstrapMakeInstance (StandardObject self, object [] arguments)
        {
            StandardObject closClass = (StandardObject) arguments [0];
            Cons initargs = Cons.SubvectorToList (arguments, 1, arguments.Length);
            if (object.ReferenceEquals (closClass, StandardDirectSlotDefinition)) {
                StandardObject instance = ManifestInstance.CreateInstance (closClass, CL.Length (SLOTS_OF_STANDARD_DIRECT_SLOT_DEFINITION));
                internalSetSlotDefinitionName (instance, (Symbol) initargs.Car);
                return instance;
            }
            else if (object.ReferenceEquals (closClass, StandardEffectiveSlotDefinition)) {
                StandardObject direct = (StandardObject) arguments [1];
                StandardObject instance = ManifestInstance.CreateInstance (closClass, CL.Length (SLOTS_OF_STANDARD_EFFECTIVE_SLOT_DEFINITION));
                internalSetSlotDefinitionName (instance, internalSlotDefinitionName (direct));
                return instance;
            }
            else if (object.ReferenceEquals (closClass, StandardClass)
                || object.ReferenceEquals (closClass, FuncallableStandardClass)) {
                StandardObject instance = ManifestInstance.CreateInstance (closClass, CL.Length (SLOTS_OF_STANDARD_CLASS));

                Cons directSlotSpecs = (Cons) Utility.GetArg (initargs, KW.DirectSlots, null);
                ConsList<StandardObject> directSlots = (ConsList<StandardObject>) CL.Map<Cons,StandardObject> (QuoteList, new MapFunction<Cons,StandardObject> (bootstrapProcessDirectSlot), directSlotSpecs);
                Cons directSuperclasses = (Cons) Utility.GetArg (initargs, KW.DirectSuperclasses, null);
                Cons classPrecedenceList = bootstrapComputeClassPrecedenceList (directSuperclasses, CL.List (instance));
                ConsList<StandardObject> inheritedSlots = AppendMap<StandardObject> (new amf (delegate (object superclass)
                {
                    return internalClassDirectSlots ((StandardObject) superclass);
                }), (Cons) CL.Cdr (classPrecedenceList));
                ConsList<StandardObject> effectiveSlots =
                    bootstrapComputeEffectiveSlots ((ConsList<StandardObject>) CL.Map<StandardObject,StandardObject> (QuoteList, new MapFunction<StandardObject, StandardObject> (bootstrapProcessEffectiveSlot), CL.Append<StandardObject> (directSlots, inheritedSlots)));
                // Put the slot descriptors in place first so that the rest of
                // accessors work.
                InstanceSet<ConsList<StandardObject>> (instance, effectiveSlotSlot, effectiveSlots);
                // internalSetClassEffectiveSlots (instance, effectiveSlots);

                // Install the name second so debugging is easier.
                internalSetClassName (instance, (Symbol) Utility.GetArg (initargs, KW.Name, QuoteAnonymous));
                internalSetClassStudlyName (instance, (string) Utility.GetArg (initargs, KW.StudlyName, "AnonymousClass"));



                Cons directDefaultInitargs = (Cons) Utility.GetArg (initargs, KW.DirectDefaultInitargs, null);

                int nfields = 0;
                ConsList<SlotInitializer> reverseSlotInitializers = null;



                Delegate allocator = new Allocator (delegate (StandardObject init)
                {
                    int f = nfields;
                    nfields += 1;
                    SlotInitializer slotInit = new SlotInitializer (MakeSlotInitFunction (init));
                    reverseSlotInitializers = new ConsList<SlotInitializer> (slotInit, reverseSlotInitializers);
                    return CL.List (
                         new SlotReader (delegate (StandardObject o)
                             {
                                 return StandardInstanceAccess (o, f);
                             }),
                             new SlotWriter (delegate (StandardObject o, object newValue)
                             {
                                 return SetStandardInstanceAccess (o, f, newValue);
                             }),
                             new SlotChanger (delegate (StandardObject o, SlotChangingFunction modify)
                             {
                                 return StandardInstanceChange (o, f, modify);
                             }));
                });



                ConsList<Cons> gettersAndSetters =
                  (ConsList<Cons>) CL.Map<StandardObject,Cons> (QuoteList, new MapFunction<StandardObject, Cons> (
                                      delegate (StandardObject s)
                                      {
                                          return CL.Cons (internalSlotDefinitionName(s), allocator.DynamicInvoke (s));
                                      }), effectiveSlots);
                if (CL.Length (effectiveSlots) < CL.Length (directSlots))
                    throw new NotImplementedException ();
                if (CL.Length (effectiveSlots) != CL.Length (gettersAndSetters))
                    throw new NotImplementedException ();
                internalSetClassDefaultInitargs (instance, null); // don't install yet
                internalSetClassDirectDefaultInitargs (instance, directDefaultInitargs);
                internalSetClassDirectSlots (instance, directSlots);
                internalSetClassDirectSubclasses (instance, null);
                internalSetClassDirectSuperclasses (instance, directSuperclasses);
                internalSetClassInitializers (instance, null);
                internalSetClassIsFinalized (instance, true);


                ConsList<SlotInitializer> slotInitializers = null;
                if (reverseSlotInitializers != null)
                    foreach (SlotInitializer si in reverseSlotInitializers)
                        slotInitializers = new ConsList<SlotInitializer> (si, slotInitializers);

                internalSetClassSlotInitializers (instance, slotInitializers);

                internalSetClassPrecedenceList (instance, classPrecedenceList);
                internalSetClassPrototype (instance, null);
                internalSetClassGettersAndSetters (instance, gettersAndSetters);

                internalSetClassInitializers (instance, null);
                return instance;
            }
            else if (object.ReferenceEquals (closClass, StandardGenericFunction)) {
                Cons sentinel = new Cons (null, null);
                Cons lambdaList = (Cons) Utility.GetArg (initargs, KW.LambdaList, sentinel);
                if (object.ReferenceEquals (sentinel, lambdaList))
                    throw new NotImplementedException ("missing lambda list");
                StandardObject answer = ManifestInstance.CreateFuncallableInstance (closClass, CL.Length (internalClassDirectSlots (closClass)));
                internalSetGenericArity (answer, new Arity (lambdaList));
                internalSetGenericApplicationCache (answer, new Cons (GenericApplicationCacheTag, new MethodCache ()));
                internalSetGenericMethodClass (answer, Utility.GetArg (initargs, KW.MethodClass, StandardMethod));
                internalSetGenericMethods (answer, null);
                internalSetGenericName (answer, Utility.GetArg (initargs, KW.Name, QuoteAnonymous));
                internalSetGenericStudlyName (answer, Utility.GetArg (initargs, KW.StudlyName, "AnonymousGeneric"));
                internalSetGenericLambdaList (answer, lambdaList);
                internalSetGenericSingletonsList (answer, null);
                return answer;
            }
            else if (object.ReferenceEquals (closClass, StandardMethod)
                || object.ReferenceEquals (closClass, StandardReaderMethod)) {
                StandardObject answer = ManifestInstance.CreateFuncallableInstance (closClass, CL.Length (internalClassEffectiveSlots (closClass)));
                internalSetMethodArity (answer, new Arity ((Delegate) Utility.GetArg (initargs, KW.Procedure, null)));
                internalSetMethodName (answer, Utility.GetArg (initargs, KW.Name, QuoteAnonymous));
                internalSetMethodStudlyName (answer, Utility.GetArg (initargs, KW.StudlyName, "AnonymousMethod"));
                InternalSetMethodProcedure (answer, (Delegate) Utility.GetArg (initargs, KW.Procedure, null));
                internalSetMethodQualifier (answer, Utility.GetArg (initargs, KW.Qualifier, KW.Primary));
                internalSetMethodSpecializers (answer, Utility.GetArg (initargs, KW.Specializers, null));
                return answer;
            }
            else

                throw new NotImplementedException ("something else");
        }

        static readonly StandardObject makeInstance = ManifestInstance.CreateFuncallableInstance (null, 0, bootstrapMakeInstance);

        static object bootstrapClassName (StandardObject className, object [] arguments)
        {
            return InternalClassName ((StandardObject) arguments [0]);
        }

        static object bootstrapGenericFunctionName (StandardObject gfn, object [] arguments)
        {
            return InternalGenericName ((StandardObject) arguments [0]);
        }        


        static readonly StandardObject className = ManifestInstance.CreateFuncallableInstance (null, 0, bootstrapClassName);
        static readonly StandardObject genericFunctionName = ManifestInstance.CreateFuncallableInstance (null, 0, bootstrapGenericFunctionName);

        static StandardObject lookupSlotInfo (StandardObject closClass, Symbol slotName)
        {
            Symbol className = null;
            object [] classSlots;
            ConsList<StandardObject> sng =  internalClassEffectiveSlots (closClass);
            StandardObject probe = null;
            foreach (StandardObject entry in sng) {
                if (internalSlotDefinitionName(entry) == slotName) {
                    probe = entry;
                    break;
                }
            }
            if (probe == null) {
                className =  InternalClassName (closClass);
                classSlots = ((ManifestInstance) closClass.Target).Slots;
                throw new NotImplementedException ("Slot not found");
            }
            return probe;
        }

        static object internalSlotRef (StandardObject obj, Symbol slotName)
        {
            return InstanceRef (obj, internalSlotDefinitionLocation (lookupSlotInfo (ClassOf (obj), slotName)));
        }

        static T internalSlotRef<T> (StandardObject obj, Symbol slotName)
        {
            return InstanceRef<T> (obj, internalSlotDefinitionLocation (lookupSlotInfo (ClassOf (obj), slotName)));
        }

        static internal object StandardObjectName (StandardObject obj)
        {
            try {
                // Try getting the StudlyName slot, if it exists.
                return internalSlotRef (obj, QuoteStudlyName);
            }
            catch (Exception) {
                try {
                    // Try getting the name slot, if it exists.
                    return internalSlotRef (obj, QuoteName);
                }
                catch (Exception) {

                    return null;
                }
            }
        }

        static object internalSlotSet (StandardObject obj, Symbol slotName, object value)
        {
            return InstanceSet (obj, internalSlotDefinitionLocation(lookupSlotInfo (ClassOf (obj), slotName)), value);
        }
                
        static T internalSlotSet<T> (StandardObject obj, Symbol slotName, T value)
        {
            return (T) InstanceSet (obj, internalSlotDefinitionLocation(lookupSlotInfo (ClassOf (obj), slotName)), value);
        }

        static object internalSlotChange (StandardObject obj, Symbol slotName, SlotChangingFunction value)
        {
            return InstanceChange (obj, internalSlotDefinitionLocation (lookupSlotInfo (ClassOf (obj), slotName)), value);
        }

        static T internalSlotChange<T> (StandardObject obj, Symbol slotName, SlotChangingFunction<T> change)
        {
            return (T) ((SlotChanger) (CL.Cadddr (lookupSlotInfo (ClassOf (obj), slotName)))) (obj, (oldValue) => change((T) oldValue));
        }

        #region InternalClassAccessors

        static StandardObject GuaranteeClass (StandardObject putativeClass)
        {
//   if (! IsInstanceOf (putativeClass, closClass))
//                throw new NotImplementedException ("GuaranteeClass");
            return putativeClass;
        }

        static SlotWriter internalSetClassDefaultInitargs =
          new SlotWriter (delegate (StandardObject obj, object newValue)
        {
            return internalSlotSet (obj, QuoteDefaultInitargs, newValue);
        });

        static SlotWriter internalSetClassDirectDefaultInitargs =
          new SlotWriter (delegate (StandardObject obj, object newValue)
        {
            return internalSlotSet (obj, QuoteDirectDefaultInitargs, newValue);
        });

        static SlotReader<ConsList<StandardObject>> internalClassDirectSlots =
          new SlotReader<ConsList<StandardObject>> (delegate (StandardObject obj)
        {
            return internalSlotRef<ConsList<StandardObject>> (obj, QuoteDirectSlots);
        });

        static SlotWriter<ConsList<StandardObject>> internalSetClassDirectSlots =
          new SlotWriter<ConsList<StandardObject>> (delegate (StandardObject obj, ConsList<StandardObject> newValue)
        {
            return internalSlotSet<ConsList<StandardObject>> (obj, QuoteDirectSlots, newValue);
        });

        static SlotWriter internalSetClassDirectSubclasses =
          new SlotWriter (delegate (StandardObject obj, object newValue)
        {
            return internalSlotSet (obj, QuoteDirectSubclasses, newValue);
        });

        static SlotReader<Cons> internalClassDirectSuperclasses =
          new SlotReader<Cons> (delegate (StandardObject obj)
        {
            return internalSlotRef<Cons> (obj, QuoteDirectSuperclasses);
        });

        static SlotWriter internalSetClassDirectSuperclasses =
          new SlotWriter (delegate (StandardObject obj, object newValue)
        {
            return internalSlotSet (obj, QuoteDirectSuperclasses, newValue);
        });

        static Cons InternalClassDirectSuperclasses (StandardObject obj)
        {
            return (Cons) internalClassDirectSuperclasses (GuaranteeClass (obj));
        }

        static SlotWriter internalSetClassIsFinalized =
          new SlotWriter (delegate (StandardObject obj, object newValue)
        {
            return internalSlotSet (obj, QuoteFinalizedP, newValue);
        });

        static public SlotReader internalClassName =
          new SlotReader (delegate (StandardObject obj)
        {
            return internalSlotRef (obj, QuoteName);
        });

        static Symbol InternalClassName (StandardObject obj)
        {
            return (Symbol) internalClassName (GuaranteeClass (obj));
        }

        static SlotWriter internalSetClassName =
          new SlotWriter (delegate (StandardObject obj, object newValue)
        {
            return internalSlotSet (obj, QuoteName, newValue);
        });

        static SlotWriter internalSetClassStudlyName =
          new SlotWriter (delegate (StandardObject obj, object newValue)
        {
            return internalSlotSet (obj, QuoteStudlyName, newValue);
        });

        static SlotReader<ConsList<StandardObject>> internalClassPrecedenceList =
          new SlotReader<ConsList<StandardObject>> (delegate (StandardObject obj)
        {
            return internalSlotRef<ConsList<StandardObject>> (obj, QuoteClassPrecedenceList);
        });

        static SlotWriter internalSetClassPrecedenceList =
          new SlotWriter (delegate (StandardObject obj, object newValue)
        {
            return internalSlotSet (obj, QuoteClassPrecedenceList, newValue);
        });

        static SlotWriter internalSetClassPrototype =
          new SlotWriter (delegate (StandardObject obj, object newValue)
        {
            return internalSlotSet (obj, QuoteClassPrototype, newValue);
            throw new NotImplementedException ();
        });

        static SlotWriter internalSetClassGettersAndSetters =
          new SlotWriter (delegate (StandardObject obj, object newValue)
        {
            //object [] slots = obj.Slots;
            return internalSlotSet (obj, QuoteGettersAndSetters, newValue);
        });

  

        static SlotWriter<ConsList<StandardObject>> internalSetClassEffectiveSlots =
          new SlotWriter<ConsList<StandardObject>> (delegate (StandardObject obj, ConsList<StandardObject> newValue)
        {
            return internalSlotSet<ConsList<StandardObject>> (obj, QuoteEffectiveSlots, newValue);
        });

        static SlotReader<ConsList<ClassInitializer>> internalClassInitializers =
            new SlotReader<ConsList<ClassInitializer>> (delegate (StandardObject obj)
            {
                return internalSlotRef<ConsList<ClassInitializer>> (obj, QuoteInitializers);
            });

        static SlotWriter internalSetClassInitializers =
            new SlotWriter (delegate (StandardObject obj, object newValue)
            {
                return internalSlotSet (obj, QuoteInitializers, newValue);
            });

        static SlotReader<ConsList<SlotInitializer>> internalClassSlotInitializers =
            new SlotReader<ConsList<SlotInitializer>> (delegate (StandardObject obj)
                {
                    return internalSlotRef<ConsList<SlotInitializer>> (obj, QuoteSlotInitializers);
                });

        static SlotWriter internalSetClassSlotInitializers =
            new SlotWriter (delegate (StandardObject obj, object newValue)
                {
                    return internalSlotSet (obj, QuoteSlotInitializers, newValue);
                });

        #endregion InternalClassAccessors

        #region InternalGenericAccessors

        static StandardObject GuaranteeGeneric (StandardObject putativeGeneric)
        {
            if (putativeGeneric.Class() != StandardGenericFunction)
                throw new NotImplementedException ("GuaranteeGeneric");
            return putativeGeneric;
        }

        static SlotReader<Arity> internalGenericArity =
            new SlotReader<Arity> (delegate (StandardObject generic)
                {
                    return internalSlotRef<Arity> (generic, QuoteArity);
                });

        static SlotWriter<Arity> internalSetGenericArity =
            new SlotWriter<Arity> (delegate (StandardObject generic, Arity rt)
                {
                    return internalSlotSet<Arity> (generic, QuoteArity, rt);
                });

        static SlotReader internalGenericApplicationCache =
            new SlotReader (delegate (StandardObject generic)
        {
            return internalSlotRef (generic, QuoteApplicationCache);
        });

        static SlotWriter internalSetGenericApplicationCache =
            new SlotWriter (delegate (StandardObject generic, object newValue)
        {
            return internalSlotSet (generic, QuoteApplicationCache, newValue);
        });

        static void InternalSetGenericApplicationCache (StandardObject generic, Cons newValue)
        {
           internalSetGenericApplicationCache (GuaranteeGeneric (generic), newValue);
        }

        static Cons InternalGenericApplicationCache (StandardObject generic)
        {
            return (Cons) internalGenericApplicationCache (GuaranteeGeneric (generic));
        }

        static SlotReader internalGenericLambdaList =
            new SlotReader (delegate (StandardObject generic)
        {
            return internalSlotRef (generic, QuoteLambdaList);
        });

        static SlotWriter internalSetGenericLambdaList =
          new SlotWriter (delegate (StandardObject obj, object newValue)
        {
            return internalSlotSet (obj, QuoteLambdaList, newValue);
        });

        static Cons InternalGenericLambdaList (StandardObject generic)
        {
            return (Cons) internalGenericLambdaList (GuaranteeGeneric (generic));
        }

        static SlotReader internalGenericMethods =
            new SlotReader (delegate (StandardObject generic)
        {
            return internalSlotRef (generic, QuoteMethods);
        });

        static Cons InternalGenericMethods (StandardObject generic)
        {
            return (Cons) internalGenericMethods (GuaranteeGeneric (generic));
        }

        static SlotWriter internalSetGenericMethods =
        new SlotWriter (delegate (StandardObject generic, object newValue)
        {
            return internalSlotSet (generic, QuoteMethods, (Cons) newValue);
        });

        static SlotChanger internalChangeGenericMethods =
            new SlotChanger (delegate (StandardObject generic, SlotChangingFunction func)
        {
            return internalSlotChange (generic, QuoteMethods, func);
        });

        static SlotReader internalGenericMethodClass =
            new SlotReader (delegate (StandardObject obj)
        {
            return internalSlotRef (obj, QuoteMethodClass);
        });

        static StandardObject InternalGenericMethodClass (StandardObject generic)
        {
            return (StandardObject) internalGenericMethodClass (GuaranteeGeneric (generic));
        }

        static Cons InternalChangeGenericMethods (StandardObject generic, SlotChangingFunction func)
        {
            return (Cons) internalChangeGenericMethods (GuaranteeGeneric (generic), func);
        }

        static SlotWriter internalSetGenericMethodClass =
          new SlotWriter (delegate (StandardObject generic, object newValue)
        {
            return internalSlotSet (generic, QuoteMethodClass, newValue);
        });

        static SlotReader<Symbol> internalGenericName =
          new SlotReader<Symbol> (delegate (StandardObject generic)
        {
            return internalSlotRef<Symbol> (generic, QuoteName);
        });

        static Symbol InternalGenericName (StandardObject obj)
        {
            return (Symbol) internalGenericName (GuaranteeGeneric (obj));
        }

        static SlotWriter internalSetGenericName =
          new SlotWriter (delegate (StandardObject generic, object newValue)
        {
            return internalSlotSet (generic, QuoteName, newValue);
        });

        static SlotWriter internalSetGenericStudlyName =
          new SlotWriter (delegate (StandardObject generic, object newValue)
        {
            return internalSlotSet (GuaranteeGeneric (generic), QuoteStudlyName, newValue);
        });

        static SlotReader internalGenericSingletonsList =
            new SlotReader (delegate (StandardObject generic)
        {
            return internalSlotRef (generic, QuoteSingletonsList);
        });

        static SlotWriter internalSetGenericSingletonsList =
          new SlotWriter (delegate (StandardObject generic, object newValue)
        {
            internalSlotSet (generic, QuoteSingletonsList, newValue);
            return newValue;
        });

        static Cons InternalGenericSingletonsList (StandardObject generic)
        {
           return (Cons) internalGenericSingletonsList (GuaranteeGeneric (generic));
        }

        static void InternalSetGenericSingletonsList (StandardObject generic, Cons newValue)
        {
           internalSetGenericSingletonsList (GuaranteeGeneric (generic), newValue);
        }

        #endregion InternalGenericAccessors

        #region InternalMethodAccessors

        // Method accessors
        static StandardObject GuaranteeMethod (StandardObject putativeMethod)
        {
            if (IsInstanceOf (putativeMethod, StandardMethod))
                return putativeMethod;
            else
                throw new NotImplementedException ("GuaranteeMethod");
        }

        static SlotReader<Arity> internalMethodArity =
     new SlotReader<Arity> (delegate (StandardObject method)
     {
         return internalSlotRef<Arity> (GuaranteeMethod (method), QuoteArity);
     });

        static SlotWriter<Arity> internalSetMethodArity =
            new SlotWriter<Arity> (delegate (StandardObject method, Arity rt)
            {
                return internalSlotSet<Arity> (GuaranteeMethod (method), QuoteArity, rt);
            });

        static SlotReader<Symbol> internalMethodName = 
            new SlotReader<Symbol> (delegate (StandardObject method)
        {
            return internalSlotRef<Symbol> (GuaranteeMethod (method), QuoteName);
        });

        static Symbol InternalMethodName (StandardObject method)
        {
            return (Symbol) internalMethodName (GuaranteeMethod (method));
        }

        static SlotWriter internalSetMethodName = 
            new SlotWriter (delegate (StandardObject method, object newValue)
        {
            return internalSlotSet (method, QuoteName, newValue);
        });

        static SlotWriter internalSetMethodStudlyName =
          new SlotWriter (delegate (StandardObject method, object newValue)
        {
            return internalSlotSet (GuaranteeMethod (method), QuoteStudlyName, newValue);
        });

        static SlotReader internalMethodProcedure = 
          new SlotReader (delegate (StandardObject method)
        {
            return internalSlotRef (method, QuoteProcedure);
        });

        static Delegate InternalMethodProcedure (StandardObject method)
        {
            return (Delegate) internalMethodProcedure (GuaranteeMethod (method));
        }

        static SlotWriter internalSetMethodProcedure = 
          new SlotWriter (delegate (StandardObject method, object newValue)
        {
            return internalSlotSet (method, QuoteProcedure, newValue);
        });

        static void InternalSetMethodProcedure (StandardObject method, Delegate proc)
        {
            internalSetMethodProcedure (GuaranteeMethod (method), proc);
        }

        static SlotReader internalMethodQualifier =
          new SlotReader (delegate (StandardObject method)
        {
            return internalSlotRef (method, QuoteQualifier);
        });

        static SlotWriter internalSetMethodQualifier =
          new SlotWriter (delegate (StandardObject method, object newValue)
        {
            internalSlotSet (method, QuoteQualifier, newValue);
            return newValue;
        });

        static Keyword InternalMethodQualifier (StandardObject method)
        {
           return (Keyword) internalMethodQualifier (GuaranteeMethod (method));
        }


        static SlotReader<Cons> internalMethodSpecializers =
            new SlotReader<Cons> (delegate (StandardObject method)
        {
            return internalSlotRef<Cons> (GuaranteeMethod (method), QuoteSpecializers);
        });

        static Cons InternalMethodSpecializers (StandardObject method)
        {
           return (Cons) internalMethodSpecializers (GuaranteeMethod (method));
        }

        static SlotChanger<Cons> internalChangeMethodSpecializers =
            new SlotChanger<Cons> (delegate (StandardObject method, SlotChangingFunction<Cons> proc)
        {
            return internalSlotChange<Cons> (method, QuoteSpecializers, proc);
        });

        static SlotWriter internalSetMethodSpecializers =
          new SlotWriter (delegate (StandardObject method, object specializers)
        {
            return internalSlotSet (method, QuoteSpecializers, specializers);
        });

        #endregion InternalMethodAccessors

        static bool IsInstanceOf (object item, object closClass)
        {
            if (closClass.Equals(Top))
                return true;
            StandardObject cx = ClassOf (item);
            if (cx.Equals(closClass))
                return true;
            else {
                return CL.Memq (closClass, internalClassPrecedenceList (GuaranteeClass (cx))) != null;
            }
        }

        static bool InstancesOf (Cons items, Cons classes)
        {
            if ((items == null) || (classes == null))
                return true;
            return IsInstanceOf (items.Car, classes.Car) && InstancesOf ((Cons)items.Cdr, (Cons)classes.Cdr);
        }

        static Cons SLOTS_OF_STANDARD_DIRECT_SLOT_DEFINITION =
            CL.List (QuoteLocation, QuoteName);

        static Cons SLOTS_OF_STANDARD_EFFECTIVE_SLOT_DEFINITION =
           (Cons) CL.Append (SLOTS_OF_STANDARD_DIRECT_SLOT_DEFINITION,
                       CL.List ());

        static SlotReader<int> internalSlotDefinitionLocation =
    new SlotReader<int> (delegate (StandardObject slotDef)
    {
        return InstanceRef<int> (slotDef, CL.Position (QuoteLocation, SLOTS_OF_STANDARD_DIRECT_SLOT_DEFINITION));
    });

        static SlotWriter<int> internalSetSlotDefinitionLocation =
            new SlotWriter<int> (delegate (StandardObject slotDef, int newValue)
                {
                    return InstanceSet<int> (slotDef, CL.Position (QuoteLocation, SLOTS_OF_STANDARD_DIRECT_SLOT_DEFINITION), newValue);
                });

        static SlotReader<Symbol> internalSlotDefinitionName =
            new SlotReader<Symbol> (delegate (StandardObject slotDef)
                {
                    return InstanceRef<Symbol> (slotDef, CL.Position (QuoteName, SLOTS_OF_STANDARD_DIRECT_SLOT_DEFINITION));
                });

        static SlotWriter<Symbol> internalSetSlotDefinitionName =
    new SlotWriter<Symbol> (delegate (StandardObject slotDef, Symbol name)
    {
        return InstanceSet<Symbol> (slotDef, CL.Position (QuoteName, SLOTS_OF_STANDARD_DIRECT_SLOT_DEFINITION), name);
    });

        static StandardObject StandardDirectSlotDefinition =
            ManifestInstance.CreateInstance (null, CL.Length (SLOTS_OF_STANDARD_CLASS));

        static StandardObject StandardEffectiveSlotDefinition =
            ManifestInstance.CreateInstance (null, CL.Length (SLOTS_OF_STANDARD_CLASS));

        static Cons SLOTS_OF_STANDARD_CLASS =
              CL.List (CL.List (QuoteDefaultInitargs),
                       CL.List (QuoteDirectDefaultInitargs),
                       CL.List (QuoteDirectSlots),
                       CL.List (QuoteDirectSubclasses),
                       CL.List (QuoteDirectSuperclasses),
                       CL.List (QuoteEffectiveSlots),
                       CL.List (QuoteSlotInitializers),
                       CL.List (QuoteFinalizedP),
                       CL.List (QuoteInitializers),
                       CL.List (QuoteName),
                       CL.List (QuoteStudlyName),
                       CL.List (QuoteClassPrecedenceList),
                       CL.List (QuoteClassPrototype),
                       CL.List (QuoteGettersAndSetters));

        static private StandardObject bootstrapMakeStandardClass ()
        {
            StandardObject instance = ManifestInstance.CreateInstance (null, CL.Length (SLOTS_OF_STANDARD_CLASS));
            ((ManifestInstance) instance.Target).Class = instance;
            return instance;
        }

        static readonly StandardObject standardClass =
            bootstrapMakeStandardClass();

        //// Bootstrap step
        delegate object MapFunction (Cons arguments);
        delegate O MapFunction<I, O> (I argument);

        static readonly ConsList<Cons> GettersAndSettersForClass =
            (ConsList<Cons>) CL.Map<Cons,Cons> (QuoteList,
                           new MapFunction<Cons,Cons> (delegate (Cons slotInfo)
        {
            Symbol name = (Symbol) CL.Car (slotInfo);
            Cons slotNames = (Cons) CL.Map (QuoteList, new MapFunction (CL.Car), SLOTS_OF_STANDARD_CLASS);
            int index = CL.Position (CL.Car (slotInfo), slotNames);
            return CL.List (name,
                            new SlotReader (delegate (StandardObject obj)
            {
                return StandardInstanceAccess (obj, index);
            }),
                            new SlotWriter (delegate (StandardObject obj, object newValue)
            {
                return SetStandardInstanceAccess (obj, index, newValue);
            }),
                            new SlotChanger (delegate (StandardObject obj, SlotChangingFunction func)
            {
                return StandardInstanceChange (obj, index, func);
            })
                            );
        }),
                           SLOTS_OF_STANDARD_CLASS);

        static StandardObject bootstrapMakeClassDirectSlot (Cons spec)
        {
            return (StandardObject) CL.Apply (CL.MakeInstance, StandardDirectSlotDefinition, spec);
        }

        static StandardObject bootstrapMakeClassEffectiveSlot (StandardObject directSlot)
        {
            return (StandardObject) CL.MakeInstance(StandardEffectiveSlotDefinition, directSlot);
        }

        static ConsList<StandardObject> classDirectSlots =
            (ConsList < StandardObject >) CL.Map<Cons, StandardObject> (QuoteList,
                                                                   new MapFunction<Cons, StandardObject> (bootstrapMakeClassDirectSlot),
                                                                   SLOTS_OF_STANDARD_CLASS);

        static ConsList<StandardObject> classEffectiveSlots =
            (ConsList<StandardObject>) CL.Map<StandardObject, StandardObject> (QuoteList,
            new MapFunction<StandardObject, StandardObject> (bootstrapMakeClassEffectiveSlot),
            classDirectSlots);

        static bool isEffectiveSlotSlot (StandardObject slot)
        {
            return QuoteEffectiveSlots == internalSlotDefinitionName (slot);
        }

        static int effectiveSlotSlot =
            CL.PositionIf<StandardObject> (new Predicate<StandardObject>(isEffectiveSlotSlot), classEffectiveSlots);

        static ConsList<StandardObject> bootstrapStep0 =
            InstanceSet<ConsList<StandardObject>> (StandardClass, effectiveSlotSlot, classEffectiveSlots);

        static ConsList<StandardObject> internalClassEffectiveSlots (StandardObject closClass)
        {
            return InstanceRef<ConsList<StandardObject>> (closClass, effectiveSlotSlot);
        }

        static bool bootstrapStep0aFunction ()
        {
            foreach (StandardObject effectiveSlot in classEffectiveSlots)
                internalSetSlotDefinitionLocation (effectiveSlot, CL.PositionIf<StandardObject> (((slot) => slot == effectiveSlot), classEffectiveSlots));
            internalSetClassDirectSlots (StandardClass, classDirectSlots);
            internalSetClassName (standardClass, QuoteStandardClass);
            internalSetClassStudlyName (standardClass, "StandardClass");
            return true;
        }

        static bool bootstrapStep0a = bootstrapStep0aFunction ();
           

        //static object bootstrapStep0aa =

        //   ((SlotWriter) (CL.Caddr (CL.Assq (QuoteGettersAndSetters, GettersAndSettersForClass)))) (StandardClass, GettersAndSettersForClass);

        //static SlotReader<ConsList<Cons>> internalClassGettersAndSetters =
        //   (SlotReader<ConsList<Cons>>) (CL.Cadr (CL.Assq (QuoteGettersAndSetters, GettersAndSettersForClass)));

        //static ConsList<Cons> InternalClassGettersAndSetters (StandardObject closClass)
        //{
        //    return internalClassGettersAndSetters (GuaranteeClass (closClass));
        //}

        //static object bootstrapStep0a =
        //    ((SlotWriter) (CL.Caddr (CL.Assq (QuoteName, GettersAndSettersForClass)))) (StandardClass, QuoteStandardClass);

        static readonly StandardObject top
            = (StandardObject) CL.MakeInstance (StandardClass,  // wrong, but fixed later
                            KW.DirectSuperclasses, CL.List (),
                            KW.Name, QuoteTop,
                            KW.StudlyName, "Top");

        static readonly StandardObject function
            = (StandardObject) CL.MakeInstance (StandardClass, // wrong, but fixed later
                            KW.DirectSuperclasses, CL.List (Top),
                            KW.Name, QuoteFunction,
                            KW.StudlyName, "Function");

        static readonly StandardObject standardObjectClass
            = (StandardObject) CL.MakeInstance (StandardClass,
                            KW.DirectSuperclasses, CL.List (Top),
                            KW.Name, QuoteStandardObject,
                            KW.StudlyName, "StandardObject");

        static readonly StandardObject metaobject
            = (StandardObject) CL.MakeInstance (StandardClass,
                            KW.DirectSuperclasses, CL.List (StandardObjectClass),
                            KW.Name, QuoteMetaobject,
                            KW.StudlyName, "MetaObject");

        static readonly StandardObject specializer
            = (StandardObject) CL.MakeInstance (StandardClass,
                            KW.DirectSuperclasses, CL.List (Metaobject),
                            KW.Name, QuoteSpecializer,
                            KW.StudlyName, "Specializer");

        static readonly StandardObject closClass
            = (StandardObject) CL.MakeInstance (StandardClass,
                            KW.DirectSuperclasses, CL.List (Specializer),
                            KW.Name, QuoteClass,
                            KW.StudlyName, "Class");


        static SlotInitializer makeStandardClassSlotInitializer (StandardObject slot)
        {
            return new SlotInitializer (MakeSlotInitFunction (slot));
        }

        // Fixup the standard class
        static int bootstrapFixupStandardClass ()
        {
            internalSetClassPrecedenceList (standardClass, CL.List (ClosClass, Specializer, Metaobject, StandardObjectClass, Top));
            // internalSetClassPrecedenceList (funcallableStandardClass, CL.List (ClosClass, Specializer, Metaobject, StandardObjectClass, Top));
            //ConsList<StandardObject> directSlotsOfClass = (ConsList<StandardObject>) CL.Map<Cons, StandardObject> (QuoteList, new MapFunction<Cons, StandardObject> (bootstrapProcessDirectSlot), SLOTS_OF_STANDARD_CLASS);
            //ConsList<StandardObject> effectiveSlotsOfClass =
            //        bootstrapComputeEffectiveSlots ((ConsList<StandardObject>) CL.Map<StandardObject, StandardObject> (QuoteList, new MapFunction<StandardObject, StandardObject> (bootstrapProcessEffectiveSlot), directSlotsOfClass));
            //internalSetClassDirectSlots (standardClass, directSlotsOfClass);
            //internalSetClassEffectiveSlots (standardClass, effectiveSlotsOfClass);
            //ConsList<StandardObject> directSlotsOfFClass = (ConsList<StandardObject>) CL.Map<Cons, StandardObject> (QuoteList, new MapFunction<Cons, StandardObject> (bootstrapProcessDirectSlot), SLOTS_OF_STANDARD_CLASS);
            //ConsList<StandardObject> effectiveSlotsOfFClass =
            //        bootstrapComputeEffectiveSlots ((ConsList<StandardObject>) CL.Map<StandardObject, StandardObject> (QuoteList, new MapFunction<StandardObject, StandardObject> (bootstrapProcessEffectiveSlot), directSlotsOfFClass));
            //internalSetClassDirectSlots (funcallableStandardClass, directSlotsOfFClass);
            //internalSetClassEffectiveSlots (funcallableStandardClass, effectiveSlotsOfFClass);
            internalSetClassDirectSuperclasses (standardClass, CL.List (ClosClass));
            // internalSetClassDirectSuperclasses (funcallableStandardClass, CL.List (ClosClass));
            internalSetClassInitializers (standardClass, null);
            // internalSetClassSlotInitializers (standardClass,
            //     (ConsList<SlotInitializer>) CL.Map<StandardObject,SlotInitializer> (QuoteList, new MapFunction<StandardObject, SlotInitializer> (delegate (StandardObject info) { return makeStandardClassSlotInitializer(info);}), directSlotsOfClass));

            // internalSetClassStudlyName (funcallableStandardClass, "FuncallableStandardClass");
            // throw new NotImplementedException ();
            //internalSetClassEffectiveSlots (standardClass, SLOTS_OF_STANDARD_CLASS);
            //internalSetClassEffectiveSlots (funcallableStandardClass, SLOTS_OF_STANDARD_CLASS);
            return 1;
        }

        static int foo = bootstrapFixupStandardClass ();
        //static object bootstrapStep1 =
        //    ((SlotWriter) (CL.Caddr (CL.Assq (QuoteGettersAndSetters, GettersAndSettersForClass)))) 
        //       (funcallableStandardClass, GettersAndSettersForClass);

        static readonly StandardObject funcallableStandardClass
    = (StandardObject) CL.MakeInstance (StandardClass,
            KW.DirectSlots, SLOTS_OF_STANDARD_CLASS,
            KW.DirectSuperclasses, CL.List (ClosClass),
            KW.Name, QuoteFuncallableStandardClass,
            KW.StudlyName, "FuncallableStandardClass");



        static readonly StandardObject builtInClass =
            (StandardObject) CL.MakeInstance (StandardClass,
                                              KW.DirectSlots, CL.List (CL.List (QuoteDotnetType, KW.Initarg, KW.DotnetType)),
                                              KW.DirectSuperclasses, CL.List (closClass),
                                              KW.Name, QuoteBuiltInClass,
                                              KW.StudlyName, "BuiltInClass");

        static readonly Dictionary<Type, StandardObject> BuiltInClassDictionary = new Dictionary<Type, StandardObject> ();

 
        static readonly StandardObject funcallableStandardObject =
           (StandardObject) CL.MakeInstance (StandardClass,
                            KW.DirectSuperclasses, CL.List (StandardObjectClass), // Function),
                            KW.Name, QuoteFuncallableStandardObject,
                            KW.StudlyName, "FuncallableStandardObject");

 
 
        static readonly StandardObject method =
            (StandardObject) CL.MakeInstance (FuncallableStandardClass,
                                              KW.DirectSuperclasses, CL.List (Metaobject, FuncallableStandardObject),
                                              KW.Name, QuoteMethod,
                                              KW.StudlyName, "Method");

        static readonly StandardObject standardMethod =
            (StandardObject) CL.MakeInstance (FuncallableStandardClass,
                                              KW.DirectSlots, CL.List (
                                                                       CL.List (QuoteArity, KW.Initvalue, null),
                                                                       CL.List (QuoteLambdaList),
                                                                       CL.List (QuoteName, KW.Initarg, KW.Name),
                                                                       CL.List (QuoteStudlyName, KW.Initarg, KW.StudlyName),
                                                                       CL.List (QuoteProcedure, KW.Initarg, KW.Procedure),
                                                                       CL.List (QuoteQualifier, KW.Initarg, KW.Qualifier, KW.Initvalue, KW.Primary),
                                                                       CL.List (QuoteSpecializers, KW.Initarg, KW.Specializers, KW.Initvalue, null)),
                                              KW.DirectSuperclasses, CL.List (Method),
                                              KW.Name, QuoteStandardMethod,
                                              KW.StudlyName, "StandardMethod");

        static readonly StandardObject genericFunction =
      (StandardObject) CL.MakeInstance (FuncallableStandardClass,
                                        KW.DirectSuperclasses, CL.List (Metaobject, FuncallableStandardObject),
                                        KW.Name, QuoteGenericFunction,
                                        KW.StudlyName, "GenericFunction");

        static readonly StandardObject standardGenericFunction =
                 (StandardObject) CL.MakeInstance (FuncallableStandardClass,
                                KW.DirectSlots, CL.List (
                                                         CL.List (QuoteArity),
                                                         CL.List (QuoteApplicationCache, KW.Initvalue, new Cons (null, null)),
                                                         CL.List (QuoteLambdaList, KW.Initarg, KW.LambdaList),
                                                         CL.List (QuoteMethods, KW.Initvalue, null),
                                                         CL.List (QuoteName, KW.Initarg, KW.Name),
                                                         CL.List (QuoteStudlyName, KW.Initarg, KW.StudlyName),
                                                         CL.List (QuoteMethodClass, KW.Initarg, KW.MethodClass, KW.Initvalue, StandardMethod),
                                                         CL.List (QuoteSingletonsList, KW.Initvalue, null)),
                                KW.DirectSuperclasses, CL.List (GenericFunction),
                                KW.Name, QuoteStandardGenericFunction,
                                KW.StudlyName, "StandardGenericFunction");


        static readonly StandardObject StandardAccessorMethod =
            (StandardObject) CL.MakeInstance (StandardClass,
                                       KW.DirectSlots, CL.List (),
                                       KW.DirectSuperclasses, CL.List (StandardMethod),
                                       KW.Name, QuoteStandardAccessorMethod,
                                       KW.StudlyName, "StandardAccessorMethod");

        static readonly StandardObject StandardReaderMethod =
            (StandardObject) CL.MakeInstance (StandardClass,
                               KW.DirectSlots, CL.List (),
                               KW.DirectSuperclasses, CL.List (StandardAccessorMethod),
                               KW.Name, QuoteStandardReaderMethod,
                               KW.StudlyName, "StandardReaderMethod");

        static readonly StandardObject StandardWriterMethod =
            (StandardObject) CL.MakeInstance (StandardClass,
                               KW.DirectSlots, CL.List (),
                               KW.DirectSuperclasses, CL.List (StandardAccessorMethod),
                               KW.Name, QuoteStandardWriterMethod,
                               KW.StudlyName, "StandardWriterMethod");

        static readonly StandardObject genericFunctionMethodClass =
            (StandardObject) CL.MakeInstance (standardGenericFunction,
                                              KW.MethodClass, StandardReaderMethod,
                                              KW.Name, QuoteGenericFunctionMethodClass,
                                              KW.StudlyName, "GenericFunctionMethodClass",
                                              KW.LambdaList, CL.List (QuoteGenericFunction));
        
        static readonly StandardObject allocateInstance =
            (StandardObject) CL.MakeInstance (StandardGenericFunction,
                                              KW.Name, QuoteAllocateInstance,
                                              KW.StudlyName, "AllocateInstance",
                                              KW.LambdaList, CL.List (QuoteClass, QuoteAndRest, QuoteInitargs, QuoteAndKey, QuoteAndAllowOtherKeys));

        static readonly StandardObject computeApplicableMethods =
            (StandardObject) CL.MakeInstance (StandardGenericFunction,
                                              KW.Name, QuoteComputeApplicableMethods,
                                              KW.StudlyName, "ComputeApplicableMethods",
                                              KW.LambdaList, CL.List (QuoteGenericFunction, QuoteArguments));

        static readonly StandardObject computeDiscriminatingFunction =
            (StandardObject) CL.MakeInstance (StandardGenericFunction,
                                              KW.Name, QuoteComputeDiscriminatingFunction,
                                              KW.StudlyName, "ComputeDiscriminatingFunction",
                                              KW.LambdaList, CL.List (QuoteGenericFunction));

        static readonly StandardObject computeEffectiveMethod =
            (StandardObject) CL.MakeInstance (StandardGenericFunction,
                                              KW.Name, QuoteComputeEffectiveMethod,
                                              KW.StudlyName, "ComputeEffectiveMethod",
                                              KW.LambdaList, CL.List (QuoteComputeEffectiveMethod));

        static readonly StandardObject computeMethodMoreSpecific =
            (StandardObject) CL.MakeInstance (StandardGenericFunction,
                                              KW.Name, QuoteComputeMethodMoreSpecific,
                                              KW.StudlyName, "ComputeMethodMoreSpecific",
                                              KW.LambdaList, CL.List (QuoteGenericFunction));

        delegate object AddMethodSignature (NextMethodFunction callNextMethod, StandardObject generic, StandardObject method);
        static StandardObject addMethod =
            (StandardObject) CL.MakeInstance (StandardGenericFunction,
                                              KW.Name, QuoteAddMethod,
                                              KW.StudlyName, "AddMethod",
                                              KW.LambdaList, CL.List (QuoteGenericFunction, QuoteMethod));

        static readonly List<StandardObject> GenericInvocationGenerics = 
            new List<StandardObject> {computeDiscriminatingFunction,
                                      computeApplicableMethods,
                                      computeEffectiveMethod,
                                      computeMethodMoreSpecific};

        static Cons GenericApplicationCacheTag = new Cons (null, null);

        static void extendSingletonsList (Cons tables, Cons specs)
        {
            if (specs != null) {
                if (internalIsSingleton (specs.Car)) {
                    throw new NotImplementedException ("extendSingletonsList");
                }
                extendSingletonsList ((Cons) tables.Cdr, (Cons) specs.Cdr);
            }
        }

        static bool IsShorter (Cons left, Cons right)
        {
            return (right != null)
                && ((left == null) || IsShorter ((Cons) left.Cdr,(Cons) right.Cdr));
        }

        static bool SameMethodSignatureLoop (Cons leftSpecs, Cons rightSpecs)
        {
            if (leftSpecs != null)
                return (rightSpecs != null) 
                     && Object.ReferenceEquals (leftSpecs, rightSpecs)
                     && SameMethodSignatureLoop ((Cons)leftSpecs.Cdr, (Cons)rightSpecs.Cdr);
             else 
                return rightSpecs == null;

       }

        static bool SameMethodSignature (StandardObject left, StandardObject right)
        {
            return Object.ReferenceEquals (internalMethodQualifier (left), internalMethodQualifier (right))
                && SameMethodSignatureLoop ((Cons) internalMethodSpecializers (left), (Cons) internalMethodSpecializers (right));
        }

        static Cons nFalses (int n)
        {
            if (n == 0)
                return null;
            else
                return new Cons (null, nFalses (n - 1));
        }

        static object bootstrapAddMethodImpl (StandardObject generic, StandardObject method)
        {
            Cons tables = InternalGenericSingletonsList (generic);
            Cons specs = InternalMethodSpecializers (method);
            Symbol qualifier =  InternalMethodQualifier (method);
            if (IsShorter (tables, specs)) {
                tables = (Cons) CL.Append (tables, nFalses (CL.Length (specs) - CL.Length (tables)));
                InternalSetGenericSingletonsList (generic, tables);
            }
            extendSingletonsList (specs, tables);
            if (GenericInvocationGenerics.Contains (generic)) {
                GenericApplicationCacheTag = new Cons (null, null);
                InternalSetGenericApplicationCache (generic, new Cons (GenericApplicationCacheTag, new MethodCache()));
            }
            InternalChangeGenericMethods (generic, (existingMethods) => new Cons (method, CL.RemoveIf (new Predicate (delegate (object existingMethod)
            {
                return SameMethodSignature ((StandardObject) existingMethod, method);
            }), existingMethods)));
            FuncallHandler df =(FuncallHandler) ComputeDiscriminatingFunction (generic);
            setFuncallableInstanceFunction (generic, df);
            return null;
        }

        static object bootstrapAddMethod (StandardObject self, object [] arguments)
        {
            if (arguments.Length != 2)
                throw new ArgumentException ("Wrong number of arguments to bootstrapAddMethod");
            return bootstrapAddMethodImpl (GuaranteeGeneric ((StandardObject) arguments[0]), GuaranteeMethod ((StandardObject) arguments[1]));
        }

        static object bootstrapGenericFunctionMethodClass (StandardObject self, object [] arguments)
        {
            return InternalGenericMethodClass ((StandardObject) arguments [0]);
        }

        static FuncallHandler bootstrapComputeDiscriminatingFunctionImpl (StandardObject generic)
        {
            StandardObject firstMethod = (StandardObject) CL.Car (InternalGenericMethods (generic));
            Delegate procedure = InternalMethodProcedure (firstMethod);
            FuncallHandler answer = (FuncallHandler) procedure.DynamicInvoke (null, new object [] {generic});
            return answer;
        }

        static FuncallHandler bootstrapComputeDiscriminatingFunction (StandardObject self, object [] arguments)
        {
            if (arguments.Length != 1)
                throw new ArgumentException ("Wrong number of arguments to bootstrapComputeDiscriminatingFunction");
            return bootstrapComputeDiscriminatingFunctionImpl (GuaranteeGeneric ((StandardObject) arguments [0]));
        }

        static object NullStandin = new Cons (null, null);

        static object last (object l)
        {
            Cons lp = l as Cons;
            if (lp.Cdr == null)
                return lp.Car;
            else
                return last (lp.Cdr);
        }

        static object [] PrePend (object element, object [] elements)
        {
            object [] result = new object [elements.Length + 1];
            elements.CopyTo (result, 1);
            result [0] = element;
            return result;
        }

        static NextMethodFunction computeCallable (bool isGroundCase, StandardObject generic, MethodCache cache, Cons keys, object [] arguments)
        {
            NextMethodFunction answer;
            if (isGroundCase) {
                Delegate m = InternalMethodProcedure ((StandardObject) last (InternalGenericMethods (generic)));
                answer = (NextMethodFunction) GroundMethodFunction.Create (m);
            }
            else {
                object [] newArguments = new object [arguments.Length + 1];
                arguments.CopyTo (newArguments, 1);
                newArguments [0] = generic;
                answer = (NextMethodFunction) ComputeEffectiveMethod (generic, ComputeApplicableMethods (newArguments));
            }
            cache.Add (keys, answer);
            return answer;
        }

        static Cons getKeysLoop (object [] arguments, int argindex, object tables, Cons keys)
        {
            if ((tables == null) || (argindex >= arguments.Length))
                return keys;
            object thisTable = CL.Car (tables);
            object thisArg = arguments [argindex];
            object thisKey;
            MethodCache thisTable1 = thisTable as MethodCache;
            if (thisTable1 != null && thisTable1.ContainsKey (thisArg))
                thisKey = thisArg;
            else
                thisKey = ClassOf (thisArg).SerialNumber();
            return getKeysLoop (arguments, argindex + 1, CL.Cdr (tables), new Cons (thisKey, keys));
        }

        static Cons getKeys (object [] arguments, object singletons)
        {
            return (Cons) CL.Reverse (getKeysLoop (arguments, 0, singletons, null));
        }

        delegate object CDF (StandardObject generic, object [] arguments);

        static object realComputeDiscriminatingFunction (StandardObject generic, object [] arguments)
        {
            Cons appCache = InternalGenericApplicationCache (generic);
            Cons lambdaList = InternalGenericLambdaList (generic);
            Cons singletons =  InternalGenericSingletonsList (generic);
            Cons keys = getKeys (arguments, singletons);
            bool isGroundCase = GenericInvocationGenerics.Contains (generic)
                && arguments.Length > 0
                && GenericInvocationGenerics.Contains ((StandardObject) arguments [0]);
            // should do arity check here

            // check if we need to flush the cache
            if (!object.ReferenceEquals (appCache.Car, GenericApplicationCacheTag)) {
                appCache = new Cons (GenericApplicationCacheTag, new MethodCache());
                InternalSetGenericApplicationCache (generic, appCache);
            }

            MethodCache cache = (MethodCache) appCache.Cdr;
            NextMethodFunction ah;
            if (!cache.TryGetValue (keys, out ah))
                ah = computeCallable (isGroundCase, generic, cache, keys, arguments);

            return ah.Invoke (arguments);
        }

        delegate FuncallHandler ComputeDiscriminatingFunctionMethodSignature (NextMethodFunction callNextMethod, params object [] ignore);

        static FuncallHandler realMethodComputeDiscriminatingFunction (NextMethodFunction callNextMethod, params object [] ignore)
        {
            return new FuncallHandler (realComputeDiscriminatingFunction);
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
                    else if (comp (GuaranteeMethod ((StandardObject) CL.Car (left)),
                                   GuaranteeMethod ((StandardObject) CL.Car (right)))) {
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
            StandardObject thisMethod = GuaranteeMethod ((StandardObject) methodList.Car);
            if (mp (thisMethod))
                return new Cons (thisMethod, FilterMethods (mp, (Cons) methodList.Cdr));
            else
                return FilterMethods (mp, (Cons) methodList.Cdr);
        }

        delegate Cons ComputeApplicableMethodsMethodSignature (NextMethodFunction callNextMethod, StandardObject generic, params object [] suppliedArguments);
        static Cons realComputeApplicableMethods (NextMethodFunction callNextMethod, StandardObject generic, params object [] arguments)
        {
            Cons arglist = Cons.SubvectorToList (arguments, 0, arguments.Length);
            Cons methods = (Cons) internalGenericMethods (generic);
            Cons filteredMethods = FilterMethods ((m) => InstancesOf (arglist, (Cons) internalMethodSpecializers (m)), methods);
            Cons sortedMethods = SortMethods (filteredMethods,
                                              MakeSortPredicate ((MethodCompare3) ComputeMethodMoreSpecific (generic), arglist));
            return sortedMethods;
        }

        static bool IsMoreSpecific (StandardObject left, StandardObject right, object arg)
        {
            Cons cc1 = CL.Memq (left, internalClassPrecedenceList (ClassOf (arg)));
            return (cc1 != null)
                && CL.Memq (right, CL.Cdr (cc1)) != null;
        }

        static bool realComputeMethodMoreSpecificLoop (StandardObject generic, StandardObject left, StandardObject right, Cons arglist)
        {
            Cons speclsLeft = (Cons) internalMethodSpecializers (GuaranteeMethod (left));
            Cons speclsRight = (Cons) internalMethodSpecializers (GuaranteeMethod (right));
            while (true) {
                if ((speclsLeft == null) && (speclsRight == null)) {
                    if (CL.Eq (internalMethodQualifier (left),
                               internalMethodQualifier (right)))
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

        delegate MethodCompare3 ComputeMethodMoreSpecificMethodSignature (NextMethodFunction callNextMethod, StandardObject generic);
        static MethodCompare3 realComputeMethodMoreSpecific (NextMethodFunction callNextMethod, StandardObject generic)
        {
            return new MethodCompare3 ((left, right, arglist) => realComputeMethodMoreSpecificLoop (generic, left, right, arglist));
        }

        static NextMethodFunction oneAroundStep (Cons methods, Cons args)
        {
            throw new NotImplementedException ();
        }

        

        static object ListRef (Cons args, int n)
        {
            if (args == null)
                throw new NotImplementedException ();
            if (n == 0)
                return args.Car;
            else
                return ListRef ((Cons) args.Cdr, n - 1);
        }

        static object [] ListToVector (Cons args)
        {
            object [] result = new object [CL.Length (args)];
            for (int i = 0; i < result.Length; i++) {
                result [i] = args.Car;
                args = (Cons) args.Cdr;
            }
            return result;
        }


        delegate NextMethodFunction ComputeEffectiveMethodMethodSignature (NextMethodFunction callNextMethod, StandardObject generic, Cons methodList);
        static NextMethodFunction realComputeEffectiveMethod (NextMethodFunction callNextMethod, StandardObject generic, Cons methodList)
        {
            Cons primaryMethods = null;
            Cons aroundMethods = null;
            Cons beforeMethods = null;
            Cons afterMethods = null;

            MethodStepper oneStep = new MethodStepper (generic);

            while (methodList != null) {
                StandardObject method = GuaranteeMethod ((StandardObject) methodList.Car);
                methodList = (Cons) methodList.Cdr;
                Symbol q = (Symbol) internalMethodQualifier (method);
                if (q.Equals (KW.Primary)) {
                    primaryMethods = new Cons (new Cons (method, MethodStepWrapper.Create ((Delegate) internalMethodProcedure (method))), primaryMethods);
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

        static StandardObject defaultInitargs =
            (StandardObject) CL.MakeInstance (StandardGenericFunction,
                                              KW.Name, QuoteDefaultInitargs,
                                              KW.StudlyName, "DefaultInitargs",
                                              KW.LambdaList, CL.List (QuoteClass, QuoteSuppliedInitargs));

        delegate Cons DefaultInitargsMethodSignature (NextMethodFunction callNextMethod, StandardObject closClass, Cons suppliedInitargs);
        static Cons methodDefaultInitargs (NextMethodFunction callNextMethod, StandardObject closClass, Cons suppliedInitargs)
        {
            // wrong!  fix later
            return suppliedInitargs;
        }

        static StandardObject initializeInstance =
	         (StandardObject) CL.MakeInstance (StandardGenericFunction,
					   KW.Name, QuoteInitializeInstance,
					   KW.StudlyName, "InitializeInstance",
					   KW.LambdaList, CL.List (QuoteInstance, QuoteAndRest, QuoteInitargs));

        delegate object InitializeInstanceMethodSignature (NextMethodFunction callNextMethod, StandardObject instance, params object [] initargs);
        static object methodInitializeInstanceAll (NextMethodFunction callNextMethod, StandardObject instance, params object [] initargs)
        {
            throw new NotImplementedException ("methodInitializeInstanceAll");
        }



        static object methodInitializeInstanceStdObj (NextMethodFunction callNextMethod, StandardObject instance, params object [] initargs)
        {
            Cons initargList = Cons.SubvectorToList (initargs, 0, initargs.Length);
            StandardObject closClass = ClassOf (instance);
            ConsList<ClassInitializer> classInitializers = (ConsList<ClassInitializer>) internalClassInitializers (closClass);
            if (classInitializers != null) {
                foreach (ClassInitializer init in (ConsList<ClassInitializer>) internalClassInitializers (closClass)) {
                    init.Initialize (instance, initargList);
                }
            }
            int index = 0;
            foreach (SlotInitializer init in (ConsList<SlotInitializer>) internalClassSlotInitializers (closClass)) {
                init.Initialize (instance, index, initargList);
                index += 1;
            }
            return null;
        }

        static StandardObject SelectCplCandidateValidateLoop (Cons lists, Cons more, StandardObject candidate, Cons l)
        {
            if (l == null)
                return candidate;
            Cons v = (Cons) l.Car;
            if (v == null)
                return SelectCplCandidateValidateLoop (lists, more, candidate, (Cons) l.Cdr);
            if (CL.Memq (candidate, (Cons) v.Cdr) != null)
                return SelectCplCandidateSearchLoop (lists, more);
            else
                return SelectCplCandidateValidateLoop (lists, more, candidate, (Cons) l.Cdr);
        }

        static StandardObject SelectCplCandidateSearchLoop (Cons lists, Cons scan)
        {
            if (scan == null)
                throw new NotImplementedException ("Inconsistent class linearization");
            Cons list = (Cons) scan.Car;
            Cons more = (Cons) scan.Cdr;
            if (list == null)
                return SelectCplCandidateSearchLoop (lists, more);
            StandardObject candidate = (StandardObject) list.Car;
            return SelectCplCandidateValidateLoop (lists, more, candidate, lists);
        }


        static StandardObject SelectCplCandidate (Cons cpls)
        {
            return SelectCplCandidateSearchLoop (cpls, cpls);
        }

        static Cons MergeCpls (Cons cpls, Cons result)
        {
            bool done = true;
            foreach (object cpl in cpls) {
                if (cpl != null) {
                    done = false;
                    break;
                }
            }
            if (done)
                return CL.Reverse (result);

            StandardObject candidate = SelectCplCandidate (cpls);
            return MergeCpls ((Cons) CL.Map (QuoteList, new MapFunction ((cpList) => (cpList == null)
                                                                    ? null
                                                                    : ((StandardObject)((Cons) cpList).Car == candidate)
                                                                    ? ((Cons) cpList).Cdr
                                                                    : cpList),
                                                        cpls),
                               new Cons (candidate, result));

        }

        delegate Cons computeClassPrecedenceListMethodSignature (NextMethodFunction callNextMethod, StandardObject closClass);
        static Cons computeClassPrecedenceListMethod  (NextMethodFunction callNextMethod, StandardObject closClass)
        {
            Cons superclasses = internalClassDirectSuperclasses (closClass);
            Cons cpls = (Cons) CL.Map (QuoteList, internalClassPrecedenceList, superclasses);
            return MergeCpls (cpls, CL.List (closClass));
        }

        static readonly StandardObject computeClassPrecedenceList =
            (StandardObject) CL.MakeInstance (StandardGenericFunction,
                                              KW.Name, QuoteComputeClassPrecedenceList,
                                              KW.StudlyName, "ComputeClassPrecedenceList",
                                              KW.LambdaList, CL.List (QuoteClass));

        delegate Cons computeSlotsMethodSignature (NextMethodFunction callNextMethod, StandardObject closClass);
        static Cons computeSlotsMethod  (NextMethodFunction callNextMethod, StandardObject closClass)
        {
            Dictionary<Symbol, ConsList<StandardObject>> allDirectSlots = new Dictionary<Symbol, ConsList<StandardObject>> ();
            foreach (StandardObject super in CL.Reverse < StandardObject> (internalClassPrecedenceList (closClass))) {
                foreach (StandardObject directSlot in internalClassDirectSlots (super)) {
                    ConsList<StandardObject> probe;
                    Symbol name = internalSlotDefinitionName (directSlot);
                    if (allDirectSlots.TryGetValue (name, out probe) == true) {
                        probe = new ConsList<StandardObject> (directSlot, probe);
                        allDirectSlots.Remove (name);
                        allDirectSlots.Add (name, probe);
                    }
                    else {
                        probe = new ConsList<StandardObject> (directSlot, null);
                        allDirectSlots.Add (name, probe);
                    }
                }

                throw new NotImplementedException ();
            }
            throw new NotImplementedException ();



        }

        static readonly StandardObject computeSlots =
            (StandardObject) CL.MakeInstance (StandardGenericFunction,
                                              KW.Name, QuoteComputeSlots,
                                              KW.StudlyName, "ComputeSlots",
                                              KW.LambdaList, CL.List (QuoteClass));

        static StandardObject makeDirectSlot (Cons specs)
        {
            throw new NotImplementedException ();
        }

        static object methodInitializeInstanceStdClass (NextMethodFunction callNextMethod, StandardObject instance, params object [] initargs)
        {
            callNextMethod ();
            Cons initargList = Cons.VectorToList (initargs);
            internalSetClassDirectDefaultInitargs (instance, Utility.GetArg (initargList, KW.DirectDefaultInitargs, null));

            // Direct Superclasses
            Cons supers = (Cons) Utility.GetArg (initargList, KW.DirectSuperclasses, null);
            internalSetClassDirectSuperclasses (instance, supers);

            Cons directSlotSpecs = (Cons) Utility.GetArg (initargList, KW.DirectSlots, null);
            ConsList<StandardObject> directSlots = (ConsList<StandardObject>) CL.Map<Cons, StandardObject> (QuoteList, new MapFunction<Cons,StandardObject> (makeDirectSlot), directSlotSpecs);
            internalSetClassDirectSlots (instance, directSlots);

            Cons cpl = (Cons) ComputeClassPrecedenceList (instance);
            internalSetClassPrecedenceList (instance, cpl);

            ConsList<StandardObject> effectiveSlots = (ConsList<StandardObject>) ComputeSlots (instance);
            internalSetClassEffectiveSlots (instance, effectiveSlots);

            throw new NotImplementedException ("methodInitializeInstanceStdClass");
        }

        static object methodInitializeInstanceStdGeneric (NextMethodFunction callNextMethod, StandardObject instance, params object [] initargs)
        {
            callNextMethod ();
            Cons initialMethods = Utility.GetArgs (initargs, KW.Method);
            if (initialMethods != null)
                foreach (object method in Utility.GetArgs (initargs, KW.Method))
                    AddMethod (instance, method);
            return instance;
        }

        static Cons TrimLoop (Cons remaining)
        {
            if (remaining == null)
                return null;
            else if (Object.ReferenceEquals (remaining.Car, Top)) {
                return TrimLoop ((Cons) remaining.Cdr);
            }
            else
                return CL.Reverse (remaining);
        }

        static Cons TrimMethodSpecializers (Cons methodSpecializers)
        {
            return TrimLoop (CL.Reverse (methodSpecializers));
        }

        static object methodInitializeInstanceStdMethod (NextMethodFunction callNextMethod, StandardObject instance, params object [] initargs)
        {
            callNextMethod ();
            internalChangeMethodSpecializers (instance, TrimMethodSpecializers);
            return instance;
        }


        
        static StandardObject methodQualifier =
            (StandardObject) CL.MakeInstance (StandardGenericFunction,
                                              KW.MethodClass, StandardReaderMethod,
                                              KW.Name, QuoteMethodQualifier,
                                              KW.StudlyName, "MethodQualifier",
                                              KW.LambdaList, CL.List (QuoteMethod));

        delegate object MethodQualifierMethodSignature (NextMethodFunction callNextMethod, StandardObject method);
        static object methodMethodQualifier (NextMethodFunction callNextMethod, StandardObject method)
        {
            return internalMethodQualifier (GuaranteeMethod (method));
        }

        static StandardObject methodSpecializers =
            (StandardObject) CL.MakeInstance (StandardGenericFunction,
                                      KW.MethodClass, StandardReaderMethod,
                                      KW.Name, QuoteMethodSpecializers,
                                      KW.StudlyName, "MethodSpecializers",
                                      KW.LambdaList, CL.List (QuoteMethod));

        delegate object MethodSpecializersMethodSignature (NextMethodFunction callNextMethod, StandardObject method);
        static object methodMethodSpecializers (NextMethodFunction callNextMethod, StandardObject method)
        {
            return internalMethodSpecializers (GuaranteeMethod (method));
        }

        delegate object AllocateInstanceMethodSignature (NextMethodFunction callNextMethod, StandardObject closClass, params object [] initargs);
        static object methodAllocateStandardInstance (NextMethodFunction callNextMethod, StandardObject closClass, params object [] initargs)
        {
            return ManifestInstance.CreateInstance (closClass, CL.Length (internalClassSlotInitializers (closClass)));
        }

        static object methodAllocateFuncallableInstance (NextMethodFunction callNextMethod, StandardObject closClass, params object [] initargs)
        {
            return ManifestInstance.CreateFuncallableInstance (closClass, ((ICollection<SlotInitializer>)internalClassSlotInitializers (closClass)).Count);
        }

        static bool bootstrapStep2Function ()
        {
            setFuncallableInstanceFunction (addMethod, bootstrapAddMethod);
            setFuncallableInstanceFunction (genericFunctionMethodClass, bootstrapGenericFunctionMethodClass);
            setFuncallableInstanceFunction (computeDiscriminatingFunction, bootstrapComputeDiscriminatingFunction);

            // Now we can a method to ComputeDiscriminatingFunction.
            CL.AddMethod (ComputeDiscriminatingFunction,
                          CL.MakeInstance (GenericFunctionMethodClass (ComputeDiscriminatingFunction),
                                           KW.Name, QuoteMethodComputeDiscriminatingFunction,
                                           KW.StudlyName, "Method:ComputeDiscriminatingFunction",
                                           KW.Procedure, new ComputeDiscriminatingFunctionMethodSignature (realMethodComputeDiscriminatingFunction),
                                           KW.Specializers, CL.List (StandardGenericFunction)));
     
            CL.AddMethod (ComputeApplicableMethods,
                          CL.MakeInstance (GenericFunctionMethodClass (ComputeApplicableMethods),
                                           KW.Name, QuoteMethodComputeApplicableMethods,
                                           KW.StudlyName, "Method:ComputeApplicableMethods",
                                           KW.Procedure, new ComputeApplicableMethodsMethodSignature (realComputeApplicableMethods),
                                           KW.Specializers, CL.List (StandardGenericFunction)));

            CL.AddMethod (ComputeEffectiveMethod,
                          CL.MakeInstance (GenericFunctionMethodClass (ComputeEffectiveMethod),
                                           KW.Name, QuoteMethodComputeEffectiveMethod,
                                           KW.StudlyName, "Method:ComputeEffectiveMethod",
                                           KW.Procedure, new ComputeEffectiveMethodMethodSignature (realComputeEffectiveMethod),
                                           KW.Specializers, CL.List (StandardGenericFunction)));

            CL.AddMethod (ComputeMethodMoreSpecific,
                          CL.MakeInstance (GenericFunctionMethodClass (ComputeMethodMoreSpecific),
                                           KW.Name, QuoteMethodComputeMethodMoreSpecific,
                                           KW.StudlyName, "Method:ComputeMethodMoreSpecific",
                                           KW.Procedure, new ComputeMethodMoreSpecificMethodSignature (realComputeMethodMoreSpecific),
                                           KW.Specializers, CL.List (StandardGenericFunction)));

            // Now we can add methods.
            CL.AddMethod (DefaultInitargs,
                          CL.MakeInstance (GenericFunctionMethodClass (DefaultInitargs),
                                           KW.Name, QuoteMethodDefaultInitargs,
                                           KW.StudlyName, "Method:DefaultInitargs",
                                           KW.Procedure, new DefaultInitargsMethodSignature (methodDefaultInitargs),
                                           KW.Specializers, CL.List (ClosClass)));  

            // Make sure we can inspect methods for duplicates when adding.
            CL.AddMethod (MethodQualifier,
                          CL.MakeInstance (GenericFunctionMethodClass (MethodQualifier),
                                           KW.Name, QuoteMethodMethodQualifier,
                                           KW.StudlyName, "Method:MethodQualifier",
                                           KW.Procedure, new MethodQualifierMethodSignature (methodMethodQualifier),
                                           KW.Specializers, CL.List (StandardMethod)));

            CL.AddMethod (MethodSpecializers,
              CL.MakeInstance (GenericFunctionMethodClass (MethodSpecializers),
                               KW.Name, QuoteMethodMethodSpecializers,
                               KW.StudlyName, "Method:MethodSpecializers",
                               KW.Procedure, new MethodSpecializersMethodSignature (methodMethodSpecializers),
                               KW.Specializers, CL.List (StandardMethod)));


            CL.AddMethod (ComputeClassPrecedenceList,
                          CL.MakeInstance (GenericFunctionMethodClass (ComputeClassPrecedenceList),
                                           KW.Procedure, new computeClassPrecedenceListMethodSignature (computeClassPrecedenceListMethod),
                                           KW.Specializers, CL.List (closClass)));

            CL.AddMethod (ComputeSlots,
                          CL.MakeInstance (GenericFunctionMethodClass (ComputeSlots),
                                           KW.Specializers, CL.List (closClass),
                                           KW.Procedure, new computeSlotsMethodSignature (computeSlotsMethod)));


            CL.AddMethod (InitializeInstance,
               CL.MakeInstance (GenericFunctionMethodClass (InitializeInstance),
                     KW.Name, QuoteMethodInitializeInstance,
                     KW.StudlyName, "Method:InitializeInstance",
                     KW.Procedure, new InitializeInstanceMethodSignature (methodInitializeInstanceAll),
                     KW.Specializers, CL.List ()));

            CL.AddMethod (InitializeInstance,
               CL.MakeInstance (GenericFunctionMethodClass (InitializeInstance),
                     KW.Name, QuoteMethodInitializeInstance,
                     KW.StudlyName, "Method:InitializeInstance",
                     KW.Procedure, new InitializeInstanceMethodSignature (methodInitializeInstanceStdObj),
                     KW.Specializers, CL.List (StandardObjectClass)));

            CL.AddMethod (InitializeInstance,
               CL.MakeInstance (GenericFunctionMethodClass (InitializeInstance),
                     KW.Name, QuoteMethodInitializeInstance,
                     KW.StudlyName, "Method:InitializeInstance",
                     KW.Procedure, new InitializeInstanceMethodSignature (methodInitializeInstanceStdClass),
                     KW.Specializers, CL.List (closClass)));

            CL.AddMethod (InitializeInstance,
                CL.MakeInstance (GenericFunctionMethodClass (InitializeInstance),
                                 KW.Name, QuoteMethodInitializeInstance,
                                 KW.StudlyName, "Method:InitializeInstance",
                                 KW.Procedure, new InitializeInstanceMethodSignature (methodInitializeInstanceStdGeneric),
                                 KW.Specializers, CL.List (StandardGenericFunction)));

            // InitializeInstance StandardMethod
            CL.AddMethod (InitializeInstance,
      CL.MakeInstance (GenericFunctionMethodClass (InitializeInstance),
                       KW.Name, QuoteMethodInitializeInstance,
                       KW.StudlyName, "Method:InitializeInstance",
                       KW.Procedure, new InitializeInstanceMethodSignature (methodInitializeInstanceStdMethod),
                       KW.Specializers, CL.List (StandardMethod)));

            CL.AddMethod (AllocateInstance,
                          CL.MakeInstance (GenericFunctionMethodClass (AllocateInstance),
                                           KW.Name, QuoteMethodAllocateInstance,
                                           KW.StudlyName, "Method:AllocateInstance",
                                         KW.Procedure, new AllocateInstanceMethodSignature (methodAllocateStandardInstance),
                                           KW.Specializers, CL.List (StandardClass)));

            CL.AddMethod (AllocateInstance,
                          CL.MakeInstance (GenericFunctionMethodClass (AllocateInstance),
                                           KW.Name, QuoteMethodAllocateInstance,
                                           KW.StudlyName, "Method:AllocateInstance",
                                           KW.Procedure, new AllocateInstanceMethodSignature (methodAllocateFuncallableInstance),
                                           KW.Specializers, CL.List (FuncallableStandardClass)));


            return true;
        }



        static readonly bool bootstrap_step2 = bootstrapStep2Function ();

        delegate object MakeInstanceMethodSignature (NextMethodFunction callNextMethod, StandardObject closClass, params object [] initargs);
        static object makeInstanceMethod (NextMethodFunction callNextMethod, StandardObject closClass, params object [] initargs)
        {
            Cons initargsList = (Cons) DefaultInitargs (closClass, Cons.SubvectorToList (initargs, 0, initargs.Length));
            StandardObject instance = (StandardObject) CL.Apply (AllocateInstance, closClass, initargsList);
            CL.Apply (InitializeInstance, instance, initargsList);
            return instance;
        }

        static bool bootstrapStep3Function ()
        {
            // Fixup the generic for make.
            StandardObject newMakeInstance = (StandardObject) CL.MakeInstance (StandardGenericFunction,
                                                                               KW.Name, QuoteMakeInstance,
                                                                               KW.StudlyName, "MakeInstance",
                                                                               KW.LambdaList, CL.List (QuoteClass, QuoteAndRest, QuoteInitargs));
            AddMethod (newMakeInstance,
                       CL.MakeInstance (StandardMethod,
                                        KW.Name, QuoteNewMakeInstanceMethod,
                                        KW.StudlyName, "Method:MakeInstance",
                                        KW.Specializers, CL.List (ClosClass),
                                        KW.Procedure, new MakeInstanceMethodSignature (makeInstanceMethod)));


            ManifestInstance oldMakeInstance = (ManifestInstance) CL.MakeInstance.Target;
            ManifestInstance newInstance = (ManifestInstance) newMakeInstance.Target;
            oldMakeInstance.Class = newInstance.Class;
            oldMakeInstance.Slots = newInstance.Slots;
            oldMakeInstance.OnFuncall = newInstance.OnFuncall;
            return true;
        }

        static readonly bool bootstrap_step3 = bootstrapStep3Function ();

        static StandardObject sharedInitialize =
     (StandardObject) CL.MakeInstance (StandardGenericFunction,
               KW.Name, QuoteDefaultInitargs,
               KW.StudlyName, "SharedInitialize",
               KW.LambdaList, CL.List (QuoteInstance, QuoteSlotNames, QuoteAndRest, QuoteInitargs));

        static bool bootstrapFixupClassOf ()
        {
            Type t = typeof (StandardObject);
            Type t1 = t.GetType ();
            while (t != t1) {
                t = t1;
                t1 = t1.GetType ();
            }

            StandardObject builtInTypeClass =
                (StandardObject) CL.MakeInstance (StandardClass,
                                                  KW.DirectSuperclasses, CL.List (builtInClass),
                                                  KW.DotnetType, t1,
                                                  KW.StudlyName, t1.Name);

            BuiltInClassDictionary.Add (t1, builtInTypeClass);

            classOf = new ClassOfFunction (delegate (object instance)
            {
                StandardObject probe = instance as StandardObject;
                if (probe != null)
                    return probe.Class ();
                Type ti = instance.GetType ();
                if (BuiltInClassDictionary.TryGetValue (t, out probe))
                    return probe;
                StandardObject super = ClassOf (t);
                probe = (StandardObject) CL.MakeInstance (builtInClass,
                                                          KW.DirectSuperclasses, CL.List (super),
                                                          KW.DotnetType, ti,
                                                          KW.StudlyName, ti.Name);
                BuiltInClassDictionary.Add (ti, probe);
                return probe;
            });

            return true;
        }


        static bool bootstrapFixupClassOfResult = bootstrapFixupClassOf ();

        class MethodAdder
        {
            StandardObject oldAdder;

            MethodAdder (StandardObject oldAdder)
            {
                this.oldAdder = oldAdder;
            }

            string ComputeMethodName (Cons specs, Symbol genericName)
            {
                throw new NotImplementedException ("Compute method name");
            }


            object AddMethod (NextMethodFunction callNextMethod, StandardObject generic, StandardObject method)
            {
                Arity genericArity = internalGenericArity (method);
                Arity methodArity = internalMethodArity (method);
                if (methodArity == null)
                    internalSetMethodArity (method, genericArity);
                if (!(methodArity == null
                    || methodArity == genericArity
                    || (methodArity.IsNary
                        && (genericArity.MinimumArgumentCount >= methodArity.MinimumArgumentCount))))
                    throw new NotImplementedException ("arity mismatch");

                Symbol name = internalMethodName (method);
                if (name == null || name == QuoteAnonymous)
                    internalSetMethodName (method, 
                        ComputeMethodName (internalMethodSpecializers (method),
                                           internalGenericName (generic)));
                return oldAdder (generic, method);
            }

            static public AddMethodSignature Create (StandardObject oldAdder)
            {
                MethodAdder ma = new MethodAdder (oldAdder);
                return (AddMethodSignature) Delegate.CreateDelegate (typeof (AddMethodSignature),
                    ma,
                    typeof (MethodAdder).GetMethod ("AddMethod", BindingFlags.NonPublic | BindingFlags.Instance));
            }
        }

        static bool bootstrapStepFixAddMethodFunction ()
        {
            StandardObject oldAddMethod = addMethod;

            addMethod = (StandardObject) CL.MakeInstance (StandardGenericFunction,
                                                           KW.Name, QuoteAddMethod,
                                                           KW.StudlyName, "AddMethod",
                                                           KW.LambdaList, CL.List (QuoteGenericFunction, QuoteMethod));

             oldAddMethod (addMethod,
                CL.MakeInstance (GenericFunctionMethodClass (addMethod),
                                 KW.StudlyName, "METHOD:add-method",
                                 KW.Specializers, CL.List (StandardGenericFunction, StandardMethod),
                                 KW.Procedure, MethodAdder.Create (oldAddMethod)));
            return true;
        }


        static readonly bool bootstrap_step_fix_add_method = bootstrapStepFixAddMethodFunction ();

        delegate StandardObject EnsureGenericFunctionSignatureSymbol (Symbol name, params object [] keyargs);
        static StandardObject ensureGenericFunctionMethodSymbol (Symbol name, params object [] keyargs)
        {
            throw new NotImplementedException();
        }

        delegate StandardObject EnsureGenericFunctionSignatureList (Cons name, params object [] keyargs);
        static StandardObject ensureGenericFunctionMethodList (Cons name, params object [] keyargs)
        {
            throw new NotImplementedException ();
        }


        static readonly StandardObject ensureGenericFunction =
                 (StandardObject) CL.MakeInstance (StandardGenericFunction,
                                                    KW.Name, QuoteEnsureGenericFunction,
                                                    KW.StudlyName, "EnsureGenericFunction",
                                                    KW.LambdaList,               
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
                                                     KW.Method, CL.MakeInstance (StandardMethod,
                                                                                 KW.Name, QuoteMethodEnsureGenericFunction,
                                                                                 KW.StudlyName, "METHOD:ensure-generic-function",
                                                                                 KW.Specializers, CL.List (typeof (Lisp.Symbol)),
                                                                                 KW.Procedure, new EnsureGenericFunctionSignatureSymbol (ensureGenericFunctionMethodSymbol)),
                                                     KW.Method, CL.MakeInstance (StandardMethod,
                                                                                 KW.Name, QuoteMethodEnsureGenericFunction,
                                                                                 KW.StudlyName, "METHOD:ensure-generic-function",
                                                                                 KW.Specializers, CL.List (typeof (Lisp.Cons)),
 
                                                                                 
                                                                                KW.Procedure, new EnsureGenericFunctionSignatureList(ensureGenericFunctionMethodList)));
 
        delegate bool ValidateSuperclassMethodSignature (NextMethodFunction callNextMethod, StandardObject closClass, StandardObject superclass);
        static bool validateSuperclassMethod (NextMethodFunction callNextMethod, StandardObject closClass, StandardObject superclass)
        {
            throw new NotImplementedException ("validate superclass");
        }

        static readonly StandardObject validateSuperclass =
            (StandardObject) CL.MakeInstance (StandardGenericFunction,
                                              KW.Name, QuoteValidateSuperclass,
                                              KW.StudlyName, "ValidateSuperclass",
                                              KW.LambdaList, CL.List (QuoteClass, QuoteSuperclass),
                                              KW.Method, CL.MakeInstance (StandardMethod,
                                                                          KW.Specializers, CL.List (closClass, closClass),
                                                                          KW.Procedure, new ValidateSuperclassMethodSignature (validateSuperclassMethod)));

        #region ExportedGenerics

        static public StandardObject AddMethod
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return addMethod;
            }
        }

        static public StandardObject AllocateInstance
        {
            [DebuggerStepThrough]
            get
            {
                return allocateInstance;
            }
        }

        static public StandardObject ClassName
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return className;
            }
        }

        static public StandardObject ClosClass
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return closClass;
            }
        }

 

        static public StandardObject ComputeApplicableMethods
        {
            [DebuggerStepThrough]
            get
            {
                return computeApplicableMethods;
            }
        }

        static public StandardObject ComputeClassPrecedenceList
        {
            [DebuggerStepThrough]
            get
            {
                return computeClassPrecedenceList;
            }
        }

        static public StandardObject ComputeDiscriminatingFunction
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return computeDiscriminatingFunction;
            }
        }

        static public StandardObject ComputeEffectiveMethod
        {
            [DebuggerStepThrough]
            get
            {
                return computeEffectiveMethod;
            }
        }


        static public StandardObject ComputeMethodMoreSpecific
        {
            [DebuggerStepThrough]
            get
            {
                return computeMethodMoreSpecific;
            }
        }

        static public StandardObject ComputeSlots
        {
            [DebuggerStepThrough]
            get
            {
                return computeSlots;
            }
        }


        static public StandardObject DefaultInitargs
        {
            [DebuggerStepThrough]
            get
            {
                return defaultInitargs;
            }
        }

        static public StandardObject EnsureGenericFunction
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return ensureGenericFunction;
            }
        }

        static public StandardObject FuncallableStandardClass
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return funcallableStandardClass;
            }
        }

        static public StandardObject FuncallableStandardObject
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return funcallableStandardObject;
            }
        }

        static public StandardObject Function
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return function;
            }
        }

        static public StandardObject GenericFunction
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return genericFunction;
            }
        }

        static public StandardObject GenericFunctionName
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return genericFunctionName;
            }
        }

        static public StandardObject GenericFunctionMethodClass
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return genericFunctionMethodClass;
            }
        }

        static public StandardObject InitializeInstance
        {
            [DebuggerStepThrough]
            get
            {
                return initializeInstance;
            }
        }

        static public StandardObject MakeInstance
        {
//            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return makeInstance;
            }
        }

        static public StandardObject Metaobject
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return metaobject;
            }
        }

        static public StandardObject Method
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return method;
            }
        }

        static public StandardObject MethodQualifier
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return methodQualifier;
            }
        }

        static public StandardObject MethodSpecializers
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return methodSpecializers;
            }
        }

        static public StandardObject SharedInitialize
        {
            [DebuggerStepThrough]
            get
            {
                return sharedInitialize;
            }
        }

        static public StandardObject Specializer
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return specializer;
            }
        }

        static public StandardObject StandardClass
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return standardClass;
            }
        }

        static public StandardObject StandardGenericFunction
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return standardGenericFunction;
            }
        }

        static public StandardObject StandardMethod
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return standardMethod;
            }
        }

        static object StandardInstanceAccess (StandardObject instance, int location)
        {
            return instance.InstanceRef (location);
        }

        static object SetStandardInstanceAccess (StandardObject instance, int location, object newValue)
        {
            return instance.InstanceSet (location, newValue);
        }

        static object StandardInstanceChange (StandardObject instance, int location, SlotChangingFunction change)
        {
            return instance.InstanceChange (location, change);
        }

        static public StandardObject StandardObjectClass
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return standardObjectClass;
            }
        }

        static public StandardObject Top
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return top;
            }
        }

        static public StandardObject ValidateSuperclass
        {
            [DebuggerStepThrough]
            get
            {
                return validateSuperclass;
            }
        }

        #endregion ExportedGenerics

        static public string PrintStandardObject (object thing)
        {
            return "A standard object";
        }

        static bool internalIsSingleton (object whatever)
        {
            return false;
        }

        static Symbol closSymbol (string name)
        {
            return closPackage.Intern (name);
        }
    }
}

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Lisp
{
    public delegate object StandardObject (params object [] arguments);
 
    class ManifestInstance
    {
        static readonly object UnboundSlotValue = Package.Sys.Intern ("[Unbound Slot Value]");
        static int serialNumberCounter = 0;
        int serialNumber;
        StandardObject closClass;
        object [] slotVector;

        public ManifestInstance (StandardObject closClass)
        {
            this.closClass = closClass;
            this.serialNumber = nextSerialNumber ();
        }

        // Used in bootstrapping the class class.
        public ManifestInstance (int slotCount)
        {
            this.slotVector = new object [slotCount];
            for (int i = 0; i < slotCount; i++) {
                this.slotVector [i] = UnboundSlotValue;
            }
            this.serialNumber = nextSerialNumber ();
        }

        // Bootstrap method
        public ManifestInstance (StandardObject closClass, int slotCount)
        {
            this.closClass = closClass;
            this.slotVector = new object [slotCount];
            for (int i = 0; i < slotCount; i++) {
                this.slotVector[i] = UnboundSlotValue;
            }
            this.serialNumber = nextSerialNumber();
        }

        static int nextSerialNumber () {
            return serialNumberCounter++;
        }

        public ManifestInstance (StandardObject iclass, object [] slotVector)
        {
            this.closClass = iclass;
            this.slotVector = slotVector;
            this.serialNumber = nextSerialNumber();
        }

        public StandardObject Class
        {
            get
            {
                return this.closClass;
            }
            set
            {
                this.closClass = value;
            }
        }

        public object InstanceRef (int index)
        {
            return this.slotVector [index];
        }

        public object InstanceSet (int index, object newValue)
        {
            this.slotVector [index] = newValue;
            return newValue;
        }

        object DefaultInstanceMethod (params object [] arguments)
        {
            throw new NotImplementedException ("Applicaton of non-funcallable instance.");
        }
    }

    delegate object FuncallableInstanceProcedure (object [] arguments);

    class FuncallableManifestInstance : ManifestInstance
    {
        FuncallableInstanceProcedure procedure;

        public FuncallableManifestInstance (StandardObject closClass, object [] slots)
            : base (closClass, slots)
        {
            this.procedure = (FuncallableInstanceProcedure) UninitializedFuncallableInstanceMethod;
        }

        static object UninitializedFuncallableInstanceMethod (object [] arguments)
        {
            throw new NotImplementedException ("Uninitialized funcallable instance method " + arguments.ToString ());
        }

        object DefaultFuncallableInstanceMethod (object [] arguments)
        {
            return this.procedure (arguments);
        }

        public FuncallableInstanceProcedure InstanceFunction
        {
           set {
               this.procedure = value;
               }
        }
    }

    class CLOS
    {
        private CLOS ()
        {
        }

        static Package closPackage = Package.Clos;

        static System.Reflection.MethodInfo defaultInstanceMethod =
            typeof (ManifestInstance)
              .GetMethod ("DefaultInstanceMethod",
                          System.Reflection.BindingFlags.Instance
                          | System.Reflection.BindingFlags.NonPublic);

        static System.Reflection.MethodInfo defaultFuncallableInstanceMethod =
            typeof (FuncallableManifestInstance)
              .GetMethod ("DefaultFuncallableInstanceMethod",
                          System.Reflection.BindingFlags.Instance
                          | System.Reflection.BindingFlags.NonPublic);

        static Symbol QuoteList = Package.CommonLisp.FindSymbol ("LIST");
        static Symbol QuoteDirectSlots = closSymbol ("DIRECT-SLOTS");
        static Symbol QuoteDirectSuperclasses = closSymbol ("DIRECT-SUPERCLASSES");
        static Symbol QuoteName = closSymbol ("NAME");

        // ClassOf
        delegate StandardObject ClassOfFunction (object obj);

        static ClassOfFunction classOf = 
           new ClassOfFunction (delegate (object instance) {
                                   StandardObject probe = instance as StandardObject;
                                   return (probe == null) 
                                     ? Top
                                     : ((ManifestInstance)(probe.Target)).Class;
                                   });

        static ClassOfFunction ClassOf
        {
            get
            {
                return classOf;
            }
        }

        // MOP
        static Cons SLOTS_OF_STANDARD_CLASS =
              CL.List (CL.List (closSymbol ("DEFAULT-INITARGS")),
                       CL.List (closSymbol ("DIRECT-DEFAULT-INITARGS")),
                       CL.List (QuoteDirectSlots),
                       CL.List (closSymbol ("DIRECT-SUBCLASSES")),
                       CL.List (QuoteDirectSuperclasses),
                       CL.List (closSymbol ("FINALIZEDP")),
                       CL.List (QuoteName),
                       CL.List (closSymbol ("CLASS-PRECEDENCE-LIST")),
                       CL.List (closSymbol ("CLASS-PROTOTYPE")),
                       CL.List (closSymbol ("GETTERS-AND-SETTERS")),
                       CL.List (closSymbol ("CLASS-SLOTS")));

        // Bootstrapping
        static private StandardObject bootstrapAllocateFuncallableInstance (StandardObject closClass, object [] slots)
        {
            return (StandardObject) Delegate.CreateDelegate (typeof (StandardObject),
                                                             new FuncallableManifestInstance (closClass, slots),
                                                             defaultFuncallableInstanceMethod);
        }

        static private StandardObject bootstrapMakeClass (Cons directSuperclasses, Cons directSlots, Symbol name)
        {
            StandardObject instance = 
               (StandardObject)
               Delegate.CreateDelegate (typeof (StandardObject),
                                        new ManifestInstance (StandardClass, CL.Length (SLOTS_OF_STANDARD_CLASS)),
                                        defaultInstanceMethod);
            // internalSetClassDefaultInitargs (instance);
            // internalSetClassDirectDefaultInitargs (instance);
            internalSetClassDirectSlots (instance, directSlots);
            // internalSetClassDirectSubclasses (instance);
            internalSetClassDirectSuperclasses (instance, directSuperclasses );
            // internalSetClassIsFinalized (instance);
            internalSetClassName (instance, name);
            // internalSetClassPrecedenceList (instance);
            // internalSetClassPrototype (instance);
            // internalSetClassGettersAndSetters(instance);
            // internalSetClassSlots(instance);
            return instance;
        }

        static private StandardObject bootstrapMakeGenericFunction (Symbol name, Cons lambdaList, Cons methods)
        {
            return bootstrapAllocateFuncallableInstance (StandardGenericFunction,
                                                         new object [] {name, 
                                                                        lambdaList
                                                                        });
        }

        static private StandardObject bootstrapMakeGenericFunction (Symbol name, Cons lambdaList)
        {
            return bootstrapAllocateFuncallableInstance (StandardGenericFunction,
                                                         new object [] {name, 
                                                                        lambdaList
                                                                        });
        }

        static private StandardObject bootstrapMakeMethod (Cons specializers)
        {
            return bootstrapAllocateFuncallableInstance (StandardMethod,
                                                         new object [] {//name, 
                                                                        // qualifiers
                                                                        });
        }

        static private StandardObject bootstrapMakeStandardClass ()
        {
            ManifestInstance manifestInstance = new ManifestInstance (CL.Length (SLOTS_OF_STANDARD_CLASS));
            StandardObject instance =
               (StandardObject) Delegate.CreateDelegate (typeof (StandardObject),
                                                         manifestInstance,
                                                         defaultInstanceMethod);
            manifestInstance.Class = instance;
            return instance;
        }

        // Internal Helpers
        static Cons lookupSlotInfo (StandardObject closClass, Symbol slotName)
        {
            Cons probe = CL.Assq (slotName, internalClassGettersAndSetters (closClass));
            if (probe == null) throw new NotImplementedException();
            return probe;
        }

        delegate object SlotWriter (StandardObject obj, object newValue);

        static object internalSlotSet (StandardObject obj, Symbol slotName, object value)
        {
            return ((SlotWriter)(CL.Caddr (lookupSlotInfo (ClassOf (obj), slotName))))(obj, value);
        }

        static SlotWriter internalSetClassDefaultInitargs =
          new SlotWriter (delegate (StandardObject obj, object newValue) { throw new NotImplementedException(); });
        static SlotWriter internalSetClassDirectDefaultInitargs =
          new SlotWriter (delegate (StandardObject obj, object newValue) { throw new NotImplementedException(); });
        static SlotWriter internalSetClassDirectSlots =
          new SlotWriter (delegate (StandardObject obj, object newValue) { return internalSlotSet (obj, QuoteDirectSlots, newValue); });
        static SlotWriter internalSetClassDirectSubclasses =
          new SlotWriter (delegate (StandardObject obj, object newValue) { throw new NotImplementedException(); });
        static SlotWriter internalSetClassDirectSuperclasses =
          new SlotWriter (delegate (StandardObject obj, object newValue) { return internalSlotSet (obj, QuoteDirectSuperclasses, newValue); });
        static SlotWriter internalSetClassIsFinalized =
          new SlotWriter (delegate (StandardObject obj, object newValue) { throw new NotImplementedException(); });
        static SlotWriter internalSetClassName =
          new SlotWriter (delegate (StandardObject obj, object newValue) { return internalSlotSet (obj, QuoteName, newValue); });
        static SlotWriter internalSetClassPrecedenceList =
          new SlotWriter (delegate (StandardObject obj, object newValue) { throw new NotImplementedException(); });
        static SlotWriter internalSetClassPrototype =
          new SlotWriter (delegate (StandardObject obj, object newValue) { throw new NotImplementedException(); });
        static SlotWriter internalSetClassGettersAndSetters =
          new SlotWriter (delegate (StandardObject obj, object newValue) { throw new NotImplementedException(); });
        static SlotWriter internalSetClassSlots =
          new SlotWriter (delegate (StandardObject obj, object newValue) { throw new NotImplementedException(); });

        // Classes
        static readonly StandardObject standardClass = bootstrapMakeStandardClass ();

        static StandardObject StandardClass
        {
            get
            {
                return standardClass;
            }
        }

        delegate object MapFunction (Cons arguments);
        delegate object SlotReader (StandardObject obj);
        delegate object SlotChanger (StandardObject obj, Delegate func);

        //// Bootstrap step
        static readonly Cons GettersAndSettersForClass =
            (Cons) CL.Map (QuoteList,
                           new MapFunction (delegate (Cons slotInfo)
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
                            new SlotChanger (delegate (StandardObject obj, Delegate func)
            {
                return SetStandardInstanceAccess (obj, index, func.DynamicInvoke (StandardInstanceAccess (obj, index)));
            })
                            );
        }),
                           SLOTS_OF_STANDARD_CLASS);

        static object bootstrapStep0 =
           ((SlotWriter) (CL.Caddr (CL.Assq (closSymbol ("GETTERS-AND-SETTERS"), GettersAndSettersForClass)))) (StandardClass, GettersAndSettersForClass);

        static SlotReader internalClassGettersAndSetters =
           (SlotReader) (CL.Cadr (CL.Assq (closSymbol ("GETTERS-AND-SETTERS"), GettersAndSettersForClass)));


        static readonly StandardObject Top =
                bootstrapMakeClass (null, // direct supers
                                    null, // direct slots
                                    closSymbol ("TOP") // name
                                    );

        static readonly StandardObject genericFunction =
            (StandardObject)
            Delegate.CreateDelegate (typeof (StandardObject),
                                     null,
                                     defaultInstanceMethod);

        static public StandardObject GenericFunction
        {
            get
            {
                return genericFunction;
            }
        }

        static readonly StandardObject standardGenericFunction =
            (StandardObject)
            Delegate.CreateDelegate (typeof (StandardObject),
                                     null,
                                     defaultInstanceMethod);

        static public StandardObject StandardGenericFunction
        {
            get
            {
                return standardGenericFunction;
            }
        }

        static readonly StandardObject StandardMethod =
            (StandardObject)
            Delegate.CreateDelegate (typeof (StandardObject),
                                     null,
                                     defaultInstanceMethod);

        static public StandardObject standardMethod
        {
            get
            {
                return standardMethod;
            }
        }

        static readonly StandardObject nullClass =
            (StandardObject)
            Delegate.CreateDelegate (typeof (StandardObject),
                                     null,
                                     defaultInstanceMethod);

        static public StandardObject NullClass
        {
            get
            {
                return nullClass;
            }
        }

        // Generic Functions
        static readonly StandardObject addDependent = bootstrapMakeGenericFunction (closSymbol ("ADD-DEPENDENT"),
                                                                                    CL.List (closSymbol ("METAOBJECT"),
                                                                                            closSymbol ("DEPENDENT")));

        static public StandardObject AddDependent
        {
            get
            {
                return addDependent;
            }
        }

        static readonly StandardObject addDirectMethod = bootstrapMakeGenericFunction (closSymbol ("ADD-DIRECT-METHOD"), CL.List (closSymbol ("SPECIALIZER"), closSymbol ("METHOD")));

        static public StandardObject AddDirectMethod
        {
            get
            {
                return addDirectMethod;
            }
        }

        static readonly StandardObject addDirectSubclass = bootstrapMakeGenericFunction (closSymbol ("ADD-DIRECT-SUBCLASS"), CL.List (closSymbol ("SUPERCLASS"), closSymbol ("SUBCLASS")));

        static public StandardObject AddDirectSubclass
        {
            get
            {
                return addDirectSubclass;
            }
        }

        static readonly StandardObject addMethod = bootstrapMakeGenericFunction (closSymbol ("ADD-METHOD"), CL.List (closSymbol ("GENERIC-FUNCTION"), closSymbol ("METHOD")));

        static public StandardObject AddMethod
        {
            get
            {
                return addMethod;
            }
        }

        static readonly StandardObject allocateInstance = bootstrapMakeGenericFunction (closSymbol ("ALLOCATE-INSTANCE"), CL.List (closSymbol ("CLASS"), Package.CommonLisp.Intern ("&REST"), closSymbol ("INITARGS")));

        static public StandardObject AllocateInstance
        {
            get
            {
                return allocateInstance;
            }
        }

        static readonly StandardObject classDefaultInitargs = bootstrapMakeGenericFunction (closSymbol ("CLASS-DEFAULT-INITARGS"), CL.List (closSymbol ("CLASS")));

        static public StandardObject ClassDefaultInitargs
        {
            get
            {
                return classDefaultInitargs;
            }
        }

        static readonly StandardObject classDirectDefaultInitargs = bootstrapMakeGenericFunction (closSymbol (""), CL.List (closSymbol ("CLASS")));

        static public StandardObject ClassDirectDefaultInitargs
        {
            get
            {
                return classDirectDefaultInitargs;
            }
        }

        static readonly StandardObject classDirectSlots = bootstrapMakeGenericFunction (closSymbol (""), CL.List (closSymbol ("CLASS")));

        static public StandardObject ClassDirectSlots
        {
            get
            {
                return classDirectSlots;
            }
        }

        static readonly StandardObject classDirectSubclasses = bootstrapMakeGenericFunction (closSymbol (""), CL.List (closSymbol ("CLASS")));

        static public StandardObject ClassDirectSubclasses
        {
            get
            {
                return classDirectSubclasses;
            }
        }

        static readonly StandardObject classDirectSuperclasses = bootstrapMakeGenericFunction (closSymbol (""), CL.List (closSymbol ("CLASS")));

        static public StandardObject ClassDirectSuperclasses
        {
            get
            {
                return classDirectSuperclasses;
            }
        }

        static readonly StandardObject classIsFinalized = bootstrapMakeGenericFunction (closSymbol (""), CL.List (closSymbol ("CLASS")));

        static public StandardObject ClassIsFinalized
        {
            get
            {
                return classIsFinalized;
            }
        }

        static readonly StandardObject className = bootstrapMakeGenericFunction (closSymbol (""), CL.List (closSymbol ("CLASS")));

        static public StandardObject ClassName
        {
            get
            {
                return className;
            }
        }

        static readonly StandardObject classPrecedenceList = bootstrapMakeGenericFunction (closSymbol ("CLASS-PRECEDENCE-LIST"), CL.List (closSymbol ("CLASS")));

        static public StandardObject ClassPrecedenceList
        {
            get
            {
                return classPrecedenceList;
            }
        }

        static readonly StandardObject setClassPrecedenceList = bootstrapMakeGenericFunction (closSymbol ("CLASS-PRECEDENCE-LIST"), CL.List (closSymbol ("CLASS")));
        static public StandardObject SetClassPrecedenceList
        {
            get
            {
                return setClassPrecedenceList;
            }
        }

        static readonly StandardObject classPrototype = bootstrapMakeGenericFunction (closSymbol (""), CL.List (closSymbol ("CLASS")));

        static public StandardObject ClassPrototype
        {
            get
            {
                return classPrototype;
            }
        }

        static readonly StandardObject classSlots = bootstrapMakeGenericFunction (closSymbol (""), CL.List (closSymbol ("CLASS")));

        static public StandardObject ClassSlots
        {
            get
            {
                return classSlots;
            }
        }

        static readonly StandardObject computeApplicableMethods = bootstrapMakeGenericFunction (closSymbol (""), CL.List (closSymbol ("GENERIC-FUNCTION"), closSymbol ("ARGUMENTS")));

        static public StandardObject ComputeApplicableMethods
        {
            get
            {
                return computeApplicableMethods;
            }
        }

        static readonly StandardObject computeApplicabelMethodsUsingClasses = bootstrapMakeGenericFunction (closSymbol (""), CL.List ());

        static public StandardObject ComputeApplicabelMethodsUsingClasses
        {
            get
            {
                return computeApplicabelMethodsUsingClasses;
            }
        }

        static readonly StandardObject computeClassPrecedenceList = bootstrapMakeGenericFunction (closSymbol (""), CL.List ());

        static public StandardObject ComputeClassPrecedenceList
        {
            get
            {
                return computeClassPrecedenceList;
            }
        }

        static readonly StandardObject computeDefaultInitargs = bootstrapMakeGenericFunction (closSymbol (""), CL.List ());

        static public StandardObject ComputeDefaultInitargs
        {
            get
            {
                return computeDefaultInitargs;
            }
        }

        static readonly StandardObject computeDiscriminatingFunction = bootstrapMakeGenericFunction (closSymbol (""), CL.List ());

        static public StandardObject ComputeDiscriminatingFunction
        {
            get
            {
                return computeDiscriminatingFunction;
            }
        }

        static readonly StandardObject computeEffectiveMethod = bootstrapMakeGenericFunction (closSymbol (""), CL.List ());

        static public StandardObject ComputeEffectiveMethod
        {
            get
            {
                return computeEffectiveMethod;
            }
        }

        static readonly StandardObject computeEffectiveSlotDefinition = bootstrapMakeGenericFunction (closSymbol (""), CL.List ());

        static public StandardObject ComputeEffectiveSlotDefinition
        {
            get
            {
                return computeEffectiveSlotDefinition;
            }
        }

        static readonly StandardObject computeSlots = bootstrapMakeGenericFunction (closSymbol (""), CL.List ());

        static public StandardObject ComputeSlots
        {
            get
            {
                return computeSlots;
            }
        }

        static readonly StandardObject directSlotDefinitionClass = bootstrapMakeGenericFunction (closSymbol (""), CL.List ());

        static public StandardObject DirectSlotDefinitionClass
        {
            get
            {
                return directSlotDefinitionClass;
            }
        }

        static readonly StandardObject effectiveSlotDefinitionClass = bootstrapMakeGenericFunction (closSymbol (""), CL.List ());

        static public StandardObject EffectiveSlotDefinitionClass
        {
            get
            {
                return effectiveSlotDefinitionClass;
            }
        }

        public static StandardObject EnsureClass (Symbol name, params object [] keywordArguments)
        {
            throw new NotImplementedException ();
        }

        static readonly StandardObject ensureClassUsingClass = bootstrapMakeGenericFunction (closSymbol (""), CL.List ());

        static public StandardObject EnsureClassUsingClass
        {
            get
            {
                return ensureClassUsingClass;
            }
        }

        // Ensure-generic-function would normally be defined thus:
        // (defgeneric ensure-generic-function (function-name &key 
        //                                      argument-precedence-order
        //                                      declare
        //                                      documentation
        //                                      environment
        //                                      generic-function-class
        //                                      lambda-list
        //                                      method-class
        //                                      method-combination)
        //    (:method (function-name &key 
        //                                      argument-precedence-order
        //                                      declare
        //                                      documentation
        //                                      environment
        //                                      generic-function-class
        //                                      lambda-list
        //                                      method-class
        //                                      method-combination)
        //       <method-body>))
        //
        // This would perform these steps:
        //  1. Check to see if function-name is bound.
        //  2. call ensure-generic-function to create the new function
        //  3. install the methods on the new gf. 

        static public StandardObject EnsureGenericFunction (object functionName, params object [] keywordArguments)
        {
            KeywordArgument<Environment> environment = new KeywordArgument<Environment> (KW.Environment);
            KeywordArgumentBase.ProcessKeywordArguments (
              new KeywordArgumentBase [] { environment },
              keywordArguments,
              true);

            Environment env = (environment.Supplied) ? environment.Value : (Environment) null;
            if (env.IsFbound ((Symbol) functionName)) {
                throw new NotImplementedException ("redefinition in ensure generic function");
            }

            switch (keywordArguments.Length) {
                case 0:
                    return (StandardObject) EnsureGenericFunctionUsingClass (null, functionName);
                case 2:
                    return (StandardObject) EnsureGenericFunctionUsingClass (null, functionName, keywordArguments [0], keywordArguments [1]);
                default:
                    throw new NotImplementedException ("ensuregenericfunction");
            }
        }

        static readonly StandardObject ensureGenericFunctionUsingClass =
            bootstrapMakeGenericFunction (closSymbol ("ENSURE-GENERIC-FUNCTION"),
                                      CL.List (closSymbol ("GENERIC-FUNCTION"),
                                                   closSymbol ("FUNCTION-NAME"),
                                               Package.CommonLisp.Intern ("&KEY"),
                                                   closSymbol ("ARGUMENT-PRECEDENCE-ORDER"),
                                                   closSymbol ("DECLARATIONS"),
                                                   closSymbol ("DOCUMENTATION"),
                                                   closSymbol ("GENERIC-FUNCTION-CLASS"),
                                                   closSymbol ("LAMBDA-LIST"),
                                                   closSymbol ("METHOD-CLASS"),
                                                   closSymbol ("METHOD-COMBINATION"),
                                                   closSymbol ("NAME"),
                                                   Package.CommonLisp.Intern ("&ALLOW-OTHER-KEYS")),
                                           CL.List ());

        static public StandardObject EnsureGenericFunctionUsingClass
        {
            get
            {
                return ensureGenericFunctionUsingClass;
            }
        }

        static object EqlSpecializerObject (StandardObject eqlSpecializer)
        {
            throw new NotImplementedException ();
        }

        static Cons ExtractLambdaList (object specializedLambdaList)
        {
            throw new NotImplementedException ();
        }

        static Cons ExtractSpecializerNames (object specializedLambdaList)
        {
            throw new NotImplementedException ();
        }

        static readonly StandardObject finalizeInheritance = bootstrapMakeGenericFunction (closSymbol (""), CL.List ());

        static public StandardObject FinalizeInheritance
        {
            get
            {
                return finalizeInheritance;
            }
        }

        static readonly StandardObject findMethodCombination = bootstrapMakeGenericFunction (closSymbol (""), CL.List ());

        static public StandardObject FindMethodCombination
        {
            get
            {
                return findMethodCombination;
            }
        }

        static object FuncallableStandardInstanceAccess (StandardObject instance, int location)
        {
            throw new NotImplementedException ();
        }

        static readonly StandardObject genericFunctionArgumentPrecedenceOrder = bootstrapMakeGenericFunction (closSymbol (""), CL.List ());

        static public StandardObject GenericFunctionArgumentPrecedenceOrder
        {
            get
            {
                return genericFunctionArgumentPrecedenceOrder;
            }
        }

        static readonly StandardObject genericFunctionDeclarations = bootstrapMakeGenericFunction (closSymbol (""), CL.List ());

        static public StandardObject GenericFunctionDeclarations
        {
            get
            {
                return genericFunctionDeclarations;
            }
        }

        static readonly StandardObject genericFunctionLambdaList = bootstrapMakeGenericFunction (closSymbol (""), CL.List ());

        static public StandardObject GenericFunctionLambdaList
        {
            get
            {
                return genericFunctionLambdaList;
            }
        }

        static readonly StandardObject genericFunctionMethodClass = bootstrapMakeGenericFunction (closSymbol (""), CL.List ());

        static public StandardObject GenericFunctionMethodClass
        {
            get
            {
                return genericFunctionMethodClass;
            }
        }

        static readonly StandardObject genericFunctionMethodCombination = bootstrapMakeGenericFunction (closSymbol (""), CL.List ());

        static public StandardObject GenericFunctionMethodCombination
        {
            get
            {
                return genericFunctionMethodCombination;
            }
        }

        static readonly StandardObject genericFunctionMethods = bootstrapMakeGenericFunction (closSymbol (""), CL.List ());

        static public StandardObject GenericFunctionMethods
        {
            get
            {
                return genericFunctionMethods;
            }
        }

        static readonly StandardObject genericFunctionName = bootstrapMakeGenericFunction (closSymbol (""), CL.List ());

        static public StandardObject GenericFunctionName
        {
            get
            {
                return genericFunctionName;
            }
        }

        static object InternEqlSpecializer (object eqlSpecializerObject)
        {
            throw new NotImplementedException ();
        }

        static readonly StandardObject makeInstance = bootstrapMakeGenericFunction (closSymbol ("MAKE-INSTANCE"), CL.List ());

        static public StandardObject MakeInstance
        {
            get
            {
                return makeInstance;
            }
        }

        static readonly StandardObject makeMethodLambda = bootstrapMakeGenericFunction (closSymbol (""), CL.List ());

        static public StandardObject MakeMethodLambda
        {
            get
            {
                return makeMethodLambda;
            }
        }

        static readonly StandardObject mapDependents = bootstrapMakeGenericFunction (closSymbol ("MAP-DEPENDENTS"), CL.List ());

        static public StandardObject MapDependents
        {
            get
            {
                return mapDependents;
            }
        }

        static readonly StandardObject methodFunction = bootstrapMakeGenericFunction (closSymbol ("METHOD-FUNCTION"), CL.List (closSymbol ("METHOD")));

        static public StandardObject MethodFunction
        {
            get
            {
                return methodFunction;
            }
        }

        static readonly StandardObject methodGenericFunction = bootstrapMakeGenericFunction (closSymbol ("METHOD-GENERIC-FUNCTION"), CL.List (closSymbol ("METHOD")));

        static public StandardObject MethodGenericFunction
        {
            get
            {
                return methodGenericFunction;
            }
        }

        static readonly StandardObject methodLambdaList = bootstrapMakeGenericFunction (closSymbol ("METHOD-LAMBDA-LIST"), CL.List (closSymbol ("METHOD")));

        static public StandardObject MethodLambdaList
        {
            get
            {
                return methodLambdaList;
            }
        }

        static readonly StandardObject methodSpecializers = bootstrapMakeGenericFunction (closSymbol ("METHOD-SPECIALIZERS"), CL.List (closSymbol ("METHOD")));

        static public StandardObject MethodSpecializers
        {
            get
            {
                return methodSpecializers;
            }
        }

        static readonly StandardObject methodQualifiers = bootstrapMakeGenericFunction (closSymbol ("METHOD-QUALIFIERS"), CL.List (closSymbol ("METHOD")));

        static public StandardObject MethodQualifiers
        {
            get
            {
                return methodQualifiers;
            }
        }

        static readonly StandardObject accessorMethodSlotDefinition = bootstrapMakeGenericFunction (closSymbol ("ACCESSOR-METHOD-SLOT-DEFINITION"), CL.List (closSymbol ("METHOD")));

        static public StandardObject AccessorMethodSlotDefinition
        {
            get
            {
                return accessorMethodSlotDefinition;
            }
        }

        static readonly StandardObject readerMethodClass = bootstrapMakeGenericFunction (closSymbol ("READER-METHOD-CLASS"), CL.List (closSymbol ("METHOD")));

        static public StandardObject ReaderMethodClass
        {
            get
            {
                return readerMethodClass;
            }
        }

        static readonly StandardObject removeDependent = bootstrapMakeGenericFunction (closSymbol ("REMOVE-DEPENDENT"), CL.List (closSymbol ("METHOD")));

        static public StandardObject RemoveDependent
        {
            get
            {
                return removeDependent;
            }
        }

        static readonly StandardObject removeDirectMethod = bootstrapMakeGenericFunction (closSymbol ("REMOVE-DIRECT-METHOD"), CL.List (closSymbol ("METHOD")));

        static public StandardObject RemoveDirectMethod
        {
            get
            {
                return RemoveDirectMethod;
            }
        }

        static readonly StandardObject removeDirectSubclass = bootstrapMakeGenericFunction (closSymbol ("REMOVE-DIRECT-SUBCLASS"), CL.List (closSymbol ("METHOD")));

        static public StandardObject RemoveDirectSubclass
        {
            get
            {
                return removeDirectSubclass;
            }
        }

        static readonly StandardObject removeMethod = bootstrapMakeGenericFunction (closSymbol ("REMOVE-METHOD"), CL.List (closSymbol ("METHOD")));

        static public StandardObject RemoveMethod
        {
            get
            {
                return removeMethod;
            }
        }

        static object setFuncallableInstanceFunction (StandardObject funcallableInstance, object function)
        {
            throw new NotImplementedException ();
        }

        static object setGenericFunctionName (object newName, StandardObject genericFunction)
        {
            throw new NotImplementedException ();
        }

        static readonly StandardObject setSlotValueUsingClass = bootstrapMakeGenericFunction (closSymbol ("SET-SLOT-VALUE-USING-CLASS"), CL.List ());

        static public StandardObject SetSlotValueUsingClass
        {
            get
            {
                return setSlotValueUsingClass;
            }
        }

        static readonly StandardObject slotIsBoundUsingClass = bootstrapMakeGenericFunction (closSymbol ("SLOT-IS-BOUND-USING-CLASS"), CL.List ());

        static public StandardObject SlotIsBoundUsingClass
        {
            get
            {
                return slotIsBoundUsingClass;
            }
        }

        static readonly StandardObject slotDefinitionAllocation = bootstrapMakeGenericFunction (closSymbol ("SLOT-DEFINITION-ALLOCATION"), CL.List (closSymbol ("SLOT-DEFINITION")));

        static public StandardObject SlotDefinitionAllocation
        {
            get
            {
                return slotDefinitionAllocation;
            }
        }

        static readonly StandardObject slotDefinitionInitargs = bootstrapMakeGenericFunction (closSymbol ("SLOT-DEFINITION-INITARGS"), CL.List (closSymbol ("SLOT-DEFINITION")));

        static public StandardObject SlotDefinitionInitargs
        {
            get
            {
                return slotDefinitionInitargs;
            }
        }

        static readonly StandardObject slotDefinitionInitform = bootstrapMakeGenericFunction (closSymbol ("SLOT-DEFINITION-INIT-FORM"), CL.List (closSymbol ("SLOT-DEFINITION")));

        static public StandardObject SlotDefinitionInitform
        {
            get
            {
                return slotDefinitionInitform;
            }
        }

        static readonly StandardObject slotDefinitionInitfunction = bootstrapMakeGenericFunction (closSymbol ("SLOT-DEFINITION-INIT-FUNCTION"), CL.List (closSymbol ("SLOT-DEFINITION")));

        static public StandardObject SlotDefinitionInitfunction
        {
            get
            {
                return slotDefinitionInitfunction;
            }
        }

        static readonly StandardObject slotDefinitionLocation = bootstrapMakeGenericFunction (closSymbol ("SLOT-DEFINITION-LOCATION"), CL.List (closSymbol ("SLOT-DEFINITION")));

        static public StandardObject SlotDefinitionLocation
        {
            get
            {
                return slotDefinitionLocation;
            }
        }

        static readonly StandardObject slotDefinitionName = bootstrapMakeGenericFunction (closSymbol ("SLOT-DEFINITION-NAME"), CL.List (closSymbol ("SLOT-DEFINITION")));

        static public StandardObject SlotDefinitionName
        {
            get
            {
                return slotDefinitionName;
            }
        }

        static readonly StandardObject slotDefinitionReaders = bootstrapMakeGenericFunction (closSymbol ("SLOT-DEFINITION-READERS"), CL.List (closSymbol ("SLOT-DEFINITION")));

        static public StandardObject SlotDefinitionReaders
        {
            get
            {
                return slotDefinitionReaders;
            }
        }

        static readonly StandardObject slotDefinitionWriters = bootstrapMakeGenericFunction (closSymbol ("SLOT-DEFINITION-WRITERS"), CL.List (closSymbol ("SLOT-DEFINITION")));

        static public StandardObject SlotDefinitionWriters
        {
            get
            {
                return slotDefinitionWriters;
            }
        }

        static readonly StandardObject slotDefinitionType = bootstrapMakeGenericFunction (closSymbol ("SLOT-DEFINITION-TYPE"), CL.List (closSymbol ("SLOT-DEFINITION")));

        static public StandardObject SlotDefinitionType
        {
            get
            {
                return slotDefinitionType;
            }
        }

        static readonly StandardObject slotMakunboundUsingClass = bootstrapMakeGenericFunction (closSymbol ("SLOT-MAKUNBOUND-USING-CLASS"), CL.List ());

        static public StandardObject SlotMakunboundUsingClass
        {
            get
            {
                return slotMakunboundUsingClass;
            }
        }

        static readonly StandardObject slotValueUsingClass = bootstrapMakeGenericFunction (closSymbol ("SLOT-VALUE-USING-CLASS"), CL.List ());

        static public StandardObject SlotValueUsingClass
        {
            get
            {
                return slotValueUsingClass;
            }
        }

        static readonly StandardObject specializerDirectGenericFunctions = bootstrapMakeGenericFunction (closSymbol ("SPECIALIZER-DIRECT-GENERIC-FUNCTIONS"), CL.List (closSymbol ("SPECIALIZER")));

        static public StandardObject SpecializerDirectGenericFunctions
        {
            get
            {
                return specializerDirectGenericFunctions;
            }
        }

        static readonly StandardObject specializerDirectMethods = bootstrapMakeGenericFunction (closSymbol ("SPECIALIZER-DIRECT-METHODS"), CL.List (closSymbol ("SPECIALIZER")));

        static public StandardObject SpecializerDirectMethods
        {
            get
            {
                return specializerDirectMethods;
            }
        }

        static object StandardInstanceAccess (StandardObject instance, int location)
        {
            return ((ManifestInstance) (instance.Target)).InstanceRef (location);
        }

        static object SetStandardInstanceAccess (StandardObject instance, int location, object newValue)
        {
            return ((ManifestInstance) (instance.Target)).InstanceSet (location, newValue);
        }

        static readonly StandardObject updateDependent = bootstrapMakeGenericFunction (closSymbol ("UPDATE-DEPENDENT"), CL.List ());

        static public StandardObject UpdateDependent
        {
            get
            {
                return updateDependent;
            }
        }

        static readonly StandardObject validateSuperclass = bootstrapMakeGenericFunction (closSymbol ("VALIDATE-SUPERCLASS"), CL.List ());

        static public StandardObject ValidateSuperclass
        {
            get
            {
                return validateSuperclass;
            }
        }

        static readonly StandardObject writerMethodClass = bootstrapMakeGenericFunction (closSymbol ("WRITER-METHOD-CLASS"), CL.List ());

        static public StandardObject WriterMethodClass
        {
            get
            {
                return WriterMethodClass;
            }
        }

        //////////////////////
        //static object [] [] SLOTS_OF_A_CLASS = {
        //                                         new object [] {Package.Sys.Intern ("CLASS-PRECEDENCE-LIST")},
        //                                         new object [] {Package.Sys.Intern ("DEFAULT-INITARGS")},
        //                                         new object [] {Package.Sys.Intern ("DIRECT-DEFAULT-INITARGS")}
        //                                      };
        //static int CLASS_SLOT_COUNT = 13;




        //private static StandardObject BootstrapAllocateInstance (StandardObject closClass)
        //{
        //    return (Lisp.StandardObject) Delegate.CreateDelegate (typeof (StandardObject),
        //        new ManifestInstance (closClass, CLASS_SLOT_COUNT),
        //        defaultInstanceMethod);
        //}

        //static Symbol anonymous = Package.Sys.Intern ("-anonymous-");
        //static Symbol Top = Package.Sys.Intern ("Top");
        //
        //static Symbol SymName = Package.Sys.Intern ("Name");

        //private static StandardObject BootstrapMakeInstance (params object [] arguments)
        //{
        //    object iclass = arguments [0];
        //    Cons initargs = Cons.SubvectorToList (arguments, 1, arguments.Length);
        //    if (iclass.Equals (ClassClass)) {
        //        StandardObject newInstance = BootstrapAllocateInstance ((StandardObject) iclass);
        //        Symbol name = (Symbol) Utility.GetArg (initargs, KW.Name, anonymous);
        //        InternalSetClassName (newInstance, name);
        //        throw new NotImplementedException ("Now what?");
        //        // return newInstance;
        //    }
        //    else {
        //        throw new NotImplementedException ();
        //    }
        //}

        //static StandardObject BootstrapClassOf (object obj)
        //{
        //    StandardObject inst = obj as StandardObject;
        //    if (inst != null) {
        //        return ((ManifestInstance) (inst.Target)).Class;
        //    }
        //    else {
        //        return TopClass;
        //    }

        //}

        //static Cons BootstrapInternalClassGettersAndSetters (StandardObject closClass)
        //{
        //    throw new NotImplementedException ();
        //}

        //static ClassOfProcedureType ClassOfProcedure = BootstrapClassOf;
        //static ClassGettersAndSettersProcedureType InternalClassGettersAndSettersProcedure = BootstrapInternalClassGettersAndSetters;

        //static StandardObject MakeInstanceProcedure = (Lisp.StandardObject)
        //    Delegate.CreateDelegate (typeof (StandardObject), null, typeof (CLOS).GetMethod ("BootstrapMakeInstance", System.Reflection.BindingFlags.Static | System.Reflection.BindingFlags.NonPublic));

        //static StandardObject ClassClass = (StandardObject)
        //    Delegate.CreateDelegate (typeof (StandardObject), new ManifestInstance (CLASS_SLOT_COUNT), defaultInstanceMethod);

        //delegate object Thunk (object [] arguments);

        //// Bootstrap step
        //static Cons GettersAndSettersForClass =
        //    (Cons) CL.Map (QuoteList, new Thunk (delegate (object [] slotInfo)
        //{
        //    Cons slotNames = (Cons) CL.Map (QuoteList, new Thunk (delegate (object [] slotInfo0)
        //    {
        //        return slotInfo0 [0];
        //    }), new object [] { SLOTS_OF_A_CLASS });
        //    int index = CL.Position (slotInfo [0], slotNames);
        //    throw new NotImplementedException ();
        //}),
        //                              new object [] { SLOTS_OF_A_CLASS });


        //static StandardObject TopClass = (StandardObject) MakeInstance (ClassClass, KW.DirectSuperclasses, null, KW.Name, Top);

        //static StandardObject MakeManifestInstance (StandardObject iclass, object [] slotVector)
        //{
        //    Type instanceType = typeof (StandardObject);
        //    ManifestInstance m = new ManifestInstance (iclass, slotVector);
        //    Type mitype = typeof (ManifestInstance);
        //    System.Reflection.MethodInfo meth = mitype.GetMethod ("DefaultInstanceMethod", System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.NonPublic);

        //    return (Lisp.StandardObject)
        //        Delegate.CreateDelegate (instanceType, m, meth);
        //}


        //static void InternalSetSlot (StandardObject closInstance, Symbol name, object newValue)
        //{
        //    StandardObject closClass = ClassOf (closInstance);
        //    Cons gettersAndSetters = InternalClassGettersAndSetters (closClass);
        //    throw new NotImplementedException ();
        //}

        //static void InternalSetClassName (StandardObject closClass, object name)
        //{
        //    InternalSetSlot (closClass, SymName, name);
        //}

        //public static ClassOfProcedureType ClassOf
        //{
        //    get
        //    {
        //        return ClassOfProcedure;
        //    }
        //}

        //static ClassGettersAndSettersProcedureType InternalClassGettersAndSetters
        //{
        //    get
        //    {
        //        return InternalClassGettersAndSettersProcedure;
        //    }
        //}

        static Symbol closSymbol (string name)
        {
            return closPackage.Intern (name);
        }

    }
}

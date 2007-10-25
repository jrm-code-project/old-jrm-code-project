using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Reflection;

namespace Lisp
{
    public delegate object SlotChangingFunction (object currentValue);
    public delegate T SlotChangingFunction<T> (T currentValue);

    public static class CLOS
    {
        // temporary to force initialization
        static object UnboundSlotValue = UnboundSlot.Value;

        public static object Init ()
        {
            return UnboundSlotValue;
        }

        #region StandardObjectExtensionMethods

        // Extension methods for StandardObject
        [DebuggerStepThrough]
        static StandardObject Class (this StandardObject obj)
        {
            return ((ManifestInstance) obj.Target).Class;
        }

        static void SetClass (this StandardObject obj, StandardObject closClass)
        {
            ((ManifestInstance) obj.Target).Class = closClass;
        }

        static object InstanceRef (this StandardObject obj, int index)
        {
            return ((ManifestInstance) obj.Target).Slots [index];
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

        public static T StandardInstanceAccess<T> (StandardObject instance, int location)
        {
            return instance.InstanceRef<T> (location);
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

        static int SerialNumber (this StandardObject obj)
        {
            return ((ManifestInstance) obj.Target).SerialNumber;
        }

        // Not an extension, but Instance-specific.
        static void setFuncallableInstanceFunction (StandardObject funcallableInstance, MethodInfo handler)
        {
            ((ManifestInstance) funcallableInstance.Target).OnFuncall = FuncallHandlerWrapper.Create (handler);
        }

        #endregion StandardObjectExtensionMethods

        #region ExportedGenerics
        static ClassOfFunction ClassOf
        {
            [DebuggerStepThrough]
            get
            {
                return classOf;
            }
        }

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

        public static StandardObject SetSlotValue
        {
            [DebuggerStepThrough]
            get
            {
                return setSlotValue;
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
            throw new NotImplementedException ();
        }

        public static T SlotValue<T> (StandardObject instance, Symbol slotName)
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
        static readonly Symbol QuoteClassSlots = closSymbol ("ClassSlots");
        static readonly Symbol QuoteComputeApplicableMethods = closSymbol ("COMPUTE-APPLICABLE-METHODS");
        static readonly Symbol QuoteComputeApplicableMethodsUsingClasses = closSymbol ("ComputeApplicableMethodsUsingClasses");
        static readonly Symbol QuoteComputeClassPrecedenceList = closSymbol ("COMPUTE-CLASS-PRECEDENCE-LIST");
        static readonly Symbol QuoteComputeDefaultInitargs = closSymbol ("ComputeDefaultInitargs");
        static readonly Symbol QuoteComputeDiscriminatingFunction = closSymbol ("COMPUTE-DISCRIMINATING-FUNCTION");
        static readonly Symbol QuoteComputeEffectiveMethod = closSymbol ("COMPUTE-EFFECTIVE-METHOD");
        static readonly Symbol QuoteComputeEffectiveSlotDefinition = closSymbol ("ComputeEffectiveSlotDefinition");
        static readonly Symbol QuoteComputeMethodMoreSpecific = closSymbol ("COMPUTE-METHOD-MORE-SPECIFIC");
        static readonly Symbol QuoteComputeSlots = closSymbol ("COMPUTE-SLOTS");
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
        static readonly Symbol QuoteEnsureGenericFunctionUsingClass = closSymbol ("EnsureGenericFunctionUsingClass");
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
        static readonly Symbol QuoteGenericFunctionClass = closSymbol ("GENERIC-FUNCTION-CLASS");
        static readonly Symbol QuoteGenericFunctionDeclarations = closSymbol ("GenericFunctionDeclarations");
        static readonly Symbol QuoteGenericFunctionLambdaList = closSymbol ("GenericFunctionLambdaList");
        static readonly Symbol QuoteGenericFunctionMethodClass = closSymbol ("GENERIC-FUNCTION-METHOD-CLASS");
        static readonly Symbol QuoteGenericFunctionMethodCombination = closSymbol ("GenericFunctionMethodCombination");
        static readonly Symbol QuoteGenericFunctionMethods = closSymbol ("GenericFunctionMethods");
        static readonly Symbol QuoteGenericFunctionName = closSymbol ("GENERIC-FUNCTION-NAME");
        static readonly Symbol QuoteGettersAndSetters = closSymbol ("GETTERS-AND-SETTERS");
        static readonly Symbol QuoteInitargs = closSymbol ("INITARGS");
        static readonly Symbol QuoteInitform = closSymbol ("INITFORM");
        static readonly Symbol QuoteInitfunction = closSymbol ("INITFUNCTION");
        static readonly Symbol QuoteInitializeInstance = closSymbol ("INITIALIZE-INSTANCE");
        static readonly Symbol QuoteInitializers = closSymbol ("INITIALIZERS");
        static readonly Symbol QuoteInstance = closSymbol ("INSTANCE");
        static readonly Symbol QuoteInternEqlSpecializer = closSymbol ("InternEqlSpecializer");
        static readonly Symbol QuoteIsClassFinalized = closSymbol ("IsClassFinalized");
        static readonly Symbol QuoteIsSlotBoundUsingClass = closSymbol ("IsSlotBoundUsingClass");
        static readonly Symbol QuoteLambdaList = closSymbol ("LAMBDA-LIST");
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
        static readonly Symbol QuoteSetClassName = closSymbol ("SetClassName");
        static readonly Symbol QuoteSetGenericFunctionName = closSymbol ("SetGenericFunctionName");
        static readonly Symbol QuoteSetSlotValue = closSymbol ("SetSlotValue");
        static readonly Symbol QuoteSetSlotValueUsingClass = closSymbol ("SetSlotValueUsingClass");
        static readonly Symbol QuoteSharedInitialize = closSymbol ("SHARED-INITIALIZE");
        static readonly Symbol QuoteSingletonsList = closSymbol ("SINGLETONS-LIST");
        static readonly Symbol QuoteSlotDefinition = closSymbol ("SLOT-DEFINITION");
        static readonly Symbol QuoteSlotDefinitionAllocation = closSymbol ("SlotDefinitionAllocation");
        static readonly Symbol QuoteSlotDefinitionInitargs = closSymbol ("SlotDefinitionInitargs");
        static readonly Symbol QuoteSlotDefinitionInitform = closSymbol ("SlotDefinitionInitform");
        static readonly Symbol QuoteSlotDefinitionInitfunction = closSymbol ("SlotDefinitionInitfunction");
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
        static readonly Symbol QuoteSlotValueUsingClass = closSymbol ("SlotValueUsingClass");
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
        static readonly Symbol QuoteTop = closSymbol ("TOP");
        static readonly Symbol QuoteType = closSymbol ("TYPE");
        static readonly Symbol QuoteUpdateDependent = closSymbol ("UpdateDependent");
        static readonly Symbol QuoteUpdateInstanceForDifferentClass = closSymbol ("UpdateInstanceForDifferentClass");
        static readonly Symbol QuoteUpdateInstanceForRedefinedClass = closSymbol ("UpdateInstanceForRedefinedClass");
        static readonly Symbol QuoteValidateSuperclass = closSymbol ("VALIDATE-SUPERCLASS");
        static readonly Symbol QuoteWriterMethodClass = closSymbol ("WriterMethodClass");
        static readonly Symbol QuoteWriters = closSymbol ("WRITERS");

        #endregion Symbols

        // To support FindClass
        static readonly Dictionary<Symbol, StandardObject> allNamedClasses = new Dictionary<Symbol, StandardObject> ();

        // ClassOf
        delegate StandardObject ClassOfFunction (object obj);

        [DebuggerStepThroughAttribute]
        static StandardObject bootstrapClassOf (object obj)
        {
            StandardObject probe = obj as StandardObject;
            return (probe == null) ? Top : probe.Class ();
        }

        static ClassOfFunction classOf = new ClassOfFunction (bootstrapClassOf);

        static readonly Dictionary<StandardObject, Cons> bootstrapInitInfo = new Dictionary<StandardObject, Cons> ();

        // This creates the uninitialized class objects that form the base of the
        // class hierarchy.  Since they refer to each other circularly, we'll specify how
        // they are initialized later.
        static readonly StandardObject top = ManifestInstance.CreateInstance (null, 0);
        static readonly StandardObject standardObjectClass = ManifestInstance.CreateInstance (null, 0);
        static readonly StandardObject metaobject = ManifestInstance.CreateInstance (null, 0);
        static readonly StandardObject specializer = ManifestInstance.CreateInstance (null, 0);
        static readonly StandardObject closClass = ManifestInstance.CreateInstance (null, 0);
        static readonly StandardObject builtInClass = ManifestInstance.CreateInstance (null, 0);
        static readonly StandardObject standardClass = ManifestInstance.CreateInstance (null, 0);
        static readonly StandardObject funcallableStandardClass = ManifestInstance.CreateInstance (null, 0);

        static readonly StandardObject function = ManifestInstance.CreateInstance (null, 0);
        static readonly StandardObject funcallableStandardObject = ManifestInstance.CreateInstance (null, 0);
        static readonly StandardObject genericFunction = ManifestInstance.CreateInstance (null, 0);
        static readonly StandardObject standardGenericFunction = ManifestInstance.CreateInstance (null, 0);

        static bool bootstrapInitialClassesFunction ()
        {
            bootstrapInitInfo.Add (Top, 
                CL.List (QuoteTop,
                         KW.DirectSuperclasses, CL.List<StandardObject> (),
                         KW.Metaclass, builtInClass,
                         KW.StudlyName, "Top"));

            bootstrapInitInfo.Add (function,
                CL.List (QuoteFunction,
                         KW.DirectSuperclasses, CL.List<StandardObject> (Top),
                         KW.Metaclass, builtInClass,
                         KW.StudlyName, "Function"));

            bootstrapInitInfo.Add (StandardObjectClass,
                CL.List (QuoteStandardObject,
                         KW.DirectSuperclasses, CL.List<StandardObject> (Top),
                         KW.StudlyName, "StandardObject"));

            bootstrapInitInfo.Add (FuncallableStandardObject,
                CL.List (QuoteFuncallableStandardObject,
                         KW.DirectSuperclasses, CL.List<StandardObject> (StandardObjectClass, function),
                         KW.StudlyName, "FuncallableStandardObject"));

            bootstrapInitInfo.Add (Metaobject,
                CL.List (QuoteMetaobject,
                         KW.DirectSuperclasses, CL.List<StandardObject> (StandardObjectClass),
                         KW.StudlyName, "Metaobject"));

            bootstrapInitInfo.Add (GenericFunction,
                CL.List (QuoteGenericFunction,
                         KW.DirectSuperclasses, CL.List<StandardObject> (Metaobject, FuncallableStandardObject),
                         KW.Metaclass, FuncallableStandardClass,
                         KW.StudlyName, "GenericFunction"));

            bootstrapInitInfo.Add (StandardGenericFunction,
                CL.List (QuoteStandardGenericFunction,
                         KW.DirectSuperclasses, CL.List<StandardObject> (GenericFunction),
                         KW.Metaclass, FuncallableStandardClass,
                         KW.StudlyName, "StandardGenericFunction"));
 
            bootstrapInitInfo.Add (Specializer,
                CL.List (QuoteSpecializer,
                         KW.DirectSuperclasses, CL.List<StandardObject> (Metaobject),
                         KW.StudlyName, "Specializer"));

            bootstrapInitInfo.Add (ClosClass,
                CL.List (QuoteClass,
                         KW.DirectSuperclasses, CL.List<StandardObject> (Specializer),
                         KW.StudlyName, "Class"));

            bootstrapInitInfo.Add (StandardClass,
                CL.List (QuoteStandardClass,
                         KW.DirectSuperclasses, CL.List<StandardObject> (ClosClass),
                         KW.StudlyName, "StandardClass"));

            bootstrapInitInfo.Add (BuiltInClass,
                CL.List (QuoteBuiltInClass,
                         KW.DirectSuperclasses, CL.List<StandardObject> (ClosClass),
                         KW.StudlyName, "BuiltInClass"));

            bootstrapInitInfo.Add (FuncallableStandardClass,
                CL.List (QuoteFuncallableStandardClass,
                         KW.DirectSuperclasses, CL.List<StandardObject> (ClosClass),
                         KW.StudlyName, "FuncallableStandardClass"));


            foreach (KeyValuePair<StandardObject,Cons> kvp in bootstrapInitInfo) {
                StandardObject closClass = kvp.Key;
                Cons initinfo = kvp.Value;
                allNamedClasses.Add ((Symbol) CL.Car (initinfo), closClass);
                closClass.SetClass ((StandardObject) Utility.GetArg (CL.Cdr (initinfo), KW.Metaclass, StandardClass));
            }

            return true;
        }

        static bool bootstrapInitialClass = bootstrapInitialClassesFunction ();


         // This has to go first.
        static readonly StandardObject ensureGenericFunction =
            bootstrapEnsureGenericFunction (null, QuoteEnsureGenericFunction, 
                                                  KW.LambdaList, CL.List (QuoteName),
                                                  KW.StudlyName, "EnsureGenericFunction",
                                                  KW.BootstrapMethod, (FuncallHandler) bootstrapEnsureGenericFunction);

        // Then the instance creation protocol.
        static readonly StandardObject allocateInstance =
            (StandardObject) ensureGenericFunction (QuoteAllocateInstance,
                                                    KW.LambdaList, CL.List (QuoteClass, QuoteAndRest, QuoteInitargs, QuoteAndKey, QuoteAndAllowOtherKeys),
                                                    KW.StudlyName, "AllocateInstance",
                                                    KW.BootstrapMethod, (FuncallHandler) bootstrapAllocateInstance);

        static readonly StandardObject ensureClass =
            (StandardObject) ensureGenericFunction (QuoteEnsureClass, 
                                                    KW.LambdaList, CL.List (QuoteName, QuoteAndKey, QuoteAndAllowOtherKeys),
                                                    KW.StudlyName, "EnsureClass",
                                                    KW.BootstrapMethod, (FuncallHandler) bootstrapEnsureClass);

        static readonly StandardObject initializeInstance =
           (StandardObject) ensureGenericFunction (QuoteInitializeInstance,
                                                   KW.LambdaList, CL.List (QuoteInstance, QuoteAndRest, QuoteInitargs, QuoteAndKey, QuoteAndAllowOtherKeys),
                                                   KW.StudlyName, "InitializeInstance",
                                                   KW.BootstrapMethod, (FuncallHandler) bootstrapInitializeInstance);

        static readonly StandardObject makeInstance =
             (StandardObject) ensureGenericFunction (QuoteMakeInstance,
                                                     KW.LambdaList, CL.List (QuoteClass, QuoteAndRest, QuoteInitargs, QuoteAndKey, QuoteAndAllowOtherKeys),
                                                     KW.BootstrapMethod, (FuncallHandler) bootstrapMakeInstance);

        // The MOP classes.
        // (that have not been defined above!)

        static readonly StandardObject method =
            (StandardObject) ensureClass (QuoteMethod,
                                          KW.DirectSuperclasses, CL.List<StandardObject> (Metaobject),
                                          KW.StudlyName, "Method");

        static readonly StandardObject standardMethod =
            (StandardObject) ensureClass (QuoteStandardMethod,
                                          KW.DirectSuperclasses, CL.List<StandardObject> (Method),
                                          KW.StudlyName, "StandardMethod");

        static readonly StandardObject standardAccessorMethodClass =
            (StandardObject) ensureClass (QuoteStandardAccessorMethod,
                                          KW.DirectSuperclasses, CL.List<StandardObject> (StandardMethod),
                                          KW.StudlyName, "StandardAccessorMethod");

        static readonly StandardObject standardReaderMethod =
            (StandardObject) ensureClass (QuoteStandardReaderMethod,
                                          KW.DirectSuperclasses, CL.List<StandardObject> (StandardAccessorMethodClass),
                                          KW.StudlyName, "StandardReaderMethod");

        static readonly StandardObject standardWriterMethod =
            (StandardObject) ensureClass (QuoteStandardWriterMethod,
                                          KW.DirectSuperclasses, CL.List<StandardObject> (StandardAccessorMethodClass),
                                          KW.StudlyName, "StandardWriterMethod");

        static readonly StandardObject methodCombinationClass =
            (StandardObject) ensureClass (QuoteMethodCombination,
                                          KW.DirectSuperclasses, CL.List<StandardObject> (Metaobject),
                                          KW.StudlyName, "MethodCombination");

        static readonly StandardObject slotDefinition =
            (StandardObject) ensureClass (QuoteSlotDefinition,
                                          KW.DirectSuperclasses, CL.List<StandardObject> (Metaobject),
                                          KW.StudlyName, "SlotDefinition");

        static readonly StandardObject directSlotDefinition =
            (StandardObject) ensureClass (QuoteDirectSlotDefinition,
                                          KW.DirectSuperclasses, CL.List<StandardObject> (SlotDefinition),
                                          KW.StudlyName, "DirectSlotDefinition");

        static readonly StandardObject effectiveSlotDefinition =
            (StandardObject) ensureClass (QuoteEffectiveSlotDefinition,
                                          KW.DirectSuperclasses, CL.List<StandardObject> (SlotDefinition),
                                          KW.StudlyName, "EffectiveSlotDefinition");

        static readonly StandardObject standardSlotDefinition =
            (StandardObject) ensureClass (QuoteStandardSlotDefinition,
                                          KW.DirectSuperclasses, CL.List<StandardObject> (SlotDefinition),
                                          KW.StudlyName, "StandardSlotDefinition");

        static readonly StandardObject standardDirectSlotDefinition =
            (StandardObject) ensureClass (QuoteStandardDirectSlotDefinition,
                                          KW.DirectSuperclasses, CL.List<StandardObject> (StandardSlotDefinition, DirectSlotDefinition),
                                          KW.StudlyName, "StandardDirectSlotDefinition");

        static readonly StandardObject standardEffectiveSlotDefinition =
            (StandardObject) ensureClass (QuoteStandardEffectiveSlotDefinition,
                                          KW.DirectSuperclasses, CL.List<StandardObject> (StandardSlotDefinition, EffectiveSlotDefinition),
                                          KW.StudlyName, "StandardEffectiveSlotDefinition");

        static readonly StandardObject eqlSpecializer =
            (StandardObject) ensureClass (QuoteEqlSpecializer,
                                          KW.DirectSuperclasses, CL.List<StandardObject> (Specializer),
                                          KW.StudlyName, "EqlSpecializer");

        static readonly StandardObject forwardReferencedClass =
            (StandardObject) ensureClass (QuoteForwardReferencedClass,
                                          KW.DirectSuperclasses, CL.List<StandardObject> (ClosClass),
                                          KW.StudlyName, "ForwardReferencedClass");


        // The MOP generic functions.

        static readonly StandardObject accessorMethodSlotDefinition =
            (StandardObject) ensureGenericFunction (QuoteAccessorMethodSlotDefinition, KW.LambdaList, CL.List (QuoteMethod));

        static readonly StandardObject addDependent =
            (StandardObject) ensureGenericFunction (QuoteAddDependent, KW.LambdaList, CL.List ());

        static readonly StandardObject addDirectMethod =
            (StandardObject) ensureGenericFunction (QuoteAddDirectMethod, KW.LambdaList, CL.List ());

        static readonly StandardObject addDirectSubclass =
            (StandardObject) ensureGenericFunction (QuoteAddDirectSubclass, KW.LambdaList, CL.List ());

        static readonly StandardObject addMethod =
            (StandardObject) ensureGenericFunction (QuoteAddMethod, KW.LambdaList, CL.List ());

        //static readonly StandardObject allocateInstance =
        //    (StandardObject) ensureGenericFunction (QuoteAllocateInstance, KW.LambdaList, CL.List ());

        static readonly StandardObject changeClass =
            (StandardObject) ensureGenericFunction (QuoteChangeClass, KW.LambdaList, CL.List ());

        static readonly StandardObject classDefaultInitargs =
            (StandardObject) ensureGenericFunction (QuoteClassDefaultInitargs, KW.LambdaList, CL.List ());

        static readonly StandardObject classDirectDefaultInitargs =
            (StandardObject) ensureGenericFunction (QuoteClassDirectDefaultInitargs, KW.LambdaList, CL.List ());

        static readonly StandardObject classDirectSlots =
            (StandardObject) ensureGenericFunction (QuoteClassDirectSlots, KW.LambdaList, CL.List (),
                                                    KW.BootstrapMethod, (FuncallHandler) bootstrapClassDirectSlots);

        static readonly StandardObject classDirectSubclasses =
            (StandardObject) ensureGenericFunction (QuoteClassDirectSubclasses, KW.LambdaList, CL.List ());

        static readonly StandardObject classDirectSuperclasses =
            (StandardObject) ensureGenericFunction (QuoteClassDirectSuperclasses, KW.LambdaList, CL.List (),
                                                    KW.BootstrapMethod, (FuncallHandler) bootstrapClassDirectSuperclasses);

        static readonly StandardObject className =
            (StandardObject) ensureGenericFunction (QuoteClassName, KW.LambdaList, CL.List (),
                                                    KW.BootstrapMethod, (FuncallHandler) bootstrapClassName);

        static readonly StandardObject classPrecedenceList =
            (StandardObject) ensureGenericFunction (QuoteClassPrecedenceList, 
                                                    KW.LambdaList, CL.List (),
                                                    KW.BootstrapMethod, (FuncallHandler) bootstrapClassPrecedenceList);

        static readonly StandardObject classPrototype =
            (StandardObject) ensureGenericFunction (QuoteClassPrototype, KW.LambdaList, CL.List ());

        static readonly StandardObject classSlots =
            (StandardObject) ensureGenericFunction (QuoteClassSlots, KW.LambdaList, CL.List ());

        static readonly StandardObject computeApplicableMethods =
            (StandardObject) ensureGenericFunction (QuoteComputeApplicableMethods, KW.LambdaList, CL.List ());

        static readonly StandardObject computeApplicableMethodsUsingClasses =
            (StandardObject) ensureGenericFunction (QuoteComputeApplicableMethodsUsingClasses, KW.LambdaList, CL.List ());

        static readonly StandardObject computeClassPrecedenceList =
            (StandardObject) ensureGenericFunction (QuoteComputeClassPrecedenceList, KW.LambdaList, CL.List ());

        static readonly StandardObject computeDefaultInitargs =
            (StandardObject) ensureGenericFunction (QuoteComputeDefaultInitargs, KW.LambdaList, CL.List ());

        static readonly StandardObject computeDiscriminatingFunction =
            (StandardObject) ensureGenericFunction (QuoteComputeDiscriminatingFunction, KW.LambdaList, CL.List ());

        static readonly StandardObject computeEffectiveMethod =
            (StandardObject) ensureGenericFunction (QuoteComputeEffectiveMethod, KW.LambdaList, CL.List ());

        static readonly StandardObject computeEffectiveSlotDefinition =
            (StandardObject) ensureGenericFunction (QuoteComputeEffectiveSlotDefinition, KW.LambdaList, CL.List ());

        static readonly StandardObject computeSlots =
            (StandardObject) ensureGenericFunction (QuoteComputeSlots, KW.LambdaList, CL.List (),
                                                    KW.BootstrapMethod, (FuncallHandler) bootstrapComputeSlots);

        static readonly StandardObject computeMethodMoreSpecific =
            (StandardObject) ensureGenericFunction (QuoteComputeMethodMoreSpecific, KW.LambdaList, CL.List ());

        static readonly StandardObject defaultInitargs =
            (StandardObject) ensureGenericFunction (QuoteDefaultInitargs, KW.LambdaList, CL.List ());

        static readonly StandardObject directSlotDefinitionClass =
            (StandardObject) ensureGenericFunction (QuoteDirectSlotDefinitionClass, KW.LambdaList, CL.List ());

        static readonly StandardObject effectiveSlotDefinitionClass =
            (StandardObject) ensureGenericFunction (QuoteEffectiveSlotDefinitionClass, KW.LambdaList, CL.List ());

        // Needed for class definition.
        // static readonly StandardObject ensureClass = (StandardObject) ensureGenericFunction (Quote, KW.LambdaList, CL.List ());

        static readonly StandardObject ensureClassUsingClass =
            (StandardObject) ensureGenericFunction (QuoteEnsureClassUsingClass, KW.LambdaList, CL.List ());

        // Needed to define the generic functions (including itself!)
        // static readonly StandardObject ensureGenericFunction = (StandardObject) ensureGenericFunction (Quote, KW.LambdaList, CL.List ());

        static readonly StandardObject ensureGenericFunctionUsingClass =
            (StandardObject) ensureGenericFunction (QuoteEnsureGenericFunctionUsingClass, KW.LambdaList, CL.List ());

        static readonly StandardObject eqlSpecializerObject =
            (StandardObject) ensureGenericFunction (QuoteEqlSpecializerObject, KW.LambdaList, CL.List ());

        static readonly StandardObject extractLambdaList =
            (StandardObject) ensureGenericFunction (QuoteExtractLambdaList, KW.LambdaList, CL.List ());

        static readonly StandardObject extractSpecializerNames =
            (StandardObject) ensureGenericFunction (QuoteExtractSpecializerNames, KW.LambdaList, CL.List ());

        static readonly StandardObject finalizeInheritance =
            (StandardObject) ensureGenericFunction (QuoteFinalizeInheritance, KW.LambdaList, CL.List ());

        static readonly StandardObject findClass =
            (StandardObject) ensureGenericFunction (QuoteFindClass, 
                                                    KW.LambdaList, CL.List (QuoteClass),
                                                    KW.BootstrapMethod, (FuncallHandler) bootstrapFindClass);

        static readonly StandardObject findMethod =
            (StandardObject) ensureGenericFunction (QuoteFindMethod, KW.LambdaList, CL.List ());

        static readonly StandardObject findMethodCombination =
            (StandardObject) ensureGenericFunction (QuoteFindMethodCombination, KW.LambdaList, CL.List ());

        static readonly StandardObject functionKeywords =
            (StandardObject) ensureGenericFunction (QuoteFunctionKeywords, KW.LambdaList, CL.List ());

        static readonly StandardObject genericFunctionArgumentPrecedenceOrder =
            (StandardObject) ensureGenericFunction (QuoteGenericFunctionArgumentPrecedenceOrder, KW.LambdaList, CL.List ());

        static readonly StandardObject genericFunctionDeclarations =
            (StandardObject) ensureGenericFunction (QuoteGenericFunctionDeclarations, KW.LambdaList, CL.List ());

        static readonly StandardObject genericFunctionLambdaList =
            (StandardObject) ensureGenericFunction (QuoteGenericFunctionLambdaList, KW.LambdaList, CL.List ());

        static readonly StandardObject genericFunctionMethodClass =
            (StandardObject) ensureGenericFunction (QuoteGenericFunctionMethodClass, KW.LambdaList, CL.List ());

        static readonly StandardObject genericFunctionMethodCombination =
            (StandardObject) ensureGenericFunction (QuoteGenericFunctionMethodCombination, KW.LambdaList, CL.List ());

        static readonly StandardObject genericFunctionMethods =
            (StandardObject) ensureGenericFunction (QuoteGenericFunctionMethods, KW.LambdaList, CL.List ());

        static readonly StandardObject genericFunctionName =
            (StandardObject) ensureGenericFunction (QuoteGenericFunctionName, KW.LambdaList, CL.List (),
                                                    KW.BootstrapMethod, (FuncallHandler) bootstrapGenericFunctionName);

        //static readonly StandardObject initializeInstance =
        //    (StandardObject) ensureGenericFunction (QuoteInitializeInstance, KW.LambdaList, CL.List (),
        //                                            KW.BootstrapMethod, (FuncallHandler) bootstrapInitializeInstance);

        static readonly StandardObject internEqlSpecializer =
            (StandardObject) ensureGenericFunction (QuoteInternEqlSpecializer, KW.LambdaList, CL.List ());

        static readonly StandardObject isClassFinalized =
            (StandardObject) ensureGenericFunction (QuoteIsClassFinalized, KW.LambdaList, CL.List ());

        //static readonly StandardObject makeInstance =
        //    (StandardObject) ensureGenericFunction (QuoteMakeInstance, KW.LambdaList, CL.List ());

        static readonly StandardObject makeInstancesObsolete =
            (StandardObject) ensureGenericFunction (QuoteMakeInstancesObsolete, KW.LambdaList, CL.List ());

        static readonly StandardObject makeLoadForm =
            (StandardObject) ensureGenericFunction (QuoteMakeLoadForm, KW.LambdaList, CL.List ());

        static readonly StandardObject makeMethodLambda =
            (StandardObject) ensureGenericFunction (QuoteMakeMethodLambda, KW.LambdaList, CL.List ());

        static readonly StandardObject mapDependents =
            (StandardObject) ensureGenericFunction (QuoteMapDependents, KW.LambdaList, CL.List ());

        static readonly StandardObject methodCombination =
            (StandardObject) ensureGenericFunction (QuoteMethodCombination, KW.LambdaList, CL.List ());

        static readonly StandardObject methodFunction =
            (StandardObject) ensureGenericFunction (QuoteMethodFunction, KW.LambdaList, CL.List ());

        static readonly StandardObject methodGenericFunction =
            (StandardObject) ensureGenericFunction (QuoteMethodGenericFunction, KW.LambdaList, CL.List ());

        static readonly StandardObject methodLambdaList =
            (StandardObject) ensureGenericFunction (QuoteMethodLambdaList, KW.LambdaList, CL.List ());

        static readonly StandardObject methodSpecializers =
            (StandardObject) ensureGenericFunction (QuoteMethodSpecializers, KW.LambdaList, CL.List ());

        static readonly StandardObject methodQualifier =
            (StandardObject) ensureGenericFunction (QuoteMethodQualifier, KW.LambdaList, CL.List ());

        static readonly StandardObject noApplicableMethod =
            (StandardObject) ensureGenericFunction (QuoteNoApplicableMethod, KW.LambdaList, CL.List ());

        static readonly StandardObject noNextMethod =
            (StandardObject) ensureGenericFunction (QuoteNoNextMethod, KW.LambdaList, CL.List ());

        static readonly StandardObject readerMethodClass =
            (StandardObject) ensureGenericFunction (QuoteReaderMethodClass, KW.LambdaList, CL.List ());

        static readonly StandardObject reinitializeInstance =
            (StandardObject) ensureGenericFunction (QuoteReinitializeInstance, KW.LambdaList, CL.List ());

        static readonly StandardObject removeDependents =
            (StandardObject) ensureGenericFunction (QuoteRemoveDependents, KW.LambdaList, CL.List ());

        static readonly StandardObject removeDirectMethod =
            (StandardObject) ensureGenericFunction (QuoteRemoveDirectMethod, KW.LambdaList, CL.List ());

        static readonly StandardObject removeDirectSubclass =
            (StandardObject) ensureGenericFunction (QuoteRemoveDirectSubclass, KW.LambdaList, CL.List ());

        static readonly StandardObject removeMethod =
            (StandardObject) ensureGenericFunction (QuoteRemoveMethod, KW.LambdaList, CL.List ());

        static readonly StandardObject sharedInitialize =
            (StandardObject) ensureGenericFunction (QuoteSharedInitialize, KW.LambdaList, CL.List ());

        static readonly StandardObject setClassName =
            (StandardObject) ensureGenericFunction (QuoteSetClassName, KW.LambdaList, CL.List ());

        static readonly StandardObject setGenericFunctionName =
            (StandardObject) ensureGenericFunction (QuoteSetGenericFunctionName, KW.LambdaList, CL.List ());

        static readonly StandardObject setSlotValue =
            (StandardObject) ensureGenericFunction (QuoteSetSlotValue, KW.LambdaList, CL.List ());

        static readonly StandardObject setSlotValueUsingClass =
            (StandardObject) ensureGenericFunction (QuoteSetSlotValueUsingClass, KW.LambdaList, CL.List ());

        static readonly StandardObject isSlotBoundUsingClass =
            (StandardObject) ensureGenericFunction (QuoteIsSlotBoundUsingClass, KW.LambdaList, CL.List ());

        static readonly StandardObject slotDefinitionAllocation =
            (StandardObject) ensureGenericFunction (QuoteSlotDefinitionAllocation, KW.LambdaList, CL.List ());

        static readonly StandardObject slotDefinitionInitargs =
            (StandardObject) ensureGenericFunction (QuoteSlotDefinitionInitargs, KW.LambdaList, CL.List ());

        static readonly StandardObject slotDefinitionInitform =
            (StandardObject) ensureGenericFunction (QuoteSlotDefinitionInitform, KW.LambdaList, CL.List ());

        static readonly StandardObject slotDefinitionInitfunction =
            (StandardObject) ensureGenericFunction (QuoteSlotDefinitionInitfunction, KW.LambdaList, CL.List ());

        static readonly StandardObject slotDefinitionLocation =
            (StandardObject) ensureGenericFunction (QuoteSlotDefinitionLocation, KW.LambdaList, CL.List ());

        static readonly StandardObject slotDefinitionName =
            (StandardObject) ensureGenericFunction (QuoteSlotDefinitionName, KW.LambdaList, CL.List ());

        static readonly StandardObject slotDefinitionType =
            (StandardObject) ensureGenericFunction (QuoteSlotDefinitionType, KW.LambdaList, CL.List ());

        static readonly StandardObject slotDefinitionReaders =
            (StandardObject) ensureGenericFunction (QuoteSlotDefinitionReaders, KW.LambdaList, CL.List ());

        static readonly StandardObject slotDefinitionWriters =
            (StandardObject) ensureGenericFunction (QuoteSlotDefinitionWriters, KW.LambdaList, CL.List ());

        static readonly StandardObject slotMakunboundUsingClass =
            (StandardObject) ensureGenericFunction (QuoteSlotMakunboundUsingClass, KW.LambdaList, CL.List ());

        static readonly StandardObject slotMissing =
            (StandardObject) ensureGenericFunction (QuoteSlotMissing, KW.LambdaList, CL.List ());

        static readonly StandardObject slotUnbound =
            (StandardObject) ensureGenericFunction (QuoteSlotUnbound, KW.LambdaList, CL.List ());

        static readonly StandardObject slotValueUsingClass =
            (StandardObject) ensureGenericFunction (QuoteSlotValueUsingClass, KW.LambdaList, CL.List ());

        static readonly StandardObject specializerDirectGenericFunctions =
            (StandardObject) ensureGenericFunction (QuoteSpecializerDirectGenericFunctions, KW.LambdaList, CL.List ());

        static readonly StandardObject specializerDirectMethods =
            (StandardObject) ensureGenericFunction (QuoteSpecializerDirectMethods, KW.LambdaList, CL.List ());

        static readonly StandardObject standardAccessorMethod =
            (StandardObject) ensureGenericFunction (QuoteStandardAccessorMethod, KW.LambdaList, CL.List ());

        static readonly StandardObject updateDependent =
            (StandardObject) ensureGenericFunction (QuoteUpdateDependent, KW.LambdaList, CL.List ());

        static readonly StandardObject updateInstanceForDifferentClass =
            (StandardObject) ensureGenericFunction (QuoteUpdateInstanceForDifferentClass, KW.LambdaList, CL.List ());

        static readonly StandardObject updateInstanceForRedefinedClass =
            (StandardObject) ensureGenericFunction (QuoteUpdateInstanceForRedefinedClass, KW.LambdaList, CL.List ());

        static readonly StandardObject validateSuperclass =
            (StandardObject) ensureGenericFunction (QuoteValidateSuperclass, KW.LambdaList, CL.List ());

        static readonly StandardObject writerMethodClass =
            (StandardObject) ensureGenericFunction (QuoteWriterMethodClass, KW.LambdaList, CL.List ());

        // For bootstrapping, we collect the initialization information
        // in a hash table that we'll clean out later.  The bootstrap functions know
        // to go to the hash table for information.
        static StandardObject bootstrapAllocateInstance (StandardObject self, object [] arguments)
        {
            StandardObject closClass = (StandardObject) arguments [0];
            if (closClass == StandardGenericFunction)
                return ManifestInstance.CreateFuncallableInstance (closClass, 0);
            else
                return ManifestInstance.CreateInstance (closClass, 0);
        }

        static Cons bootstrapClassDirectSlots (StandardObject self, object [] arguments)
        {
            StandardObject closClass = (StandardObject) arguments [0];
            Cons initargs = bootstrapLookupInitInfo (closClass);
            Cons slotSpecs = (Cons) Utility.GetArg (CL.Cdr (initargs), KW.DirectSlots, null);
            return slotSpecs;
        }

        static ConsList<StandardObject> bootstrapClassDirectSuperclasses (StandardObject self, object [] arguments)
        {
            StandardObject closClass = (StandardObject) arguments [0];
            Cons initargs = bootstrapLookupInitInfo (closClass);
            return (ConsList<StandardObject>) Utility.GetArg (CL.Cdr (initargs), KW.DirectSuperclasses, null);
        }

        static Symbol bootstrapClassName (StandardObject self, object [] arguments)
        {
            StandardObject closClass = (StandardObject) arguments [0];
            Cons initargs = bootstrapLookupInitInfo (closClass);
            return (Symbol) CL.Car (initargs);
        }

        static ConsList<StandardObject> bootstrapClassPrecedenceList (StandardObject self, object [] arguments)
        {
            StandardObject closClass = (StandardObject) arguments [0];

            ConsList<StandardObject> superclasses = (ConsList<StandardObject>) ClassDirectSuperclasses (closClass);
            Cons cpls = null;
            if (superclasses != null) {
                foreach (StandardObject superclass in superclasses) {
                    cpls = new Cons (ClassPrecedenceList (superclass), cpls);
                }
            }
            if (cpls != null)
                return MergeCpls (cpls, new ConsList<StandardObject> (closClass, null));
            else
                return new ConsList<StandardObject> (closClass, null);
        }

        static Cons bootstrapComputeSlots (StandardObject self, object [] arguments)
        {
            StandardObject closClass = (StandardObject) arguments [0];
            ConsList<StandardObject> precedenceList = (ConsList<StandardObject>) ClassPrecedenceList (closClass);
            Cons slots = null;
            foreach (StandardObject precClass in precedenceList) {
                slots = (Cons) CL.Reconc (ClassDirectSlots (precClass), slots);
            }
            return CL.Reverse (slots);
        }


        static StandardObject bootstrapEnsureGenericFunction (StandardObject self, params object [] arguments)
        {
            Cons initargs = Cons.VectorToList (arguments);
            StandardObject metaclass = (StandardObject) Utility.GetArg ((Cons) CL.Cdr (initargs), KW.Metaclass, StandardGenericFunction);
            if (metaclass == null)
                throw new NotImplementedException ("Null metaclass");

            // Make instance might not be bootstrapped.  If it isn't, use a shortcut.
            return
                (MakeInstance == null)
                ? bootstrapMakeInstanceShortcut (metaclass, initargs)
                : (StandardObject) CL.Apply (MakeInstance, metaclass, initargs);
        }

        static StandardObject bootstrapEnsureClass (StandardObject self, object [] arguments)
        {
	        Cons initargs = Cons.VectorToList (arguments);
            StandardObject metaclass = (StandardObject) Utility.GetArg ((Cons) CL.Cdr (initargs), KW.Metaclass, StandardClass);
            if (metaclass == null)
                throw new NotImplementedException ("Null metaclass");
            StandardObject closClass = (StandardObject) CL.Apply (MakeInstance, metaclass, initargs);
            allNamedClasses.Add ((Symbol) CL.Car (initargs), closClass);
            return closClass;
        }

        static StandardObject bootstrapFindClass (StandardObject self, object [] arguments)
        {
            StandardObject answer = null;
            if (allNamedClasses.TryGetValue ((Symbol) arguments [0], out answer))
                return answer;
            throw new NotImplementedException ("Missing class during bootstrap");
        }

        static Symbol bootstrapGenericFunctionName (StandardObject self, object [] arguments)
        {
            StandardObject closClass = (StandardObject) arguments [0];
            Cons initargs = bootstrapLookupInitInfo (closClass);
            return (Symbol) CL.Car (initargs);
        }

        static object bootstrapInitializeInstance (StandardObject self, object [] arguments)
        {
            StandardObject instance = (StandardObject) arguments [0];
            Cons initargs = Cons.SubvectorToList (arguments, 1, arguments.Length);
            bootstrapInitInfo.Add (instance, initargs);

            if (ClassOf (instance) == StandardGenericFunction) {
                FuncallHandler bootstrapHandler =
                    (FuncallHandler) Utility.GetArg ((Cons) CL.Cdr (initargs), KW.BootstrapMethod, (FuncallHandler) bootstrapUninitializedGenericFunction);
                ((ManifestInstance) instance.Target).OnFuncall = bootstrapHandler;
            }
                
            return instance;
        }

        static void bootstrapInitializeInstanceShortcut (StandardObject instance, Cons initargs)
        {
            if (InitializeInstance != null)
                throw new NotImplementedException ("Huh?  Should not be called if InitializeInstance can be called.");
 
            object [] newArglist = new object [CL.Length (initargs) + 1];
            newArglist [0] = instance;
            int index = 1;
            foreach (object initarg in initargs) {
                newArglist [index] = initarg;
                index += 1;
            }
            bootstrapInitializeInstance (null, newArglist);
        }

        static object bootstrapInitializeSlot (Cons slotSpec, int index, Cons initargs)
        {
            Symbol slotName = (Symbol) CL.Car (slotSpec);
            Symbol initarg = (Symbol) Utility.GetArg (CL.Cdr(slotSpec), KW.Initarg, null);
            if (initarg != null) {
                object value = Utility.GetArg (initargs, initarg, null);
                if (value != null)
                    return value;
            }

            Symbol initfunction = (Symbol) Utility.GetArg (CL.Cdr (slotSpec), KW.Initfunction, null);
            if (initfunction != null)
                throw new NotImplementedException ("We have an initfunction");

            return UnboundSlotValue;
        }

        static Cons bootstrapLookupInitInfo (StandardObject obj)
        {
            Cons initInfo = null;
            if (bootstrapInitInfo.TryGetValue (obj, out initInfo))
                return initInfo;
            throw new NotImplementedException ("Couldn't find bootstrap info!!!");
        }


        static object bootstrapMakeInstance (StandardObject self, object [] arguments)
        {
            Cons arglist = Cons.VectorToList (arguments);
            StandardObject closClass = (StandardObject) CL.Car (arglist);
            if (closClass == StandardClass) {
                StandardObject instance = (StandardObject) CL.Apply (AllocateInstance, arglist);
                CL.Apply (InitializeInstance, instance, CL.Cdr (arglist));
                return instance;
            }
            else if (closClass == StandardGenericFunction) {
                // AllocateInstance might not be defined yet,
                // so take a shortcut if necessary.
                StandardObject instance =
                    (AllocateInstance == null)
                        ? bootstrapAllocateInstance (null, arguments)
                        : (StandardObject) CL.Apply (AllocateInstance, arglist);
                if (InitializeInstance == null)
                    bootstrapInitializeInstanceShortcut (instance, (Cons) CL.Cdr (arglist));
                else
                    CL.Apply (InitializeInstance, instance, CL.Cdr (arglist));
                return instance;
            }
            else
                throw new NotImplementedException ("Can't make this");

        }

        static StandardObject bootstrapMakeInstanceShortcut (StandardObject metaclass, Cons initargs)
        {
            if (MakeInstance != null)
                throw new NotImplementedException ("Huh?  Should not be called if MakeInstance can be called.");
            object [] arglist = new object [CL.Length (initargs) + 1];
            arglist [0] = metaclass;
            int index = 1;

            foreach (object element in initargs) {
                arglist [index] = element;
                index += 1;
            }

            return (StandardObject) bootstrapMakeInstance (null, arglist);
        }


        static StandardObject bootstrapUninitializedGenericFunction (StandardObject self, object [] arguments)
        {
            throw new NotImplementedException (((Symbol) GenericFunctionName(self)).Name + " has no bootstrap implementation.");
        }

        static bool bootstrapCLOS ()
        {
            FieldInfo [] fields = typeof (CLOS).GetFields (BindingFlags.NonPublic | BindingFlags.Static);
            ICollection<StandardObject> instances = new List<StandardObject>();
            foreach (FieldInfo fInfo in fields) {
                StandardObject item = fInfo.GetValue (null) as StandardObject;
                if (item != null)
                    instances.Add(item);
            }

            //// We're sort of doing a parallel `make instance' here.

            // Then allocate a slot vectors for each.
            foreach (StandardObject instance in instances) {
                StandardObject closClass = classOf (instance);
                if (closClass == builtInClass
                    || closClass == funcallableStandardClass
                    || closClass == standardGenericFunction
                    || closClass == standardClass) {
                    Cons slots = (Cons) computeSlots (closClass);
                    ((ManifestInstance) instance.Target).Slots = new object [CL.Length (slots)];
                }
                else {
                    throw new NotImplementedException ("What's this?");
                }
            }

            // Now initialize them
            //foreach (StandardObject instance in instances) {
            //    CL.Apply (InitializeInstance, instance, CL.Cddr (bootstrapLookupInitInfo(instance)));
            //}

            throw new NotImplementedException ();

        }

        static bool IsBootstrapped = bootstrapCLOS ();

        static StandardObject SelectCplCandidateValidateLoop (Cons lists, Cons more, StandardObject candidate, Cons l)
        {
            if (l == null)
                return candidate;
            ConsList<StandardObject> v = (ConsList<StandardObject>) l.Car;
            if (v == null)
                return SelectCplCandidateValidateLoop (lists, more, candidate, (Cons) l.Cdr);
            if (v.Cdr == null)
                return SelectCplCandidateValidateLoop (lists, more, candidate, (Cons) l.Cdr);
            if (((ICollection<StandardObject>) v.Cdr).Contains (candidate))
                return SelectCplCandidateSearchLoop (lists, more);
            else
                return SelectCplCandidateValidateLoop (lists, more, candidate, (Cons) l.Cdr);
        }

        static StandardObject SelectCplCandidateSearchLoop (Cons lists, Cons scan)
        {
            if (scan == null)
                throw new NotImplementedException ("Inconsistent class linearization");
            ConsList<StandardObject> list = (ConsList<StandardObject>) scan.Car;
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
        
        delegate object MapFunction (Cons arguments);

        static ConsList<StandardObject> MergeCpls (Cons cpls, ConsList<StandardObject> result)
        {
            bool done = true;
            foreach (object cpl in cpls) {
                if (cpl != null) {
                    done = false;
                    break;
                }
            }
            if (done)
                return CL.Reverse<StandardObject> (result);

            StandardObject candidate = SelectCplCandidate (cpls);

            Cons newCpls = null;
            foreach (object oldCpl in cpls) {
                if (oldCpl == null)
                    newCpls = new Cons (null, newCpls);
                ConsList<StandardObject> socpl = (ConsList<StandardObject>) oldCpl;
                if (socpl.Car == candidate)
                    newCpls = new Cons (socpl.Cdr, newCpls);
                else
                    newCpls = new Cons (socpl, newCpls);
            }

            return MergeCpls (CL.Reverse (newCpls),
                               new ConsList<StandardObject> (candidate, result));

        }


        // Various and sundry utilities.

        static Symbol closSymbol (string name)
        {
            return closPackage.Intern (name);
        }

        public static object StandardObjectName (StandardObject obj)
        {
            if (ClassOf (obj) == FuncallableStandardClass)
                return ((Symbol) ClassName (obj)).Name;
            else if (ClassOf (obj) == StandardClass)
                return ((Symbol) ClassName (obj)).Name;
            else if (ClassOf (obj) == StandardGenericFunction)
                return ((Symbol) GenericFunctionName (obj)).Name;
            else
                return null;
        }
    }
}

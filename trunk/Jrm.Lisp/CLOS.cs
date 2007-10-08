using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;

namespace Lisp
{

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
        internal bool TryGetValue (Cons key, out ApplyHandler value)
        {
            Cons probe = Assoc (key == null ? standinForNull : key, table);
            if (probe == null) {
                value = null;
                return false;
            }
            else {
                value = (ApplyHandler) probe.Cdr;
                return true;
            }
        }

        internal void Add (object key, ApplyHandler value)
        {
            // will shadow
                table = new Cons (new Cons (key, value), table);
        }

        internal bool ContainsKey (object key)
        {
            throw new NotImplementedException ("MethodCache::ContainsKey");
        }
    }

    public delegate object NextMethodFunction (object [] arguments);
    public delegate object MethodFunction (NextMethodFunction callNextMethod, object [] arguments);

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

        static object InstanceSet (this StandardObject obj, int index, object newValue)
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
        static void setFuncallableInstanceFunction (StandardObject funcallableInstance, ApplyHandler handler)
        {
            ((ManifestInstance) funcallableInstance.Target).OnApply = handler;
        }
                      

 
        static readonly Package closPackage = Package.Clos;

        static readonly Symbol QuoteAndAllowOtherKeys = Package.CommonLisp.FindSymbol ("&ALLOW-OTHER-KEYS");
        static readonly Symbol QuoteAndKey = Package.CommonLisp.FindSymbol ("&KEY");
        static readonly Symbol QuoteAndRest = Package.CommonLisp.FindSymbol ("&REST");
        static readonly Symbol QuoteAnonymous = Package.CommonLisp.FindSymbol ("[unnamed]");
        static readonly Symbol QuoteAddMethod = Package.CommonLisp.FindSymbol ("ADD-METHOD");
        static readonly Symbol QuoteAllocateInstance = Package.CommonLisp.FindSymbol ("ALLOCATE-INSTANCE");
        static readonly Symbol QuoteApplicationCache = closSymbol ("APPLICATION-CACHE");
        static readonly Symbol QuoteArguments = closSymbol ("ARGUMENTS");
        static readonly Symbol QuoteArgumentPrecedenceOrder = closSymbol ("ARGUMENT-PRECEDENCE-ORDER");
        static readonly Symbol QuoteBuiltInClass = closSymbol ("BUILT-IN-CLASS");
        static readonly Symbol QuoteClassPrecedenceList = closSymbol ("CLASS-PRECEDENCE-LIST");
        static readonly Symbol QuoteClassPrototype = closSymbol ("CLASS-PROTOTYPE");
        static readonly Symbol QuoteClassSlots = closSymbol ("CLASS-SLOTS");
        static readonly Symbol QuoteClass = closSymbol ("CLASS");
        static readonly Symbol QuoteComputeApplicableMethods = closSymbol ("COMPUTE-APPLICABLE-METHODS");
        static readonly Symbol QuoteComputeDiscriminatingFunction = closSymbol ("COMPUTE-DISCRIMINATING-FUNCTION");
        static readonly Symbol QuoteComputeEffectiveMethod = closSymbol ("COMPUTE-EFFECTIVE-METHOD");
        static readonly Symbol QuoteDeclarations = closSymbol ("DECLARATIONS");
        static readonly Symbol QuoteDefaultInitargs = closSymbol ("DEFAULT-INITARGS");
        static readonly Symbol QuoteDirectDefaultInitargs = closSymbol ("DIRECT-DEFAULT-INITARGS");
        static readonly Symbol QuoteDirectSlots = closSymbol ("DIRECT-SLOTS");
        static readonly Symbol QuoteDirectSubclasses = closSymbol ("DIRECT-SUBCLASSES");
        static readonly Symbol QuoteDirectSuperclasses = closSymbol ("DIRECT-SUPERCLASSES");
        static readonly Symbol QuoteDocumentation = closSymbol ("DOCUMENTATION");
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
        static readonly Symbol QuoteInitargs = closSymbol ("INITARGS");
        static readonly Symbol QuoteLambdaList = closSymbol ("LAMBDA-LIST");
        static readonly Symbol QuoteList = Package.CommonLisp.FindSymbol ("LIST");
        static readonly Symbol QuoteMakeInstance = Package.CommonLisp.FindSymbol ("MAKE-INSTANCE");
        static readonly Symbol QuoteMetaobject = closSymbol ("METAOBJECT");
        static readonly Symbol QuoteMethod = closSymbol ("METHOD");
        static readonly Symbol QuoteMethodComputeApplicableMethods = closSymbol ("METHOD:compute-applicable-methods");
        static readonly Symbol QuoteMethodComputeDiscriminatingFunction = closSymbol ("METHOD:compute-discriminating-function");
        static readonly Symbol QuoteMethodComputeEffectiveMethod = closSymbol ("METHOD:compute-effective-method");
        static readonly Symbol QuoteMethods = closSymbol ("METHODS");
        static readonly Symbol QuoteMethodClass = closSymbol ("METHOD-CLASS");
        static readonly Symbol QuoteMethodCombination = closSymbol ("METHOD-COMBINATION");
        static readonly Symbol QuoteName = closSymbol ("NAME");
        static readonly Symbol QuoteNewMakeInstance = closSymbol ("NEW-MAKE-INSTANCE");
        static readonly Symbol QuoteNewMakeInstanceMethod = closSymbol ("NEW-MAKE-INSTANCE-METHOD");
        static readonly Symbol QuoteProcedure = closSymbol ("PROCEDURE");
        static readonly Symbol QuoteQualifier = closSymbol ("QUALIFIER");
        static readonly Symbol QuoteSingletonsList = closSymbol ("SINGLETONS-LIST");
        static readonly Symbol QuoteStandardClass = closSymbol ("STANDARD-CLASS");
        static readonly Symbol QuoteSpecializer = closSymbol ("SPECIALIZER");
        static readonly Symbol QuoteSpecializers = closSymbol ("SPECIALIZERS");
        static readonly Symbol QuoteStandardGenericFunction = closSymbol ("STANDARD-GENERIC-FUNCTION");
        static readonly Symbol QuoteStandardMethod = Package.CommonLisp.FindSymbol ("STANDARD-METHOD");
        static readonly Symbol QuoteStandardObject = closSymbol ("STANDARD-OBJECT");
        static readonly Symbol QuoteTop = closSymbol ("TOP");

        public delegate object SlotReader (StandardObject obj);
        delegate object SlotWriter (StandardObject obj, object newValue);
        delegate object SlotChangingFunction (object currentValue);
        delegate object SlotChanger (StandardObject obj, SlotChangingFunction func);


                                   

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

        delegate Cons amf (Object obj);
        static Cons AppendMap (amf del, Cons list)
        {
            Cons answer = null;
            while (true) {
                if (list == null)
                    break;
                answer = (Cons) CL.Append (del (list.Car), answer);
                list = (Cons) list.Cdr;
            }

            return answer;

        }

        static Cons bootstrapComputeClassPrecedenceList (Cons superclasses, Cons soFar)
        {
            if (superclasses == null)
                return (Cons) CL.Reverse (soFar);
            return bootstrapComputeClassPrecedenceList ((Cons) CL.Append (superclasses.Cdr, InternalClassDirectSuperclasses ((StandardObject) superclasses.Car)),
                                                        CL.Memq (superclasses.Car, soFar)
                                                           ? soFar
                                                           : CL.Cons (superclasses.Car, soFar));
        }

        delegate Cons Allocator (object initargs);

        static object unspecificInitializer ()
        {
            throw new NotImplementedException ();
        }

        static object bootstrapMakeInstance (StandardObject self, object [] arguments)
        {
            StandardObject closClass = (StandardObject) arguments [0];
            Cons initargs = Cons.SubvectorToList (arguments, 1, arguments.Length);
            if (object.ReferenceEquals (closClass,StandardClass)
                || object.ReferenceEquals (closClass,FuncallableStandardClass)) {
                StandardObject instance = ManifestInstance.CreateInstance (closClass, CL.Length (SLOTS_OF_STANDARD_CLASS));

                Cons directDefaultInitargs = (Cons) Utility.GetArg (initargs, KW.DirectDefaultInitargs, null);
                Cons directSlots = (Cons) Utility.GetArg (initargs, KW.DirectSlots, null);
                Cons directSuperclasses = (Cons) Utility.GetArg (initargs, KW.DirectSuperclasses, null);
                Symbol name = (Symbol) Utility.GetArg (initargs, KW.Name, QuoteAnonymous);
                Cons classPrecedenceList = bootstrapComputeClassPrecedenceList (directSuperclasses, CL.List (instance));
                Cons inheritedSlots = AppendMap (new amf (delegate (object superclass)
                {
                    return InternalClassDirectSlots ((StandardObject) superclass);
                }), (Cons) CL.Cdr (classPrecedenceList));
                Cons slots = (Cons) CL.Append (directSlots, inheritedSlots);

                int nfields = 0;
                Cons fieldInitializers = null;



                Delegate allocator = new Allocator (delegate (object init)
                {
                    int f = nfields;
                    nfields += 1;
                    fieldInitializers = new Cons (init, fieldInitializers);
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

                Cons gettersAndSetters =
                  (Cons) CL.Map (QuoteList, new MapFunction (
                                      delegate (Cons s)
                                      {
                                          return CL.Cons (s.Car, allocator.DynamicInvoke (Utility.GetArg (s.Cdr, KW.Initializer, null)));
                                      }), slots);
                if (CL.Length (slots) < CL.Length (directSlots))
                    throw new NotImplementedException ();
                if (CL.Length (slots) != CL.Length (gettersAndSetters))
                    throw new NotImplementedException ();
                internalSetClassDefaultInitargs (instance, null); // don't install yet
                internalSetClassDirectDefaultInitargs (instance, directDefaultInitargs);
                internalSetClassDirectSlots (instance, directSlots);
                internalSetClassDirectSubclasses (instance, null);
                internalSetClassDirectSuperclasses (instance, directSuperclasses);
                internalSetClassIsFinalized (instance, true);
                internalSetClassName (instance, name);
                internalSetClassPrecedenceList (instance, classPrecedenceList);
                internalSetClassPrototype (instance, null);
                internalSetClassGettersAndSetters (instance, gettersAndSetters);
                internalSetClassSlots (instance, null);
                return instance;
            }
            else if (object.ReferenceEquals (closClass, StandardGenericFunction)) {
                Cons sentinel = new Cons (null, null);
                Cons lambdaList = (Cons) Utility.GetArg (initargs, KW.LambdaList, sentinel);
                if (object.ReferenceEquals (sentinel, lambdaList))
                    throw new NotImplementedException ("missing lambda list");
                StandardObject answer = ManifestInstance.CreateFuncallableInstance (closClass, CL.Length (internalClassDirectSlots (closClass)));
                internalSetGenericApplicationCache (answer, new Cons (GenericApplicationCacheTag, new MethodCache()));
                internalSetGenericMethodClass (answer, Utility.GetArg (initargs, KW.MethodClass, StandardMethod));
                internalSetGenericMethods (answer, null);
                internalSetGenericName (answer, Utility.GetArg (initargs, KW.Name, QuoteAnonymous));
                internalSetGenericLambdaList (answer, lambdaList);
                internalSetGenericSingletonsList (answer, null);
                return answer;
            }
            else if (object.ReferenceEquals (closClass, StandardMethod)) {
                StandardObject answer = ManifestInstance.CreateFuncallableInstance (closClass, CL.Length (internalClassDirectSlots (closClass)));
	            internalSetMethodName (answer, Utility.GetArg (initargs, KW.Name, QuoteAnonymous));
                InternalSetMethodProcedure (answer, (MethodFunction) Utility.GetArg (initargs, KW.Procedure, null));
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

        static Cons lookupSlotInfo (StandardObject closClass, Symbol slotName)
        {
            Symbol className = null;
            object [] classSlots;
            Cons sng =  InternalClassGettersAndSetters (closClass);
            Cons probe = CL.Assq (slotName, sng);
            if (probe == null) {
                className =  InternalClassName (closClass);
                classSlots = ((ManifestInstance) closClass.Target).Slots;
                throw new NotImplementedException ();
            }
            return probe;
        }

        static object internalSlotRef (StandardObject obj, Symbol slotName)
        {
            return ((SlotReader) (CL.Cadr (lookupSlotInfo (ClassOf (obj), slotName)))) (obj);
        }

        static internal object StandardObjectName (StandardObject obj)
        {
            try {
                // Try getting the name slot, if it exists.
                return internalSlotRef (obj, QuoteName);
            }
            catch (Exception ) {

                return null;
            }
        }

        static object internalSlotSet (StandardObject obj, Symbol slotName, object value)
        {
            return ((SlotWriter) (CL.Caddr (lookupSlotInfo (ClassOf (obj), slotName)))) (obj, value);
        }

        static object internalSlotChange (StandardObject obj, Symbol slotName, SlotChangingFunction value)
        {
            return ((SlotChanger) (CL.Cadddr (lookupSlotInfo (ClassOf (obj), slotName)))) (obj, value);
        }

        static StandardObject GuaranteeClass (StandardObject putativeClass)
        {
            if ((putativeClass.Class () != StandardClass)
                && (putativeClass.Class () != FuncallableStandardClass))
                throw new NotImplementedException ("GuaranteeClass");
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

        static SlotReader internalClassDirectSlots =
          new SlotReader (delegate (StandardObject obj)
        {
            return internalSlotRef (obj, QuoteDirectSlots);
        });

        static Cons InternalClassDirectSlots (StandardObject obj)
        {
            return (Cons) internalClassDirectSlots (GuaranteeClass (obj));
        }

        static SlotWriter internalSetClassDirectSlots =
          new SlotWriter (delegate (StandardObject obj, object newValue)
        {
            return internalSlotSet (obj, QuoteDirectSlots, newValue);
        });

        static SlotWriter internalSetClassDirectSubclasses =
          new SlotWriter (delegate (StandardObject obj, object newValue)
        {
            return internalSlotSet (obj, QuoteDirectSubclasses, newValue);
        });

        static SlotReader internalClassDirectSuperclasses =
          new SlotReader (delegate (StandardObject obj)
        {
            return internalSlotRef (obj, QuoteDirectSuperclasses);
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

        static SlotReader internalClassSlots =
          new SlotReader (delegate (StandardObject obj)
        {
            return internalSlotRef (obj, QuoteClassSlots);
        });

        static SlotWriter internalSetClassSlots =
          new SlotWriter (delegate (StandardObject obj, object newValue)
        {
            return internalSlotSet (obj, QuoteClassSlots, newValue);
        });

        static StandardObject GuaranteeGeneric (StandardObject putativeGeneric)
        {
            if (putativeGeneric.Class() != StandardGenericFunction)
                throw new NotImplementedException ("GuaranteeGeneric");
            return putativeGeneric;
        }

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
        return internalSlotSet (generic, QuoteMethods, newValue);
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

        static SlotReader internalGenericName =
            new SlotReader (delegate (StandardObject generic)
        {
            return internalSlotRef (generic, QuoteName);
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

        // Method accessors
        static StandardObject GuaranteeMethod (StandardObject putativeMethod)
        {
            if (putativeMethod.Class() != StandardMethod)
                throw new NotImplementedException ("GuaranteeMethod");
            return putativeMethod;
        }

        static SlotReader internalMethodName = 
            new SlotReader (delegate (StandardObject method)
        {
            return internalSlotRef (method, QuoteName);
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

        static SlotReader internalMethodProcedure = new SlotReader (delegate (StandardObject method)
        {
            return internalSlotRef (method, QuoteProcedure);
        });

        static MethodFunction InternalMethodProcedure (StandardObject method)
        {
            return (MethodFunction) internalMethodProcedure (GuaranteeMethod (method));
        }

        static SlotWriter internalSetMethodProcedure = new SlotWriter (delegate (StandardObject method, object newValue)
        {
            return internalSlotSet (method, QuoteProcedure, newValue);
        });

        static void InternalSetMethodProcedure (StandardObject method, MethodFunction proc)
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


        static SlotReader internalMethodSpecializers =
            new SlotReader (delegate (StandardObject method)
        {
            return internalSlotRef (method, QuoteSpecializers);
        });

        static Cons InternalMethodSpecializers (StandardObject method)
        {
           return (Cons) internalMethodSpecializers (GuaranteeMethod (method));
        }

        static SlotWriter internalSetMethodSpecializers =
    new SlotWriter (delegate (StandardObject method, object specializers)
    {
        return internalSlotSet (method, QuoteSpecializers, specializers);
    });


	// End of internal slot accessors

        static Cons SLOTS_OF_STANDARD_CLASS =
              CL.List (CL.List (QuoteDefaultInitargs),
                       CL.List (QuoteDirectDefaultInitargs),
                       CL.List (QuoteDirectSlots),
                       CL.List (QuoteDirectSubclasses),
                       CL.List (QuoteDirectSuperclasses),
                       CL.List (QuoteFinalizedP),
                       CL.List (QuoteName),
                       CL.List (QuoteClassPrecedenceList),
                       CL.List (QuoteClassPrototype),
                       CL.List (QuoteGettersAndSetters),
                       CL.List (QuoteClassSlots));

        static private StandardObject bootstrapMakeStandardClass ()
        {
            //ManifestInstance manifestInstance = new ManifestInstance (CL.Length (SLOTS_OF_STANDARD_CLASS));
            //StandardObject instance =
            //   (StandardObject) Delegate.CreateDelegate (typeof (StandardObject),
            //                                             manifestInstance,
            //                                             defaultInstanceMethod);
            // manifestInstance.Class = instance;
            StandardObject instance = ManifestInstance.CreateInstance (null, CL.Length (SLOTS_OF_STANDARD_CLASS));
            ((ManifestInstance) instance.Target).Class = instance;
            return instance;
        }

        static readonly StandardObject standardClass
                = bootstrapMakeStandardClass ();

        //// Bootstrap step
        delegate object MapFunction (Cons arguments);

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
                            new SlotChanger (delegate (StandardObject obj, SlotChangingFunction func)
            {
                return StandardInstanceChange (obj, index, func);
            })
                            );
        }),
                           SLOTS_OF_STANDARD_CLASS);

        static object bootstrapStep0 =
           ((SlotWriter) (CL.Caddr (CL.Assq (QuoteGettersAndSetters, GettersAndSettersForClass)))) (StandardClass, GettersAndSettersForClass);

        static SlotReader internalClassGettersAndSetters =
           (SlotReader) (CL.Cadr (CL.Assq (QuoteGettersAndSetters, GettersAndSettersForClass)));

        static Cons InternalClassGettersAndSetters (StandardObject closClass)
        {
            return (Cons) internalClassGettersAndSetters (GuaranteeClass (closClass));
        }

        static object bootstrapStep0a =
            ((SlotWriter) (CL.Caddr (CL.Assq (QuoteName, GettersAndSettersForClass)))) (StandardClass, QuoteStandardClass);

        static readonly StandardObject top
            = (StandardObject) CL.MakeInstance (StandardClass,  // wrong, but fixed later
                            KW.DirectSuperclasses, CL.List (),
                            KW.Name, QuoteTop);

        static readonly StandardObject function
            = (StandardObject) CL.MakeInstance (StandardClass, // wrong, but fixed later
                            KW.DirectSuperclasses, CL.List (Top),
                            KW.Name, QuoteFunction);

        static readonly StandardObject standardObjectClass
            = (StandardObject) CL.MakeInstance (StandardClass,
                            KW.DirectSuperclasses, CL.List (Top),
                            KW.Name, QuoteStandardObject);

        static readonly StandardObject metaobject
            = (StandardObject) CL.MakeInstance (StandardClass,
                            KW.DirectSuperclasses, CL.List (StandardObjectClass),
                            KW.Name, QuoteMetaobject);

        static readonly StandardObject specializer
            = (StandardObject) CL.MakeInstance (StandardClass,
                            KW.DirectSuperclasses, CL.List (Metaobject),
                            KW.Name, QuoteSpecializer);

        static readonly StandardObject closClass
            = (StandardObject) CL.MakeInstance (StandardClass,
                            KW.DirectSuperclasses, CL.List (Specializer),
                            KW.Name, QuoteClass);

        static readonly StandardObject funcallableStandardClass
    = (StandardObject) CL.MakeInstance (StandardClass,
                    KW.DirectSuperclasses, CL.List (ClosClass),
                    KW.Name, QuoteFuncallableStandardClass);




        // Fixup the standard class
        static int bootstrapFixupStandardClass ()
        {
            internalSetClassPrecedenceList (standardClass, CL.List (ClosClass, Specializer, Metaobject, StandardObjectClass, Top));
            internalSetClassPrecedenceList (funcallableStandardClass, CL.List (ClosClass, Specializer, Metaobject, StandardObjectClass, Top));
            internalSetClassDirectSlots (standardClass, SLOTS_OF_STANDARD_CLASS);
            internalSetClassDirectSlots (funcallableStandardClass, SLOTS_OF_STANDARD_CLASS);
            internalSetClassDirectSuperclasses (standardClass, CL.List (ClosClass));
            internalSetClassDirectSuperclasses (funcallableStandardClass, CL.List (ClosClass));
            internalSetClassName (standardClass, QuoteStandardClass);
            internalSetClassSlots (standardClass, SLOTS_OF_STANDARD_CLASS);
            internalSetClassSlots (funcallableStandardClass, SLOTS_OF_STANDARD_CLASS);
            if (internalClassDirectSlots (standardClass) != SLOTS_OF_STANDARD_CLASS)
                throw new NotImplementedException ();
            return 1;
        }

        static int foo = bootstrapFixupStandardClass ();
        static object bootstrapStep1 =
   ((SlotWriter) (CL.Caddr (CL.Assq (QuoteGettersAndSetters, GettersAndSettersForClass)))) (funcallableStandardClass, GettersAndSettersForClass);


        static StandardObject bootstrapMakeBuiltInClass ()
        {
            StandardObject builtInClass = (StandardObject) CL.MakeInstance (StandardClass,
                                                           KW.DirectSuperclasses, CL.List (ClosClass),
                                                           KW.Name, QuoteBuiltInClass);
            // fixup Top and Function
            // Hard to do.  I need to determine the class slots of builtInClass
            // and bootstrap them.
            //((ManifestInstance)(Top.Target)).Class = builtInClass;
            //  ((ManifestInstance)(Function.Target)).Class = builtInClass;
            return builtInClass;
        }

        static readonly StandardObject builtInClass = 
bootstrapMakeBuiltInClass ();


        static readonly StandardObject funcallableStandardObject =
        (StandardObject) CL.MakeInstance (StandardClass,
                            KW.DirectSuperclasses, CL.List (StandardObjectClass), // Function),
                            KW.Name, QuoteFuncallableStandardObject);

        static readonly StandardObject genericFunction
            = (StandardObject) CL.MakeInstance (FuncallableStandardClass,
                                               KW.DirectSuperclasses, CL.List (Metaobject, FuncallableStandardObject),
                                               KW.Name, QuoteGenericFunction);

        static readonly StandardObject standardGenericFunction
                = (StandardObject) CL.MakeInstance (FuncallableStandardClass,
                                KW.DirectSlots, CL.List (CL.List (QuoteApplicationCache),
                                                         CL.List (QuoteLambdaList), 
                                                         CL.List (QuoteMethods),
                                                         CL.List (QuoteName),
                                                         CL.List (QuoteMethodClass),
                                                         CL.List (QuoteSingletonsList)),
                                KW.DirectSuperclasses, CL.List (GenericFunction),
                                KW.Name, QuoteStandardGenericFunction);

        static readonly StandardObject genericFunctionMethodClass =
            (StandardObject) CL.MakeInstance (standardGenericFunction,
                                              KW.Name, QuoteGenericFunctionMethodClass,
                                              KW.LambdaList, CL.List (QuoteGenericFunction));

        static readonly StandardObject method =
            (StandardObject) CL.MakeInstance (FuncallableStandardClass,
                                              KW.DirectSuperclasses, CL.List (Metaobject, FuncallableStandardObject),
                                              KW.Name, QuoteMethod);

        static readonly StandardObject standardMethod =
            (StandardObject) CL.MakeInstance (FuncallableStandardClass,
                                              KW.DirectSlots, CL.List (
                                                                       CL.List (QuoteLambdaList),
                                                                       CL.List (QuoteName),
                                                                       CL.List (QuoteProcedure),
                                                                       CL.List (QuoteQualifier),
                                                                       CL.List (QuoteSpecializers)),
                                              KW.DirectSuperclasses, CL.List (Method),
                                              KW.Name, QuoteStandardMethod);

        static readonly StandardObject allocateInstance =
            (StandardObject) CL.MakeInstance (StandardGenericFunction,
                                              KW.Name, QuoteAllocateInstance,
                                              KW.LambdaList, CL.List (QuoteClass, QuoteAndRest, QuoteInitargs, QuoteAndKey, QuoteAndAllowOtherKeys));

        static readonly StandardObject computeApplicableMethods =
            (StandardObject) CL.MakeInstance (StandardGenericFunction,
                                              KW.Name, QuoteComputeApplicableMethods,
                                              KW.LambdaList, CL.List (QuoteGenericFunction, QuoteArguments));

        static readonly StandardObject computeDiscriminatingFunction =
            (StandardObject) CL.MakeInstance (StandardGenericFunction,
                                              KW.Name, QuoteComputeDiscriminatingFunction,
                                              KW.LambdaList, CL.List (QuoteGenericFunction));

        static readonly StandardObject computeEffectiveMethod =
            (StandardObject) CL.MakeInstance (StandardGenericFunction,
                                              KW.Name, QuoteComputeEffectiveMethod,
                                              KW.LambdaList, CL.List (QuoteComputeEffectiveMethod));

        static readonly StandardObject addMethod =
            (StandardObject) CL.MakeInstance (StandardGenericFunction,
                                              KW.Name, QuoteAddMethod,
                                              KW.LambdaList, CL.List (QuoteGenericFunction, QuoteMethod));

        static readonly List<StandardObject> GenericInvocationGenerics = 
            new List<StandardObject> {computeDiscriminatingFunction,
                                      computeApplicableMethods,
                                      computeEffectiveMethod};

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

        static bool SameMethodSignature (StandardObject left, StandardObject right)
        {
            throw new NotImplementedException ("SameMethodSignature");
        }

        static Cons nFalses (int n)
        {
            if (n == 0)
                return null;
            else
                return new Cons (null, nFalses (n - 1));
        }

        static object bootstrapAddMethod (StandardObject self, object [] arguments)
        {
            StandardObject generic = (StandardObject) (arguments [0]);
            StandardObject method = (StandardObject) (arguments [1]);

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
            ApplyHandler df =(ApplyHandler) ComputeDiscriminatingFunction (generic);
            setFuncallableInstanceFunction (generic, df);
            return null;
        }

        static object bootstrapGenericFunctionMethodClass (StandardObject self, object [] arguments)
        {
            return InternalGenericMethodClass ((StandardObject) arguments [0]);
        }

        static object bootstrapComputeDiscriminatingFunction (StandardObject self, object [] arguments)
        {
            StandardObject generic = (StandardObject) arguments[0];
            StandardObject firstMethod = (StandardObject) CL.Car (InternalGenericMethods (generic));
            MethodFunction procedure = InternalMethodProcedure (firstMethod);
            ApplyHandler answer = (ApplyHandler) procedure (null, arguments);
            return answer;
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

        static ApplyHandler computeCallable (bool isGroundCase, StandardObject generic, MethodCache cache, Cons keys, object [] arguments)
        {
            ApplyHandler answer;
            if (isGroundCase) {
                MethodFunction m = InternalMethodProcedure ((StandardObject) last (InternalGenericMethods (generic)));
                answer = new ApplyHandler (delegate (StandardObject gen, object [] args) { return m(null, args); });
            }
            else {
                object [] newArguments = new object [arguments.Length + 1];
                arguments.CopyTo (newArguments, 1);
                newArguments [0] = generic;
                answer = (ApplyHandler) ComputeEffectiveMethod (generic, ComputeApplicableMethods (newArguments));
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
            ApplyHandler ah;
            if (!cache.TryGetValue (keys, out ah))
                ah = computeCallable (isGroundCase, generic, cache, keys, arguments);

            return ah (generic, arguments);
        }

        static ApplyHandler realMethodComputeDiscriminatingFunction (NextMethodFunction callNextMethod, object [] ignore)
        {
            return  new ApplyHandler (realComputeDiscriminatingFunction);
        }

        static MethodFunction realComputeApplicableMethods =
            new MethodFunction (delegate (NextMethodFunction callNextMethod, object [] args)
                {
                    throw new NotImplementedException ("realComputeApplicableMethods");
                });

        static MethodFunction realComputeEffectiveMethod =
            new MethodFunction (delegate (NextMethodFunction callNextMethod, object [] args)
            {
                throw new NotImplementedException ("realComputeEffectiveMethod");
            });

        static bool bootstrapStep2Function ()
        {
            setFuncallableInstanceFunction (addMethod, bootstrapAddMethod);
            setFuncallableInstanceFunction (genericFunctionMethodClass, bootstrapGenericFunctionMethodClass);
            setFuncallableInstanceFunction (computeDiscriminatingFunction, bootstrapComputeDiscriminatingFunction);
            // Now we can add methods.
            CL.AddMethod (ComputeDiscriminatingFunction,
                          CL.MakeInstance (GenericFunctionMethodClass (ComputeDiscriminatingFunction),
                                           KW.Name, QuoteMethodComputeDiscriminatingFunction,
                                           KW.Procedure, (MethodFunction) realMethodComputeDiscriminatingFunction,
                                           KW.Specializers, CL.List (StandardGenericFunction)));
            CL.AddMethod (ComputeApplicableMethods,
                          CL.MakeInstance (GenericFunctionMethodClass (ComputeApplicableMethods),
                                           KW.Name, QuoteMethodComputeApplicableMethods,
                                           KW.Procedure, realComputeApplicableMethods,
                                           KW.Specializers, CL.List (StandardGenericFunction)));
            CL.AddMethod (ComputeEffectiveMethod,
                          CL.MakeInstance (GenericFunctionMethodClass (ComputeEffectiveMethod),
                                           KW.Name, QuoteMethodComputeEffectiveMethod,
                                           KW.Procedure, realComputeEffectiveMethod,
                                           KW.Specializers, CL.List (StandardGenericFunction)));
            return true;
        }



        static readonly bool bootstrap_step2 = bootstrapStep2Function ();

        static object realMakeInstance (NextMethodFunction callNextMethod, object [] args)
        {
            throw new NotImplementedException ("rMI");
        }

        static bool bootstrapStep3Function ()
        {
            // Fixup the generic for make.
            StandardObject newMakeInstance = (StandardObject) CL.MakeInstance (StandardGenericFunction,
                                                                               KW.Name, QuoteNewMakeInstance,
                                                                               KW.LambdaList, CL.List (QuoteClass, QuoteAndRest, QuoteInitargs));
            AddMethod (newMakeInstance,
                       CL.MakeInstance (StandardMethod,
                                        KW.Name, QuoteNewMakeInstanceMethod,
                                        KW.Specializers, CL.List (StandardClass),
                                        KW.Procedure, (MethodFunction) realMakeInstance));

            ManifestInstance oldMakeInstance = (ManifestInstance) CL.MakeInstance.Target;
            ManifestInstance newInstance = (ManifestInstance) newMakeInstance.Target;
            oldMakeInstance.Class = newInstance.Class;
            oldMakeInstance.Slots = newInstance.Slots;
            oldMakeInstance.OnApply = newInstance.OnApply;
            return true;
        }

        static readonly bool bootstrap_step3 = bootstrapStep3Function ();

        static readonly StandardObject ensureGenericFunction
                = (StandardObject) CL.MakeInstance (StandardGenericFunction,
                                                    KW.Name, QuoteEnsureGenericFunction,
                                                    KW.LambdaList,               
                                                    CL.List (QuoteGenericFunction,
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
                                                          QuoteAndAllowOtherKeys));

        static public StandardObject AddMethod
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return addMethod;
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

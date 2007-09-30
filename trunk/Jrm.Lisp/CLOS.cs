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
        static int serialNumberCounter; // starts at zero
        readonly int serialNumber;
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

        public int SerialNumber
        {
           get
           {
               return this.SerialNumber;
           }
        }

        public object SlotVector
        {
            get
            {
                return this.slotVector;
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
            throw new NotImplementedException ("Application of non-funcallable instance " + this.ToString() + " applied to " + arguments.ToString());
        }
    }

    delegate object FuncallableInstanceProcedure (object [] arguments);

    class FuncallableManifestInstance : ManifestInstance
    {
        FuncallableInstanceProcedure procedure;

        public FuncallableManifestInstance (StandardObject closClass, int slotCount)
            : base (closClass, slotCount)
        {
            this.procedure = (FuncallableInstanceProcedure) UninitializedFuncallableInstanceMethod;
        }

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

        static Symbol QuoteAndAllowOtherKeys       = Package.CommonLisp.FindSymbol ("&ALLOW-OTHER-KEYS");
        static Symbol QuoteAndKey                  = Package.CommonLisp.FindSymbol ("&KEY");
        static Symbol QuoteAnonymous               = Package.CommonLisp.FindSymbol ("[unnamed]");
        static Symbol QuoteArgumentPrecedenceOrder = closSymbol ("ARGUMENT-PRECEDENCE-ORDER");
        static Symbol QuoteBuiltInClass            = closSymbol ("BUILT-IN-CLASS");
        static Symbol QuoteClassPrecedenceList     = closSymbol ("CLASS-PRECEDENCE-LIST");
        static Symbol QuoteClassPrototype          = closSymbol ("CLASS-PROTOTYPE");
        static Symbol QuoteClassSlots              = closSymbol ("CLASS-SLOTS");
        static Symbol QuoteClass                   = closSymbol ("CLASS");
        static Symbol QuoteDeclarations            = closSymbol ("DECLARATIONS");
        static Symbol QuoteDefaultInitargs         = closSymbol ("DEFAULT-INITARGS");
        static Symbol QuoteDirectDefaultInitargs   = closSymbol ("DIRECT-DEFAULT-INITARGS");
        static Symbol QuoteDirectSlots             = closSymbol ("DIRECT-SLOTS");
        static Symbol QuoteDirectSubclasses        = closSymbol ("DIRECT-SUBCLASSES");
        static Symbol QuoteDirectSuperclasses      = closSymbol ("DIRECT-SUPERCLASSES");
        static Symbol QuoteDocumentation           = closSymbol ("DOCUMENTATION");
    	static Symbol QuoteEnsureGenericFunction   = Package.CommonLisp.FindSymbol ("ENSURE-GENERIC-FUNCTION");
        static Symbol QuoteFinalizedP              = closSymbol ("FINALIZEDP");
        static Symbol QuoteFuncallableStandardClass = closSymbol ("FUNCALLABLE-STANDARD-CLASS");
        static Symbol QuoteFuncallableStandardObject = closSymbol ("FUNCALLABLE-STANDARD-OBJECT");
        static Symbol QuoteFunction                = closSymbol ("FUNCTION");
        static Symbol QuoteFunctionName            = closSymbol ("FUNCTION-NAME");
        static Symbol QuoteGenericFunction         = closSymbol ("GENERIC-FUNCTION");
        static Symbol QuoteGenericFunctionClass    = closSymbol ("GENERIC-FUNCTION-CLASS");
        static Symbol QuoteGettersAndSetters       = closSymbol ("GETTERS-AND-SETTERS");
        static Symbol QuoteLambdaList              = closSymbol ("LAMBDA-LIST");
        static Symbol QuoteList                    = Package.CommonLisp.FindSymbol ("LIST");
        static Symbol QuoteMakeInstance            = Package.CommonLisp.FindSymbol ("MAKE-INSTANCE");
        static Symbol QuoteMetaobject              = closSymbol ("METAOBJECT");
        static Symbol QuoteMethodClass             = closSymbol ("METHOD-CLASS");
        static Symbol QuoteMethodCombination       = closSymbol ("METHOD-COMBINATION");
        static Symbol QuoteName                    = closSymbol ("NAME");
        static Symbol QuoteSpecializer             = closSymbol ("SPECIALIZER");
        static Symbol QuoteStandardGenericFunction = closSymbol ("STANDARD-GENERIC-FUNCTION");
        static Symbol QuoteStandardObject          = closSymbol ("STANDARD-OBJECT");
        static Symbol QuoteTop                     = closSymbol ("TOP");

        delegate object SlotReader (StandardObject obj);
        delegate object SlotWriter (StandardObject obj, object newValue);
        delegate object SlotChanger (StandardObject obj, Delegate func);

        // ClassOf
        delegate StandardObject ClassOfFunction (object obj);

        static ClassOfFunction classOf =
           new ClassOfFunction (delegate (object instance)
        {
            StandardObject probe = instance as StandardObject;
            return (probe == null)
              ? Top
              : ((ManifestInstance) (probe.Target)).Class;
        });

        static ClassOfFunction ClassOf
        {
            get
            {
                return classOf;
            }
        }

	delegate Cons amf (Object obj);
	static Cons AppendMap (amf del, Cons list) {
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
            return bootstrapComputeClassPrecedenceList ((Cons) CL.Append (superclasses.Cdr, internalClassDirectSuperclasses ((StandardObject) superclasses.Car)),
                                                        CL.Memq (superclasses.Car, soFar)
                                                           ? soFar
                                                           : CL.Cons (superclasses.Car, soFar));
        }

    delegate Cons Allocator (object initargs);

    static object unspecificInitializer ()
    {
        throw new NotImplementedException ();
    }

        static object bootstrapMakeInstance (object [] arguments)
        {
            StandardObject closClass = (StandardObject) arguments [0];
            Cons initargs = Cons.SubvectorToList (arguments, 1, arguments.Length);
            if (closClass == StandardClass 
                || closClass == FuncallableStandardClass) {
                StandardObject instance =
                   (StandardObject)
                    Delegate.CreateDelegate (typeof (StandardObject),
                            new ManifestInstance (closClass, CL.Length (SLOTS_OF_STANDARD_CLASS)),
                            defaultInstanceMethod);

	        Cons directDefaultInitargs = (Cons) Utility.GetArg (initargs, KW.DirectDefaultInitargs, null);
	        Cons directSlots           = (Cons) Utility.GetArg (initargs, KW.DirectSlots, null);
                Cons directSuperclasses         = (Cons) Utility.GetArg (initargs, KW.DirectSuperclasses, null);
                Symbol name                = (Symbol) Utility.GetArg (initargs, KW.Name, QuoteAnonymous);
                Cons classPrecedenceList   = bootstrapComputeClassPrecedenceList (directSuperclasses, CL.List (instance));
                Cons inheritedSlots = AppendMap (new amf (delegate (object superclass)
                {
                    return (Cons) internalClassDirectSlots ((StandardObject) superclass);
                }), (Cons) CL.Cdr (classPrecedenceList));
                Cons slots                 = (Cons) CL.Append (directSlots, inheritedSlots);
                                                        
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
                             new SlotChanger (delegate (StandardObject o, Delegate modify)
                             {
                                 return SetStandardInstanceAccess (o, f, modify.DynamicInvoke (StandardInstanceAccess (o, f)));
                             }));
                });

            Cons gettersAndSetters =
              (Cons) CL.Map (QuoteList, new MapFunction (
                                  delegate (Cons s) {
                                      return CL.Cons (s.Car, allocator.DynamicInvoke (Utility.GetArg (s.Cdr, KW.Initializer, null)));
}), slots);
                if (CL.Length (slots) < CL.Length (directSlots))
                    throw new NotImplementedException();
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
            else if (closClass == StandardGenericFunction) {
                StandardObject instance =
                    (StandardObject)
                    Delegate.CreateDelegate (typeof (StandardObject),
                                             new FuncallableManifestInstance (closClass, CL.Length (internalClassSlots (closClass))),
                                             defaultFuncallableInstanceMethod);
                return instance;
            }

            throw new NotImplementedException ("something else");
        }

        static readonly StandardObject makeInstance =
                (StandardObject) Delegate.CreateDelegate (typeof (StandardObject),
                                                          null,
                                                          typeof (CLOS)
                                                              .GetMethod ("bootstrapMakeInstance",
                                                                          System.Reflection.BindingFlags.Static
                                                                          | System.Reflection.BindingFlags.NonPublic));


        static Cons lookupSlotInfo (StandardObject closClass, Symbol slotName)
        {
            Symbol className = null;
            object [] classSlots;
            Cons probe = CL.Assq (slotName, internalClassGettersAndSetters (closClass));
            if (probe == null) {
                className = (Symbol) internalClassName (closClass);
                classSlots = (object []) ((ManifestInstance) (closClass.Target)).SlotVector;
                throw new NotImplementedException ();
            }
            return probe;
        }

        static object internalSlotRef (StandardObject obj, Symbol slotName)
        {
            return ((SlotReader) (CL.Cadr (lookupSlotInfo (ClassOf (obj), slotName)))) (obj);
        }

        static object internalSlotSet (StandardObject obj, Symbol slotName, object value)
        {
            return ((SlotWriter) (CL.Caddr (lookupSlotInfo (ClassOf (obj), slotName)))) (obj, value);
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
        static SlotWriter internalSetClassIsFinalized =
          new SlotWriter (delegate (StandardObject obj, object newValue)
        {
            return internalSlotSet (obj, QuoteFinalizedP, newValue);
        });
        static SlotReader internalClassName =
          new SlotReader (delegate (StandardObject obj)
        {
            return internalSlotRef (obj, QuoteName);
        });
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
            object [] slots = (object[])((ManifestInstance)(obj.Target)).SlotVector;
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
            ManifestInstance manifestInstance = new ManifestInstance (CL.Length (SLOTS_OF_STANDARD_CLASS));
            StandardObject instance =
               (StandardObject) Delegate.CreateDelegate (typeof (StandardObject),
                                                         manifestInstance,
                                                         defaultInstanceMethod);
            manifestInstance.Class = instance;
            
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
                            new SlotChanger (delegate (StandardObject obj, Delegate func)
            {
                return SetStandardInstanceAccess (obj, index, func.DynamicInvoke (StandardInstanceAccess (obj, index)));
            })
                            );
        }),
                           SLOTS_OF_STANDARD_CLASS);

        static object bootstrapStep0 =
           ((SlotWriter) (CL.Caddr (CL.Assq (QuoteGettersAndSetters, GettersAndSettersForClass)))) (StandardClass, GettersAndSettersForClass);

        static SlotReader internalClassGettersAndSetters =
           (SlotReader) (CL.Cadr (CL.Assq (QuoteGettersAndSetters, GettersAndSettersForClass)));

        static readonly StandardObject top
            = (StandardObject)CL.MakeInstance (StandardClass,  // wrong, but fixed later
                            KW.DirectSuperclasses, CL.List (),
                            KW.Name, QuoteTop);

        static readonly StandardObject function
            = (StandardObject)CL.MakeInstance (StandardClass, // wrong, but fixed later
                            KW.DirectSuperclasses, CL.List (Top),
                            KW.Name, QuoteFunction);

        static readonly StandardObject standardObjectClass
            = (StandardObject)CL.MakeInstance (StandardClass,
                            KW.DirectSuperclasses, CL.List (Top),
                            KW.Name, QuoteStandardObject);

        static readonly StandardObject metaobject
            = (StandardObject)CL.MakeInstance (StandardClass,
                            KW.DirectSuperclasses, CL.List (StandardObjectClass),
                            KW.Name, QuoteMetaobject);

        static readonly StandardObject specializer
            = (StandardObject)CL.MakeInstance (StandardClass,
                            KW.DirectSuperclasses, CL.List (Metaobject),
                            KW.Name, QuoteSpecializer);

        static readonly StandardObject closClass
            = (StandardObject)CL.MakeInstance (StandardClass,
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
            internalSetClassSlots (standardClass, SLOTS_OF_STANDARD_CLASS);
            internalSetClassSlots (funcallableStandardClass, SLOTS_OF_STANDARD_CLASS);
            if (internalClassDirectSlots (standardClass) != SLOTS_OF_STANDARD_CLASS)
                throw new NotImplementedException();
            return 1;
        }

        int foo = bootstrapFixupStandardClass ();
        static object bootstrapStep1 =
   ((SlotWriter) (CL.Caddr (CL.Assq (QuoteGettersAndSetters, GettersAndSettersForClass)))) (funcallableStandardClass, GettersAndSettersForClass);


	static StandardObject bootstrapMakeBuiltInClass ()
        {
            StandardObject builtInClass = (StandardObject) CL.MakeInstance (StandardClass,
                                                           KW.DirectSuperclasses, CL.List (ClosClass),
                                                           KW.Name, QuoteBuiltInClass);
	    // fixup Top and Function
            ((ManifestInstance)(Top.Target)).Class = builtInClass;
            ((ManifestInstance)(Function.Target)).Class = builtInClass;
            return builtInClass;
        }

        static readonly StandardObject builtInClass
            = bootstrapMakeBuiltInClass ();


        static readonly StandardObject funcallableStandardObject
            = (StandardObject)CL.MakeInstance (StandardClass,
                            KW.DirectSuperclasses, CL.List (StandardObjectClass, Function),
                            KW.Name, QuoteFuncallableStandardObject);

        static readonly StandardObject genericFunction
            = (StandardObject) CL.MakeInstance (FuncallableStandardClass,
                                               KW.DirectSuperclasses, CL.List (Metaobject, FuncallableStandardObject),
                                               KW.Name, QuoteGenericFunction);

        static readonly StandardObject standardGenericFunction 
                = (StandardObject)CL.MakeInstance (FuncallableStandardClass,
                                KW.DirectSuperclasses, CL.List (GenericFunction),
                                KW.Name, QuoteStandardGenericFunction);



        static readonly StandardObject ensureGenericFunction
                = (StandardObject)CL.MakeInstance (StandardGenericFunction, CL.List (QuoteGenericFunction,
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

	static public StandardObject ClosClass
        {
            get
            {
               return closClass;
            }
        }

	static public StandardObject EnsureGenericFunction
        {
            get
            {
               return ensureGenericFunction;
            }
        }

	static public StandardObject FuncallableStandardClass
        {
            get
            {
               return funcallableStandardClass;
            }
        }

	static public StandardObject FuncallableStandardObject
        {
            get
            {
               return funcallableStandardObject;
            }
        }

	static public StandardObject Function
        {
            get
            {
               return function;
            }
        }

	static public StandardObject GenericFunction
        {
            get
            {
               return genericFunction;
            }
        }


        static public StandardObject MakeInstance
        {
            get
            {
                return makeInstance;
            }
        }

        static public StandardObject Metaobject
        {
            get
            {
                return metaobject;
            }
        }

        static public StandardObject Specializer
        {
            get
            {
                return specializer;
            }
        }

        static public StandardObject StandardClass
        {
            get
            {
                return standardClass;
            }
        }

        static public StandardObject StandardGenericFunction
        {
            get
            {
                return standardGenericFunction;
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

        static public StandardObject StandardObjectClass
        {
            get
            {
                return standardObjectClass;
            }
        }

        static public StandardObject Top
        {
            get
            {
 		return top;
            }
        }



	


        static Symbol closSymbol (string name)
        {
            return closPackage.Intern (name);
        }

    }
}

using System;
using System.Diagnostics;

namespace Lisp
{
    public sealed class KW
    {
        private KW ()
        {
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword absolute = (Keyword) Package.Keyword.Intern ("ABSOLUTE");
        public static Keyword Absolute
        {
            [DebuggerStepThrough]
            get
            {
                return absolute;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword allocation = (Keyword) Package.Keyword.Intern ("ALLOCATION");
        public static Keyword Allocation
        {
            [DebuggerStepThrough]
            get
            {
                return allocation;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword allowOtherKeys = (Keyword) Package.Keyword.Intern ("ALLOW-OTHER-KEYS");
        public static Keyword AllowOtherKeys
        {
            [DebuggerStepThrough]
            get
            {
                return allowOtherKeys;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword append = (Keyword) Package.Keyword.Intern ("APPEND");
        public static Keyword Append
        {
            [DebuggerStepThrough]
            get
            {
                return append;
            }
        }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        static readonly Keyword arity = (Keyword) Package.Keyword.Intern ("ARITY");
        public static Keyword Arity
        {
            [DebuggerStepThrough]
            get
            {
                return arity;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword back = (Keyword) Package.Keyword.Intern ("BACK");
        public static Keyword Back
        {
            [DebuggerStepThrough]
            get
            {
                return back;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword kase = (Keyword) Package.Keyword.Intern ("CASE");
        public static Keyword Case
        {
            [DebuggerStepThrough]
            get
            {
                return kase;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword common = (Keyword) Package.Keyword.Intern ("COMMON");
        public static Keyword Common
        {
            [DebuggerStepThrough]
            get
            {
                return common;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword compileToplevel = (Keyword) Package.Keyword.Intern ("COMPILE-TOPLEVEL");
        public static Keyword CompileToplevel
        {
            [DebuggerStepThrough]
            get
            {
                return compileToplevel;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword create = (Keyword) Package.Keyword.Intern ("CREATE");
        public static Keyword Create
        {
            [DebuggerStepThrough]
            get
            {
                return create;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword dfault = (Keyword) Package.Keyword.Intern ("DEFAULT");
        public static Keyword Default
        {
            [DebuggerStepThrough]
            get
            {
                return dfault;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword defaults = (Keyword) Package.Keyword.Intern ("DEFAULTS");
        public static Keyword Defaults
        {
            [DebuggerStepThrough]
            get
            {
                return defaults;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword device = (Keyword) Package.Keyword.Intern ("DEVICE");
        public static Keyword Device
        {
            [DebuggerStepThrough]
            get
            {
                return device;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword directDefaultInitargs = (Keyword) Package.Keyword.Intern ("DIRECT-DEFAULT-INITARGS");
        public static Keyword DirectDefaultInitargs
        {
            [DebuggerStepThrough]
            get
            {
                return directDefaultInitargs;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword directSlots = (Keyword) Package.Keyword.Intern ("DIRECT-SLOTS");
        public static Keyword DirectSlots
        {
            [DebuggerStepThrough]
            get
            {
                return directSlots;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword directSuperclasses = (Keyword) Package.Keyword.Intern ("DIRECT-SUPERCLASSES");
        public static Keyword DirectSuperclasses
        {
            [DebuggerStepThrough]
            get
            {
                return directSuperclasses;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword directory = (Keyword) Package.Keyword.Intern ("DIRECTORY");
        public static Keyword Directory
        {
            [DebuggerStepThrough]
            get
            {
                return directory;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword direction = (Keyword) Package.Keyword.Intern ("DIRECTION");
        public static Keyword Direction
        {
            [DebuggerStepThrough]
            get
            {
                return direction;
            }
        }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        static readonly Keyword dotnetType = (Keyword) Package.Keyword.Intern ("DOTNET-TYPE");
        public static Keyword DotnetType
        {
            [DebuggerStepThrough]
            get
            {
                return dotnetType;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword elementType = (Keyword) Package.Keyword.Intern ("ELEMENT-TYPE");
        public static Keyword ElementType
        {
            [DebuggerStepThrough]
            get
            {
                return elementType;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword end = (Keyword) Package.Keyword.Intern ("END");
        public static Keyword End
        {
            [DebuggerStepThrough]
            get
            {
                return end;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword end1 = (Keyword) Package.Keyword.Intern ("END1");
        public static Keyword End1
        {
            [DebuggerStepThrough]
            get
            {
                return end1;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword end2 = (Keyword) Package.Keyword.Intern ("END2");
        public static Keyword End2
        {
            [DebuggerStepThrough]
            get
            {
                return end2;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword environment = (Keyword) Package.Keyword.Intern ("ENVIRONMENT");
        public static Keyword Environment
        {
            [DebuggerStepThrough]
            get
            {
                return environment;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword error = (Keyword) Package.Keyword.Intern ("ERROR");
        public static Keyword Error
        {
            [DebuggerStepThrough]
            get
            {
                return error;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword externalFormat = (Keyword) Package.Keyword.Intern ("EXTERNAL-FORMAT");
        public static Keyword ExternalFormat
        {
            [DebuggerStepThrough]
            get
            {
                return externalFormat;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword execute = (Keyword) Package.Keyword.Intern ("EXECUTE");
        public static Keyword Execute
        {
            [DebuggerStepThrough]
            get
            {
                return execute;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword fromEnd = (Keyword) Package.Keyword.Intern ("FROM-END");
        public static Keyword FromEnd
        {
            [DebuggerStepThrough]
            get
            {
                return fromEnd;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword host = (Keyword) Package.Keyword.Intern ("HOST");
        public static Keyword Host
        {
            [DebuggerStepThrough]
            get
            {
                return host;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword ifDoesNotExist = (Keyword) Package.Keyword.Intern ("IF-DOES-NOT-EXIST");
        public static Keyword IfDoesNotExist
        {
            [DebuggerStepThrough]
            get
            {
                return ifDoesNotExist;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword ifExists = (Keyword) Package.Keyword.Intern ("IF-EXISTS");
        public static Keyword IfExists
        {
            [DebuggerStepThrough]
            get
            {
                return ifExists;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword input = (Keyword) Package.Keyword.Intern ("INPUT");
        public static Keyword Input
        {
            [DebuggerStepThrough]
            get
            {
                return input;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword initializer = (Keyword) Package.Keyword.Intern ("INITIALIZER");
        public static Keyword Initializer
        {
            [DebuggerStepThrough]
            get
            {
                return initializer;
            }
        }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        static readonly Keyword initarg = (Keyword) Package.Keyword.Intern ("INITARG");
        public static Keyword Initarg
        {
            [DebuggerStepThrough]
            get
            {
                return initarg;
            }
        }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        static readonly Keyword initargs = (Keyword) Package.Keyword.Intern ("INITARGS");
        public static Keyword Initargs
        {
            [DebuggerStepThrough]
            get
            {
                return initargs;
            }
        }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        static readonly Keyword initform = (Keyword) Package.Keyword.Intern ("INITFORM");
        public static Keyword Initform
        {
            [DebuggerStepThrough]
            get
            {
                return initform;
            }
        }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        static readonly Keyword initfunction = (Keyword) Package.Keyword.Intern ("INITFUNCTION");
        public static Keyword Initfunction
        {
            [DebuggerStepThrough]
            get
            {
                return initfunction;
            }
        }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        static readonly Keyword initvalue = (Keyword) Package.Keyword.Intern ("INITVALUE");
        public static Keyword Initvalue
        {
            [DebuggerStepThrough]
            get
            {
                return initvalue;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword io = (Keyword) Package.Keyword.Intern ("IO");
        public static Keyword IO
        {
            [DebuggerStepThrough]
            get
            {
                return io;
            }
        }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        static readonly Keyword instance = (Keyword) Package.Keyword.Intern ("INSTANCE");
        public static Keyword Instance
        {
            [DebuggerStepThrough]
            get
            {
                return instance;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword junkAllowed = (Keyword) Package.Keyword.Intern ("JUNK-ALLOWED");
        public static Keyword JunkAllowed
        {
            [DebuggerStepThrough]
            get
            {
                return junkAllowed;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword key = (Keyword) Package.Keyword.Intern ("KEY");
        public static Keyword Key
        {
            [DebuggerStepThrough]
            get
            {
                return key;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword lambdaList = (Keyword) Package.Keyword.Intern ("LAMBDA-LIST");
        public static Keyword LambdaList
        {
            [DebuggerStepThrough]
            get
            {
                return lambdaList;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword loadToplevel = (Keyword) Package.Keyword.Intern ("LOAD-TOPLEVEL");
        public static Keyword LoadToplevel
        {
            [DebuggerStepThrough]
            get
            {
                return loadToplevel;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword local = (Keyword) Package.Keyword.Intern ("LOCAL");
        public static Keyword Local
        {
            [DebuggerStepThrough]
            get
            {
                return local;
            }
        }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        static readonly Keyword method = (Keyword) Package.Keyword.Intern ("METHOD");
        public static Keyword Method
        {
            [DebuggerStepThrough]
            get
            {
                return method;
            }
        }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        static readonly Keyword methodClass = (Keyword) Package.Keyword.Intern ("METHOD-CLASS");
        public static Keyword MethodClass
        {
            [DebuggerStepThrough]
            get
            {
                return methodClass;
            }
        }


        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword name = (Keyword) Package.Keyword.Intern ("NAME");
        public static Keyword Name
        {
            [DebuggerStepThrough]
            get
            {
                return name;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword newest = (Keyword) Package.Keyword.Intern ("NEWEST");
        public static Keyword Newest
        {
            [DebuggerStepThrough]
            get
            {
                return newest;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword newVersion = (Keyword) Package.Keyword.Intern ("NEW-VERSION");
        public static Keyword NewVersion
        {
            [DebuggerStepThrough]
            get
            {
                return newVersion;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword output = (Keyword) Package.Keyword.Intern ("OUTPUT");
        public static Keyword Output
        {
            [DebuggerStepThrough]
            get
            {
                return output;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword overwrite = (Keyword) Package.Keyword.Intern ("OVERWRITE");
        public static Keyword Overwrite
        {
            [DebuggerStepThrough]
            get
            {
                return overwrite;
            }
        }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        static readonly Keyword precedenceList = (Keyword) Package.Keyword.Intern ("PRECEDENCE-LIST");
        public static Keyword PrecedenceList
        {
            [DebuggerStepThrough]
            get
            {
                return precedenceList;
            }
        }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        static readonly Keyword primary = (Keyword) Package.Keyword.Intern ("PRIMARY");
        public static Keyword Primary
        {
            [DebuggerStepThrough]
            get
            {
                return primary;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword print = (Keyword) Package.Keyword.Intern ("PRINT");
        public static Keyword Print
        {
            [DebuggerStepThrough]
            get
            {
                return print;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword probe = (Keyword) Package.Keyword.Intern ("PROBE");
        public static Keyword Probe
        {
            [DebuggerStepThrough]
            get
            {
                return probe;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword procedure = (Keyword) Package.Keyword.Intern ("PROCEDURE");
        public static Keyword Procedure
        {
            [DebuggerStepThrough]
            get
            {
                return procedure;
            }
        }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        static readonly Keyword prototype = (Keyword) Package.Keyword.Intern ("PROTOTYPE");
        public static Keyword Prototype
        {
            [DebuggerStepThrough]
            get
            {
                return prototype;
            }
        }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        static readonly Keyword qualifier = (Keyword) Package.Keyword.Intern ("QUALIFIER");
        public static Keyword Qualifier
        {
            [DebuggerStepThrough]
            get
            {
                return qualifier;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword rename = (Keyword) Package.Keyword.Intern ("RENAME");
        public static Keyword Rename
        {
            [DebuggerStepThrough]
            get
            {
                return rename;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword renameAndDelete = (Keyword) Package.Keyword.Intern ("RENAME-AND-DELETE");
        public static Keyword RenameAndDelete
        {
            [DebuggerStepThrough]
            get
            {
                return renameAndDelete;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword relative = (Keyword) Package.Keyword.Intern ("RELATIVE");
        public static Keyword Relative
        {
            [DebuggerStepThrough]
            get
            {
                return relative;
            }
        }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        static readonly Keyword slots = (Keyword) Package.Keyword.Intern ("SLOTS");
        public static Keyword Slots
        {
            [DebuggerStepThrough]
            get
            {
                return slots;
            }
        }


        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        static readonly Keyword specializers = (Keyword) Package.Keyword.Intern ("SPECIALIZERS");
        public static Keyword Specializers
        {
            [DebuggerStepThrough]
            get
            {
                return specializers;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword start = (Keyword) Package.Keyword.Intern ("START");
        public static Keyword Start
        {
            [DebuggerStepThrough]
            get
            {
                return start;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword start1 = (Keyword) Package.Keyword.Intern ("START1");
        public static Keyword Start1
        {
            [DebuggerStepThrough]
            get
            {
                return start1;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword start2 = (Keyword) Package.Keyword.Intern ("START2");
        public static Keyword Start2
        {
            [DebuggerStepThrough]
            get
            {
                return start2;
            }
        }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        static readonly Keyword studlyName = (Keyword) Package.Keyword.Intern ("STUDLY-NAME");
        public static Keyword StudlyName
        {
            [DebuggerStepThrough]
            get
            {
                return studlyName;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword supersede = (Keyword) Package.Keyword.Intern ("SUPESEDE");
        public static Keyword Supersede
        {
            [DebuggerStepThrough]
            get
            {
                return supersede;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword test = (Keyword) Package.Keyword.Intern ("TEST");
        public static Keyword Test
        {
            [DebuggerStepThrough]
            get
            {
                return test;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword testNot = (Keyword) Package.Keyword.Intern ("TEST-NOT");
        public static Keyword TestNot
        {
            [DebuggerStepThrough]
            get
            {
                return testNot;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword type = (Keyword) Package.Keyword.Intern ("TYPE");
        public static Keyword Type
        {
            [DebuggerStepThrough]
            get
            {
                return type;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword unspecific = (Keyword) Package.Keyword.Intern ("UNSPECIFIC");
        public static Keyword Unspecific
        {
            [DebuggerStepThrough]
            get
            {
                return unspecific;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword verbose = (Keyword) Package.Keyword.Intern ("VERBOSE");
        public static Keyword Verbose
        {
            [DebuggerStepThrough]
            get
            {
                return verbose;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword version = (Keyword) Package.Keyword.Intern ("VERSION");
        public static Keyword Version
        {
            [DebuggerStepThrough]
            get
            {
                return version;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword wild = (Keyword) Package.Keyword.Intern ("WILD");
        public static Keyword Wild
        {
            [DebuggerStepThrough]
            get
            {
                return wild;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static readonly Keyword wildInferiors = (Keyword) Package.Keyword.Intern ("WILD-INFERIORS");
        public static Keyword WildInferiors
        {
            [DebuggerStepThrough]
            get
            {
                return wildInferiors;
            }
        }
    }
}

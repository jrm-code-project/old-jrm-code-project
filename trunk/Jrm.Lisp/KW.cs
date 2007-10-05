using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Lisp
{
    public sealed class KW
    {
        private KW ()
        {
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword absolute = (Keyword) Package.Keyword.Intern ("ABSOLUTE");
        public static Keyword Absolute
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return absolute;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword allowOtherKeys = (Keyword) Package.Keyword.Intern ("ALLOW-OTHER-KEYS");
        public static Keyword AllowOtherKeys
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return allowOtherKeys;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword append = (Keyword) Package.Keyword.Intern ("APPEND");
        public static Keyword Append
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return append;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword back = (Keyword) Package.Keyword.Intern ("BACK");
        public static Keyword Back
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return back;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword kase = (Keyword) Package.Keyword.Intern ("CASE");
        public static Keyword Case
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return kase;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword common = (Keyword) Package.Keyword.Intern ("COMMON");
        public static Keyword Common
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return common;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword compileToplevel = (Keyword) Package.Keyword.Intern ("COMPILE-TOPLEVEL");
        public static Keyword CompileToplevel
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return compileToplevel;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword create = (Keyword) Package.Keyword.Intern ("CREATE");
        public static Keyword Create
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return create;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword dfault = (Keyword) Package.Keyword.Intern ("DEFAULT");
        public static Keyword Default
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return dfault;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword defaults = (Keyword) Package.Keyword.Intern ("DEFAULTS");
        public static Keyword Defaults
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return defaults;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword device = (Keyword) Package.Keyword.Intern ("DEVICE");
        public static Keyword Device
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return device;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword directDefaultInitargs = (Keyword) Package.Keyword.Intern ("DIRECT-DEFAULT-INITARGS");
        public static Keyword DirectDefaultInitargs
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return directDefaultInitargs;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword directSlots = (Keyword) Package.Keyword.Intern ("DIRECT-SLOTS");
        public static Keyword DirectSlots
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return directSlots;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword directSuperclasses = (Keyword) Package.Keyword.Intern ("DIRECT-SUPERCLASSES");
        public static Keyword DirectSuperclasses
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return directSuperclasses;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword directory = (Keyword) Package.Keyword.Intern ("DIRECTORY");
        public static Keyword Directory
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return directory;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword direction = (Keyword) Package.Keyword.Intern ("DIRECTION");
        public static Keyword Direction
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return direction;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword elementType = (Keyword) Package.Keyword.Intern ("ELEMENT-TYPE");
        public static Keyword ElementType
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return elementType;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword end = (Keyword) Package.Keyword.Intern ("END");
        public static Keyword End
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return end;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword end1 = (Keyword) Package.Keyword.Intern ("END1");
        public static Keyword End1
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return end1;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword end2 = (Keyword) Package.Keyword.Intern ("END2");
        public static Keyword End2
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return end2;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword environment = (Keyword) Package.Keyword.Intern ("ENVIRONMENT");
        public static Keyword Environment
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return environment;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword error = (Keyword) Package.Keyword.Intern ("ERROR");
        public static Keyword Error
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return error;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword externalFormat = (Keyword) Package.Keyword.Intern ("EXTERNAL-FORMAT");
        public static Keyword ExternalFormat
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return externalFormat;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword execute = (Keyword) Package.Keyword.Intern ("EXECUTE");
        public static Keyword Execute
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return execute;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword fromEnd = (Keyword) Package.Keyword.Intern ("FROM-END");
        public static Keyword FromEnd
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return fromEnd;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword host = (Keyword) Package.Keyword.Intern ("HOST");
        public static Keyword Host
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return host;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword ifDoesNotExist = (Keyword) Package.Keyword.Intern ("IF-DOES-NOT-EXIST");
        public static Keyword IfDoesNotExist
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return ifDoesNotExist;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword ifExists = (Keyword) Package.Keyword.Intern ("IF-EXISTS");
        public static Keyword IfExists
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return ifExists;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword input = (Keyword) Package.Keyword.Intern ("INPUT");
        public static Keyword Input
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return input;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword initializer = (Keyword) Package.Keyword.Intern ("INITIALIZER");
        public static Keyword Initializer
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return initializer;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword io = (Keyword) Package.Keyword.Intern ("IO");
        public static Keyword IO
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return io;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword junkAllowed = (Keyword) Package.Keyword.Intern ("JUNK-ALLOWED");
        public static Keyword JunkAllowed
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return junkAllowed;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword key = (Keyword) Package.Keyword.Intern ("KEY");
        public static Keyword Key
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return key;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword lambdaList = (Keyword) Package.Keyword.Intern ("LAMBDA-LIST");
        public static Keyword LambdaList
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return lambdaList;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword loadToplevel = (Keyword) Package.Keyword.Intern ("LOAD-TOPLEVEL");
        public static Keyword LoadToplevel
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return loadToplevel;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword local = (Keyword) Package.Keyword.Intern ("LOCAL");
        public static Keyword Local
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return local;
            }
        }

        [System.Diagnostics.DebuggerBrowsable (System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword methodClass = (Keyword) Package.Keyword.Intern ("METHOD-CLASS");
        public static Keyword MethodClass
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return methodClass;
            }
        }


        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword name = (Keyword) Package.Keyword.Intern ("NAME");
        public static Keyword Name
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return name;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword newest = (Keyword) Package.Keyword.Intern ("NEWEST");
        public static Keyword Newest
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return newest;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword newVersion = (Keyword) Package.Keyword.Intern ("NEW-VERSION");
        public static Keyword NewVersion
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return newVersion;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword output = (Keyword) Package.Keyword.Intern ("OUTPUT");
        public static Keyword Output
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return output;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword overwrite = (Keyword) Package.Keyword.Intern ("OVERWRITE");
        public static Keyword Overwrite
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return overwrite;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword print = (Keyword) Package.Keyword.Intern ("PRINT");
        public static Keyword Print
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return print;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword probe = (Keyword) Package.Keyword.Intern ("PROBE");
        public static Keyword Probe
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return probe;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword rename = (Keyword) Package.Keyword.Intern ("RENAME");
        public static Keyword Rename
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return rename;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword renameAndDelete = (Keyword) Package.Keyword.Intern ("RENAME-AND-DELETE");
        public static Keyword RenameAndDelete
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return renameAndDelete;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword relative = (Keyword) Package.Keyword.Intern ("RELATIVE");
        public static Keyword Relative
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return relative;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword start = (Keyword) Package.Keyword.Intern ("START");
        public static Keyword Start
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return start;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword start1 = (Keyword) Package.Keyword.Intern ("START1");
        public static Keyword Start1
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return start1;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword start2 = (Keyword) Package.Keyword.Intern ("START2");
        public static Keyword Start2
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return start2;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword supersede = (Keyword) Package.Keyword.Intern ("SUPESEDE");
        public static Keyword Supersede
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return supersede;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword test = (Keyword) Package.Keyword.Intern ("TEST");
        public static Keyword Test
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return test;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword testNot = (Keyword) Package.Keyword.Intern ("TEST-NOT");
        public static Keyword TestNot
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return testNot;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword type = (Keyword) Package.Keyword.Intern ("TYPE");
        public static Keyword Type
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return type;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword unspecific = (Keyword) Package.Keyword.Intern ("UNSPECIFIC");
        public static Keyword Unspecific
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return unspecific;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword verbose = (Keyword) Package.Keyword.Intern ("VERBOSE");
        public static Keyword Verbose
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return verbose;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword version = (Keyword) Package.Keyword.Intern ("VERSION");
        public static Keyword Version
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return version;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword wild = (Keyword) Package.Keyword.Intern ("WILD");
        public static Keyword Wild
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return wild;
            }
        }

        [System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)]
        static readonly Keyword wildInferiors = (Keyword) Package.Keyword.Intern ("WILD-INFERIORS");
        public static Keyword WildInferiors
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return wildInferiors;
            }
        }
    }
}

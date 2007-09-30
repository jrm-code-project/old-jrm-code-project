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

        static readonly Keyword absolute = (Keyword) Package.Keyword.Intern ("ABSOLUTE");
        public static Keyword Absolute
        {
            get
            {
                return absolute;
            }
        }

        static readonly Keyword allowOtherKeys = (Keyword) Package.Keyword.Intern ("ALLOW-OTHER-KEYS");
        public static Keyword AllowOtherKeys
        {
            get
            {
                return allowOtherKeys;
            }
        }

        static readonly Keyword append = (Keyword) Package.Keyword.Intern ("APPEND");
        public static Keyword Append
        {
            get
            {
                return append;
            }
        }

        static readonly Keyword back = (Keyword) Package.Keyword.Intern ("BACK");
        public static Keyword Back
        {
            get
            {
                return back;
            }
        }

        static readonly Keyword kase = (Keyword) Package.Keyword.Intern ("CASE");
        public static Keyword Case
        {
            get
            {
                return kase;
            }
        }

        static readonly Keyword common = (Keyword) Package.Keyword.Intern ("COMMON");
        public static Keyword Common
        {
            get
            {
                return common;
            }
        }

        static readonly Keyword compileToplevel = (Keyword) Package.Keyword.Intern ("COMPILE-TOPLEVEL");
        public static Keyword CompileToplevel
        {
            get
            {
                return compileToplevel;
            }
        }

        static readonly Keyword create = (Keyword) Package.Keyword.Intern ("CREATE");
        public static Keyword Create
        {
            get
            {
                return create;
            }
        }

        static readonly Keyword dfault = (Keyword) Package.Keyword.Intern ("DEFAULT");
        public static Keyword Default
        {
            get
            {
                return dfault;
            }
        }

        static readonly Keyword defaults = (Keyword) Package.Keyword.Intern ("DEFAULTS");
        public static Keyword Defaults
        {
            get
            {
                return defaults;
            }
        }

        static readonly Keyword device = (Keyword) Package.Keyword.Intern ("DEVICE");
        public static Keyword Device
        {
            get
            {
                return device;
            }
        }

        static readonly Keyword directDefaultInitargs = (Keyword) Package.Keyword.Intern ("DIRECT-DEFAULT-INITARGS");
        public static Keyword DirectDefaultInitargs
        {
            get
            {
                return directDefaultInitargs;
            }
        }

        static readonly Keyword directSlots = (Keyword) Package.Keyword.Intern ("DIRECT-SLOTS");
        public static Keyword DirectSlots
        {
            get
            {
                return directSlots;
            }
        }

        static readonly Keyword directSuperclasses = (Keyword) Package.Keyword.Intern ("DIRECT-SUPERCLASSES");
        public static Keyword DirectSuperclasses
        {
            get
            {
                return directSuperclasses;
            }
        }

        static readonly Keyword directory = (Keyword) Package.Keyword.Intern ("DIRECTORY");
        public static Keyword Directory
        {
            get
            {
                return directory;
            }
        }

        static readonly Keyword direction = (Keyword) Package.Keyword.Intern ("DIRECTION");
        public static Keyword Direction
        {
            get
            {
                return direction;
            }
        }

        static readonly Keyword elementType = (Keyword) Package.Keyword.Intern ("ELEMENT-TYPE");
        public static Keyword ElementType
        {
            get
            {
                return elementType;
            }
        }

        static readonly Keyword end = (Keyword) Package.Keyword.Intern ("END");
        public static Keyword End
        {
            get
            {
                return end;
            }
        }

        static readonly Keyword end1 = (Keyword) Package.Keyword.Intern ("END1");
        public static Keyword End1
        {
            get
            {
                return end1;
            }
        }

        static readonly Keyword end2 = (Keyword) Package.Keyword.Intern ("END2");
        public static Keyword End2
        {
            get
            {
                return end2;
            }
        }

        static readonly Keyword environment = (Keyword) Package.Keyword.Intern ("ENVIRONMENT");
        public static Keyword Environment
        {
            get
            {
                return environment;
            }
        }

        static readonly Keyword error = (Keyword) Package.Keyword.Intern ("ERROR");
        public static Keyword Error
        {
            get
            {
                return error;
            }
        }

        static readonly Keyword externalFormat = (Keyword) Package.Keyword.Intern ("EXTERNAL-FORMAT");
        public static Keyword ExternalFormat
        {
            get
            {
                return externalFormat;
            }
        }

        static readonly Keyword execute = (Keyword) Package.Keyword.Intern ("EXECUTE");
        public static Keyword Execute
        {
            get
            {
                return execute;
            }
        }

        static readonly Keyword fromEnd = (Keyword) Package.Keyword.Intern ("FROM-END");
        public static Keyword FromEnd
        {
            get
            {
                return fromEnd;
            }
        }

        static readonly Keyword host = (Keyword) Package.Keyword.Intern ("HOST");
        public static Keyword Host
        {
            get
            {
                return host;
            }
        }

        static readonly Keyword ifDoesNotExist = (Keyword) Package.Keyword.Intern ("IF-DOES-NOT-EXIST");
        public static Keyword IfDoesNotExist
        {
            get
            {
                return ifDoesNotExist;
            }
        }

        static readonly Keyword ifExists = (Keyword) Package.Keyword.Intern ("IF-EXISTS");
        public static Keyword IfExists
        {
            get
            {
                return ifExists;
            }
        }

        static readonly Keyword input = (Keyword) Package.Keyword.Intern ("INPUT");
        public static Keyword Input
        {
            get
            {
                return input;
            }
        }

        static readonly Keyword initializer = (Keyword) Package.Keyword.Intern ("INITIALIZER");
        public static Keyword Initializer
        {
            get
            {
                return initializer;
            }
        }

        static readonly Keyword io = (Keyword) Package.Keyword.Intern ("IO");
        public static Keyword IO
        {
            get
            {
                return io;
            }
        }

        static readonly Keyword junkAllowed = (Keyword) Package.Keyword.Intern ("JUNK-ALLOWED");
        public static Keyword JunkAllowed
        {
            get
            {
                return junkAllowed;
            }
        }

        static readonly Keyword key = (Keyword) Package.Keyword.Intern ("KEY");
        public static Keyword Key
        {
            get
            {
                return key;
            }
        }

        static readonly Keyword loadToplevel = (Keyword) Package.Keyword.Intern ("LOAD-TOPLEVEL");
        public static Keyword LoadToplevel
        {
            get
            {
                return loadToplevel;
            }
        }

        static readonly Keyword local = (Keyword) Package.Keyword.Intern ("LOCAL");
        public static Keyword Local
        {
            get
            {
                return local;
            }
        }

        static readonly Keyword name = (Keyword) Package.Keyword.Intern ("NAME");
        public static Keyword Name
        {
            get
            {
                return name;
            }
        }

        static readonly Keyword newest = (Keyword) Package.Keyword.Intern ("NEWEST");
        public static Keyword Newest
        {
            get
            {
                return newest;
            }
        }

        static readonly Keyword newVersion = (Keyword) Package.Keyword.Intern ("NEW-VERSION");
        public static Keyword NewVersion
        {
            get
            {
                return newVersion;
            }
        }

        static readonly Keyword output = (Keyword) Package.Keyword.Intern ("OUTPUT");
        public static Keyword Output
        {
            get
            {
                return output;
            }
        }

        static readonly Keyword overwrite = (Keyword) Package.Keyword.Intern ("OVERWRITE");
        public static Keyword Overwrite
        {
            get
            {
                return overwrite;
            }
        }

        static readonly Keyword print = (Keyword) Package.Keyword.Intern ("PRINT");
        public static Keyword Print
        {
            get
            {
                return print;
            }
        }

        static readonly Keyword probe = (Keyword) Package.Keyword.Intern ("PROBE");
        public static Keyword Probe
        {
            get
            {
                return probe;
            }
        }

        static readonly Keyword rename = (Keyword) Package.Keyword.Intern ("RENAME");
        public static Keyword Rename
        {
            get
            {
                return rename;
            }
        }

        static readonly Keyword renameAndDelete = (Keyword) Package.Keyword.Intern ("RENAME-AND-DELETE");
        public static Keyword RenameAndDelete
        {
            get
            {
                return renameAndDelete;
            }
        }

        static readonly Keyword relative = (Keyword) Package.Keyword.Intern ("RELATIVE");
        public static Keyword Relative
        {
            get
            {
                return relative;
            }
        }

        static readonly Keyword start = (Keyword) Package.Keyword.Intern ("START");
        public static Keyword Start
        {
            get
            {
                return start;
            }
        }

        static readonly Keyword start1 = (Keyword) Package.Keyword.Intern ("START1");
        public static Keyword Start1
        {
            get
            {
                return start1;
            }
        }

        static readonly Keyword start2 = (Keyword) Package.Keyword.Intern ("START2");
        public static Keyword Start2
        {
            get
            {
                return start2;
            }
        }

        static readonly Keyword supersede = (Keyword) Package.Keyword.Intern ("SUPESEDE");
        public static Keyword Supersede
        {
            get
            {
                return supersede;
            }
        }

        static readonly Keyword test = (Keyword) Package.Keyword.Intern ("TEST");
        public static Keyword Test
        {
            get
            {
                return test;
            }
        }

        static readonly Keyword testNot = (Keyword) Package.Keyword.Intern ("TEST-NOT");
        public static Keyword TestNot
        {
            get
            {
                return testNot;
            }
        }

        static readonly Keyword type = (Keyword) Package.Keyword.Intern ("TYPE");
        public static Keyword Type
        {
            get
            {
                return type;
            }
        }

        static readonly Keyword unspecific = (Keyword) Package.Keyword.Intern ("UNSPECIFIC");
        public static Keyword Unspecific
        {
            get
            {
                return unspecific;
            }
        }

        static readonly Keyword verbose = (Keyword) Package.Keyword.Intern ("VERBOSE");
        public static Keyword Verbose
        {
            get
            {
                return verbose;
            }
        }

        static readonly Keyword version = (Keyword) Package.Keyword.Intern ("VERSION");
        public static Keyword Version
        {
            get
            {
                return version;
            }
        }

        static readonly Keyword wild = (Keyword) Package.Keyword.Intern ("WILD");
        public static Keyword Wild
        {
            get
            {
                return wild;
            }
        }

        static readonly Keyword wildInferiors = (Keyword) Package.Keyword.Intern ("WILD-INFERIORS");
        public static Keyword WildInferiors
        {
            get
            {
                return wildInferiors;
            }
        }
    }
}

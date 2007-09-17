using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Lisp
{
    public class Package
    {
        private static List<Package> AllPackages = new List<Package> ();
        private static Package CommonLispUserPackage = new Package("COMMON-LISP-USER",
            new string [] {"CL-USER"});

        private string name;
        private List<string> nicknames;

        public Package (string name, string [] nicknames)
        {
            this.name = String.Intern(name);
            this.nicknames = new List<string> ();
            if (nicknames != null)
                foreach (string nickname in nicknames)
                    this.nicknames.Add (nickname);
            AllPackages.Add (this);
        }

        public override string ToString ()
        {
            return "#<PACKAGE \"" + this.name + "\">";
        }

        static public Package CommonLispUser
        {
            get
            {
                return CommonLispUserPackage;
            }
        }

    }
}

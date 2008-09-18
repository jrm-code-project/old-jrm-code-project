using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    [Serializable]
    public class WorldState
    {
        public readonly ControlPoint  cp;
        public readonly GlobalEnvironment ge;


        public  Constant defaultObject;
        public  Constant eofObject;
        public  Constant aux;
        public  Constant key;
        public  Constant optional;
        public  Constant rest;
        public  Constant externalUnassigned;
        public  Constant unspecific;
        public WorldState (ControlPoint cp, GlobalEnvironment ge)
        {
            this.cp = cp;
            this.ge = ge;
            this.defaultObject = Constant.DefaultObject;
            this.eofObject = Constant.EofObject;
            this.aux = Constant.theAuxMarker;
            this.key = Constant.theKeyMarker;
            this.optional = Constant.theOptionalMarker;
            this.rest = Constant.theRestMarker;
            this.externalUnassigned = Constant.ExternalUnassigned;
            this.unspecific = Constant.Unspecific;
        }

    }
}

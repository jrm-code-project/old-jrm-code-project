using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    /// <summary>
    /// Holds a dumped image.
    /// Must be public for listener.
    /// </summary>
    [Serializable]
    public class WorldState
    {
        internal readonly ControlPoint  cp;
        internal readonly GlobalEnvironment ge;

        internal Constant defaultObject;
        internal Constant eofObject;
        internal Constant aux;
        internal Constant key;
        internal Constant optional;
        internal Constant rest;
        internal Constant externalUnassigned;
        internal Constant unspecific;
        internal WorldState (ControlPoint cp, GlobalEnvironment ge)
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

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    class UninternedSymbol: SchemeObject, ISystemPair
    {
        string printName;

        public UninternedSymbol (string printName)
            : base (TC.UNINTERNED_SYMBOL)
        {
            this.printName = printName;
        }


        #region ISystemPair Members

        public object SystemPairCar
        {
            get
            {
                return this.printName;
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        public object SystemPairCdr
        {
            get
            {
                throw new NotImplementedException ();
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        #endregion
    }
}

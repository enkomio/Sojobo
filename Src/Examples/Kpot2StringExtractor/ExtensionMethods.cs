using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static ES.Sojobo.Model;

namespace ES.Kpot2StringExtractor.ExtensionMethods
{
    public static class BitVectorHelperExtensions
    {
        public static Int32 ToInt32(this EmulatedValue value)
        {
            return B2R2.BitVector.ToInt32(value.Value);
        }
        
        public static UInt64 ToUInt64(this EmulatedValue value)
        {
            return B2R2.BitVector.ToUInt64(value.Value);
        }
    }
}

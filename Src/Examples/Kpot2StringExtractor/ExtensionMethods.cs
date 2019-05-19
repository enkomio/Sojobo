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

        public static EmulatedValue New(this EmulatedValue value, Int32 newValue)
        {
            return new EmulatedValue(value.Name, value.IsTemp, B2R2.BitVector.OfInt32(newValue, 32), value.Type);
        }

        public static UInt64 ToUInt64(this EmulatedValue value)
        {
            return B2R2.BitVector.ToUInt64(value.Value);
        }
    }
}

using System;
using static ES.Sojobo.Model;

namespace ES.Sojobo
{
    public static class EmulatedValueExtensions
    {
        public static Int32 ToInt32(this EmulatedValue value)
        {
            return B2R2.BitVector.ToInt32(value.Value);
        }

        public static UInt32 ToUInt32(this EmulatedValue value)
        {
            return B2R2.BitVector.ToUInt32(value.Value);
        }

        public static Int64 ToInt64(this EmulatedValue value)
        {
            return B2R2.BitVector.ToInt64(value.Value);
        }

        public static UInt64 ToUInt64(this EmulatedValue value)
        {
            return B2R2.BitVector.ToUInt64(value.Value);
        }
    }
}

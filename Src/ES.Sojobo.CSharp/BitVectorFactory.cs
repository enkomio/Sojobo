using B2R2;
using System;

namespace ES.Sojobo.CSharp
{
    public sealed class BitVectorFactory
    {
        public static BitVector Create(UInt32 value)
        {
            return BitVector.OfUInt32(value, 32);
        }

        public static BitVector Create(Int32 value)
        {
            return BitVector.OfInt32(value, 32);
        }

        public static BitVector Create(UInt64 value)
        {
            return BitVector.OfUInt64(value, 64);
        }

        public static BitVector Create(System.Int64 value)
        {
            return BitVector.OfInt64(value, 64);
        }
    }
}

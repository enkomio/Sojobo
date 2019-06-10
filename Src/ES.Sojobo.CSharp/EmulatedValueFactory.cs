using System;
using static ES.Sojobo.Model;

namespace ES.Sojobo.CSharp
{
    public sealed class EmulatedValueFactory
    {
        public static EmulatedValue Create(String name, UInt32 value, Boolean isTemporary = false)
        {
            return new EmulatedValue(name, isTemporary, B2R2.BitVector.OfUInt32(value, 32), EmulatedType.DoubleWord);
        }

        public static EmulatedValue Create(String name, Int32 value, Boolean isTemporary = false)
        {
            return new EmulatedValue(name, isTemporary, B2R2.BitVector.OfInt32(value, 32), EmulatedType.DoubleWord);
        }

        public static EmulatedValue Create(String name, UInt64 value, Boolean isTemporary = false)
        {
            return new EmulatedValue(name, isTemporary, B2R2.BitVector.OfUInt64(value, 64), EmulatedType.QuadWord);
        }

        public static EmulatedValue Create(String name, Int64 value, Boolean isTemporary = false)
        {
            return new EmulatedValue(name, isTemporary, B2R2.BitVector.OfInt64(value, 32), EmulatedType.QuadWord);
        }
    }
}

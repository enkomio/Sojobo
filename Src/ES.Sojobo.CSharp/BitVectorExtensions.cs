using Microsoft.FSharp.Core;

namespace ES.Sojobo
{
    public static class BitVectorExtensions
    {
        public static FSharpOption<B2R2.BitVector> ToOption(this B2R2.BitVector value)
        {
            return new FSharpOption<B2R2.BitVector>(value);
        }
    }
}

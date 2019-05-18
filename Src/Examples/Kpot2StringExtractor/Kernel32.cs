using ES.Sojobo;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static ES.Sojobo.Model;

namespace Kpot2StringExtractor
{
    // the name of the class must be equals to the one of the original library
    public static class Kernel32
    {
        public static CallbackResult GetLastError(ISandbox sandbox)
        {
            var pointerSize = 32;
            var functionReturnValue = B2R2.BitVector.OfInt32(0x57, pointerSize);
            var returnValue = new Microsoft.FSharp.Core.FSharpOption<B2R2.BitVector>(functionReturnValue);
            return new CallbackResult(returnValue, CallingConvention.Cdecl);
        }
    }
}

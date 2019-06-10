using ES.Sojobo;
using ES.Sojobo.CSharp;
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
            var functionReturnValue = BitVectorFactory.Create(0x57);
            return new CallbackResult(functionReturnValue.ToOption(), CallingConvention.Cdecl);
        }
    }
}

using ES.Sojobo;
using ES.Sojobo.CSharp;
using static ES.Sojobo.Model;

namespace KpotEmulator
{
    public static class Kernel32
    {
        public static CallbackResult GetLastError(ISandbox sandbox)
        {
            var functionReturnValue = BitVectorFactory.Create(0x57);
            return new CallbackResult(functionReturnValue.ToOption(), CallingConvention.Cdecl);
        }
    }
}

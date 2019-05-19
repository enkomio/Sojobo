using ES.Sojobo;
using System;
using System.Text;

namespace ES.Kpot2StringExtractor
{
    public class EncryptedString
    {
        public UInt16 EncryptionKey;
        public UInt16 StringLength;
        public UInt32 Buffer;

        public String Decrypt(IProcessContainer process)
        {
            var buffer = process.Memory.ReadMemory(this.Buffer, this.StringLength);
            var stringContent = new StringBuilder();
            foreach(var b in buffer)
            {
                stringContent.Append((Char)(b ^ this.EncryptionKey));
            }

            return stringContent.ToString();
        }
    }
}

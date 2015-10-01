using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;

namespace RACLCompiler
{
    class Program
    {
        static void Main(string[] args)
        {
            if (args.Length != 1)
                Console.WriteLine("usage: RACLCompiler programfile");
            else
            {
                using (FileStream fs = new FileStream(args[0], FileMode.Open))
                {
                    string data = new StreamReader(fs).ReadToEnd();
                    var compiler = new Compiler.Compiler();
                    var compiledFunctions = compiler.Compile(data);
                    foreach (var cfunc in compiledFunctions)
                    {
                        var fileName = cfunc.Name + ".dnl";
                        if (File.Exists(fileName))
                            File.Delete(fileName);
                        using(var fs2 = new FileStream(fileName,FileMode.Create))
                        {
                            var writter = new StreamWriter(fs2);
                            writter.Write(cfunc.Body);
                            writter.Flush();
                        }
                    }
                }     
            }
        }
    }
}

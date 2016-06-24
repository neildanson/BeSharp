using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Xunit;

namespace BeSharp.CompilerTests
{
    public class TestCompiledOutput
    {
        [Fact]
        public void TestEquality()
        {
            var item1 = new Hello(100, 200.0);
            var item2 = new Hello(100, 200.0);
            var equal = item1.Equals(item2);
            Assert.True(equal);
        }
    }
}

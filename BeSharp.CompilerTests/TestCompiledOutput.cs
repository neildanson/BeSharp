using Xunit;

namespace BeSharp.CompilerTests
{
    public struct Hello2
    {
        public readonly int hello;

        public Hello2(int num)
        {
            this.hello = num;
        }

        public override bool Equals(object obj)
        {
            Hello2 other = (Hello2)obj;
            return this.hello.Equals(other.hello);
        }
    }

}
//public class TestCompiledOutput
//{
//    [Fact]
//    public void TestEquality()
//    {
//        var item1 = new Hello(100);
//        var item2 = new Hello(100);
//        var equal = item1.Equals(item2);
//        Assert.True(equal);
//    }
//}


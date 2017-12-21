using System;
using System.Collections.Generic;
using adventofcode2017.days.day16;

namespace adventofcode2017
{
    class Program
    {
        static void Main(string[] args)
        {
            var d16 = new Day16();

            Console.WriteLine($"Program ordering after dancing: {d16.GetProgramOrderAfterNumDances(1)}");
            Console.WriteLine($"Program ordering after dancing: {d16.GetProgramOrderAfterNumDances(1000000000)}");
        }
    }
}

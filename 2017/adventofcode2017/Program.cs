using System;
using System.Collections.Generic;
using adventofcode2017.days.day18;

namespace adventofcode2017
{
    class Program
    {
        static void Main(string[] args)
        {
            var d18 = new Day18();

            Console.WriteLine($"First sound recovered: {d18.GetValueOfFirstRecovered()}");
            Console.WriteLine($"Num program 1 sends: {d18.GetNumProgramSends()}");
        }
    }
}

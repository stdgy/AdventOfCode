using System;
using System.Collections.Generic;
using adventofcode2017.days.day14;

namespace adventofcode2017
{
    class Program
    {
        static void Main(string[] args)
        {
            var d14 = new Day14();

            Console.WriteLine($"Number of squares used: {d14.GetNumUsedSquares()}");
            Console.WriteLine($"Number of distinct groups: {d14.GetNumberOfGroups()}");
        }
    }
}

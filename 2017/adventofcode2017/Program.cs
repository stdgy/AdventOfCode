using System;
using System.Collections.Generic;
using adventofcode2017.days.day19;

namespace adventofcode2017
{
    class Program
    {
        static void Main(string[] args)
        {
            var d19 = new Day19();

            Console.WriteLine($"Letters on path: {d19.GetLettersOnPath()}");
            Console.WriteLine($"Path length: {d19.GetTotalPathLength()}");
        }
    }
}

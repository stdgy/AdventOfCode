using System;
using System.Collections.Generic;
using adventofcode2017.days.day15;

namespace adventofcode2017
{
    class Program
    {
        static void Main(string[] args)
        {
            var d15 = new Day15();
            Console.WriteLine($"Number of matches: {d15.GetNumMatches(40000000)}");
            Console.WriteLine($"Number of matches: {d15.GetNumMatchesModifiedGenerators(5000000)}");
        }
    }
}

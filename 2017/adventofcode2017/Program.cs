using System;
using System.Collections.Generic;
using adventofcode2017.days.day11;

namespace adventofcode2017
{
    class Program
    {
        static void Main(string[] args)
        {
            var day11 = new Day11();
            Console.WriteLine($"Steps away: {day11.GetFewestStepsToChild()}");
            Console.WriteLine($"Max Distance: {day11.GetMaxLocation()}");
        }
    }
}

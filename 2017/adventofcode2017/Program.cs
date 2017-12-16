using System;
using System.Collections.Generic;
using adventofcode2017.days.day10;

namespace adventofcode2017
{
    class Program
    {
        static void Main(string[] args)
        {
            var day10 = new Day10(256);
            Console.WriteLine($"Multiple: {day10.GetMultipleOfFirstTwoNumbers()}");

            day10 = new Day10(256, parseAllBytes:true);
            Console.WriteLine($"Knot Hash: {day10.GetKnotHash()}");
        }
    }
}

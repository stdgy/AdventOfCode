using System;
using System.Collections.Generic;
using adventofcode2017.days.day17;

namespace adventofcode2017
{
    class Program
    {
        static void Main(string[] args)
        {
            var d17 = new Day17();

            Console.WriteLine($"Next Value in Buffer: {d17.GetNextValueInCompletedBuffer()}");
            Console.WriteLine($"Final Value: {d17.GetValueForAngrySpinlock()}");
        }
    }
}

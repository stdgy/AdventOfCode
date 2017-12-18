using System;
using System.Collections.Generic;
using adventofcode2017.days.day13;

namespace adventofcode2017
{
    class Program
    {
        static void Main(string[] args)
        {
            var d13 = new Day13();

            Console.WriteLine($"Trip Severity: {d13.GetTripSeverity(0)}");
            Console.WriteLine($"Minimum amount to wait for safe passage: {d13.GetMinDelay()}");
        }
    }
}

using System;

using adventofcode2017.days.day08;

namespace adventofcode2017
{
    class Program
    {
        static void Main(string[] args)
        {
            var d8 = new Day08();

            Console.WriteLine($"Largest Value in Registers: {d8.GetLargestValueInRegisters()}");
            Console.WriteLine($"Largest register value generated: {d8.GetLargestValueGenerated()}");
        }
    }
}

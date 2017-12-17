using System;
using System.Collections.Generic;
using adventofcode2017.days.day12;

namespace adventofcode2017
{
    class Program
    {
        static void Main(string[] args)
        {
            var d12 = new Day12();

            Console.WriteLine($"Nodes reachable from 0: {d12.GetNumberOfConnectionsFromNode(0)}");
            Console.WriteLine($"Distint Groups: {d12.GetTotalNumberOfGroups()}");
        }
    }
}

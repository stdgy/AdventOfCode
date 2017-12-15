using System;
using System.Collections.Generic;
using adventofcode2017.days.day09;

namespace adventofcode2017
{
    class Program
    {
        static void Main(string[] args)
        {
            var tests = new List<string> {
                "{}",
                "{{{}}}",
                "{{},{}}",
                "{{{},{},{{}}}}",
                "{<a>,<a>,<a>,<a>}",
                "{{<ab>},{<ab>},{<ab>},{<ab>}}",
                "{{<!!>},{<!!>},{<!!>},{<!!>}}",
                "{{<a!>},{<a!>},{<a!>},{<ab>}}"
            };

            foreach (var test in tests)
            {
                var d9 = new Day09(test);
                Console.WriteLine($"Score: {d9.GetTotalScore()}");
            }

            var d = new Day09();
            Console.WriteLine($"Score: {d.GetTotalScore()}");
            Console.WriteLine($"Garbage Count: {d.GetGarbageCount()}");
        }
    }
}

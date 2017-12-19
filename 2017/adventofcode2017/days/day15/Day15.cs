using System;
using System.IO;
using System.Linq;

namespace adventofcode2017.days.day15
{
    public class Day15
    {
        private UInt64 _startA;
        private UInt64 _startB;

        public Day15 ()
        {
            var nums = File.ReadAllLines("inputs/day15/input.txt")
                .Select(line => line.Substring(line.LastIndexOf(' ')))
                .Select(word => UInt64.Parse(word));
            
            _startA = nums.First();
            _startB = nums.Last();
        }

        public int GetNumMatches(int rounds)
        {
            UInt64 factorA = 16807;
            UInt64 factorB = 48271;
            UInt64 valA = _startA;
            UInt64 valB = _startB;
            var matches = 0;
            UInt64 divisor = 2147483647;


            for (var i = 0; i < rounds; i++)
            {
                valA = (valA * factorA) % divisor;
                valB = (valB * factorB) % divisor;

                // Cmopare lowest 16 bits for a match
                if ((valA & 0xffff) == (valB & 0xffff))
                {
                    matches++;
                }
            }

            return matches;
        }
        
        public int GetNumMatchesModifiedGenerators(int rounds)
        {
            UInt64 factorA = 16807;
            UInt64 factorB = 48271;
            UInt64 valA = _startA;
            UInt64 valB = _startB;
            var matches = 0;
            UInt64 divisor = 2147483647;

            for (var i = 0; i < rounds; i++)
            {
                valA = (valA * factorA) % divisor;
                // While not divisible by 4
                while ((valA & 0x3) != 0)
                {
                    valA = (valA * factorA) % divisor;
                }

                valB = (valB * factorB) % divisor;
                // While not divisible by 8
                while ((valB & 0x7) != 0)
                {
                    valB = (valB * factorB) % divisor;
                }

                // Compare lowest 16 bits for match
                if ((valA & 0xffff) == (valB & 0xffff))
                {
                    matches++;
                }
            }

            return matches;
        }
    }
}
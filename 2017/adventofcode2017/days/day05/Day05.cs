using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace adventofcode2017.days.day05 
{
    public class Day05 
    {
        private List<int> _jumps;

        public Day05 ()
        {
            _jumps = File.ReadAllText("inputs/day05/input.txt")
                .Split('\n')
                .Select(s => Int32.Parse(s))
                .ToList();
        }

        public int GetJumpsToExit ()
        {
            var jumps = 0;
            var currOffset = 0;

            while (currOffset < _jumps.Count)
            {
                currOffset += _jumps[currOffset]++;
                jumps++;
            }

            return jumps;
        }  

        public int GetStrangeJumpsToExit ()
        {
            var jumps = 0;
            var currOffset = 0;
            var jumpVal = 0;

            while (currOffset < _jumps.Count)
            {
                jumpVal = _jumps[currOffset];

                if (jumpVal > 2)
                {
                    _jumps[currOffset]--;
                }
                else 
                {
                    _jumps[currOffset]++;
                }

                currOffset += jumpVal;
                jumps++;
            }

            return jumps;
        }

        public int Test ()
        {
            _jumps = new List<int> {
                0, 3, 0, 1, -3
            };

            return GetStrangeJumpsToExit();
        }
    }
}
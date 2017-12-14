using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace adventofcode2017.days.day06 
{
    public class Day06 
    {
        HashSet<string> _memoryConfigurations;
        IDictionary<string, int> _memoryConfigsWithCycle;
        List<int> _memoryBanks;

        public Day06 ()
        {
            _memoryConfigurations = new HashSet<string>();
            _memoryConfigsWithCycle = new Dictionary<string, int>();
            _memoryBanks = File.ReadAllText("inputs/day06/input.txt")
                .Split('\t')
                .Select(s => Int32.Parse(s))
                .ToList();
        }

        private void BalanceMemoryBanks (List<int> memoryBanks)
        {
            var maxIndex = 0;
            var memoryToDistribute = 0;

            for (var i = 0; i < memoryBanks.Count; i++)
            {
                if (memoryBanks[i] > memoryBanks[maxIndex])
                    maxIndex = i;
            }

            memoryToDistribute = memoryBanks[maxIndex];
            memoryBanks[maxIndex] = 0;

            while (memoryToDistribute > 0)
            {
                maxIndex = (maxIndex + 1) % memoryBanks.Count;
                memoryBanks[maxIndex]++;
                memoryToDistribute--;
            }
        }

        private string GetMemoryString (IEnumerable<int> memoryBanks)
        {
            return memoryBanks
                .Select(memoryBank => memoryBank.ToString())
                .Aggregate((left, right) => left + "," + right);
        }

        public int CountRedistributionCycles ()
        {
            var cycleCount = 0;
            var memoryString = GetMemoryString(_memoryBanks);

            while (!_memoryConfigurations.Contains(memoryString))
            {
                _memoryConfigurations.Add(memoryString);
                BalanceMemoryBanks(_memoryBanks);
                cycleCount++;
                memoryString = GetMemoryString(_memoryBanks);
            }

            return cycleCount;
        }

        public int CountLoopCycles ()
        {
            var cycleCount = 0;
            var memoryString = GetMemoryString(_memoryBanks);

            while (!_memoryConfigsWithCycle.ContainsKey(memoryString))
            {
                _memoryConfigsWithCycle.Add(memoryString, cycleCount);
                BalanceMemoryBanks(_memoryBanks);
                cycleCount++;
                memoryString = GetMemoryString(_memoryBanks);
            }

            return cycleCount - _memoryConfigsWithCycle[memoryString];
        }
    }
}
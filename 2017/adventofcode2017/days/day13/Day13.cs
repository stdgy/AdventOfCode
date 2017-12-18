using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace adventofcode2017.days.day13 
{
    public class Day13 
    {
        private IDictionary <int, int> _layers;

        public Day13 ()
        {
            _layers = File.ReadAllLines("inputs/day13/input.txt")
                .Select(line => line.Split(':'))
                .ToDictionary(
                    splitString => Int32.Parse(splitString.First()), 
                    splitString => Int32.Parse(splitString.Last()) );
        }

        private bool IsCaught(int depth, int time)
        {
            var isCaught = false;
            if (_layers.ContainsKey(depth))
            {
                isCaught = ((depth + time) % (_layers[depth]*2 - 2)) == 0;
            }
            return isCaught;
        }

        private int GetCurrentSeverity (int depth)
        {
            var severity = 0;
            if (IsCaught(depth, 0))
            {
                severity = depth * _layers[depth];
            }
            return severity;
        }

        public int GetTripSeverity (int startTime)
        {
            var totalSeverity = 0;

            foreach (var depth in _layers.Keys)
            {
                totalSeverity += GetCurrentSeverity(depth);
            }
            return totalSeverity;
        }

        public bool CanEvadeFirewall (int startTime)
        {
            foreach (var depth in _layers.Keys)
            {
                if (IsCaught(depth, startTime))
                    return false;
            }
            return true;
        }

        public int GetMinDelay ()
        {
            var startTime = 0;
            while (!CanEvadeFirewall(startTime))
            {
                startTime++;
            }

            return startTime;
        }
    }
}
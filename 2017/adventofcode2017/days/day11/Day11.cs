using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace adventofcode2017.days.day11 
{

    public class HexGridLocation 
    {
        // North - South Axis
        public int X { get; set; }
        // North East - South West Axis
        public int Y { get; set; }
        // South East - North West Axis
        public int Z { get; set; }
        public void Move (string direction)
        {
            switch (direction)
            {
                case "n":
                    Y++;
                    Z--;
                    break;
                case "s":
                    Y--;
                    Z++;
                    break;
                case "ne":
                    X++;
                    Z--;
                    break;
                case "sw":
                    X--;
                    Z++;
                    break;
                case "se":
                    X++;
                    Y--;
                    break;
                case "nw":
                    X--;
                    Y++;
                    break;
                default:
                    throw new ArgumentException("Incorrect directional argument.");
            }
        }
    }

    public class Day11 
    {
        private IEnumerable<string> _directions;
        private HexGridLocation _childLocation;
        private int _furthestLocation;

        public Day11 ()
        {
            _directions = File.ReadAllText("inputs/day11/input.txt")
                .Split(',');
            _childLocation = new HexGridLocation();
            _furthestLocation = 0;
            ComputeChildLocation();
        }

        private int GetMaxLocation (HexGridLocation hex)
        {
            return new int [] {
                hex.X, hex.Y, hex.Z
            }.Select(num => Math.Abs(num))
            .Max();
        }

        private void ComputeChildLocation ()
        {
            foreach (var direction in _directions)
            {
                _childLocation.Move(direction);
                var max = GetMaxLocation(_childLocation);

                if (max > _furthestLocation)
                    _furthestLocation = max;
            }
        }

        public int GetFewestStepsToChild ()
        {
            return  GetMaxLocation(_childLocation);
        }

        public int GetMaxLocation ()
        {
            return _furthestLocation;
        }
    }
}
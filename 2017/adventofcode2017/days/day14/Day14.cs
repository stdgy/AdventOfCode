using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using adventofcode2017.days.day10;

namespace adventofcode2017.days.day14 
{
    public class Day14 
    {
        private string _input;
        private Day10 _d10;

        public Day14 ()
        {
            _input = File.ReadAllText("inputs/day14/input.txt");
            _d10 = new Day10(256);
        }

        public string GetBinaryFromHex (string hex)
        {
            return String.Join(String.Empty, 
                hex.Select(ch => 
                    Convert.ToString(Convert.ToInt32(ch.ToString(), 16), 2).PadLeft(4, '0'))
            );
        }

        private List<HashSet<Tuple<int,int>>> SeparateCoordinateIntoGroups ( 
            IEnumerable<Tuple<int,int>> coordinates)
        {
            var groups = new List<HashSet<Tuple<int,int>>>();

            foreach (var coord in coordinates)
            {
                var row = coord.Item1;
                var column = coord.Item2;
                var left = column > 0 ? new Tuple<int,int>(row, column-1) : null;
                var above =  row > 0 ? new Tuple<int,int>(row-1, column) : null;

                var aboveGroup = groups.Where(g => g.Contains(above)).FirstOrDefault();
                var leftGroup = groups.Where(g => g.Contains(left)).FirstOrDefault();

                if (aboveGroup != null && leftGroup != null &&
                    aboveGroup != leftGroup)
                {
                    // Merge left group into above
                    aboveGroup.UnionWith(leftGroup);
                    groups.Remove(leftGroup);
                    aboveGroup.Add(coord);
                } 
                else if (aboveGroup != null) 
                {
                    aboveGroup.Add(coord);
                } 
                else if (leftGroup != null) 
                {
                    leftGroup.Add(coord);
                } 
                else 
                {
                    groups.Add(new HashSet<Tuple<int, int>>() {
                        coord
                    });
                }
            }
            return groups;
        }

        public int GetNumUsedSquares ()
        {   
            return Enumerable.Range(0, 128)
                .Select(i => _input + "-" + i)
                .Select(input => _d10.GetKnotHash(input, 256))
                .Select(hash => GetBinaryFromHex(hash))
                .Select(binary => binary.Count(num => num == '1'))
                .Sum(); 
        }

        public int GetNumberOfGroups ()
        {
            var usedCoordinates = Enumerable.Range(0, 128)
                .Select(i => _input + "-" + i)
                .Select(input => _d10.GetKnotHash(input, 256))
                .SelectMany((hash, row) => GetBinaryFromHex(hash)
                    .Select((digit, column) => {
                        if (digit == '0') 
                            return null; 
                        return new Tuple<int,int>(row, column);
                    })
                    .Where(t => t != null));
            
            var groups = SeparateCoordinateIntoGroups(usedCoordinates);
            return groups.Count();
        }
    }
}
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace adventofcode2017.days.day19
{
    public enum Direction {
        North, South, East, West
    }

    public class Location {
        public int Row { get; set; }
        public int Column { get; set; }
    }

    public class Day19 
    {
        private List<List<char>> _map;

        public Day19 ()
        {
            _map = File.ReadAllLines("inputs/day19/input.txt")
                .Select(line => line.Select(c => c).ToList())
                .ToList();
        }

        private int GetStartingBlock ()
        {
            return _map[0].IndexOf('|');
        }

        private Location NextLoc (Location loc, ref Direction direction)
        {
            var nextLoc = new Location()
            {
                Row = loc.Row,
                Column = loc.Column
            };

            if (_map[loc.Row][loc.Column] == '+')
            {
                if (direction == Direction.North || direction == Direction.South)
                {
                    // Check whether we need to go east or west
                    if (loc.Column > 0 && _map[loc.Row][loc.Column-1] == '-')
                    {
                        nextLoc.Column = loc.Column - 1;
                        direction = Direction.West;
                    }
                    else 
                    {
                        nextLoc.Column = loc.Column + 1;
                        direction = Direction.East;
                    }
                }
                else 
                {
                    // Check whether we need to go north or south
                    if (loc.Row > 0 && _map[loc.Row-1][loc.Column] == '|')
                    {
                        nextLoc.Row = loc.Row - 1;
                        direction = Direction.North;
                    }
                    else 
                    {
                        nextLoc.Row = loc.Row + 1;
                        direction = Direction.South;
                    }
                }
            }
            else 
            {
                switch (direction)
                {
                    case Direction.North:
                        nextLoc.Row--;
                        break;
                    case Direction.South:
                        nextLoc.Row++;
                        break;
                    case Direction.East:
                        nextLoc.Column++;
                        break;
                    case Direction.West:
                        nextLoc.Column--;
                        break;
                }
            }
            return nextLoc;
        }

        public void TraversePath (Func<char,bool> BlockProcessor)
        {
            var loc = new Location() {
                Row = 0,
                Column = GetStartingBlock()
            };
            var direction = Direction.South;
            var letters = new List<char>();
            var validPathChars = new char[] { '-', '|', '+' };

            while (_map[loc.Row][loc.Column] != ' ')
            {
                var val = _map[loc.Row][loc.Column];
                BlockProcessor(val);
                loc = NextLoc(loc, ref direction);
            }
        }

        public string GetLettersOnPath ()
        {
            var letters = new List<char>();
            var validPathChars = new char[] { '-', '|', '+' };

            TraversePath((blockVal) => {
                if (!validPathChars.Contains(blockVal))
                {
                    letters.Add(blockVal);
                }
                return true;
            });

            return new String(letters.ToArray());
        }


        public int GetTotalPathLength ()
        {
            var length = 0;

            TraversePath((blockVal) => { 
                length++;
                return true;
            });

            return length;
        }
        
    }
}
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace adventofcode2017.days.day21 
{
    public class Day21 
    {
        private List<char> _grid;
        private IDictionary<string, string> _rules;
        private ISet<string> _2x2Mappings;
        private ISet<string> _3x3Mappings;

        private char[] RotateRight (char[] grid)
        {
            var g = new char[grid.Length];
            if (grid.Length == 4)
            {
                // 2x2
                for (var i = 0; i < grid.Length; i++)
                {
                    g[i] = grid[(grid.Length-1+i) % grid.Length];
                }
            } 
            else 
            {
                // 3x3
                g[0] = grid[6];
                g[1] = grid[3];
                g[2] = grid[0];
                g[3] = grid[7];
                g[4] = grid[4];
                g[5] = grid[1];
                g[6] = grid[8];
                g[7] = grid[5];
                g[8] = grid[2];
            }

            return g;
        }

        private char[] Flip (char[] grid)
        {
            var g = new char[grid.Length];
            if (g.Length == 4)
            {
                // 2 x 2
                g[0] = grid[1];
                g[1] = grid[0];
                g[2] = grid[3];
                g[3] = grid[2];
            }
            else 
            {
                // 3 x 3
                g[0] = grid[2];
                g[1] = grid[1];
                g[2] = grid[0];
                g[3] = grid[5];
                g[4] = grid[4];
                g[5] = grid[3];
                g[6] = grid[8];
                g[7] = grid[7];
                g[8] = grid[6];
            }

            return g;
        }

        private void ComputeMapping (ISet<string> mappings, char[] mapping)
        {
            var rotated = new char[mapping.Length];
            mapping.CopyTo(rotated, 0);
            
            for (var i = 0; i < 3; i++)
            {
                if (mappings.Add(new String(rotated)))
                {
                    ComputeMapping(mappings, Flip(rotated));
                }
                rotated = RotateRight(mapping);
            }

            if (mappings.Add(new String(rotated)))
            {
                ComputeMapping(mappings, Flip(rotated));
            }

        }

        private HashSet<string> Compute2X2Mappings ()
        {
            var g = new char[4] {
                '0', '1', '2', '3'
            };
            var mappings = new HashSet<string>();

            ComputeMapping(mappings, g);    
            return mappings;        
        }

        private HashSet<string> Compute3X3Mappings ()
        {
            var g = new char[9] {
                '0', '1', '2',
                '3', '4', '5',
                '6', '7', '8'
            };
            var mappings = new HashSet<string>();

            ComputeMapping(mappings, g);    
            return mappings;  
        }
        
        public Day21 ()
        {
            // Read in the rules and initialize 
            // our grid.
            _rules = File.ReadAllLines("inputs/day21/input.txt")
                .Select(line => line.Replace("/", String.Empty))
                .Select(line => new {
                    Rule = line.Substring(0, line.IndexOf('=')-1),
                    Result = line.Substring(line.IndexOf('>')+1)
                })
                .ToDictionary(rule => rule.Rule, rule => rule.Result);
            
            _grid = new List<char>() {
                '.', '#', '.',
                '.', '.', '#',
                '#', '#', '#'
            };

            _2x2Mappings = Compute2X2Mappings();
            _3x3Mappings = Compute3X3Mappings();

            Console.WriteLine("test");
                
        }


    }
}
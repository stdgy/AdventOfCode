using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace adventofcode2017.days.day07 
{
    public class Program 
    {
        public string Name { get; set; }
        public int Weight { get; set; }
        public int TotalWeight { get; set; }
        public IEnumerable<string> Children { get; set; }
        public string Parent { get; set; }
    }

    public class Day07 
    {
        IDictionary<string, Program> _programs;

        private void PopulateParents(IDictionary<string, Program> programs)
        {
            // Assume we have children already populated.
            foreach (var name in programs.Keys)
            {
                foreach (var child in programs[name].Children)
                {
                    programs[child].Parent = name;
                }
            }
        }

        private void SetTotalWeights (string node)
        {
            var program = _programs[node];

            foreach (var child in program.Children)
            {
                SetTotalWeights(child);
            }

            program.TotalWeight = program.Weight + 
                program.Children
                    .Select(child => _programs[child].TotalWeight)
                    .Sum();
        }

        private string FindLastUnbalancedNode (string node)
        {
            var weightGroups = _programs[node]
                .Children
                .Select(child => _programs[child])
                .GroupBy(program => program.TotalWeight)
                .Select(group => new {
                    TotalWeight = group.Key,
                    Names = group.Select(p => p.Name) 
                })
                .OrderBy(g => g.Names.Count());
            
            if (weightGroups.Count() == 0 || weightGroups.Count() == 1)
            {
                return node;
            }

            return FindLastUnbalancedNode(weightGroups.First().Names.First());
        }

        private int GetSiblingWeight (string node)
        {
            var parent = _programs[node].Parent;
            
            return _programs[parent]
                .Children
                .Where(child => child != node)
                .Select(child => _programs[child].TotalWeight)
                .First();
        }

        public Day07()
        {
            _programs = File.ReadLines("inputs/day07/input.txt")
                .Select(line => { 
                    var endName = line.IndexOf(' ');
                    var startWeight = line.IndexOf('(');
                    var endWeight = line.IndexOf(')');
                    var startChildren = line.IndexOf('>');

                    var p =  new Program () {
                        Name = line.Substring(0, endName),
                        Weight = Int32.Parse(line.Substring(startWeight+1, endWeight-startWeight-1)),
                        Children = new List<string>()
                    };

                    if (startChildren > 0)
                    {
                        p.Children = line.Substring(startChildren+2).Split(", ");
                    }

                    return p;
                })
                .ToDictionary( prog => prog.Name);
        }

        public string FindRootProgram()
        {
            PopulateParents(_programs);

            return _programs
                .Where(p => p.Value.Parent == null)
                .Select(p => p.Key)
                .First();
        }

        public int FindCorrectWeight (string root)
        {
            SetTotalWeights(root);
            var lastUnbalancedNode = FindLastUnbalancedNode(root);
            var program = _programs[lastUnbalancedNode];

            return GetSiblingWeight(lastUnbalancedNode) - program.TotalWeight + program.Weight; 
        }
    }
}
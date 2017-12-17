using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace adventofcode2017.days.day12 
{
    public class Day12 
    {
        IDictionary <int, IEnumerable<int>> _nodeConnections;

        public Day12 ()
        {
            _nodeConnections = File.ReadAllLines("inputs/day12/input.txt")
                .Select(line => new {
                    ID = Int32.Parse(line.Substring(0, line.IndexOf('<')-1)),
                    Connections = line.Substring(line.IndexOf('>')+1)
                        .Split(',')
                        .Select(str => Int32.Parse(str))
                })
                .ToDictionary(
                    item => item.ID,
                    item => item.Connections
                );
        }

        private ISet<int> GetGroup(int startNode)
        {
            var group = new HashSet<int>();
            var q = new Queue<int>();
            q.Enqueue(startNode);
            
            while (q.Count > 0)
            {
                var node = q.Dequeue();
                if (group.Add(node))
                {
                    foreach (var child in _nodeConnections[node])
                    {
                        if (!group.Contains(child))
                        {
                            q.Enqueue(child);
                        }
                    }
                }
            }
            return group;
        }

        public int GetNumberOfConnectionsFromNode(int startNode)
        {
            return GetGroup(startNode).Count;
        }

        public int GetTotalNumberOfGroups ()
        {
            var groups = new List<ISet<int>> ();

            foreach (var node in _nodeConnections.Keys)
            {
                if (groups.Where(group => group.Contains(node)).Count() == 0)
                {
                    groups.Add(GetGroup(node));
                }
            }

            return groups.Count;
        }
    }
}
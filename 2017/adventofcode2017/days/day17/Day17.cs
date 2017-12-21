using System;
using System.Collections.Generic;
using System.IO;

namespace adventofcode2017.days.day17 
{
    public class Day17 
    {
        private int _stepSize;

        public Day17 () 
        {
            _stepSize = Int32.Parse(File.ReadAllText("inputs/day17/input.txt"));
        }

        public int GetNextValueInCompletedBuffer ()
        {
            var currVal = 0;
            var list = new LinkedList<int>();
            var node = new LinkedListNode<int>(currVal);
            list.AddFirst(node);

            while (node.Value != 2017)
            {
                currVal++;
                for (var step = 0; step < _stepSize; step++)
                {
                    if (node.Next == null)
                        node = list.First;
                    else 
                        node = node.Next;
                    
                }
                var newNode = new LinkedListNode<int>(currVal);
                list.AddAfter(node, newNode);
                node = newNode;
            }

            return node.Next.Value;
        }

        public int GetValueForAngrySpinlock ()
        {
            // Returns the value after 0 after 
            // it has made 50,000,000 insertions.
            var size = 1;
            var valAfterZero = 0;
            var currIndex = 0;
            var value = 0;

            while (value < 50000000)
            {
                currIndex = (currIndex + _stepSize) % size;
                value++;
                
                if (currIndex == 0)
                {
                    valAfterZero = value;
                }
                size++;
                currIndex++;
            }

            return valAfterZero;
        }
    }
}
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace adventofcode2017.days.day10 
{
    public class Day10 
    {
        IEnumerable<byte> _lengths;
        IList<byte> _list;

        public Day10 (int length, bool parseAllBytes=false)
        {
            if (parseAllBytes)
            {
                _lengths = File.ReadAllText("inputs/day10/input.txt")
                    .Select(ch => (byte)ch)
                    .Concat(new List<byte>() {
                        17, 31, 73, 47, 23
                    });
            }
            else 
            {
                _lengths = File.ReadAllText("inputs/day10/input.txt")
                    .Split(",")
                    .Select(str => Byte.Parse(str));
            }

            _list = Enumerable.Range(0, length)
                .Select(num => (byte)num)
                .ToList();
        }

        private void ProcessLength(int currentPosition, int skipSize, int length)
        {
            for (var i = 0; i < length/2; i++)
            {
                var index = (currentPosition + i) % _list.Count;
                var reverseIndex = (currentPosition + length - 1 - i) % _list.Count;
                var temp = _list[index];
                _list[index] = _list[reverseIndex];
                _list[reverseIndex] = temp;
            }
        }

        private void ProcessLengths (int rounds)
        {
            var currentPosition = 0;
            var skipSize = 0;

            for (var i = 0; i < rounds; i++)
            {
                foreach (var length in _lengths)
                {
                    ProcessLength(currentPosition, skipSize, length);
                    currentPosition = (currentPosition + length + skipSize) % _list.Count;
                    skipSize++;
                }
            }
        }

        private IEnumerable<byte> GetDenseHash ()
        {
            var denseHash = new List<byte>();
            for (var i = 0; i < _list.Count/16; i++)
            {
                byte xor = 0;
                for(var j = 0; j < 16; j++)
                {
                    xor = (byte)(xor ^ _list[(i*16) + j]);
                }
                denseHash.Add(xor);
            }
            return denseHash;
        }

        private string GetHex(IEnumerable<byte> bytes)
        {
            return string.Concat(bytes.Select(b => b.ToString("x2")));
        }

        public int GetMultipleOfFirstTwoNumbers ()
        {
            ProcessLengths(1);
            return _list[0] * _list[1];
        }

        public string GetKnotHash ()
        {
            ProcessLengths(64);
            var denseHash = GetDenseHash();
            return GetHex(denseHash);
        }
    }
}
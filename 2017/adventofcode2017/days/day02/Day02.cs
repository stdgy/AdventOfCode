using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace adventofcode2017.days.day02
{
    public class Day02
    {
        private List<List<int>> _spreadsheet;

        public Day02 ()
        {
            _spreadsheet = new List<List<int>>();
            using (var inputFile = File.OpenRead("inputs/day02/input1.txt"))
            using (var streamReader = new StreamReader(inputFile))
            {
                var line = streamReader.ReadLine();
                while (line != null) 
                {
                    _spreadsheet.Add(line.Split('\t').Select(w => Int32.Parse(w)).ToList());
                    line = streamReader.ReadLine();
                }
            }
        }

        public int ComputeChecksum ()
        {
            int checksum = 0;
            int min = 0;
            int max = 0;
            foreach (var row in _spreadsheet)
            {
                min = row[0];
                max = row[0];
                foreach (var number in row)
                {
                    if (number < min) 
                    {
                        min = number;
                    }
                    if (number > max)
                    {
                        max = number;
                    }
                }
                checksum += (max - min);
            }
            return checksum;
        }

        private (int Numerator, int Denominator) GetDivisors (List<int> numbers)
        {
            for (var i = 0; i < numbers.Count; i++)
                for (var j = 0; j < numbers.Count; j++)
                {
                    if (j != i && (numbers[i] % numbers[j]) == 0)
                    {
                        return (numbers[i], numbers[j]);
                    }
                }
            
            return (0, 0);
        }

        public int GetSumOfEvenlyDivisibleDivisions ()
        {
            int sum = 0;

            foreach (var row in _spreadsheet)
            {
                var (numerator, denominator) = GetDivisors(row);
                if (denominator != 0)
                    sum += (numerator/denominator);
            }
            return sum;
        }

        public void RunTestCases ()
        {
            _spreadsheet = new List<List<int>> ();
        }
    }
}
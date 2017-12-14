using System;
using System.IO;

namespace adventofcode2017.days.day03 
{
    public class Day03 
    {
        private int puzzleInput;

        private enum Direction {
            NE, SW
        };

        public Day03 () 
        {
            using (var file = File.OpenRead("inputs/day03/input1.txt"))
            using (var streamReader = new StreamReader(file))
            {
                puzzleInput = Int32.Parse(streamReader.ReadToEnd());
            }
        }

        public int GetMovesRequiredToLeave (int position)
        {
            // Count to first largest corner
            var corner = 1;
            var amountChange = 2;
            var cornerNumber = 0;

            if (position == 1)
                return 0;

            while ( corner < position)
            {
                corner += amountChange;
                cornerNumber++;

                if (cornerNumber == 4 && corner < position)
                {
                    amountChange += 2;
                    cornerNumber = 0;
                }
            }

            var movesToCenterLine = Math.Abs((corner - (amountChange/2)) - position);
            var movesInward = amountChange/2;

            return movesToCenterLine + movesInward;
        }

        public void Execute () 
        {
            Console.WriteLine($"{GetMovesRequiredToLeave(1)}");
            Console.WriteLine($"{GetMovesRequiredToLeave(12)}");
            Console.WriteLine($"{GetMovesRequiredToLeave(23)}");
            Console.WriteLine($"{GetMovesRequiredToLeave(1024)}");
            Console.WriteLine($"{GetMovesRequiredToLeave(312051)}");
        }
    }
}
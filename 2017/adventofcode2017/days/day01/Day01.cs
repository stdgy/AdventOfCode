using System.IO;

namespace adventofcode2017.days.day01 
{
    public class Day01 
    {
        private string _input;
        public int GetSumOfNextDigits ()
        {
            int sum = 0;
            for(var i = 0; i < _input.Length; i++)
            {
                if (_input[i] == _input[(i+1) % _input.Length])
                {
                    sum += (_input[i] - 48);
                }
            }
            return sum;
        }

        public int GetSumOfCircularDigits ()
        {
            int sum = 0;
            for (var i = 0; i < _input.Length; i++)
            {
                if (_input[i] == _input[(i + _input.Length/2) % _input.Length])
                {
                    sum += (_input[i] - 48);
                }
            }
            return sum;
        }

        public Day01()
        {
            _input = File.ReadAllText("inputs/day01/input.txt");
        }
    }
}
using System.IO;

namespace adventofcode2017.days.day09 
{
    public class Day09 
    {
        private string _stream;
        private bool _isGarbage;
        private bool _isIgnored;
        private int _groupDepth;
        private int _score;
        private int _garbageCount;

        private void Read (char c)
        {
            if (_isIgnored)
            {
                _isIgnored = false;
                return;
            }

            if (c == '!')
            {
                _isIgnored = true;
                return;
            }

            if (_isGarbage)
            {
                if (c == '>')
                {
                    _isGarbage = false;
                }

                if (_isGarbage)
                    _garbageCount++;

                return;
            }

            switch (c)
            {
                case '{':
                    _groupDepth++;
                    break;
                case '}':
                    _score += _groupDepth--;
                    break;
                case '<':
                    _isGarbage = true;
                    break;
                default:
                    // Just eat the character
                    break;
            }
        }

        private void ParseStream ()
        {
            foreach (var character in _stream)
            {
                Read(character);
            }
        }

        public Day09 ()
        {
            _stream = File.ReadAllText("inputs/day09/input.txt");
            ParseStream();
        }

        public Day09 (string input)
        {
            _stream = input;
            ParseStream();
        }

        public int GetTotalScore ()
        {
            return _score;
        }

        public int GetGarbageCount ()
        {
            return _garbageCount;
        }
    }
}
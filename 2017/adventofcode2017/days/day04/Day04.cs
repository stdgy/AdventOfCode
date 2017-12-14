using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace adventofcode2017.days.day04
{
    public class Day04 
    {
        private List<string> _passphrases;

        private bool IsPassphraseValid (string passphrase)
        {
            var words = passphrase.Split(' ');
            return words.Length == words.Distinct().Count();
        }

        private bool IsPassphraseAnagramValid (string passphrase)
        {
            var words = passphrase.Split(' ');
            var numDistinctAnagram = words
                .Select(word => new String(word.ToArray().OrderBy(c => c).ToArray()))
                .Distinct()
                .Count();
            
            return words.Length == numDistinctAnagram;
        }
        
        public Day04 ()
        {
            // Setup input
            using (var sr = File.OpenText("inputs/day04/input.txt"))
            {
                _passphrases = new List<string>();
                var line = String.Empty;

                while ((line = sr.ReadLine()) != null)
                {
                    _passphrases.Add(line);
                }
            }
        }

        private int GetNumValidPassphrases(Func<string, bool> validChecker)
        {
            return _passphrases
                .Where(phrase => validChecker(phrase))
                .Count();
        }

        public int GetNumValidPassphrases ()
        {
            return GetNumValidPassphrases(IsPassphraseValid);
        }

        public int GetNumValidAnagramPassphrases () 
        {
            return GetNumValidPassphrases(IsPassphraseAnagramValid);
        }
        
        public int Test ()
        {
            _passphrases = new List<string> {
                "aa bb cc dd ee",
                "aa bb cc dd aa",
                "aa bb cc dd aaa"
            };

            return GetNumValidPassphrases();
        }
    }
}
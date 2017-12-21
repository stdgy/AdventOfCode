using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace adventofcode2017.days.day16
{
    public enum DanceMoveType
    {
        Spin, Exchange, Partner
    }

    public interface IDanceMove
    {
        DanceMoveType GetType();
        int ExecuteMove(int startIndex, IDictionary<int,string> posToName, IDictionary<string,int> nameToPos);
    }

    public class Spin : IDanceMove
    {
        public int NumToMove { get; set; }

        DanceMoveType IDanceMove.GetType()
        {
            return DanceMoveType.Spin;
        }

        int IDanceMove.ExecuteMove(int startIndex, IDictionary<int, string> posToName, IDictionary<string, int> nameToPos)
        {
            return (startIndex + 16 - NumToMove) % 16;
        }

    }

    public class Exchange : IDanceMove
    {
        public int PositionA { get; set; }
        public int PositionB { get; set; }

        int IDanceMove.ExecuteMove(int startIndex, IDictionary<int, string> posToName, IDictionary<string, int> nameToPos)
        {
            var a = (PositionA + startIndex) % 16;
            var b = (PositionB + startIndex) % 16;
            var temp = posToName[a];
            posToName[a] = posToName[b];
            posToName[b] = temp;

            nameToPos[posToName[a]] = a;
            nameToPos[posToName[b]] = b;

            return startIndex;
        }

        DanceMoveType IDanceMove.GetType()
        {
            return DanceMoveType.Exchange;
        }
    }

    public class Partner : IDanceMove
    {
        public string PositionA { get; set; }
        public string PositionB { get; set; }

        int IDanceMove.ExecuteMove(int startIndex, IDictionary<int, string> posToName, IDictionary<string, int> nameToPos)
        {
            var temp = nameToPos[PositionA];
            nameToPos[PositionA] = nameToPos[PositionB];
            nameToPos[PositionB] = temp;

            var aPos = nameToPos[PositionA];
            var bPos = nameToPos[PositionB];

            posToName[aPos] = PositionA;
            posToName[bPos] = PositionB;

            return startIndex;
        }

        DanceMoveType IDanceMove.GetType()
        {
            return DanceMoveType.Partner;
        }
    }


    public class Day16 
    {
        private IEnumerable<IDanceMove> _danceMoves;
        
        private int _startIndex = 0;
        private IDictionary<int, string> _posToName;
        private IDictionary<string,int> _nameToPos;

        public Day16 ()
        {
            _danceMoves = File.ReadAllText("inputs/day16/input.txt")
                .Split(',')
                .Select(move => ParseMove(move))
                .ToList();
        }

        private void Initialize () 
        {
            _posToName = Enumerable.Range(0, 16)
                .ToDictionary(
                    num => num, 
                    num => ((char)(97 + num)).ToString()
                );
            _nameToPos = Enumerable.Range(0, 16)
                .ToDictionary(
                    num => ((char)(97 + num)).ToString(),
                    num => num
                );
            _startIndex = 0;
        }

        private IDanceMove ParseMove (string move)
        {
            IDanceMove parsedMove = null;
            switch (move[0])
            {
                case 's':
                    parsedMove = new Spin() {
                        NumToMove = Int32.Parse(move.Substring(1))
                    };
                    break;
                case 'x':
                    parsedMove = new Exchange() {
                        PositionA = Int32.Parse(move.Substring(1, 
                            move.IndexOf('/')-1)),
                        PositionB = Int32.Parse(move.Substring(
                            move.IndexOf('/')+1
                        ))
                    };
                    break;
                case 'p':
                    parsedMove = new Partner() {
                        PositionA = move.Substring(1, 
                            move.IndexOf('/')-1),
                        PositionB = move.Substring(move.IndexOf('/')+1)
                    };
                    break;
                default:
                    throw new InvalidDataException("Dance move data is invalid.");
            }

            return parsedMove;
        }

        private void PerformDance ()
        {
            foreach (var move in _danceMoves)
            {
                _startIndex = move.ExecuteMove(_startIndex, _posToName, _nameToPos);
            }
        }

        private string GetPrograms ()
        {
            return String.Join(String.Empty, 
                Enumerable.Range(0, 16)
                    .Select(num => (num + _startIndex) % 16)
                    .Select(index => _posToName[index]));
        }

        // Returns the period of repetition
        private int GetPeriodOfRepition ()
        {
            Initialize();
            PerformDance();
            var programs = GetPrograms();
            var period = 0;

            do
            {
                PerformDance();
                period++;
            } while (programs != GetPrograms());

            return period;
        }

        public string GetProgramOrderAfterNumDances (int numDances)
        {
            var period = GetPeriodOfRepition();
            var requiredDances = numDances % period;

            Initialize();
            for (var i = 0; i < requiredDances; i++)
            {
                PerformDance();
            }
            
            return GetPrograms();
        }
    }
}
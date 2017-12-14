using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace adventofcode2017.days.day08 
{
    public class Day08 
    {
        private IEnumerable<string> _instructions;
        private IDictionary<string, int> _registers;
        private int _maxRegisterValue = int.MinValue;

        private enum Operation
        {
            Increment,
            Decrement
        }

        private enum Equality
        {
            LessThan,
            LessThanEqualTo,
            Equal,
            NotEqual,
            GreaterThanEqualTo,
            GreaterThan
        }

        private class Instruction 
        {
            public string Register { get; set; }
            public Operation Operation { get; set; }
            public int Quantity { get; set; }
            public string TargetRegister { get; set; }
            public Equality Equality { get; set; }
            public int ComparisonAmount { get; set; }
        }

        private Instruction ParseInstruction (string instruction)
        {
            var parts = instruction.Split(' ');
            var register = parts[0];
            Operation operation;
            Equality equality;

            if (parts[1] == "inc") 
                operation = Operation.Increment;
            else 
                operation = Operation.Decrement;
            
            var quantity = Int32.Parse(parts[2]);
            var targetRegister = parts[4];

            switch (parts[5])
            {
                case "<":
                    equality = Equality.LessThan;
                    break;
                case "<=":
                    equality = Equality.LessThanEqualTo;
                    break;
                case "==":
                    equality = Equality.Equal;
                    break;
                case "!=":
                    equality = Equality.NotEqual;
                    break;
                case ">=":
                    equality = Equality.GreaterThanEqualTo;
                    break;
                case ">":
                    equality = Equality.GreaterThan;
                    break;
                default:
                    throw new FormatException("Equality operator is not recognized.");
            }

            var comparisonAmount = Int32.Parse(parts[6]);

            return new Instruction () {
                Register = register,
                Operation = operation,
                Quantity = quantity,
                TargetRegister = targetRegister,
                Equality = equality,
                ComparisonAmount = comparisonAmount
            };
        }

        private void Execute (Instruction instruction)
        {
            int registerVal = 0;
            bool comparisonResult = false;

            if (_registers.ContainsKey(instruction.TargetRegister))
            {
                registerVal = _registers[instruction.TargetRegister];
            }

            switch (instruction.Equality)
            {
                case Equality.LessThan:
                    comparisonResult = registerVal < instruction.ComparisonAmount;
                    break;
                case Equality.LessThanEqualTo:
                    comparisonResult = registerVal <= instruction.ComparisonAmount;
                    break;
                case Equality.Equal:
                    comparisonResult = registerVal == instruction.ComparisonAmount;
                    break;
                case Equality.NotEqual:
                    comparisonResult = registerVal != instruction.ComparisonAmount;
                    break;
                case Equality.GreaterThanEqualTo:
                    comparisonResult = registerVal >= instruction.ComparisonAmount;
                    break;
                case Equality.GreaterThan:
                    comparisonResult = registerVal > instruction.ComparisonAmount;
                    break;
                default:
                    throw new FormatException("Equality operator is not recognized.");
            }

            if (comparisonResult)
            {
                int operationResult = 0;
                switch (instruction.Operation)
                {
                    case Operation.Increment:
                        operationResult = 
                            (_registers.ContainsKey(instruction.Register) ? _registers[instruction.Register] : 0) +
                            instruction.Quantity;
                        break;
                    case Operation.Decrement:
                        operationResult = 
                            (_registers.ContainsKey(instruction.Register) ? _registers[instruction.Register] : 0) -
                            instruction.Quantity;
                        break;
                }

                _registers[instruction.Register] = operationResult;
                TrackLargestRegister(operationResult);
            }
        }

        private void TrackLargestRegister (int value)
        {
            if (value > _maxRegisterValue)
                _maxRegisterValue = value;
        }

        private void ProcessInstructions ()
        {
            foreach (var instruction in _instructions)
            {
                Execute(ParseInstruction(instruction));
            }
        }

        public Day08 ()
        {
            _instructions = File.ReadLines("inputs/day08/input.txt");
            _registers = new Dictionary<string, int>();
        }

        public int GetLargestValueInRegisters ()
        {
            if (_registers.Count() == 0)
            {
                ProcessInstructions();
            }
            return _registers.Values.Max();
        }

        public int GetLargestValueGenerated ()
        {
            if (_registers.Count() == 0)
            {
                ProcessInstructions();
            }
            return _maxRegisterValue;
        }
    }
}
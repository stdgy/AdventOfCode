using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace adventofcode2017.days.day18 
{
    public class Program 
    {
        private List<List<string>> _instructions;
        private IDictionary<char,Int64> _registers;
        private Int64 _lastSoundPlayed;
        private int _sendCount;
        private bool _isDeadlocked;
        private bool _isDone;
        private int _currInstruction;
        private Queue<Int64> _messages;

        public int SendCount { get { return _sendCount; } }
        public bool IsDeadlocked { get { return _isDeadlocked; } }
        public bool IsDone { get { return _isDone; } }
        public Queue<Int64> Messages { get { return _messages; } }
        public Program LinkedProgram { get; set; }
       
        public Program (List<List<string>> instructions, int programId)
        {
            _instructions = instructions;
            _registers = new Dictionary<char,Int64>();
            _currInstruction = 0;
            _sendCount = 0;
            _currInstruction = 0;
            _registers['p'] = programId;
            _isDone = false;
            _isDeadlocked = false;
            _messages = new Queue<Int64>();
        }

        private Int64 GetValue(string r)
        {
            Int64 val = 0;
            if (!Int64.TryParse(r, out val))
            {
                var reg = Char.Parse(r);
                val =_registers.Keys.Contains(reg) ? _registers[reg] : 0;
            } 
            return val;
        }

        private void snd(string arg1)
        {
            _lastSoundPlayed = GetValue(arg1);
            _sendCount++;
        }

        private void set(string arg1, string arg2)
        {
            _registers[Char.Parse(arg1)] = GetValue(arg2);
        }

        private void add(string arg1, string arg2)
        {
            _registers[Char.Parse(arg1)] = GetValue(arg1) + GetValue(arg2);
        }

        private void mul(string arg1, string arg2)
        {
            _registers[Char.Parse(arg1)] = GetValue(arg1) * GetValue(arg2);
        }

        private void mod(string arg1, string arg2)
        {
            _registers[Char.Parse(arg1)] = GetValue(arg1) % GetValue(arg2);
        }

        private Int64? rcv(string arg1)
        {
            if (GetValue(arg1) != 0)
            {
                return _lastSoundPlayed;
            }
            return null;
        }

        private int jgz(string arg1, string arg2)
        {
            Int64 amt = 0;
            if (GetValue(arg1) > 0)
                amt = GetValue(arg2) - 1;

            return (int)amt;
        }

        private bool send (string reg)
        {
            var val = GetValue(reg);
            LinkedProgram.Messages.Enqueue(val);
            _sendCount++;
            return true;
        }

        private bool receive (string reg)
        {
            if (Messages.Count > 0)
            {
                _registers[Char.Parse(reg)] = Messages.Dequeue();
                return true;
            }
            return false;
        }
        
        public int GetValueOfFirstRecovered ()
        {
            var count = _instructions.Count();
            while (_currInstruction > -1 && _currInstruction < count)
            {
                var instruction = _instructions[_currInstruction][0];
                var arg1 = _instructions[_currInstruction][1];
                var arg2 = _instructions[_currInstruction].Count == 3 ? _instructions[_currInstruction][2] : null;

                switch (instruction)
                {
                    case "snd":
                        snd(arg1);
                        break;
                    case "set":
                        set(arg1, arg2);
                        break;
                    case "add":
                        add(arg1, arg2);
                        break;
                    case "mul":
                        mul(arg1, arg2);
                        break;
                    case "mod":
                        mod(arg1, arg2);
                        break;
                    case "rcv":
                        var lastSound = rcv(arg1);
                        if (lastSound != null)
                        {
                            return (int)lastSound;
                        }
                        break;
                    case "jgz":
                        _currInstruction += jgz(arg1, arg2);
                        break;
                    default:
                        throw new InvalidDataException("instruction is not allowed");
                }

                _currInstruction++;
            }

            return 0;
        }

        public void Run ()
        {
            // Excutes the program until it waits to receives a
            // message, deadlocks or completes.
            var count = _instructions.Count();
            var instructionsProcessed = 0;
            while (_currInstruction > -1 && _currInstruction < count)
            {
                var instruction = _instructions[_currInstruction][0];
                var arg1 = _instructions[_currInstruction][1];
                var arg2 = _instructions[_currInstruction].Count == 3 ? _instructions[_currInstruction][2] : null;

                switch (instruction)
                {
                    case "snd":
                        send(arg1);
                        break;
                    case "set":
                        set(arg1, arg2);
                        break;
                    case "add":
                        add(arg1, arg2);
                        break;
                    case "mul":
                        mul(arg1, arg2);
                        break;
                    case "mod":
                        mod(arg1, arg2);
                        break;
                    case "rcv":
                        if (!receive(arg1))
                        {
                            if (instructionsProcessed == 0)
                            {
                                _isDeadlocked = true;
                                _isDone = true;
                            }
                            return;
                        }
                        break;
                    case "jgz":
                        _currInstruction += jgz(arg1, arg2);
                        break;
                    default:
                        throw new InvalidDataException("instruction is not allowed");
                }
                _currInstruction++;
                instructionsProcessed++;
            }
            _isDone = true;
        }
    }
    public class Day18 
    {
        private List<List<string>> _instructions;

        public Day18 ()
        {
            _instructions = File.ReadAllLines("inputs/day18/input.txt")
                .Select(line => line.Split(' ').ToList())
                .ToList();

        }

        public int GetValueOfFirstRecovered ()
        {
            var p = new Program(_instructions, 0);
            return p.GetValueOfFirstRecovered();
        }

        public int GetNumProgramSends ()
        {
            var p0 = new Program(_instructions, 0);
            var p1 = new Program(_instructions, 1);

            p0.LinkedProgram = p1;
            p1.LinkedProgram = p0;

            while (!p0.IsDone || !p1.IsDone)
            {
                p0.Run();
                p1.Run();
            }

            return p1.SendCount;
        }

    }
}
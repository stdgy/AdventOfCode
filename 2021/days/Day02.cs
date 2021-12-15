using System.Text.RegularExpressions;

public class Day02 
{
    public enum Direction 
    {
        Up,
        Down,
        Forward
    }

    public (Direction, int) ParseCommand(string command)
    {
        var rx = new Regex("^(\\w+)\\s(\\d+)$");
        var match = rx.Match(command);
        var direction = match.Groups[1].Value;
        var amount = Int32.Parse(match.Groups[2].Value);

        if (direction == "up")
        {
            return (Direction.Up, amount);
        }
        else if (direction == "down")
        {
            return (Direction.Down, amount);
        }
        else 
        {
            return (Direction.Forward, amount);
        }
    }

    public void Part1()
    {
        // Aggregating over (horizontal position, depth)
        var coordinates = File.ReadAllLines("inputs/02_01.txt")
            .Select(command => ParseCommand(command))
            .Aggregate((0, 0), (accum, parsedCommand) => {
                switch (parsedCommand.Item1)
                {
                    case Direction.Up:
                        return (accum.Item1, accum.Item2 - parsedCommand.Item2);
                    case Direction.Down:
                        return (accum.Item1, accum.Item2 + parsedCommand.Item2);
                    case Direction.Forward:
                        return (accum.Item1 + parsedCommand.Item2, accum.Item2);
                }

                return (0,0);
            });

        var total = coordinates.Item1 * coordinates.Item2;

        Console.WriteLine($"Horizontal position x depth = {total}");
    }

    public void Part2()
    {
        // Aggregating over (horizontal position, depth, aim)
        var coordinates = File.ReadAllLines("inputs/02_01.txt")
            .Select(command => ParseCommand(command))
            .Aggregate((0, 0, 0), (accum, parsedCommand) => {
                switch (parsedCommand.Item1)
                {
                    case Direction.Up:
                        return (accum.Item1, accum.Item2, accum.Item3 - parsedCommand.Item2);
                    case Direction.Down:
                        return (accum.Item1, accum.Item2, accum.Item3 + parsedCommand.Item2);
                    case Direction.Forward:
                        return (accum.Item1 + parsedCommand.Item2, accum.Item2 + (accum.Item3 * parsedCommand.Item2), accum.Item3);
                }

                return (0,0,0);
            });
        
        var total = coordinates.Item1 * coordinates.Item2;

        Console.WriteLine($"Horizontal position x depth = {total}");
    }
}

public class Day03
{
    public List<int> SetupAccumulator(string line)
    {
        var accumulator = new List<int>(line.Length);

        for (var i = 0; i < line.Length; i++)
        {
            accumulator.Add(0);
        }

        return accumulator;
    }

    public List<int> CalculateGammaRate (List<int> accumulator, string[] lines)
    {
        var threshold = lines.Length / 2;
        var gammaRate = new List<int>(accumulator.Count);

        foreach (var line in lines)
        {
            for(var i = 0; i < line.Length; i++)
            {
                if (line[i] == '1')
                {
                    accumulator[i] += 1;
                }
            }
        }

        for(var i = 0; i < accumulator.Count; i++)
        {
            if (accumulator[i] > threshold)
            {
                gammaRate.Add(1);
            }
            else 
            {
                gammaRate.Add(0);
            }
        }

        return gammaRate;
    }

    public List<int> CalculateEpsilonRate(List<int> gammaRate)
    {
        return gammaRate
            .Select(val => val == 1 ? 0 : 1)
            .ToList();
    }

    public long BinaryToDecimal(List<int> binary)
    {
        long total = 0;
        for (var i = 0; i < binary.Count; i++)
        {
            if (binary[i] == 1)
            {
                total += (1 << (binary.Count - i - 1));
            }
        }

        return total;
    }

    public List<int> CalculateOxygenGeneratorRating(string[] lines)
    {
        List<string> remainingLines = new List<string>(lines);

        // Calculate most common value
        for (var i = 0; i < remainingLines[0].Length; i++)
        {
            int ones = remainingLines.Sum(line => line[i] == '1' ? 1 : 0);
            int zeroes = remainingLines.Count - ones;

            if (ones > zeroes || ones == zeroes)
            {
                remainingLines.RemoveAll(line => line[i] == '0');
            }
            else
            {
                remainingLines.RemoveAll(line => line[i] == '1');
            }

            if (remainingLines.Count == 1)
            {
                return remainingLines[0].Select(c => c == '0' ? 0 : 1).ToList();
            }
        }

        return remainingLines[0].Select(c => c == '0' ? 0 : 1).ToList();
    }

    public List<int> CalculateC02ScrubberRating(string[] lines)
    {

        List<string> remainingLines = new List<string>(lines);

        // Calculate most common value
        for (var i = 0; i < remainingLines[0].Length; i++)
        {
            int ones = remainingLines.Sum(line => line[i] == '1' ? 1 : 0);
            int zeroes = remainingLines.Count - ones;

            if (ones > zeroes || ones == zeroes)
            {
                remainingLines.RemoveAll(line => line[i] == '1');
            }
            else
            {
                remainingLines.RemoveAll(line => line[i] == '0');
            }

            if (remainingLines.Count == 1)
            {
                return remainingLines[0].Select(c => c == '0' ? 0 : 1).ToList();
            }
        }

        return remainingLines[0].Select(c => c == '0' ? 0 : 1).ToList();
    }

    public void Part1() 
    {
        var input = File.ReadAllLines("inputs/03_01.txt");
        var accumulator = SetupAccumulator(input[0]);
        var gammaRate = CalculateGammaRate(accumulator, input);
        var epsilonRate = CalculateEpsilonRate(gammaRate);

        var total = BinaryToDecimal(gammaRate) * BinaryToDecimal(epsilonRate);

        Console.WriteLine($"Total is: {total}");
    }

    public void Part2()
    {
        var input = File.ReadAllLines("inputs/03_01.txt");

        var oxygenGeneratorRating = CalculateOxygenGeneratorRating(input);
        var c02ScrubberRating = CalculateC02ScrubberRating(input);

        var total = BinaryToDecimal(oxygenGeneratorRating) * BinaryToDecimal(c02ScrubberRating);

        Console.WriteLine($"Total is: {total}");
    }
}

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
    }
}
public class Day01 
{
    public static void Part1()
    {
        var numbers = File.ReadAllLines("inputs/01_01.txt")
            .Select(num => Int64.Parse(num));
        var numIncreasing = 0;
        long? prevNum = null;

        foreach(var num in numbers)
        {
            if (prevNum != null)
            {
                if (num > prevNum)
                {
                    numIncreasing++;
                }
            }
            prevNum = num;
        }
        
        Console.WriteLine($"Number of entries greater than previous entry: {numIncreasing}");
    }

    public static void Part2()
    {
        var numbers = File.ReadAllLines("inputs/01_01.txt")
            .Select(num => Int64.Parse(num))
            .ToList();
        var numIncreasingCount = 0;
        long? lastWindowSum = null;
        
        for(var i = 0; i < numbers.Count-2; i++)
        {
            var windowSum = numbers[i] + numbers[i+1] + numbers[i+2];

            if (lastWindowSum != null)
            {
                if (windowSum > lastWindowSum)
                {
                    numIncreasingCount++;
                }
            }

            lastWindowSum = windowSum;
        }

        Console.WriteLine($"The number of increasing sliding windows is: {numIncreasingCount}");
    }
}
using System.Text.RegularExpressions;

public class Day05 
{
    private Regex lineRegex => new Regex(@"^(\d+),(\d+) -> (\d+),(\d+)", RegexOptions.Compiled);

    public record Point(int x, int y);

    public record Line(Point p1, Point p2);

    public Line ReadLine(string line)
    { 
        var match = lineRegex.Match(line);
        
        return new Line(
            new Point(Int32.Parse(match.Groups[1].Value), Int32.Parse(match.Groups[2].Value)),
            new Point(Int32.Parse(match.Groups[3].Value), Int32.Parse(match.Groups[4].Value))
        );
    }

    public bool IsHorizontalOrVertical(Line d)
    {
        return d.p1.x == d.p2.x || d.p1.y == d.p2.y;
    }

    public List<Point> LineToPoints(Line l)
    {
        var points = new List<Point>();
        var p = l.p1;
        var hasDeltaX = l.p1.x != l.p2.x;
        var hasDeltaY = l.p1.y != l.p2.y;
        var isIncX = l.p1.x < l.p2.x;
        var isIncY = l.p1.y < l.p2.y;
        while (p != l.p2)
        {
            points.Add(p);
            var x = hasDeltaX ? (isIncX ? p.x + 1 : p.x - 1) : p.x;
            var y = hasDeltaY ? (isIncY ? p.y + 1 : p.y - 1) : p.y;
            p = new Point(x,y);
        }
        points.Add(p);
    
        return points;
    }

    public void Part1()
    {
        var numberOfOverlaps = File.ReadAllLines("inputs/05_01.txt")
            .Select(line => ReadLine(line))
            .Where(line => IsHorizontalOrVertical(line))
            .SelectMany(line => LineToPoints(line))
            .Aggregate(new Dictionary<Point, int>(), (d, p) => {
                d[p] = 1 + (d.ContainsKey(p) ? d[p] : 0);
                return d;
            })
            .Where(p => p.Value > 1)
            .Count();

        Console.WriteLine($"Number of overlapping points: {numberOfOverlaps}");
    }

    public void Part2()
    {
        var numberOfOverlaps = File.ReadAllLines("inputs/05_01.txt")
            .Select(line => ReadLine(line))
            .SelectMany(line => LineToPoints(line))
            .Aggregate(new Dictionary<Point, int>(), (d, p) => {
                d[p] = 1 + (d.ContainsKey(p) ? d[p] : 0);
                return d;
            })
            .Where(p => p.Value > 1)
            .Count();

        Console.WriteLine($"Number of overlapping points: {numberOfOverlaps}");
    }
}
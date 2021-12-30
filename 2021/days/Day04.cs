public class Day04 
{
    private List<int[,]> boards = new List<int[,]>();
    private List<bool[,]> marks = new List<bool[,]>();
    private Queue<int> selections = new Queue<int>();
    private List<int> remainingBoards = new List<int>();
    private Stack<int> winningBoards = new Stack<int>();

    public void ReadBoards (string[] lines)
    {
        for(var i = 2; i < lines.Length; i+=6)
        {
            var board = new int[5, 5];
            for(var j = 0; j < 5; j++)
            {
                for(var k = 0; k < 5; k++)
                {
                    board[j,k] = Int32.Parse(lines[j + i].Substring(k*3, 2));
                }
            }

            boards.Add(board);
            marks.Add(new bool[5,5]);
        }
    }

    public void MarkBoards(int selection)
    {
        for (var b = 0; b < boards.Count; b++)
        {
            for(var i = 0; i < 5; i++)
            {
                for(var j = 0; j < 5; j++)
                {
                    if (boards[b][i,j] == selection)
                    {
                        marks[b][i,j] = true;
                    }
                }
            }
        }
    }

    public int? GetWinningBoard()
    {
        for (var b = 0; b < boards.Count; b++)
        {
            for (var i = 0; i < 5; i++)
            {
                if (DidWin(b))
                {
                    return b;
                }
            }
        }

        return null;
    }

    public bool DidWin(int board)
    {
        for (var i = 0; i < 5; i++)
        {
            // Check horizontal
            if (marks[board][i, 0] &&
                marks[board][i, 1] && 
                marks[board][i, 2] && 
                marks[board][i, 3] &&
                marks[board][i, 4])
            {
                return true;
            }

            // Check vertical
            if (marks[board][0, i] &&
                marks[board][1, i] && 
                marks[board][2, i] && 
                marks[board][3, i] &&
                marks[board][4, i])
            {
                return true;
            }
        }
        return false;
    }

    public void RemoveWinningBoards()
    {
        for (var i = 0; i < remainingBoards.Count; i++)
        {
            if (DidWin(remainingBoards[i]))
            {
                winningBoards.Push(remainingBoards[i]);
                remainingBoards.RemoveAt(i);
                i--;
            }
        }
    }

    public int CalculateBoardScore(int b)
    {
        int total = 0;
        for (var i = 0; i < 5; i++)
        {
            for (var j = 0; j < 5; j++)
            {
                if (!marks[b][i,j])
                {
                    total += boards[b][i,j];
                }
            }
        }
        return total;
    }

    public void Part1()
    {
        var lines = File.ReadAllLines("inputs/04_01.txt");
        int? winner = null;

        selections = new Queue<int>(lines[0] 
            .Split(',')
            .Select(num => Int32.Parse(num)));
        
        ReadBoards(lines);

        while (winner == null)
        {
            var selection = selections.Dequeue();
            MarkBoards(selection);
            winner = GetWinningBoard();

            if (winner != null)
            {
                var score = CalculateBoardScore(winner.Value) * selection;
                Console.WriteLine($"First winner score: {score}");
            }
        }
    }

    public void Part2()
    {
        var lines = File.ReadAllLines("inputs/04_01.txt");
        selections = new Queue<int>(lines[0] 
            .Split(',')
            .Select(num => Int32.Parse(num)));
        
        ReadBoards(lines);
        remainingBoards = Enumerable.Range(0, boards.Count).ToList();

        foreach (var selection in selections)
        {
            MarkBoards(selection);
            RemoveWinningBoards();

            if (remainingBoards.Count == 0)
            {
                var score = CalculateBoardScore(winningBoards.Peek()) * selection;
                Console.WriteLine($"Last remaining board score: {score}");
                break;
            }
        }
    }
}
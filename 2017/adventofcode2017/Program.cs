using System;
using System.Collections.Generic;
using adventofcode2017.days.day20;

namespace adventofcode2017
{
    class Program
    {
        static void Main(string[] args)
        {
            var d20 = new Day20();

            Console.WriteLine($"Particle remaining closest to origin: {d20.GetParticleThatStaysClosestToOrigin()}");
            Console.WriteLine($"Number of particles after collisions: {d20.GetNumParticlesAfterCollisions()}");
        }
    }
}

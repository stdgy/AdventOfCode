
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace adventofcode2017.days.day20
{
    public class Vector 
    {
        public int X { get; set; }
        public int Y { get; set; }
        public int Z { get; set; }
    }
    public class Particle 
    {
        public Vector Position { get; set; } 
        public Vector Velocity { get; set; }
        public Vector Acceleration { get; set; }
    }

    public class Day20 
    {
        private IEnumerable<Particle> _particles;

        private Vector ParseVector (string input, int start)
        {
            var startIndex = input.IndexOf('<', start)+1;
            var endIndex = input.IndexOf('>', start);
            var coords = input.Substring(startIndex, 
                endIndex - startIndex);
            var numbers = coords.Split(',')
                .Select(num => Int32.Parse(num))
                .ToList();
            
            return new Vector () {
                X = numbers[0],
                Y = numbers[1],
                Z = numbers[2]
            };
        }

        private Vector ParsePosition (string input)
        {
            var start = input.IndexOf("p=")+2;
            return ParseVector(input, start);
        }

        private Vector ParseVelocity (string input)
        {
            var start = input.IndexOf("v=")+2;
            return ParseVector(input, start);
        }

        private Vector ParseAcceleration (string input)
        {
            var start = input.IndexOf("a=")+2;
            return ParseVector(input, start);
        }

        private List<int> GetLinearSolution (int b, int c)
        {
            var intersection = new List<int>();

            if (c == 0)
            {
                intersection.Add(0);
                return intersection;
            }
            // Check whether evenly divisible 
            if ((c % b) != 0)
            {
                return intersection;
            }
            if ((-c)/b > 0)
            {
                intersection.Add((-c)/b);
            }
            return intersection;
        }

        private List<int> GetQuadraticRoots (int a, int b, int c)
        {
            var discriminant = (b*b) - (4*a*c);
            var roots = new List<int>();

            if (discriminant < 0)
            {
                return roots;
            }
            
            if (discriminant == 0)
            {
                // Check if evenly divisible. If not,
                // throw an exception.
                if (b % (2*a) != 0)
                {
                    return roots;
                }
                roots.Add(-(b / (2*a)));
            } else 
            {
                // Check whether discriminant is a perfect square.
                // If it isn't, then I'm not sure we'll find an 
                // integer match?
                var sqrt = (int)Math.Floor(Math.Sqrt(discriminant));
                if ((sqrt*sqrt) != discriminant)
                {
                    return roots;
                }
                // Check whether we get an evenly divisible result
                var posRemainder = (-b + sqrt) % (2*a);
                var negRemainder = (-b - sqrt) % (2*a);

                if (posRemainder == 0)
                {
                    roots.Add( (-b + sqrt) / (2*a) );
                }
                if (negRemainder == 0)
                {
                    roots.Add( (-b - sqrt) / (2*a) );
                }
            }
            
            return roots
                .Where(r => r >= 0)
                .ToList();
        }

        private int GetQuadraticResult (int a, int b, int c, int t)
        {
            return (a * (t * t)) + (2*b + a)*t + (2*c);
        }

        private bool AreQuadraticsEqual(int a1, int b1, int c1, 
            int a2, int b2, int c2, int t)
        {
            // Check whether quadratics are equal at time t
            // (1/2)at^2 + abt + c
            // at^2 + 2abt + 2c
            var r1 = GetQuadraticResult(a1, b1, c1, t);
            var r2 = GetQuadraticResult(a2, b2, c2, t);
            
            return GetQuadraticResult(a1, b1, c1, t) == GetQuadraticResult(a2, b2, c2, t);
        }

        private bool DoParticlesIntersect(Particle a, Particle b)
        {
            /* 
                First figure out whether and when they intersect along a singe 
                dimension.

                This breaks down into a recurrence relation:
                p(t) = p(t-1) + v(t)
                p(0) = p0
                Where
                v(t) = v(t-1) + a0
                v(0) = v0 + a0
                Bang your head against the wall for awhile and you wind up with:
                p(t) = p0 + (v0)t + (t(t+1)/2)a0 
                Combine terms and do some multiplication to get rid of the fracts:
                p(t) = 2p0 + (a0 + 2(v0))t + (a0)t^2

                Now we have an equation that will give us the position of a particle
                at time t. 

                To figure out whether 2 particles intersect, we can subtract one particle's
                position equation from another (along the same axis) and then solve
                for the roots (The inputs required to make the equation 0).
             */

            var aX = a.Acceleration.X - b.Acceleration.X;
            var bX = (2*a.Velocity.X + a.Acceleration.X) - (2*b.Velocity.X + b.Acceleration.X);
            var cX = (2*a.Position.X) - (2*b.Position.X);
            var intersections = new List<int>();

            if (aX == 0 && bX == 0)
            {
                intersections.Add(0);
            }
            else if (aX == 0)
            {
                intersections = GetLinearSolution(bX, cX);
            }
            else 
            {
                intersections = GetQuadraticRoots(aX, bX, cX);
            }
            
            if (intersections.Count == 0)
                return false;

            // Try to see whether they intersect along the other dimensions
            // at the same time.
            foreach (var intersection in intersections)
            {
                var y1 = GetQuadraticResult(a.Acceleration.Y, a.Velocity.Y, a.Position.Y, intersection);
                var z1 = GetQuadraticResult(a.Acceleration.Z, a.Velocity.Z, a.Position.Z, intersection);
                var y2 = GetQuadraticResult(b.Acceleration.Y, b.Velocity.Y, b.Position.Y, intersection);
                var z2 = GetQuadraticResult(b.Acceleration.Z, b.Velocity.Z, b.Position.Z, intersection);

                if (y1 == y2 && z1 == z2)
                {
                    return true;
                }
            }
            return false;
        }

        public Day20 ()
        {
            _particles = File.ReadAllLines("inputs/day20/input.txt")
                .Select(line => new Particle() {
                    Position = ParsePosition(line),
                    Velocity = ParseVelocity(line),
                    Acceleration = ParseAcceleration(line)
                });
        }

        public int GetParticleThatStaysClosestToOrigin ()
        {
            // As t heads towards infinity, don't I only care about
            // the smallest acceleration? 

            return _particles
                .Select(particle => Math.Abs(particle.Acceleration.X) +
                    Math.Abs(particle.Acceleration.Y) + 
                    Math.Abs(particle.Acceleration.Z))
                .Select((sum,index) => new {
                    Sum = sum, 
                    ParticleNum = index 
                })
                .OrderBy(s => s.Sum)
                .First()
                .ParticleNum;
        }

        public int GetNumParticlesAfterCollisions ()
        {
            // I 'think' we can speed up this process by finding
            // the potential points of intersection between
            // each particle, rather than brute force iterating
            // through each time step.

            return _particles.Count() - _particles
                .Where((p1, i) => _particles
                    .Where((p2, j) => i != j)
                    .Any(p2 => DoParticlesIntersect(p1,p2)))
                .Count();
        }
    }
}
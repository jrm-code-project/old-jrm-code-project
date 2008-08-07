using System;
using System.Collections.Generic;

namespace BHCTest
{
    static class Utility
    {
        public static double LogPi = Math.Log (Math.PI);

        static double [] LanczosCoefficients = {
                                                 57.156235665862923517,
                                                -59.597960355475491248,
                                                14.136097974741747174,
                                                -0.49191381609762019978,
                                                .33994649984811888699e-4,
                                                .46523628927048575665e-4,
                                                -.98374475304879564677e-4,
                                                .15808870322491248884e-3,
                                                -.21026444172410488319e-3,
                                                .21743961811521264320e-3,
                                                -.16431810653676389022e-3,
                                                .84418223983852743293e-4,
                                                -.26190838401581408670e-4,
                                                .36899182659531622704e-5
                                                };

        public static double LogGamma (double xx)
        {
            if (xx > 0) {
                if (xx < 1.0) {
                    // reflection
                    double OneMinusX = 1.0 - xx;

                    return (LogPi + Math.Log (OneMinusX))
                        - (LogGamma (1.0 + OneMinusX)
                           + Math.Log (Math.Sin (Math.PI * OneMinusX)));
                }
                else {
                    double g = 607.0 / 128.0;
                    double sum = 0.99999999999999709182;

                    int i = 1;
                    foreach (double coefficient in LanczosCoefficients)
                        sum += coefficient / (xx + i++);
                    double tmp = xx + g + .5;
                    return (.5 * Math.Log (Math.PI * 2))
                        + Math.Log (sum / xx)
                        - tmp
                        + ((xx + .5) * Math.Log (tmp));
                }
            }
            else
                throw new ArgumentOutOfRangeException ("xx", xx, "Parameter is not positive.");
        }

        public static double Gamma (double x)
        {
            return Math.Exp (LogGamma (x));
        }

        public static double TenLogTen (double x)
        {
            return Math.Log10 (x) * 10.0;
        }

        public static double BernoulliBeta (double alpha, double beta, int heads, int tails)
        {
            return (Utility.Gamma (alpha + beta) * Utility.Gamma (alpha + heads) * Utility.Gamma (beta + tails))
                 / (Utility.Gamma (alpha) * Utility.Gamma (beta) * Utility.Gamma (alpha + beta + heads + tails));
        }

        public static double Product<ElementType> (Func<ElementType, double> f, IList<ElementType> elements)
        {
            double answer = 1;
            foreach (ElementType element in elements)
                answer *= f (element);
            return answer;
        }
    }

    static class Globals
    {
        public static double Alpha = 1.0;
        public static double Beta = 1.0;
        public static IList<string> AllTerms = new List<string> { "brown", "fox", "quick", "rabbit", "rest", "run" };

        public static double TuningAlpha = 1.0;

    }

    abstract class Cluster
    {
        public abstract int CountOccurrances (string term);

        double TermWeight (string term)
        {
            int present = this.CountOccurrances (term);
            int absent = this.N () - present;
            return Utility.BernoulliBeta (Globals.Alpha, Globals.Beta, present, absent);
        }

        protected double PH1 ()
        {
            return Utility.Product<string> (this.TermWeight, Globals.AllTerms);
        }

        public abstract int N ();
        public abstract double D ();
        public abstract double Pi ();
        public abstract double P ();

    }

    class DataPoint : Cluster
    {
        IList<string> terms;

        public DataPoint (IList<string> terms)
        {
            this.terms = terms;
        }

        public override int N ()
        {
            return 1;
        }

        public override int CountOccurrances (string term)
        {
            return terms.Contains (term) ? 1 : 0;
        }

        public override double D ()
        {
            return Globals.TuningAlpha;
        }

        public override double Pi ()
        {
            return 1.0;
        }

        public override double P ()
        {
            return PH1 ();
        }

    }

    class Tree : Cluster
    {
        Cluster leftChild;
        Cluster rightChild;

        public Tree (Cluster leftChild, Cluster rightChild)
        {
            this.leftChild = leftChild;
            this.rightChild = rightChild;
        }

        public override int N ()
        {
            return leftChild.N () + rightChild.N ();
        }

        public override int CountOccurrances (string term)
        {
            return leftChild.CountOccurrances (term) + rightChild.CountOccurrances (term);
        }

        public override double D ()
        {
            return Globals.TuningAlpha * Utility.Gamma (N ()) + leftChild.D () * rightChild.D ();
        }

        public override double Pi ()
        {
            return Globals.TuningAlpha * Utility.Gamma (N ()) / D ();
        }

        public override double P ()
        {
            double pi_k = Pi ();
            return pi_k * PH1 () + ((1.0 - pi_k) * leftChild.P () * rightChild.P ());
        }

        public double R ()
        {
            return (Pi () * PH1 ()) / P ();
        }
    }

    class Program
    {
        // sample data
        
        static DataPoint dp0 = new DataPoint (new List<string> {"brown", "fox", "quick"});
        static DataPoint dp1 = new DataPoint (new List<string> {"quick", "rabbit", "run"});
        static DataPoint dp2 = new DataPoint (new List<string> {"rabbit", "run"});
        static DataPoint dp3 = new DataPoint (new List<string> {"rabbit", "rest"});
        static DataPoint dp4 = new DataPoint (new List<string> {"quick", "rabbit", "run"});

        static void Main (string [] args)
        {
            Tree c1 = new Tree (dp0, dp1);
            Tree c2 = new Tree (dp1, dp2);
            Tree c3 = new Tree (c2, dp3);

            Console.Out.WriteLine (c1.R());
            Console.Out.WriteLine (c2.R());
            Console.Out.WriteLine (c3.R());
            return;
        }
    }
}

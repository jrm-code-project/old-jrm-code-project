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

        public static double ProbabilityToLogOdds (double p)
        {
            return TenLogTen (ProbabilityToOdds (p));
        }

        public static double ProbabilityToOdds (double p)
        {
            return p / (1.0 - p);
        }


        public static Func<double, double> P2O =
         delegate (double p)
        {
            return p / (1.0 - p);
        };

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

        //public double ROdds ()
        //{
        //    // return p => (p / (1.0 - p)) ((Pi () * PH1 ()) / P ());
        //    // return ((Func<double,double>) (p => (p / (1.0 - p)))) ((Pi () * PH1 ()) / P ());
        //    //return ((Func<double, double>) delegate (double p) { return p / (1.0 - p); }) ((Pi () * PH1 ()) / P ());
        //    // return ((Pi () * PH1 ()) / P ()) / (1.0 - (Pi () * PH1 ()) / P ());
        //    double t0 = PH1 ();
        //    // double t1 = Pi ();
        //    //double t1 = (this is DataPoint)
        //    //    ? 1
        //    //    : Globals.TuningAlpha * Utility.Gamma (N ()) / D ();
        //    //double t1 = (this is DataPoint)
        //    //    ? 1
        //    //    : Globals.TuningAlpha * Utility.Gamma (N ())
        //    //        / ((this is DataPoint)
        //    //            ? Globals.TuningAlpha
        //    //            : Globals.TuningAlpha * Utility.Gamma (N ()) 
        //    //               + ((Tree)this).leftChild.D () * ((Tree)this).rightChild.D ());
        //    double t1 =  Globals.TuningAlpha * Utility.Gamma (N ())
        /// ( Globals.TuningAlpha * Utility.Gamma (N ())
        //       + ((Tree) this).leftChild.D () * ((Tree) this).rightChild.D ());

        //    // double pi_k = Pi ();
        //    //double t2 = (this is DataPoint) 
        //    //    ? PH1() 
        //    //    : t1 * PH1 () + ((1.0 - t1) * ((Tree)this).leftChild.P () * ((Tree)this).rightChild.P ());
        //    double t2 = t1 * PH1 () + ((1.0 - t1) * ((Tree) this).leftChild.P () * ((Tree) this).rightChild.P ());

        //    return (t1 * t0 / t2) / (1.0 - (t1 * t0 / t2));
        //}

        public abstract double Px ();
        public double P1a () 
        {      
            return Globals.TuningAlpha * Utility.Gamma (N ()) * PH1 ();
        }

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

 
        public override double Px ()
        {
            return P1a();
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
            return Pi () * PH1 () / P ();
        }

        public double ROdds1 ()
        {
            double p;
            return ((p = (Pi () * PH1 ()) / P ()) == 0.0 || true) ? 
                        
                p / (1.0 - p) : (double) 0.0;
        }

        public double ROdds ()
        {
            // return p => (p / (1.0 - p)) ((Pi () * PH1 ()) / P ());
            // return ((double p) => (p / (1.0 - p))) (Pi () * PH1 () / P ());
            // return ((Func<double,double>) (p => (p / (1.0 - p)))) ((Pi () * PH1 ()) / P ());
            //return ((Func<double, double>) delegate (double p) { return p / (1.0 - p); }) ((Pi () * PH1 ()) / P ());
            // return ((Pi () * PH1 ()) / P ()) / (1.0 - (Pi () * PH1 ()) / P ());
            // double t0 = PH1 ();
            // double t1 = Pi ();
            //double t1 = (this is DataPoint)
            //    ? 1
            //    : Globals.TuningAlpha * Utility.Gamma (N ()) / D ();
            //double t1 = this is DataPoint
            //    ? 1
            //    : Globals.TuningAlpha * Utility.Gamma (N ())
            //        / ((this is DataPoint)
            //            ? Globals.TuningAlpha
            //            : Globals.TuningAlpha * Utility.Gamma (N ())
            //               + ((Tree) this).leftChild.D () * ((Tree) this).rightChild.D ());
            // double t3 = Globals.TuningAlpha * Utility.Gamma (N ());
            //double t1 =  Globals.TuningAlpha * Utility.Gamma (N ())
            //            / (Globals.TuningAlpha * Utility.Gamma (N ())
            //               + leftChild.D () * rightChild.D ());
            //double t1 =  t3 / (t3 + leftChild.D () * rightChild.D ());

            // double pi_k = Pi ();
            //double t2 = (this is DataPoint) 
            //    ? PH1() 
            //    : t1 * PH1 () + ((1.0 - t1) * ((Tree)this).leftChild.P () * ((Tree)this).rightChild.P ());
            //double t4 = t1 * t0;
            // double t2 = t4 + ((1.0 - t1) * leftChild.P () * rightChild.P ());

            // return (t1 * t0 / t2) / (1.0 - (t1 * t0 / t2));
            // return (t4 / t2) / (1.0 - t4 / t2);
            // return t4 /((1.0 - t1) * leftChild.P () * rightChild.P ());
            //return t4 / ((leftChild.P () * rightChild.P ())
            //              - (t1 * leftChild.P () * rightChild.P ()));
            //return (t4 / t1) / (((leftChild.P () * rightChild.P ())
            //              - (t1 * leftChild.P () * rightChild.P ())) / t1);
            // return t0 / ((leftChild.P () * rightChild.P ()) / (t3 / (t3 + leftChild.D () * rightChild.D ())) - leftChild.P () * rightChild.P ());
 
            //return t0 / (leftChild.P () * rightChild.P () * (t3 + leftChild.D () * rightChild.D ()) / t3
            //             - leftChild.P () * rightChild.P ());
            // return (t0 * t3)/ (t3 * leftChild.P () * rightChild.P () * ((t3 + leftChild.D () * rightChild.D ()) / t3 - 1));
            // return (t0 * t3) / (leftChild.P () * rightChild.P () * leftChild.D () * rightChild.D ());
            //return Globals.TuningAlpha * Utility.Gamma (N ()) * PH1 ()
            //        / (leftChild.Px () * rightChild.Px ());


           return P1a () / (leftChild.Px () * rightChild.Px ());
        }

        public override double Px ()
        {
            return P1a() + leftChild.Px () * rightChild.Px ();
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

            Console.Out.WriteLine (Utility.P2O);
            Console.Out.WriteLine (Utility.P2O (c1.R()));
            Console.Out.WriteLine (c1.ROdds1 ());
            Console.Out.WriteLine (Utility.ProbabilityToOdds (c2.R()));
            Console.Out.WriteLine (c2.ROdds ());
            Console.Out.WriteLine (Utility.ProbabilityToOdds (c3.R()));
            Console.Out.WriteLine (c3.ROdds ());
            return;
        }
    }
}

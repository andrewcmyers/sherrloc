package util;

/*************************************************************************
 *  Compilation:  javac Harmonic.java
 *  Execution:    java Harmonic N
 *  
 *  Prints the Nth harmonic number: 1/1 + 1/2 + ... + 1/N.
 * 
 *  % java Harmonic 10
 *  2.9289682539682538
 *
 *  % java Harmonic 10000
 *  9.787606036044348
 *
 *************************************************************************/

public class Harmonic { 

   // return Nth Harmonic number
   public static double H(int N) {
       if (N < 1000) return Hsmall(N);
       else          return Hlarge(N);
   }

   // compute Nth Harmonic number when N is small
   public static double Hsmall(int N) {
       double sum = 0.0;
       for (int i = 1; i <= N; i++)
           sum += 1.0 / i;
       return sum;
   }

   // compute Nth Harmonic number when N is large
   public static double Hlarge(int N) {

       // Euler-Mascheroni constant (http://en.wikipedia.org/wiki/Euler-Mascheroni_constant)
       double GAMMA = 0.577215664901532;

       return Math.log(N) + GAMMA + 1.0/(2.0*N) - 1.0/(12.0*N*N) + 1.0/(120.0*N*N*N*N);
   }
}


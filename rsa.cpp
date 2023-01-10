/*===========================================================================*/
/*                      Homework 3: RSA - Adan Gandarilla                    */
/*===========================================================================*/
/*
 * Here is my solution for problems 3-5
 * I have an explanation for each section
 * All test cases are in the main function
 * The primes generated are 512 bits, or about 150 digits
 * After multiplication, we get numbers over 300 digits
 * I'm using Boost library for these large number operations
 */

#include<iostream>
#include<random>
#include<boost/multiprecision/cpp_int.hpp>
#include<boost/random.hpp>
#include<tuple>
using namespace std;
using boost::multiprecision::cpp_int;
cpp_int mod_pow (cpp_int, cpp_int, cpp_int);


cpp_int modulo (const cpp_int a, const cpp_int b) {
     if (b < 0)
          return modulo(-a, -b);
     
     const cpp_int result = a % b;
     if (result >= 0)
          return result;

     return result + b;
}


/*===========================================================================*/
/*                            Problem 3                                      */
/*===========================================================================*/
/*
 * This is implementation of euclid alogrithm.
 * parameters A and B are to keep track of GCD part
 * parameters X and Y are to keep track of the coeficients 
 * at the end, the gcd will be stored in B, and the inverse is Y(mod M):
 * Y*A + X*B = 1 or Y * A = 1(mod B)
 * Both values are returned in a std::pair
 * the wanted value will be returned by functions gcd() and  mod_inv()
 */

pair<cpp_int, cpp_int> euclid (const cpp_int a,
                               const cpp_int b,
                               const cpp_int x,
                               const cpp_int y,
                               const cpp_int m)
{
     cpp_int r = a % b;

     if (r == 0)
     {
          cpp_int abs = b;
          if (abs < 0)
               abs *= -1;

          pair<cpp_int, cpp_int> p = make_pair<cpp_int, cpp_int>(0, 0);
          p.first = abs;
          p.second = modulo(y, m);
          return p;
     }
        
     return euclid(b, r, y, (x - (y * (a / b))), m);
}

cpp_int gcd (const cpp_int a, const cpp_int b)
{
     pair<cpp_int, cpp_int> p = euclid(a, b, 1, 0, b);
     return p.first;
}

cpp_int mod_inv (const cpp_int a, const cpp_int b)
{
     pair<cpp_int, cpp_int> p = euclid(a, b, 1, 0, b);
     return p.second;
}


/*===========================================================================*/
/*                            Problem 4                                      */
/*===========================================================================*/
/*
 * This is implementation of prime checking alogrithm.
 * is_prime_helper() takes a prime candidate P and a random testing number TEST
 * and applies Fermat test to make a prediction
 * is_prime() will take a prime candidate P and a amplify value
 * it will run prime_helper() on P and a random number below P
 * and will return true if is_prime_helper returns true AMPLIFY times.
 * Fermat's test has a probability of being wrong of <= 0.5
 * Since I'm amplifying 25 times, the probability is just (1/2)^25
 * This is really good, and still pretty fast too.
 */

bool is_prime_helper (const cpp_int p, const cpp_int test)
{
     cpp_int the_gcd = gcd(test, p);
     cpp_int pow = mod_pow(test, (p - 1) / 2, p);
     return the_gcd <= 1 && (pow == 1 || pow == p - 1);
}

bool is_prime (const cpp_int p, const int amplify = 1)
{
     srand(time(NULL));
     
     if (p == 2)
     {
          return true;
     }
     else if (p < 2 || p % 2 == 0)
     {
          return false;
     }
     else
     {
          for (int x = 0; x < amplify; x++)
          {
               const cpp_int test = cpp_int{ (rand() % (p - 1)) + 1 };
               if (not is_prime_helper(p, test))
               {
                    return false;
               }
          }
          return true;
     }
}


/*===========================================================================*/
/*                            Problem 5                                      */
/*===========================================================================*/
/*
 * Using all the functions above, we can solve problem 5
 * and implement the entire RSA algorithm
 * The encryption and decryption will only work
 * if all of the functions above are working correctly
 * The function for exponents is mod_pow()
 * and below is the generation for 2 large primes
 */

cpp_int mod_pow (const cpp_int base, const cpp_int exp, const cpp_int mod)
{
     cpp_int ans = 1;
     cpp_int tempExp = exp;
     cpp_int tempBase = base;

     while (tempExp > 0)
     {
          if (tempExp % 2 != 0)
          {
               ans = modulo((ans * tempBase), mod);
               tempExp = tempExp - 1;
          }
          else
          {
               tempBase = modulo((tempBase * tempBase), mod);
               tempExp = tempExp / 2;
          }
     }
     return ans;
}


cpp_int gen_odd ()
{
     using boost::random::independent_bits_engine;
     using boost::random::mt19937;
     typedef independent_bits_engine<mt19937, 512, cpp_int> gen_type;
     gen_type gen(time(NULL));

     cpp_int random = gen();
     if (random % 2 == 0)
     {
          random += 1;
     }
     
     return random;
}

cpp_int gen_prime (const cpp_int other_than = -1) {
     cpp_int test = gen_odd();
     
     while (test == other_than ||
            !is_prime(test, 25))
     {
          test += 2;
     }
     
     return test;
}

cpp_int rsa_encrypt (const cpp_int msg, const cpp_int n, const cpp_int e)
{
     return mod_pow(msg, e, n);
}

cpp_int rsa_decrypt (const cpp_int cipher, const cpp_int n, const cpp_int d)
{
     return mod_pow(cipher, d, n);
}

int main ()
{
     cout << "PROBLEM 3 TESTS: GCD AND INVERSE" << endl;
     cout << "--------------------------------" << endl;
     cout << "GCD(3, 26): " << gcd(3, 26) << endl;
     cout << "INV(3, 26): " << mod_inv(3, 26) << endl;
     cout << "GCD(5, 30): " << gcd(5, 30) << endl;
     cout << "INV(5, 30): " << mod_inv(5, 30) << endl;
     cout << "GCD(31, 3480): " << gcd(31, 3480) << endl;
     cout << "INV(31, 3480): " << mod_inv(31, 3480) << endl << endl << endl;


     cout << "PROBLEM 4 TESTS: PRIMES < 1000  " << endl;
     cout << "--------------------------------" << endl;
     for (int x = 0; x < 1000; x++)
     {
          if (is_prime(x))
               cout << x << " ";
     }
     cout << endl << endl << endl;

     
     cout << "PROBLEM 5 TESTS: RSA EXAMPLE    " << endl;
     cout << "--------------------------------" << endl;
     const cpp_int p = gen_prime();
     cout << "P: " << p << endl << endl;
     const cpp_int q = gen_prime(p);
     cout << "Q: " << q << endl << endl;
     const cpp_int n = p * q;
     cout << "N: " << n << endl << endl;
     const cpp_int e = 65537;
     cout << "E: " << e << endl << endl;
     const cpp_int phi_n = (p - 1) * (q - 1);
     cout << "phi(N): " << phi_n << endl << endl;
     const cpp_int d = mod_inv(e, phi_n);
     cout << "D: " << d << endl << endl;
     const cpp_int msg = 1234;
     cout << "MESSAGE: "<< msg << endl << endl;
     const cpp_int cipher = rsa_encrypt(msg, n, e);
     cout << "CIPHER: " << cipher << endl << endl;
     const cpp_int plain = rsa_decrypt(cipher, n, d);
     cout << "PLAINTEXT: " << plain << endl << endl;
    
     return 0;
}

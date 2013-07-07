#include <stdio.h>
#include <iostream>
#include <fstream>
#include <sstream>
#include <chrono>
#include <vector>
#include <climits>
#include <boost/algorithm/string.hpp>
#include <boost/functional/hash.hpp>
#include <boost/tokenizer.hpp>
#include <boost/random/mersenne_twister.hpp>
#include <boost/random/uniform_int_distribution.hpp>

using namespace std;
using boost::tokenizer;
using boost::hash;

#define PRIME 109297


/*
 * lsh_docs infile outfile nhashes
*/
int main( int argc, char ** argv ) {

	ifstream infile(argv[1]);
	ofstream outfile(argv[2]);
  string line;
	int nhashes = atoi(argv[3]);
	
	vector< string > minstr(nhashes);
	boost::hash<std::string> string_hash;

  boost::random::mt19937 gen;
  boost::random::uniform_int_distribution<> dista(0,PRIME);
  boost::random::uniform_int_distribution<> distb(0,PRIME-1);
  vector< int > a(nhashes);
  vector< int > b(nhashes);

	
  auto t1 = chrono::high_resolution_clock::now();

  for( int h = 0; h<nhashes; ++h )
  {
    a[h] = dista(gen);
    b[h] = distb(gen);
  }

	while( getline(infile,line) ) {

    tokenizer<> tok(line);
    vector< long int > minhashes(nhashes,PRIME);

    for( tokenizer<>::iterator feat=tok.begin(); feat!=tok.end(); ++feat )
    {
      size_t hf = string_hash(*feat);
      // do something with *feat
      for( int h=0; h<nhashes; ++h )
      {
        long int f = (hf*a[h] + b[h]) % PRIME;
        if( minhashes[h] > f )
        {
          minhashes[h] = f;
          minstr[h] = *feat;
        }
      }
    }

    int h;
    for(h=0; h<nhashes-1; ++h )
    {
      outfile << minstr[h] << "\t";
    }
    outfile << minstr[h] << endl;
  }

	auto t2 = chrono::high_resolution_clock::now();
  cout << nhashes << " - LSH_RAND time = " << chrono::duration_cast<chrono::milliseconds>(t2-t1).count() << " ms" << endl;

  return 0;
}

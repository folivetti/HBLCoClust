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

  auto t1 = chrono::high_resolution_clock::now();

	while( getline(infile,line) ) {

    tokenizer<> tok(line);
    vector< long int > minhashes(nhashes,PRIME);

    for( tokenizer<>::iterator feat=tok.begin(); feat!=tok.end(); ++feat )
    {
      // do something with *feat
      size_t hf = atoi(feat->c_str());
      long int k1=(200*hf)%PRIME; // magic arbitrary number
      long int k2=(11+hf)%PRIME;
      for( long int i=k1,h=0; h<nhashes; i+=k2,++h )
      {
        long int f = (k1+i%PRIME)%PRIME;
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
  cout << nhashes << " - LSH_ITER time = " << chrono::duration_cast<chrono::milliseconds>(t2-t1).count() << " ms" << endl;

  return 0;
}

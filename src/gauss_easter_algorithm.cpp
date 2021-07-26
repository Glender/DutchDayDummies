#include <Rcpp.h>
#include <string>

using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
std::string gauss_easter_algorithm_Cpp(int year){
    // Calculate easter day based on a year
    int golden_nr = (year % 19) + 1;
    int century = ((year - (year % 100)) / 100) + 1;
    int X = (((3*century) - ((3*century) %4 )) / 4) - 12;
    
    int metonic = (((8*century+5)-((8*century+5)%25))/25) - 5;
    int sunday = (((5*year)-((5*year) % 4)) / 4) - X - 10;
    int epacta = (11*golden_nr+20+metonic-X) % 30;
    
    if(epacta==24 | epacta==25 & golden_nr>11){
        epacta += 1;
    }
    int N = 44 - epacta;
    if(N < 21) N += 30;
    int P = (N + 7) - ((sunday+N) % 7);
    
    int month = 3;
    if (P > 31){
        P -= 31;
        month += 1;
    }
    // output format is yyyy-mm-dd
    std::string easter_date = to_string(year) + "-"
     + to_string(month) + "-" + to_string(P);  

    return easter_date;
}
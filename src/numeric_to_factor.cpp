#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <string>
using namespace Rcpp;

std::vector<double> _sort_actuals = std::vector<double>();

// Sort into the *permutation* that would give the correct shuffle
bool sortshuffle (int i, int j) {
  return (_sort_actuals[i] < _sort_actuals[j]);
}

// [[Rcpp::export]]
CharacterVector numeric_to_factor(NumericVector num,
    CharacterVector levs, bool na_to_missing = true) {
  CharacterVector charnums = CharacterVector(num.size());
  const int nlevs = levs.size();

  CharacterVector ranged_levels = CharacterVector();
  CharacterVector other_levels = CharacterVector();
  for (int j = 0; j < nlevs; j++) {
    bool other = false;
    for (unsigned int k = 0; k < strlen(levs[j]); k++ ) {
      if (!(isdigit(levs[j][k]) || levs[j][k] == ',' || levs[j][k] == '.' ||
             levs[j][k] == '(' || levs[j][k] == ')' || levs[j][k] == '[' ||
             levs[j][k] == ']' || levs[j][k] == '-' || levs[j][k] == ' ')) {
        other = true;
        other_levels.push_back(levs[j]);
        break;
      }
    }
    if (!other) ranged_levels.push_back(levs[j]);
  }

  if (ranged_levels.size() == 0) {
    for (int row = 0; row < num.size(); row++) charnums[row] = num[row];
    return charnums;
  }

  const int nrlevs = ranged_levels.size();
  std::vector<double> lefts, rights;      // left/right bounds on ranges
  std::vector<bool> leftinc, rightinc; // left/right inclusive 

  CharacterVector clean_ranged_levels = CharacterVector(nrlevs);
  for (int j = 0; j < nrlevs; j++) {
    std::string tmp = "";
    for (int k = 0; k < strlen(ranged_levels[j]); k++)
      if (ranged_levels[j][k] != ' ') tmp += ranged_levels[j][k];
    clean_ranged_levels[j] = tmp;
  }

  // Compute right and left bounds from ranges
  for (int j = 0; j < nrlevs; j++) {
    if (clean_ranged_levels[j][0] != '[' && clean_ranged_levels[j][0] != '(') {
      // Assume this is just a number
      lefts.push_back(atof(clean_ranged_levels[j]));
      //lefts[j] = atof(clean_ranged_levels[j]);
      rights.push_back(lefts[j]);
      //rights[j] = lefts[j];
      //leftinc[j] = rightinc[j] = true;
      leftinc.push_back(true); rightinc.push_back(true);
      continue;
    }
      
    int levsize = clean_ranged_levels[j].size();
    leftinc.push_back(clean_ranged_levels[j][0] == '[');
    rightinc.push_back(clean_ranged_levels[j][levsize - 1] == ']');
    //leftinc[j] = clean_ranged_levels[j][0] == '[';
    //rightinc[j] = clean_ranged_levels[j][levsize - 1] == ']';
    // Find the comma
    int comma; for (comma = 0; comma < levsize &&
                   clean_ranged_levels[j][comma] != ','; comma++);
    if (comma == levsize) {
      BEGIN_RCPP
      throw (Rcpp::exception("numeric_to_factor did not find a comma in expected range level", "numeric_to_factorC.cpp", 51));
      END_RCPP 
    }
    lefts.push_back(atof( ((std::string)(clean_ranged_levels[j])).substr(1, comma - 1).c_str() ));
    rights.push_back(atof( ((std::string)(clean_ranged_levels[j])).substr(comma + 1, (levsize - 1) - (comma + 1)).c_str() ));
    // lefts[j] = atof( ((std::string)(clean_ranged_levels[j])).substr(1, comma - 1).c_str() );
    // rights[j] = atof( ((std::string)(clean_ranged_levels[j])).substr(comma + 1, (levsize - 1) - (comma + 1)).c_str() );
  } // Right & lefts bounds and inclusivity booleans have been set

  // Optimization trick: sort lefts, remembering the order
  _sort_actuals = std::vector<double>();
  std::vector<int> sorted_indices = std::vector<int>();
  for (int k = 0; k < nrlevs; k++) {
    _sort_actuals.push_back(lefts[k]);
    sorted_indices.push_back(k);
  }
  std::sort(sorted_indices.begin(), sorted_indices.end(), sortshuffle);

  // Now actually restore the levels for each num
  int h, cur;
  for (int row = 0; row < num.size(); row++) {
    for (cur = 0; cur < nrlevs; cur++) {
      h = sorted_indices[cur];
      if (leftinc[h] ? num[row] < lefts[h] : num[row] <= lefts[h]) {
        break;
      }
    }
    if (cur == 0) {
      charnums[row] = na_to_missing ? (String)"Missing" : NA_STRING;
      continue;
    }
    h = sorted_indices[cur - 1]; // back up, we went too far!
    if (rightinc[h] ? num[row] <= rights[h] : num[row] < rights[h]) {
      charnums[row] = ranged_levels[h]; 
    } else charnums[row] = na_to_missing ? (String)"Missing" : NA_STRING;
  }

  return charnums;
}


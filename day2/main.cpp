#include <print>
#include <sstream>
#include <vector>
#include <iostream>
#include <ranges>
#include <algorithm>

namespace sr = std::ranges;
namespace sv = sr::views;

using Report = std::vector<int>;

auto readInput() {
  struct {
    std::vector<Report> reports;
  } res;

  std::string s;
  while (std::getline(std::cin, s)) {
    res.reports.push_back({});
    std::istringstream ss { s };
    int d;
    while (ss >> d) {
      res.reports.back().push_back(d);
    }
  }

  return res;
}

using P = std::pair<int, int>;
static constexpr auto lessThanOrEq = [](P const& p) {
  return p.first <= p.second;
};

static constexpr auto grtThanOrEq = [](P const& p) {
  return p.first >= p.second;
};

bool isDecrOrIncr(sr::range auto&& rep) {
  if (rep.size() <= 1) 
    return true;

  // without first element
  auto const wof = sr::subrange(rep.begin() + 1, rep.end());
  // all pairs (A[i], A[i + 1]) w/ i > 1
  auto const rest = sv::zip(wof, wof | sv::drop(1));

  if (rep[0] < rep[1]) 
    return sr::all_of(rest, lessThanOrEq);
  else if (rep[0] > rep[1]) 
    return sr::all_of(rest, grtThanOrEq);
  else
    return isDecrOrIncr(wof);
}

bool hasLowLevelDiff(sr::range auto&& rep) {
  auto r = sv::zip(rep, rep | sv::drop(1));
  return sr::all_of(r, [](P const& p) {
    int d = std::abs(p.second - p.first);
    return d >= 1 && d <= 3;
  });
}

bool isSafe(Report const& rep) {
  return isDecrOrIncr(rep) && hasLowLevelDiff(rep);
}

int main(int argc, char** argv)
{
  auto [ reports ] = readInput();
  std::println("{}", sr::count_if(reports, isSafe));

  return 0;
}

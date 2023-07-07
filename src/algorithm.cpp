#include "algorithm.hpp"

void topological_sort_visit(
    size_t i,
    std::vector<size_t> &result,
    std::vector<int> &marked,
    const std::vector<std::vector<size_t>> &dependencies);

std::vector<size_t>
topological_sort(const std::vector<std::vector<size_t>> &dependencies)
{
  std::vector<size_t> result;
  std::vector<int> marked(dependencies.size(), -1);

  for (auto i = 0; i < dependencies.size(); i++) {
    topological_sort_visit(i, result, marked, dependencies);
  }

  return std::vector(result.rbegin(), result.rend());
}

void topological_sort_visit(
    size_t i,
    std::vector<size_t> &result,
    std::vector<int> &marked,
    const std::vector<std::vector<size_t>> &dependencies)
{
  auto &mark = marked[i];
  if (mark == 0) {
    throw "graph has a cycle";
  } else if (mark == 1) {
    return;
  }

  mark = 0;

  for (auto &target : dependencies[i]) {
    topological_sort_visit(target, result, marked, dependencies);
  }

  mark = 1;
  result.push_back(i);
}
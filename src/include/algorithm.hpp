#pragma once

#include <stddef.h>
#include <utility>
#include <vector>

std::vector<size_t>
topological_sort(const std::vector<std::vector<size_t>> &dependencies);
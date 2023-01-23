// NOTE: compile with C++20

#include <algorithm>
#include <fstream>
#include <limits>
#include <span>
#include <vector>

constexpr auto kInputFile = "inputs/day9.txt";
constexpr int kPremableSize = 25;

bool IsPermutable(int buffer[kPremableSize], int n) {
  for (int i = 0; i < kPremableSize; ++i) {
    // i and j cannot be the same (problem description: must pick two different
    // numbers from the premable)
    for (int j = i + 1; j < kPremableSize; ++j) {
      if (buffer[i] + buffer[j] == n) {
        return true;
      }
    }
  }
  return false;
}

int main() {
  std::vector<int> inputs;
  {
    std::ifstream ifs(kInputFile);
    if (!ifs)
      return -1;
    while (ifs) {
      int n;
      ifs >> n;
      inputs.push_back(n);
    }
  }

  int buffer[kPremableSize];

  for (int i = 0; i < kPremableSize; ++i) {
    buffer[i] = inputs[i];
  }
  int invalidNumber = -1;
  {
    printf("===== Part 1 =====\n");

    for (int n : std::span(inputs.begin() + kPremableSize, inputs.end())) {
      if (!IsPermutable(buffer, n)) {
        invalidNumber = n;
        printf("Found invalid entry: %d\n", n);
        goto part1End;
      }

      // Put the new number at the end of buffer
      std::rotate(std::begin(buffer), std::begin(buffer) + 1, std::end(buffer));
      buffer[kPremableSize - 1] = n;
    }
    printf("Nothing found\n");
  part1End:;
  }

  for (int i = 0; i < kPremableSize; ++i) {
    buffer[i] = inputs[i];
  }
  {
    printf("===== Part 2 =====\n");

    // I read the requirement wrong, it wasn't trying to find contigous range of
    // invalid numbers, it's finding a contigous range that sums to the invalid
    // number
#if 0
    struct Sequence {
      int startIdx = -1;
      int endIdx = -1;
    };
    std::vector<Sequence> foundSequences;
    Sequence currSeq;

    int i = 0;
    for (int n : std::span(inputs.begin() + kPremableSize, inputs.end())) {
      if (!IsPermutable(buffer, n)) {
        if (currSeq.startIdx == -1) {
          currSeq.startIdx = i;
        }
      } else {
        if (currSeq.startIdx != -1 && currSeq.endIdx == -1) {
          currSeq.endIdx = i;
          foundSequences.push_back(currSeq);
          currSeq = {};
        }
      }

      std::rotate(std::begin(buffer), std::begin(buffer) + 1, std::end(buffer));
      buffer[kPremableSize - 1] = n;

      ++i;
    }

    for (auto &seq : foundSequences) {
      printf("- Sequence [%d, %d)\n", seq.startIdx, seq.endIdx);
      printf("  ");
      int min = std::numeric_limits<int>::max();
      int max = 0;
      for (int i = seq.startIdx; i < seq.endIdx; ++i) {
        int n = inputs[i];
        printf("%d, ", n);
        min = std::min(min, n);
        max = std::max(max, n);
      }
      printf("\n");
      printf("  min: %d, max: %d\n", min, max);
    }
#endif

    // Given [a1, a2, a3, ..., an] with length n
    // Construct [0, a1, a1 + a2, ..., a1 + a2 + ... + an] with length n + 1
    // (i.e. the nth element in prefix sum array is the sum of all elements before n,
    //       the last element of the prefix sum array is the sum of all elements)
    // This allows summing on any [i, j) interval with simply prefixSum[j] - prefixSum[i]
    // where i and j are indices into the original array
    std::vector<int> prefixSums;
    prefixSums.reserve(inputs.size() + 1);
    int currPrefixSum = 0;
    for (int n : inputs) {
      prefixSums.push_back(currPrefixSum);
      currPrefixSum += n;
    }
    prefixSums.push_back(currPrefixSum);

    for (int i = 0; i < inputs.size(); ++i) {
      // Requirement: the range must be at least two elements long
      for (int j = i + 2; j < inputs.size(); ++j) {
        // [i, j)
        int sum = prefixSums[j] - prefixSums[i];
        if (sum == invalidNumber) {
          int min = std::numeric_limits<int>::max();
          int max = 0;
          for (int idx = i; idx < j; ++idx) {
            int n = inputs[idx];
            min = std::min(min, n);
            max = std::max(max, n);
          }
          printf("Found range: [%d, %d) with min=%d, max=%d, min+max=%d\n", i, j, min, max, min + max);
        }
      }
    }
  }

  return 0;
}

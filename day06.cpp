#include <string>
#include <vector>
#include <fstream>
#include <bitset>
#include <cstdlib>

constexpr int kAnswerCount = 27;

struct PersonResponse {
  std::string answerText;
  std::bitset<kAnswerCount> answers;
};

struct Form {
  std::vector<PersonResponse> persons;
};

int main() {
  std::vector<Form> forms;
  {
    std::ifstream ifs("inputs/day6.txt");
    if (!ifs) return -1;

    Form currForm;
    std::string line;
    while (std::getline(ifs, line)) {
      if (line.empty()) {
        // Separator between forms
        //printf("Form+ ans: %s\n", currForm.answers.to_string().c_str());
        forms.push_back(std::move(currForm));
        currForm = {};
        //break;
      } else {
        // Another person's response
        PersonResponse pr;
        for (char c : line) {
          int idx = c - 'a';
          //printf("%d ", idx);
          pr.answers.set(idx);
        }
        //printf("\n");
        //printf("+ line: %s\n", line.c_str());
        //printf("  ans: %s\n", pr.answers.to_string().c_str());

        pr.answerText = std::move(line);
        line.clear();

        currForm.persons.push_back(std::move(pr));
        pr = {};
      }
    }
    forms.push_back(std::move(currForm));
  }

  // ======== Part 1 ========
  printf("======== Part 1 ========\n");
  int sum = 0;
  for (auto& form : forms) {
    // printf("-- %s\n", form.answers.to_string().c_str());
    std::bitset<kAnswerCount> answers;
    for (auto& pr : form.persons) {
      answers |= pr.answers;
    }
    sum += answers.count();
  }
  printf("Sum of 'yes' counts: %d\n", sum);

  // ======== Part 2 ========
  printf("======== Part 2 ========\n");
  int sum2 = 0;
  for (auto& form : forms) {
    std::bitset<kAnswerCount> answers;
    answers.set(); // Set all bits to 1

    for (auto& pr : form.persons) {
      answers &= pr.answers;
    }
    sum2 += answers.count();
  }
  printf("Sum of 'yes' counts: %d\n", sum2);
}

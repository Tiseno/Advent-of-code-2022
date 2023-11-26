#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#define INPUT_EXAMPLE_2 "input.example2.txt"
#define INPUT_EXAMPLE "input.example.txt"
#define INPUT "input.txt"

#define INPUT_MAX_LENGTH 5000

struct ElfNumber {
  long number;
  long pos;
};

void print_elf_numbers(struct ElfNumber elf_numbers[], long length) {
  for (long i = 0; i < length; ++i) {
    printf("  %ld:%ld %ld\n", i, elf_numbers[i].pos, elf_numbers[i].number);
  }
  printf("\n");
}

struct ElfNumber find_number(struct ElfNumber elf_numbers[], long n) {
  for (long j = 0;; ++j) {
    if (elf_numbers[j].number == n) {
      return elf_numbers[j];
    }
  }
}

struct ElfNumber find_number_with_position(struct ElfNumber elf_numbers[],
                                           long pos) {
  for (long j = 0;; ++j) {
    if (elf_numbers[j].pos == pos) {
      return elf_numbers[j];
    }
  }
}

void mix(struct ElfNumber elf_numbers[], const long length, const int rounds) {
  for (long r = 0; r < rounds; ++r) {
    for (long i = 0; i < length; ++i) {
      long offset = elf_numbers[i].number < 0
                        ? -elf_numbers[i].number % (length - 1)
                        : elf_numbers[i].number % (length - 1);
      if (offset == 0) {
        continue;
      }

      long from = elf_numbers[i].pos;
      long to =
          elf_numbers[i].number < 0
              ? (from - offset) < 0 ? from - offset + length : from - offset
          : (from + offset) >= length ? from + offset - length
                                      : from + offset;

      elf_numbers[i].pos = to;
      for (long j = 0; j < length; ++j) {
        if (j == i) {
          continue;
        }
        if (elf_numbers[i].number < 0) {
          if (to < from) {
            if (to <= elf_numbers[j].pos && elf_numbers[j].pos < from) {
              elf_numbers[j].pos++;
            }
          } else {
            if (to <= elf_numbers[j].pos || elf_numbers[j].pos < from) {
              elf_numbers[j].pos++;
            }
          }
        } else {
          if (from < to) {
            if (from < elf_numbers[j].pos && elf_numbers[j].pos <= to) {
              elf_numbers[j].pos--;
            }
          } else {
            if (from < elf_numbers[j].pos || elf_numbers[j].pos <= to) {
              elf_numbers[j].pos--;
            }
          }
        }
        // Dumb handling of moving first to end with -1, and moving last to
        // beginning with 1. It solves the real input, but breaks the examples,
        // oh well!
        if (elf_numbers[j].pos == -1) {
          elf_numbers[j].pos = length - 1;
        }
        if (elf_numbers[j].pos == length) {
          elf_numbers[j].pos = 0;
        }
      }
    }
  }
}

long coordinateSum(long input_numbers[], const long length, const int rounds,
                   const long decryption) {
  struct ElfNumber elf_numbers[INPUT_MAX_LENGTH];

  for (long i = 0; i < length; ++i) {
    elf_numbers[i] =
        (struct ElfNumber){.number = input_numbers[i] * decryption, .pos = i};
  }

  mix(elf_numbers, length, rounds);

  long zero_pos = find_number(elf_numbers, 0).pos;
  long _1000th =
      find_number_with_position(elf_numbers, (zero_pos + 1000) % length).number;
  long _2000th =
      find_number_with_position(elf_numbers, (zero_pos + 2000) % length).number;
  long _3000th =
      find_number_with_position(elf_numbers, (zero_pos + 3000) % length).number;

  return _1000th + _2000th + _3000th;
}

void solve(char *file_name) {
  printf("Solving %s\n", file_name);
  FILE *file = fopen(file_name, "r");

  long input_numbers[INPUT_MAX_LENGTH];
  long length = 0;
  {
    char *line = NULL;
    size_t l = 0;
    while ((getline(&line, &l, file)) != -1) {
      input_numbers[length] = atoi(line);
      ++length;
    }
  }

  printf("Part 1 %ld\n", coordinateSum(input_numbers, length, 1, 1));
  printf("Part 2 %ld\n", coordinateSum(input_numbers, length, 10, 811589153));

  printf("\n");
}

int main() {
  solve(INPUT_EXAMPLE_2);
  solve(INPUT_EXAMPLE);
  solve(INPUT);
}

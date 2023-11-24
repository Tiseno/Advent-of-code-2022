#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

#define N_ROCKS_TO_CALCULATE 1000000000000

#define CHAMBER_WIDTH 7
#define CHAMBER_HEIGHT 20000
#define JET_PATTERN_LENGTH 10093
#define TOP_SIZE 240

#define MAX_PATTERNS 20000

#define N_SHAPES 5
#define SHAPE_SIZE 4
char shapes[N_SHAPES][SHAPE_SIZE] = {
    {15, 0, 0, 0}, {2, 7, 2, 0}, {7, 4, 4, 0}, {1, 1, 1, 1}, {3, 3, 0, 0},
};
char shape_heights[N_SHAPES] = {1, 3, 3, 4, 2};
char shape_widths[N_SHAPES] = {4, 3, 3, 1, 2};

void print_row(char row) {
  printf("|");
  for (int x = 0; x < CHAMBER_WIDTH; ++x) {
    if (row & (1 << x)) {
      printf("%c", '#');
    } else {
      printf("%c", '.');
    }
  }
  printf("|");
}

void print_shape(char shape[SHAPE_SIZE], int x) {
  print_row(shape[2] << x);
  printf("\n");
  print_row(shape[1] << x);
  printf("\n");
  print_row(shape[0] << x);
  printf("\n");
}

void print_chamber(char chamber[CHAMBER_HEIGHT], int highest_point) {
  for (int y = highest_point; y >= 0; y--) {
    print_row(chamber[y]);
    if (y % 100 == 0) {
      printf(" 100");
    }
    printf("\n");
  }
}

bool row_overlap(char chamber_row, char shape_row, int x) {
  if (x < 0) {
    return (chamber_row & (shape_row >> -x)) > 0;
  } else {
    return (chamber_row & (shape_row << x)) > 0;
  }
}

bool shape_overlap(char chamber_row[CHAMBER_HEIGHT], char shape[SHAPE_SIZE],
                   int x, int y) {
  return row_overlap(chamber_row[y + 0], shape[0], x) ||
         row_overlap(chamber_row[y + 1], shape[1], x) ||
         row_overlap(chamber_row[y + 2], shape[2], x) ||
         row_overlap(chamber_row[y + 3], shape[3], x);
}

void settle(char chamber[CHAMBER_HEIGHT], char shape[SHAPE_SIZE], int x,
            int y) {
  if (x < 0) {
    chamber[y + 0] |= (shape[0] >> -x);
    chamber[y + 1] |= (shape[1] >> -x);
    chamber[y + 2] |= (shape[2] >> -x);
    chamber[y + 3] |= (shape[3] >> -x);
  } else {
    chamber[y + 0] |= (shape[0] << x);
    chamber[y + 1] |= (shape[1] << x);
    chamber[y + 2] |= (shape[2] << x);
    chamber[y + 3] |= (shape[3] << x);
  }
}

struct Pattern {
  long long r;
  int highest;
  char shape_index;
  int jet_index;
  char tower_top[TOP_SIZE];
};

int main() {
  // FILE *file = fopen("example.input.txt", "r");
  FILE *file = fopen("input.txt", "rb");

  char chamber[CHAMBER_HEIGHT] = {0};
  struct Pattern chamber_patterns[MAX_PATTERNS] = {0};
  char jet_pattern[JET_PATTERN_LENGTH] = {0};

  int jet_bytes = fread(jet_pattern, 1, JET_PATTERN_LENGTH - 1, file);
  int jet_pattern_length = jet_bytes - 1; // remove newline

  int highest = 0;
  long long extra_cycle_height = 0;

  int found_pattern = 0;

  int jet_index = 0;
  long long r = 0;
  for (; r < N_ROCKS_TO_CALCULATE; ++r) {
    char shape_index = r % N_SHAPES;
    char *shape = shapes[shape_index];
    char shape_height = shape_heights[shape_index];
    char shape_width = shape_widths[shape_index];

    int x = 2;
    int y = highest + 3;
    if (y > CHAMBER_HEIGHT) {
      printf("Warning: out of bounds!\n");
      return 1;
    }

    while (true) {
      int nx = x;
      int ny = y;

      if (jet_pattern[jet_index] == '<') {
        nx--;
      } else {
        nx++;
      }
      jet_index = (jet_index + 1) % jet_pattern_length;

      if (nx < 0) {
        nx = 0;
      } else if (nx + shape_width > 7) {
        nx = x;
      } else if (shape_overlap(chamber, shape, nx, ny)) {
        nx = x;
      }

      x = nx;
      ny = ny - 1;
      if (ny < 0 || shape_overlap(chamber, shape, nx, ny)) {
        break;
      }
      y = ny;
    }

    settle(chamber, shape, x, y);
    highest = highest > y + shape_height ? highest : y + shape_height;

    if (found_pattern == 0) {
      struct Pattern current_pattern = {.r = r,
                                        .highest = highest,
                                        .shape_index = shape_index,
                                        jet_index = jet_index};
      for (int i = 0; i < TOP_SIZE && highest - i >= 0; ++i) {
        current_pattern.tower_top[i] = chamber[highest - i];
      }
      chamber_patterns[r] = current_pattern;

      for (int cp = 0; cp < r; ++cp) {
        if (chamber_patterns[cp].shape_index == current_pattern.shape_index &&
            chamber_patterns[cp].jet_index == current_pattern.jet_index) {
          bool same = true;
          for (int t = 0; t < TOP_SIZE; ++t) {
            if (chamber_patterns[cp].tower_top[t] !=
                current_pattern.tower_top[t]) {
              same = false;
            }
          }
          if (same) {
            found_pattern = 1;
            long long cycle_length = r - chamber_patterns[cp].r;
            int height_diff = highest - chamber_patterns[cp].highest;
            long long needed_cycles = (N_ROCKS_TO_CALCULATE - r) / cycle_length;
            r += needed_cycles * cycle_length;
            extra_cycle_height = needed_cycles * height_diff;
            break;
          }
        }
      }
    }
  }

  printf("%lld rocks have fallen\n", r);
  printf("Simulated tower height %d\n", highest);
  printf("Cycles height %lld\n", extra_cycle_height);
  printf("Total height %lld\n", highest + extra_cycle_height);
  return 0;
}

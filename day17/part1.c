#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

#define N_ROCKS 2022
#define CHAMBER_WIDTH 7

#define CHAMBER_HEIGHT 5265
#define JET_PATTERN_LENGTH 10093

// Napkin complexity:
// CHAMBER_HEIGHT * N_ROCKS = O(C * N)
// Should be good enough

#define N_SHAPES 5
#define SHAPE_SIZE 4
short shapes[N_SHAPES][SHAPE_SIZE] = {
    {15, 0, 0, 0}, {2, 7, 2, 0}, {7, 4, 4, 0}, {1, 1, 1, 1}, {3, 3, 0, 0},
};

short shape_heights[N_SHAPES] = {1, 3, 3, 4, 2};

void print_row(short row) {
  printf("|");
  for (int x = 0; x < CHAMBER_WIDTH; ++x) {
    if (row & (1 << x)) {
      printf("%c", '#');
    } else {
      printf("%c", '.');
    }
  }
  printf("|\n");
}

void print_shape(short shape[SHAPE_SIZE], int x) {
  print_row(shape[2] << x);
  print_row(shape[1] << x);
  print_row(shape[0] << x);
}

void print_chamber(short chamber[CHAMBER_HEIGHT], int highest_point) {
  for (int y = highest_point; y >= 0; y--) {
    print_row(chamber[y]);
  }
}

bool row_overlap(short chamber_row, short shape_row, int x) {
  if (x < 0) {
    return (chamber_row & (shape_row >> -x)) > 0;
  } else {
    return (chamber_row & (shape_row << x)) > 0;
  }
}

bool shape_overlap(short chamber_row[CHAMBER_HEIGHT], short shape[SHAPE_SIZE],
                   int x, int y) {
  return row_overlap(chamber_row[y + 0], shape[0], x) ||
         row_overlap(chamber_row[y + 1], shape[1], x) ||
         row_overlap(chamber_row[y + 2], shape[2], x) ||
         row_overlap(chamber_row[y + 3], shape[3], x);
}

void settle(short chamber[CHAMBER_HEIGHT], short shape[SHAPE_SIZE], int x,
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

int main() {
  // FILE *file = fopen("example.input.txt", "r");
  FILE *file = fopen("input.txt", "rb");

  // TODO make this char instead of short
  short chamber[CHAMBER_HEIGHT] = {0};
  for (int i = 0; i < CHAMBER_HEIGHT; ++i) {
    chamber[i] = 128;
  }
  char jet_pattern[JET_PATTERN_LENGTH] = {0};

  int jet_bytes = fread(jet_pattern, 1, JET_PATTERN_LENGTH - 1, file);
  int jet_pattern_length = jet_bytes - 1; // remove newline

  int highest = 0;

  int jet_index = 0;
  int r = 0;
  for (; r < N_ROCKS; ++r) {
    short *shape = shapes[r % N_SHAPES];
    short shape_height = shape_heights[r % N_SHAPES];
    int x = 2;
    int y = highest + 3;

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
  }

  int print_h = highest + 3 > 25 ? highest + 3 : 25;
  print_chamber(chamber, print_h);
  printf("\n%d rocks have fallen\n", r);
  printf("\n%d\n", highest);
  return 0;
}

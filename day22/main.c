#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define FILE_NAME "input.txt"

FILE *openFile(const char *pathname) {
  FILE *file = fopen(pathname, "r");

  if (file == NULL) {
    printf("Could not open file %s", pathname);
    exit(EXIT_FAILURE);
  }

  return file;
}

struct _Size {
  size_t width;
  size_t height;
};
typedef struct _Size Size;

Size readWidthAndHeight() {
  FILE *file = openFile(FILE_NAME);
  char *line = NULL;
  size_t allocated = 0;
  size_t read;

  size_t width = 0;
  size_t height = 0;

  while ((read = getline(&line, &allocated, file)) != -1) {
    if (!strcmp(line, "\n")) {
      break;
    }
    size_t w = read - 1;
    if (w > width) {
      width = w;
    }
    height++;
  }

  Size a = {
      .width = width,
      .height = height,
  };
  return a;
}

typedef char *Matrix;

size_t idx(size_t x, size_t y, Size s) { return y * s.width + x; }

Matrix readMatrix(Size s) {
  size_t byteSize = s.width * s.height * sizeof(char);
  Matrix m = malloc(byteSize);
  memset(m, ' ', byteSize);

  FILE *file = openFile(FILE_NAME);
  char *line = NULL;
  size_t allocated = 0;
  size_t read;

  size_t y = 0;
  while ((read = getline(&line, &allocated, file)) != -1) {
    if (!strcmp(line, "\n")) {
      break;
    }

    for (size_t x = 0; x < s.width && x < read - 1; x++) {
      m[idx(x, y, s)] = line[x];
    }
    y++;
  }

  return m;
}

void printMatrix(Matrix m, Size s) {
  for (size_t y = 0; y < s.height; y++) {
    for (size_t x = 0; x < s.width; x++) {
      printf("%c", m[y * s.width + x]);
    }
    printf("\n");
  }
  printf("\n");
}

void printMatrixShape(Matrix m, Size s) {
  for (size_t y = 0; y < s.height; y++) {
    for (size_t x = 0; x < s.width; x++) {
      if (m[y * s.width + x] == ' ') {
        printf("  ");
      } else {
        printf("OO");
      }
    }
    printf("\n");
  }
  printf("\n");
}

enum _Turn { TurnLeft = -1, TurnRight = -2 };
typedef enum _Turn Turn;

struct _Point {
  int x;
  int y;
};
typedef struct _Point Point;

struct _Direction {
  struct _Direction *left;
  struct _Direction *right;
  char ch;
  Point vec;
};
typedef struct _Direction Direction;
Direction Up = {.ch = '^', .vec = {.x = 0, .y = -1}};
Direction Down = {.ch = 'v', .vec = {.x = 0, .y = 1}};
Direction Left = {.ch = '<', .vec = {.x = -1, .y = 0}};
Direction Right = {.ch = '>', .vec = {.x = 1, .y = 0}};

enum DIRECTION { UP, DOWN, LEFT, RIGHT };

struct _Path {
  size_t length;
  int *steps;
};
typedef struct _Path Path;

int readInt(char *line, int *number) {
  char buffer[100];
  size_t i = 0;
  for (; isdigit(line[i]); i++) {
    buffer[i] = line[i];
  }
  (*number) = atoi(buffer);
  return i;
}

Path readPath() {
  FILE *file = openFile(FILE_NAME);
  char *line = NULL;
  size_t allocated = 0;
  size_t read;

  while ((read = getline(&line, &allocated, file)) != -1) {
    if (!strcmp(line, "\n")) {
      break;
    }
  }

  read = getline(&line, &allocated, file);
  if (read == -1) {
    exit(EXIT_FAILURE);
  }

  int *steps = malloc(0);
  size_t length = 0;
  for (size_t i = 0; line[i] != '\0' && line[i] != '\n';) {
    // Reallocate every insert ftw
    steps = realloc(steps, sizeof(int) * (length + 1));
    if (isdigit(line[i])) {
      int number;
      size_t read = readInt(line + i, &number);
      steps[length] = number;
      i += read;
    } else {
      steps[length] = line[i] == 'L' ? TurnLeft : TurnRight;
      i++;
    }
    length++;
  }

  Path path = {
      .length = length,
      .steps = steps,
  };
  return path;
}

Point findStart(Matrix matrix, Size s) {
  for (size_t y = 0; y < s.height; y++) {
    for (size_t x = 0; x < s.width; x++) {
      if (matrix[idx(x, y, s)] == '.') {
        Point start = {
            .x = x,
            .y = y,
        };
        return start;
      }
    }
  }
  exit(EXIT_FAILURE);
}

int lastInRow(Matrix matrix, Size s, int y) {
  for (size_t x = s.width - 1; x >= 0; x--) {
    if (matrix[idx(x, y, s)] != ' ') {
      return x;
    }
  }
  exit(EXIT_FAILURE);
}

int firstInRow(Matrix matrix, Size s, int y) {
  for (size_t x = 0; x < s.width; x++) {
    if (matrix[idx(x, y, s)] != ' ') {
      return x;
    }
  }
  exit(EXIT_FAILURE);
}

int lastInCol(Matrix matrix, Size s, int x) {
  for (size_t y = s.height - 1; y >= 0; y--) {
    if (matrix[idx(x, y, s)] != ' ') {
      return y;
    }
  }
  exit(EXIT_FAILURE);
}

int firstInCol(Matrix matrix, Size s, int x) {
  for (size_t y = 0; y < s.width; y++) {
    if (matrix[idx(x, y, s)] != ' ') {
      return y;
    }
  }
  exit(EXIT_FAILURE);
}

Point move(Matrix matrix, Size s, Point pos, Direction dir) {
  Point npos;
  npos.x = pos.x + dir.vec.x;
  npos.y = pos.y + dir.vec.y;
  if (npos.x < 0) {
    npos.x = lastInRow(matrix, s, npos.y);
  } else if (npos.y < 0) {
    npos.y = lastInCol(matrix, s, npos.x);
  } else if (npos.x > s.width) {
    npos.x = firstInRow(matrix, s, npos.y);
  } else if (npos.y > s.height) {
    npos.y = firstInCol(matrix, s, npos.x);
  }

  if (matrix[idx(npos.x, npos.y, s)] == ' ') {
    if (dir.ch == Left.ch) {
      npos.x = lastInRow(matrix, s, npos.y);
    } else if (dir.ch == Right.ch) {
      npos.x = firstInRow(matrix, s, npos.y);
    } else if (dir.ch == Up.ch) {
      npos.y = lastInCol(matrix, s, npos.x);
    } else {
      npos.y = firstInCol(matrix, s, npos.x);
    }
  }

  if (matrix[idx(npos.x, npos.y, s)] == '#') {
    return pos;
  }

  matrix[idx(pos.x, pos.y, s)] = dir.ch;
  return npos;
}

struct _Result {
  Point position;
  Direction direction;
};

typedef struct _Result Result;

Result walkPath(Matrix matrix, Size s, Path path) {
  Point pos = findStart(matrix, s);
  Direction dir = Right;

  for (size_t i = 0; i < path.length; i++) {
    int step = path.steps[i];
    if (step == TurnLeft || step == TurnRight) {
      dir = step == TurnLeft ? (*dir.left) : (*dir.right);
    } else {
      for (size_t x = 0; x < step; x++) {
        pos = move(matrix, s, pos, dir);
      }
    }
  }
  Result result = {
      .position = pos,
      .direction = dir,
  };
  return result;
}

struct _Portal {
  // Where it leads
  Point point;
  // What direction we will face after going there
  Direction direction;
};
typedef struct _Portal Portal;

// A corner will have two portals
// Edges will only have one
struct _CubePoint {
  bool isFoldCorner;
  bool isEndCorner;
  bool isEdge;
  int iteration;
  bool turned;
  // Portals somewhere in the direction we are currently going
  // i.e. we exit this point into a void in a direct -> take the portal in that
  // direction
  Portal up;
  Portal down;
  Portal left;
  Portal right;
};

typedef struct _CubePoint CubePoint;

bool isEmpty(Matrix matrix, Size size, int x, int y) {
  return x < 0 || x >= size.width || y < 0 || y >= size.height ||
         matrix[idx(x, y, size)] == ' ';
}

int diagonalNeighbors(Matrix matrix, Size size, int x, int y) {
  return (isEmpty(matrix, size, x + 1, y + 1) ? 0 : 1) +
         (isEmpty(matrix, size, x + 1, y - 1) ? 0 : 1) +
         (isEmpty(matrix, size, x - 1, y - 1) ? 0 : 1) +
         (isEmpty(matrix, size, x - 1, y + 1) ? 0 : 1);
}

int neighbors(Matrix matrix, Size size, int x, int y) {
  return (isEmpty(matrix, size, x + 1, y) ? 0 : 1) +
         (isEmpty(matrix, size, x - 1, y) ? 0 : 1) +
         (isEmpty(matrix, size, x, y + 1) ? 0 : 1) +
         (isEmpty(matrix, size, x, y - 1) ? 0 : 1);
}

bool isFoldCorner(Matrix matrix, Size size, int x, int y) {
  return !isEmpty(matrix, size, x, y) &&
         diagonalNeighbors(matrix, size, x, y) == 3 &&
         neighbors(matrix, size, x, y) == 4;
}

bool isEndCorner(Matrix matrix, Size size, int x, int y) {
  return !isEmpty(matrix, size, x, y) &&
         diagonalNeighbors(matrix, size, x, y) == 1 &&
         neighbors(matrix, size, x, y) == 2;
}

bool isEdge(Matrix matrix, Size size, int x, int y) {
  return !isEmpty(matrix, size, x, y) &&
         (diagonalNeighbors(matrix, size, x, y) == 2 ||
          diagonalNeighbors(matrix, size, x, y) == 3) &&
         neighbors(matrix, size, x, y) == 3;
}

struct _CubeMap {
  CubePoint *matrix;
  Point *foldCorners;
  size_t numberOfFoldCorners;
};
typedef struct _CubeMap CubeMap;

CubeMap readCubeMap(Matrix matrix, Size size) {
  CubePoint *cubeMatrix = malloc(size.width * size.height * sizeof(CubePoint));
  const Point nullPoint = {.x = -1, .y = -1};
  const Portal nullPortal = {.point = nullPoint, .direction = Up};
  const CubePoint nullCPoint = {.isFoldCorner = false,
                                .isEndCorner = false,
                                .isEdge = false,
                                .iteration = -1,
                                .turned = false,
                                .up = nullPortal,
                                .down = nullPortal,
                                .left = nullPortal,
                                .right = nullPortal};

  for (size_t i = 0; i < size.width * size.height; i++) {
    cubeMatrix[i] = nullCPoint;
  }

  // 1. Find all positive corners
  // 	- All points on the map where there is only one non map corner neighbor:
  //
  //        ##
  // 	    ##
  //      ##o#
  //      ####
  //
  //  2. Create stitching from a corner
  //  	- Walk along edges in both directions
  //  	- Corners have two mappings
  //
  //            3 4
  //           2# #
  //
  // 	       1# #
  //    4 3 2 1
  //    # # # # o #
  //    # # # # # #
  //
  //  3. When two 270 degree corners meets the stitch ends
  //
  //  4. When all stitches created from starting at a 90 degree corner is made,
  //  there might be one stitch more to make (did not implement this as the real
  //  input did not need it)
  //
  //  5. We need to keep track of the new rotation after going over a stitch,
  //  when we go over an edge we always "turn" inwards from the edge
  //  	- Going over the edges "2", "4", and "6":
  //
  //            3 4
  //           2< ^5
  //
  // 	       1# >6
  //    4 3 2 1
  //   5v # v # o # ...
  //   6> # # # # # ...

  // There can at most exist 4 90 degree corners which we need to find
  size_t numberOfFoldCorners = 0;
  Point *foldCorners = malloc(4 * sizeof(Point));

  for (size_t y = 0; y < size.height; y++) {
    for (size_t x = 0; x < size.width; x++) {
      if (isFoldCorner(matrix, size, x, y)) {
        foldCorners[numberOfFoldCorners] = (Point){.x = x, .y = y};
        numberOfFoldCorners++;
      }
      Point point = {.x = x, .y = y};
      CubePoint cPoint = {
          .isFoldCorner = isFoldCorner(matrix, size, x, y),
          .isEndCorner = isEndCorner(matrix, size, x, y),
          .isEdge = isEdge(matrix, size, x, y),
          .iteration = -1,
          .turned = false,
          .up = nullPortal,
          .down = nullPortal,
          .left = nullPortal,
          .right = nullPortal,
      };
      cubeMatrix[idx(x, y, size)] = cPoint;
    }
  }

  // Fold all 90 degree corners
  //  - Walk in both directions until we hit two corners
  //  - Create portals between the paths
  for (size_t i = 0; i < numberOfFoldCorners; ++i) {
    long x = foldCorners[i].x;
    long y = foldCorners[i].y;
    long dx = isEmpty(matrix, size, x + 1, y + 1) ||
                      isEmpty(matrix, size, x + 1, y - 1)
                  ? 1
                  : -1;
    long dy = isEmpty(matrix, size, x + 1, y + 1) ||
                      isEmpty(matrix, size, x - 1, y + 1)
                  ? 1
                  : -1;

    Point da = {.x = dx, .y = 0};
    Point a = {.x = x + da.x, .y = y + da.y};

    Point db = {.x = 0, .y = dy};
    Point b = {.x = x + db.x, .y = y + db.y};

    while (true) {
      Direction direction_a; // The direction we will face after going here,
                             // saved in b
      if (da.y == 0) {       // We have a portal going up or down
        if (isEmpty(matrix, size, a.x,
                    a.y - 1)) { // If empty space is above, we should face down
                                // if we take a portal here
          direction_a = Down;
        } else {
          direction_a = Up;
        }
      } else { // We have a portal going left or right
        if (isEmpty(matrix, size, a.x + 1,
                    a.y)) { // If empty space is to the right, we should face
                            // left if we take a portal here
          direction_a = Left;
        } else {
          direction_a = Right;
        }
      }

      Direction direction_b; // The direction we will face after going here,
                             // saved in a
      if (db.y == 0) {       // We have a portal going up or down
        if (isEmpty(matrix, size, b.x,
                    b.y - 1)) { // If empty space is above, we should face down
                                // if we take a portal here
          direction_b = Down;
        } else {
          direction_b = Up;
        }
      } else { // We have a portal going left or right
        if (isEmpty(matrix, size, b.x + 1,
                    b.y)) { // If empty space is to the right, we should face
                            // left if we take a portal here
          direction_b = Left;
        } else {
          direction_b = Right;
        }
      }

      if (da.y == 0) { // We have a portal going up or down
        if (isEmpty(matrix, size, a.x,
                    a.y - 1)) { // If empty space is above, we have a portal up
          cubeMatrix[idx(a.x, a.y, size)].up = (Portal){
              .point = b,
              .direction = direction_b,
          };
        } else {
          cubeMatrix[idx(a.x, a.y, size)].down = (Portal){
              .point = b,
              .direction = direction_b,
          };
        }
      } else { // We have a portal going left or right
        if (isEmpty(matrix, size, a.x + 1,
                    a.y)) { // If empty space is to the right, we have a portal
                            // right
          cubeMatrix[idx(a.x, a.y, size)].right = (Portal){
              .point = b,
              .direction = direction_b,
          };
        } else {
          cubeMatrix[idx(a.x, a.y, size)].left = (Portal){
              .point = b,
              .direction = direction_b,
          };
        }
      }

      if (db.y == 0) { // We have a portal going up or down
        if (isEmpty(matrix, size, a.x,
                    b.y - 1)) { // If empty space is above, we have a portal up
          cubeMatrix[idx(b.x, b.y, size)].up = (Portal){
              .point = a,
              .direction = direction_a,
          };
        } else {
          cubeMatrix[idx(b.x, b.y, size)].down = (Portal){
              .point = a,
              .direction = direction_a,
          };
        }
      } else { // We have a portal going left or right
        if (isEmpty(matrix, size, b.x + 1,
                    b.y)) { // If empty space is to the right, we have a portal
                            // right
          cubeMatrix[idx(b.x, b.y, size)].right = (Portal){
              .point = a,
              .direction = direction_a,
          };
        } else {
          cubeMatrix[idx(b.x, b.y, size)].left = (Portal){
              .point = a,
              .direction = direction_a,
          };
        }
      }
      cubeMatrix[idx(a.x, a.y, size)].iteration = i;
      cubeMatrix[idx(b.x, b.y, size)].iteration = i;

      // If both are corners we cannot continue
      //  - In the input case, when this happens we have assigned all portals
      //  all around, so we only need to loop once really
      if (cubeMatrix[idx(a.x, a.y, size)].isEndCorner &&
          cubeMatrix[idx(b.x, b.y, size)].isEndCorner) {
        break;
      }

      // If we are a corner we want to turn the corner, i.e. change direction of
      // iteration but stay here
      if (cubeMatrix[idx(a.x, a.y, size)].isEndCorner &&
          cubeMatrix[idx(a.x, a.y, size)].turned ==
              false) { // End corner we have not turned yet
        cubeMatrix[idx(a.x, a.y, size)].turned = true;

        if (da.x == 0) { // We are going up or down
          if (isEmpty(
                  matrix, size, a.x + 1,
                  a.y)) { // If we have empty space to the right, we turn left
            da = (Point){.x = -1, y = 0};
          } else {
            da = (Point){.x = 1, y = 0};
          }
        } else { // We are going left or right
          if (isEmpty(matrix, size, a.x,
                      a.y + 1)) { // If we have empty space above, we turn down
            da = (Point){.x = 0, y = -1};
          } else {
            da = (Point){.x = 0, y = 1};
          }
        }
      } else {
        // Otherwise, advance in our direction
        a = (Point){.x = a.x + da.x, .y = a.y + da.y};
      }

      if (cubeMatrix[idx(b.x, b.y, size)].isEndCorner &&
          cubeMatrix[idx(b.x, b.y, size)].turned ==
              false) { // End corner we have not turned yet
        cubeMatrix[idx(b.x, b.y, size)].turned = true;
        if (db.x == 0) { // We are going up or down
          if (isEmpty(
                  matrix, size, b.x + 1,
                  b.y)) { // If we have empty space to the right, we turn left
            db = (Point){.x = -1, y = 0};
          } else {
            db = (Point){.x = 1, y = 0};
          }
        } else { // We are going left or right
          if (isEmpty(matrix, size, b.x,
                      b.y + 1)) { // If we have empty space below (negative
                                  // direction is ip), we turn upwards
            db = (Point){.x = 0, y = -1};
          } else {
            db = (Point){.x = 0, y = 1};
          }
        }
      } else {
        b = (Point){.x = b.x + db.x, .y = b.y + db.y};
      }
    }
  }

  CubeMap cubeMap = {.matrix = cubeMatrix,
                     .foldCorners = foldCorners,
                     .numberOfFoldCorners = numberOfFoldCorners};
  return cubeMap;
}

void printCubeMatrix(CubePoint *cubeMatrix, Size s) {
  for (size_t y = 0; y < s.height; y++) {
    for (size_t x = 0; x < s.width; x++) {

      if ((((cubeMatrix[idx(x, y, s)].up.point.x != -1) ? 1 : 0) +
           ((cubeMatrix[idx(x, y, s)].down.point.x != -1) ? 1 : 0) +
           ((cubeMatrix[idx(x, y, s)].left.point.x != -1) ? 1 : 0) +
           ((cubeMatrix[idx(x, y, s)].right.point.x != -1) ? 1 : 0)) > 1) {
        printf("¤");
      } else if (cubeMatrix[idx(x, y, s)].up.point.x != -1) {
        printf("^");
      } else if (cubeMatrix[idx(x, y, s)].down.point.x != -1) {
        printf("v");
      } else if (cubeMatrix[idx(x, y, s)].left.point.x != -1) {
        printf("<");
      } else if (cubeMatrix[idx(x, y, s)].right.point.x != -1) {
        printf(">");
      } else if (cubeMatrix[idx(x, y, s)].isFoldCorner) {
        printf("X");
      } else if (cubeMatrix[idx(x, y, s)].isEndCorner) {
        printf("@");
      } else if (cubeMatrix[idx(x, y, s)].isEdge) {
        printf("?");
      } else {
        printf(" ");
      }

      if (cubeMatrix[idx(x, y, s)].iteration != -1) {
        printf("%d", cubeMatrix[idx(x, y, s)].iteration);
      } else {
        printf(" ");
      }
    }
    printf("\n");
  }
  printf("\n");
}

struct MoveResult {
  Point pos;
  Direction dir;
};

struct MoveResult moveCube(Matrix matrix, Size size, Point pos, Direction dir,
                           CubeMap cubeMap) {
  Point npos;
  npos.x = pos.x + dir.vec.x;
  npos.y = pos.y + dir.vec.y;
  if (!isEmpty(matrix, size, npos.x, npos.y)) {
    if (matrix[idx(npos.x, npos.y, size)] == '#') {
      return (struct MoveResult){.pos = pos, .dir = dir};
    }
    matrix[idx(pos.x, pos.y, size)] = dir.ch;
    return (struct MoveResult){.pos = npos, .dir = dir};
  } else {
    struct MoveResult result;
    if (dir.ch == '^') {
      result = (struct MoveResult){
          .pos = cubeMap.matrix[idx(pos.x, pos.y, size)].up.point,
          .dir = cubeMap.matrix[idx(pos.x, pos.y, size)].up.direction,
      };
    }
    if (dir.ch == 'v') {
      result = (struct MoveResult){
          .pos = cubeMap.matrix[idx(pos.x, pos.y, size)].down.point,
          .dir = cubeMap.matrix[idx(pos.x, pos.y, size)].down.direction,
      };
    }
    if (dir.ch == '<') {
      result = (struct MoveResult){
          .pos = cubeMap.matrix[idx(pos.x, pos.y, size)].left.point,
          .dir = cubeMap.matrix[idx(pos.x, pos.y, size)].left.direction,
      };
    }
    if (dir.ch == '>') {
      result = (struct MoveResult){
          .pos = cubeMap.matrix[idx(pos.x, pos.y, size)].right.point,
          .dir = cubeMap.matrix[idx(pos.x, pos.y, size)].right.direction,
      };
    }

    if (matrix[idx(result.pos.x, result.pos.y, size)] == '#') {
      return (struct MoveResult){.pos = pos, .dir = dir};
    }
    matrix[idx(pos.x, pos.y, size)] = dir.ch;
    return result;
  }
}

Result walkCubePath(Matrix matrix, Size s, Path path, CubeMap cubeMap) {
  Point pos = findStart(matrix, s);
  Direction dir = Right;

  for (size_t i = 0; i < path.length; i++) {
    int step = path.steps[i];
    if (step == TurnLeft || step == TurnRight) {
      dir = step == TurnLeft ? (*dir.left) : (*dir.right);
    } else {
      for (size_t x = 0; x < step; x++) {
        struct MoveResult result = moveCube(matrix, s, pos, dir, cubeMap);
        pos = result.pos;
        dir = result.dir;
      }
    }
  }
  Result result = {
      .position = pos,
      .direction = dir,
  };
  return result;
}

int password(Result result) {
  return 1000 * (result.position.y + 1) + 4 * (result.position.x + 1) +
         (result.direction.ch == '>'   ? 0
          : result.direction.ch == 'v' ? 1
          : result.direction.ch == '<' ? 2
                                       : 3);
}

void printResult(Result result) {
  printf("Ended up at (%i, %i) direction %c\n", result.position.x,
         result.position.y, result.direction.ch);
  printf("The password is %i\n", password(result));
}

int main() {
  Up.left = &Left;
  Up.right = &Right;

  Right.left = &Up;
  Right.right = &Down;

  Down.left = &Right;
  Down.right = &Left;

  Left.left = &Down;
  Left.right = &Up;

  Size size = readWidthAndHeight();
  Matrix matrix = readMatrix(size);
  Path path = readPath();
  Result result = walkPath(matrix, size, path);

  printMatrix(matrix, size);
  printf("Part 1\n");
  printResult(result);
  // Somehow this program is non deterministic
  // it gives the correct answer most of the time but can also sometimes result
  // in 23502, 46226, 109350, 149022, 68290, 189162, 28530, 196102
  printf("\n");

  matrix = readMatrix(size);
  CubeMap cubeMap = readCubeMap(matrix, size);

  // printCubeMatrix(cubeMap.matrix, size);
  // printMatrix(matrix, size);
  // printMatrixShape(matrix, size);

  Result result2 = walkCubePath(matrix, size, path, cubeMap);
  printMatrix(matrix, size);
  printf("Part 2\n");
  printResult(result2);
  return 0;
}

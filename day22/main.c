#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define FILE_NAME "input.txt"

FILE * openFile(const char * pathname) {
	FILE * file = fopen(pathname, "r");

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
	FILE * file = openFile(FILE_NAME);
	char * line = NULL;
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

typedef char * Matrix;

size_t idx(size_t x, size_t y, Size s) {
	return y * s.width + x;
}

Matrix readMatrix(Size s) {
	size_t byteSize = s.width * s.height * sizeof(char);
	Matrix m = malloc(byteSize);
	memset(m, ' ', byteSize);

	FILE * file = openFile(FILE_NAME);
	char * line = NULL;
	size_t allocated = 0;
	size_t read;

	size_t y = 0;
  while ((read = getline(&line, &allocated, file)) != -1) {
		if (!strcmp(line, "\n")) {
			break;
		}

		for(size_t x = 0; x < s.width && x < read - 1; x++) {
			m[idx(x, y, s)] = line[x];
		}
		y++;
	}

	return m;
}

void printMatrix(Matrix m, Size s) {
	for(size_t y = 0; y < s.height; y++) {
		for(size_t x = 0; x < s.width; x++) {
			printf("%c", m[y * s.width + x]);
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
	struct _Direction * left;
	struct _Direction * right;
	char ch;
	Point vec;
};
typedef struct _Direction Direction;
Direction Up    = { .ch = '^', .vec = { .x =  0, .y = -1 }};
Direction Down  = { .ch = 'v', .vec = { .x =  0, .y =  1 }};
Direction Left  = { .ch = '<', .vec = { .x = -1, .y =  0 }};
Direction Right = { .ch = '>', .vec = { .x =  1, .y =  0 }};

struct _Path {
	size_t length;
	int * steps;
};
typedef struct _Path Path;

int readInt(char * line, int * number) {
	char buffer[100];
	size_t i = 0;
	for(; isdigit(line[i]); i++) {
		buffer[i] = line[i];
	}
	(*number) = atoi(buffer);
	return i;
}

Path readPath() {
	FILE * file = openFile(FILE_NAME);
	char * line = NULL;
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

	int * steps = malloc(0);
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
	for(size_t y = 0; y < s.height; y++) {
		for(size_t x = 0; x < s.width; x++) {
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

int password(Result result) {
	return 1000 * (result.position.y + 1) + 4 * (result.position.x + 1) +
		(result.direction.ch == '>' ? 0 :
		result.direction.ch == 'v' ? 1 :
		result.direction.ch == '<' ? 2 : 3);
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
	printf("Ended up at (%i, %i) direction %c\n", result.position.x, result.position.y, result.direction.ch);

	printf("\nThe password is %i", password(result));
	// Somehow this program is non deterministic
	// it gives the correct answer most of the time but can also sometimes result in
	// 23502, 46226, 109350, 149022, 68290, 189162, 28530, 196102
	return 0;
}

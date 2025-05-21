#include <raylib.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#define MAX_SNAKE_LENGTH 500
#define GRID_SIZE 20
#define CELL_SIZE 20
typedef struct {
  int positions_x[MAX_SNAKE_LENGTH];
  int positions_y[MAX_SNAKE_LENGTH];
  int length;
  int direction;
  bool alive;
  int food_x;
  int food_y;
  int grid_width;
  int grid_height;
  int score;
} GameState;

GameState gameState;

int bridge_init_game(int width, int height, const char *title) {
  InitWindow(width, height, title);
  if (!IsWindowReady()) {
    return 1;
  }

  SetTargetFPS(30);
  SetRandomSeed(GetTime() * 1000);
  gameState.grid_width = width / CELL_SIZE;
  gameState.grid_height = height / CELL_SIZE;
  gameState.length = 3;
  gameState.direction = 0; // Start moving right
  gameState.alive = true;
  gameState.score = 0;
  for (int i = 0; i < gameState.length; i++) {
    gameState.positions_x[i] = 10 - i;
    gameState.positions_y[i] = 10;
  }
  gameState.food_x = GetRandomValue(0, gameState.grid_width - 1);
  gameState.food_y = GetRandomValue(0, gameState.grid_height - 1);

  return 0;
}

bool check_snake_collision(int x, int y) {
  for (int i = 0; i < gameState.length; i++) {
    if (x == gameState.positions_x[i] && y == gameState.positions_y[i]) {
      return true;
    }
  }
  return false;
}

void bridge_generate_food() {
  int x, y;
  do {
    x = GetRandomValue(0, gameState.grid_width - 1);
    y = GetRandomValue(0, gameState.grid_height - 1);
  } while (check_snake_collision(x, y));

  gameState.food_x = x;
  gameState.food_y = y;
}

int bridge_move_snake() {
  for (int i = gameState.length - 1; i > 0; i--) {
    gameState.positions_x[i] = gameState.positions_x[i - 1];
    gameState.positions_y[i] = gameState.positions_y[i - 1];
  }

  switch (gameState.direction) {
  case 0: // Right
    gameState.positions_x[0]++;
    break;
  case 1: // Down
    gameState.positions_y[0]++;
    break;
  case 2: // Left
    gameState.positions_x[0]--;
    break;
  case 3: // Up
    gameState.positions_y[0]--;
    break;
  }

  return 0;
}

bool bridge_check_wall_collision() {
  return (gameState.positions_x[0] < 0 ||
          gameState.positions_x[0] >= gameState.grid_width ||
          gameState.positions_y[0] < 0 ||
          gameState.positions_y[0] >= gameState.grid_height);
}

bool bridge_check_self_collision() {
  for (int i = 1; i < gameState.length; i++) {
    if (gameState.positions_x[0] == gameState.positions_x[i] &&
        gameState.positions_y[0] == gameState.positions_y[i]) {
      return true;
    }
  }
  return false;
}

bool bridge_check_food_collision() {
  return (gameState.positions_x[0] == gameState.food_x &&
          gameState.positions_y[0] == gameState.food_y);
}

void bridge_grow_snake() {
  if (gameState.length < MAX_SNAKE_LENGTH) {
    gameState.length++;
    gameState.score++;
  }
}

void bridge_set_direction(int direction) {
  if ((gameState.direction + 2) % 4 != direction) {
    gameState.direction = direction;
  }
}

int bridge_get_input() {
  if (IsKeyPressed(KEY_RIGHT) || IsKeyPressed(KEY_D)) {
    return 0; // Right
  } else if (IsKeyPressed(KEY_DOWN) || IsKeyPressed(KEY_S)) {
    return 1; // Down
  } else if (IsKeyPressed(KEY_LEFT) || IsKeyPressed(KEY_A)) {
    return 2; // Left
  } else if (IsKeyPressed(KEY_UP) || IsKeyPressed(KEY_W)) {
    return 3; // Up
  }
  return -1; // No input
}

bool bridge_should_close() { return WindowShouldClose(); }

void bridge_set_game_over() { gameState.alive = false; }

void bridge_render_frame() {
  BeginDrawing();

  ClearBackground(BLACK);
  for (int i = 0; i < gameState.grid_width; i++) {
    for (int j = 0; j < gameState.grid_height; j++) {
      DrawRectangleLines(i * CELL_SIZE, j * CELL_SIZE, CELL_SIZE, CELL_SIZE,
                         DARKGRAY);
    }
  }

  DrawRectangle(gameState.food_x * CELL_SIZE, gameState.food_y * CELL_SIZE,
                CELL_SIZE, CELL_SIZE, RED);

  for (int i = 0; i < gameState.length; i++) {
    if (i == 0) {
      DrawRectangle(gameState.positions_x[i] * CELL_SIZE,
                    gameState.positions_y[i] * CELL_SIZE, CELL_SIZE, CELL_SIZE,
                    GREEN);
    } else {
      DrawRectangle(gameState.positions_x[i] * CELL_SIZE,
                    gameState.positions_y[i] * CELL_SIZE, CELL_SIZE, CELL_SIZE,
                    DARKGREEN);
    }
  }

  DrawText(TextFormat("SCORE: %d", gameState.score), 10, 10, 20, WHITE);
  EndDrawing();
}

void bridge_render_game_over() {
  BeginDrawing();
  ClearBackground(BLACK);

  int width = GetScreenWidth();
  int height = GetScreenHeight();

  DrawText("GAME OVER", width / 2 - MeasureText("GAME OVER", 40) / 2,
           height / 2 - 40, 40, RED);
  DrawText(TextFormat("SCORE: %d", gameState.score),
           width / 2 -
               MeasureText(TextFormat("SCORE: %d", gameState.score), 20) / 2,
           height / 2 + 20, 20, WHITE);

  EndDrawing();
}

void bridge_close_game() { CloseWindow(); }

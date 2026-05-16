#pragma once

typedef struct {
    unsigned char r;
    unsigned char g;
    unsigned char b;
    unsigned char a;
} Color;

void InitWindow(int width, int height, const char* title);
int WindowShouldClose();
void BeginDrawing();
void ClearBackground(Color color);
void DrawText(const char* text, int posX, int posY, int fontSize, Color color);
void EndDrawing();
void CloseWindow();
void SetTargetFPS(int fps);

#include <ctype.h>
#include <stdint.h>
#include <stdlib.h>

#include "raylib.h"
#include "doomgeneric.h"

enum {
    RAYLIB_KEY_ENTER = KEY_ENTER,
    RAYLIB_KEY_ESCAPE = KEY_ESCAPE,
    RAYLIB_KEY_LEFT = KEY_LEFT,
    RAYLIB_KEY_RIGHT = KEY_RIGHT,
    RAYLIB_KEY_UP = KEY_UP,
    RAYLIB_KEY_DOWN = KEY_DOWN,
    RAYLIB_KEY_LEFT_CONTROL = KEY_LEFT_CONTROL,
    RAYLIB_KEY_RIGHT_CONTROL = KEY_RIGHT_CONTROL,
    RAYLIB_KEY_SPACE = KEY_SPACE,
    RAYLIB_KEY_LEFT_SHIFT = KEY_LEFT_SHIFT,
    RAYLIB_KEY_RIGHT_SHIFT = KEY_RIGHT_SHIFT,
    RAYLIB_KEY_LEFT_ALT = KEY_LEFT_ALT,
    RAYLIB_KEY_RIGHT_ALT = KEY_RIGHT_ALT,
    RAYLIB_KEY_F2 = KEY_F2,
    RAYLIB_KEY_F3 = KEY_F3,
    RAYLIB_KEY_F4 = KEY_F4,
    RAYLIB_KEY_F5 = KEY_F5,
    RAYLIB_KEY_F6 = KEY_F6,
    RAYLIB_KEY_F7 = KEY_F7,
    RAYLIB_KEY_F8 = KEY_F8,
    RAYLIB_KEY_F9 = KEY_F9,
    RAYLIB_KEY_F10 = KEY_F10,
    RAYLIB_KEY_F11 = KEY_F11,
    RAYLIB_KEY_EQUAL = KEY_EQUAL,
    RAYLIB_KEY_KP_ADD = KEY_KP_ADD,
    RAYLIB_KEY_MINUS = KEY_MINUS,
    RAYLIB_KEY_KP_SUBTRACT = KEY_KP_SUBTRACT,
    RAYLIB_KEY_A = KEY_A,
    RAYLIB_KEY_Z = KEY_Z,
};

#include "doomkeys.h"

#define KEYQUEUE_SIZE 32

static Texture2D s_texture;
static unsigned short s_key_queue[KEYQUEUE_SIZE];
static unsigned int s_key_queue_write_index;
static unsigned int s_key_queue_read_index;

static unsigned char convert_to_doom_key(int key)
{
    switch (key)
    {
    case RAYLIB_KEY_ENTER:
        return KEY_ENTER;
    case RAYLIB_KEY_ESCAPE:
        return KEY_ESCAPE;
    case RAYLIB_KEY_LEFT:
        return KEY_LEFTARROW;
    case RAYLIB_KEY_RIGHT:
        return KEY_RIGHTARROW;
    case RAYLIB_KEY_UP:
        return KEY_UPARROW;
    case RAYLIB_KEY_DOWN:
        return KEY_DOWNARROW;
    case RAYLIB_KEY_LEFT_CONTROL:
    case RAYLIB_KEY_RIGHT_CONTROL:
        return KEY_FIRE;
    case RAYLIB_KEY_SPACE:
        return KEY_USE;
    case RAYLIB_KEY_LEFT_SHIFT:
    case RAYLIB_KEY_RIGHT_SHIFT:
        return KEY_RSHIFT;
    case RAYLIB_KEY_LEFT_ALT:
    case RAYLIB_KEY_RIGHT_ALT:
        return KEY_LALT;
    case RAYLIB_KEY_F2:
        return KEY_F2;
    case RAYLIB_KEY_F3:
        return KEY_F3;
    case RAYLIB_KEY_F4:
        return KEY_F4;
    case RAYLIB_KEY_F5:
        return KEY_F5;
    case RAYLIB_KEY_F6:
        return KEY_F6;
    case RAYLIB_KEY_F7:
        return KEY_F7;
    case RAYLIB_KEY_F8:
        return KEY_F8;
    case RAYLIB_KEY_F9:
        return KEY_F9;
    case RAYLIB_KEY_F10:
        return KEY_F10;
    case RAYLIB_KEY_F11:
        return KEY_F11;
    case RAYLIB_KEY_EQUAL:
    case RAYLIB_KEY_KP_ADD:
        return KEY_EQUALS;
    case RAYLIB_KEY_MINUS:
    case RAYLIB_KEY_KP_SUBTRACT:
        return KEY_MINUS;
    default:
        return (unsigned char)(key >= RAYLIB_KEY_A && key <= RAYLIB_KEY_Z ? tolower(key) : key);
    }
}

static void add_key_to_queue(int pressed, int key_code)
{
    unsigned char key = convert_to_doom_key(key_code);
    unsigned short key_data = (unsigned short)((pressed << 8) | key);

    s_key_queue[s_key_queue_write_index] = key_data;
    s_key_queue_write_index = (s_key_queue_write_index + 1) % KEYQUEUE_SIZE;
}

static void handle_key_input(void)
{
    int key;

    while ((key = GetKeyPressed()) != 0)
    {
        add_key_to_queue(1, key);
    }

    for (key = 0; key <= 348; key++)
    {
        if (IsKeyReleased(key))
        {
            add_key_to_queue(0, key);
        }
    }
}

void DG_Init(void)
{
    Image image;

    InitWindow(DOOMGENERIC_RESX, DOOMGENERIC_RESY, "DOOM");
    SetTargetFPS(70);

    image = GenImageColor(DOOMGENERIC_RESX, DOOMGENERIC_RESY, BLACK);
    s_texture = LoadTextureFromImage(image);
    UnloadImage(image);
}

void DG_DrawFrame(void)
{
    UpdateTexture(s_texture, DG_ScreenBuffer);

    BeginDrawing();
    ClearBackground(BLACK);
    DrawTexture(s_texture, 0, 0, WHITE);
    EndDrawing();

    handle_key_input();
}

void DG_SleepMs(uint32_t ms)
{
    WaitTime((double)ms / 1000.0);
}

uint32_t DG_GetTicksMs(void)
{
    return (uint32_t)(GetTime() * 1000.0);
}

int DG_GetKey(int *pressed, unsigned char *doom_key)
{
    unsigned short key_data;

    if (s_key_queue_read_index == s_key_queue_write_index)
    {
        return 0;
    }

    key_data = s_key_queue[s_key_queue_read_index];
    s_key_queue_read_index = (s_key_queue_read_index + 1) % KEYQUEUE_SIZE;
    *pressed = key_data >> 8;
    *doom_key = key_data & 0xff;
    return 1;
}

void DG_SetWindowTitle(const char *title)
{
    SetWindowTitle(title);
}

int main(int argc, char **argv)
{
    doomgeneric_Create(argc, argv);

    while (!WindowShouldClose())
    {
        doomgeneric_Tick();
    }

    UnloadTexture(s_texture);
    CloseWindow();
    return 0;
}

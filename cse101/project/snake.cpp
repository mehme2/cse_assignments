#include "snake.h"
#include "arduino.h"
#include <LiquidCrystal.h>
#include "pins.h"
#include "memory_game.h"

void snakegame()
{
  LiquidCrystal lcd(lcd1,lcd2,lcd3,lcd4,lcd5,lcd6); 
  lcd.begin(16,2);
  int lastState[4];
  int curState[4];
  byte scrBuf[4][2][8]; randomSeed(analogRead(A0)); 
  struct snekPart { int x; 
    int y; 
    snekPart* next; 
  } *head; 
  head = malloc(sizeof(snekPart)); 
  head->x = 10; 
  head->y = 8; 
  head->next = nullptr; int dx = 1; 
  int dy = 0; 
  int fx = random(20); 
  int fy = random(16); 
  while(1) 
  { 
    
    int nx = (head->x + dx) % 20; 
    int ny = (head->y + dy) % 16; 
    if(nx<0)nx=19; 
    if(ny<0)ny=15; 
    if(nx == fx && ny == fy) 
    { 
      fx = random(20);
      fy = random(16); 
      snekPart* newHead = malloc(sizeof(snekPart)); 
      newHead->x = nx; 
      newHead->y = ny; 
      newHead->next = head; 
      head = newHead; 
    } 
    else 
    { 
      snekPart* cur = head;
      int ox = cur->x;
      int oy = cur->y;
      while(cur->next != nullptr) 
      {
        int tx = cur->next->x;
        int ty = cur->next->y;
        cur->next->x = ox; 
        cur->next->y = oy; 
        ox = tx;
        oy = ty;
        cur = cur->next; 
      } 
      head->x = nx; 
      head->y = ny; 
    } 
    bool hit = false; 
    memset(scrBuf, 0, 64); 
    scrBuf[int(head->x * 0.2)][int(head->y * 0.125)][head->y % 8] |= 1 << (4-(head->x % 5)); 
    scrBuf[int(fx * 0.2)][int(fy * 0.125)][fy % 8] |= 1 << (4-(fx % 5)); 
    snekPart* cur = head->next; 
    while(cur != nullptr) 
    { 
      hit = hit || (cur->x == head->x && cur->y == head->y); 
      int x = cur->x * 0.2; 
      int y = cur->y * 0.125; 
      scrBuf[x][y][cur->y - (y * 8)] |= (1 << (4 - (cur->x - (x * 5)))); 
      cur = cur->next; 
    }
    lcd.createChar(0,scrBuf[0][0]);
    lcd.createChar(1,scrBuf[1][0]);
    lcd.createChar(2,scrBuf[2][0]); 
    lcd.createChar(3,scrBuf[3][0]);
    lcd.createChar(4,scrBuf[0][1]); 
    lcd.createChar(5,scrBuf[1][1]);
    lcd.createChar(6,scrBuf[2][1]);
    lcd.createChar(7,scrBuf[3][1]);
    lcd.clear(); 
    lcd.setCursor(6,0); 
    lcd.write((byte)0); 
    lcd.write((byte)1); 
    lcd.write((byte)2); 
    lcd.write((byte)3); 
    lcd.setCursor(6,1); 
    lcd.write((byte)4); 
    lcd.write((byte)5); 
    lcd.write((byte)6); 
    lcd.write((byte)7); 
    if(hit) break; 
    unsigned long long time = millis();
    while(millis() - time < 200)
    {
      lastState[0] = curState[0]; 
    lastState[1] = curState[1]; 
    lastState[2] = curState[2]; 
    lastState[3] = curState[3]; 
    curState[0] = digitalRead(button1); 
    curState[1] = digitalRead(button2); 
    curState[2] = digitalRead(button3); 
    curState[3] = digitalRead(button4); 
    if(lastState[2] == LOW && curState[2] == HIGH) 
    { 
      int tmp = dx;
      dx = -dy; 
      dy = tmp; 
    } 
    if(lastState[3] == LOW && curState[3] == HIGH) 
    { 
      int tmp = dx;
      dx = dy;
      dy = -tmp; 
    }
    }
  }
}
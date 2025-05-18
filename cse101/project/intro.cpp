#include "intro.h"
#include "arduino.h"
#include <LiquidCrystal.h>
#include "pins.h"
#include "buzzer.h"

void intro()
{
  LiquidCrystal lcd(lcd1,lcd2,lcd3,lcd4,lcd5,lcd6); 
  lcd.begin(16,2);
  static const uint8_t logo[4][16] = {
    {
      0b01110,
      0b10001,
      0b10000,
      0b10111,
      0b10001,
      0b10001,
      0b01111,
      0b00000,
      0b00000,
      0b00000,
      0b00000,
      0b00000,
      0b00000,
      0b00000,
      0b00000,
      0b00000
    },
    {
      0b11111,
      0b00100,
      0b00100,
      0b00100,
      0b00100,
      0b00100,
      0b00100,
      0b00000,
      0b11110,
      0b10001,
      0b10001,
      0b11110,
      0b10001,
      0b10001,
      0b11110,
      0b00000
    },
    {
      0b10001,
      0b10001,
      0b10001,
      0b10001,
      0b10001,
      0b10001,
      0b01110,
      0b00000,
      0b01110,
      0b10001,
      0b10001,
      0b10001,
      0b10001,
      0b10001,
      0b01110,
      0b00000
    },
    {
      0b00000,
      0b00000,
      0b00000,
      0b00000,
      0b00000,
      0b00000,
      0b00000,
      0b00000,
      0b10001,
      0b10001,
      0b10001,
      0b01010,
      0b00100,
      0b00100,
      0b00100,
      0b00000
    }
  };
  uint8_t scrBuf[2][4][8];
  memset(scrBuf, 0, 64);
  for(int y = 0; y < 16; y++)
  {
    for(int i = 0; i < y; i++)
    {
      for(int j = 0; j < 4; j++)
      {
        if(i < 8)
        {
          scrBuf[0][j][i] = logo[j][15 + i - y];
        }
        else 
        {
          scrBuf[1][j][i - 8] = logo[j][15 + i - y];
        }
      }
    }
    for(int i = 0; i < 8; i++)
    {
      lcd.createChar(i, scrBuf[i / 4][i % 4]);
    }
    lcd.clear();
    lcd.setCursor(6, 0);
    lcd.write(byte(0));
    lcd.write(1);
    lcd.write(2);
    lcd.write(3);
    lcd.setCursor(6, 1);
    lcd.write(4);
    lcd.write(5);
    lcd.write(6);
    lcd.write(7);
    delay(200);
  }
  play_intro();
  lcd.clear();
}
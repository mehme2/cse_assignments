#include "pins.h"
#include "memory_game.h"
#include "buzzer.h"
#include <LiquidCrystal.h>
#include "intro.h"
#include "snake.h"


LiquidCrystal lcd(lcd1,lcd2,lcd3,lcd4,lcd5,lcd6);

void setup()
{
  Serial.begin(9600);
  
  lcd.begin(16,2);
  
  pinMode(led1, OUTPUT);
  pinMode(led2, OUTPUT);
  pinMode(led3, OUTPUT);
  pinMode(led4, OUTPUT);
  pinMode(button1, INPUT);
  pinMode(button2, INPUT);
  pinMode(button3, INPUT);
  pinMode(button4, INPUT);

  pinMode(buzzerpin1, OUTPUT);
  pinMode(buzzerpin2, OUTPUT);

  intro();
}

void loop()
{
  lcd.print("Select a game!");
  delay(1000);
  lcd.clear();
  lcd.print("Y: Memory");
  lcd.setCursor(0,1);
  lcd.print("R:Snake ");
  int selection = which_pressed();
  lcd.clear();
  if(selection==1){
    start_game();
  }
  if(selection==2){
    snakegame();
  }

  lcd.print("Out.");
  delay(1000);
  lcd.clear();
  while(1){
    ;
    }
}



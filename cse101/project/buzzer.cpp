#include "pins.h"
#include "arduino.h"
#include "buzzer.h"
#include <LiquidCrystal.h>

void play_intro()
{
    tone(buzzerpin1,NOTE_B5,100);
    delay(100);
    tone(buzzerpin1,NOTE_E6,850);
    delay(600);
    noTone(buzzerpin1);
}

void play_gameover()
{
    tone(buzzerpin1,NOTE_E6,850);
    delay(600);
    tone(buzzerpin1,NOTE_B5,100);
    delay(100);
    noTone(buzzerpin1);
}

void play_button1(){
    tone(buzzerpin1,700);
    delay(50);
    noTone(buzzerpin1);
    delay(50);
}

void play_button2(){
    tone(buzzerpin1,1000);
    delay(50);
    noTone(buzzerpin1);
    delay(50);
}

void play_button3(){
    tone(buzzerpin1,1300);
    delay(50);
    noTone(buzzerpin1);
    delay(50);
}

void play_button4(){
    tone(buzzerpin1,1600);
    delay(50);
    noTone(buzzerpin1);
    delay(50);
}

void play_win(){
    tone(buzzerpin1,NOTE_E6,125);
    delay(130);
    tone(buzzerpin1,NOTE_G6,125);
    delay(130);
    tone(buzzerpin1,NOTE_E7,125);
    delay(130);
    tone(buzzerpin1,NOTE_C7,125);
    delay(130);
    tone(buzzerpin1,NOTE_D7,125);
    delay(130);
    tone(buzzerpin1,NOTE_G7,125);
    delay(125);
    noTone(buzzerpin1);
}

void play_lose(){
    tone(buzzerpin1,450,125);
    delay(130);
    tone(buzzerpin1,350,125);
    delay(130);
    tone(buzzerpin1,400,125);
    delay(130);
    tone(buzzerpin1,300,125);
    delay(130);
    tone(buzzerpin1,350,125);
    delay(125);
    tone(buzzerpin1,200,125);
}
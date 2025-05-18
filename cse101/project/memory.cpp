#include "memory_game.h"
#include "pins.h"
#include "buzzer.h"
#include "arduino.h"
#include <LiquidCrystal.h>

void start_game(){
  LiquidCrystal lcd(lcd1,lcd2,lcd3,lcd4,lcd5,lcd6);

  lcd.begin(16,2);
  
  int condition=1;

  int difficulty;

  while(condition){
    //Opening part
    lcd.setCursor(6,0);
    lcd.print("MEMORY");
    lcd.setCursor(7,1);
    lcd.print("GAME");
    play_intro();
    delay(2000);
    lcd.clear();
    lcd.setCursor(0, 0);
    
    //Difficulty sellection
    lcd.print("Select ");
    lcd.setCursor(0,1);
    lcd.print("Difficulty");
    delay(2000);
    lcd.clear();
    lcd.setCursor(0,0);
    lcd.print("Yellow:1 red:2");
    lcd.setCursor(0,1);
    lcd.print("Blue:3");

    difficulty = which_pressed();

    lcd.clear();

    lcd.setCursor(0,0);

    if(difficulty == 1){
      lcd.print("Difficulty 1");
      delay(2000);
      lcd.clear();
      memorygame1();
    }
    if(difficulty == 2){
      lcd.print("Difficulty 2");
      delay(2000);
      lcd.clear();
      memorygame2();
    }
    if(difficulty == 3){
      //Explenation of difficuly 3
      lcd.print("Press in");
      lcd.setCursor(0,1);
      lcd.print("Reverse order");
      delay(2000);
      lcd.clear();
      lcd.setCursor(0,0);

      lcd.print("Difficulty 3");
      delay(2000);
      lcd.clear();
      memorygame3();
    }
    if(difficulty == 4){
      while(1){
        which_pressedq();
      }
    }

    //Asking whether or not they want to continue
    lcd.print("Do you want to");
    lcd.setCursor(0,1);
    lcd.print("Continue?");
    delay(2000);
    lcd.clear();
    lcd.setCursor(0,0);
    lcd.print("Yellow : Yes");
    lcd.setCursor(0,1);
    lcd.print("Red: No");

    difficulty = which_pressed();

    if(difficulty == 2){
      condition = 0;
      play_gameover();
    }

    lcd.clear();
    lcd.setCursor(0,0);
  }
}

int memorygame1(){
  randomSeed(analogRead(A0));

  LiquidCrystal lcd(lcd1,lcd2,lcd3,lcd4,lcd5,lcd6);

  lcd.begin(16,2);

  int score = 0; 
  int led_que[50]; //This array holds which leds will light up	
  int button_que[50]; //This array holds which buttons are pressed in order
  int level = 1; //How many lights user have to remember for the first game
  int current_queue=0;
  
  lcd.print("Level: ");
  lcd.print(level);
  delay(3000);
  lcd.clear();
  while(1){
    led_que[current_queue] = random(1,5);
    current_queue++;

    sequence(level,led_que);

    for(int i = 0; i<level ; i++){
      button_que[i] = which_pressed();
    }

    for(int i = 0; i<level ; i++){
      if(button_que[i]!=led_que[i]){
          lcd.print("You Lose!");
          play_lose();
          delay(1000);
          lcd.clear();
          lcd.print("Final score: ");
          lcd.print(score);
          delay(3000);
          lcd.clear();
      	  return 0;
      }
    }

    score = calc_score(level,score);
    lcd.print("Congratulations!");
    play_win();
    delay(1000);
    lcd.clear();
    level=level+1;
    lcd.print("Level: ");
    lcd.print(level);
    delay(3000);
    lcd.clear();
    delay(1000);
  }
}

int memorygame2(){
  
  randomSeed(analogRead(A0));

  LiquidCrystal lcd(lcd1,lcd2,lcd3,lcd4,lcd5,lcd6);

  lcd.begin(16,2);
  
  //Necessary variables
  int score = 0; 
  int led_que[50]; //This array holds which leds will light up	
  int button_que[50]; //This array holds which buttons are pressed in order
  int level = 4; //How many lights user have to remember for the first game
  
  lcd.print("Level: ");
  lcd.print(level-3);
  delay(3000);
  lcd.clear();

  while(1){
    for(int i = 0; i<level ; i++){
      led_que[i] = random(1,5);
    }

    sequence(level,led_que);

    for(int i = 0; i<level ; i++){
      button_que[i] = which_pressed();
    }

    for(int i = 0; i<level ; i++){
      if(button_que[i]!=led_que[i]){
          lcd.print("You Lose!");
          play_lose();
          delay(1000);
          lcd.clear();
          lcd.print("Final score: ");
          lcd.print(score);
          delay(3000);
          lcd.clear();
      	  return 0;
      }
    }

    score = calc_score(level,score);
    lcd.print("Congratulations!");
    play_win();
    delay(1000);
    lcd.clear();
    level=level+1;
    lcd.print("Level: ");
    lcd.print(level-3);
    delay(3000);
    lcd.clear();
    delay(1000);
  }
}

int memorygame3(){
  randomSeed(analogRead(A0));

  LiquidCrystal lcd(lcd1,lcd2,lcd3,lcd4,lcd5,lcd6);

  lcd.begin(16,2);
  
  //Necessary variables
  int score = 0; 
  int led_que[50]; //This array holds which leds will light up	
  int button_que[50]; //This array holds which buttons are pressed in order
  int reverse_que[50]; //This array holds reversed led que
  int level = 4; //How many lights user have to remember for the first game
  
  lcd.print("Level: ");
  lcd.print(level-3);
  delay(3000);
  lcd.clear();

  while(1){
    for(int i = 0; i<level ; i++){
      led_que[i] = random(1,5);
    }
    for(int i = 0; i<level; i++){
      reverse_que[i]=led_que[(level-1)-i];
    }

    sequence(level,led_que);

    for(int i = 0; i<level ; i++){
      button_que[i] = which_pressed();
      delay(100);
    }

    for(int i = 0; i<level ; i++){
      if(button_que[i]!=reverse_que[i]){
          lcd.print("You Lose!");
          play_lose();
          delay(1000);
          lcd.clear();
          lcd.print("Final score: ");
          lcd.print(score);
          delay(3000);
          lcd.clear();
      	  return 0;
      }
    }

    score = calc_score(level,score);
    lcd.print("Congratulations!");
    play_win();
    delay(1000);
    lcd.clear();
    level=level+1;
    lcd.print("Level: ");
    lcd.print(level-3);
    delay(3000);
    lcd.clear();
    delay(1000);
  }
}

int which_pressed(){ 
  
  //Button state variables;
  int current_state1;
  int current_state2;
  int current_state3;
  int current_state4;
  int last_state1 = digitalRead(button1);
  int last_state2 = digitalRead(button2);
  int last_state3 = digitalRead(button3);
  int last_state4 = digitalRead(button4);
  
  while(1){
    current_state1 = digitalRead(button1);
    current_state2 = digitalRead(button2);
    current_state3 = digitalRead(button3);
    current_state4 = digitalRead(button4);
    delay(50);
      
      if(last_state1 == LOW && current_state1 == HIGH){
        digitalWrite(led1,HIGH);
        play_button1();
        digitalWrite(led1,LOW);
        return 1;
      }
      if(last_state2 == LOW && current_state2 == HIGH){
        digitalWrite(led2,HIGH);
        play_button2();
        digitalWrite(led2,LOW);	
        return 2;
      }
      if(last_state3 == LOW && current_state3 == HIGH){
        digitalWrite(led3,HIGH);
        play_button3();
        digitalWrite(led3,LOW);
        return 3;
      }
      if(last_state4 == LOW && current_state4 == HIGH){
        digitalWrite(led4,HIGH);
        play_button4();
        digitalWrite(led4,LOW);	
        return 4;
      }
      last_state1 = current_state1;
      last_state2 = current_state2;
      last_state3 = current_state3;
      last_state4 = current_state4;
  }
}

int which_pressedq(){ 
  
  //Button state variables;
  int current_state1;
  int current_state2;
  int current_state3;
  int current_state4;
  int last_state1 = digitalRead(button1);
  int last_state2 = digitalRead(button2);
  int last_state3 = digitalRead(button3);
  int last_state4 = digitalRead(button4);
  
  while(1){
    current_state1 = digitalRead(button1);
    current_state2 = digitalRead(button2);
    current_state3 = digitalRead(button3);
    current_state4 = digitalRead(button4);      
      if(last_state1 == LOW && current_state1 == HIGH){
        digitalWrite(led1,HIGH);
        play_button1();
        digitalWrite(led1,LOW);
        return 1;
      }
      if(last_state2 == LOW && current_state2 == HIGH){
        digitalWrite(led2,HIGH);
        play_button2();
        digitalWrite(led2,LOW);	
        return 2;
      }
      if(last_state3 == LOW && current_state3 == HIGH){
        digitalWrite(led3,HIGH);
        play_button3();
        digitalWrite(led3,LOW);
        return 3;
      }
      if(last_state4 == LOW && current_state4 == HIGH){
        digitalWrite(led4,HIGH);
        play_button4();
        digitalWrite(led4,LOW);	
        return 4;
      }
      last_state1 = current_state1;
      last_state2 = current_state2;
      last_state3 = current_state3;
      last_state4 = current_state4;
  }
}

void sequence(int lvl, int  *sequence){
  int bekleme_suresi=400;
  for(int i=0; i<lvl;i++){
    if(sequence[i]==1){
      digitalWrite(led1,HIGH);
      play_button1();
      delay(200);
      delay(bekleme_suresi);
      digitalWrite(led1,LOW);
    }
    if(sequence[i]==2){
      digitalWrite(led2,HIGH);
      play_button2();
      delay(bekleme_suresi);
      digitalWrite(led2,LOW);
    }
    if(sequence[i]==3){
      digitalWrite(led3,HIGH);
      play_button3();
      delay(bekleme_suresi);
      digitalWrite(led3,LOW);
    }
    if(sequence[i]==4){
      digitalWrite(led4,HIGH);
      play_button4();
      delay(bekleme_suresi);
      digitalWrite(led4,LOW);
    }
    delay(bekleme_suresi);
  }
}

int calc_score(int level,int score){
  if(level<=7){
    return score+20;
  }
  if(level>7 && level<=10){
    return score+40;
  }
  if(level>10 && level<=13){
    return score+60;
  }
  else{
    return score+80;
  }
}